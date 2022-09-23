class ArrayValue:
    def __init__(self, type: type, arr=[]):
        self.valType = type
        self.arr = arr

    def __repr__(self):
        return self.__str__()

    def __str__(self):
        return f"ArrayValue({self.valType}, {repr(self.arr)})"

    def verify_value(self, value):
        return isinstance(value, self.valType)

    def add(self, value, index=None):
        if self.verify_value(value=value):
            if index != None:
                if index < len(self.arr):
                    self.arr[index] = value
                else:
                    raise IndexError("Index out of range")
            else:
                self.arr.append(value)
        else:
            raise TypeError("Element type does not match array type")

    def get_arr(self, index=None):
        if index != None:
            if index < len(self.arr):
                return self.arr[index]
            else:
                raise IndexError("Index out of range")
        else:
            return self.arr


class Entry:
    def __init__(self, type: type):
        self.type = type


class SymbolEntry(Entry):
    def __init__(self, type: type, value):
        super().__init__(type)
        self.value = value

    def __repr__(self):
        return self.__str__()

    def __str__(self):
        return f"SymbolEntry({self.type}, {repr(self.value)})"


class FuncEntry(Entry):
    def __init__(
        self, returnType: type, argSymbols: list, funcBody, context, isReturnArr=False
    ):
        super().__init__(returnType)
        self.argSymbols = argSymbols
        self.funcBody = funcBody
        self.context = context
        self.isReturnArr = isReturnArr

    def __repr__(self):
        return self.__str__()

    def __str__(self):
        return f"FuncEntry({self.type}, {repr(self.argSymbols)})"

    def type_checker_return(self):
        # check function type matches return type
        return (
            (isinstance(self.context.funcReturnVal, self.type))
            or (
                isinstance(self.context.funcReturnVal, list)
                and (
                    not self.context.funcReturnVal
                    or isinstance(self.context.funcReturnVal[0], self.type)
                )
            )
            or (self.context.funcReturnVal == None and not self.isReturnArr)
        )


class TheContext:
    def __init__(self, parent):
        self.parent = parent
        self.symbolTable: dict[str, SymbolEntry] = {}
        self.funcTable: dict[str, FuncEntry] = {}

        self.lineNo = 1
        self.columnNo = 1

        self.returnFlag = False
        self.breakFlag = False
        self.continueFlag = False
        self.inLoop = False
        self.inFunc = False
        self.funcReturnVal = None

        self.should_return = (
            lambda: self.inFunc
            and self.returnFlag
            and not self.breakFlag
            and not self.continueFlag
        )
        self.should_break = lambda: self.inLoop and self.breakFlag
        self.should_continue = lambda: self.inLoop and self.continueFlag

    def reset_flags_func(self):
        self.returnFlag = False
        self.continueFlag = False
        self.breakFlag = False
        self.inLoop = False
        self.funcReturnVal = None

    def get_ident_type(self, symbol: str, getArr=False):
        sEntry: SymbolEntry = self.symbolTable.get(symbol, None)
        if not sEntry and self.parent:
            return self.parent.get_ident_type(symbol=symbol, getArr=getArr)

        if sEntry:
            if getArr:
                sArr: ArrayValue = sEntry.value
                return sArr.valType
            else:
                return sEntry.type

        return None

    def set_ident(
        self,
        symbol: str,
        valType: type,
        value,
        index=None,
        isAppend=False,
        isArr=False,
    ):
        if isArr:
            sArrType = type([])
            if index != None:
                if not isinstance(self.get_ident(symbol=symbol), ArrayValue):
                    raise TypeError(
                        self.get_error_message(f"Variable {symbol} is not an array")
                    )

                try:
                    sArr: ArrayValue = self.get_ident(symbol=symbol)  # get arr object
                    sArr.add(value=value, index=index)  # add value at index
                    sEntryNew = SymbolEntry(type=sArrType, value=sArr)
                    self.symbolTable[symbol] = sEntryNew
                except Exception as e:
                    raise ValueError(self.get_error_message(str(e)))
            elif isAppend:
                if not isinstance(self.get_ident(symbol=symbol), ArrayValue):
                    raise TypeError(
                        self.get_error_message(f"Variable {symbol} is not an array")
                    )

                try:
                    sArr: ArrayValue = self.get_ident(symbol=symbol)  # get arr object
                    sArr.add(value=value)  # append at index
                    sEntryNew = SymbolEntry(type=sArrType, value=sArr)
                    self.symbolTable[symbol] = sEntryNew
                except Exception as e:
                    raise ValueError(self.get_error_message(str(e)))
            else:
                if valType:  # new arr declaration
                    sType = valType
                else:  # arr re assignment
                    sType = self.get_ident_type(symbol=symbol, getArr=True)
                    if not sType:
                        raise ValueError(
                            self.get_error_message(
                                f"Variable {symbol} doesn't exist/has not yet been defined"
                            )
                        )

                if not self.verify_arr_type(arr=value, arrType=sType):
                    raise TypeError(
                        self.get_error_message(
                            "Element(s) in array do not match array type"
                        )
                    )

                arrVal = ArrayValue(type=sType, arr=value)
                sEntry = SymbolEntry(type=sArrType, value=arrVal)
                self.symbolTable[symbol] = sEntry
        else:
            if valType:  # new variable declaration
                sType = valType
            else:  # variable re assignment
                sType = self.get_ident_type(symbol=symbol)
                if not sType:
                    raise ValueError(
                        self.get_error_message(
                            f"Variable {symbol} doesn't exist/has not yet been defined"
                        )
                    )
            sEntry = SymbolEntry(type=sType, value=value)
            self.symbolTable[symbol] = sEntry

    def get_ident(self, symbol: str, index=None, getArr=False):
        sEntry: SymbolEntry = self.symbolTable.get(symbol, None)
        if not sEntry and self.parent:
            return self.parent.get_ident(symbol=symbol, index=index, getArr=getArr)

        if sEntry:
            if getArr:
                if not isinstance(sEntry.value, ArrayValue):
                    raise TypeError(
                        self.get_error_message(f"Variable {symbol} is not an array")
                    )
                try:
                    if index != None:
                        return sEntry.value.get_arr(index=index)
                    else:
                        return sEntry.value.get_arr()
                except Exception as e:
                    raise ValueError(self.get_error_message(str(e)))
            else:
                return sEntry.value

        return None

    def remove_ident(self, symbol: str):
        del self.symbolTable[symbol]

    def set_func(
        self,
        symbol: str,
        returnType: type,
        argSymbols: list,
        funcBody,
        context,
        isReturnArr=False,
    ):
        fEntry = FuncEntry(
            returnType=returnType,
            argSymbols=argSymbols,
            funcBody=funcBody,
            context=context,
            isReturnArr=isReturnArr,
        )
        self.funcTable[symbol] = fEntry

    def get_func(self, symbol: str):
        fEntry: FuncEntry = self.funcTable.get(symbol, None)
        if not fEntry and self.parent:
            return self.parent.get_func(symbol=symbol)
        else:
            return fEntry

    def remove_func(self, symbol: str):
        del self.funcTable[symbol]

    def type_checker_assign(self, left: str, right, hasIndex=False):
        # check variable type matches assigned value type
        if right == None:  # when value = None 'null' keyword, allow it to be stored
            return True

        if hasIndex:
            varType = self.get_ident_type(symbol=left, getArr=True)
            if varType:
                return isinstance(right, varType)
            else:
                raise ValueError(
                    self.get_error_message(
                        f"Variable {left} doesn't exist/has not yet been defined"
                    )
                )
        else:
            varType = self.get_ident_type(symbol=left)
            if varType:
                return isinstance(right, varType)
            else:
                raise ValueError(
                    self.get_error_message(
                        f"Variable {left} doesn't exist/has not yet been defined"
                    )
                )

    def type_checker(self, left, right):
        # check types of lhs and rhs values match in binary operation
        if isinstance(left, (int, float)) and isinstance(right, (int, float)):
            return True

        if isinstance(left, type(None)) or isinstance(right, type(None)):
            return True

        return type(left) == type(right)

    def is_bool(self, value):
        return isinstance(value, bool)

    def is_real(self, value):
        return isinstance(value, (int, float))

    def verify_arr_type(self, arr: list, arrType: type):
        if arr == None:
            return False  # don't want arrs to be null but variables can be null
        return all(isinstance(x, arrType) for x in arr)

    def update_line_col(self, lineNo: int, columnNo: int):
        self.lineNo = lineNo
        self.columnNo = columnNo

    def get_error_message(self, message):
        return f"line: {repr(self.lineNo)} column: {repr(self.columnNo)}. {message}"

    def _type_builtin(self, left, right):
        # function for type keyword
        if isinstance(left, (str, int, float, bool)) and isinstance(
            right, (str, int, float, bool)
        ):
            return type(left) == type(right)
        elif isinstance(left, list) and isinstance(right, list):
            if (
                len(left) == 0 or len(right) == 0
            ):  # if either arrs are empty return false
                return False
            else:
                return type(left[0]) == type(right[0])  # else compare their contents
        else:
            raise ValueError(self.get_error_message("Argument(s) have invalid types"))
