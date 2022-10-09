import copy


class Value:
    def __init__(self, type: type):
        self.valType = type


class ArrayValue(Value):
    def __init__(self, type: type, arr=[]):
        super().__init__(type)
        self.arr: list = arr

    def __repr__(self):
        return self.__str__()

    def __str__(self):
        return f"ArrayValue(type={self.valType}, elems={repr(self.arr)})"

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

    def remove(self, index=None):
        if index != None:
            if index < len(self.arr):
                del self.arr[index]
            else:
                raise IndexError("Index out of range")
        else:
            raise TypeError("Invalid index")

    def get_arr(self, index=None):
        if index != None:
            if index < len(self.arr):
                return self.arr[index]
            else:
                raise IndexError("Index out of range")
        else:
            return self.arr


class MapValue(Value):
    def __init__(self, map={}):
        super().__init__(object)
        self.map: dict = map

    def __repr__(self):
        return self.__str__()

    def __str__(self):
        return f"MapValue(type={self.valType}, elems={repr(self.map)})"

    def add(self, value, index=None):
        if index != None:
            self.map[index] = value
        else:
            raise KeyError("Invalid index")

    def remove(self, index=None):
        if index != None:
            if index in self.map:
                del self.map[index]
            else:
                raise KeyError(f"Key, {index}, doesn't exist in map")
        else:
            raise TypeError("Invalid index")

    def get_map(self, index=None):
        if index != None:
            return self.map.get(index, None)
        else:
            return self.map


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
        return f"SymbolEntry(type={self.type}, value={repr(self.value)})"


class FuncEntry(Entry):
    def __init__(
        self, returnType: type, argSymbols: list, funcBody, context, isReturnArr
    ):
        super().__init__(returnType)
        self.argSymbols = argSymbols
        self.funcBody = funcBody
        self.context = context
        self.isReturnArr = isReturnArr

    def __repr__(self):
        return self.__str__()

    def __str__(self):
        return f"FuncEntry(type={self.type}, params={repr(self.argSymbols)})"

    def copy(self):
        newContext = TheContext(parent=self.context.parent)
        newContext.inFunc = self.context.inFunc
        newContext.inClass = self.context.inClass
        newContext.classVal = self.context.classVal
        newContext.symbolTable = copy.deepcopy(self.context.symbolTable)
        return FuncEntry(
            returnType=self.type,
            argSymbols=self.argSymbols,
            funcBody=self.funcBody,
            context=newContext,
            isReturnArr=self.isReturnArr,
        )

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


class ClassEntry(Entry):
    def __init__(self, constructor: FuncEntry, parentSymbol: str, context):
        super().__init__(object)
        self.constructor = constructor
        self.parentSymbol = parentSymbol
        self.context = context

    def __repr__(self):
        return self.__str__()

    def __str__(self):
        return f"ClassEntry(type={self.type}, name=<{repr(self.context.classVal)}>)"

    def copy(self):
        newContext = TheContext(parent=self.context.parent)
        newContext.inClass = self.context.inClass
        newContext.classVal = self.context.classVal
        newContext.symbolTable = copy.deepcopy(self.context.symbolTable)
        newContext.funcTable = self.context.funcTable
        newContext.classTable = self.context.classTable
        return ClassEntry(
            constructor=self.constructor.copy(),
            parentSymbol=self.parentSymbol,
            context=newContext,
        )


class TheContext:
    def __init__(self, parent=None):
        self.parent = parent
        self.symbolTable: dict[str, SymbolEntry] = {}
        self.funcTable: dict[str, FuncEntry] = {}
        self.classTable: dict[str, ClassEntry] = {}

        self.lineNo = 1
        self.columnNo = 1

        self.returnFlag = False
        self.breakFlag = False
        self.continueFlag = False
        self.inLoop = False
        self.inFunc = False
        self.inClass = False
        self.funcReturnVal = None  # return value of function
        self.classVal = None  # class name

        self.should_return = (
            lambda: self.inFunc
            and self.returnFlag
            and not self.breakFlag
            and not self.continueFlag
        )
        self.should_break = lambda: self.inLoop and self.breakFlag
        self.should_continue = lambda: self.inLoop and self.continueFlag

    def get_ident_type(self, symbol: str, getArrMap=False):
        sEntry: SymbolEntry = self.symbolTable.get(symbol, None)
        if not sEntry and self.parent:
            return self.parent.get_ident_type(symbol=symbol, getArrMap=getArrMap)

        if sEntry:
            if getArrMap:
                if isinstance(sEntry.value, Value):
                    sVal = sEntry.value
                    return sVal.valType
                else:
                    raise TypeError(
                        self.get_error_message(
                            f"Variable {symbol} is not an array or map"
                        )
                    )
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
        isMap=False,
    ):
        sArrType = list
        sMapType = dict

        if isArr:
            if index != None:
                if not isinstance(self.get_ident(symbol=symbol), ArrayValue):
                    raise TypeError(
                        self.get_error_message(f"Variable {symbol} is not an array")
                    )
                if not self.is_real(index):
                    raise TypeError(
                        self.get_error_message(
                            "Invalid index type. Should be real value"
                        )
                    )

                try:
                    sArr: ArrayValue = self.get_ident(symbol=symbol)  # get arr object
                    sArr.add(value=value, index=int(index))  # add value at index
                except Exception as e:
                    raise ValueError(self.get_error_message(str(e)))
            elif isAppend:
                if not isinstance(self.get_ident(symbol=symbol), ArrayValue):
                    raise TypeError(
                        self.get_error_message(f"Variable {symbol} is not an array")
                    )

                try:
                    sArr: ArrayValue = self.get_ident(symbol=symbol)  # get arr object
                    sArr.add(value=value)  # append
                except Exception as e:
                    raise ValueError(self.get_error_message(str(e)))
            else:
                if valType:  # new arr declaration
                    sType = valType
                else:  # arr re assignment
                    if self.type_checker_object(symbol=symbol):
                        sType = object
                    else:
                        sType = self.get_ident_type(symbol=symbol, getArrMap=True)
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
        elif isMap:
            if index != None:
                if not isinstance(self.get_ident(symbol=symbol), MapValue):
                    raise TypeError(
                        self.get_error_message(f"Variable {symbol} is not a map")
                    )

                try:
                    sMap: MapValue = self.get_ident(symbol=symbol)
                    sMap.add(value=value, index=index)
                except Exception as e:
                    raise ValueError(self.get_error_message(str(e)))
            else:
                if valType:  # new map declaration
                    pass
                else:  # map re assignment
                    if self.get_ident_type(symbol=symbol, getArrMap=True):
                        pass
                    else:
                        raise ValueError(
                            self.get_error_message(
                                f"Variable {symbol} doesn't exist/has not yet been defined"
                            )
                        )

                mapVal = MapValue(map=value)
                sEntry = SymbolEntry(type=sMapType, value=mapVal)
                self.symbolTable[symbol] = sEntry
        else:
            if valType:  # new variable declaration
                sType = valType
            else:  # variable re assignment
                if self.type_checker_object(symbol=symbol):
                    sType = object
                else:
                    sType = self.get_ident_type(symbol=symbol, getArrMap=False)
                if not sType:
                    raise ValueError(
                        self.get_error_message(
                            f"Variable {symbol} doesn't exist/has not yet been defined"
                        )
                    )

            # allows for obj y := [..] or obj y := {..}
            if isinstance(value, list) and sType == object:
                arrVal = ArrayValue(type=sType, arr=value)
                sEntry = SymbolEntry(type=sArrType, value=arrVal)
            elif isinstance(value, dict):
                mapVal = MapValue(map=value)
                sEntry = SymbolEntry(type=sMapType, value=mapVal)
            else:
                sEntry = SymbolEntry(type=sType, value=value)

            self.symbolTable[symbol] = sEntry

        if self.inClass and self.parent:
            if symbol in self.parent.symbolTable:
                self.parent.set_ident(
                    symbol=symbol,
                    valType=valType,
                    value=value,
                    index=index,
                    isAppend=isAppend,
                    isArr=isArr,
                    isMap=isMap,
                )

    def get_ident(self, symbol: str, index=None, getArr=False, getMap=False):
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
                        if not self.is_real(index):
                            raise TypeError(
                                self.get_error_message(
                                    "Invalid index type. Should be real value"
                                )
                            )
                        return sEntry.value.get_arr(index=int(index))
                    else:
                        return sEntry.value.get_arr()
                except Exception as e:
                    raise ValueError(self.get_error_message(str(e)))
            elif getMap:
                if not isinstance(sEntry.value, MapValue):
                    raise TypeError(
                        self.get_error_message(f"Variable {symbol} is not a map")
                    )
                try:
                    if index != None:
                        return sEntry.value.get_map(index=index)
                    else:
                        return sEntry.value.get_map()
                except Exception as e:
                    raise ValueError(self.get_error_message(str(e)))
            else:
                return sEntry.value

        return None

    def remove_ident(self, symbol: str, index=None):
        sType = self.get_ident_type(symbol=symbol, getArrMap=False)
        if not sType:
            raise ValueError(
                self.get_error_message(
                    f"Variable {symbol} doesn't exist/has not yet been defined"
                )
            )

        if index != None:
            if sType == list:
                if not self.is_real(index):
                    raise TypeError(
                        self.get_error_message(
                            "Invalid index type. Should be real value"
                        )
                    )

                sArr: ArrayValue = self.get_ident(symbol=symbol)
                sArr.remove(index=int(index))
            elif sType == dict:
                sMap: MapValue = self.get_ident(symbol=symbol)
                sMap.remove(index=index)
            else:
                raise TypeError(
                    self.get_error_message(f"Variable {symbol} is not an array or map")
                )
        else:
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

    def set_class(
        self, symbol: str, constructor: FuncEntry, context, parentSymbol=None
    ):
        cEntry = ClassEntry(
            constructor=constructor,
            parentSymbol=parentSymbol,
            context=context,
        )
        self.classTable[symbol] = cEntry

    def get_class(self, symbol: str):
        cEntry: ClassEntry = self.classTable.get(symbol, None)
        if not cEntry and self.parent:
            return self.parent.get_class(symbol=symbol)
        else:
            return cEntry

    def remove_class(self, symbol: str):
        del self.classTable[symbol]

    def get_var_context(self, symbol: str):
        # when trying to retrieve class attribute object.var1.var2
        *head, tail = symbol.split(".")

        if len(head) == 1 and head[0] == "":  # .var
            if self.inClass:
                return self, tail
            else:
                raise ValueError(
                    self.get_error_message(
                        "Cannot retrieve class attribute outside class"
                    )
                )

        context = self
        for symb in head:
            if symb == "":
                continue
            cEntry = context.get_ident(symbol=symb)
            if not cEntry:
                raise ValueError(
                    self.get_error_message(
                        f"Variable {symb} doesn't exist/has not yet been defined"
                    )
                )

            if not isinstance(cEntry, ClassEntry):
                raise ValueError(
                    self.get_error_message(f"Variable {symb} is not a class object")
                )

            context = cEntry.context

        return context, tail

    def type_checker_assign(self, left: str, right, hasIndex=False):
        # check variable type matches assigned value type
        if right == None:  # when value = None 'null' keyword, allow it to be stored
            return True

        if hasIndex:
            varType = self.get_ident_type(symbol=left, getArrMap=True)
            if varType:
                return isinstance(right, varType)
            else:
                raise ValueError(
                    self.get_error_message(
                        f"Variable {left} doesn't exist/has not yet been defined"
                    )
                )
        else:
            if self.type_checker_object(symbol=left):
                return True
            else:
                varType = self.get_ident_type(symbol=left, getArrMap=False)
                if varType:
                    return isinstance(right, varType)
                else:
                    raise ValueError(
                        self.get_error_message(
                            f"Variable {left} doesn't exist/has not yet been defined"
                        )
                    )

    def type_checker_object(self, symbol: str):
        sType = None
        try:
            sType = self.get_ident_type(symbol=symbol, getArrMap=True)
        except Exception:
            return False

        return (
            self.get_ident_type(symbol=symbol, getArrMap=False) in [list, dict]
            and sType == object
        ) or self.get_ident_type(symbol=symbol, getArrMap=False) == object

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
        elif isinstance(left, (list, dict)) and isinstance(right, (list, dict)):
            raise ValueError(
                self.get_error_message("type function does not compare arrays or maps")
            )
        else:
            raise ValueError(self.get_error_message("Argument(s) have invalid types"))

    def _display_builtin_helper(self, strOut: str, isDict=False):
        if isDict:
            strOut = strOut.replace(":", " ->")

        return (
            strOut.replace("None", "null")
            .replace("True", "true")
            .replace("False", "false")
        )
