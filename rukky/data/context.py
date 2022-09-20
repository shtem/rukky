class ListValue:
    def __init__(self, type: type, lst=[]):
        self.valType = type
        self.lst = lst

    def __repr__(self):
        return self.__str__()

    def __str__(self):
        return f"ListValue({self.valType}, {repr(self.lst)})"

    def verify_value(self, value):
        return isinstance(value, self.valType)

    def add(self, value, index=None):
        if self.verify_value(value=value):
            if index:
                if index < len(self.lst):
                    self.lst[index] = value
                else:
                    raise IndexError
            else:
                self.lst.append(value)
        else:
            raise TypeError

    def get_lst(self, index=None):
        if index:
            if index < len(self.lst):
                return self.lst[index]
            else:
                raise IndexError
        else:
            return self.lst


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
    def __init__(self, returnType: type, argTypes: list, func):
        super().__init__(returnType)
        self.argTypes = argTypes
        self.func = func  # FunctionASTnode (?)

    def __repr__(self):
        return self.__str__()

    def __str__(self):
        return f"FuncEntry({self.type}, {repr(self.argTypes)})"


class TheContext:
    def __init__(self, parent):
        self.parent = parent
        self.symbolTable: dict[str, SymbolEntry] = {}
        self.funcTable: dict[str, FuncEntry] = {}

        self.returnFlag = False
        self.breakFlag = False
        self.inLoop = False
        self.inFunc = False
        self.funcReturnVal = None

        self.should_return = lambda: self.inFunc and self.returnFlag
        self.should_break = lambda: self.inLoop and self.breakFlag
    
    def reset_flags(self):
        self.returnFlag = False
        self.breakFlag = False
        self.inLoop = False
        self.inFunc = False
        self.funcReturnVal = None

    def get_ident_type(self, symbol: str, getList=False):
        sEntry: SymbolEntry = self.symbolTable.get(symbol, None)
        if not sEntry and self.parent:
            sEntry = self.parent.get_ident(symbol=symbol)

        if getList:
            sList: ListValue = sEntry.value
            return sList.valType
        else:
            return sEntry.type

    def set_ident(
        self,
        symbol: str,
        valType: type,
        value,
        index=None,
        isAppend=False,
        isList=False,
    ):
        if isList:
            sListType = type([])
            if index:
                sList: ListValue = self.get_ident(symbol=symbol)  # get list object
                sList.add(value=value, index=index)  # add value at index
                sEntryNew = SymbolEntry(type=sListType, value=sList)
                self.symbolTable[symbol] = sEntryNew
            elif isAppend:
                sList: ListValue = self.get_ident(symbol=symbol)  # get list object
                sList.add(value=value)  # append at index
                sEntryNew = SymbolEntry(type=sListType, value=sList)
                self.symbolTable[symbol] = sEntryNew
            else:
                if valType:  # new list declaration
                    sType = valType
                else:  # list re assignment
                    sType = self.get_ident_type(symbol=symbol, getList=True)
                    if not sType:
                        raise TypeError

                if not self.verify_list_type(lst=value, type=sType):
                    raise TypeError
                lstVal = ListValue(type=sType, lst=value)
                sEntry = SymbolEntry(type=sListType, value=lstVal)
                self.symbolTable[symbol] = sEntry
        else:
            if valType:  # new variable declaration
                sType = valType
            else:  # variable re assignment
                sType = self.get_ident_type(symbol=symbol)
                if not sType:
                    raise TypeError
            sEntry = SymbolEntry(type=sType, value=value)
            self.symbolTable[symbol] = sEntry

    def get_ident(self, symbol: str, index=None, getList=False):
        sEntry: SymbolEntry = self.symbolTable.get(symbol, None)
        if not sEntry and self.parent:
            sEntry = self.parent.get_ident(symbol=symbol)

        if getList:
            if index:
                return sEntry.value.get_lst(index=index)
            else:
                return sEntry.value.get_lst()
        else:
            return sEntry.value

    def remove_ident(self, symbol: str):
        del self.symbolTable[symbol]

    def set_func(self, symbol: str, returnType: type, argTypes: list, func):
        vEntry = FuncEntry(type=returnType, argTypes=argTypes, func=func)
        self.funcTable[symbol] = vEntry

    def get_func(self, symbol: str):
        vEntry: FuncEntry = self.funcTable.get(symbol, None)
        if not vEntry and self.parent:
            return self.parent.get_func(symbol)
        else:
            return vEntry

    def remove_func(self, symbol: str):
        del self.funcTable[symbol]

    def type_checker_params(self, left: list, right: list):
        # case (1) check all func param types match call param types
        return left == right

    def type_checker_assign(self, left: str, right, hasIndex=False):
        # case (2) check func/var type matches return/assigned value type
        if right == None:  # when value = None 'null' keyword, allow it to be stored
            return True

        if hasIndex:
            return self.get_ident_type(symbol=left, getList=True) == type(right)
        else:
            return self.get_ident_type(symbol=left) == type(right)

    def type_checker(self, left, right):
        # case (3) check types of lhs and rhs values match in binary operation
        if isinstance(left, (int, float)) and isinstance(right, (int, float)):
            return True

        if isinstance(left, type(None)) or isinstance(right, type(None)):
            return True

        return type(left) == type(right)

    def is_bool(self, value):
        return isinstance(value, bool)

    def is_real(self, value):
        return isinstance(value, (int, float))

    def verify_list_type(self, lst: list, type: type):
        if lst == None:
            return False  # don't want lists to be null but variables can be null
        return all(isinstance(x, type) for x in lst)

    def _type_builtin(self, left, right):
        # function for type keyword
        if not (isinstance(left, list) or isinstance(right, list)):
            return type(left) == type(right)
        else:  # doesn't get type for list
            raise ValueError
