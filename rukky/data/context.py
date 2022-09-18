from data.ast import FunctionASTNode


class Entry:
    def __init__(self, type: type):
        self.type = type


class SymbolEntry(Entry):
    def __init__(self, type: type, value):
        super().__init__(type)
        self.value = value


class FuncEntry(Entry):
    def __init__(self, returnType: type, argTypes: list, func: FunctionASTNode):
        super().__init__(returnType)
        self.argTypes = argTypes
        self.func = func


class TheContext:
    def __init__(self, parent):
        self.parent = parent
        self.symbolTable: dict[str, SymbolEntry] = {}
        self.funcTable: dict[str, FuncEntry] = {}

    def set_ident(self, symbol: str, type: type, value, index=None):
        if index:
            sList = self.get_ident(symbol) # get list
            sListType = self.get_ident_type(symbol) # get list type
            if not sListType:
                raise TypeError
            sList[index] = value # change value at index to new value
            sEntryNew = SymbolEntry(type=sListType, value=sList)
            self.symbolTable[symbol] = sEntryNew
        else:
            if type: # new variable declaration
                sType = type 
            else: # variable re assignment
                sType = self.get_ident_type(symbol)
                if not sType:
                    raise TypeError
            sEntry = SymbolEntry(type=sType, value=value)
            self.symbolTable[symbol] = sEntry
    
    def get_ident_type(self, symbol: str):
        sEntry: SymbolEntry = self.symbolTable.get(symbol, None)
        if not sEntry and self.parent:
            sEntry = self.parent.get_ident(symbol)
        
        return sEntry.type

    def get_ident(self, symbol: str, index=None):
        sEntry: SymbolEntry = self.symbolTable.get(symbol, None)
        if not sEntry and self.parent:
            sEntry = self.parent.get_ident(symbol)

        if index:
            return sEntry.value[index]
        else:
            return sEntry.value

    def remove_ident(self, symbol: str):
        del self.symbolTable[symbol]

    def set_func(
        self, symbol: str, returnType: type, argTypes: list, func: FunctionASTNode
    ):
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

    def type_checker(self, left: list, right: list):
        # case (1) check all func param types match call param types
        return left == right

    def type_checker(self, left: Entry, right):
        # case (2) check func/var type matches return/assigned value type
        return left.type == type(right)

    def type_checker(self, left, right):
        # case (3) check types of lhs and rhs values match in binary operation
        return type(left) == type(right)

    def is_bool(self, value):
        return isinstance(value, bool)

    def is_real(self, value):
        return isinstance(value, (int, float))
