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

    def setIdent(self, symbol: str, type: type, value, index=None):
        if index:
            sList = self.getIndex(symbol)
            sList[index] = value
            sEntryNew = SymbolEntry(type=type, value=sList)
            self.symbolTable[symbol] = sEntryNew
        else:
            sEntry = SymbolEntry(type=type, value=value)
            self.symbolTable[symbol] = sEntry

    def getIdent(self, symbol: str, index=None):
        sEntry: SymbolEntry = self.symbolTable.get(symbol, None)
        if not sEntry and self.parent:
            sEntry = self.parent.getIdent(symbol)

        if index:
            return sEntry.value[index]
        else:
            return sEntry.value

    def removeIdent(self, symbol: str):
        del self.symbolTable[symbol]

    def setFunc(
        self, symbol: str, returnType: type, argTypes: list, func: FunctionASTNode
    ):
        vEntry = FuncEntry(type=returnType, argTypes=argTypes, func=func)
        self.funcTable[symbol] = vEntry

    def getFunc(self, symbol: str):
        vEntry: FuncEntry = self.funcTable.get(symbol, None)
        if not vEntry and self.parent:
            return self.parent.getFunc(symbol)
        else:
            return vEntry

    def removeFunc(self, symbol: str):
        del self.funcTable[symbol]

    def typechecker(self, left: list, right: list):
        # case (1) check all func param types match call param types
        return left == right

    def typeChecker(self, left: Entry, right):
        # case (2) check func/var type matches return/assigned value type
        return left.type == type(right)

    def typechecker(self, left, right):
        # case (3) check types of lhs and rhs values match in binary operation
        return type(left) == type(right)

    def isCondBool(self, cond):
        return isinstance(cond, bool)

    def isValReal(self, value):
        return isinstance(value, (int, float))
