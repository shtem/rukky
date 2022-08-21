class Token:
    def __init__(self, type, lexVal, lineNo=None, columnNo=None):
        self.type = type
        self.lexVal = lexVal
        self.lineNo = lineNo
        if lexVal == None:
            self.columnNo = columnNo - 1
        else:
            self.columnNo = columnNo - len(lexVal)

    def __str__(self):
        # Token(TokenType.ID, 7, lineNo=5, columnNo=10)
        return f"Token({self.type}, {repr(self.lexVal)}, lineNo={self.lineNo}, columnNo={self.columnNo})"

    def __repr__(self):
        return self.__str__()
