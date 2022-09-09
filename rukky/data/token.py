from common.lex_enums import TokenType


class Token:
    def __init__(self, type: TokenType, lexVal: str, lineNo: int, columnNo: int):
        self.type = type
        self.lexVal = lexVal
        self.lineNo = lineNo
        if not lexVal:
            self.columnNo = columnNo - 1
        else:
            self.columnNo = columnNo - len(lexVal)

    def __str__(self):
        # Token(TokenType.ID, 'y', lineNo=5, columnNo=10)
        return f"Token({self.type}, {repr(self.lexVal)}, lineNo={self.lineNo}, columnNo={self.columnNo})"

    def __repr__(self):
        return self.__str__()
