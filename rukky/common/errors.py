from enum import Enum


class ErrorCode(Enum):
    LEXER_ERROR = "Lexer Error"
    SYNTAX_ERROR = "Syntax Error"
    RUNTIME_ERROR = "Runtime Error"


class Error:
    def __init__(self, errorCode: ErrorCode, message: str):
        self.errorCode = errorCode
        self.message = f"{self.errorCode.value}: {message}"

    def __str__(self):
        return self.message

    def __repr__(self):
        return self.__str__()


class LexerError(Error):
    def __init__(self, message: str):
        super().__init__(ErrorCode.LEXER_ERROR, message)


class ParserError(Error):
    def __init__(self, message: str):
        super().__init__(ErrorCode.SYNTAX_ERROR, message)


class SemanticError(Error):
    def __init__(self, message: str):
        super().__init__(ErrorCode.RUNTIME_ERROR, message)
