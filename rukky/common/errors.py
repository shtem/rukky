from enum import Enum

class ErrorCode(Enum):
    ILLEGAL_CHAR     = 'Lexer Error'
    SYNTAX_ERROR     = 'Syntax Error'
    RUNTIME_ERROR    = 'Runtime Error'

class Error:
    def __init__(self, errorCode=None, token=None, message=None):
        self.errorCode = errorCode
        self.token = token
        self.message = f'{self.errorCode.value}: {message}'
    
    def __str__(self):
        return self.message

    def __repr__(self):
        return self.__str__()


class LexerError(Error):
    def __init__(self, message=None):
        super().__init__(ErrorCode.ILLEGAL_CHAR, None, message)


class ParserError(Error):
    def __init__(self, token=None, message=None):
        super().__init__(ErrorCode.SYNTAX_ERROR, token, message)


class SemanticError(Error):
    def __init__(self, token=None, message=None):
        super().__init__(ErrorCode.RUNTIME_ERROR, token, message)