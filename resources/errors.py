from enum import Enum

class ErrorCode(Enum):
    ILLEGAL_CHAR     = 'Illegal Character'
    SYNTAX_ERROR     = 'Syntax Error'
    UNEXPECTED_TOKEN = 'Unexpected Token'
    SEMANTIC_ERROR   = 'Semantic Error'
    ID_NOT_FOUND     = 'Identifier Not Found'
    ID_DUPLICATE     = 'Identifier Already Exists'


class Error(Exception):
    def __init__(self, errorCode=None, token=None, message=None):
        self.errorCode = errorCode
        self.token = token
        self.message = f'{self.__class__.__name__}: {message}'


class LexerError(Error):
    pass


class ParserError(Error):
    pass


class SemanticError(Error):
    pass