from enum import Enum

class TokenType(Enum):
    PLUS          = '+'
    MINUS         = '-'
    MUL           = '*'
    FLOAT_DIV     = '/'
    INT_DIV       = '//'
    MOD           = '%'
    EXP           = '^'
    OR            = '||'
    AND           = '&&'
    NOT           = '~'
    GT            = '>'
    GE            = '>='
    LT            = '<'
    LE            = '<='
    EQ            = '<>'
    NE            = '<!'
    APPEND        = '<<'

    LBRACE        = '{'
    RBRACE        = '}'
    LPAREN        = '('
    RPAREN        = ')'
    LSQUARE       = '['
    RSQUARE       = ']'
    COLON         = ':'
    RES_COLON     = '::'
    COMMA         = ','
    ASSIGN        = ':='
    
    EOL           = '\n'
    EOF           = None

    VOID          = 'void'
    REAL          = 'real'
    BOOL          = 'bool'
    STRING        = 'string'
    TRUE          = 'true'
    FALSE         = 'false'
    FOR           = 'for'
    WHILE         = 'while'
    IF            = 'if'
    ELIF          = 'elif'
    ELSE          = 'else'
    RETURN        = 'return'
    BREAK         = 'break'

    ID            = -1 # [a-zA-Z_][a-zA-Z_0-9]*
    REAL_LIT      = -2 # [0-9]+ | ([0-9]*.[0-9]+)
    STRING_LIT    = -3 # ".*"
    BOOL_LIT      = TRUE,FALSE # true or false

def _build_reserved_keywords():
    tokenList = list(TokenType)
    start = tokenList.index(TokenType.VOID)
    end = tokenList.index(TokenType.BREAK)
    reserved = {
        tkType.value: tkType
        for tkType in tokenList[start:end + 1]
    }

    return reserved

RESERVED_KEYWORDS = _build_reserved_keywords()

class Token:
    def __init__(self, type, lexVal, lineNo=None, columnNo=None):
        self.type = type
        self.lexVal = lexVal
        self.lineNo = lineNo
        self.columnNo = columnNo - len(lexVal) - 1

    def __str__(self):
        # Token(TokenType.ID, 7, lineNo=5, columnNo=10)
        return f'Token({self.type}, {repr(self.lexVal)}, lineNo={self.lineNo}, columnNo={self.columnNo}'

    def __repr__(self):
        return self.__str__()



