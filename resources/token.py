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
    LT            = '<'
    GT            = '>'
    LE            = '<='
    GE            = '>='
    EQ            = '<>'
    NE            = '</>'
    APPEND        = '<<'

    LBRACE        = '{'
    RBRACE        = '}'
    LPAREN        = '('
    RPAREN        = ')'
    COLON         = ':'
    RES_COLON     = '::'
    COMMA         = ','
    COMMENT       = '$'

    ASSIGN        = ':='
    EOL           = '\r\n'
    EOF           = None

    VOID          = 'void'
    REAL          = 'real'
    BOOL          = 'bool'
    STRING        = 'string'
    FOR           = 'for'
    WHILE         = 'while'
    IF            = 'if'
    ELIF          = 'elif'
    ELSE          = 'else'
    RETURN        = 'return'
    BREAK         = 'break'

    ID            = -1 # [a-zA-Z_][a-zA-Z_0-9]*
    REAL_LIT      = -2 # [0-9]+ | ([0-9]*.[0-9]+)
    BOOL_LIT      = -3 # true or false
    STRING_LIT    = -4 # ".*"

def _build_reserved_keywords():
    tt_list = list(TokenType)
    start_index = tt_list.index(TokenType.VOID)
    end_index = tt_list.index(TokenType.BREAK)
    reserved_keywords = tt_list[start_index:end_index + 1]

    return reserved_keywords

RESERVED_KEYWORDS = _build_reserved_keywords()

class Token:
    def __init__(self, type, lexVal, lineNo=None, columnNo=None):
        self.type = type
        self.lexVal = lexVal
        self.lineNo = lineNo
        self.columnNo = columnNo

    def __str__(self):
        # Token(TokenType.ID, 7, lineNo=5, columnNo=10)
        return f"Token({self.type}, {repr(self.lexVal)}, lineNo={self.lineNo}, columnNo={self.columnNo}"

    def __repr__(self):
        return self.__str__()



