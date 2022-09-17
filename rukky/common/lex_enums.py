from enum import Enum


class TokenType(Enum):
    PLUS = "+"
    MINUS = "-"
    MUL = "*"
    FLOAT_DIV = "/"
    INT_DIV = "//"
    MOD = "%"
    EXP = "^"
    OR = "||"
    AND = "&&"
    NOT = "~"
    GT = ">"
    GE = ">="
    LT = "<"
    LE = "<="
    EQ = "<>"
    NE = "<!"
    APPEND = "<<"

    LBRACE = "{"
    RBRACE = "}"
    LPAREN = "("
    RPAREN = ")"
    LSQUARE = "["
    RSQUARE = "]"
    COLON = ":"
    SEM_COLON = ";"
    RES_COLON = "::"
    COMMA = ","
    ASSIGN = ":="
    LIST_ASSIGN = "@"

    EOL = "\n"
    EOF = None

    VOID = "void"
    REAL = "real"
    BOOL = "bool"
    STRING = "str"
    TRUE = "true"
    FALSE = "false"
    NULL = "null"
    FOR = "for"
    WHILE = "while"
    IF = "if"
    ELIF = "elif"
    ELSE = "else"

    DISPLAY = "display"
    LENGTH = "len"
    TYPE = "type"
    STRINGIFY = "getStr"
    REALIFY = "getReal"
    RANDOM = "rand"
    FLOOR = "floor"
    CEIL = "ceil"
    SQRT = "sqrt"
    LOG = "log"
    SIN = "sin"
    COS = "cos"
    TAN = "tan"
    PI = "pi"
    EULER = "eul"

    RETURN = "return"
    BREAK = "break"

    ID = -1  # [a-zA-Z_][a-zA-Z_0-9]*
    REAL_LIT = -2  # [0-9]+ | ([0-9]*.[0-9]+)
    STRING_LIT = -3  # ".*"
    BOOL_LIT = TRUE, FALSE  # true or false


def _build_reserved_keywords():
    tokenList = list(TokenType)
    start = tokenList.index(TokenType.VOID)
    end = tokenList.index(TokenType.BREAK)
    reserved = {tkType.value: tkType for tkType in tokenList[start : end + 1]}

    return reserved


RESERVED_KEYWORDS = _build_reserved_keywords()
