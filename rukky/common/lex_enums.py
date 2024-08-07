from enum import Enum


class TokenType(Enum):
    PLUS = "+"
    MINUS = "-"
    MUL = "*"
    FLOAT_DIV = "/"
    INT_DIV = "//"
    MOD = "%"
    POW = "^"
    OR = "||"
    AND = "&&"
    NOT = "~"
    IN = "?"
    GT = ">"
    GE = ">="
    LT = "<"
    LE = "<="
    EQ = "=="
    NE = "<>"
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
    INDEX = "@"
    MAP_LINK = "->"

    EOL = "\n"
    EOF = None

    VOID = "void"
    REAL = "real"
    BOOL = "bool"
    STRING = "str"
    OBJECT = "obj"

    TRUE = "true"
    FALSE = "false"
    NULL = "null"

    FOR = "for"
    WHILE = "while"
    GIVE = "give"
    IF = "if"
    ELIF = "elif"
    ELSE = "else"

    DISPLAY = "display"
    LENGTH = "len"
    TYPE = "type"
    STRINGIFY = "getStr"
    REALIFY = "getReal"
    MAX = "max"
    MIN = "min"
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

    CLASS = "class"
    SUPER = "super"
    DELETE = "del"

    RETURN = "return"
    BREAK = "break"
    CONTINUE = "continue"

    ID = -1  # [a-zA-Z_][a-zA-Z_0-9]*
    REAL_LIT = -2  # [0-9]+ | ([0-9]*.[0-9]+)
    STRING_LIT = -3  # ".*"
    BOOL_LIT = TRUE, FALSE  # true or false


def _build_reserved_keywords():
    tokenList = list(TokenType)
    start = tokenList.index(TokenType.VOID)
    end = tokenList.index(TokenType.CONTINUE)
    reserved = {tkType.value: tkType for tkType in tokenList[start : end + 1]}

    return reserved


RESERVED_KEYWORDS = _build_reserved_keywords()
