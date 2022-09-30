from common.lex_enums import TokenType, RESERVED_KEYWORDS
from common.errors import LexerError
from data.token import Token
import sys


class Lexer:
    def __init__(self, text: str):
        self.text = text
        self.idx = 0
        self.currChar = self.text[self.idx]
        self.lineNo = 1
        self.columnNo = 1

    def error(self):
        print(
            LexerError(
                message=f'Illegal or Missing Character "{str() if not self.currChar else self.currChar}" on line: {self.lineNo} column: {self.columnNo}'
            )
        )
        sys.exit(0)

    def reset(self):
        self.idx = 0
        self.currChar = self.text[self.idx]
        self.lineNo = 1
        self.columnNo = 1

    def advance(self):
        if self.currChar == "\n":
            self.lineNo += 1
            self.columnNo = 0

        self.idx += 1
        if self.idx > len(self.text) - 1:
            self.currChar = None
            self.columnNo += 1
        else:
            self.currChar = self.text[self.idx]
            self.columnNo += 1

    def next_char(self):
        peekIdx = self.idx + 1
        if peekIdx > len(self.text) - 1:
            return None
        else:
            return self.text[peekIdx]

    def _make_identifier(self):
        idValue = ""

        while self.currChar and (self.currChar.isalnum() or self.currChar == "_"):
            idValue += self.currChar
            self.advance()

        matchType = RESERVED_KEYWORDS.get(idValue)
        tokType = matchType if matchType else TokenType.ID

        if tokType in [TokenType.TRUE, TokenType.FALSE]:
            tokType = TokenType.BOOL_LIT

        return Token(
            type=tokType, lexVal=idValue, lineNo=self.lineNo, columnNo=self.columnNo
        )

    def _make_real(self):
        realValue = ""
        dotCount = 0

        while self.currChar and (self.currChar.isdigit() or self.currChar == "."):
            if self.currChar == ".":
                if dotCount == 1:
                    break
                dotCount += 1

            realValue += self.currChar
            self.advance()

        return Token(
            type=TokenType.REAL_LIT,
            lexVal=realValue,
            lineNo=self.lineNo,
            columnNo=self.columnNo,
        )

    def _make_string(self):
        strValue = ""
        self.advance()  # for first opening quote '"'

        while self.currChar and self.currChar != '"':
            strValue += self.currChar
            self.advance()

        if self.currChar == '"':
            self.advance()  # for last opening quote '"'
        else:
            self.error()

        return Token(
            type=TokenType.STRING_LIT,
            lexVal=strValue,
            lineNo=self.lineNo,
            columnNo=self.columnNo
            - 2,  # want to take into account opening and closing quote in columnNo
        )

    def _make_div(self):
        tokType = TokenType.FLOAT_DIV  # /
        self.advance()

        if self.currChar == "/":
            self.advance()
            tokType = TokenType.INT_DIV  # //

        return Token(
            type=tokType,
            lexVal=tokType.value,
            lineNo=self.lineNo,
            columnNo=self.columnNo,
        )

    def _make_minus_or_arrow(self):
        tokType = TokenType.MINUS  # -
        self.advance()

        if self.currChar == ">":
            self.advance()
            tokType = TokenType.MAP_ASSIGN  # ->

        return Token(
            type=tokType,
            lexVal=tokType.value,
            lineNo=self.lineNo,
            columnNo=self.columnNo,
        )

    def _make_colon_or_assign(self):
        tokType = TokenType.COLON  # :
        self.advance()

        if self.currChar == ":":
            self.advance()
            tokType = TokenType.RES_COLON  # ::
        if self.currChar == "=":
            self.advance()
            tokType = TokenType.ASSIGN  # :=

        return Token(
            type=tokType,
            lexVal=tokType.value,
            lineNo=self.lineNo,
            columnNo=self.columnNo,
        )

    def _make_greater_than(self):
        tokType = TokenType.GT  # >
        self.advance()

        if self.currChar == "=":
            self.advance()
            tokType = TokenType.GE  # >=

        return Token(
            type=tokType,
            lexVal=tokType.value,
            lineNo=self.lineNo,
            columnNo=self.columnNo,
        )

    def _make_other_comparison_op(self):
        tokType = TokenType.LT  # <
        self.advance()

        if self.currChar == "=":  # <=
            self.advance()
            tokType = TokenType.LE
        if self.currChar == ">":  # <>
            self.advance()
            tokType = TokenType.EQ
        if self.currChar == "!":  # <!
            self.advance()
            tokType = TokenType.NE
        if self.currChar == "<":  # <<
            self.advance()
            tokType = TokenType.APPEND

        return Token(
            type=tokType,
            lexVal=tokType.value,
            lineNo=self.lineNo,
            columnNo=self.columnNo,
        )

    def _skip_comment(self):
        self.advance()  # for first '$'

        while self.currChar and self.currChar != "$":
            self.advance()

        if self.currChar == "$":
            self.advance()  # for last'$'
        else:
            self.error()

    def get_next_token(self):
        while self.currChar:
            if self.currChar in [" ", "\t"]:
                self.advance()
            elif self.currChar == "$":
                self._skip_comment()
            elif self.currChar == "\n":
                self.advance()
                return Token(
                    type=TokenType.EOL,
                    lexVal=TokenType.EOL.value,
                    lineNo=self.lineNo,
                    columnNo=self.columnNo,
                )
            elif self.currChar.isalpha():
                return self._make_identifier()
            elif self.currChar.isdigit():
                return self._make_real()
            elif self.currChar == '"':
                return self._make_string()
            elif self.currChar == "+":
                self.advance()
                return Token(
                    type=TokenType.PLUS,
                    lexVal=TokenType.PLUS.value,
                    lineNo=self.lineNo,
                    columnNo=self.columnNo,
                )
            elif self.currChar == "-":
                return self._make_minus_or_arrow()
            elif self.currChar == "*":
                self.advance()
                return Token(
                    type=TokenType.MUL,
                    lexVal=TokenType.MUL.value,
                    lineNo=self.lineNo,
                    columnNo=self.columnNo,
                )
            elif self.currChar == "%":
                self.advance()
                return Token(
                    type=TokenType.MOD,
                    lexVal=TokenType.MOD.value,
                    lineNo=self.lineNo,
                    columnNo=self.columnNo,
                )
            elif self.currChar == "^":
                self.advance()
                return Token(
                    type=TokenType.POW,
                    lexVal=TokenType.POW.value,
                    lineNo=self.lineNo,
                    columnNo=self.columnNo,
                )
            elif self.currChar == "/":
                return self._make_div()
            elif self.currChar == "~":
                self.advance()
                return Token(
                    type=TokenType.NOT,
                    lexVal=TokenType.NOT.value,
                    lineNo=self.lineNo,
                    columnNo=self.columnNo,
                )
            elif self.currChar == "|" and self.next_char() == "|":
                self.advance()
                self.advance()
                return Token(
                    type=TokenType.OR,
                    lexVal=TokenType.OR.value,
                    lineNo=self.lineNo,
                    columnNo=self.columnNo,
                )
            elif self.currChar == "&" and self.next_char() == "&":
                self.advance()
                self.advance()
                return Token(
                    type=TokenType.AND,
                    lexVal=TokenType.AND.value,
                    lineNo=self.lineNo,
                    columnNo=self.columnNo,
                )
            elif self.currChar == ">":
                return self._make_greater_than()
            elif self.currChar == "<":
                return self._make_other_comparison_op()
            elif self.currChar == "{":
                self.advance()
                return Token(
                    type=TokenType.LBRACE,
                    lexVal=TokenType.LBRACE.value,
                    lineNo=self.lineNo,
                    columnNo=self.columnNo,
                )
            elif self.currChar == "}":
                self.advance()
                return Token(
                    type=TokenType.RBRACE,
                    lexVal=TokenType.RBRACE.value,
                    lineNo=self.lineNo,
                    columnNo=self.columnNo,
                )
            elif self.currChar == "(":
                self.advance()
                return Token(
                    type=TokenType.LPAREN,
                    lexVal=TokenType.LPAREN.value,
                    lineNo=self.lineNo,
                    columnNo=self.columnNo,
                )
            elif self.currChar == ")":
                self.advance()
                return Token(
                    type=TokenType.RPAREN,
                    lexVal=TokenType.RPAREN.value,
                    lineNo=self.lineNo,
                    columnNo=self.columnNo,
                )
            elif self.currChar == "[":
                self.advance()
                return Token(
                    type=TokenType.LSQUARE,
                    lexVal=TokenType.LSQUARE.value,
                    lineNo=self.lineNo,
                    columnNo=self.columnNo,
                )
            elif self.currChar == "]":
                self.advance()
                return Token(
                    type=TokenType.RSQUARE,
                    lexVal=TokenType.RSQUARE.value,
                    lineNo=self.lineNo,
                    columnNo=self.columnNo,
                )
            elif self.currChar == ",":
                self.advance()
                return Token(
                    type=TokenType.COMMA,
                    lexVal=TokenType.COMMA.value,
                    lineNo=self.lineNo,
                    columnNo=self.columnNo,
                )
            elif self.currChar == "@":
                self.advance()
                return Token(
                    type=TokenType.LIST_ASSIGN,
                    lexVal=TokenType.LIST_ASSIGN.value,
                    lineNo=self.lineNo,
                    columnNo=self.columnNo,
                )
            elif self.currChar == ";":
                self.advance()
                return Token(
                    type=TokenType.SEM_COLON,
                    lexVal=TokenType.SEM_COLON.value,
                    lineNo=self.lineNo,
                    columnNo=self.columnNo,
                )
            elif self.currChar == ":":
                return self._make_colon_or_assign()
            else:
                self.error()

        return Token(
            type=TokenType.EOF,
            lexVal=TokenType.EOF.value,
            lineNo=self.lineNo,
            columnNo=self.columnNo,
        )

    def peek_next_token(self):
        idxCopy = self.idx
        currCharCopy = self.currChar
        lineNoCopy = self.lineNo
        columnNoCopy = self.columnNo

        token = self.get_next_token()

        self.idx = idxCopy
        self.currChar = currCharCopy
        self.lineNo = lineNoCopy
        self.columnNo = columnNoCopy

        return token
