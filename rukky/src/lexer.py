from common.lex_enums import TokenType, RESERVED_KEYWORDS
from common.errors import LexerError
from data.token import Token
import sys

class Lexer:
    def __init__(self, text):
        self.text = text
        self.idx = 0
        self.currChar = self.text[self.idx]
        self.lineNo = 1
        self.columnNo = 1

    def error(self):
        print(LexerError(
            message=f'Illegal Character "{self.currChar}" on line: {self.lineNo} column: {self.columnNo}'
        ))
        sys.exit(0)
    
    def advance(self):
        if self.currChar == '\n':
            self.lineNo += 1
            self.columnNo = 1

        self.idx += 1
        if self.idx > len(self.text) - 1:
            self.currChar = None 
            self.columnNo += 1
        else:
            self.currChar = self.text[self.idx]
            self.columnNo += 1

    def peek(self):
        peekIdx = self.idx + 1
        if peekIdx > len(self.text) - 1:
            return None
        else:
            return self.text[peekIdx]
    
    def _make_identifier(self):
        idValue = ''

        while self.currChar != None and (self.currChar.isalnum() or self.currChar == '_'):
            idValue += self.currChar
            self.advance()

        matchType = RESERVED_KEYWORDS.get(idValue)
        tokType = matchType if matchType != None else TokenType.ID

        if tokType in [TokenType.TRUE, TokenType.FALSE]:
            tokType = TokenType.BOOL_LIT
        
        return Token(type=tokType, lexVal=idValue, lineNo=self.lineNo, columnNo=self.columnNo)
    
    def _make_real(self):
        realValue = ''

        while self.currChar != None and (self.currChar.isdigit() or self.currChar == '.'):
            realValue += self.currChar
            self.advance() 
        
        return Token(type=TokenType.REAL_LIT, lexVal=realValue, lineNo=self.lineNo, columnNo=self.columnNo)
    
    def _make_string(self):
        strValue = ''
        self.advance() # for first opening quote '"'

        while self.currChar != None and self.currChar != '"':
            strValue += self.currChar
            self.advance() 
        
        self.advance() # for last opening quote '"'

        return Token(type=TokenType.STRING_LIT, lexVal=strValue, lineNo=self.lineNo, columnNo=self.columnNo)

    def _make_div(self):
        tokType = TokenType.FLOAT_DIV # /
        self.advance()

        if self.currChar == '/':
            self.advance()
            tokType = TokenType.INT_DIV # //
        
        return Token(type=tokType, lexVal=tokType.value, lineNo=self.lineNo, columnNo=self.columnNo)
    
    def _make_colon_or_assign(self):
        tokType = TokenType.COLON # :
        self.advance()

        if self.currChar == ':':
            self.advance()
            tokType = TokenType.RES_COLON # ::
        if self.currChar == '=':
            self.advance()
            tokType = TokenType.ASSIGN # :=
        
        return Token(type=tokType, lexVal=tokType.value, lineNo=self.lineNo, columnNo=self.columnNo)
    
    def _make_greater_than(self):
        tokType = TokenType.GT # >
        self.advance()

        if self.currChar == '=':
            self.advance()
            tokType = TokenType.GE # >=
        
        return Token(type=tokType, lexVal=tokType.value, lineNo=self.lineNo, columnNo=self.columnNo)

    def _make_other_comparison_op(self):
        tokType = TokenType.LT # <
        self.advance()

        if self.currChar == '=': # <=
            self.advance()
            tokType = TokenType.LE
        if self.currChar == '>': # <>
            self.advance()
            tokType = TokenType.EQ
        if self.currChar == '!': # <!
            self.advance()
            tokType = TokenType.NE
        if self.currChar == '<': # <<
            self.advance()
            tokType = TokenType.APPEND
        
        return Token(type=tokType, lexVal=tokType.value, lineNo=self.lineNo, columnNo=self.columnNo)
    
    def skip_comment(self):
        self.advance() # for first opening quote '$'

        while self.currChar != '$':
            self.advance() 
        
        self.advance() # for last opening quote '$'

    def get_next_token(self):
        
        while self.currChar != None:
            if self.currChar in [' ', '\t']:
                self.advance()
            elif self.currChar == '$':
                self.skip_comment()
            elif self.currChar == '\n':
                self.advance()
                return Token(type=TokenType.EOL, lexVal=TokenType.EOL.value, lineNo=self.lineNo, columnNo=self.columnNo)
            elif self.currChar.isalpha():
                return self._make_identifier()
            elif self.currChar.isdigit():
                return self._make_real()
            elif self.currChar == '"':
                return self._make_string()
            elif self.currChar == '+':
                self.advance()
                return Token(type=TokenType.PLUS, lexVal=TokenType.PLUS.value, lineNo=self.lineNo, columnNo=self.columnNo)
            elif self.currChar == '-':
                self.advance()
                return Token(type=TokenType.MINUS, lexVal=TokenType.MINUS.value, lineNo=self.lineNo, columnNo=self.columnNo)
            elif self.currChar == '*':
                self.advance()
                return Token(type=TokenType.MUL, lexVal=TokenType.MUL.value, lineNo=self.lineNo, columnNo=self.columnNo)
            elif self.currChar == '%':
                self.advance()
                return Token(type=TokenType.MOD, lexVal=TokenType.MOD.value, lineNo=self.lineNo, columnNo=self.columnNo)
            elif self.currChar == '^':
                self.advance()
                return Token(type=TokenType.EXP, lexVal=TokenType.EXP.value, lineNo=self.lineNo, columnNo=self.columnNo)
            elif self.currChar == '/':
                return self._make_div()
            elif self.currChar == '~':
                self.advance()
                return Token(type=TokenType.NOT, lexVal=TokenType.NOT.value, lineNo=self.lineNo, columnNo=self.columnNo)
            elif self.currChar == '|' and self.peek() == '|':
                self.advance()
                self.advance()
                return Token(type=TokenType.OR, lexVal=TokenType.OR.value, lineNo=self.lineNo, columnNo=self.columnNo)
            elif self.currChar == '&' and self.peek() == '&':
                self.advance()
                self.advance()
                return Token(type=TokenType.AND, lexVal=TokenType.AND.value, lineNo=self.lineNo, columnNo=self.columnNo)
            elif self.currChar == '>':
                return self._make_greater_than()
            elif self.currChar == '<':
                return self._make_other_comparison_op()
            elif self.currChar == '{':
                self.advance()
                return Token(type=TokenType.LBRACE, lexVal=TokenType.LBRACE.value, lineNo=self.lineNo, columnNo=self.columnNo)
            elif self.currChar == '}':
                self.advance()
                return Token(type=TokenType.RBRACE, lexVal=TokenType.RBRACE.value, lineNo=self.lineNo, columnNo=self.columnNo)
            elif self.currChar == '(':
                self.advance()
                return Token(type=TokenType.LPAREN, lexVal=TokenType.LPAREN.value, lineNo=self.lineNo, columnNo=self.columnNo)
            elif self.currChar == ')':
                self.advance()
                return Token(type=TokenType.RPAREN, lexVal=TokenType.RPAREN.value, lineNo=self.lineNo, columnNo=self.columnNo)
            elif self.currChar == '[':
                self.advance()
                return Token(type=TokenType.LSQUARE, lexVal=TokenType.LSQUARE.value, lineNo=self.lineNo, columnNo=self.columnNo)
            elif self.currChar == ']':
                self.advance()
                return Token(type=TokenType.RSQUARE, lexVal=TokenType.RSQUARE.value, lineNo=self.lineNo, columnNo=self.columnNo)
            elif self.currChar == ',':
                self.advance()
                return Token(type=TokenType.COMMA, lexVal=TokenType.COMMA.value, lineNo=self.lineNo, columnNo=self.columnNo)
            elif self.currChar == ':':
                return self._make_colon_or_assign()
            else:
                self.error()
        
        return Token(type=TokenType.EOF, lexVal=TokenType.EOF.value, lineNo=self.lineNo, columnNo=self.columnNo)


