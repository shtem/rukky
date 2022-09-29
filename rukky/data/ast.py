from common.lex_enums import TokenType
from data.context import TheContext, FuncEntry
from data.token import Token
from abc import ABC, abstractmethod


class ASTNode(ABC):
    def __init__(self):
        self.level = 0

    def update_level(self):
        self.level = self.level + 1

    def __repr__(self):
        self.update_level()
        return self.__str__()

    @abstractmethod
    def __str__(self):
        pass

    @abstractmethod
    def code_gen(self):
        pass


class StmtASTNode(ASTNode):
    pass


class ExprASTNode(StmtASTNode):
    pass


class RealASTNode(ExprASTNode):
    def __init__(self, token: Token, value: str):
        super().__init__()
        self.token = token
        self.value = float(value)

    def __str__(self):
        return f"-> RealASTNode (lineNo={self.token.lineNo}, columnNo={self.token.columnNo}) {self.value}"

    def code_gen(self, context: TheContext):
        context.update_line_col(lineNo=self.token.lineNo, columnNo=self.token.columnNo)

        return self.value


class BoolASTNode(ExprASTNode):
    def __init__(self, token: Token, value: int):
        super().__init__()
        self.token = token
        self.value = bool(value)

    def __str__(self):
        return f"-> BoolASTNode (lineNo={self.token.lineNo}, columnNo={self.token.columnNo}) {self.value}"

    def code_gen(self, context: TheContext):
        context.update_line_col(lineNo=self.token.lineNo, columnNo=self.token.columnNo)

        return self.value


class StringASTNode(ExprASTNode):
    def __init__(self, token: Token, value: str):
        super().__init__()
        self.token = token
        self.value = str(value)

    def __str__(self):
        return f"-> StringASTNode (lineNo={self.token.lineNo}, columnNo={self.token.columnNo}) {self.value}"

    def code_gen(self, context: TheContext):
        context.update_line_col(lineNo=self.token.lineNo, columnNo=self.token.columnNo)

        return self.value


class IdentifierASTNode(ExprASTNode):
    def __init__(
        self, token: Token, type: str, ident: str, index: ExprASTNode, arrFlag: bool
    ):
        super().__init__()
        self.token = token
        self.type = type
        self.ident = ident
        self.index = index
        self.arrFlag = arrFlag

    def __str__(self):
        out = f"-> IdentifierASTNode (lineNo={self.token.lineNo}, columnNo={self.token.columnNo}) {self.ident} {self.type if self.type else ''}{list() if self.arrFlag else ''} "
        if self.index:
            self.index.level = self.level + 3
            out += f"\n{' ' * (self.level)}@-¬"
            out += f"\n{' ' * (self.level + 3)}-{repr(self.index)}"

        return out

    def get_ident(self):
        return self.ident

    def get_type(self):
        return self.type

    def set_index(self, i):
        self.index = i

    def set_arr_flag(self, flag):
        self.arrFlag = flag

    def is_arr(self):
        return self.arrFlag

    def determine_type_value(self):
        if self.type == "real":
            return type(0.0), 0.0
        elif self.type == "bool":
            return type(False), False
        elif self.type == "str":
            return type(""), ""
        elif self.type == "void":  # only applies to functions
            return type(None), None
        elif self.type == "obj":
            return object, None
        else:
            raise TypeError

    def code_gen(self, context: TheContext):
        context.update_line_col(lineNo=self.token.lineNo, columnNo=self.token.columnNo)

        symbol = self.ident
        if (
            self.type
        ):  # variable declaration: type != None e.g. var_type ID .. or var_type[] ID ..
            type, defaultValue = self.determine_type_value()

            if self.arrFlag:
                defaultValue = []

            context.set_ident(
                symbol=symbol,
                valType=type,
                value=defaultValue,
                index=None,
                isAppend=False,
                isArr=self.arrFlag,
            )

            return defaultValue
        else:  # variable retrieval: type = None e.g. ID or ID[expr]
            valType = context.get_ident_type(symbol=symbol, getArr=False)
            if not valType:
                raise ValueError(
                    context.get_error_message(
                        f"Variable {symbol} doesn't exist/has not yet been defined"
                    )
                )  # variable does not exist

            if self.index:
                indexVal = self.index.code_gen(context=context)
                if context.is_real(indexVal):
                    varValue = context.get_ident(
                        symbol=symbol, index=int(indexVal), getArr=self.arrFlag
                    )
                else:
                    raise TypeError(
                        context.get_error_message(
                            "Invalid index type. Should be real value"
                        )
                    )
            else:
                isArr = valType == list if valType else False
                varValue = context.get_ident(symbol=symbol, index=None, getArr=isArr)

            return varValue


class ReservedKeyWordASTNode(IdentifierASTNode):
    def __init__(
        self,
        token: Token,
        ident: str,
        value,
        isFunc=False,
        returnType=None,
        argNum=0,
        argType=None,
    ):
        super().__init__(token=token, type=None, ident=ident, index=None, arrFlag=False)
        self.value = value
        self.isFunc = isFunc
        self.returnType = returnType
        self.argNum = argNum
        self.argType = argType

    def __str__(self):
        return f"-> ReservedKeyWordASTNode (lineNo={self.token.lineNo}, columnNo={self.token.columnNo}) {self.ident} {self.value}"

    def generate_builtin(self, context: TheContext):
        symbol = self.get_ident()
        isReturnArr = False
        returnType = self.returnType

        argSymbols = []

        funcContext = TheContext(parent=context)
        funcContext.inFunc = True

        for par in range(self.argNum):
            argSymbols.append(str(par))  # store parameter variable names
            # add parameters as variables to function context, don't care about arr content type for display and stringify
            funcContext.set_ident(
                symbol=str(par),
                valType=self.argType if self.argType != list else object,
                value=None,
                index=None,
                isAppend=False,
                isArr=False,
            )

        context.set_func(
            symbol=symbol,
            returnType=returnType,
            argSymbols=argSymbols,
            funcBody=context._type_builtin
            if symbol == TokenType.TYPE.value
            else self.value,
            context=funcContext,
            isReturnArr=isReturnArr,
        )

    def code_gen(self, context: TheContext):
        context.update_line_col(lineNo=self.token.lineNo, columnNo=self.token.columnNo)

        if self.isFunc:
            self.generate_builtin(context=context)
            return None
        else:
            return self.value


class UnaryExprASTNode(ExprASTNode):
    def __init__(self, op: Token, rhs: ExprASTNode):
        super().__init__()
        self.op = op
        self.rhs = rhs

    def __str__(self):
        self.rhs.level = self.level
        return f"-> UnaryExprASTNode (lineNo={self.op.lineNo}, columnNo={self.op.columnNo}) {self.op.lexVal}\n{' ' * (self.level)}-{repr(self.rhs)}"

    def code_gen(self, context: TheContext):
        context.update_line_col(lineNo=self.op.lineNo, columnNo=self.op.columnNo)

        rVal = self.rhs.code_gen(context=context)

        if rVal == None:
            raise ValueError(context.get_error_message("Unexpected null value"))

        match self.op.type:
            case TokenType.MINUS:
                if context.is_real(value=rVal):
                    return -rVal
                else:
                    raise TypeError(
                        context.get_error_message(
                            "Incompatible types for unary operation"
                        )
                    )
            case TokenType.NOT:
                if context.is_bool(value=rVal):
                    return not rVal
                else:
                    raise TypeError(
                        context.get_error_message(
                            "Incompatible types for unary operation"
                        )
                    )
            case _:
                raise ValueError(context.get_error_message("Invalid unary operator"))


class BinaryExprASTNode(ExprASTNode):
    def __init__(self, op: Token, lhs: ExprASTNode, rhs: ExprASTNode):
        super().__init__()
        self.op = op
        self.lhs = lhs
        self.rhs = rhs

    def __str__(self):
        self.rhs.level = self.level
        self.lhs.level = self.level
        return f"-> BinaryExprASTNode (lineNo={self.op.lineNo}, columnNo={self.op.columnNo}) {self.op.lexVal}\n{' ' * (self.level)}-{repr(self.lhs)}\n{' ' * (self.level)}-{repr(self.rhs)}"

    def code_gen(self, context: TheContext):
        context.update_line_col(lineNo=self.op.lineNo, columnNo=self.op.columnNo)

        lVal = self.lhs.code_gen(context=context)
        rVal = self.rhs.code_gen(context=context)

        if (
            lVal == None
            and not isinstance(
                self.lhs, (ReservedKeyWordASTNode, IdentifierASTNode, CallExprASTNode)
            )
        ) or (
            rVal == None
            and not isinstance(
                self.rhs, (ReservedKeyWordASTNode, IdentifierASTNode, CallExprASTNode)
            )
        ):
            raise ValueError(context.get_error_message("Unexpected null value"))

        errorStr = context.get_error_message("Incompatible types for binary operation")

        match self.op.type:
            case TokenType.PLUS:
                if (
                    context.type_checker(left=lVal, right=rVal)
                    and lVal != None
                    and rVal != None
                ):
                    result = lVal + rVal  # allows for addition and concatenation
                    if context.is_real(value=result):
                        return float(result)
                    else:
                        return result
                else:
                    raise TypeError(errorStr)  # incompatible types
            case TokenType.MINUS:
                if context.is_real(value=lVal) and context.is_real(value=rVal):
                    return float(lVal - rVal)
                else:
                    raise TypeError(errorStr)
            case TokenType.MUL:
                if context.is_real(value=lVal) and context.is_real(value=rVal):
                    result = lVal * rVal
                elif isinstance(lVal, (str, list)) and context.is_real(value=rVal):
                    result = lVal * int(rVal)
                else:
                    raise TypeError(errorStr)

                if context.is_real(value=result):
                    return float(result)
                else:
                    return result
            case TokenType.FLOAT_DIV:
                if context.is_real(value=lVal) and context.is_real(value=rVal):
                    if rVal != 0:
                        return float(lVal / rVal)
                    else:
                        raise ZeroDivisionError(
                            context.get_error_message("Division by zero")
                        )
                else:
                    raise TypeError(errorStr)
            case TokenType.INT_DIV:
                if context.is_real(value=lVal) and context.is_real(value=rVal):
                    if rVal != 0:
                        return float(lVal // rVal)
                    else:
                        raise ZeroDivisionError(
                            context.get_error_message("Division by zero")
                        )
                else:
                    raise TypeError(errorStr)
            case TokenType.MOD:
                if context.is_real(value=lVal) and context.is_real(value=rVal):
                    if rVal != 0:
                        return float(lVal % rVal)
                    else:
                        raise ZeroDivisionError(
                            context.get_error_message("Modulo by zero")
                        )
                else:
                    raise TypeError(errorStr)
            case TokenType.EXP:
                if context.is_real(value=lVal) and context.is_real(value=rVal):
                    return float(lVal**rVal)
                else:
                    raise TypeError(errorStr)
            case TokenType.AND:
                if context.is_bool(value=lVal) and context.is_bool(value=rVal):
                    return bool(lVal and rVal)
                else:
                    raise TypeError(errorStr)
            case TokenType.OR:
                if context.is_bool(value=lVal) and context.is_bool(value=rVal):
                    return bool(lVal or rVal)
                else:
                    raise TypeError(errorStr)
            case TokenType.GT:
                if context.is_real(value=lVal) and context.is_real(value=rVal):
                    return bool(lVal > rVal)
                else:
                    raise TypeError(errorStr)
            case TokenType.GE:
                if context.is_real(value=lVal) and context.is_real(value=rVal):
                    return bool(lVal >= rVal)
                else:
                    raise TypeError(errorStr)
            case TokenType.LT:
                if context.is_real(value=lVal) and context.is_real(value=rVal):
                    return bool(lVal < rVal)
                else:
                    raise TypeError(errorStr)
            case TokenType.LE:
                if context.is_real(value=lVal) and context.is_real(value=rVal):
                    return bool(lVal <= rVal)
                else:
                    raise TypeError(errorStr)
            case TokenType.EQ:
                return bool(lVal == rVal)
            case TokenType.NE:
                return bool(lVal != rVal)
            case TokenType.APPEND:
                if isinstance(self.lhs, IdentifierASTNode):
                    if self.lhs.is_arr():
                        ident = self.lhs.get_ident()
                        context.set_ident(
                            symbol=ident,
                            valType=None,
                            value=rVal,
                            index=None,
                            isAppend=True,
                            isArr=True,
                        )
                        return context.get_ident(symbol=ident, index=None, getArr=True)
                    else:
                        raise TypeError(
                            context.get_error_message("Can only append to an array")
                        )
                else:
                    raise TypeError(
                        context.get_error_message(
                            "Can only append to an array stored in variable"
                        )
                    )
            case _:
                raise ValueError(context.get_error_message("Invalid binary operator"))


class CallExprASTNode(ExprASTNode):
    def __init__(
        self, token: Token, callee: IdentifierASTNode, args: list[ExprASTNode]
    ):
        super().__init__()
        self.token = token
        self.callee = callee
        self.args = args

    def __str__(self):
        self.callee.level = self.level
        out = f"-> CallExprASTNode (lineNo={self.token.lineNo}, columnNo={self.token.columnNo})\n{' ' * (self.level)}-{repr(self.callee)} "
        if self.args:
            out += f"\n{' ' * (self.level)}()-¬"
            for arg in self.args:
                arg.level = self.level + 3
                out += f"\n{' ' * (self.level + 3)}-{repr(arg)}"

        return out

    def execute_builtin(self, fEntry: FuncEntry):
        # retrieve args values from context of reserved call and pass to inbuilt function
        argVals = (
            fEntry.context.get_ident(
                symbol=arg,
                index=None,
                getArr=fEntry.context.get_ident_type(symbol=arg, getArr=False) == list,
            )
            for arg in fEntry.argSymbols
        )

        if fEntry.funcBody == print:
            strArg = str(*argVals)
            strArg = fEntry.context._display_builtin_helper(strOut=strArg)
            argVals = (strArg,)

        rVal = fEntry.funcBody(*argVals)
        fEntry.context.funcReturnVal = rVal

    def code_gen(self, context: TheContext):
        context.update_line_col(lineNo=self.token.lineNo, columnNo=self.token.columnNo)

        if self.callee == None:
            raise ValueError(context.get_error_message("Invalid function call"))

        if isinstance(self.callee, ReservedKeyWordASTNode):
            self.callee.code_gen(context=context)  # generate builtin function

        symbol = self.callee.get_ident()
        entry: FuncEntry = context.get_func(symbol=symbol)
        fEntry = entry.copy() 

        if fEntry == None:
            raise ValueError(
                context.get_error_message(
                    f"Function {symbol} doesn't exist/has not yet been defined"
                )
            )

        if len(self.args) != len(fEntry.argSymbols):
            raise ValueError(context.get_error_message("Incorrect number of arguments"))

        argValues = [arg.code_gen(context=context) for arg in self.args]
        argValSymb = zip(fEntry.argSymbols, argValues)

        for (
            par,
            val,
        ) in (
            argValSymb
        ):  # for each argument value try assigning to parameter to check types in function context
            if fEntry.context.type_checker_assign(left=par, right=val, hasIndex=False):
                fEntry.context.set_ident(
                    symbol=par,
                    valType=None,
                    value=val,
                    index=None,
                    isAppend=False,
                    isArr=fEntry.context.get_ident_type(symbol=par, getArr=False)
                    == list,
                )
            else:
                raise TypeError(
                    context.get_error_message(
                        f"Type of argument {val} does not match type of positional function parameter"
                    )
                )

        if isinstance(self.callee, ReservedKeyWordASTNode):
            self.execute_builtin(fEntry=fEntry)
        else:
            funcReturn = fEntry.funcBody.code_gen(
                context=fEntry.context
            )  # execute function body using function context
            fEntry.context.funcReturnVal = funcReturn
        
        if fEntry.type_checker_return():
            rVal = fEntry.context.funcReturnVal
            fEntry.context.reset_flags_func()
            if context.is_real(value=rVal) and not context.is_bool(value=rVal):
                return float(rVal)
            else:
                return rVal
        else:
            raise TypeError(
                context.get_error_message("Function type does not match return type")
            )


class ArrayASTNode(ExprASTNode):
    def __init__(self, token: Token, elems: list[ExprASTNode]):
        super().__init__()
        self.token = token
        self.elems = elems

    def __str__(self):
        out = f"-> ArrayASTNode (lineNo={self.token.lineNo}, columnNo={self.token.columnNo}) "
        if self.elems:
            for elem in self.elems:
                elem.level = self.level
                out += f"\n{' ' * (self.level)}-{repr(elem)}"

        return out

    def code_gen(self, context: TheContext):
        context.update_line_col(lineNo=self.token.lineNo, columnNo=self.token.columnNo)

        arrVal = [elem.code_gen(context=context) for elem in self.elems]

        return arrVal


class AssignASTNode(ExprASTNode):
    def __init__(self, token: Token, var: IdentifierASTNode, value: ExprASTNode):
        super().__init__()
        self.token = token
        self.var = var
        self.value = value

    def __str__(self):
        self.var.level = self.level
        self.value.level = self.level + 3
        out = f"-> AssignASTNode (lineNo={self.token.lineNo}, columnNo={self.token.columnNo})\n{' ' * (self.level)}-{repr(self.var)} "
        out += f"\n{' ' * (self.level)}:=-¬"
        out += f"\n{' ' * (self.level + 3)}-{repr(self.value)}"

        return out

    def get_var(self):
        return self.var

    def code_gen(self, context: TheContext):
        context.update_line_col(lineNo=self.token.lineNo, columnNo=self.token.columnNo)

        if self.var.get_type():
            self.var.code_gen(context=context)

        assignVal = self.value.code_gen(context=context)
        symbol = self.var.get_ident()

        if assignVal == None and not isinstance(
            self.value, (ReservedKeyWordASTNode, IdentifierASTNode, CallExprASTNode)
        ):
            raise ValueError(context.get_error_message("Unexpected null value"))

        if self.var.index:
            if context.type_checker_assign(left=symbol, right=assignVal, hasIndex=True):
                indexVal = self.var.index.code_gen(context=context)
                if not context.is_real(indexVal):
                    raise TypeError(
                        context.get_error_message(
                            "Invalid index type. Should be real value"
                        )
                    )
                context.set_ident(
                    symbol=symbol,
                    valType=None,
                    value=assignVal,
                    index=int(indexVal),
                    isAppend=False,
                    isArr=self.var.arrFlag,
                )
            else:
                raise TypeError(
                    context.get_error_message(
                        "Value type does not match variable type in assignment"
                    )
                )
        else:
            if context.type_checker_assign(
                left=symbol, right=assignVal, hasIndex=False
            ):
                context.set_ident(
                    symbol=symbol,
                    valType=None,
                    value=assignVal,
                    index=None,
                    isAppend=False,
                    isArr=self.var.arrFlag,
                )
            else:
                raise TypeError(
                    context.get_error_message(
                        "Value type does not match variable type in assignment"
                    )
                )

        return None


class StmtBlockASTNode(StmtASTNode):
    def __init__(self, token: Token, stmtList: list[StmtASTNode]):
        super().__init__()
        self.token = token
        self.stmtList = stmtList

    def __str__(self):
        out = f"-> StmtBlockASTNode (lineNo={self.token.lineNo}, columnNo={self.token.columnNo}) "
        for stmt in self.stmtList:
            stmt.level = self.level
            out += f"\n{' ' * (self.level)}-{repr(stmt)}"

        return out

    def code_gen(self, context: TheContext):
        context.update_line_col(lineNo=self.token.lineNo, columnNo=self.token.columnNo)

        if not self.stmtList:
            raise ValueError(context.get_error_message("Empty block"))

        stmtVal = None
        for decl in self.stmtList:
            stmtVal = decl.code_gen(context=context)

            if context.should_continue():
                return stmtVal

        return stmtVal


class ElifStmtASTNode(StmtASTNode):
    def __init__(self, token: Token, cond: ExprASTNode, elifBody: StmtBlockASTNode):
        super().__init__()
        self.token = token
        self.cond = cond
        self.elifBody = elifBody

    def __str__(self):
        self.cond.level = self.level + 3
        self.elifBody.level = self.level
        out = f"-> ElifStmtASTNode (lineNo={self.token.lineNo}, columnNo={self.token.columnNo}) "
        out += f"\n{' ' * (self.level)}?-¬"
        out += f"\n{' ' * (self.level + 3)}-{repr(self.cond)}"
        out += f"\n{' ' * (self.level)}-{repr(self.elifBody)}"

        return out

    def code_gen(self, context: TheContext):
        context.update_line_col(lineNo=self.token.lineNo, columnNo=self.token.columnNo)

        if self.cond == None or self.elifBody == None:
            raise ValueError(
                context.get_error_message("Invalid condition or elif body")
            )

        condVal = self.cond.code_gen(context=context)

        if not context.is_bool(value=condVal):
            raise TypeError(
                context.get_error_message(
                    "Invalid condition type. Should be boolean value"
                )
            )

        return condVal, self.elifBody


class IfStmtASTNode(StmtASTNode):
    def __init__(
        self,
        token: Token,
        cond: ExprASTNode,
        ifBody: StmtBlockASTNode,
        elifStmts: list[ElifStmtASTNode],
        elseBody: StmtBlockASTNode,
    ):
        super().__init__()
        self.token = token
        self.cond = cond
        self.ifBody = ifBody
        self.elifStmts = elifStmts
        self.elseBody = elseBody

    def __str__(self):
        self.cond.level = self.level + 3
        self.ifBody.level = self.level
        out = f"-> IfStmtASTNode (lineNo={self.token.lineNo}, columnNo={self.token.columnNo}) "
        out += f"\n{' ' * (self.level)}?-¬"
        out += f"\n{' ' * (self.level + 3)}-{repr(self.cond)}"
        out += f"\n{' ' * (self.level)}-{repr(self.ifBody)}"
        if self.elifStmts:
            for el in self.elifStmts:
                el.level = self.level
                out += f"\n{' ' * (self.level)}-{repr(el)}"
        if self.elseBody:
            self.elseBody.level = self.level
            out += f"\n{' ' * (self.level)}-{repr(self.elseBody)}"

        return out

    def code_gen(self, context: TheContext):
        context.update_line_col(lineNo=self.token.lineNo, columnNo=self.token.columnNo)

        if self.cond == None or self.ifBody == None:
            raise ValueError(context.get_error_message("Invalid condition or if body"))

        condVal = self.cond.code_gen(context=context)

        if not context.is_bool(value=condVal):
            raise TypeError(
                context.get_error_message(
                    "Invalid condition type. Should be boolean value"
                )
            )

        if condVal:  # if condition true evaluate if body
            return self.ifBody.code_gen(context=context)

        if self.elifStmts:
            for el in self.elifStmts:
                elCond, elBody = el.code_gen(context=context)
                if elCond:
                    return elBody.code_gen(context=context)

        if self.elseBody:
            return self.elseBody.code_gen(context=context)

        return None


class WhileStmtASTNode(StmtASTNode):
    def __init__(self, token: Token, cond: ExprASTNode, whileBody: StmtBlockASTNode):
        super().__init__()
        self.token = token
        self.cond = cond
        self.whileBody = whileBody

    def __str__(self):
        self.cond.level = self.level + 3
        out = f"-> WhileStmtASTNode (lineNo={self.token.lineNo}, columnNo={self.token.columnNo}) "
        out += f"\n{' ' * (self.level)}?-¬"
        out += f"\n{' ' * (self.level + 3)}-{repr(self.cond)}"
        if self.whileBody:
            self.whileBody.level = self.level
            out += f"\n{' ' * (self.level)}-{repr(self.whileBody)}"

        return out

    def code_gen(self, context: TheContext):
        context.update_line_col(lineNo=self.token.lineNo, columnNo=self.token.columnNo)

        if self.cond == None or self.whileBody == None:
            raise ValueError(
                context.get_error_message("Invalid condition or while body")
            )

        context.inLoop = True

        bVal = None

        while True:
            condVal = self.cond.code_gen(context=context)
            if not context.is_bool(value=condVal):
                raise TypeError(
                    context.get_error_message(
                        "Invalid condition type. Should be boolean value"
                    )
                )

            if not condVal:
                break

            bVal = self.whileBody.code_gen(context=context)
            context.inLoop = True

            if context.should_return():
                return bVal

            if context.should_continue():
                context.continueFlag = False
                continue

            if context.should_break():
                context.breakFlag = False
                break

        context.inLoop = False

        return bVal


class ForStmtASTNode(StmtASTNode):
    def __init__(
        self,
        token: Token,
        counter: IdentifierASTNode,
        start: ExprASTNode,
        end: ExprASTNode,
        increment: ExprASTNode,
        forBody: StmtBlockASTNode,
    ):
        super().__init__()
        self.token = token
        self.counter = counter
        self.start = start
        self.end = end
        self.increment = increment
        self.forBody = forBody

    def __str__(self):
        self.counter.level = self.level
        self.start.level = self.level + 3
        self.end.level = self.level + 3
        self.increment.level = self.level + 3
        out = f"-> ForStmtASTNode (lineNo={self.token.lineNo}, columnNo={self.token.columnNo})\n{' ' * (self.level)}-{repr(self.counter)} "
        out += f"\n{' ' * (self.level)}#-¬"
        out += f"\n{' ' * (self.level + 3)}-{repr(self.start)}"
        out += f"\n{' ' * (self.level + 3)}-{repr(self.end)}"
        out += f"\n{' ' * (self.level + 3)}-{repr(self.increment)}"
        if self.forBody:
            self.forBody.level = self.level
            out += f"\n{' ' * (self.level)}-{repr(self.forBody)}"

        return out

    def code_gen(self, context: TheContext):
        context.update_line_col(lineNo=self.token.lineNo, columnNo=self.token.columnNo)

        if (
            self.counter == None
            or self.start == None
            or self.end == None
            or self.increment == None
            or self.forBody == None
        ):
            raise ValueError(context.get_error_message("Invalid condition or for body"))

        context.inLoop = True

        self.counter.code_gen(context=context)
        symbol = self.counter.get_ident()

        startVal = self.start.code_gen(context=context)
        endVal = self.end.code_gen(context=context)
        incVal = self.increment.code_gen(context=context)

        if (
            not context.is_real(value=startVal)
            or not context.is_real(value=endVal)
            or not context.is_real(value=incVal)
        ):
            raise TypeError(
                context.get_error_message(
                    "Invalid start, end or increment type. Should be real value"
                )
            )

        if incVal == 0:
            raise ValueError(
                context.get_error_message("Invalid increment value. Cannot be zero")
            )

        i = startVal

        if incVal >= 0:
            for_cond = lambda: i < endVal
        else:
            for_cond = lambda: i > endVal

        context.set_ident(
            symbol=symbol,
            valType=None,
            value=startVal,
            index=None,
            isAppend=False,
            isArr=False,
        )

        bVal = None

        while for_cond():
            i += incVal
            bVal = self.forBody.code_gen(context=context)
            context.inLoop = True

            context.set_ident(
                symbol=symbol,
                valType=None,
                value=i,
                index=None,
                isAppend=False,
                isArr=False,
            )

            if context.should_return():
                return bVal

            if context.should_continue():
                context.continueFlag = False
                continue

            if context.should_break():
                context.breakFlag = False
                break

        context.inLoop = False

        return bVal


class ReturnStmtASTNode(StmtASTNode):
    def __init__(self, token: Token, returnBody: ExprASTNode):
        super().__init__()
        self.token = token
        self.returnBody = returnBody

    def __str__(self):
        out = f"-> ReturnStmtASTNode (lineNo={self.token.lineNo}, columnNo={self.token.columnNo})"
        if self.returnBody:
            self.returnBody.level = self.level
            out += f"\n{' ' * (self.level)}-{repr(self.returnBody)}"

        return out

    def code_gen(self, context: TheContext):
        context.update_line_col(lineNo=self.token.lineNo, columnNo=self.token.columnNo)

        if not context.inFunc:
            raise ValueError(
                context.get_error_message("Cannot return outside a function")
            )

        rVal = None
        context.returnFlag = True

        if self.returnBody:
            rVal = self.returnBody.code_gen(context=context)
            context.funcReturnVal = rVal

        return rVal


class BreakStmtASTNode(StmtASTNode):
    def __init__(self, token: Token):
        super().__init__()
        self.token = token

    def __str__(self):
        return f"-> BreakStmtASTNode (lineNo={self.token.lineNo}, columnNo={self.token.columnNo})"

    def code_gen(self, context: TheContext):
        context.update_line_col(lineNo=self.token.lineNo, columnNo=self.token.columnNo)

        if not context.inLoop:
            raise ValueError(context.get_error_message("Cannot break outside a loop"))

        context.breakFlag = True

        return None


class ContinueStmtASTNode(StmtASTNode):
    def __init__(self, token: Token):
        super().__init__()
        self.token = token

    def __str__(self):
        return f"-> ContinueStmtASTNode (lineNo={self.token.lineNo}, columnNo={self.token.columnNo})"

    def code_gen(self, context: TheContext):
        context.update_line_col(lineNo=self.token.lineNo, columnNo=self.token.columnNo)

        if not context.inLoop:
            raise ValueError(
                context.get_error_message("Cannot continue outside a loop")
            )

        context.continueFlag = True

        return None


class FunctionASTNode(ASTNode):
    def __init__(
        self,
        token: Token,
        funcName: IdentifierASTNode,
        params: list[IdentifierASTNode],
        funcBody: StmtBlockASTNode,
    ):
        super().__init__()
        self.token = token
        self.funcName = funcName
        self.params = params
        self.funcBody = funcBody

    def __str__(self):
        self.funcName.level = self.level
        out = f"-> FunctionASTNode (lineNo={self.token.lineNo}, columnNo={self.token.columnNo})\n{' ' * (self.level)}-{repr(self.funcName)} "
        if self.params:
            out += f"\n{' ' * (self.level)}()-¬"
            for par in self.params:
                par.level = self.level + 3
                out += f"\n{' ' * (self.level + 3)}-{repr(par)}"
        if self.funcBody:
            self.funcBody.level = self.level
            out += f"\n{' ' * (self.level)}-{repr(self.funcBody)}"

        return out

    def code_gen(self, context: TheContext):
        context.update_line_col(lineNo=self.token.lineNo, columnNo=self.token.columnNo)

        if self.funcName == None and self.funcBody == None:
            raise ValueError(
                context.get_error_message("Invalid function name or function body")
            )

        symbol = self.funcName.get_ident()
        isReturnArr = self.funcName.is_arr()
        returnType, _ = self.funcName.determine_type_value()

        argSymbols = []

        funcContext = TheContext(parent=context)
        funcContext.inFunc = True

        for par in self.params:
            argSymbols.append(par.get_ident())  # store parameter names
            par.code_gen(
                context=funcContext
            )  # add parameters as variables to function context

        context.set_func(
            symbol=symbol,
            returnType=returnType,
            argSymbols=argSymbols,
            funcBody=self.funcBody,
            context=funcContext,
            isReturnArr=isReturnArr,
        )

        return None


class ProgramASTNode(ASTNode):
    def __init__(self, declarList: list[ASTNode]):
        super().__init__()
        self.declarList = declarList
        self.globalContext = TheContext(parent=None)

    def __str__(self):
        out = f"|-> ProgramASTNode "
        if self.declarList:
            for decl in self.declarList:
                decl.level = self.level
                out += f"\n{' ' * (self.level)}-{repr(decl)}"
        out += "\n<-|"

        return out

    def code_gen(self):
        progVal = None
        for decl in self.declarList:
            progVal = decl.code_gen(context=self.globalContext)

        return progVal
