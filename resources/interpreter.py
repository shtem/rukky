# Interpreter visits each AST node - processes that node and visit all the child nodes
# Does code generation for each node when it visits node class -> calls codegen for that AST node
# error handler in class - semantic error