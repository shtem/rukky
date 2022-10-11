# rukky

### An interpreter for a simple programming language, written in Python
I began by writing a [grammar](https://github.com/shtem/rukky/blob/main/rukky/resources/grammar.txt) for the *rukky* procedural programming language which I then developed an interpreter for. This invented language is quite simple and borrows features and concepts from multiple programming languages, with a few syntactical tweaks here and there. I didn't create the language or the interpreter with speed, efficiency or novelty in mind, just as a fun mini-project.

The interpreter utilises a predictive top-down parsing technique called a recursive descent parser. The parser  is LL(1) in most cases, but not all.

---

#### Features

- [x] Comments ``$ ... $``
- [x] Real, Boolean and String Literals
- [x] Arithmetic Operations ``+, -, *, /, //, %, ^``
- [x] Comparison Operations ``?, <>, <!, >, >=, <, <=``
- [x] Logical Operations ``~, &&, ||``
- [x] Types ``real, bool, str, void, obj``
- [x] Variables ``x := 10``
- [x] Maps
- [x] Arrays
    - Indexing
        - Retrieve ``lst[0]``
        - Update ``lst@0 := x``
    - Appending ``lst << x``
- [x] If Statement ``if:: elif:: else::``
- [x] Delete Statement ``del::``
- [x] Loop Statements ``for:: while:: give::``
- [x] Reserved Keywords ``null, pi, eul``
- [x] Functions
- [x] Function Calls ``func:x::``
- [x] Branching Statements ``return:: break:: continue::``
- [x] Reserved Functions ``display, len, type, min, max, rand, floor, ceil, sqrt, log, sin, cos, tan, getStr, getReal ``
- [x] Classes ``class::``
    - Inheritance ``Child : Parent``
        - Super Keyword ``super::``

---

#### Running Interpreter

```
> python rukky -f path/to/rukky/test/files/factorial.rk
result: 120.0
```

##### Usage
```
> python rukky --help                                                  
usage: rukky [-h] (-s | -f FILE) [-t | -a | -g | -d]

Interpreter for the "rukky" programming language. Interprets code and outputs result.

options:
  -h, --help      show this help message and exit
  -s, --shell     run interpreter in shell mode. Directly enter input into the REPL to be interpreted
  -f FILE         run interpreter in file mode. Pass path to file to be interpreted
  -t, --tokens    outputs list of all tokens returned by lexer
  -a, --ast       outputs AST returned by parser
  -g, --global    outputs global symbol tables returned by interpreter, along with the result
  -d, --duration  outputs time it takes to interpret inputted program, along with the result
```

##### Requirements
```
python_version >= '3.10'
```

##### factorial.rk
```
:: real factorial := (real n) {
    real i
    real factorial

    factorial := 1
    i := 1

    while:: i <= n {
        factorial := factorial * i
        i := i + 1
    }

    return:: factorial
}

real fac := factorial:5::
display:"result: " + getStr:fac::::
```

Can find more example files [here](https://github.com/shtem/rukky/tree/main/rukky/test/files).

---

#### Acknowledgements
Sources and tutorials I used to help with the development of this project:

[Make Your Own Programming Language in Python](https://github.com/davidcallanan/py-myopl-code) - David Callanan

[Let's Build A Simple Interpreter](https://github.com/rspivak/lsbasi) - Ruslan Spivak

[A Simple Interpreter From Scratch In Python](https://jayconrod.com/posts/37/a-simple-interpreter-from-scratch-in-python--part-1-) - Jay Conrad