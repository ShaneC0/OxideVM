# scanner.rs
    Scanning (or "lexing") is the process of breaking an input string down into its most fundamental elements, called "tokens". 
    A token is a basic syntactic unit of the language, like keywords, identifiers, or punctuation. 
    Each token has a "lexeme" which is the actual text of which the token consists of.
    For example, the input string
    """
    let a = 25 / 3;
    """
    Is tokenized into the following token lexeme pairs.
    """
    (TOKEN::LET, "let") (TOKEN::IDENTIFIER, "a") (TOKEN::EQUAL, "=") (TOKEN::NUMERIC_LITERAL, "25") (TOKEN::SLASH, "/") (TOKEN::NUMERIC_LITERAL, "3") (TOKEN::SEMICOLON, ";")
    """
    The heart of our scanner module is the "next" function.
    The next function returns the next token in the input stream.
    If there are no more tokens, it returns a special EOF token.
    As soon as the scanner can tell that a lexeme does not correspond to any token, it returns a special error token.
    

# parser.rs
    Parsing is the process of transforming a linear sequence of tokens into a heirarchical structure called an Abstract Syntax Tree (AST).
    This structure of the AST is suited to capture the recursive nature of programs, and is easier for later stages of the pipeline to operate on.
    For example, the input string
    """
    let a = 25 + 3 / 30;
    """
    Is parsed into a structure like this:
    """
    Declaration:
      Ident: a
      Expr: 
        Binary:
          Left: 25
          Op:   +
          Right: 
            Binary
                Left: 3
                Op: /
                Right: 30
    """
    This structure captures the meaning of the statement by ensuring that, when evaluated, the order of operations is maintained.
    The parser in Oxide is an LL(2)* Recursive Descent parser. The only part of the parser which uses the second token of lookahead is when seeing an identifier
        in the beginning of a statement. The parser must look ahead past the token to see if the next is an '=' to differentiate an assignment and an expression statement.
    The parser also makes use of a technique called "Pratt Parsing" in order to parse expressions and ensure operator precedence.
        The core idea of Pratt Parsing is making the expression parser itself aware of the context in which it is parsing, 
        or in other words, the precedence of the super-expression which is parsing this sub-expression.
        You basically build a table of precedence levels, and pass higher or lower indices into that table to parse different expression precedences.

    * The parser allows for arbitrary lookahead but only two tokens of lookahead are used

# compiler.rs
    The compiler is responsible for transforming the tree-like AST into a flat sequence of bytecode instructions that the VM can understand.
    The compiler also resolves the scopes of all local variables before the program is run, and stores all the constants in the program into a constant table.
    The compiler also makes use of a data structure called an "interner" to manage its heap memory.
    When some piece of data is given to the interner, it stores it in the pool and returns you its index.
    If that data is already in the pool, it will return the index of the existing data.
    For example, the input string
    """
    lex x = "hello";
    print x;
    """
    Compiles into the follwing information.
    """
    Bytecode: 01 00 0f 01 10 02 0c 
    Constants: [String(0), String(1), String(1)]
    Interner: HeapInterner { map: {String("hello"): 0, String("x"): 1}, vec: [String("hello"), String("x")] }
    """
    Note that the string types in the constant array hold numbers. Those are their indexes in the interner.

# Grammar!
    program         ::= decl* eof
    decl            ::= let_decl | stmt
    let_decl        ::= "let" ident ( "=" expr )? ";"
    stmt            ::= if_stmt | while_stmt | print_stmt | expr_stmt | assign_stmt | block
    if_stmt         ::= "if" "(" expr ")" stmt ( "else" stmt )?
    while_stmt      ::= "while" "(" expr ")" stmt
    print_stmt      ::= "print" expr ";"
    expr_stmt       ::= expression ";"
    assign_stmt     ::= ident "=" expr ";"
    block           ::= "{" stmt* "}"
    expr            ::= logic_expr
    logic_expr      ::= equal_expr ( ( "and" | "or" ) logic_expr )?
    equal_expr      ::= relational_expr ( ( "==" | "!=" ) equal_expr )?
    relational_expr ::= add_expr ( ( "<" | "<=" | ">" | ">") relational_expr )?
    add_expr        ::= mult_expr ( ( "+" | "-" ) add_expr )?
    mult_expr       ::= unary_expr( ( "*" | "/" ) mult_expr )?
    unary_expr      ::= ( "!" | "-" ) unary_expr | primary_expr
    primary_expr    ::= number_literal | bool_literal | string_literal | identifier | "(" expr ")"

