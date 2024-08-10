# scanner.rs
    Scanning (or "lexing") is the process of breaking an input string down into its most fundamental elements, called "tokens". 
    A token is a basic syntactic unit of the language, like keywords, identifiers, or punctuation. 
    Some tokens, like identifiers, have an associated "lexeme". In the context of an identifier, the lexeme is its name.
    For many tokens, such as punctuation, lexemes are redundant and so aren't included.
    For example, the input string
    """
    let a = 25 / 3;
    """
    Is tokenized into the following token lexeme pairs.
    """
    (TOKEN::LET) (TOKEN::IDENTIFIER, "a") (TOKEN::EQUAL) (TOKEN::NUMERIC_LITERAL, "25") (TOKEN::SLASH) (TOKEN::NUMERIC_LITERAL, "3") (TOKEN::SEMICOLON)
    """

    Possible Errors: 
        - Unrecognized tokens: Return a special 'error token'.

# parser.rs
    builds ast
        
# vm.rs
    abstractly: execute bytecode instructions and maintain runtime state

# Toy Grammar
    program         ::= expression eof
    expression      ::= logic_expr
    logic_expr      ::= equal_expr ( ( "and" | "or" ) equal_expr )?
    equal_expr      ::= relational_expr ( ( "==" | "!=" ) relational_expr )?
    relational_expr ::= add_expr ( ( "<" | "<=" | ">" | ">") add_expr )?
    add_expr        ::= mult_expr ( ( "+" | "-" ) mult_expr )?
    mult_expr       ::= unary_expr( ( "*" | "/" ) unary_expr )?
    unary_expr      ::= ( "!" | "-" ) unary_expr | primary_expr
    primary_expr    ::= number_literal | bool_literal | "(" expression ")"

# Statements!
    program         ::= stmt* eof
    stmt            ::= print_stmt | expr_stmt | block
    print_stmt      ::= "print" expr ";"
    expr_stmt       ::= expression ";"
    block           ::= "{" stmt* "}"
    expr            ::= logic_expr
    logic_expr      ::= equal_expr ( ( "and" | "or" ) logic_expr )?
    equal_expr      ::= relational_expr ( ( "==" | "!=" ) equal_expr )?
    relational_expr ::= add_expr ( ( "<" | "<=" | ">" | ">") relational_expr )?
    add_expr        ::= mult_expr ( ( "+" | "-" ) add_expr )?
    mult_expr       ::= unary_expr( ( "*" | "/" ) mult_expr )?
    unary_expr      ::= ( "!" | "-" ) unary_expr | primary_expr
    primary_expr    ::= number_literal | bool_literal | identifier | "(" expr ")"

# Variables

