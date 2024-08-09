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
    program       ::= expression* eof
    expression    ::= add_expr
    add_expr      ::= mult_expr ( ( "+" | "-" ) mult_expr )?
    mult_expr     ::= unary_expr( ( "*" | "/" ) unary_expr )?
    unary_expr    ::= ( "!" | "-" ) unary_expr | primary_expr
    primary_expr  ::= number_literal | "(" expression ")"

# Tokens 

# Op Codes
