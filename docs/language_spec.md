# Rask Language Specification (Draft)

## Phase 1-4 Grammar

```text
program      -> statement* EOF ;
statement    -> var_decl | destructure_decl | func_def | return_stmt | print_stmt | expr_stmt ;
statement    -> use_stmt | ... ;
use_stmt     -> "use" IDENT ("." IDENT)* ("as" IDENT)? ;
var_decl     -> IDENT (":" type)? "=" expression ;
destructure_decl -> pattern "=" expression ;
func_def     -> "def" IDENT "(" params? ")" ("->" type)? block ;
params       -> param ("," param)* ;
param        -> IDENT (":" type)? ;
block        -> "{" statement* "}" ;
return_stmt  -> "return" expression? ;
print_stmt   -> "print" ("(" expression ")" | expression) ;
expr_stmt    -> expression ;

expression   -> assignment ;
assignment   -> IDENT "=" assignment | coalesce ;
coalesce     -> equality ( "or" ("return" expression | equality) )* ;
equality     -> comparison (("==" | "!=") comparison)* ;
comparison   -> term ((">" | ">=" | "<" | "<=") term)* ;
term         -> factor (("+" | "-") factor)* ;
factor       -> unary (("*" | "/" | "%") unary)* ;
unary        -> ("!" | "-") unary | postfix ;
postfix      -> primary ( "(" arguments? ")" | "." IDENT | "?." IDENT | "[" expression "]" | "!" )* ;
primary      -> NUMBER | STRING | "true" | "false" | "nil"
             | IDENT | "(" expression ")" | match_expr | list_literal | map_literal ;
list_literal -> "[" (expression ("," expression)*)? "]"
             | "[" expression "for" IDENT "in" expression ("if" expression)? "]" ;
map_literal  -> "{" ((IDENT | STRING) ":" expression ("," (IDENT | STRING) ":" expression)*)? "}" ;
match_expr   -> "match" expression "{" match_arm* "}" ;
match_arm    -> pattern "=>" expression ("," | ";" | NEWLINE)* ;
pattern      -> "_" | IDENT | NUMBER | STRING | "true" | "false" | "nil"
             | "[" (pattern ("," pattern)*)? "]"
             | "{" (IDENT (":" pattern)? ("," IDENT (":" pattern)?)*)? "}" ;

type         -> union ;
union        -> nullable ("|" nullable)* ;
nullable     -> primary_type ("?")* ;
primary_type -> IDENT generic_args?
             | "(" (type ("," type)*)? ")" "->" type
             | "(" type ")" ;
generic_args -> "<" type ("," type)* ">" ;
```

## Runtime Flags

- `--allow-read=<path1,path2,...>`
- `--allow-write=<path1,path2,...>`
- `--allow-net=<domain1,domain2,...>` (reserved for HTTP module work)
- `--allow-env`
- `--allow-all`
