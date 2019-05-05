# The task

Use Dijkstra (=Shunting-Yard) algorithm to develop the compiler for 
arithmetic-boolean expressions.

The expressions contain
  
  - numbers in any format
  - identifiers for variables of real and boolean type 
  (variables of real type begin with R, variables of boolean type begin with B)
  - square brackets `[` and `]`
  - operators, precedence with associativity and sorted by precedence decreasing
    1. prec 9, assoc `<--`, function calls `exp(...)` and `ln(...)` 
    1. prec 8, assoc `<--`, unary `+` and `-`
    1. prec 7, assoc `-->`, binary `*` and `/`
    1. prec 6, assoc `-->`, binary `^`
    1. prec 5, assoc `-->`, binary `+` and `-`
    1. prec 4, assoc `-->`, binary boolean `<`, `>`, `=`, `#` (not equal)
    1. prec 3, assoc `<--`, unary boolean `~` (negation)
    1. prec 2, assoc `-->`, binary boolean and `&`
    1. prec 1, assoc `-->`, binary boolean or `!`
    
   