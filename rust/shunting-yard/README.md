# The task

Use Dijkstra (=Shunting-Yard) algorithm to develop the compiler for 
arithmetic-boolean expressions.

The expressions contain
  
  - numbers in any format
  - identifiers for variables of real and boolean type 
  (variables of real type start with `R`, variables of boolean type start with `B`)
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
    
### Examples

- `ln ( exp ( 1.234 ) / 2 * [ 3 + 4 ])` should evaluate to `2.48676...`
- `ln(R0) + ln(exp(R1) * 2 > 1 & Bx)` with bindings `{"R0": 1.0, "R1": 0.5, "Bx": true}` should fail with type error (the attempt to calculate logarithm out of a boolean value)

### Useful links

- [Shunting-Yard algorithm in Wiki](https://en.wikipedia.org/wiki/Shunting-yard_algorithm)
- [Writing a Simple Parser in Rust](https://adriann.github.io/rust_parser.html)

   
