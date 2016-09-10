## Puzzle

## Valid constraint
Generate random arithmetic expressions such that they have `L` literals,
each literal is integer and does not exceed `M` in absolute value.
Expression evaluation result should be _integer_ and not exceed `N` in absolute value.

Allowed arithmetic operations: addition, subtraction, multiplication, division.
Operations have usual precedence and left associativity.

### Goals
- Implementation should read `L`, `M` and `N` from command line arguments.
- Generate a random expression and print it on the screen.
- The expression should be valid.
- The expression should not contain redundant parentheses. For instance, `((5+(6*7))` should actually be `5+6*7`, whereas `5*(-2)` has no redundant parentheses.
- Generation should be comprehensive (if some expression is valid, it should be possible to generate it in principle)

### Examples
| L |  M |   N |               Expression |
|---|----|-----|--------------------------|
| 3 | 10 | 100 | `(2 + 8) * (-10)`        |
| 4 | 20 | 100 | `-16 / 4 * 19 + 14`      |
| 5 | 30 | 100 | `-30 * 10 + 20 * 11 / 1` |


## How to run
1. Install `stack`
2. Run `stack setup` once

   Useful commands:
   - `stack build` - compile only
   - `stack ghci` - run REPL with project modules
   - `stack test` - run unit tests

3. Run `exprgen --help` for further instructions
   Example:
   `exprgen -l 3 -n 10 -m 20`
