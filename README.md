## Puzzle

## Valid constraint
Generate random arithmetic expressions such that they have `L` literals,
each literal is integer and does not exceed `N` in absolute value.
The evaluation result of the whole expression
should be integer and do not exceed `M` in absolute value.

Allowed arithmetic operations: addition, subtraction, multiplication, division.
Operations have usual precedence and left associativity.

### Goals

- Implementation should read `L`, `M` and `N` from command line arguments.
- Generate a random expression and print it on the screen.
- The expression should be valid.
- The expression should _not_ contain redundant parentheses. For instance, `((5+(6*7))` should actually be `5+6*7`, whereas `5*(-2)` and `5+(-2)` has no redundant parentheses (to remove parentheses here we need a simplification process implemented, which is not needed for the task). The parentheses removal must satisfy to the following constraints:

  1. must not change the expression evaluation result.
  2. must not result in symbol sequences, like `--`, `+-`, `/-`, `*-` (even)
  3. must not change operation type, simplification is not the task: `2 + (-3)` ⇏ `2 - 3`.
  4. Removing parentheses can change evaluation order under associativity: `2 + (3 + 4)` ⇒ `2 + 3 + 4`.

- Generation should be comprehensive (if some expression is valid, it should be possible to generate it in principle).

#### Notes on validity

- The expression `1 / 2 * 4` is valid even since `1 / 2` is not integer.
- The expression `10 * 10 * 10 / 10` is valid for, say, `M = 100`,
despite it evaluates as `(((10 * 10) * 10) / 10)` and intermediate expression `100 * 10`
is too big.
- The expression `3 * 4 / (1 - 1) * (1 - 1)` is invalid, since it has division by zero.

#### Notes on removing parentheses

- Positive literals aren't wrapped: `3 + 4`
- Negative literals aren't wrapped if they are at the
  beginning of the expression or right after the paren:
  - `((-3) + 4)` ⇒ `-3 + 4`
  - `(2 * ((-3) + 4))` ⇒ `2 * (-3 + 4)`
- Sums are flattened:
  - `((3 + (-4)) + ((-5) + 6))` ⇒ `3 + (-4) + (-5) + 6`
- Products are flattened:
  - `((3 * (-4)) * ((-5) * 6))` ⇒ `3 * (-4) * (-5) * 6`
- Precedence rules are applied:
  - `((1 * 2) + (3 / 4))` ⇒ `1 * 2 + 3 / 4`
- Products before division aren't put in parens:
  - `(3 * (-4)) / 3` ⇒ `3 * (-4) / 3`

### Examples
| L |  N |   M |               Expression |
|---|----|-----|--------------------------|
| 4 |  4 |   4 | `4 / (0 - 4) - 3`        |
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
