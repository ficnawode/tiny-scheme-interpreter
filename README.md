# Tiny Scheme Interpreter

This is an interpreter for a small subset of the MIT scheme language.

# Usage 

Compile with

```
make
```

then use 

```
./bin/scheme
```

to enter the shell or alternatively

```
./bin/scheme <path-to-dot-lisp-file>
```

to interpret an entire file. 

In order to run the built-in test suites:

```
make test 
```

The test suites consist of two parts: the main one is written in scheme itself and tests the functionality of the language, the other one is written in C and tests the basic properties of the number tower. 

# Features

## Core
- number tower
  - fixnums (integers)
  - bignums (arbitrary length integers)
  - floatnums (doubles)
  - ratnums (rational numbers)
  - complex numbers
- closures
- strings 
- booleans `#t`, `#f` (`#f` is the only false value, `nil`/`'()` is not)
- `display` and `newline`
- runtime errors are values! 
- tail calls (proper TCO - function call in tail position reuses the current function’s stack frame instead of creating a new one, preventing stack growth, meaning we can turn recursion into iteration)
- garbage collection (for now a simple mark and sweep gc)

### Special forms
- `quote` (`'`), `quasiquote` (`` ` ``), `unquote` (`,`), `unquote-splicing` (`,@`)
- `if`
- `define`
- `lambda`
- `load` (for multiple file projects)
- `define-macro` (manually-hygienic macros)
- `begin`
- `set!`
- `apply`

### Primitives
 - `+`, `-`, `*`, `/`, `=`, `>`, `<`
 - `number? `
 - `cons`, `car`, `cdr`, `list?`
 - `eq? `
 - `atom?`
 - `null?`
 - `string?`, `string-length`, `string-append`
 - `gensym`
 - `display`
 - `newline`

## Standard library macros
- `cadr`
- `append`
- `let`, `letrec`, `let*` 
- `cond` 
- `and`, `or`, `not` 
- `map`, `reverse`  
- `equal?` 

# Error handling

## Parser Errors
Parser and lexer errors are built into the C core. They do not propagate far through the program, since a badly parsed file is usually not worth interpreting. These should also show debug information about file/line/column location of the error. In the REPL a parsing error looks like this: 

```
Parser Error [<repl>::(1,48)]: missing ')'
(define (hello-world) (display "hello, world!")
                                               ^
```

## Runtime errors 
Runtime errors on the other hand are valid values. In addition to a few built-in runtime error cases, they can also be defined by the user. When encountered, if they are unhandled and reach the top of the program, they pause its execution and print the error message, along with a stack trace of the operations leading up to the error.

As an example, here is `test/stack_trace.scm`: 
```
(define (game-turn-A count)
  (display "Player A's turn. Count is: ") (display count) (newline)

  (if (= count 0)
      (error "Boom! Game over.")
      (game-turn-B (- count 1))))


(define (game-turn-B count)
  (display "Player B's turn. Count is: ") (display count) (newline)
  
  (game-turn-A (- count 1)))


(game-turn-B 3)
```

This code will result in the following output: 

```
Player B's turn. Count is: 3
Player A's turn. Count is: 2
Player B's turn. Count is: 1
Player A's turn. Count is: 0
Runtime Error: Boom! Game over.
Stack trace:
  (error Boom! Game over.)
  (game-turn-A (- count 1))
  (game-turn-B (- count 1))
  (game-turn-A (- count 1))
  (game-turn-B 3)
```


Inspired by MIT 6.001 (1986)
