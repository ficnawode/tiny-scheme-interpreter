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

In order to run the tests (written in scheme itself):
```
make test 
```

# Features

## Core
- integers
- closures
- strings 
- booleans `#t`, `#f` (`#f` is the only false value, `nil`/`'()` is not)
- `display` and `newline`
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

Inspired by MIT 6.001 (1986)
