# chip8

Common Lisp implementation of a CHIP-8 emulator.

## Development

### Dependencies

Install [quicklisp](https://www.quicklisp.org/beta/).

### Loading the Project

At the Slime REPL, assuming quicklisp, load the project using:
```common-lisp
(ql:quickload :chip8)
```

To run the tests, first load them:
```common-lisp
(ql:quickload :chip8/tests)
```

Run the tests using [`rove`](https://github.com/fukamachi/rove):
```common-lisp
(rove:run :chip8/tests)
```
