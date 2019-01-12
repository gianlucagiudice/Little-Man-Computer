# Little Man Computer

Gianluca Giudice 830694
Implementation of "little man computer" by Gianluca Giudice in Prolog and Common Lisp.

## File structure

### Prolog

- **lmc** Entry point for the program
- **compiler** Compile assembly file
- **executer** Execute the content of the memory

### Lisp

- **lmc** The whole LMC implementation

## Notes

- In the lisp implementation, the function "lmc-run" contains *(compile 'execution-loop)* in order to optimize tail recursion and avoid stack overflow

## Version

- **Prolog** SWI-Prolog 7.6.4 on Ubuntu 18.10
- **Lisp** Clisp 2.49.92 (2018-02-18) on Ubuntu 18.10
