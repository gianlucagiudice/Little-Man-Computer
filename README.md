# Little Man Computer

Gianluca Giudice 830594
Implementation of "little man computer" by Gianluca Giudice in prolog and common lisp.

## File structure

### Prolog

- **little_man_computer** Entry point for the program
- **compiler** Compile assembly file
- **util** Some handy wrappers
- **executer** Execute the content of the memory

### Lisp

- **little-man-computer** The whole LMC implementation

## Notes

- In the lisp implementation, the function "lmc-run" contains *(compile 'execution-loop)* in order to optimize tail recursion and avoid stack overflow
- A label can be ANYTHING that isn't a number (1label IS a label, 1 is NOT)
- A label cant't be defined with the same name as any of the instructions

## Version

- **Prolog** SWI-Prolog 7.6.4 on Ubuntu 18.10
- **Lisp** Clisp 2.49.92 (2018-02-18) on Ubuntu 18.10