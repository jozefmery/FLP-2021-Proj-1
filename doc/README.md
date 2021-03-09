File:     README.md  
Author:   Jozef Méry - xmeryj00@vutbr.cz  
Project:  FLP-2021-xmeryj00-simplify-bkg  
Date:     3.3.2021

## Description

Console application for simplifying a text-represented context-free grammar.
The simplification is based on a formal language theory algorithm,
which has been translated into haskell in a 1:1 manner with one notable
exception. Checking if a rule right-side is in a given set's iteration is done
by converting the rule right-side to a character set and checking if this
set is a subset of the given set for simplicity and efficiency.
The only possible error source in the application is either wrong flag
usage or an invalid input grammar, i.e. the algorithm cannot fail on
any valid grammar.

## Building the application (run the following command):

  make

## Running the application:

  simplify-bkg [-h] {-i | -1 | -2} [<input_file>]

| Option      | Description                                                   |
|:-           |:-                                                             |  
| -h, --help  | Prints help.                                                  |
| -i          | Prints loaded and parsed grammar from internal representation <br /> without modifications. Order of elements may differ. |
| -1          | Executes the first step of the simplification algorithm <br /> (removing rules that do not generate finite words), and prints the resulting grammar. |
| -2          | Executes the second step of the simplification algorithm <br /> (removing inaccessible symbols), and prints the resulting grammar. |
| input_file | Optional file name to read the input grammar from. If not provided, <br /> stdin is read instead. |

At least one of the stage flags (-i, -1, -2) has to be used.

## I/O Grammar description

The grammar is represented using ASCII text. Only unix-style *EOL* character is accepted.
Input lines have the following format:
1. (**N**) unique comma separated non-terminal capital characters
2. (**T**) unique comma separated non-terminal lowercase characters
3. (**S**) initial non-terminal belonging to N 
4. (**P**) 0 to M unique new-line separated rules with the following format: <br />*A->X or A->#*, where A belongs to N, X belongs to *(N u T)+*, <br /> and # is a special empty string character (Epsilon). Empty lines between rules are ignored.
   
Valid grammar example:

  A,B,C,S
  a,b,c
  S
  S->#

  A->a
  A->aAaB
  
  A->#
  B->b
  C->c
  C->#
  S->AB
  S->#