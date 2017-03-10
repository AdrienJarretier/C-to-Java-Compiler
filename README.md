#C-to-Java-Compile

A compiler to translate C programs to Java bytecode.

The input language is a realistic but restricted subset of C language.


##Build the project

Go into Projet_compil and run make :

````
cd Projet_Compil/
make
````


##Tests

### unit test

There are unit tests in typing.ml and gen.ml at the end of the files.
to run them open Ocaml interpreter.
Copy paste everything that's commented out after **TESTS** into your interpreter

( In the first part of the comment block,
there are the use and open directives, then global definitions,

the actual functions tests are at the end with a clear separation between the tests raising exceptions as expected and the other ones.
)



