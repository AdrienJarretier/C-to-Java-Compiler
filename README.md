#C-to-Java-Compiler

## Author

* **Adrien Jarretier** - [C-to-Java-Compiler](https://github.com/AdrienJarretier/C-to-Java-Compiler)

---

A compiler to translate C programs to Java bytecode.

The input language is a realistic but restricted subset of C language.

*For this project I worked in collaboration with Laurie Houseaux*


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


### java jasmin bytecode generation

this is handled by the file : Tests/testsExpr.ml

In the *Ocaml interpreter* paste

````
#use "use.ml";;
open Lang;;
#use "Tests/testsExpr.ml";;
exit 0;;
````

This is executing the file which is calling :
	- **tp_expr** to type the expressions at the beginning of the files (a, b and aTimesb)
	- **gen_expr** to generate a list of intructions
	- **pr_instrs** to generate the string in java bytecode corresponding to this list of instructions.

at the end the ouput is written to **Tests/TestsExpr.j**

This file can be compiled with jasmin
````
cd Tests/
java -jar jasmin.jar TestsExpr.j
````

Then tested with WrapperTestsExpr
````
javac WrapperTestsExpr.java
java WrapperTestsExpr
````

The wrapper is filling the register by giving 9, 3, 6 and 12 to the method
which correspond to (3 - 2) * (6 - (12 + 2)) = -8
