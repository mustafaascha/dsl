
## Functions, classes and operators {#sec:functions-classes-operators}

Everything you do in R, you do with functions, so if you want to implement a domain specific language, you must do so by writing functions. All the actions your new DSL should support, you must implement using functions, and should you want a special syntax for your DSL, you will have to write functions for parsing this syntax. When implementing an embedded DSL, as we shall see, much of the parsing can be outsourced to R’s parser. The price for this is some restrictions to the syntax for the DSL—the DSL syntactically valid R code. We cannot construct arbitrary syntaxes for embedded languages, but by using operator overloading and by defining new infix operators we do have some flexibility in designing our DSLs.
