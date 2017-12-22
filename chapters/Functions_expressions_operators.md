
## Functions, classes and operators {#sec:functions-classes-operators}

Everything you do in R, you do with functions, so if you want to implement a domain specific language, you must do so by writing functions. All the actions your new DSL should support, you must implement using functions, and should you want a special syntax for your DSL, you will have to write functions for parsing this syntax. When implementing an embedded DSL, as we shall see, much of the parsing can be outsourced to R’s parser. The price for this is some restrictions to the syntax for the DSL—the DSL must be syntactically valid R code. We cannot construct arbitrary syntaxes for embedded languages, but by using operator overloading or meta-programming, and by defining new infix operators, we do have some flexibility in designing our DSLs.

In the previous two chapters we have already seen examples of how to use both operator overloading and meta-programming to treat R expressions as expressions in an embedded language. The purpose of this chapter is to go into more detail of the operator option while the next chapter will cover the option of explicitly manipulating expressions through meta-programming.

### The S3 object-oriented programming system

You could write a whole book about the object-oriented programming systems supported by R—I know, because I have written such a book—but for the purpose of operator overloading and implementing DSL parsers, we only need a few of the object-orientation features and this section will give you a quick introduction to those. We will only use the simplest object-orientation system in this book, the S3 system.If you are already familiar with the S3 system, feel free to skip ahead to the next section.

#### Objects and classes



#### Generic functions


#### Group generics

### Precedence and evaluation order

Operator      Usual meaning
--------      -----------------
`[` `[[`	    Indexing
`^`	          Exponentiation (Evaluates right to left)
`-` `+`	      Unary minus and plus
`:`	          Sequence operator
`%any%`       Special operators
`*` `/`	      Multiply, divide
`+` `-`	      Binary add and subtract
`<` `>` 
`<=` `>=`     Ordering and comparison
`==` `!=` 
`!`	          Negation
`&` `&&`	    And
`|` `||`	    Or

