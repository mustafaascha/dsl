
# Functions, classes and operators {#sec:functions-classes-operators}

Everything you do in R, you do with functions, so if you want to implement a domain specific language, you must do so by writing functions. All the actions your new DSL should support, you must implement using functions, and should you want a special syntax for your DSL, you will have to write functions for parsing this syntax. When implementing an embedded DSL, as we shall see, much of the parsing can be outsourced to R’s parser. The price for this is some restrictions to the syntax for the DSL—the DSL must be syntactically valid R code. We cannot construct arbitrary syntaxes for embedded languages, but by using operator overloading or meta-programming, and by defining new infix operators, we do have some flexibility in designing our DSLs.

In the previous two chapters we have already seen examples of how to use both operator overloading and meta-programming to treat R expressions as expressions in an embedded language. The purpose of this chapter is to go into more detail of the operator option while the next chapter will cover the option of explicitly manipulating expressions through meta-programming.

## The S3 object-oriented programming system

You could write a whole book about the object-oriented programming systems supported by R—I know, because I have written such a book—but for the purpose of operator overloading and implementing DSL parsers, we only need a few of the object-orientation features and this section will give you a quick introduction to those. We will only use the simplest object-orientation system in this book, the S3 system.If you are already familiar with the S3 system, feel free to skip ahead to the next section.

### Objects and classes

The S3 system has a very simple approach to object-orientation. It lets us associate classes with any object (except for `NULL`), but classes are just text strings and there is no structure to them. You can get the classes associated to any object by calling the function `class`:

```{r}
class(4)
class("foo")
class(TRUE)
class(sin)
```

You can set the class of an object with the corresponding assignment function

```{r}
class(sin) <- "foo"
class(sin)
```

without this in other ways affecting what the object is or does

```{r}
sin(0)
```

There are some limits to which objects you can change the class of—you cannot change the class of literal numbers and strings, for example, so if you attempted this

```r
class(2) <- "character"
class("foo") <- "numeric"
```

you would would get errors. Still, you are generally free to modify the class of objects at any time you want. The class associated with an object is just an object attribute containing one or more class names—in the cases above, all the objects have a single class, but the matrix expressions we created [Chapter @sec:matrix-expressions], for example, had multiple classes. Vectors of class names are used both for multiple inheritance and single inheritance, and there is not formal class structure at all. You can set the classes any way you want. The class attribute is just a vector of strings that are interpreted as class names.

The only thing that makes the class attribute interesting, compared to any other attributes you could associate with objects, is how it is used for dispatching generic functions. When you call a generic function, the actual function that gets called depends on the class of one of its arguments, usually the first (and by default the first).

### Generic functions

The generic function mechanism in the S3 system is implemented in the `UseMethod` and `NextMethod` functions. To define a generic function, `foo`, you would for example write:

```{r}
foo <- function(x, y, z) UseMethod("foo")
```

Calling `foo` would then invoke `UseMethod` that would search for concrete implementations of `foo`. Such functions are identified by their name alone—any function whose name starts with `foo.` is considered an implementation of `foo`, and the one that will be chosen depends on the class of the argument `x`. We haven’t *any* implementations of `foo` yet, so calling the function will just give us an error for now.

```{r}
foo(1, 2, 3)
```

However, we can make a default implementation. The default implementation for a generic function—the one that will be used if `UseMethod` can find no better matching implementation—has a name that ends with `.default`. We can implement

```{r}
foo.default <- function(x, y, z) {
   cat("default foo\\n")
}
```

and this function will be invoked if we call `foo` with an object that doesn’t have a better implementation

```{r}
foo(1, 2, 3)
```

To specialise a generic function to specific classes, we just have to define functions with appropriate names. Any function name that begins with `foo.` can be used and will be called if we call `foo` with an object of the appropriate class. To specialise `foo` to `numeric` values, for example, we could write

```{r}
foo.numeric <- function(x, y, z) {
   cat("numeric\\n")
}
```

and now this function will be called if `x` is numeric

```{r}
foo(1, 2, 3)
```

A quick comment here, before we continue exploring how dispatching is done on generic functions for user-defined classes: when we used `UseMethod` in the definition of `foo` we called it with the name “foo”. This is why it looks for that name when it searches for implementations of the generic function. We could have asked it to search for other functions, the name we give the generic function when we defined `foo` is not what determines what we search for when we call the function—that is determined by the name we give `UseMethod`. Further, we have seen it dispatch on the first argument of `foo`, but this is just a default. We *could* give `UseMethod` another object to dispatch on.

```{r}
bar <- function(x, y, z) UseMethod("foo", y)
```

I do not recommend doing this—it goes against the conventions used in R—but it is possible and with this function we would dispatch on the `y` argument (and search for the generic function `foo`, not `bar`).

```{r}
foo("foo",2,3)
bar("foo",2,3)
bar(1,"bar",3)
```

Ok, back to the rules for dispatching. When `UseMethod` is called, it starts to search for functions based on their name. It will take the classes of an object and search those in order. If it doesn’t find any matching function, it will call the default, if it exists.

So, let us consider again `foo` where we have a numeric and a default implementation.

```{r}
x <- 1
foo(x, 2, 3)
```

Here, we have created the object `x` which is numeric, so when we call `foo`, we match the numeric function. But we can chance the class of `x` and see what happens.

```{r}
class(x) <- c("a", "b", "c")
foo(x, 2, 3)
```

Now, because `x` has the classes “a”, “b”, and “c”, but not “numeric”, `UseMethod` doesn’t find the numeric version but hits the default one. We can, of course, define functions for the other classes and see what happens.

```{r}
foo.a <- function(x, y, z) cat("a\\n")
foo.b <- function(x, y, z) cat("b\\n")
foo.c <- function(x, y, z) cat("c\\n")
foo(x, 2, 3)
```

Because we now have functions for `x`’s classes, we can find them, and because “a” is the first class, that is the one that will be called. If we change the order of `x`’s classes, we call the other functions—`UseMethod` always calls the first it finds.

```{r}
class(x) <- c("b", "a", "c")
foo(x, 2, 3)

class(x) <- c("c", "b", "a")
foo(x, 2, 3)
```

When calling `UseMethod` we will find and call the first matching function. The related `NextMethod` function can be invoked to find and call the next function in the chain of classes. To see it in action, we can redefine the three “a”, “b”, and “c”, `foo` implementations and make them call the next function in line.

```{r}
foo.a <- function(x, y, z) {
  cat("a\\n")
  NextMethod()
}
foo.b <- function(x, y, z) {
  cat("b\\n")
  NextMethod()
}
foo.c <- function(x, y, z) {
  cat("c\\n")
  NextMethod()
}
```

They will all call the next function in line, and since we have a default implementation of `foo`, the last in line will call that one. The order in which they are called depend entirely on the order of `x`’s classes.

```{r}
class(x) <- c("a", "b", "c")
foo(x, 2, 3)

class(x) <- c("b", "a", "c")
foo(x, 2, 3)

class(x) <- c("c", "b", "a")
foo(x, 2, 3)
```

This generic dispatch mechanism is obviously extremely flexible, so it can require some discipline to ensure a robust object-oriented design. For the purpose of implementing domain-specific languages, though, we are just interested in how we can use it to add operators to our language.

### Operator overloading

Most operators, with exceptions such as assignments and slot and component access (`@` and `$`), behave as generic functions and can be overloaded. The mechanism for overloading operators is not different from the mechanism for implementing new version of generic functions—if you name a function the right way, it will be invoked when a generic function is called.

For example, to define addition on “a” objects, we need to define the `+.a`, which we could do like this:

```{r}
`+.a` <- function(e1, e2) {
  cat("+.a\\n")
  NextMethod()
}
x + 2
```

Here, we just print some output and then invoke the underlying numeric addition—since the object `x` here is a numeric value as well as an object of class “a”—by invoking `NextMethod`. It is important that we use `NextMethod` here. If we just used addition again, we would be calling `+.a` once again. So do not attempt this:

```r
`+.a` <- function(e1, e2) {
  cat("+.a\\n")
  e1 + e2
}
```

This is an infinite recursion and not what you want.

You overload operators exactly like you would define specialisations of generic functions, but there *are* a few differences between the two and how they dispatch. With normal generic functions, you would dispatch based on the first argument. With operators, there are a few heuristics that are there to make sure that the same function is called regardless of the order of the operands. You could easily imagine the pain it would be to debug programs where, switching the order of the operands in addition, would call completely different functions. That doesn’t happen in R, because operations are not *exactly* the same as other generic functions.

If we have defined `+.a` and we try to add a number to `x`, then we can do that in either order, and it will be the `+.a` function that will be called.

```{r}
x + 3
3 + x
```

This is also the case if we add to `x` an object of a different class

```{r}
x <- 1 ; y <- 3
class(x) <- "a"
class(y) <- "b"
x + y
y + x
```

*unless* we have also defined an addition operator for that class.

```{r}
`+.b` <- function(e1, e2) {
  cat("+.b\n")
  NextMethod()
}

x + y
y + x
```

If both “a” and “b” have their own version of addition, then we need a way to resolve which version `x+y` and `y+x` should call. Here, the first operand takes precedence, so it determines which function is called—and you get a well deserved warning for getting up to such shenanigans.

You *might* be able to think up some other rules for how you would want such situations to be resolved. For example, you could say that the most abstract method should be called, so if both `x` and `y` were of class “b” but only `x` was also of class “a”, then we should call `+.b`.

```r
class(x) <- c("a","b")
class(y) <- "b"
```

Unfortunately, you cannot make such rules in the S3 system. It is possible with the S4 system, where you can dispatch generic functions based on multiple arguments, but that goes beyond the scope of this book. We will simply make sure to avoid situations where we have to add different classes of objects that define the same operators—and by constructing grammars appropriately, this is not a problem.

Notice, though, that the combination of multiple classes and `NextMethod` still works as before. We do not have a good method, in the S3 system, to dispatch based on multiple arguments, but we can use `NextMethod` to invoke several methods as we evaluate an operator. If we invoke `+.a` on object `x`, which now has classes “a” and “b”, the call to `NextMethod` in the implementation of that function will invoke `+.b` before *that* function invokes the numeric addition.

```{r}
class(x) <- c("a", "b")
x + 2
x + y
```

R has unary operators as well as binary operators. The negation operator, `!`, only exists in a unary operator, and you can specialise it for a given class by defining a function that takes as single argument.

```{r}
`!.a` <- function(x) {
  cat("Not for a\\n")
  NextMethod()
}
!x
```

For the `-` and `+` operators, though, we have the same symbol used for both unary and binary operators. Here, we have to use the same function for both—since the only way we identify operator functions is through their name and that would be the same for the unary and binary operators. The way we can determine if the function is called as part of a unary or binary operator is to test if the second argument is missing. If it is, we have a unary operator, if it isn’t, it is binary.

```{r}
`+.a` <- function(e1, e2) {
  if (missing(e2)) {
    cat("Unary\\n")
  } else {
    cat("Binary\\n")
  }
  NextMethod()
}

class(x) <- "a"
+x
2+x
```


### Group generics

There is another way you can overload operators based on their operands’ class: group generics. Group generics, as the name hints, groups several operators. They provide a way for us to define a single function that handles all operators of a given type. For arithmetic and logical operators, "+", "-", "*", "/", "^", "%%", “%/%”, "&", "|", “!”, "==", "!=", "<", "<=", ">=", and “>”, the relevant group generic is `Ops`.

If we define `Ops.c`, then we define function that will be called for all of these operators when used on an element of class “c”.

```{r}
Ops.c <- function(e1, e2) {
  cat(paste0("Ops.c (", .Generic, ")\\n"))
  NextMethod()
}

z <- 2
class(z) <- "c"
z + 1
1 + z
z ^ 3
```

The “magical” variable `.Generic` contains the name of the operator that is actually called, and calling `NextMethod` will dispatch to the relevant next implementation of the operator.

If you implement both the `Ops` group generic *and* concrete implementations of some individual operator generics, then the latter takes precedence. If, for example, we have an object of class “a” *and* “c”, where we have defined addition for class “a” and have the group generic for “c”, the addition will invoke the `+.a` function and all other operators will invoke the `Ops.c` function.

```{r}
class(z) <- c("a", "c")
1 + z
2 * z
```

With `Ops` you have a method for catching all operators that you do not explicitly write specialised generics for.

## Precedence and evaluation order

As soon as we start working with operators, their precedence become important. The syntax for normal function calls makes the evaluation order relatively clear—with nested function calls we have inner and outer functions in an expression, and while that does not give us guarantees about which order parameters to a function will be evaluated in, we do know that arguments to a function will be evaluated before the function itself.[^lazy-eval] With operators, the syntax does not tell us in which order functions will be called. To know that, we need to know the precedence rules.

Precedence rules tell us the order in which operator functions get called by ordering the operators from highest to lowest precedence and by specifying if operators are evaluated from left to right or from right to left. In an expression such as

```r
x + y * z
```

we know that the multiplication, `y * z`, is evaluated before the addition, so the expression is equivalent to

```r
x + (y * z)
```

because multiplication has higher precedence than addition. With operators at the same level of precedence we might be less aware of the order, but for those, the left-to-right or right-to-left order is also guaranteed by precedence rules. Of the operators you can overload, only the exponentiation operator evaluates right-to-left while all the others evaluate left-to-right, so

```r
x ^ y ^ z
```

is equivalent to

```r
x ^ (y ^ z)
```

while 

```r
x * y / z
```

will be evaluated as

```r
(x * y) / z
```

The operators you can overload are listed, from highest to lowest precedence, below:

Operator      Usual meaning
--------      -----------------
`[` `[[`	    Indexing
`^`	          Exponentiation (Evaluates right to left)
`-` `+`	      Unary minus and plus
`%any%`       Special operators
`*` `/`	      Multiply, divide
`+` `-`	      Binary add and subtract
`<` `>` 
`<=` `>=`     Ordering and comparison
`==` `!=` 
`!`	          Negation
`&` `&&`	    And
`|` `||`	    Or
`:=`          Assignment

In the graph specifications language from the previous chapter we could use addition and `%=>%` together because user-defined infix operators—those defined using percentages symbols—have higher precedence than `+`, so we would construct edges before we would add them to a graph. Using `>` and `|` together works for the same reason. Further, because addition (or logical or) is evaluated from left to right, the `dag()` object we created at the beginning of a graph specification would be added to the first edge, which would produce another graph that then would be added to the next edge and so forth. If the evaluation of addition was right-to-left, we would be adding edges to edges, instead of graphs to edges, which would complicate the implementation of the parser.

The last operator in the table, the `:=` assignment operator, is special in the list. It isn’t actually an operator that is defined in R. You cannot use it as an assignment operator—for that you should use `<-` or `->`—but the R parser recognises it as an infix operator which means that you can use it when you design a domain-specific language.



[^lazy-eval]: I am not being entirely honest here. R has lazy evaluation, so there is no guarantee that arguments to a function will be evaluated at all—but if they are, they will be evaluated before we return from the function they are arguments to, so conceptually we can think of them as being evaluated before we call the function.



## Code blocks

A final syntactical component that can be very useful when designing a domain-specific language isn’t an operator but the braces that constructs a block of code. We cannot overload how these are interpreted, but we can certainly find use for them when we create a new language. Before we can exploit them fully, we need to know both how to manipulate expressions and how to evaluate them in different contexts—which we cover in the next chapter and in [Chapter @sec:env_and_expr]—but as a quick example, consider creating an index operator that repeats a statement a number of times. We can define it this way:

```{r}
`%times%` <- function(n, body) {
  body <- substitute(body)
  for (i in 1:n)
    eval(body, parent.frame())
}
```

The 

```r
  body <- substitute(body)
```

takes the `body` argument and changes it to an expression so we can evaluate it repeatedly. If we didn’t do this, we would only evaluate it the first time accessed `body`—this is how R’s lazy evaluation works—but using `substitute` we can get the verbatim expression out of the argument. To evaluate it, we then have to use the `eval` function, and to evaluate it in the context where we call the `%times%` operator, we need the calling frame, which we get using `parent.frame` (see [Chapter @@sec:env_and_expr] for more details on evaluation and environments).

The `body` argument to `%times%` can be a single statement

```{r}
4 %times% cat("foo\\n")
```

but since `{}` are considered expressions as well, we can also use a sequence of statements as long as we wrap them in braces:

```{r}
2 %times% {
  cat("foo\\n")
  cat("bar\\n")
}
```

Because we can use braces to pass blocks of code as arguments to functions, we can use these to create new control structures, like the `%times%` operator above. To fully exploit this, however, we need to understand how we evaluate general expressions in R, and in particular how we control the environment in which we evaluate them, and we need to parse and manipulate expressions to analyse blocks of code and maybe modify them. We will therefore leave further discussion of braces until we have covered those topics.
