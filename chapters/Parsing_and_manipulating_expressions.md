## Parsing and manipulating expressions {#sec:parsing_and_manipulating_expressions}

A powerful feature of the R programming language is that it readily allows us to treat expressions in the language itself as data that we can examine and modify as part of a program—so-called “meta-programming”. From within a program we can take a piece of R code and computationally manipulate it before we evaluate it. We just need to get hold of the code *before* it is evaluated, but there are several ways to do that. The simplest is to “quote” expressions, which leaves them as unevaluated expressions.

### Quoting and evaluating

If you write an expression such as

```r
2 * x + y
```

R will immediately try to evaluate it. It will look for the variables `x` and `y` in the current scope and if it finds them it will evaluate the expression, if it does not, it will report an error. By the time R has evaluated the expression we either have a value or an error, and if the former, the expression is essentially equivalent to the result of evaluating the expression (computation time not withstanding). A literate expression as this one is not something we can get a hold on in a program—we either get an error or the value it evaluates to. If we want to get hold of the actual expression, we need to “quote” it. If we wrap the expression in a call to the function `quote`, then we proven the evaluation of the expression and instead get a data structure that represent the un-evaluated expression.

```{r}
quote(2 * x + y)
```

The class of an expression is a “call”

```{r}
expr <- quote(2 * x + y)
class(expr)
```

This is because infix operators are syntactic sugar for function calls and all function call expressions will have this time. For “call” objects we can get their components by indexing as we would a list. The first element will be the function name and the remaining elements the arguments to the function call. For binary operators there will, of course, be two arguments.

For this expression, the function call is addition. 

```{r}
expr[[1]]
expr[[2]]
expr[[3]]
```

This is because multiplication has higher precedence than addition, so the expression is equivalent to

```r
(2 * x) + y
```

so the multination is nested deeper in the expression than the addition—the multiplication can be accessed as the first argument to the addition call, so the second element in the object:

```{r}
expr[[2]][[1]]
expr[[2]][[2]]
expr[[2]][[3]]
```

To evaluate a quoted expression, we can use the function `eval`. The expression

```r
eval(quote(2 * x + y))
```

is equivalent to writing the literate express

```r
2 * x + y
```

The `eval` function provides more flexibility in how an expression is evaluated since we can modify the scope of the evaluation, something we return to in much more detail in [Chapter @sec:env_and_expr].

Combining quoted expressions and functions introduces a few complications, at least if we want to handle the quoting within a function call. We can, of course, always pass quoted expressions as parameters to a function

```{r}
f <- function(expr) expr[[1]]
f(quote(2 * x + y))
```

but if we want to provide the literate expression to the function it gets more complicated

```{r}
f(2 * x + y)
```

In the function `f`, when we return `expr[[1]]`, R will first attempt to evaluate the expression, but the expression depends on variables `x` and `y` that are undefined. Even if we define `x` and `y`, we still do not get a “call” object that we can manipulate. We simply get the result of evaluating the expression.

```{r}
x <- 2
y <- 3
f(2 * x + y)
```

Using `quote` inside the function doesn’t help us. If we write `quote(expr)` we get the expression `expr`—a single symbol—as a result, not the argument we give to `f`. 

```{r}
f <- function(expr) {
  expr <- quote(expr)
  expr[[1]]
}
f(2 * x + y)
```

To get the actual argument as a quoted expression, we need to use the function `substitute`.

```{r}
f <- function(expr) {
  expr <- substitute(expr)
  expr[[1]]
}
f(2 * x + y)
```

Two things come together to make this work. First, function arguments in R are lazy evaluated, so the `expr` argument is never evaluated if we do not use it in an expression. So, even though `x` and `y` are not defined, we do not get any errors as long as we do not evaluate the argument to `f`. Second, `substitute` does not evaluate its argument but return a quoted object where variables are replaced with the value they have in the current scope.[^substitute-global-scope] The argument to `substitute` does not have to be a single variable name. It can be any expression that will be considered quoted after which variable substitution is done, and the return value will be the modified quoted expression.

```{r}
f <- function(expr) {
  expr <- substitute(expr + expr)
  expr
}
f(2 * x + y)
```

Another complication appears if we attempt to evaluate a quoted expression inside a function. You might expect these two functions to be equivalent, since `eval(quote(expr))` should be the same as `expr` but they are *not* equivalent.

```{r}
f <- function(expr) {
  expr + expr
}
g <- function(expr) {
  x <- substitute(expr + expr)
  eval(x)
}
```

If we make sure that both `x` and `y` are defined, then the function `f` returns twin the value of the expression.

```{r}
x <- 2; y <- 3
f(2 * x + y)
```

Function `g`, on the other hand, raises an error because the type of `x` is incorrect.

```{r}
g(2 * x + y)
```

The `eval` function will, by default, evaluate an expression in the current scope, which inside a function in that function’s scope. Inside `g`, we have defined `x` to be the expression we get from the call to `substitute`, so it is *this* `x` that is seen by `eval`. If you want `eval` to evaluate an expression in another scope, you need to give it an environment as a second argument. If you want it to evaluate the expression in the scope where the function is *called*, rather than inside the function scope itself, then you can get that using the `parent.frame` function:

```{r}
g <- function(expr) {
  x <- substitute(expr + expr)
  eval(x, parent.frame())
}
g(2 * x + y)
```

We will discuss environments, scopes, and how expressions are evaluated in much more detail in [Chapter @sec:env_and_expr]. For the remainder of this chapter, we will focus on manipulating expressions and not evaluating them.


### Exploring expressions

An expression is a recursive data structure and you can explore it as such. We can define expressions in terms of a grammar as this:

```
EXPRESSION ::= CONSTANT
            |  NAME
            |  PAIRLIST
            |  CALL EXPRESSION_LIST
EXPRESSION_LIST 
           ::= EXPRESSION 
            |  EXPRESSION EXPRESSION_LIST
```

Of course, for expressions we do not really have a grammar for constructing as such—we use R code—but this is the type declaration for expressions. All expressions are one of the four, and when it is a call there will be other expressions involved. We can explore expressions using recursive functions where the first three meta-variables, `CONSTANT`, `NAME`, and `PAIRLIST` are basic cases and the third, `CALL` is the recursive call.

Of the meta-variables, `CONSTANT` refers to any literal data such as numbers or strings, `NAME` to any variable name, `PAIRLIST` refers to formal arguments in function definitions—more on this below—and CALL to function calls. Function calls captures everything more complex than the first three options since everything in R that does anything is considered a function call, including such statements as function definitions and control structures. As we saw above, calls are list-like and always have at least one element. The first element is the function that is being called and the remaining elements the arguments to that function.

To recursively explore an expression we can write functions that test the four cases. Constants are recognised by the `is.atomic` function, names by the `is.name` function, pair lists by the `is.pairlist` and calls by the `is.call` function. A function for printing out an expression’s structure can look like this:

```{r}
print_expression <- function(expr, indent = "") {
  if (is.atomic(expr)) {
    if (inherits(expr, "srcref")) {
      expr <- paste0("srcref = ", expr)
    }
    cat(indent, " - ", expr, "\\n")
    
  } else if (is.name(expr)) {
    if (expr == "") {
      expr <- "MISSING"
    }
    cat(indent, " - ", expr, "\\n")
    
  } else if (is.pairlist(expr)) {
    cat(indent, " - ", "[\\n")
    new_indent <- paste0(indent, "       ")
    vars <- names(expr)
    for (i in seq_along(expr)) {
      cat(indent, "    ", vars[i], " ->\\n")
      print_expression((expr[[i]]), new_indent)
    }
    cat(indent, "    ]\\n")
    
  } else {
    print_expression((expr[[1]]), indent)
    new_indent <- paste0("  ", indent)
    for (i in 2:length(expr)) {
      print_expression(expr[[i]], new_indent)
    }
  }
}
```

Here, we do not explicitly test for the type of calls—if the expression is not one of the first three cases it must the fourth. There are two special cases we handle in this printing expression—source references for function definitions and missing expressions in pair lists. We discuss these below.

We can see the function in action by calling it on the expression we explored above:

```{r}
print_expression(quote(2 * x + y))
```

The pretty-printed expression shows the structure we explored explicitly in the last section.

Declaring a function is considered a function call—a call to the function `function`:

```{r}
print_expression(quote(function(x) x))
```

For a function definition, we have a call object where the first argument is `function`, the second argument is the pair list that defines the function parameters, and the third element is the function body—another expression.  There is also a fourth element, however, called “srcdef”. This is an atomic vector that captures the actual code used to define the function. In the printing function we just print the text representation of the source definition, which we get by pasting the expression.

The argument list of a function we declare is where the pair list data structure is used—and only here. We can get the names of the formal parameters using the `names` function and the default arguments by indexing into the pair list. Parameters without default arguments are a special case here—the expression they contain is an empty string. In the printing function we make this explicit by changing the empty string to the string `MISSING`. If we have default arguments, then those are represented as expressions we can explore recursively.

```{r}
print_expression(quote(function(x = 2 * 2 + 4) x))
print_expression(quote(function(x, y = 2 * x) x + y))
```

The usual case for function calls is that the first element in the “call” list is a symbol that refers to a function, but any expression that returns a function can be used as a function in R, so the first element of calls can be any expression. For example, if we define a function and call it right after, the first element of the call object will be the function definition.

```{r}
expr <- quote((function(x) x)(2))
print_expression(expr)
expr[[1]]
expr[[2]]
```

As an example of doing something non-trivial with expressions, we can write a function that collects all unbound variables in an expression. If we simply recurse through an expression we can collect all the symbols—bound or unbound. To only get the unbound variables, we can keep track of those that are bound and not collect those. Ignoring, at first, those variables that might be bound outside of the expression itself—in the scope where we will call the function—the variables that are bound are those that are named in a function definition. Those, we can recognise as part of the pair list that is the second argument to calls to `function`. When recursing over expressions, we capture those and pass them on down the recursion. Otherwise, we simply collect the symbols. In the implementation below I use the linked lists we have seen earlier to collect the symbols and I translate the symbols into characters when I collect them. I do this because I can use the character representation of symbols to check if a symbol exist in an environment later on.

```{r}
collect_symbols_rec <- function(expr, lst, bound) {
  if (is.symbol(expr) && expr != "") {
    if (as.character(expr) %in% bound) lst
    else cons(as.character(expr), lst)
    
  } else if (is.pairlist(expr)) {
    for (i in seq_along(expr)) {
      lst <- collect_symbols_rec(expr[[i]], lst, bound)
    }
    lst
    
  } else if (is.call(expr)) {
    if (expr[[1]] == as.symbol("function"))
      bound <- c(names(expr[[2]]), bound)
    
    for (i in 1:length(expr)) {
      lst <- collect_symbols_rec(expr[[i]], lst, bound)
    }
    lst
    
  } else {
    lst
  }
}
```

When we collect symbols, we explicitly avoid the empty symbol. This is the symbol we get when we recurse on a pair list for a function parameter without a default value, and we do not consider this a variable, bound or otherwise. Other than that, the way we handle symbols is straight forward. For pair lists we collect the parameters that will be bound and recurse through the default arguments to collect any unbound variables there, and for calls we handle the function definitions by extending the list of bound variables and then recursing. For anything else—which in practice mean for any atomic value—we just return the list we called the function with. There are no unbound variables in constant values.

The recursive function works on a quoted expression and collect all symbols that are not bound within the expression itself. We wrap it in a function that does the quoting of the expression, call the recursive function, and then remove the symbols that are defined in the calling scope (the `parent.frame`).

```{r}
collect_symbols <- function(expr) {
  expr <- substitute(expr)
  bound <- c()
  lst <- collect_symbols_rec(expr, NULL, bound)
  lst %>% lst_to_list %>% unique %>% 
          purrr::discard(exists, parent.frame()) %>%
          unlist
}
```

Here, I use the `discard` function from the `purrr` package to remove all elements that satisfy a predicate, and for the predicate I use the function `exists` with a second argument that is the calling environment, `parent.frame`. This gets rid of symbols that are defined in the scope where we call `collect_symbols`, including globally defined functions such as `*`, `+` and `function`. We *could* collect all defined variables and pass them along in the `bound` variable, but collecting all symbols is more work than removing symbols afterward with the combination of `discard` and `exists`.

I pipe the final result through `unlist` to translate the `list` into a character vector. This is only for pretty printing reasons. It gives nicer output to show in the book—for programming, you can work with `list`s as easily as vectors.

If we get rid of variables `x` and `y` that we have defined above, the expression `2 * x + y + z` should have three unbound variables, `x`, `y`, and `z`, and indeed that is what we find:

```{r}
rm(x) ; rm(y)
collect_symbols(2 * x + y + z)
```

If we define one of the variables, for example `z`, then that is no longer unbound.

```{r}
z <- 3
collect_symbols(2 * x + y + z)
```

Function definitions also bind variables, so those are not collected:

```{r}
collect_symbols(function(x) 2 * x + y + z)
collect_symbols(function(x) function(y) f(2 * x + y))
```

(The value `character(0)` is an empty vector).

Default values can contain unbound variables, and those we collect:

```{r}
collect_symbols(function(x, y = 2 * w) 2 * x + y)
```



### Manipulating expressions


[^substitute-global-scope]: The `substitute` function will replace variables by the value they contain in the current scope or in an environment you provide as a second argument, *except* for variables in the global environment. Those variables are left alone. If you experiment with `substitute`, be aware that it behaves differently inside the scope of a function from how it behaves in the global scope.