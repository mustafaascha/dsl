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

### Rewriting expressions


[^substitute-global-scope]: The `substitute` function will replace variables by the value they contain in the current scope or in an environment you provide as a second argument, *except* for variables in the global environment. Those variables are left alone. If you experiment with `substitute`, be aware that it behaves differently inside the scope of a function from how it behaves in the global scope.