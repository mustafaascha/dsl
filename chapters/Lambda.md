# Lambda expressions

With the techniques we have seen so far, we are now able to implement some useful domain-specific languages. In this chapter, we examine a toy example, lambda-expressions, something we would probably not use in real-world code, but which is an excellent example of code doing something potentially useful and a chance for us to experiment with syntax.

## Lambda expressions

Lambda expressions are anonymous functions, i.e., functions we have not named. In R, we already have anonymous functions. It is actually the default kind of functions—whenever we define function it is anonymous until we assign it to a variable. If we do not wish to shave a function in a variable to get access to it later, we can just use the `function` expression to create it where we need it. For example, to map over a vector of numbers we could write:

```{r}
sapply(1:4, function(x) x**2)
```

It is a toy example, of course, since vector expressions preferable in situations like this

```{r}
(1:4)**2
```

but it illustrates the point.

Using `function` expressions is very verbose, so we might want to construct an alternative syntax for anonymous functions and we can use that wish as an exercises in constructing a domain specific language. Our goal is to change the `sapply` syntax above into this syntax:

```r
sapply(1:4, x := x**2)
```

We use the `:=` assignment operator for two reasons. One, we can overload it, something we cannot do with `->` or `<-`, and two, it has the lowest precedence of the operators, so the operator we create will be called with the left- and right-hand sides before these are evaluated.

To implement this syntax, we need to make the left hand side of assignments into function headers—which means pair lists of arguments—and we need to make the right hand side into a function body we can evaluate in the environment where we define the lambda expression. The good news is, that this only involves techniques we have already seen. We can write a function for turning a list of arguments into a pair list we can use to define the formal arguments of a function as this:

```{r}
make_args_list <- function(args) {
  res <- replicate(length(args), substitute())
  names(res) <- args
  as.pairlist(res)
}
```

For the assignment operator, we need to use `substitute` to avoid evaluating the two arguments, we then use `make_args_list` to turn the left hand side into formal arguments but we keep the right hand side as the expression it is. We then turn the combination into a function using `new_function` from the `rlang` package. Since we want to evaluate the new function in the scope where we define the lambda expression, we use `caller_env` to get this environment and provide it to `new_function`. The entire implementation is as simple as this:

```{r}
`:=` <- function(header, body) {
  header <- substitute(header)
  body <- substitute(body)
  args <- make_args_list(as.character(header))
  new_function(args, body, caller_env())
} 
```

Now, we can use the new syntax as syntactic sugar for anonymous functions:

```{r}
sapply(1:4, x := x**2)
```

What about lambda expressions with more than one argument? We could wish for syntax similar to this

```r
mapply(x,y := x*y, x = 1:6, y = 1:2)
```

but since R interprets commas a certain way, and we cannot override that, this is unfortunately not possible. If we want to group some parameters, we need to put them in a function call, and as it turns out, this will work just fine:

```{r}
mapply(.(x,y) := x*y, x = 1:6, y = 1:2)
```

What happens here is that the `make_args_list` translates all the components of the left hand side expression into function parameters. A function call object is just like any other expression list, so in this particular example, we create a function with three arguments, `.`, `x` and `y`. Since `.` is not used inside the function body, it doesn’t matter that we do not provide it when the function is called. If we reused one of the parameter names as the function name in the call, however, we see that this is what is happening:

```{r}
mapply(x(x,y) := x*y, x = 1:6, y = 1:2)
```

We can get rid of the function name in calls by removing the first element in the list if it is a call

```{r}
`:=` <- function(header, body) {
  header <- substitute(header)
  if (is.call(header)) header <- header[-1]
  body <- substitute(body)
  args <- make_args_list(as.character(header))
  new_function(args, body, caller_env())
} 
```

and now the example from above will work.

```{r}
mapply(x(x,y) := x*y, x = 1:6, y = 1:2)
```


## Experiments with alternative syntax

Using an assignment operator to define a function in this way might not be the most obvious syntax you could choose, but we have plenty of options for playing around with alternatives.

```{r}
lambda <- structure(NA, class = "lambda")
`[.lambda` <- function(x, ...) {
  spec <- eval(substitute(alist(...)))
  n <- length(spec)
  args <- make_args_list(spec[-n])
  body <- spec[[n]]
  new_function(args, body, caller_env())
}
```

```{r}
sapply(1:4, lambda[x, 4 * x**2])
mapply(lambda[x, y, y*x], x = 1:4, y = 4:7)
```

## Don’t do this at home

Implementing syntactic sugar for lambda expressions as we have just done only saves us very little typing compared to using `function` expressions and potentially does more harm than good—after all, everyone should be familiar with `function` expressions but they might not be with our home-made syntax for the same. Consequently, I would not recommend that you write new syntax for language constructions that are already implemented in R. We implemented the lambda expressions here simply as an example that illustrates how we can construct new syntax with very little code.