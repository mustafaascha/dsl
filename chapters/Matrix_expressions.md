
## Matrix expressions {#sec:matrix-expressions}

In the next chapter we discuss computer languages and how they are manipulated in a more theoretical way, but first we will consider a concrete example---the matrix expressions mentioned in the introduction. This is a relatively simple example of a domain specific language, but parsing matrix expressions, optimising them, and then evaluating them, captures all the phases we usually have to implement in any DSL and the implementation will also have examples of most of the techniques we will cover in more detail later. The example will use some techniques that are not explained until later in the book, so some details of the example might not be completely clear at this point, but the broader strokes should be, and will hopefully serve as a taste of what follows in future chapters.

To remind you, our goal for writing a language for matrix expressions is to improve upon the default performance the built-in matrix expressions have. We achieve this by taking a more global view of expressions that R does---R will handle each operator one at a time from left to right, but we will analyse expressions and rearrange them to improve performance. The steps we must take to do this are these: we must parse expressions into data that we can manipulate, then we must rearrange the expressions into more efficient expressions, and finally we must  provide a way to evaluate the expressions.

### Parsing expressions

To keep things simple, we will only consider matrix multiplication and matrix addition. We do not include scalar multiplication or inverting or transposing matrices or any other functionality. Adding to the example to include more components of the expression language will follow the same ideas as we need for multiplication and addition, and will not teach us anything new in terms of embedding DSLs in R. When you understand the example, you will be able to easily do this yourself.

With these restrictions, we can say that a matrix expression is either just a matrix, the product of two matrix expressions, or the sum of two matrix expressions. We can represent this as a class hierarchy with one (abstract) super class representing expressions and three (concrete) sub-classes for actual data, products, and sums. If you are not familiar with object-oriented programming in R, we will have short guide to all you need to know to follow this book in [Chapter @sec:functions-classes-operators]. Constructors for creating objects of the three concrete classes can look like these:

```{r}
m <- function(data) {
  structure(list(data = data), 
            nrow = nrow(data),
            ncol = ncol(data),
            def_expr = deparse(substitute(data)),
            class = c("matrix_data", "matrix_expr"))
}
matrix_mult <- function(A, B) {
  structure(list(left = A, right = B),
            nrow = nrow(A),
            ncol = ncol(B),
            class = c("matrix_mult", "matrix_expr"))
}
matrix_sum <- function(A, B) {
  structure(list(left = A, right = B),
            nrow = nrow(A),
            ncol = ncol(B),
            class = c("matrix_sum", "matrix_expr"))
}
```

We generally just wrap the parameters of the constructors in a list and set the appropriate class attributes, and we store the number of rows and number of columns because we will need these when optimising matrix multiplication as we saw in the introduction. 

The only purpose of the `def_expr` attribute we set in the `m` function is pretty printing. It makes the output of the expressions we manipulate below easier to follow. Strictly speaking, we do not *need* any pretty printing for manipulating expressions, but it does make debugging easier, so I tend to always write some code for that. For the matrix expressions we can use the following code:

```{r}
toString.matrix_data <- function(x, ...) {
  paste0("[", attr(x, "def_expr"), "]")
}
toString.matrix_mult <- function(x, ...) {
  paste0("(", toString(x$left), " * ", toString(x$right), ")")
}
toString.matrix_sum <- function(x, ...) {
  paste0("(", toString(x$left), " + ", toString(x$right), ")")
}
print.matrix_expr <- function(x, ...) {
  print(toString(x))
}
```

Using the constructors and the pretty printing code, we can try to construct a small expression:

```{r}
A <- matrix(1, nrow = 10, ncol = 20)
B <- matrix(1, nrow = 20, ncol = 10)
C <- matrix(1, nrow = 10, ncol = 10)

matrix_sum(matrix_mult(m(A), m(B)), m(C))
```

There is nothing in what we have done so far that really qualifies as providing a *language* as such. We have just implemented a few constructor functions. However, if we overload the multiplication and addition operators for matrix expressions, we get something that starts to resemble a language, at least:

```{r}
`*.matrix_expr` <- function(A, B) {
  stopifnot(ncol(A) == nrow(B))
  matrix_mult(A, B)
}
`+.matrix_expr` <- function(A, B) {
  stopifnot(dim(A) == dim(B))
  matrix_sum(A, B)
}
```

With these, we can write the same expression in a more familiar way:

```{r}
m(A) * m(B) + m(C)
```

I have put some assertions in the operators to make sure that the dimensions of the matrices involved in operators are valid. We *could* also have put these in the constructor functions, but later on we will manipulate expressions where we know that the dimensions are valid, so we don't need to check them there. We don't expect a user to call the constructors directly, but use the operators, so this is the natural place to put the checks.

We use the `dim` function for the sanity check in the addition operator, so we need a version of this that works on matrix expressions. It could look like this:

```{r}
dim.matrix_expr <- function(x) {
  c(attr(x, "nrow"), attr(x, "ncol"))
}
```

You might be wondering why we need the `m` function. After all, it doesn't really contribute anything to expressions instead of just wrapping matrices. Couldn't we just use the matrices directly? The answer is no, and it has to do with how we use operator overloading. For `*` and `+` to be the matrix expression versions, we need the first arguments given to them to be a matrix expression. If we wrote simply

```{r}
A * B + C
```

we would be invoking the operators for R's matrix class instead. And since `*` is not matrix multiplication, for that you need to use `%*%`---the `*` operator is component-wise multiplication---you get an error.

We need a way of bootstrapping us from R's matrices to the matrices in our expression language. That is what we use `m` for.

#### Meta-programming parsing

Using an explicit function such as `m` to bootstrap us into the matrix expression language is the simplest way to use R's own parser for our benefits, but it is not the only way. In R, we can manipulate expressions as if they were data, a feature known as *meta-programming*, and something we return to in [Chapter @sec:parsing_and_manipulating_expressions]. For now, it suffices to know that an expression can be recursively explored. We can use the predicate `is.name` to check if the expression refers to a variable, and we can use the predicate `is.call` to check if it is a function call---and all operators are function calls. So, given an expression that doesn't use the `m` function, and thus doesn't enter our DSL, we can transform it into one that does like this:

```{r}
build_matrix_expr <- function(expr) {
  if (is.name(expr)) 
    return(substitute(m(name), list(name = expr)))
  
  if (is.call(expr)) {
    if (expr[[1]] == as.name("*") || expr[[1]] == as.name("%*%"))
      return(call('*', 
                  build_matrix_expr(expr[[2]]), 
                  build_matrix_expr(expr[[3]])))
    if (expr[[1]] == as.name("+"))
      return(call('+', 
                  build_matrix_expr(expr[[2]]), 
                  build_matrix_expr(expr[[3]])))
  }
  
  stop(paste("Parse error for", expr))
}
```

In this implementation, we consider both `*` and `%*%` matrix multiplication, just so we would consider an R expression that actually uses matrix multiplication as such.

For this function to work, it needs a so-called "quoted" expression to work with. If we write a raw expression in R, then R will try to evaluate it before we can manipulate it. We will get an error before we even get to rewriting the expression.

```{r}
build_matrix_expr(A * B)
```

To avoid this, we need to quote the expression:

```{r}
build_matrix_expr(quote(A * B))
```

We can avoid having to explicitly quote expressions every time we call the function by wrapping it in another function that does it for us. If we call the function `substitute` on a function parameter, we get the expression it contains, so we could write a function like this:

```{r}
parse_matrix_expr <- function(expr) {
  expr <- substitute(expr)
  build_matrix_expr(expr)
}
```

Now, we do not need to quote expressions to do the rewriting.

```{r}
parse_matrix_expr(A * B)
```

This isn't a perfect solution, and there are some pitfalls, among which is that you cannot use this function from other functions directly. The `substitute` function can be difficult to work with. A further problem is that we are creating a new expression, but an R expression and not the data structure we want in our matrix expression language. The R expression, you can think of as a literate piece of code; it is not yet evaluated to become the result we want. For that, we need the `eval` function and we need to evaluate the expression in the right context. Working with expressions, and especially evaluating expressions in different environments, are among the more advanced aspects of R programming, so if it looks very complicated right now do not despair. We cover it in detail in [Chapter @sec:env_and_expr]. For now, we will just use this function:

```{r}
parse_matrix_expr <- function(expr) {
  expr <- substitute(expr)
  modified_expr <- build_matrix_expr(expr)
  eval(modified_expr, parent.frame())
}
```

It gets the (quoted) expression, build the corresponding matrix expression, and then evaluate that expression in the "parent frame", which is the environment where we call the function. With this function, we can get a data structure in our matrix language from an otherwise ordinary R expression:

```{r}
parse_matrix_expr(A * B)
```

The approach we take here, involves translating one R expression into another, in order to use our `m` function to move us from R to matrix expressions. This involves parsing the expression twice, once when we transform it, and again when we ask R to evaluate the result. The approach is also less expressive than using the `m` function directly. We can call `m` with any expression that generates a matrix, but in the expression transformation we only allow identifiers.

As an alternative, we can build the matrix expression directly using our constructor functions. We will use `matrix_mult` and `matrix_sum` when we have a call that is `*`, `%*%`, or `+`, and otherwise we will call `m`. This way, any expression we do not recognise as multiplication or addition will be interpreted as a value we should consider a matrix. This approach, however, adds one complication. When we call function `m`, we need to call it with a value, but what we have when traverse the expression is *quoted* expressions. We need to evaluate such expressions, and we need to do so in the right environment. We will need to pass an environment along with the traversal for this to work.

```{r}
build_matrix_expr <- function(expr, env) {
  if (is.call(expr)) {
    if (expr[[1]] == as.name("*") || expr[[1]] == as.name("%*%"))
      return(matrix_mult(build_matrix_expr(expr[[2]], env), 
                         build_matrix_expr(expr[[3]], env)))
    if (expr[[1]] == as.name("+"))
      return(matrix_sum(build_matrix_expr(expr[[2]], env), 
                        build_matrix_expr(expr[[3]], env)))
  }
  data_matrix <- m(eval(expr, env))
  attr(data_matrix, "def_expr") <- expr
  data_matrix
}
```

Most of this function should be self-explanatory, except for where we explicitly set the `def_expr` attribute of a data matrix. This is the attribute be use for pretty printing, and when we call the `m` function it is set to the literate expression we called `m` with. This would be `eval(expr, env)` for all matrices we create with this function. To avoid that, we explicitly set it to the (quoted) expression we use in the evaluation.

Once again, we can wrap the function in another that gets us the quoted expression and provide the environment we should evaluate expressions in.

```{r}
parse_matrix_expr <- function(expr) {
  expr <- substitute(expr)
  build_matrix_expr(expr, parent.frame())
}

parse_matrix_expr(A * B)
```

There is much more to manipulating expressions, and especially to how they are evaluated, but we return to that in later chapters.

### Expression manipulation

#### Optimising addition

```{r, cache=TRUE}
sum3 <- function(A, B, C) {
  result <- matrix(0, nrow = nrow(A), ncol = ncol(A))
  for (i in seq_along(nrow(A))) {
    for (j in seq_along(ncol(A))) {
      result[i,j] <- A[i,j] + B[i,j] + C[i,j]
    }
  }
  result
}

A <- matrix(1, nrow = 400, ncol = 3000)
res <- microbenchmark(A + A + A, sum3(A, A, A))
options(microbenchmark.unit="relative")
print(res, signif = 3, order = "mean")
```

```{r}
matrix_3sum <- function(A, B, C) {
  structure(list(A = A, B = B, C = C),
            nrow = nrow(A),
            ncol = ncol(A),
            class = c("matrix_3sum", "matrix_expr"))
}

toString.matrix_3sum <- function(x, ...) {
  paste0("(", toString(x$A), " 3+3 ", toString(x$B), " 3+3 ", toString(x$C), ")")
}

rearrange_matrix_expr.matrix_sum <- function(expr) {
  if (inherits(expr$left, "matrix_sum")) {
    new_expr <- matrix_3sum(expr$left$left, expr$left$right, expr$right)
    rearrange_matrix_expr(new_expr)
  } else if (inherits(expr$right, "matrix_sum")) {
    new_expr <- matrix_3sum(expr$left, expr$right$left, expr$right$right)
    rearrange_matrix_expr(new_expr)
  } else {
    matrix_sum(rearrange_matrix_expr(expr$left),
               rearrange_matrix_expr(expr$right))  
  }
}

rearrange_matrix_expr.matrix_3sum <- function(expr) {
  matrix_3sum(rearrange_matrix_expr(expr$A),
              rearrange_matrix_expr(expr$B),
              rearrange_matrix_expr(expr$C))
}
```

#### Optimising multiplication


### Expression evaluation

