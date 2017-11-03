
## Introduction

This book gives an introduction to embedded domain specific languages in R. The term *domain specific languages,* or DSL, refers to programming languages specialised for a given purpose, as opposed to general purpose programming languages. Domain specific languages ideally give you a precise way of specifying tasks you want to do and goals you want to achieve within a specialised context. Regular expressions is one example, where you have a specialised language to express patterns of text. You can use this domain specific language to define text strings to search for or specify rules for modifying text. Regular expressions are often considered very hard to read, but they do provide a very powerful language for describing text patterns. Another example of a domain specific language is SQL---a language specialised for extracting from and modifying in a relational data base. With SQL, you have a powerful domain specific language in which you can specify rules for which data points in a data base you want to access or modify.

Regular expressions and SQL expressions are typically specified as strings when you use them in a program, and these strings must be parsed and interpreted when your program runs. In a sense, they are languages separated from the programming language you use them *in*. They need to separately be compiled and used by a different compiler or interpreter. In contrast, *embedded* domain specific languages provide domain specific languages expressed in the general purpose language in which they are used. In R, the grammar of graphics implemented in `ggplot2` or the data transformation operations implemented in `dplyr` provide small languages---domain specific languages---that you can use from within R, and you write the programs for these languages in R as well.

Embedded DSLs extend the programming language you are working in. They provide more than you usually find in a framework in the form of functions and classes as they provide a high level of flexibility in what you can do with them. They are programming languages, after all, and you can express complex ideas and tasks in them. They provide a language for expressing thoughts in a specialised domain, so they do not provide a general programming language as the language you use them from, but they do extend that surrounding language with new expressive power. Being embedded in that language, however, they will follow the rules you are familiar with there. Or mostly, at least, since in languages such as R, it is possible to modify the rules a bit programatically. You can expect the syntax of the embedded DSL to follow the rules of the general purpose language. The semantics will be determined by the DSL, but when you write programs in the DSL the syntax is already familiar to you. If you implement a DSL yourself, embedding it in a general purpose language lets you reuse the parsing and evaluation done by the general purpose language, so you can focus on just the domain specific part of the language.

Implementing embedded domain specific languages involves *meta programming*, that is, it involves treating the program you are writing as data to be manipulated by the program itself. This might sound more complicated than it is, but quite often, it is fairly straightforward to achieve. You *can* use meta programming to manipulate expressions in R and modify them and change how they are evaluated---and we will see examples in this book---but you can also construct expressive languages just from combining traditional functions and operator overloading.

The first chapters in this book will only use R's object-oriented programming constructions and its operator overloading to implement domain specific languages. Later in the book, we will see how we can implement more powerful languages through more complex meta programming. But as an example, one we cover in detail in [Chapter @sec:matrix-expressions], consider matrix arithmetic. You might not consider arithmetic a domain specific language because you are familiar with it as part of general purpose languages, but the way you write arithmetic expressions in R is different from how you write other function calls, so in many ways, you perhaps *should* consider it a domain specific language---one for doing arithmetic.

Anyway, R supports arithmetic with matrices and if you use the operator `%*%` you can do matrix multiplication (if you use `*` you will do component-wise multiplication instead). Multiplications are done one at a time, so if you have a series of them, such as this

```r
A %*% B %*% C %*% D
```

the product will be computed from left to right,

```r
((A %*% B) %*% C) %*% D
```

and for each multiplication you produce a new matrix that will be used in the next multiplication.

Now, matrix multiplication is associative, so you should be able to set the parentheses in any way, as long as you respect the left-to-right order of the matrices (matrix multiplication is not commutative, after all), and you will get the same result. The running time, however, will not be the same. We can do a small experiment to see this using the `microbenchmark` package:

```{r, cache=TRUE}
A <- matrix(1, nrow = 400, ncol = 300)
B <- matrix(1, nrow = 300, ncol = 30)
C <- matrix(1, nrow = 30, ncol = 500)
D <- matrix(1, nrow = 500, ncol = 400)

library(microbenchmark)
res <- microbenchmark(A %*% B %*% C %*% D,
                      ((A %*% B) %*% C) %*% D,
                      (A %*% (B %*% C)) %*% D,
                      (A %*% B) %*% (C %*% D),
                      A %*% (B %*% (C %*% D)),
                      A %*% ((B %*% C) %*% D))

options(microbenchmark.unit="relative")
print(res, signif = 3, order = "mean")
```

Here, I've computed the matrix product in the five different ways possible. There are six expressions, but the first two will compute the matrix multiplication in the same order. With `microbenchmark` we compute each expression 100 times and collect the time each evaluation takes. We collect the time it takes to compute each expression and here I have displayed the running time relative to the fastest expression, sorted by the mean evaluation time.

On average, there is almost a factor of ten between the fastest and the slowest evaluation (for the slowest evaluations in the two cases the differences is a factor of two, which is still a substantial relative difference). Clearly, there is something to be gained by setting parentheses optimally if we multiply together several large matrices. The dimensions of matrices are not necessarily known before runtime, however, so ideally we want to set the parentheses when we evaluate expressions in an optimal way.

The approach we take in [Chapter @sec:matrix-expressions] is to delay the evaluation of matrix multiplication and instead build a data structure for matrix expressions. We wrap matrices in a class that just contains data:

```{r}
m <- function(data) {
  structure(data, 
            nrow = nrow(data),
            ncol = ncol(data),
            class = c("matrix_expr", class(data)))
}
```

and we do the same for matrix multiplication, where we do not evaluate the multiplication but simply save references to the matrices that we want to multiply together:

```{r}
matrix_mult <- function(A, B) {
  structure(list(left = A, right = B),
            nrow = nrow(A),
            ncol = ncol(B),
            class = c("matrix_mult", "matrix_expr"))
}

`*.matrix_expr` <- function(A, B) {
  matrix_mult(A, B)
}
```

When we need to evaluate a matrix multiplication we can analyse the expression we have delayed evaluation of, rearrange multiplications to get the optimal performance, and then evaluate the expression. In [Chapter @sec:matrix-expressions] we will implement the functions `rearrange_matrix_mult` and `eval_matrix_mult` that does this. Here, we just define a function, `v`, for evaluating a matrix multiplication:

```{r, echo=FALSE}
library(microbenchmark) # necessary because the loading above
                        # is cached
source("../R/Expressions/introduction-matrix-expressions.R")
```

```{r}
v <- function(expr)
	eval_matrix_mult(rearrange_matrix_mult(expr))
```

We can compare this automatic parenthesis setting procedure with the default evaluation and the optimal evaluation order we saw above:

```{r, cache=FALSE}
res <- microbenchmark(A %*% B %*% C %*% D,
                      (A %*% B) %*% (C %*% D),
                      v(m(A) * m(B) * m(C) * m(D)))

options(microbenchmark.unit="relative")
print(res, signif = 3, order = "mean")
```

The automatic solution is only slightly slower than the optimal solution and about a factor of six better than the default evaluation.

Notice that this is achieved without any fancy meta programming. We simply combine object-orientation and operator overloading. Many embedded domain specific languages can be constructed in this way, and this is what we will explore first in this book, starting in the next chapter.

### Who this book is for

This book is aimed at experienced R programmers. Some of the concepts we cover in this book are advanced, so at the very least you should be familiar with functional and object-oriented programming in R (although the next chapter will give you a reminder of the object-oriented programming features we will use). It will be helpful to have some experience with meta programming, but [Chapter @sec:metaprogramming] gives a crash course in the features we will use in this book, so you should be able to pick it up with a little effort from there.
