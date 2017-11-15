
## Matrix expressions {#sec:matrix-expressions}

### Parsing expressions

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

#### Optimising multiplication

### Expression evaluation

