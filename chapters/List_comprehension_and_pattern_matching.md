# List comprehension and pattern matching

## List comprehension

```{r}
library(rlang)

lcomp <- function(expr, ...) {
  expr <- enquo(expr)
  rest <- quos(...)

  lists <- Map(eval_tidy, rest[names(rest) != ""])
  predicates <- Map(UQE, rest[names(rest) == ""])

  f <- new_function(lists, body = UQE(expr), env = get_env(expr))
  values <- do.call(Map, c(f, lists))

  keep_index <- rep(TRUE, length(lists[[1]]))
  for (pred in predicates) {
    p <- new_function(lists, body = pred, env = get_env(expr))
    keep_index <- keep_index & unlist(do.call(Map, c(p, lists)))
  }

  values[keep_index]
}
```


```{r}
qsort <- function(x) {
  n <- length(x)

  if (n < 2) return(x)

  pivot <- x[[sample(n, size = 1)]]
  smaller <- lcomp(y, y = x, y < pivot)
  equal <- lcomp(y, y = x, y == pivot)
  larger <- lcomp(y, y = x, y > pivot)

  c(qsort(smaller), equal, qsort(larger))
}
```