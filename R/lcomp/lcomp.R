
qsort <- function(lst) {
  n <- length(lst)
  if (n < 2) return(lst)
  
  pivot <- lst[[sample(n, size = 1)]]
  smaller <- Filter(function(x) x < pivot, lst)
  equal <- Filter(function(x) x == pivot, lst)
  larger <- Filter(function(x) x > pivot, lst)
  c(qsort(smaller), equal, qsort(larger))
}
(lst <- sample(1:10))
unlist(qsort(lst))


library(rlang)
library(purrr)

lcomp <- function(expr, ...) {
  expr <- enquo(expr)
  rest <- quos(...)
  
  lists <- map(rest[names(rest) != ""], eval_tidy)
  predicates <- map(rest[names(rest) == ""], UQE)

  f <- new_function(lists, body = UQE(expr), env = get_env(expr))
  values <- pmap(lists, f)
  
  keep_index <- rep(TRUE, length(lists[[1]]))
  for (pred in predicates) {
    p <- new_function(lists, body = pred, env = get_env(expr))
    keep_index <- keep_index & unlist(pmap(lists, p))
  }
  
  values[keep_index]
}

qsort <- function(lst) {
  n <- length(lst)
  if (n < 2) return(lst)
  
  pivot <- lst[[sample(n, size = 1)]]
  smaller <- lcomp(x, x = lst, x < pivot)
  equal <- lcomp(x, x = lst, x == pivot)
  larger <- lcomp(x, x = lst, x > pivot)
  
  c(qsort(smaller), equal, qsort(larger))
}

(lst <- sample(1:10))
unlist(qsort(lst))

