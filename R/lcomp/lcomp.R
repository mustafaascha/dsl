
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

lc <- function(expr, ...) {
  expr <- enquo(expr)
  rest <- quos(...)
  
  lists <- map(rest[names(rest) != ""], eval_tidy)
  predicates <- map(rest[names(rest) == ""], UQE)
  
  keep_index <- rep(TRUE, length(lists[[1]]))
  for (pred in predicates) {
    p <- new_function(lists, body = pred, env = get_env(expr))
    keep_index <- keep_index & unlist(pmap(lists, p))
  }
  filtered_lists <- map(lists, ~.x[keep_index])
  
  f <- new_function(lists, body = UQE(expr), env = get_env(expr))
  pmap(filtered_lists, f)
}

qsort <- function(lst) {
  n <- length(lst)
  if (n < 2) return(lst)
  
  pivot <- lst[[sample(n, size = 1)]]
  smaller <- lc(x, x = lst, x < pivot)
  equal <- lc(x, x = lst, x == pivot)
  larger <- lc(x, x = lst, x > pivot)
  
  c(qsort(smaller), equal, qsort(larger))
}

(lst <- sample(1:10))
unlist(qsort(lst))


lc(rep(x, 5), x = 1:5)
matrix_from_list <- function(lst) {
  nrows <- length(lst)
  matrix(unlist(lst), nrow = nrows)
}
matrix_from_list(lc(rep(x, 5), x = 1:5))

not_primes <- lc(seq(from = 2*x, to = 100, by = x), x = 2:10) %>% unlist %>% unique
not_primes
primes <- lc(p, p = 1:10, !(p %in% not_primes)) %>% unlist
primes

get_primes <- function(n) {
  not_primes <- lc(seq(from = 2*x, to = n, by = x), x = 2:sqrt(n)) %>% unlist %>% unique
  lc(p, p = 1:n, !(p %in% not_primes)) %>% unlist
}
get_primes(100)

cons <- function(car, cdr) list(car = car, cdr = cdr)
lst_length <- function(lst) {
  len <- 0
  while (!is.null(lst)) {
    lst <- lst$cdr
    len <- len + 1
  }
  len
}
lst_to_list <- function(lst) {
  v <- vector(mode = "list", length = lst_length(lst))
  index <- 1
  while (!is.null(lst)) {
    v[[index]] <- lst$car
    lst <- lst$cdr
    index <- index + 1
  }
  v
}

get_primes <- function(n) {
  candidates <- 2:n
  primes <- NULL
  while (length(candidates) > 0) {
    p <- candidates[[1]]
    primes <- cons(p, primes)
    candidates <- lc(x, x = candidates, x %% p != 0)
  }
  primes %>% lst_to_list %>% unlist %>% rev
}
get_primes(100) 

zip <- function(x, y) {
  lc(c(x,y), x = x, y = y) %>% { do.call(rbind,.) }
}
zip(1:4,1:4)

