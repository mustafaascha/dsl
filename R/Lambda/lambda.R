library(rlang)

make_args_list <- function(args) {
  res <- replicate(length(args), substitute())
  names(res) <- args
  as.pairlist(res)
}

`:=` <- function(header, body) {
  header <- substitute(header)
  if (is.call(header)) header <- header[-1]
  body <- substitute(body)
  args <- make_args_list(as.character(header))
  new_function(args, body, caller_env())
} 

lambda <- function(...) {
  spec <- eval(substitute(alist(...)))
  n <- length(spec)
  
  args <- make_args_list(spec[-n])
  body <- spec[[n]]
  new_function(args, body, caller_env())
}

sapply(1:4, lambda(x, 4 * x**2))
mapply(lambda(x, y, y*x), x = 1:4, y = 4:7)


lambda <- structure(NA, class = "lambda")
`[.lambda` <- function(x, ...) {
  spec <- eval(substitute(alist(...)))
  n <- length(spec)
  
  args <- make_args_list(spec[-n])
  body <- spec[[n]]
  new_function(args, body, caller_env())
}

sapply(1:4, lambda[x, 4 * x**2])
mapply(lambda[x, y, y*x], x = 1:4, y = 4:7)
