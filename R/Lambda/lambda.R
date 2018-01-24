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


make_function_quo <- function(args, body) {
  body <- rlang::enquo(body)
  
}
make_function_quote <- function(args, body) {
  body <- substitute(body)
  rlang::new_function(args, body, rlang::caller_env())
}

cons <- function(elm, lst) list(car=elm, cdr=lst)
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
library(magrittr)

expressions <- function() list(ex = NULL)
add_expression <- function(ex, expr) {
  ex$ex <- cons(rlang::enquo(expr), ex$ex)
  ex
}
make_functions <- function(ex, args) {
  results <- vector("list", length = lst_length(ex$ex))
  i <- 1; lst <- ex$ex
  while (!is.null(lst)) {
    results[[i]] <- rlang::new_function(args, rlang::UQE(lst$car), rlang::get_env(lst$car))
    i <- i + 1
    lst <- lst$cdr
  }
  rev(results)
}

make_line_expressions <- function(intercept) {
  expressions() %>% 
    add_expression(coef + intercept) %>%
    add_expression(2*coef + intercept) %>% 
    add_expression(3*coef + intercept) %>% 
    add_expression(4*coef + intercept)
}
eval_line <- function(ex, coef) {
  ex %>% make_functions(alist(coef=)) %>%
    purrr::invoke_map(coef = coef) %>% unlist
}

make_line_expressions(intercept = 0) %>% eval_line(coef = 1)
make_line_expressions(intercept = 0) %>% eval_line(coef = 2)
make_line_expressions(intercept = 1) %>% eval_line(coef = 1)

add_expression <- function(ex, expr) {
  ex$ex <- cons(substitute(expr), ex$ex)
  ex
}
make_functions <- function(ex, args) {
  results <- vector("list", length = lst_length(ex$ex))
  i <- 1; lst <- ex$ex
  while (!is.null(lst)) {
    results[[i]] <- rlang::new_function(args, lst$car, rlang::caller_env())
    i <- i + 1
    lst <- lst$cdr
  }
  rev(results)
}

make_line_expressions(intercept = 0) %>% eval_line(coef = 1)
