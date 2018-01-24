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

expressions <- function() list(expressions = NULL)
add_expression_quo <- function(expressions, expr) {
  expressions$expressions <- cons(rlang::enquo(expr), expressions$expressions)
  expressions
}
make_functions_quo <- function(expressions, args) {
  results <- vector("list", length = lst_length(expressions$expressions))
  i <- 1; lst <- expressions$expressions
  while (!is.null(lst)) {
    results[[i]] <- rlang::new_function(args, rlang::UQE(lst$car), rlang::get_env(lst$car))
    i <- i + 1
    lst <- lst$cdr
  }
  rev(results)
}

make_line_function <- function(intercept) {
  expressions() %>% 
    add_expression_quo(coef + intercept) %>%
    add_expression_quo(2*coef + intercept) %>% 
    add_expression_quo(3*coef + intercept) %>% 
    add_expression_quo(4*coef + intercept) %>% 
    make_functions_quo(alist(coef=)) 
}

make_line_function(0) %>% purrr::invoke_map(coef = 1) %>% unlist
make_line_function(0) %>% purrr::invoke_map(coef = 2) %>% unlist
make_line_function(1) %>% purrr::invoke_map(coef = 1) %>% unlist

make_functions_quote <- function(expressions, args) {
  results <- vector("numeric", length = lst_length(expressions$expressions))
  i <- 1; lst <- expressions$expressions
  while (!is.null(lst)) {
    results[i] <- rlang::new_function(expressions$args, lst$car, rlang::caller_env())
    i <- i + 1
    lst <- lst$cdr
  }
  results
}


eval_expressions <- function(expressions, ...) {
  results <- vector("numeric", length = lst_length(expressions$functions))
  i <- 1; lst <- expressions$functions
  while (!is.null(lst)) {
    results[i] <- lst$car(...)
    i <- i + 1
    lst <- lst$cdr
  }
  results
}

make_expressions <- function(z, adder) {
  z <- 2
  expressions(alist(x=,y=)) %>%
    adder(x + y + z) %>%
    adder(x * y * z)
}
ex_quo <- make_expressions(z = 2, add_expression_quo)
ex_quo %>% eval_expressions(x = 4, y = 6)

add_expression_quote <- function(expressions, expr) {
  body <- substitute(expr)
  f <- rlang::new_function(expressions$args, body, rlang::caller_env())
  expressions$functions <- cons(f, expressions$functions)
  expressions
}
ex_quote <- make_expressions(z = 2, add_expression_quote)
ex_quote %>% eval_expressions(x = 4, y = 6)
