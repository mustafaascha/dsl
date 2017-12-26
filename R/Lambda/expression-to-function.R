library(magrittr)


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


collect_symbols_rec <- function(expr, lst, bound) {
  if (is.symbol(expr) && expr != "") {
    if (as.character(expr) %in% bound) lst
    else cons(as.character(expr), lst)
    
  } else if (is.pairlist(expr)) {
    for (i in seq_along(expr)) {
      lst <- collect_symbols_rec(expr[[i]], lst, bound)
    }
    lst
    
  } else if (is.call(expr)) {
    if (expr[[1]] == as.symbol("function"))
      bound <- c(names(expr[[2]]), bound)
    
    for (i in 1:length(expr)) {
      lst <- collect_symbols_rec(expr[[i]], lst, bound)
    }
    lst
    
  } else {
    lst
  }
}

collect_symbols_ <- function(expr, env) {
  bound <- c()
  lst <- collect_symbols_rec(expr, NULL, bound)
  lst %>% lst_to_list %>% unique %>% 
    purrr::discard(exists, env) %>%
    unlist
}
collect_symbols <- function(expr) {
  collect_symbols_(substitute(expr), parent.frame())
}

collect_symbols(function(x) 2 + x + y + z)

library(rlang)
make_alist <- function(args) {
  res <- replicate(length(args), substitute())
  names(res) <- args
  res
}
expr_to_function <- function(expr) {
  expr <- substitute(expr)
  unbound <- collect_symbols_(expr, caller_env())
  new_function(make_alist(unbound), expr, caller_env())
}

expr_to_function <- function(expr) {
  expr <- substitute(expr)
  unbound <- collect_symbols_(expr, caller_env())
  new_function(make_alist(unbound), expr, caller_env())
  eval(call("function", make_alist(unbound), expr), caller_env())
}

f <- expr_to_function(2 * x + y)
f
f(x = 2, y = 3)
