library(magrittr)
library(rlang)

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

make_args_list <- function(args) {
  res <- replicate(length(args), substitute())
  names(res) <- args
  as.pairlist(res)
}



ctmc <- function()
  structure(list(from = NULL,
                 rate = NULL,
                 to = NULL,
                 params = NULL,
                 compiled = FALSE),
            class = "ctmc")


# expr has to be already quoted here
collect_symbols_q <- function(expr, env) {
  bound <- c()
  lst <- collect_symbols_rec(expr, NULL, bound)
  lst %>% lst_to_list %>% unique %>%
    purrr::discard(exists, env) %>%
    unlist
}

add_edge <- function(ctmc, from, rate, to) {
  ctmc$from <- cons(from, ctmc$from)
  ctmc$to <- cons(to, ctmc$to)

  r <- substitute(rate)
  ctmc$rate <- cons(list(expr = r, env = caller_env()), ctmc$rate)
  ctmc$params <- cons(collect_symbols_q(r, caller_env()), ctmc$params)

  ctmc
}

rate_matrix_function <- function(ctmc) {
  from <- lst_to_list(ctmc$from)
  to <- lst_to_list(ctmc$to)
  rate <- lst_to_list(ctmc$rate)
  nodes <- c(from, to) %>% unique %>% unlist
  parameters <- lst_to_list(ctmc$params) %>% unique
  n <- length(ctmc$nodes)

  f <- function() {
    Q <- matrix(0, nrow = n, ncol = n)
    rownames(Q) <- colnames(Q) <- nodes
    for (i in seq_along(from)) {
      Q[from[[i]], to[[i]]] <- eval(rate[[i]]$expr, rate[[i]]$env)
    }
    diag(Q) <- -rowSums(Q)
    Q
  }
  formals(f) <- make_args_list(ctmc$parameters)

  f
}

#print.ctmc <- function(x, ...) {
#  cat(ctmc$nodes, "\n")
#}

library(expm)
transition_probabilities <- function(Q, t) expm(Q * t)


m <- ctmc() %>%
  add_edge("foo", a, "bar") %>%
  add_edge("foo", 2*a, "baz") %>%
  add_edge("bar", b, "baz")

%>%
  rate_matrix_function(a = 2, b = 4)

m$make_rate_matrix(2,3)
m
