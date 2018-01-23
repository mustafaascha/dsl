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

  r <- enquo(rate)
  ctmc$rate <- cons(r, ctmc$rate)
  ctmc$params <- cons(collect_symbols_q(r, get_env(r)), ctmc$params)

  ctmc
}

print.ctmc <- function(x, ...) {
  from <- lst_to_list(x$from)
  to <- lst_to_list(x$to)
  rate <- lst_to_list(x$rate)
  parameters <- lst_to_list(x$params) %>%
    purrr::discard(is_null) %>% unique

  cat("CTMC:\n")
  cat("parameters:", paste(parameters), "\n")
  cat("transitions:\n")
  for (i in seq_along(from)) {
    cat(from[[i]], "->", to[[i]], "\t[", deparse(rate[[i]]), "]\n")
  }
  cat("\n")
}



m <- ctmc() %>%
  add_edge("foo", a, "bar") %>%
  add_edge("foo", 2*a, "baz") %>%
  add_edge("bar", b, "baz")
m

rate_matrix_function <- function(ctmc) {
  from <- lst_to_list(ctmc$from)
  to <- lst_to_list(ctmc$to)
  rate <- lst_to_list(ctmc$rate)

  nodes <- c(from, to) %>% unique %>% unlist
  parameters <- lst_to_list(ctmc$params) %>%
    purrr::discard(is_null) %>% unique

  n <- length(nodes)

  f <- function() {
    Q <- matrix(0, nrow = n, ncol = n)
    rownames(Q) <- colnames(Q) <- nodes
    for (i in seq_along(from)) {
      Q[from[[i]], to[[i]]] <- eval_tidy(rate[[i]], environment())
    }
    diag(Q) <- -rowSums(Q)
    Q
  }
  formals(f) <- make_args_list(parameters)

  f
}

q <- m %>% rate_matrix_function
q
q(a = 2, b = 4)

library(expm)
transition_probabilities <- function(Q, t) expm(Q * t)

ff <- function(expr, data) {
  eval(expr[[2]], data, environment(expr))
}
ffq <- function(expr, data) {
  expr <- eval(substitute(~ expr))
  environment(expr) <- rlang::caller_env()
  ff(expr, data)
}

g <- function(x, y, z) {
  w <- x + y + z
  ff( ~ w + u + v, data.frame(u = 1:4, v = 1:4))
}
h <- function(x, y, z) {
  w <- x + y + z
  ffq(w + u + v, data.frame(u = 1:4, v = 1:4))
}

g(1:4, 1:4, 1:4) == (1:4 + 1:4 + 1:4) + 1:4 + 1:4
h(1:4, 1:4, 1:4) == (1:4 + 1:4 + 1:4) + 1:4 + 1:4
