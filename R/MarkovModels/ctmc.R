## preliminary stuff -- already defined elsewhere

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

# expr has to be already quoted here
collect_symbols_q <- function(expr, env) {
  bound <- c()
  lst <- collect_symbols_rec(expr, NULL, bound)
  lst %>% lst_to_list %>% unique %>%
    purrr::discard(exists, env) %>%
    unlist
}

## This is where the language start ######

ctmc <- function()
  structure(list(from = NULL,
                 rate = NULL,
                 to = NULL,
                 params = NULL),
            class = "ctmc")

add_edge <- function(ctmc, from, rate, to) {
  ctmc$from <- cons(from, ctmc$from)
  ctmc$to <- cons(to, ctmc$to)

  r <- enquo(rate)
  ctmc$rate <- cons(r, ctmc$rate)
  ctmc$params <- cons(collect_symbols_q(UQE(r), get_env(r)), ctmc$params)

  ctmc
}

print.ctmc <- function(x, ...) {
  from <- lst_to_list(x$from) %>% rev
  to <- lst_to_list(x$to) %>% rev
  rate <- lst_to_list(x$rate) %>% rev
  parameters <- lst_to_list(x$params) %>% unlist %>% unique %>% rev

  cat("CTMC:\n")
  cat("parameters:", paste(parameters), "\n")
  cat("transitions:\n")
  for (i in seq_along(from)) {
    cat(from[[i]], "->", to[[i]], "\t[", deparse(UQE(rate[[i]])), "]\n")
  }
  cat("\n")
}

x <- 2
m <- ctmc() %>%
  add_edge("foo", a, "bar") %>%
  add_edge("foo", 2*a, "baz") %>%
  add_edge("foo", 4, "qux") %>%
  add_edge("bar", b, "baz") %>%
  add_edge("baz", a + x*b, "qux") %>%
  add_edge("qux", a + UQ(x)*b, "foo")
m

rate_matrix_function <- function(ctmc) {
  from <- lst_to_list(ctmc$from) %>% rev
  to <- lst_to_list(ctmc$to) %>% rev
  rate <- lst_to_list(ctmc$rate) %>% rev

  nodes <- c(from, to) %>% unique %>% unlist
  parameters <- lst_to_list(ctmc$params) %>% unlist %>% unique %>% rev

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

Q <- m %>% rate_matrix_function
Q
Q(a = 2, b = 4)

x <- 1
Q(a = 2, b = 4)

library(expm)
transition_probabilities <- function(Q, t) expm(Q * t)

