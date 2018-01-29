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
## Construction ####
ctmc <- function()
  structure(list(from = NULL,
                 rate = NULL,
                 to = NULL,
                 params = NULL),
            class = "ctmc")

add_edge <- function(ctmc, from, rate, to) {
  from <- enexpr(from) ; stopifnot(is_symbol(from))
  to <- enexpr(to) ; stopifnot(is_symbol(to))

  from <- as_string(from)
  to <- as_string(to)

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

## Rate matrices ####

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

## Traces ####
ctmc_trace <- function(ctmc) {
  nodes <- c(lst_to_list(ctmc$from), lst_to_list(ctmc$to)) %>%
    unique %>% unlist
  structure(list(nodes=nodes, states=NULL, at=NULL),
            class="ctmc_trace")
}
start_state <- function(trace, state, at=0) {
  state <- enexpr(state)
  stopifnot(is_symbol(state))
  state <- as_string(state)
  stopifnot(state %in% trace$nodes)
  stopifnot(is.numeric(at))

  trace$states <- cons(state, trace$states)
  trace$at <- cons(at, trace$at)

  trace
}
transition <- function(trace, at, to) {
  stopifnot(is.numeric(at))
  to <- enexpr(to)
  stopifnot(is_symbol(to))
  to <- as_string(to)
  stopifnot(to %in% trace$nodes)

  trace$states <- cons(to, trace$states)
  trace$at <- cons(at, trace$at)

  trace
}

as_tibble.ctmc_trace <- function(x, ...) {
  states <- x$states %>% lst_to_list %>% unlist %>% rev
  at <- x$at %>% lst_to_list %>% unlist %>% rev
  tibble::tibble(state = states, at = at)
}
print.ctmc_trace <- function(x, ...) {
  df <- tibble::as_tibble(x)
  print(df)
}

tr <- ctmc_trace(m) %>%
  start_state(foo) %>%
  transition(0.1, bar) %>%
  transition(0.3, baz) %>%
  transition(0.5, qux) %>%
  transition(0.7, foo) %>%
  transition(1.1, baz)
tr


## Likelihoods ####
likelihood_function <- function(ctmc, trace) {
  rate_func <- ctmc %>% rate_matrix_function
  trace_df <- tibble::as_tibble(trace)

  lhd_function <- function() {
    args <- as_list(environment())
    Q <- do.call(rate_func, args)

    n <- length(trace_df$state)
    from <- trace_df$state[-n]
    to <- trace_df$state[-1]
    delta_t <- trace_df$at[-1] - trace_df$at[-n]

    lhd <- 1
    for (i in seq_along(from)) {
      P <- transition_probabilities(Q, delta_t[i])
      lhd <- lhd * P[from[i],to[i]]
    }
    lhd
  }
  formals(lhd_function) <- formals(rate_func)

  lhd_function
}

lhd <- m %>% likelihood_function(tr)
lhd(a = 2, b = 4)



## Constructing the CTMC ####
x <- 2
m <- ctmc() %>%
  add_edge(foo, a, bar) %>%
  add_edge(foo, 2*a, baz) %>%
  add_edge(foo, 4, qux) %>%
  add_edge(bar, b, baz) %>%
  add_edge(baz, a + x*b, qux) %>%
  add_edge(qux, a + UQ(x)*b, foo)
m

## Get a rate matrix ####
Q <- m %>% rate_matrix_function
Q(a = 2, b = 4)

## Constructing a trace ####
tr <- ctmc_trace(m) %>%
  start_state(foo) %>%
  transition(0.1, bar) %>%
  transition(0.3, baz) %>%
  transition(0.5, qux) %>%
  transition(0.7, foo) %>%
  transition(1.1, baz)
tr

## Compute a likelihood ####
lhd <- m %>% likelihood_function(tr)
lhd(a = 2, b = 4)
