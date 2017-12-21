
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


ctmc <- function() structure(list(from = NULL, rate = NULL, to = NULL), class = "ctmc")
add_edge <- function(ctmc, from, rate, to) {
  ctmc$from <- cons(from, ctmc$from)
  ctmc$rate <- cons(rate, ctmc$rate)
  ctmc$to <- cons(to, ctmc$to)
  ctmc
}

rate_matrix <- function(ctmc) {
  from <- lst_to_list(ctmc$from)
  to <- lst_to_list(ctmc$to)
  rate <- lst_to_list(ctmc$rate)
  nodes <- c(from, to) %>% unique %>% unlist
  
  n <- length(nodes)
  Q <- matrix(0, nrow = n, ncol = n)
  rownames(Q) <- colnames(Q) <- nodes
  
  for (i in seq_along(from)) {
    Q[from[[i]], to[[i]]] <- rate[[i]]
  }
  
  diag(Q) <- - rowSums(Q)
  
  Q
}

ctmc() %>% 
  add_edge("foo", 1, "bar") %>% 
  add_edge("foo", 2, "baz") %>% 
  add_edge("bar", 2, "baz") %>% rate_matrix

