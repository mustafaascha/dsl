

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
lst_to_vec <- function(lst) unlist(lst_to_list(lst))

leaf <- function(x) structure(x, class = c("leaf", "tree"))
inner <- function(left, right) 
  structure(list(left = left, right = right),
            class = c("inner", "tree"))

collect_leaves_rec <- function(tree, lst) 
  UseMethod("collect_leaves_rec")

collect_leaves_rec.leaf <- function(tree, lst) {
  cons(tree, lst)
}

collect_leaves_rec.inner <- function(tree, lst) {
  collect_leaves_rec(tree$left, collect_leaves_rec(tree$right, lst))
}

collect_leaves <- function(tree) {
  lst_to_vec(collect_leaves_rec(tree, NULL))
}

tree <- inner(leaf(1), inner(inner(leaf(2), leaf(3)), leaf(4)))
collect_leaves(tree)
