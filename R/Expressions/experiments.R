

cons <- function(car, cdr) list(car = car, cdr = cdr)
lst_length <- function(lst, size = 1) {
  if (is.null(lst)) size - 1
  else lst_length(lst$cdr, size + 1)
}
lst_to_list <- function(lst) {
  v <- vector(mode = "list", length = lst_length(lst))
  insert <- function(lst, size = 1) {
    if (!is.null(lst)) {
      v[[size]] <<- lst$car
      insert(lst$cdr, size + 1)
    }
  }
  insert(lst)
  v
}

lst_to_vec <- function(lst) 
  unlist(lst_to_list(lst))

lst_to_vec2 <- function(lst) {
  size <- lst_length(lst)
  if (size == 0) return(vector(length = 0))

  v <- vector(mode = mode(lst$car), length = size)
  insert <- function(lst, size = 1) {
    if (!is.null(lst)) {
      v[size] <<- lst$car
      insert(lst$cdr, size + 1)
    }
  }
  insert(lst)
  v
}

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

collect_leaves2 <- function(tree) {
  lst_to_vec2(collect_leaves_rec(tree, NULL))
}
collect_leaves2(tree)


microbenchmark(collect_leaves(tree), collect_leaves2(tree))



