
colour := R | B
rb_tree := E | T(col : colour, value, left : rb_tree, right : rb_tree)


member <- function(tree, x) {
  match(tree,
        E -> FALSE,
        T(col, val, left, right) -> {
          if (x < val) member(left, x)
          else if (x > val) member(right, x)
          else TRUE
        })
}

tree <- T(R, 2, E, T(B, 5, E, E))
for (i in 1:6) {
  cat(i, " : ", member(tree, i), "\n")
}


insert <- function(tree, x) {

}
