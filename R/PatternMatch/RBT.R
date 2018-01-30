
colour := R | B
rb_tree := E | T(col : colour, left : rb_tree, value, right : rb_tree)


member <- function(tree, x) {
  match(tree,
        E -> FALSE,
        T(col, left, val, right) -> {
          if (x < val) member(left, x)
          else if (x > val) member(right, x)
          else TRUE
        })
}

tree <- T(R, E, 2, T(B, E, 5, E))
for (i in 1:6) {
  cat(i, " : ", member(tree, i), "\n")
}

insert_rec <- function(tree, x) {
  match(tree,
        E -> T(R, E, x, E),
        T(col, left, val, right) -> {
          if (x < val)
            balance(T(col, insert_rec(left, x), val, right))
          else if (x > val)
            balance(T(col, left, val, insert_rec(right, x)))
          else
            T(col, left, x, right) # already here
        })
}
insert <- function(tree, x) {
  tree <- insert_rec(tree, x)
  tree$col <- B
  tree
}

balance <- function(tree) {
  match(tree,
        T(B, T(R, a, x, T(R, b, y, c)), z, d) -> T(R, T(B,a,x,b), y, T(B,c,z,d)),
        otherwise -> tree)
}

tree <- E
for (i in sample(2:4))
  tree <- insert(tree, i)
for (i in 1:6) {
  cat(i, " : ", member(tree, i), "\n")
}

tree <- T(B, T(R, E, 0, T(R, E, 1, E)), 2, E)
balance(tree)
