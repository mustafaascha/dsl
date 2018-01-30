
search_tree := E | T(left : search_tree, value, right : search_tree)

insert <- function(tree, x) {
  match(tree,
        E -> T(E, x, E),
        T(left, val, right) ->
          if (x < val)
            T(insert(left, x), val, right)
          else if (x > val)
            T(left, val, insert(right, x))
          else
            T(left, x, right)
        )
}

member <- function(tree, x) {
  match(tree,
        E -> FALSE,
        T(left, val, right) -> {
          if (x < val) member(left, x)
          else if (x > val) member(right, x)
          else TRUE
        })
}

tree <- E
for (i in sample(2:4))
  tree <- insert(tree, i)

for (i in 1:6) {
  cat(i, " : ", member(tree, i), "\n")
}


