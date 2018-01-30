
search_tree := E | T(value, left : search_tree, right : search_tree)

insert <- function(tree, x) {
  match(tree,
        E -> T(x, E, E),
        T(val, left, right) ->
          if (x < val)
            T(val, insert(left, x), right)
          else if (x > val)
            T(val, left, insert(right, x))
          else
            T(x, left, right)
        )
}

tree <- E
for (i in sample(2:4))
  tree <- insert(tree, i)

member <- function(tree, x) {
  match(tree,
        E -> FALSE,
        T(val, left, right) -> {
          if (x < val) member(left, x)
          else if (x > val) member(right, x)
          else TRUE
        })
}

for (i in 1:6) {
  cat(i, " : ", member(tree, i), "\n")
}


