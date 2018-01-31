
linked_list := NIL | CONS(car, cdr : linked_list)

reverse_list <- function(lst, acc = NIL) {
  force(acc)
  match(lst,
        NIL -> acc,
        CONS(car, cdr) -> reverse_list(cdr, CONS(car, acc)))
}

list_length <- function(lst, acc = 0) {
  force(acc)
  match(lst,
        NIL -> acc,
        CONS(car, cdr) -> list_length(cdr, acc + 1))
}

list_to_vector <- function(lst) {
  n <- list_length(lst)
  v <- vector("list", length = n)
  f <- function(lst, i) {
    force(i)
    match(lst,
          NIL -> NULL,
          CONS(car, cdr) -> {
            v[[i]] <<- car
            f(cdr, i + 1)
            }
          )
  }
  f(lst, 1)
  v %>% unlist
}

vector_to_list <- function(vec) {
  lst <- NIL
  for (i in seq_along(vec)) {
    lst <- CONS(vec[[i]], lst)
  }
  reverse_list(lst)
}

lst <- vector_to_list(1:5)
list_length(lst)
list_to_vector(lst)
lst %>% reverse_list %>% list_to_vector

