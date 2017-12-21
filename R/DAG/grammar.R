
cons <- function(car, cdr) list(car = car, cdr = cdr)
dag <- function() structure(list(edges = NULL), class = "dag")
`%=>%` <- function(from, to) c(from, to)

`+.dag` <- function(dag, edge) {
    dag$edges <- cons(edge, dag$edges)
    dag
}
dag() + 
  "foo" %=>% "bar" +
  "bar" %=>% "baz"

node <- function(name) structure(name, class = "node")
`>.node` <- function(from, to) c(from, to)

node("foo") > node("bar")

dag() + node("foo") > node("bar")
dag() + (node("foo") > node("bar"))

`|.dag` <- function(dag, edge) {
  dag$edges <- cons(edge, dag$edges)
  dag
}

dag() | node("foo") > node("bar")


