
constructor <- function() NA
variable <- function() NA

T <- L <- constructor()
v <- w <- variable()

match <- function(expr, ...) {

}

match(T(L(4), L(5)),
      T(L(v), L(w)) := v + w)
