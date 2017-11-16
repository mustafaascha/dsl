## Helper functionality -- lists ################################
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

## Parsing #######################################################
m <- function(data) {
  structure(list(data = data), 
            nrow = nrow(data),
            ncol = ncol(data),
            def_expr = deparse(substitute(data)), # used for pretty-printing
            class = c("matrix_data", "matrix_expr"))
}
matrix_mult <- function(A, B) {
  structure(list(left = A, right = B),
            nrow = nrow(A),
            ncol = ncol(B),
            class = c("matrix_mult", "matrix_expr"))
}
matrix_sum <- function(A, B) {
  structure(list(left = A, right = B),
            nrow = nrow(A),
            ncol = ncol(B),
            class = c("matrix_sum", "matrix_expr"))
}


`*.matrix_expr` <- function(A, B) {
  stopifnot(ncol(A) == nrow(B))
  matrix_mult(A, B)
}
`+.matrix_expr` <- function(A, B) {
  stopifnot(dim(A) == dim(B))
  matrix_sum(A, B)
}

build_matrix_expr <- function(expr, env) {
  if (is.call(expr)) {
    if (expr[[1]] == as.name("(")) 
      return(build_matrix_expr(expr[[2]], env))
    if (expr[[1]] == as.name("*") || expr[[1]] == as.name("%*%"))
      return(matrix_mult(build_matrix_expr(expr[[2]], env), 
                         build_matrix_expr(expr[[3]], env)))
    if (expr[[1]] == as.name("+"))
      return(matrix_sum(build_matrix_expr(expr[[2]], env), 
                        build_matrix_expr(expr[[3]], env)))
  }
  data_matrix <- m(eval(expr, env))
  attr(data_matrix, "def_expr") <- deparse(expr)
  data_matrix
}

parse_matrix_expr <- function(expr) {
  expr <- substitute(expr)
  build_matrix_expr(expr, parent.frame())
}

C <- B <- A <- matrix(0, 10, 10)
parse_matrix_expr((A + B) * C)

## Helper functions ##############################################
dim.matrix_expr <- function(x) {
  c(attr(x, "nrow"), attr(x, "ncol"))
}

toString.matrix_expr <- function(x, ...) {
  paste0("[", attr(x, "def_expr"), "]")
}
toString.matrix_mult <- function(x, ...) {
  paste0("(", toString(x$left), " * ", toString(x$right), ")")
}
toString.matrix_sum <- function(x, ...) {
  paste0("(", toString(x$left), " + ", toString(x$right), ")")
}
print.matrix_expr <- function(x, ...) {
  cat(toString(x), "\n")
}




## Expression optimisation ####################################################
rearrange_matrix_expr <- function(expr) UseMethod("rearrange_matrix_expr")

rearrange_matrix_expr.matrix_data <- function(expr) {
  expr
}
rearrange_matrix_expr.matrix_mult <- function(expr) {
  matrix_mult(rearrange_matrix_expr(expr$left),
              rearrange_matrix_expr(expr$right))
}
rearrange_matrix_expr.matrix_sum <- function(expr) {
  matrix_sum(rearrange_matrix_expr(expr$left),
             rearrange_matrix_expr(expr$right))
}


### Multiplication optimisation ######
backtrack_matrix_mult <- function(i, j, dims, N, matrices) {
  if (i == j) {
    matrices[[i]]
  } else {
    k <- i:(j - 1)
    candidates <- dims[i,1]*dims[k,2]*dims[j,2] + N[i,k] + N[k + 1,j]
    split <- k[which(N[i,j] == candidates)][1]
    left <- backtrack_matrix_mult(i, split, dims, N, matrices)
    right <- backtrack_matrix_mult(split + 1, j, dims, N, matrices)
    matrix_mult(left, right)
  }
}

arrange_optimal_matrix_mult <- function(matrices) {
  n <- length(matrices)
  dims <- matrix(0, nrow = n, ncol = 2)
  for (i in seq_along(matrices)) {
    dims[i,] <- dim(matrices[[i]])
  }
  
  N <- matrix(0, nrow = n, ncol = n)
  for (len in 2:n) {
    for (i in 1:(n - len + 1)) {
      j <- i + len - 1
      k <- i:(j - 1)
      N[i,j] <- min(dims[i,1]*dims[k,2]*dims[j,2] + N[i,k] + N[k + 1,j])
    }
  }

  backtrack_matrix_mult(1, n, dims, N, matrices)  
}

collect_mult_components_rec <- function(expr, lst)
  UseMethod("collect_mult_components_rec")

collect_mult_components_rec.default <- function(expr, lst) 
  cons(rearrange_matrix_expr(expr), lst)

collect_mult_components_rec.matrix_mult <- function(expr, lst)
    collect_mult_components_rec(expr$left, collect_mult_components_rec(expr$right, lst))

collect_mult_components <- function(expr)
    lst_to_list(collect_mult_components_rec(expr, NULL))

rearrange_matrix_expr.matrix_mult <- function(expr) {
  matrices <- collect_mult_components(expr)
  arrange_optimal_matrix_mult(matrices)
}


## Evaluating ####################################################
eval_matrix_expr <- function(expr) 
  UseMethod("eval_matrix_expr")
eval_matrix_expr.matrix_data <- function(expr) 
  expr$data
eval_matrix_expr.matrix_mult <- function(expr)
  eval_matrix_expr(expr$left) %*% eval_matrix_expr(expr$right)
eval_matrix_expr.matrix_sum <- function(expr)
  eval_matrix_expr(expr$left) + eval_matrix_expr(expr$right)

## Evaluation function... #######################################
v <- function(expr) eval_matrix_expr(rearrange_matrix_expr(expr))

## Testing ########################################################
library(microbenchmark)

A <- matrix(1, nrow = 400, ncol = 300)
B <- matrix(1, nrow = 300, ncol = 30)
C <- matrix(1, nrow = 30, ncol = 500)
D <- matrix(1, nrow = 500, ncol = 400)
X <- matrix(1, nrow = 400, ncol = 400)

expr <- m(A) * m(B) * m(C) * m(D) * (m(X) + m(A) * m(B) * m(C) * m(D) + m(X))
expr
rearrange_matrix_expr(expr)


optimise_eval <- function(expr) {
  expr <- substitute(expr)
  v(build_matrix_expr(expr, parent.frame()))
}
#microbenchmark(A %*% B %*% C %*% D %*% (X + A %*% B %*% C %*% D + X),
#               optimise_eval(A %*% B %*% C %*% D %*% (X + A %*% B %*% C %*% D + X)))


#microbenchmark(v(expr), v2(expr), v3(expr))

#v(m(A) * m(B) * m(C) * m(D))

#microbenchmark(A %*% B %*% C %*% D,
#               eval_matrix_mult(expr1),
#               (A %*% B) %*% (C %*% D),
#               eval_matrix_mult(expr2))


#microbenchmark(A %*% B %*% C %*% D,
#               (A %*% B) %*% (C %*% D),
#               v(m(A) * m(B) * m(C) * m(D)))

#microbenchmark(A %*% B %*% C %*% D %*% (X + A %*% B %*% C %*% D + X),
#               v(m(A) * m(B) * m(C) * m(D) * (m(X) + m(A) * m(B) * m(C) * m(D) + m(X))))


A <- matrix(1, nrow = 10, ncol = 20)
B <- matrix(1, nrow = 20, ncol = 10)
C <- matrix(1, nrow = 10, ncol = 10)
v(m(A) * m(B) * m(C))
A %*% B %*% C
