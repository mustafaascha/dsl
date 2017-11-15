## Helper functionality -- lists ################################
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
toString.matrix_3sum <- function(x, ...) {
  paste0("(", toString(x$A), " 3+3 ", toString(x$B), " 3+3 ", toString(x$C), ")")
}
print.matrix_expr <- function(x, ...) {
  print(toString(x))
}




## Expression optimisation ####################################################
rearrange_matrix_expr <- function(expr) UseMethod("rearrange_matrix_expr")

# working solution, but doesn't call recursively...ok for matrix_data, though
rearrange_matrix_expr.default <- function(expr) expr

# better trivial solutions...
rearrange_matrix_expr.matrix_mult <- function(expr) {
  matrix_mult(rearrange_matrix_expr(expr$left),
              rearrange_matrix_expr(expr$right))
}
rearrange_matrix_expr.matrix_sum <- function(expr) {
  matrix_sum(rearrange_matrix_expr(expr$left),
             rearrange_matrix_expr(expr$right))
}


### Multiplication optimisation ######
backtrack_matrix_mult <- function(i, j, dims, tbl, matrices) {
  if (i == j) {
    matrices[[i]]
  } else {
    k <- i:(j - 1)
    candidates <- dims[i,1]*dims[k,2]*dims[j,2] + tbl[i,k] + tbl[k + 1,j]
    split <- k[which(tbl[i,j] == candidates)][1]
    left <- backtrack_matrix_mult(i, split, dims, tbl, matrices)
    right <- backtrack_matrix_mult(split + 1, j, dims, tbl, matrices)
    matrix_mult(left, right)
  }
}

arrange_optimal_matrix_mult <- function(matrices) {
  n <- length(matrices)
  dims <- matrix(0, nrow = n, ncol = 2)
  for (i in seq_along(matrices)) {
    dims[i,] <- dim(matrices[[i]])
  }
  
  tbl <- matrix(0, nrow = n, ncol = n)
  for (len in 2:n) {
    for (i in 1:(n - len + 1)) {
      j <- i + len - 1
      k <- i:(j - 1)
      tbl[i,j] <- min(dims[i,1]*dims[k,2]*dims[j,2] + tbl[i,k] + tbl[k + 1,j])
    }
  }
  
  backtrack_matrix_mult(1, n, dims, tbl, matrices)  
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

## Sum optimisation ##############################################
matrix_3sum <- function(A, B, C) {
  structure(list(A = A, B = B, C = C),
            nrow = nrow(A),
            ncol = ncol(A),
            class = c("matrix_3sum", "matrix_expr"))
}

rearrange_matrix_expr.matrix_sum <- function(expr) {
  if (inherits(expr$left, "matrix_sum")) {
    new_expr <- matrix_3sum(expr$left$left, expr$left$right, expr$right)
    rearrange_matrix_expr(new_expr)
  } else if (inherits(expr$right, "matrix_sum")) {
    new_expr <- matrix_3sum(expr$left, expr$right$left, expr$right$right)
    rearrange_matrix_expr(new_expr)
  } else {
    matrix_sum(rearrange_matrix_expr(expr$left),
               rearrange_matrix_expr(expr$right))  
  }
}

rearrange_matrix_expr.matrix_3sum <- function(expr) {
  matrix_3sum(rearrange_matrix_expr(expr$A),
              rearrange_matrix_expr(expr$B),
              rearrange_matrix_expr(expr$C))
}

sum3 <- function(A, B, C) {
  result <- matrix(0, nrow = nrow(A), ncol = ncol(A))
  for (i in seq_along(nrow(A))) {
    for (j in seq_along(ncol(A))) {
      result[i,j] <- A[i,j] + B[i,j] + C[i,j]
    }
  }
  result
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
eval_matrix_expr.matrix_3sum <- function(expr)
  sum3(eval_matrix_expr(expr$A),
       eval_matrix_expr(expr$B),
       eval_matrix_expr(expr$C))

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
