
`%do%` <- function(x, f) {
  f <- substitute(f)
  if (class(f) == "{") {
    cat(deparse(f[[2]])); cat("\n")
    cat(f[[2]][[2]])
  } else {
    f <- eval(f, parent.frame())
    unlist(Map(f, x))
  }
}

f <- function(x) x
1:5 %do% f
1:5 %do% function(x) x**2

1:5 %do% { x | x**2 }


print_expression <- function(expr, indent = "") {
  if (is.atomic(expr)) {
    if (inherits(expr, "srcref")) {
      expr <- paste0("srcref = ", expr)
    }
    cat(indent, " - ", expr, "\n")
    
  } else if (is.name(expr)) {
    if (expr == "") {
      expr <- "MISSING"
    }
    cat(indent, " - ", expr, "\n")
    
  } else if (is.pairlist(expr)) {
    cat(indent, " - ", "[\n")
    new_indent <- paste0(indent, "       ")
    vars <- names(expr)
    for (i in seq_along(expr)) {
      cat(indent, "    ", vars[i], " ->\n")
      print_expression((expr[[i]]), new_indent)
    }
    cat(indent, "    ]\n")
    
  } else {
    print_expression((expr[[1]]), indent)
    new_indent <- paste0("  ", indent)
    for (i in 2:length(expr)) {
      print_expression(expr[[i]], new_indent)
    }
  }
}

print_expression(quote(2 * x + y))
print_expression(quote(function(x) x))
print_expression(quote( (function(x) x)(2)))
print_expression(quote(function(x, y = 2 * x) x + y))
