
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

collect_symbols_rec <- function(expr, lst, bound) {
  if (is.symbol(expr) && expr != "") {
    if (as.character(expr) %in% bound) lst
    else cons(as.character(expr), lst)
    
  } else if (is.pairlist(expr)) {
    for (i in seq_along(expr)) {
      lst <- collect_symbols_rec(expr[[i]], lst, bound)
    }
    lst
    
  } else if (is.call(expr)) {
    if (expr[[1]] == as.symbol("function"))
      bound <- c(names(expr[[2]]), bound)
    
    for (i in 1:length(expr)) {
      lst <- collect_symbols_rec(expr[[i]], lst, bound)
    }
    lst
    
  } else {
    lst
  }
}

collect_symbols <- function(expr) {
  expr <- substitute(expr)
  bound <- c()
  lst <- collect_symbols_rec(expr, NULL, bound)
  lst %>% lst_to_list %>% unique %>% purrr::discard(exists, parent.frame())
}

collect_symbols(2 * x + y)
collect_symbols(function(x) 2 * x + y)
collect_symbols(function(x) function(y) f(2 * x + y))

collect_symbols(function(x, y = 2 * w) 2 * x + y)
