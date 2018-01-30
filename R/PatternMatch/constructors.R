
make_args_list <- function(args) {
  res <- replicate(length(args), substitute())
  names(res) <- args
  as.pairlist(res)
}



library(rlang)

process_arguments <- function(constructor_arguments) {
  process_arg <- function(argument) {
    stopifnot(is.call(argument))
    stopifnot(argument[[1]] == ":")
    arg <- quo_name(argument[[2]])
    type <- quo_name(argument[[3]])
    tibble::tibble(arg = arg, type = type)
  }
  constructor_arguments %>% as.list %>% purrr::map(process_arg) %>% bind_rows
}

process_constructor <- function(constructor, data_type_name, env) {
  stopifnot(is.call(constructor))
  constructor_name <- quo_name(constructor[[1]])
  constructor_arguments <- process_arguments(constructor[-1])

  # create the constructor function
  constructor <- function() {
    args <- as_list(environment())

    # type check!
    stopifnot(length(args) == length(constructor_arguments$arg))
    for (i in seq_along(args)) {
      arg <- args[[constructor_arguments$arg[i]]]
      type <- constructor_arguments$type[i]
      stopifnot(type %in% class(arg))
    }

     structure(args,
               constructor = constructor_name,
               class = data_type_name)
  }
  formals(constructor) <- make_args_list(constructor_arguments$arg)

  # set meta information about the constructor
  class(constructor) <- c("constructor", "function")

  # put the constructor in the binding scope
  assign(constructor_name, constructor, envir = env)
}

process_alternatives <- function(constructors, data_type_name, env) {
  stopifnot(is.call(constructors))
  if (constructors[[1]] == "|") {
    process_alternatives(constructors[[2]], data_type_name, env)
    process_alternatives(constructors[[3]], data_type_name, env)
  } else {
    process_constructor(constructors, data_type_name, env)
  }
}

deparse_construction <- function(object) {
  constructor_name <- attr(object, "constructor")
  if (is_null(constructor_name)) {
    # this is not a constructor, so just get the value
    return(as.character(object))
  }

  components <- names(object)
  values <- as_list(object) %>% purrr::map(deparse_construction)

  print_args <- vector("character", length = length(components))
  for (i in seq_along(components)) {
    print_args[i] <- paste0(components[i], " = ", values[i])
  }

  paste0(constructor_name, "(", print_args, ")", collapse = ", ")
}
construction_printer <- function(x, ...) {
  cat(deparse_construction(x), "\n")
}

`:=` <- function(data_type, constructors) {
  data_type <- enquo(data_type)
  constructors <- enexpr(constructors)

  stopifnot(quo_is_symbol(data_type))
  data_type_name <- quo_name(data_type)

  c(data_type_name, constructors)
  process_alternatives(constructors, data_type_name, get_env(data_type))
  assign(paste0("print.", data_type_name), construction_printer, envir = get_env(data_type))
}


tree := T(left : tree, right : tree) | L(value : numeric)

x <- T(T(L(1),L(2)),L(3))
x

x$left$left$value
x$left$right$value
x$right$value

