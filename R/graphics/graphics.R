
# linked list so we can add elements to a list in constant time
cons <- function(head, tail) {
  list(head = head, tail = tail)
}

## Canvas #######
canvas <- function(width, height) {
  structure(list(width=width, height=height, elements = NULL),
            class = "canvas")  
}

`+.canvas` <- function(x, y) {
  x$elements <- cons(y, x$elements)
  x
}

plot.canvas <- function(x, ...) {
  # create the canvas to draw on
  plot(c(0, x$width), c(0, x$height), type = 'n', 
       axes = FALSE, xlab = '', ylab = '')
  
  # then draw all the elements
  elements <- x$elements
  while (!is.null(elements)) {
    draw(elements$head)
    elements <- elements$tail
  }
}

## Graphical object methods #####
graphical_object <- function() {
  structure(list(), class = "graphical_object")
}

draw <- function(obj) UseMethod("draw")

## Transformations
move <- function(x, y) function(obj) move_(obj, x, y)
move_ <- function(obj, x, y) UseMethod("move_")
scale <- function(fac) function(obj) scale_(obj, fac)
scale_ <- function(object, fac) UseMethod("scale_")

`*.graphical_object` <- function(obj, trans) {
  trans(obj)
}

## Points ######
point <- function(x, y) {
  object <- graphical_object()
  object$x <- x
  object$y <- y
  class(object) <- c("point", class(object))
  object
}

draw.point <- function(x) {
  points(x$x, x$y, pch = 19)
}

move_.point <- function(obj, x, y) {
  obj$x <- obj$x + x
  obj$y <- obj$y + y
  obj
}

## Lines ######
line <- function(x1, y1, x2, y2) {
  object <- graphical_object()
  object$x1 <- x1
  object$y1 <- y1
  object$x2 <- x2
  object$y2 <- y2
  class(object) <- c("line", class(object))
  object
}

draw.line <- function(x) {
  lines(c(x$x1, x$x2), c(x$y1, x$y2))
}

move_.line <- function(obj, x, y) {
  obj$x1 <- obj$x1 + x
  obj$x2 <- obj$x2 + x
  obj$y1 <- obj$y1 + y
  obj$y2 <- obj$y2 + y
  obj
}


## Test ####
drawing <- canvas(10, 10) 
p <- point(0, 0)
l <- line(0, 0, 1, 1)
for (i in 0:10) {
  drawing <- drawing + p * move(i, 0) + p * move(i, 1) + l * move(i, 0)
}
plot(drawing)

