# Environments and expressions {#sec:env_and_expr}

We have already used environments in a couple of examples to evaluate expressions in a different context than they are usually evaluated, something known as *non-standard evaluation*. Many domain specific languages we could want to implement in R will need some sort of non-standard evaluation,  but getting evaluation to occur in the right context can be problematic. The rules for how expressions are evaluated are simple, but the way evaluation contexts—environments—are chained together can be complex.

## Scopes and environments

Whenever R evaluates an expression, it does so in a *scope*. A scope determines which values any given variable refers to. In standard evaluation, R uses what is known as *lexical scope*, which essentially means that variables in an expression refers to the variables defined in blocks around the expression. If you write an expression at the outermost level of an R script—the so-called *global environment*—then variable names in the expression refer to global variables. An expression inside a function, on the other hand, is evaluated in the scope of a function call, which means that variable symbols refer to local variables or function parameters, if these are defined, and only if they are not defined do they refer to global variables. A function defined inside another function will have a *nested scope*—variables in an expression there will be searched for first in the innermost function, then the surrounding function, and only if they are not found either place, in the global environment.

Consider this abstract example:

```{r}
x <- 1
f <- function(y) {
  z <- 3
  function() x + y + z
}
g <- f(2)
h <- f(3)
g()
h()
```

Here, we define four variables in the global environment, `x`, `f`, `g`, and `h`. In the function `f` we have one formal parameter, `y`, and one local variable `z`. Whenever we call `f`, a scope where `y` exists is created and the first statement in the function call is to also add `z` to this scope. The function returns another function that contains an expression that refers to variable `x`, `y`, and `z`. If we call this function, which we do when we call functions `g` and `h`, which are the results of two separate calls to `f`, this expression will be evaluated. When R evaluates the expression, it needs to find the three variables. They are neither formal arguments or local variables in the functions we call, `g` and `h`, but since the functions were created inside calls to `f`, they can see `y` and `z` in the surrounding scope, and both can find `x` in the global environment. Since `g` and `h` are the result of separate calls to `f`, the surrounding scope of calls to them are *different* instances of local scopes of `f`.

Scopes are implemented through *environments*, and the rules that guide environments and evaluation are very simple—even though you have to be careful if you start manipulating them. Environments are tables that map variables to values and all environments have a parent environment, a surrounding scope, that R will search in if a variable is not found when it searches the environment. The different environments you can evaluate expressions in have a tree structure with a root in the *empty environment*. This environment is the root of all environments. Packages you load are put on top of this environment, and on top of all loaded environments we have the *global environment*—which is why you can find variables define in packages if you search in the global environment. Strictly speaking there are a few other details in how packages and environments interact, but they are not important for the discussion here. If you are interested, you can find a description of this in my other book, *Meta-programming in R*. For the purpose of this book, we will simply assume that everything we define at the global level or any package is found in the global environment and consider this the root of the environment tree.

When we define new functions we do not create new environments, but we do associate the functions with one—the environment in which we define the function. When we defined function `f` in the example above, it got associated with the global environment, because that is where we defined it. We can get the environment a function is associated with using the `environment` function:

```{r}
environment(f)
```

Since `f` is defined at the global level, its environment is the global environment. When we call a function is when we create a new environment. This environment is where we store parameters and local variables. The environment associated with the function will be made the *parent environment* for this function-call environment. When we call function `f` we thus create an environment where we get a mapping from `y` and `z` to their values and with a parent environment that is the global environment, in which we can find the variable `x`. Inside the call to `f` we create a new (anonymous) function and return it. This function will also have an environment associated with it, but this time it is the local environment we created when we called `f`. Thus, the environments associated with `g` and `h` are two different environments as they are the result of two different calls to `f`.

```{r}
environment(g)
environment(h)
```

Functions defined inside other functions thus carry along with them environments that were created when the surrounding function was called and if we return them from the surrounding function they still carry this enclosing scope along with them—remembering local variable and parameters from the enclosing scope—and we call such functions *closures*.

In [@fig:calling-g-environments] I have drawn a slightly simplified graph of which environments exist and how they are wired together in the example at the point where we call function `g`. Environments are shown with a grey background, variables as circles with pointers to the values the variables refer to, and functions are shown as the three components that define a function: the formal parameters, the function body, and the environment associated with the function—the enclosing scope.

The enclosing environment for function `f` is, as we have already discussed, the global environment, while the enclosing scopes of `g` and `h` are two different instances of calls to `f`. These instance-environments have the global environment as their parents since that is the enclosing scope of `f`, the function they are instances of. Because they are two different instances of `f`, the variables in them can point to different values, as we see for variable `y`. For function `g`, variable `y` points to 3, while for function `h`, `y` points to 2. In a call to function `g` we create a local environment for the function call—shown at the bottom right in the figure. We do not have any local variables in `g` so this environment does not contain any variables, but it has a parent which is the `f` instance where we created `g`.

![Environment graph when calling `g`.](figures/calling-g-environments){#fig:calling-g-environments}

When we evaluate the expression `x + y + z` inside the call to `g` we need to map variables to values. The search will start in the local environment and then progress up the parent links until it finds a matching variable. For variables `y` and `z` we find values in the parent of the `g` call—the instance of the `f` call that created `g`—and for `x` we find the value in the grandparent, the global environment.

The rule for evaluating expressions is always the same: we look up variables by searching in environments, starting with the immediate environment where we evaluate the expression and searching up the chain of parent environments and check these in turn until we find the variable we are looking for. We get the standard evaluation rules of lexical scoping because functions get associated with the environment in which they are created and since this environment is set as the parent environment of instances of function calls. The only trick to understanding how expressions are evaluated in R is to understand which environments are used. For the body of functions it is as simple  as I have just explained, but for function parameters there are a few more rules to consider…

## Default parameters, lazy evaluation, and promises

When you pass primitive values such as numbers to a function parameter there is nothing we need to evaluate, so there are no complications. This is why we didn’t have to worry about the environment of the arguments in the previous example. If we pass expressions along as parameters, however, we need to know how these should be evaluated.

Most of the time, R behaves as if expressions are evaluated before a function is called, but this isn’t actually what happens. If we passed values along to functions instead of expressions, we would have been able to get the expressions using `substitute` as we have done in previous chapters. When we call a function in R, the parameters will refer to unevaluated expressions, so-called *promises*. These expressions are evaluated the first time we use a parameter variable but not before—an approach to parameter evaluation known as *lazy evaluation*. If we never refer to an argument, the corresponding expression will never be evaluated, so we can write code such as this without raising exceptions:

```{r}
f <- function(x, y) x
f(2, stop("error!"))
```

We never refer to the parameter `y` inside the body of `f`, so we never evaluate it. Consequently, we never call `stop` to raise the error.

So, since parameters can contain expressions, we need rule for how to evaluate these. Here, there is a difference between default parameters—specified when the function is created—and parameters provided when the function is called. The former is evaluated in the local scopes of function calls while the latter is evaluated in the environment where the function is called.

Consider this function:

```{r}
f <- function(y, z = 2 * y) y + z
```

The function takes two parameters, `y` and `z`

```{r}
f(2, 1)
```

but if we only provide `y`, then `z` will be set to `2 * y`:

```{r}
f(2)
```

When we evaluate the promise that `z` points to when the function is called—we do this in the expression where we use the variable—the promise-expression is evaluated. This means that R needs to find the variable `y`. If we tried to evaluate the expression `2 * y` in the scope where the function is defined—the global environment—then we would get an error; there is no `y` variable defined there. The semantics of default parameters *could* be such that we evaluated them in the scope where we define a function, but if so, we wouldn’t be able to make default parameters depend on other parameters, which is what we want here—we want `z` to depend on `y` if we do not explicitly provide a value to it. The actual semantics is that the promise is evaluated in the function-call environment. When we call `f`, before we evaluate the `y + z` expression, the situation is therefore as shown in [@fig:default-parameters-f]. Here, I have drawn the promise for `z` as the expression passed along as the function argument together with the environment in which it should be evaluated.

![Default parameter promise.](figures/default-parameters-f){#fig:default-parameters-f}

When we call `f` with a parameter that is an expression we do *not* want to evaluate this expression in function-call scope. Consider this:

```{r}
y <- 2
f(2 * y)
```

Here, clearly, the intent is to call `f` with `2 * y` which should be `4` since `y` is 2. If we tried to evaluate it inside the function call, however, we would have a circular dependency. Inside the function call, `y` is a variable and if it points to `2 * y` we cannot evaluate the expression without known what `y` is, which we cannot know until we have evaluated the expression, which we cannot…

When we call a function with an expression as argument, the corresponding promise will be evaluated in the environment where we call the function, so before we evaluate `y + z` inside `f`, the situation is as shown in [@fig:default-and-promise-parameters-f]. Inside the environment of the function call, both `y` and `z` refer to promises, but these promises are associated with different environments. To evaluate the expression `y + z` we need to evaluate both promises. To get the value for `y` we need to evaluate `2 * y` *in the global scope*, which gives us 4 and to get the value for `z` we need to evaluate `2 * y` *in the local scope*, which gives us 8.

![Calling `f` with an expression for `y`.](figures/default-and-promise-parameters-f){#fig:default-and-promise-parameters-f}

At the risk of taking the example a step too far, let us consider the situation where we call `f` from another function:

```{r}
g <- function(x) f(2 * x)
g(2 * y)
```

Just before we evaluate the expression `y + z` inside function `f`, the environment graph is connected as shown in [@fig:default-and-promise-parameters-g]. It takes a little effort to decode what happens when we want to evaluate `y + z`, but doing this exercise will go a long way towards understanding environments.

![Calling `g` with an expression for `x` that depend on variable `y`.](figures/default-and-promise-parameters-g){#fig:default-and-promise-parameters-g}

Both `y` and `z` are promises we haven’t evaluated yet. Since `z` depend on `y` we need to evaluate `y` first. To do this, we need to evaluate the expression `2 * x` in the scope of the call to `g`. Here, we need to evaluate `x` which is another promise, the expression `2 * y` which should be evaluated in the global scope (where `y` refers to a different variable than then local variable inside the `f` instance). In the global scope, `y` refers to the value 2, so we can evaluate `2 * y` directly and get the value 4. This value then replaces the promise in the scope of the call to `g`. Once we have evaluated a promise, the variable refers to the value and no longer the expression—more on that below. This now means that we can evaluate `2 * x` in the scope of the `g` call to get 8. So now `y` in the call to `f` refers to 8. This means we can evaluate `2 * y` to get 16 which we assign the variable `z`. Finally, we can evaluate `y + z` to get 8 + 16 = 24.

To summarise this section, parameters we pass to functions, if these are not primitive values, are considered expressions that must be evaluated at some point, and together with the expressions we have an associated scope in which to evaluate the expression. There is one more caveat, though, which I hinted to in the previous example: parameters are only considered expressions until the first time we evaluate them. After that, they are the result of this evaluation.

There are pros and cons with this semantics—although, to be honest, predominantly cons. Because promises are not evaluated until we refer to the variable that holds them we can avoid computing values we do not need and we can make default parameters that depend on some computation inside a function call as long as we do those computations before we use the variable that needs them. For example, we can define a default parameter in terms of a variable we set inside a function

```{r}
h <- function(x, y = 2 * w) {
  w <- 2
  x + y
}
h(1)
```

but this will fail if we refer to the promise that needs the variable before we compute it:

```{r}
h <- function(x, y = 2 * w) {
  res <- x + y
  w <- 2
  res
}
h(1)
```

If a promise depend on a variable that we update during a computation we also have to be careful. The promise is only ever evaluated once; after we have evaluated a promise, the variable that used to hold it now hold the result of the evaluation and no longer the promise expression. If we change variables that occurred in the promise, we do not update the value the variable now hold.

```{r}
h <- function(x, y = 2 * w) {
  w <- 1
  res <- x + y
  w <- 2
  res
}
h(1)
```

For promises held by default parameters, this usually does not cause any problems. It is simple to follow what local variables will change in the function and at which point the promise will be evaluated. Lazy evaluation of arguments, however, is a common source of problems when combined with closures. Consider this function:

```{r}
make_adder <- function(n) function(m) n + m
```

which returns a closure that will add `n` to its argument, `m`. We can use it like this:

```{r}
add_1 <- make_adder(1)
add_2 <- make_adder(2)
add_1(1)
add_2(1)
```

No problems here, but now consider this:

```{r}
adders <- vector("list", 3)
for (i in 1:3) adders[[i]] <- make_adder(i)
```

The intent here is to create three adder functions that add 1, 2, and 3, respectively, to their argument. When we call the first function, though, we get an unpleasant surprise:

```{r}
adders[[1]](1)
```

The expression `n + m` inside the closure is not evaluated until we call it. Just before we evaluate the body in the `adders[[1]](1)` call, the environment graph looks like [@fig:addders]. All three adders are closures that refer to different instances of `make_adders`, but all these instances have `n` refer to a promise that is the expression `i`. The variable `i` is found in the global environment and not in the closure environment, and after we have created all three closures, `i` refers to the number 3. To evaluate `n + m` inside the adder, we must first evaluate the promise that `n` refers to—we search for `n` and find it in the parent environment of the function call, which is the closure environment, and here `n` refers to `i` that should be evaluated in the global environment. We evaluate it and now `n` refers to 3, see [@fig:adders-2]. This is why the result of calling `adders[[1]]` with `m` set to one returns four and not two.

![Adders just before evaluating the body of the `adders[[1]](1)` call.](figures/adders){#fig:adders}

![Adders after evaluating the `n` promise in `adders[[1]]`.](figures/adders-2){#fig:adders-2}

After we have called this closure, the variable `n` no longer refers to a promise but to the value 3, so changing `i` at this point will not affect the closure:

```{r}
i <- 1
adders[[1]](1)
```

It will, however, affect the closures where we haven’t evaluated the promise yet, so if we call one of the other closures after changing `i` we will see result of the change:

```{r}
adders[[2]](1)
```

This is a problem that only occurs when you create closures, but every time you do, the risk is there. You can avoid the problem by explicitly evaluating promises before you return the closure; this is what the function `force` is for.

```{r}
make_adder <- function(n) {
  force(n)
  function(m) n + m
}
for (i in 1:3) adders[[i]] <- make_adder(i)
for (i in 1:3) print(adders[[i]](0))
```


## Quotes and non-standard evaluation

## Quosures

## Quasi-quoting

