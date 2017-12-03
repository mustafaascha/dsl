## Components of a programming language

While this is not a book about compilers and computer languages in general, it will be helpful to have some basic understanding of the components of software that parse and manipulate computer languages—or at least domain specific computer languages.

When we write software for processing languages, we usually structure this such that the input gets manipulated in distinct phases from the raw input text to the desired result, a result that is often running code or some desired data structures. When processing an embedded DSL, however, there isn’t necessarily a clear separation between parsing your DSL, manipulating expressions in the language, and evaluating them. In many cases, embedded DSLs describe a series of operations to be done sequentially—this is, for example, the case with graphical transformations in `ggplot2` or data transformations in `magrittr` and `dplyr`. When this is the case, you wouldn’t necessarily split evaluations of DSL expressions into a parsing phase and an evaluation phase; you can perform transformations one at a time as they are seen by the R parser. Conceptually, though, there are still two steps involved—parsing a DSL statement and evaluating it—and with more complex DSL you have to be explicit about this. Even for simple DSLs, however, there are benefits to keeping the different processing phases separate. It introduces some overhead in programming as you need to represent the input language in some explicit form before you can implement its semantics, but it also allows you to separate the responsibility of the various processing phases into separate software modules, making those easier to implement and test.

This chapter describes the various components of computer languages and the phases involved in processing a domain specific language. 


### Text, tokens, grammars, and semantics

First, we need to define some terminology. Since this book is not about language or parser theory, so I will stick with some informal working definitions, but there are a few terms we need in this chapter that we need to define.

When we consider at a language, we can look at it at different levels of detail, from the most basic components to the meaning associated with expressions and statements. For a spoken language, the most basic components are the *phonemes*—the distinct sounds used in the language. Strung together, phonemes become words, words combine to make sentences, and sentences have meaning. For a written language, the atomic elements are *glyphs*—the letters in languages written using alphabets, such as English. Sequences of letters can form words, but a written sentence contains more than just words—we have punctuation symbols as well. Together, we can call these *tokens*. A string of *tokens* form a sentence, and again, we can assign meaning to sentences.

For computer languages, we have the same levels of abstractions on strings of symbols. The most primitive level is just a stream of input characters, but we will have rules for translating such character sequences into sequences of *tokens*. This process is called *tokenisation*. The formal definition of a programming language will specify what the available tokens in the language are, and how a string of characters should be translated into a string of tokens.

Consider the following string of R code:

```r
foo(x, 2*x)
```

This is obviously a function call, but seen by the tokeniser it is a string of characters that it needs to translate into a sequence of tokens. What it will produce is this:

```
identifier["foo"] '(' identifier["x"], 
                      number[2], '*', identifier["x"] 
                   ')'
```

I'm using a home-brewed notation for this, but the idea is that a tokeniser will recognise that there are some identifiers—and it will identify those and what the identifiers are—and a number and then some verbatim tokens such as `'('`, `'*'`, and `')'`.

The tokeniser, however, will be equally happy to process a string such as

```r
foo x ( 2 ) x *
```

into the sequence

```
identifier["foo"] identifier["x"] '('
                  number[2] ')' identifer["x"] '*'
```

This is clearly not a valid piece of R code, but the tokeniser does not worry about this. It simply translates the string into a sequence of tokens (with some associated data, such as the strings "foo" and "x" for the identifiers and the number 2 for the number). It doesn't worry about higher levels of the language.

When it comes to tokenising an embedded language, we are bound to what that language will consider valid tokens. We cannot create arbitrary kinds of tokens since all languages we write as embedded DSLs must also be valid R. The tokens we can use are either already R tokens or variables and functions we define to have special meaning. Mostly, this means creating objects through function calls and defining functions for operator overloading.

What a language considers a valid string of tokens is defined by its *grammar*.^[Technically, what I refer to as *grammar* is actually *syntax*. Linguists use *grammar* to refer to both *morphology* and *syntax*, where *syntax* is the rules for stringing words together. In computer science, though, the term *grammar* is used as I use it here, so I will use syntax and grammar interchangeably.] A *parser* is responsible for translating a sequence of tokens into an expression or a language statement. Usually, what a parser does is translate a string of tokens into an expression *tree*—often referred to as an *abstract syntax tree* (AST).[^ast-vs-parse-tree] The tree structure associates more structure to a piece of code than the simple sequential structure of the raw input and the result of the tokenisation.  An example of how an abstract syntax tree for the function call we tokenised earlier could look like is shown in [@fig:example-AST]. Here, the italic labels refer to a syntactic concept in the grammar while the monospace font labels refer to verbatim input text. Tokens are shown in grey boxes. As we saw, these can either be verbatim text or have some grammatical information associated, describing what type of token they are (in this example, this is either an identifier or a number). When there is information associated, I have chosen to show this as two nodes in the tree, one that describes the syntactical class the token is (identifier or number) and a child of that node that contains the actual information (`foo`, `x`, and `2` in this case).

[^ast-vs-parse-tree]: The purists might complain here and say that a parse will construct a *parse tree* and not an AST. The difference between the two is that a parse tree contains all the information in the input, parentheses and spaces and all, but not the meta-information about which grammatical structures they represent. The AST contains only the relevant parts of the input but does include grammatical information on top of that. If you want to, you can consider parsing and then translating the result into an AST as two separate steps in handling an input language. I consider them part of the same and will claim that a parser constructs an AST.

![Example of an abstract syntax tree for a concrete function call.](figures/example-AST){#fig:example-AST}

Grammatical statements are those a parser will consider valid. It is, if we return to natural languages, those sentences that obey the grammatical rules. This is distinct from the set of sentences that have some associated *meaning*. It is quite possible to construct meaningless, but grammatically correct, sentences. The sentence "Colourless green ideas sleep furiously" is such a sentence, created by the linguist Noam Chomsky. It is completely correct grammatically and also completely meaningless. *Semantics* is the term we use to link grammatical sentences to their meaning. You will know this distinction in programming languages when you run into runtime exceptions. If you get an exception when you run a program, you will have constructed a grammatical sentence—otherwise the parser would have complained about syntactical errors—but a sentence that does not have a well defined meaning. Perhaps because you try to add a number to a string—this happens when the statements you have written violates type rules. Semantics, when it comes to programming languages, define what actual computations a statement describes. A compiler or an interpreter—the latter for R programs—gives meaning to grammatical statements.^[Notice, however, that there is a distinction between giving a statement meaning and giving it the *correct* meaning. Just because your program computes *something* doesn't mean that it computes what you intended it to. When we construct a language, domain specific or general, we can give meaning to statements, but we cannot—this is theoretically impossible—guarantee that it is the *intended* meaning. That will always be the responsibility of the programmer.]

For embedded DSLs, the semantics of a program is simply what we do to evaluate an expression once we have parsed it. We are not going to formally specify semantics or implement interpreters, so for the purposes of this book, the semantics part of a DSL is just plain old R programs. More often than not, what we use embedded DSLs for is an interface to some library or framework. It is the functionality of this framework that provides the semantics of what we do with the DSL, the actual language is just an interface to the framework.

### Specifying a grammar

Since we are using R to parse expressions, we do not have much flexibility in what can be considered tokens and we have some limitations in the kinds of grammar we can implement, but for the grammars we also have some flexibility. To specify grammars in this book, I will take a traditional approach and describe them in terms of “rules” for generating sentences valid within a grammar. Consider the following grammar:

```
EXPRESSION ::= NUMBER 
            |  EXPRESSION '+' EXPRESSION
            |  EXPRESSION '*' EXPRESSION
            |  '(' EXPRESSION ')'
```

This grammar describes rules for generating expressions consisting of addition and multiplication of numbers, with parentheses to group expressions.

You should read this as “an expression is either a number, the sum of two expressions, the product of two expressions, or an expression in parentheses”.  The definition is recursive—an expression is defined in terms of other expressions—but we have a basis case, a number, that lets us create a basic expression, and from such an expressions we can create more complex expressions.

The syntax I use here for specifying grammars is itself a grammar—a meta-grammar if you will. The way you should interpret it is thus: the grammatical object we are defining is to the left of the `::=` object. After that, we have a sequence of one or more ways to construct such an object, separated by `|`. These rules for constructing the object we define will be a sequence of other grammatical objects. These can either be objects we define by other rules—I will write those in all capitals and refer to them as meta-variables—or concrete lexical tokens—I write those in single quotes, as the `'+'` in the second rule for creating a sum. This notation is analogue to the graphical notation I used in [@fig:example-AST] where meta-variables are shown in italics and concrete tokens are put in grey boxes.

Meta-grammars like this are used to formally define languages, and there are many tools that will let you automatically create parsers from a grammar specification in a meta-grammar. I will use this home-made meta-grammar much less formally. I just use it as a concise way of describing the grammar of DSLs we create, and you can think of it as simply pseudo-code for a grammar.

To create an expression we must follow the meta-grammar rules, so we must use one of the four alternatives provided: Either reduce an expression to a number, a sum or product, or create another in parentheses. For example, we can apply the rules in turn and get:

```
EXPRESSION > EXPRESSION '*' EXPRESSION                        (3)
           > '(' EXPRESSION ')' '*' EXPRESSION                (4)
           > '(' EXPRESSION '+' EXPRESSION ')' '*' EXPRESSION (2)
           > '(' number[2] '+' number[2] ')' '*' number[3]  (1x3)
```

Which lets us construct the expression `(2 + 2) * 3` from the rules.

If there are several different ways to go from meta-variables to the same sequence of terminal rules (so there are several rules that lead to the exact sequence of lexical tokens), then we have a problem with interpreting the language. The exact same sequence of tokens could be interpreted as specifying two different grammatical objects. For the expression grammar, we have ambiguities when we have expressions such as `2 + 2 * 3`. We can parse this in two different ways, depending on which rules we apply to get from the meta-variable `EXPRESSION` to the concrete expression. We can apply multiplication first and get what amounts to `(2 + 2) * 3`, or we can apply the addition rule first and get `2 + (2 * 3)`. We know from traditional mathematical notation that we should get the second expression—multiplication has higher precedence than addition so the `*` symbol binds 2 and 3 together tighter than `+` does 2 and 2, but the grammar does not guarantee this. The grammar is ambiguous.

It is possible to fix this by changing the grammar to this:

```
EXPRESSION ::= TERM '+' EXPRESSION | TERM
TERM ::= TERM '*' FACTOR | FACTOR
FACTOR ::= '(' EXPR ')' | NUMBER
```

This is a more complex grammar that lets you create the same expressions, but through three meta-variables that are recursively defined in terms of each other. It is structure such that products will naturally group closer than sums—the only way to construct the expression `2 + 2 * 3` is **FIXME: FIGURE**. The order in which we apply the rules can vary, but the tree will always be this form and group the product closer than the sum.

An unambiguous grammar is preferable over an ambiguous for obvious reasons, but creating one can complicate the specification of the grammar, as we see for expressions. This can be alleviated by making smarter parser that take such things as operator precedence into account or keep track of context when parsing a string.

When writing embedded DSLs, we are stuck with R's parser, and we must obey its rules. If you are writing your own parser entirely, you can pass context along as you parse a sequence of tokens, but if you want to exploit R’s parser and create an embedded DSL, you are better off ensuring that all grammatically valid sequences of tokens unambiguously refer to one grammatical meta-variable. Precedence ambiguities will be taken care of by R as will associativity—the rules that means that `1 + 2 + 3 + 4` is interpreted as `(((1 + 2) + 3) + 4)`. Exploiting R's parsing rules, we can construct languages where each expression uniquely matches a parser meta-variable if we are a little careful with designing the language.

**FIXME: state space / graph example**

### Designing semantics

The reason we write domain specific languages is to achieve some effect—we want to associate meaning, or semantics, to expressions in the DSL and we want our DSL expressions to achieve some result, whether that is executing some computations or building some data structures. The purpose of the DSL is simply to provide a convenient interface to whatever 
Semantics we want the language to have.

If we always make our parsing code construct a parse tree, then the next step in processing the DSL involves manipulation of this tree to achieve a desired effect. There might be a number of steps involved in this—for example, we rewrote expressions in the matrix expression example in order to optimise computations—but at the end of the processing we will execute the commands the DSL expression describes.

Executing the DSL is sometimes straightforward and can be done as a final traversal of the parse tree. This is what we did with the matrix expressions where the purpose of the DSL was to rewrite expressions rather than evaluating them—the latter being a simple matter of multiplying and adding matrices. In other cases, it makes sense to separate the semantic model and the DSL by having a framework for the actions we want the language to allow for. Having a framework for the semantics of the language lets us develop and test the semantic model separately from the language we use as an interface for it, and it even allows us to have different input languages for manipulating the same semantic model if different people prefer different flavors of DSL—not that I would recommend having many different languages to achieve the same goals.



