---
date: 2024-11-20
tags: links programming
---

# Interesting Links for November 2024

A special *Programming Languages: Theory, Design and Implementation* edition of some interesting articles I recently read on the internet:

- There is something amazing about making your own programming language. In ["You Should Make a New Programming Language"](https://ntietz.com/blog/you-should-make-a-new-terrible-programming-language/) Nicole Tietz-Sokolsaya puts forward some great reasons to do the same, but I do it just for the sheer excitement of witnessing a program written in my own language run.

- Why aren't there programming languages that are convenient to write but slow by default, and allow the programmer to drop to a harder to write but more performant form, if required? Alex Kladov ponders on this question in ["On Ousterhout’s Dichotomy"](https://matklad.github.io/2024/10/06/ousterhouts-dichotomy.html), and offers a possible solution.

- I am big fan of [_Algebraic data types_](https://en.wikipedia.org/wiki/Algebraic_data_type), and consider them an indispensable tool in the modern programmers' toolbox. In ["Where Does the Name 'Algebraic Data Type' Come From?"](https://blog.poisson.chat/posts/2024-07-26-adt-history.html) Li-yao Xia investigates the possible sources of the name, going back to the programming languages from half a century ago.

- Follow Casey Rodarmor through the rabbithole to learn where an unexpected newline character comes from in this entertaining and [enlightening](https://dl.acm.org/doi/10.1145/358198.358210) article ["Whence '\\n'?"](https://casey.github.io/blog/whence-newline/).

- [Turnstyle](https://jaspervdj.be/posts/2024-08-21-turnstyle.html) is an esoteric, graphical functional language by Jasper Van der Jeugt. I have never seem anything like it before. It's truly mind-blowing and I'm still trying to understand how it works.

- As good programmers, we try to stay away from the dark corners of programming languages, but Justine Tunney takes a head-first dive into them and comes up with an enthralling tale in the article ["Weird Lexical Syntax"](https://justine.lol/lex/).

- I am not going to lie, I love Lisps! I must have implemented at least a dozen of them by now. If you are like me, you may have wondered ["Why Is It Easy to Implement a Lisp?"](https://eli.thegreenplace.net/2022/why-is-it-easy-to-implement-a-lisp/). Eli Bendersky puts forward a compelling argument.

- How better to implement a fast (and small) Lisp than to compile it to LLVM IR. Using Clojure this time, John Jacobsen showcases it in ["To The Metal... Compiling Your Own Language(s)"](http://johnj.com/posts/to-the-metal/).

- Phil Eaton takes an ingenious approach for ["Compiling Dynamic Programming Languages"](https://notes.eatonphil.com/compiling-dynamic-programming-languages.html), one that has never occurred to me before, but now will be a part of my toolbox forever.

- Here's another technique that I was only vaguely familiar with: JIT compilation using macros. In ["Runtime Optimization with Eval"](https://cuddly-octo-palm-tree.com/posts/2021-02-14-eval-opti/) Gary Verhaegen demonstrates this technique using Clojure.

- When compiling dynamically typed programming languages, we need to tag pointers to data with the runtime type information. In ["What Is the Best Pointer Tagging Method?"](https://coredumped.dev/2024/09/09/what-is-the-best-pointer-tagging-method/) Troy Hinckley describes some good ways of doing the same.

- I relish Max Bernstein's articles about programming language implementation techniques. In ["What’s in an e-graph?"](https://bernsteinbear.com/blog/whats-in-an-egraph/) they describe an optimization technique using e-graphs used in compilers.

- I love atypical uses of Programming Language Theory. Adam Dueck explains their PLT adventure in ["How I Learned Pashto Grammar Through Programming Syntax Trees"](https://adueck.github.io/blog/how-i-learned-pashto-grammar-through-programming-syntax-trees/).

- Brainfuck, the most popular of esoteric programming languages, has been a lot on my mind recently. And who better to learn about compiling BF from than Wilfred Hughes. In ["An Optimising BF Compiler"](https://www.wilfred.me.uk/blog/2015/08/29/an-optimising-bf-compiler/) they go over the algorithms they used to write ["An Industrial-Grade Brainfuck Compiler"](https://bfc.wilfred.me.uk/).

- And lastly, from the wicked mind of Srijan Paul, comes a twist: ["Compiling to Brainf#ck"](https://injuly.in/blog/bfinbf/index.html) about their programming language Meep that, you guessed right, compiles to BF.

Like, share, or comment on this post on [Mastodon](https://fantastic.earth/@abnv/113516066682728028){:class="mastodon-link"}.
