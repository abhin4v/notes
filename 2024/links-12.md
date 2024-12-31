---
date: 2024-12-31
tags: links programming haskell
---

# Interesting Links for December 2024

A special *Haskell* edition of some interesting articles I recently read on the internet, starting
with some Haskell-in-practice articles:

- I like this new trend of making scripting dialects of full-fledged programming languages. Joining the
  likes of [Babashka](https://babashka.org/) and [Small Java](https://horstmann.com/unblog/2024-12-11/index.html)
  is [Hell, a Shell scripting Haskell dialect](https://chrisdone.com/posts/hell/) by Chris Done.
  I usually write my scripts in Python, but I sorely miss static typing and functional programming, so naturally, I'm excited for Hell.
  It is in a nascent stage now, but hopefully, it will grow into something nice and usable.

- Many think that Haskell is too complex for real-world projects. I really like this solid
  advice by Patrick Thomson for putting Haskell in real-world use:
  [Towards Faster Iteration in Industrial Haskell](https://blog.sumtypeofway.com/posts/fast-iteration-with-haskell.html),
  where he writes about Haskell editors and tooling, GHC extensions, type system, building, and deployment.

- Haskell is one of the few programming languages that are lazy by default, and often this is a source of
  a lot of headaches for programmers, causing space leaks, slow computation, or hard to debug stack traces. But
  sometimes laziness can be harnessed for writing better programs, like Jasper Van der Jeugt does
  to create an efficient and elegant layout algorithm for creating photo collages in
  [Lazy Layout](https://jaspervdj.be/posts/2023-07-22-lazy-layout.html).

- I love it when people use advanced programming techniques to solve their day-to-day problems! In
  [Planning Weekly Workouts in 100 Lines of Haskell](https://alt-romes.github.io/posts/2024-08-14-planning-a-workout-week-with-100-lines-of-haskell.html),
  Rodrigo Mesquita uses [Logic Programming](https://en.wikipedia.org/wiki/Logic_programming) in Haskell
  to create a custom workout plan for themselves.

- For some unknown reasons, functional programming and music seem to mesh well together. Maybe it is
  because both have combinatorial and compositional natures. In [Cheap Guitars and Drums in Haskell](https://blog.fmap.fr/posts/karplus-strong-sound-synthesis.html),
  Alp Mestanogullari uses Haskell to do digital audio synthesis by implementing the
  [Karplus-Strong algorithm](https://en.wikipedia.org/wiki/Karplus%E2%80%93Strong_string_synthesis).

- Another interesting use of Haskell to solve problems in clean and interesting way: Vehbi Sinan Tunalioglu
  uses the Diagrams library to generate dynamic [OpenGraph](https://ogp.me/) preview image for their website pages as detailed in
  [More Haskell Diagrams: Dynamic OpenGraph Images](https://thenegation.com/posts/haskell-diagrams-dynamic-og/).
  Currently I do this for my website in the crudest way possible: open the page in a browser manually
  and save a screenshot. Naturally, I end up doing it only once per page and the previews are not dynamic.
  I plan to switch to using Vehbi's technique in future.

Moving on to some Haskell-for-fun articles:

- Water Sort is a puzzle game where you have to sort coloured water into bottles. In [Water Sort in Haskell](https://nicaudinet.github.io/2024/10/14/watersort-haskell/)
  Nicolas Audinet de Pieuchon creates the game as a terminal UI in Haskell using [the Elm architecture](https://guide.elm-lang.org/architecture/).
  I like how Haskell and functional programming make writing and understanding such software easy by
  cleanly separating the game logic and the rendering logic.

- I've done [Avdent of Code](https://adventofcode.com) in Haskell for a couple of years now, and that has made me quite interested
  in efficient and convenient data structures in Haskell. Brent Yorgey has written about a bunch of them in
  their [Competitive Programming in Haskell](https://byorgey.github.io/blog/tag/challenge.html) series, and the latest article is about efficiently
  calculating a measure for sliding windows of lists: [Stacks, Queues, and Monoidal Sliding Windows](https://byorgey.github.io/blog/posts/2024/11/27/stacks-queues.html).

Next, some Haskell concepts articles:

- Haskell is notorious as a hard to learn language and I think some of that ill fame in deserved. Because
  of a powerful and flexible type-system, Haskell lets users solve their problems in many different ways,
  some of which could be too obtuse for many to understand.
  [Seven Levels of Type Safety in Haskell: Lists](https://blog.jle.im/entry/levels-of-type-safety-haskell-lists.html)
  by Justin Le shows seven different ways of working with lists in Haskell, only one of which is
  what we know as List (`[a]`) in usual Haskell. It's still fun to learn about these, and who knows,
  some of them may come handy some day.

- `fix` is one of those clever Haskell things that are very cool and terse but take a while to understand.
  In [`fix` by Example](https://gilmi.me/blog/post/2021/03/11/fix-by-example) Gil Mizrahi explains how it
  works by providing motivating examples. But to be honest, I took me quite some tries to **get** it even
  after reading a bunch about it.

- Arrows are a part of Haskell that I don't much understand, or use. Haskell even has [special syntax](https://www.haskell.org/arrows/syntax.html) for
  them. In [Why Arrows?](https://langdev.stackexchange.com/revisions/2372/2) [Alexis King](https://lexi-lambda.github.io/)
  explains why we need arrows at all. Instead of commenting on it, I'm gonna quote a part:
  > The key insight behind arrows comes from the following observation: it’s impossible to analyze the
  structure of a monadic function `a -> m b` without applying it because functions are opaque—the only
  thing we can do with one is apply it. Instead, we need to build our computation out of "function-like values"
  that are not opaque—we must be able to do more than just apply them.

- Here is an interesting use of a rather complicated concept from Category Theory: MangoIV shows in
  [Codensity for Resource Management](https://mangoiv.com/posts/2024-11-23-codensity.html) a way to
  use the [Codensity monad](https://en.wikipedia.org/wiki/Codensity_monad) for conveniently managing
  resources in Haskell code.

- If you have read any Haskell code, you'd know that Haskellers love writing terse code (myself included).
  That includes using a lot of single-letter variable names, which may make the code very unreadable for an
  unacquainted eye. But there is a method to this madness, and Jack Kelly captures a comprehensive knowledge
  about such names in [A Dictionary of Single-Letter Variable Names](http://jackkelly.name/blog/archives/2024/10/12/a_dictionary_of_single-letter_variable_names/index.html).

- Stephen Diehl writes a thorough tutorial about how to generate X86 assembly from scratch for [JIT compilation](https://en.wikipedia.org/wiki/Just-in-time_compilation),
  and how to make it easy by exploiting a monadic interface
  in [Monads to Machine Code](https://www.stephendiehl.com/posts/monads_machine_code/).

Finally, some Haskell philosophy to finish it off:

- Adam Dueck muses "[Why Does Everyone Hate Haskell, Jazz, and Pure Math?](https://adueck.github.io/blog/on-haskell-jazz-and-pure-math/)",
  and posits that exploring interesting things that may not be immediately useful may actually cause
  more practical advances in the long term.

That's it for this year. Have a happy and prosperous 2025!

Like, share, or comment on this post on [Mastodon](https://fantastic.earth/@abnv/113748806936137827){:class="mastodon-link"}.
