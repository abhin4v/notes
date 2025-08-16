---
date: 2025-08-16
tags: indieweb personal observations software
---

# How I Write on Internet

I wrote this post for a prompt given during our [local](https://underline.center/t/indiewebclub-8-with-ankur-and-tanvi/503) [Indieweb](https://indieweb.org/) club meeting. It is also a response to a [post](https://ankursethi.com/blog/writing-without-a-plan/) by my friend [Ankur](https://ankursethi.com/) on a simiar topic.

I’ve had [a website and blog](https://abhinavsarkar.net) for about eight years now. I started writing blog posts on a whim because I was bored at job. This was back in the summer of 2017. Over the last eight years, I have written 24 long-form posts and 96 short notes on my website. This note is a meta one; it looks into how these writings come about.

## The Inspiration

Most of my posts start in the same way. Inspiration strikes me seemingly at random. When I’m taking a walk, or sitting in a cab, or browsing the internet. An idea appears in my head and takes a hold. Mind you, this does not happen very often. Maybe once or twice a month. But if the idea sticks, it sticks.

Some ideas give me a stomach ache. I need to write about them as soon as possible. Thankfully most of such ideas lead to short notes like this one, so it’s easy to get rid of them soon.

Other idea are slow burn. It has taken me years to write about some of these, as you can notice from the long-running [posts series](https://abhinavsarkar.net/series/) I have that have taken many years to finish.

## The Process

The notes, I write without much care or research. They are usually about some small but interesting things I encounter, or about a personal event, or a memory. I get done with these in few hours, almost all of them in one sitting. These don’t require any specific process or tools. They just come to my mind and go to the internet via a simple text editor.

The long-form posts and post series, however, take careful planning, research, experimentation, writing and editing. Since most of these are programming related, I start with creating a prototype program. Then I rearrange the program so that it fits nicely into a narrative. I create outlines in form of post titles, headings, and bulleted lists of main points, and slowly expand them over time.

During the process, I end up doing a lot of research into the topics, and more often than not, I reveal new things about them. This leads to rewrites of both programs and the narratives, sometimes quite drastic ones. For example, for the [latest series of posts](https://abhinavsarkar.net/posts/arithmetic-bytecode-vm-parser/) I am writing, I have already changed the program 50 times since I started. This is not unexpected and I’m comfortable with this process. As they say, writing is thinking.

I also create a lot of [diagrams](https://abhinavsarkar.net/posts/implementing-co-4/#channel-operations) to visually explain complex topics, and also try to include useful examples, numbers and charts in my posts. For one post, I created [an interactive demo](https://abhinavsarkar.net/posts/repling-with-haskeline/#the-demo) as well. I write copious footnotes and provide references to relevant literature. All of these end up taking a lot of time, and it is not unusual that it takes me months to write a single long-form post. This may be off-putting to some, but I consider this worth my time.

## The Tools

These days, I write short posts in the [Bear](https://bear.app/) app. Then I export it as [Markdown](https://en.wikipedia.org/wiki/Markdown) and publish it on my [notes website](https://notes.abhinavsarkar.net) that uses [Github pages](https://docs.github.com/en/pages). That’s it!

For the long ones, I write them in Markdown in my code editor alongside the program I’m working on. These days it is [Zed](https://zed.dev/), but it used to be [VSCode](https://zed.dev/) in the past. I use [Monodraw](https://monodraw.helftone.com/) for making diagrams, and [Plotly](https://plotly.com/python/) for making charts. I use [Pandoc](https://pandoc.org/) to generate my static HTML website from the Markdown content, though I have many custom written plugins to enhance the produced HTML. I use [Nix](https://nixos.org) to orchestrate the building and deployment of my website. See [the colophon](https://abhinavsarkar.net/colophon/) of my website for more details.

That’s all for this post. Drop me a [comment on Mastodon](https://fantastic.earth/@abnv/115038474494713013){:class="mastodon-link"} about what your writing process looks like, or any question about mine.
