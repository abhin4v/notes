---
date: 2024-06-16
tags: software
---

# Customizing Miniflux to Be a Two-Pane Feed Reader

I have been using [Reeder](https://reederapp.com/) on my laptop as my feed reader app backed by [Miniflux](https://miniflux.app/) for the last couple of years. Recently, I found myself often clicking away from Reeder to the original blog posts in the browser.

I realised that most of the blogs I follow do not need sanitisation as done by feed readers because they are usually well-designed and do not contain ads. In fact, some of them are great looking, and have their own unique looks. I noticed that I prefer reading them in the browser, in their full glory, with their different styles, instead of all of them looking same black-and-white minimal-styled text in the reading pane of Reeder.

So, I went out looking for a feed reader that manages subscriptions like usual, but instead of having its own reading pane, opens the articles in a side-by-side browser pane. And I found **none**. Not a single feed reader I could find with links displayed in a sidebar, and original posts in a browser pane.

So I hacked my own using some custom CSS and JS added to the Miniflux UI:

![Screenshot of my custom Miniflux based two-pane feed reader](https://fantastic.earth/system/media_attachments/files/112/332/268/041/648/969/original/756962eb32d4fa84.png "Screenshot of a hacked together feed reader comprised of two side-by-side browser windows, with links displayed in Miniflux in the browser window on the left side, and the article open in another browser window on the right side.")
[Screenshot of my custom Miniflux based two-pane feed reader](https://fantastic.earth/system/media_attachments/files/112/332/268/041/648/969/original/756962eb32d4fa84.png)

Now, when I click on a link in Miniflux, the link opens in another window that I can place by the side of the Miniflux window. Clicking on any other link in Miniflux also opens its website in the same window so a new window is not opened everytime[^1]. Opening a link also marks it as read in Miniflux.

[^1]: This works most of the time, but breaks for some websites that override their window name upon opening.

Sometime I want to read certain entries inside the Miniflux reader instead. Clicking on a link with the <kbd>Command</kbd> button pressed opens the link inside the Miniflux reader in the same reader window as before[^2].

[^2]: Unfortunately, I was not able to figure out how to make this work with Miniflux's keyboard navigation.

This also allows me to customize each website using various Firefox extensions. I can remove various parts of pages if required using [uBlock Origin](https://github.com/gorhill/uBlock), I can turn pages dark using [Dark reader](https://darkreader.org/), or restyle them using [Stylus](https://add0n.com/stylus.html), or even change their functionality using [Tampermonkey](https://www.tampermonkey.net/) scripts. All of this is great!

I've had this setup for three months now, and I'm loving it. I have stopped using Reeder altogether!

<details markdown="1">
<summary>Here is the custom CSS I set in my Miniflux settings:</summary>

```css
body {
  max-width: 55em;
}
.item {
  border: none;
}
.item-title {
  font-size: 1.2rem;
}
.item-meta {
  display: flex;
  justify-content: space-between;
}
.item-meta a {
  color: #AAA;
}
.item-meta-icons li > :is(a, button) {
  color: #AAA;
}
.item-meta-icons :is(a:is(:focus, :hover), button:is(:focus, :hover)) {
  color: lightsalmon;
}
.item-meta-info :is(a:is(:focus, :hover), button:is(:focus, :hover)) {
  color: lightblue;
}
.item-meta-icons li {
  margin-top: 0;
}
.item-meta .item-meta-icons-comments {
  display: none;
}
.item-meta-icons .item-meta-icons-external-url {
  margin-right: 0;
  display: none;
}
.item-meta-info {
  font-size: inherit;
}
.item-header .category {
  position: relative;
  bottom: 0.2em;
}
.item-header h2 a img {
  background-color: #eee;
}
.entry header h1, .entry header .entry-actions, .entry header .entry-meta {
  margin-bottom: 10px;
}
.entry-content pre {
  padding: 5px 10px;
}
.entry-content code {
  font-size: 0.95em;
}
.entry-actions :is(.page-button, .page-link) {
  &:is(:hover, :focus) {
    color: lightsalmon;
  }
}
.header {
  position: sticky;
  top: 0;
  background-color: var(--body-background);
  z-index: 2;
  padding-bottom: 0.5rem;
}
.page-header, section.entry {
  position: sticky;
  top: calc(0.9rem + 10px);
  background-color: var(--body-background);
  z-index: 2;
  padding-bottom: 0.5rem;
}
.header li a:hover, .entry header h1 a:hover, .page-header h1 a:hover {
  color: var(--link-hover-color);
}
:root {
--entry-content-color: #BABABA;
--body-background: #141414;
--entry-content-code-color: #dfdfdf;
--entry-content-code-background: #222;
--entry-content-code-border-color: transparent;
--category-link-hover-color: lightblue;
--category-has-unread-background-color: unset;
--category-has-unread-border-style: unset;
--category-has-unread-border-color: unset;
--feed-parsing-error-border-style: unset;
--feed-parsing-error-border-color: unset;
--feed-has-unread-background-color: unset;
--feed-has-unread-border-style: unset;
--feed-has-unread-border-color: unset;
}
```

</details>

Along with other small style tweaks as per my taste, this positions actions buttons for entries on the right, hides links for entry comments and external URLs, and makes the page headers sticky so that they stay visible even when you scroll down.

<details markdown="1">
<summary>Here is the custom JavaScript I set in Miniflux for changing its default behaviour:</summary>

```js
(function() {
  'use strict';

  function openArticle(article, openInMinifluxReader) {
    let articleURL = openInMinifluxReader
      ? article.querySelector(".item-header h2 a").href
      : article.querySelector(".item-meta-icons-external-url a").href;
    if (window.open(articleURL, "miniflux-viewer")) {
      article
        .querySelector(".item-meta-icons-read button[data-value=unread]")
        .click();
    }
  }

  document.addEventListener("DOMContentLoaded", () => {
    document.querySelectorAll(".entry-item .item-title a").forEach(a => {
      a.addEventListener("click", function(ev) {
        ev.preventDefault();
        let article = a.parentElement.parentElement.parentElement;
        openArticle(article, ev.metaKey);
      });
    });
  });

})();
```

</details>

That's it! This works very well for me, and I hope this helps someone else too.

Like, share, or comment on this post on [Mastodon](https://fantastic.earth/@abnv/112625573297103844){:class="mastodon-link"}.
