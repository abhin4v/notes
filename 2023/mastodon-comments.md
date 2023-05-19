---
date: 2023-05-19
tags: fediverse programming indieweb
---

# Adding Mastodon Comments to a Website

Two of the core ideas of [IndieWeb](https://indieweb.org/) are [POSSE](https://indieweb.org/POSSE) and [Backfeed](https://indieweb.org/backfeed). The idea behind POSSE is that you publish your content on your own website first, and then syndicate it to other platforms. Backfeed means that you pull back the comments and likes from the other platforms to your own website. In this post, I describe how I implemented these ideas for this website with [Mastodon](https://joinmastodon.org/).

Though it is possible to use services like [Bridgy](https://brid.gy/) to implement POSSE and Backfeed, I chose to do it myself. Syndicating is just posting the URL of a blog post to Mastodon, so I do it manually. That lets me write a custom message for the post, and also lets me choose the right image to attach. For Backfeed, I use the [Mastodon API](https://docs.joinmastodon.org/api/) to pull back the comments to this website.

At the end of each post, I add a link to the Mastodon post that links to the blog post, like:

```markdown
You can like, share, or comment on this post on
[Mastodon](https://fantastic.earth/@abnv/110362391871164484){:class="mastodon-link"}.
```

Then this small Javascipt code pulls back the comments from Mastodon, and displays them on the post:

```js
document.addEventListener("DOMContentLoaded", function() {
  let mastodonLink = document.querySelector("a.mastodon-link");
  if (mastodonLink != null) {
    let parts = mastodonLink.href.split("/");
    let postId = parts[parts.length - 1];
    let main = document.querySelector("main");
    fetch(`https://fantastic.earth/api/v1/statuses/${postId}/context`)
    .then(res => res.json())
    .then(res => {
      let comments = res.descendants.filter(c => c.visibility === "public");
      if (comments.length === 0)
        return;

      let commentContainer = document.createElement("div");
      commentContainer.className = "comments";

      let commentsHeader = document.createElement("h2");
      commentsHeader.innerHTML = "Comments";
      commentContainer.appendChild(commentsHeader);

      for (let comment of comments) {
        let html = `
<a class="account" href="${comment.account.url}">${comment.account.username}</a>
<a class="pubtime" href="${comment.url}">
  <time>${new Date(comment.created_at).toLocaleString()}</time>
</a>
<div class="content">${comment.content}</div>`;
        let commentElement = document.createElement("div");
        commentElement.innerHTML = html;
        commentElement.className = "comment";
        if (comment.in_reply_to_id !== postId) {
          commentElement.classList.add("reply");
        }
        commentContainer.appendChild(commentElement);
      }
      main.appendChild(commentContainer);
    });
  }
});
```

The Javascript code looks for the aforementioned link, and uses the Mastodon API to get the public comments on the linked Mastodon post. The comments are then added to the bottom of the blog post.

I also added some SCSS code to style the comments:

```scss
.comment {
  font-size: 0.95em;
  border: 1px dashed #787882;
  padding: 0.5rem;
  margin: 0.5rem 0;
  .content p:last-of-type {
    margin-bottom: 0;
  }
  .pubtime {
    float: right;
  }
  .content p {
    margin: 0.5rem auto;
  }
  &.reply {
    margin-left: 1rem;
  }
}
```

 The result looks like this:

 ![Screenshot of the comments](/files/mastodon-comments/comments.png)

That's it! Now go ahead and like, share, or comment on this post on [Mastodon](https://fantastic.earth/@abnv/110395414125998660){:class="mastodon-link"}.
