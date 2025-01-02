---
date: 2023-04-13
tags: personal observations software
---

# My Reading Process

Recently, I decided to streamline how I read articles on the Internet. I mostly read programming related content, and most of them happen to be blog posts. So I decided to use a feed reader to keep track of the content I want to read. I've been using Miniflux couple of months now, and I've come up a process that works for me. I thought I'd share it here.

![My reading process](/files/reading-process/process.svg "My reading process")
[My reading process](/files/reading-process/process.svg "My reading process")

At the center of the process is [Miniflux](https://miniflux.app/). Miniflux is a feed reader that runs on a server. Though it can be self-hosted, I pay for the hosted version. It has APIs that various client apps can use to read subscribed feeds, so I'm not bound to one particular feed reader app.

The process starts with me adding feeds to Miniflux for interesting blogs I find on the internet. I usually find these blogs through communities like [Mastodon](https://joinmastodon.org/), ~~[Twitter](https://twitter.com/)~~ [Bluesky](https://bsky.app/), or various [Slack](https://slack.com/) instances and [Telegram](https://telegram.org/) channels, through newsletters that syndicate blog articles, or through forums like [Lobste.rs](https://lobste.rs/), [Hacker News](https://news.ycombinator.com/), [Reddit](https://www.reddit.com/), or various [Discourse](https://www.discourse.org/) forums. Sometimes I also find them through link from other articles I read.

I add the feeds to Miniflux using the web interface, and then I read them through the feed reader apps. I use [Reeder 5](https://reederapp.com/) on my MacBook, [Unread](https://apps.apple.com/us/app/unread-an-rss-reader/id1363637349) on my iPad, ~~[FeedMe](https://play.google.com/store/apps/details?id=com.seazon.feedme)~~ [FocusReader](https://play.google.com/store/apps/details?id=allen.town.focus.reader) on my Android phone, and [Miniflux Reader](https://reader.miniflux.app/) on the web[^custom-miniflux]. All of these sync with Miniflux, so reading an article on one device marks it as read on all the other devices.

[^custom-miniflux]: I've [customized](https://notes.abhinavsarkar.net/2024/customizing-miniflux) Miniflux Reader with my own CSS and JS.

If there is no feed, then I save the article in my [Feedlynx](https://github.com/wezm/feedlynx/) server, which provides an RSS feed of saved articles that I subscribe to from Miniflux, and read in the readers mentioned above.

For long articles that I cannot read in one sitting, or for when I'd like to read them later offline, I save them to [Instapaper](https://www.instapaper.com/), either directly through its web interface, or through feed reader apps. Then I have Instapaper send them to my Kindle, so I can read them on the go.

If I find an article interesting enough to reread or reference later, I archive it in [Wallabag](https://wallabag.org/), either directly, or through feed reader apps. Wallabag give me a permanent archive of the articles, and I can annotate them with my own notes. I can also do a full-text search of all the articles I've archived. I self-host Wallabag on my VPS.

This streamlined process has helped me read more articles than before, and I've been able to keep up with the content I'm interested in. I hope this helps someone else too.

I post the most interesting of the articles I read as [collections of links](/tags/links) every month. You can download the OPML file of the feeds I subscribe to (at the time of writing this note) from [here](/files/reading-process/feeds.opml).

You can like, share, or comment on this note on [Mastodon](https://fantastic.earth/@abnv/110190621043044862){:class="mastodon-link"}.
