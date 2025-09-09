---
date: 2025-09-09
tags: indieweb observations
---

# The Many Broken Feeds

RSS/Atom feeds are central to my [reading process](/2023/reading-process). I subscribe to over 700 feeds that I have curated over years. Most of them work fine most of the time, but a few are always broken in various different ways (different ones at different times). As a big proponent of [Indieweb](https://indieweb.org/) and [feeds](https://aboutfeeds.com/), I'm writing this note to categorize the ways feeds break, and offer some solutions.

Before we get on to the issues, the very first thing you must do is to subscribe to the feeds of your own websites, so that your feed reader notifies you of any breakages. Now, let's see our culprits.

## Expired SSL Certificates

This is the most frequent way I see feeds break. People forget to renew their websites' SSL certificates, causing them to expire. Most feed readers will stop fetching feeds for such websites. However, some provide options in their settings to ignore SSL expiration. The right way to deal with it is to set up reminders to manually renew certificates, or to set up scheduled jobs to renew them automatically. There is also an option to put your website behind a CDN front, such as Cloudflare or Github Pages, and let the CDN provider deal with renewals.

## Timeouts Caused by Slow Servers

Feed readers always use a timeout when fetching feeds because they cannot afford to get stuck on individual feeds forever. Usually this is between 30 to 60 seconds. If the server serving a website is slow enough to not respond within the timeout, the feed is marked as broken. Most of the time this is an intermittent error that resolves itself after a while when the load on the server goes down. But if it happens often, I'd recommend looking into your serving provider/infrastructure/software, and see if the serving latency or bandwidth can be improved by caching or by upgrading hardware or service tier.

## Misconfigured Firewalls

This issue is relatively new, and I suspect it's because people are reconfiguring their firewall rules to block website scrapping bots. The problem is, feeds are inherently meant to be read by bots (feed readers) on their users' behalf. If you are doing this for your website, please make sure to exclude feed URLs. Or if you don't want the bots to get your blog post content, please consider offering a free-to-all link-only feed with no content. Seeing my favourite feeds go down with "Access Forbidden" messages is heartbreaking.

## Servers Going Down

The server hosting a website may go down occasionally. Ideally people should set up some sort of monitoring and alerting to let them know when that happens. Some options are [Openstatus](https://openstatus.dev/) and [Digital Ocean](https://www.digitalocean.com/) uptime checks.

## Changed Feed URLs

Sometimes people rewrite their websites, or switch to a different blogging platform or site generator. This may cause their feed URLs to change. If you are making such a change, please pay attention because [cool URLs don't change](https://www.w3.org/Provider/Style/URI). You can either configure your system to provide feeds on same URLs, or redirect old URLs to new ones. If you can't do these, consider sticking a banner or post on your new website informing the readers of new feed URLs to subscribe.

## Feed Parsing Failures

This issue is infrequent but I've seen it happen more than a dozen times now. I suspect this happens when people hand-roll their feed generators, and do not take care of addressing all Unicode or XML encoding edge-cases. I suggest occasionally [validating your feeds](https://validator.w3.org/feed/) to catch such issues.

## Deleted Feeds

Sometimes people decide to not offer feeds anymore. Maybe they think that feeds are dying, or maybe they want to switch to newsletters. Let me assure you that [feeds](https://audmcname.com/comics/rss-is-not-dead-yet/) [are](https://chrisdone.com/posts/death-of-rss-greatly-exaggerated/) [not](https://sheep.horse/2024/3/rss_is_not_dead.html) [dead](https://bacardi55.io/2024/03/16/happy-25th-birthday-to-the-backbone-of-my-internet-rss-feeds/). Please reconsider.

## Deleted Websites

This one is straightforward but unfortunate. You decided to delete your website and its feed. That makes me sad, but you do you.

That's all I have on this topic. I hope this helps someone.

Like, share, or comment on this post on [Mastodon](https://fantastic.earth/@abnv/115173982539000772){:class="mastodon-link"}.
