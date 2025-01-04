---
date: 2025-01-04
tags: software
---

# Customizing Miniflux to Add Custom Sorting

I have been using [Miniflux](https://miniflux.app/) as my feed reader for a couple of years now. It is intentionally minimalistic, and hence it lacks certain features. One thing that I sorely miss is the capability to sort the feed entries by various criteria like entry reading time etc. Thankfully, Miniflux lets users customize the UI by injecting custom JavaScript code via settings. So I took this in my own hands:

![Screenshot of the Miniflux reader showing my custom sorting options](https://fantastic.earth/system/media_attachments/files/113/769/651/212/783/903/original/dfe5435c5346c89d.png "Screenshot of the customized Miniflux reader web app showing a dropdown of sorting options")
[Screenshot of the Miniflux reader showing my custom sorting options](https://fantastic.earth/system/media_attachments/files/113/769/651/212/783/903/original/dfe5435c5346c89d.png)

Here is what the different sorting options mean:

Default
: No custom sorting applied. Default to the sorting order specified in the Miniflux settings.

Random
: Sort randomly. Changes on every refresh.

URL
: Sort alphabetically by entry URL. Useful for finding duplicate entries.

Oldest
: Sort by entry published date, oldest first.

Newest
: Sort by entry published date, newest first.

Longest
: Sort by entry estimated reading time, longest first. Entries with unknown reading time sorted at the end.

Shortest
: Sort by entry estimated reading time, shortest first. Entries with unknown reading time sorted at the end.

Fewest
: Grouped by feed name, groups sorted by number of entries in the feed, fewest first. Entries sorted by the default sorting order.

Most
: Grouped by feed name, groups sorted by number of entries in the feed, most first. Entries sorted by the default sorting order.

Feed
: Grouped and sorted by feed name, entries sorted by the default sorting order.

Category
: Grouped and sorted by category name then feed name, entries sorted by the default sorting order.

The sorting options do not appear on the History page. The options to sort by feed and category do
not appear on feed specific pages. The options to sort by category does
not appear on category specific pages.

Additionally, the option selected for any page is remembered in the browser's local storage so
when you go back to a page, you find it sorted by same option that you had chosen the last time.

<details markdown="1">
<summary>Here is the custom JavaScript:</summary>

```js
(function () {
  "use strict";

  function entryFeed(e) {
    return e.querySelector(".item-meta-info-title a").getAttribute("title");
  }

  function entryCategory(e) {
    return e.querySelector(".category").innerText;
  }

  function entryTime(e) {
    return e.querySelector(".item-meta-info-timestamp time").dateTime;
  }

  function entryUrl(e) {
    return e
      .querySelector(".item-meta-icons-external-url a")
      .href.split("://")[1];
  }

  function entryReadingTime(e) {
    const ertE = e.querySelector(".item-meta-info-reading-time");
    return ertE == null ? null : parseInt(ertE.innerText);
  }

  function sortEntriesBy(entries, f, comparator) {
    return entries
      .map((e) => [f(e), e])
      .sort(comparator)
      .map(([_, e]) => e);
  }

  function sortEntries(entries, sortMode) {
    if (entries.length < 2) {
      return entries;
    }

    switch (sortMode) {
      case "random":
        return sortEntriesBy(entries, (_) => Math.random());

      case "url":
        return sortEntriesBy(entries, entryUrl);

      case "fewest":
        return Array.from(Map.groupBy(entries, entryFeed).values())
          .sort((a, b) => a.length > b.length)
          .flat();
      case "most":
        return Array.from(Map.groupBy(entries, entryFeed).values())
          .sort((a, b) => a.length < b.length)
          .flat();

      case "oldest":
        return sortEntriesBy(entries, entryTime);
      case "newest":
        return sortEntriesBy(entries, entryTime).reverse();

      case "shortest":
        return sortEntriesBy(entries, entryReadingTime, ([a], [b]) =>
          a == null ? 1 : b == null ? -1 : a - b,
        );
      case "longest":
        return sortEntriesBy(entries, entryReadingTime, ([a], [b]) =>
          a == null ? 1 : b == null ? -1 : b - a,
        );

      case "feed":
        return sortEntriesBy(entries, entryFeed);
      case "category":
        return sortEntriesBy(
          entries,
          (e) => entryCategory(e) + "-" + entryFeed(e),
        );

      default:
        return entries;
    }
  }

  function setupSortModeSelector() {
    const entries = Array.from(
      document.querySelectorAll(
        ".items .entry-item:not(:has(.item-title a[href^='/history']))",
      ),
    );
    if (entries.length < 2) {
      return;
    }

    const path = window.location.pathname;
    const lsKey = path + "-sortmode";
    const sortMode = localStorage.getItem(lsKey);
    const ph = document.querySelector(".page-header");
    const sortOpts = document.createElement("select");
    sortOpts.id = "sortmode";

    const options = [
      "Default",
      "Random",
      "URL",
      "Oldest",
      "Newest",
      "Longest",
      "Shortest",
    ];
    if (path.startsWith("/unread")) {
      options.push("Fewest", "Most", "Feed", "Category");
    } else if (path.startsWith("/category")) {
      options.push("Fewest", "Most", "Feed");
    }

    for (const opt of options) {
      const optE = document.createElement("option");
      optE.text = opt;
      optE.value = opt.toLowerCase();
      if (optE.value === sortMode) {
        optE.selected = true;
      }
      sortOpts.appendChild(optE);
    }
    ph.insertBefore(sortOpts, ph.querySelector("#page-header-title"));
    sortOpts.onchange = function (event) {
      const sortMode = event.target.value;
      sortEntries(entries, sortMode).forEach((e, i) => (e.style.order = i));
      localStorage.setItem(lsKey, sortMode);
    };

    if (sortMode != null) {
      sortEntries(entries, sortMode).forEach((e, i) => (e.style.order = i));
    }
  }

  document.addEventListener("DOMContentLoaded", function () {
    setupSortModeSelector();
  });
})();
```

</details>

That's it! This works very well for me, and I hope this helps someone else too.

Like, share, or comment on this post on [Mastodon](https://fantastic.earth/@abnv/113769651292830575){:class="mastodon-link"}.
