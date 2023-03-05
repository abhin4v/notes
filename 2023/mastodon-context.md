---
date: 2023-03-05
tags: fediverse programming
---

# Pulling missing context of replied toots in Mastodon

Mastodon is a decentralized social media platform that's become increasingly popular in recent years. Unlike centralized social media platforms like Twitter and Facebook, Mastodon is run on separate individual servers, each with its own rules and community.

Mastodon servers are connected to each other via the [ActivityPub](https://activitypub.rocks/) protocol, which allows users to interact across servers. This means that users on one server can follow users on other servers and reply to their toots (Mastodon posts).

However, Mastodon does not automatically pull the context toots of the toots that users replied to from their original server. This means if you reply to a toot from another server, the all other replies to that toot may not be shown to you. See the discussion about this issue [here](https://github.com/mastodon/mastodon/discussions/22608).

This is a problem because it means that you may not be able to see the full context of a conversation, which can make it difficult to understand what's going on.

The following script is a Python program that extracts the context toots of the toots that users replied to from their original server, and adds them to the local server. The script accomplishes this by first getting all local users that have posted a toot in the last `x` hours, where `x` is a configurable parameter. It then retrieves all replies to other users by those users in the last `x` hours. From there, it gets the context toots of those replies that are already present on the local server.

Then, for each reply toot, it finds the server and ID of the toot that it replied to. It then gets the context toots of those toots from their original server. Finally, it adds those context toots to the local server, which are not already present on the local server by removing the already present context toots from the list of toots to add.

It also caches the URLs of all toots that it has already added to the local server, so that it doesn't add the same toot multiple times on subsequent runs.

You can run it like this:

```bash
ACCESS_TOKEN=XXXX python3 pull_context.py fantastic.earth 24
```

The access token needs to have the following scopes enabled:

- _admin:read:accounts_ for getting the user IDs of the users.
- _read:statuses_ for getting the replies to other users.
- _read:search_ for adding the context toots to the local server.

The script should be run periodically, for example, every 15 minutes. Its only dependency is the [`requests`](https://github.com/psf/requests/) library.

<details markdown="1">
<summary>Click to see the script</summary>

```python
#!/usr/bin/env python3

from datetime import datetime, timedelta
import itertools
import json
import os
import re
import sys
import requests


def pull_context(
    server,
    access_token,
    seen_urls,
    replied_toot_server_ids,
    reply_interval_hours,
):
    """pull the context toots of toots user replied to, from their
    original server, and add them to the local server."""
    user_ids = get_active_user_ids(server, access_token, reply_interval_hours)
    reply_toots = get_all_reply_toots(
        server, user_ids, access_token, seen_urls, reply_interval_hours
    )
    known_context_urls = get_all_known_context_urls(server, reply_toots)
    seen_urls.update(known_context_urls)
    replied_toot_ids = get_all_replied_toot_server_ids(
        server, reply_toots, replied_toot_server_ids
    )
    context_urls = get_all_context_urls(server, replied_toot_ids)
    add_context_urls(server, access_token, context_urls, seen_urls)


def get_active_user_ids(server, access_token, reply_interval_hours):
    """get all user IDs on the server that have posted a toot in the given
       time interval"""
    since = datetime.now() - timedelta(days=reply_interval_hours / 24 + 1)
    url = f"https://{server}/api/v1/admin/accounts"
    resp = requests.get(
        url, headers={"Authorization": f"Bearer {access_token}"}, timeout=5
    )
    if resp.status_code == 200:
        for user in resp.json():
            last_status_at = user["account"]["last_status_at"]
            if last_status_at is not None:
                last_active = datetime.strptime(last_status_at, "%Y-%m-%d")
                if last_active > since:
                    print(f"Found active user: {user['username']}")
                    yield user["id"]
    elif resp.status_code == 403:
        raise Exception(
            f"Error getting user IDs on server {server}. Status code: {resp.status_code}. "
            "Make sure you have the admin:read:accounts scope enabled for your access token."
        )
    else:
        raise Exception(
            f"Error getting user IDs on server {server}. Status code: {resp.status_code}"
        )


def get_all_reply_toots(
    server, user_ids, access_token, seen_urls, reply_interval_hours
):
    """get all replies to other users by the given users in the last day"""
    replies_since = datetime.now() - timedelta(hours=reply_interval_hours)
    reply_toots = list(
        itertools.chain.from_iterable(
            get_reply_toots(
                user_id, server, access_token, seen_urls, replies_since
            )
            for user_id in user_ids
        )
    )
    print(f"Found {len(reply_toots)} reply toots")
    return reply_toots


def get_reply_toots(user_id, server, access_token, seen_urls, reply_since):
    """get replies by the user to other users since the given date"""
    url = f"https://{server}/api/v1/accounts/{user_id}/statuses?exclude_replies=false&limit=40"

    try:
        resp = requests.get(
            url, headers={"Authorization": f"Bearer {access_token}"}, timeout=5
        )
    except Exception as ex:
        print(
            f"Error getting replies for user {user_id} on server {server}: {ex}"
        )
        return []

    if resp.status_code == 200:
        toots = [
            toot
            for toot in resp.json()
            if toot["in_reply_to_id"] is not None
            and toot["url"] not in seen_urls
            and datetime.strptime(toot["created_at"], "%Y-%m-%dT%H:%M:%S.%fZ")
            > reply_since
        ]
        for toot in toots:
            print(f"Found reply toot: {toot['url']}")
        return toots
    elif resp.status_code == 403:
        raise Exception(
            f"Error getting replies for user {user_id} on server {server}. Status code: {resp.status_code}. "
            "Make sure you have the read:statuses scope enabled for your access token."
        )

    raise Exception(
        f"Error getting replies for user {user_id} on server {server}. Status code: {resp.status_code}"
    )


def get_all_known_context_urls(server, reply_toots):
    """get the context toots of the given toots from their original server"""
    known_context_urls = set(
        filter(
            lambda url: not url.startswith(f"https://{server}/"),
            itertools.chain.from_iterable(
                get_toot_context(*parse_mastodon_url(toot["url"]), toot["url"])
                for toot in reply_toots
            ),
        )
    )
    print(f"Found {len(known_context_urls)} known context toots")
    return known_context_urls


def get_all_replied_toot_server_ids(
    server, reply_toots, replied_toot_server_ids
):
    """get the server and ID of the toots the given toots replied to"""
    return filter(
        lambda x: x is not None,
        (
            get_replied_toot_server_id(server, toot, replied_toot_server_ids)
            for toot in reply_toots
        ),
    )


def get_replied_toot_server_id(server, toot, replied_toot_server_ids):
    """get the server and ID of the toot the given toot replied to"""
    in_reply_to_id = toot["in_reply_to_id"]
    in_reply_to_account_id = toot["in_reply_to_account_id"]
    mentions = [
        mention
        for mention in toot["mentions"]
        if mention["id"] == in_reply_to_account_id
    ]
    if len(mentions) == 0:
        return None

    mention = mentions[0]

    o_url = f"https://{server}/@{mention['acct']}/{in_reply_to_id}"
    if o_url in replied_toot_server_ids:
        return replied_toot_server_ids[o_url]

    url = get_redirect_url(o_url)

    if url is None:
        return None

    match = parse_mastodon_url(url)
    if match is not None:
        replied_toot_server_ids[o_url] = (url, match)
        return (url, match)

    match = parse_pleroma_url(url)
    if match is not None:
        replied_toot_server_ids[o_url] = (url, match)
        return (url, match)

    print(f"Error parsing toot URL {url}")
    replied_toot_server_ids[o_url] = None
    return None


def parse_mastodon_url(url):
    """parse a Mastodon URL and return the server and ID"""
    match = re.match(
        r"https://(?P<server>.*)/@(?P<username>.*)/(?P<toot_id>.*)", url
    )
    if match is not None:
        return (match.group("server"), match.group("toot_id"))
    return None


def parse_pleroma_url(url):
    """parse a Pleroma URL and return the server and ID"""
    match = re.match(r"https://(?P<server>.*)/objects/(?P<toot_id>.*)", url)
    if match is not None:
        server = match.group("server")
        url = get_redirect_url(url)
        match = re.match(r"/notice/(?P<toot_id>.*)", url)
        if match is not None:
            return (server, match.group("toot_id"))
        return None
    return None


def get_redirect_url(url):
    """get the URL given URL redirects to"""
    try:
        resp = requests.head(url, allow_redirects=False, timeout=5)
    except Exception as ex:
        print(f"Error getting redirect URL for URL {url}. Exception: {ex}")
        return None

    if resp.status_code == 200:
        return None
    elif resp.status_code == 302:
        redirect_url = resp.headers["Location"]
        print(f"Discovered redirect for URL {url}")
        return redirect_url
    else:
        print(
            f"Error getting redirect URL for URL {url}. Status code: {resp.status_code}"
        )
        return None


def get_all_context_urls(server, replied_toot_ids):
    """get the URLs of the context toots of the given toots"""
    return filter(
        lambda url: not url.startswith(f"https://{server}/"),
        itertools.chain.from_iterable(
            get_toot_context(server, toot_id, url)
            for (url, (server, toot_id)) in replied_toot_ids
        ),
    )


def get_toot_context(server, toot_id, toot_url):
    """get the URLs of the context toots of the given toot"""
    url = f"https://{server}/api/v1/statuses/{toot_id}/context"
    try:
        resp = requests.get(url, timeout=5)
    except Exception as ex:
        print(f"Error getting context for toot {toot_url}. Exception: {ex}")
        return []

    if resp.status_code == 200:
        res = resp.json()
        print(f"Got context for toot {toot_url}")
        return (toot["url"] for toot in (res["ancestors"] + res["descendants"]))

    print(
        f"Error getting context for toot {toot_url}. Status code: {resp.status_code}"
    )
    return []


def add_context_urls(server, access_token, context_urls, seen_urls):
    """add the given toot URLs to the server"""
    count = 0
    for url in context_urls:
        if url not in seen_urls:
            if add_context_url(url, server, access_token):
              seen_urls.add(url)
              count += 1

    print(f"Added {count} new context toots")


def add_context_url(url, server, access_token):
    """add the given toot URL to the server"""
    search_url = f"https://{server}/api/v2/search?q={url}&resolve=true&limit=1"

    try:
        resp = requests.get(
            search_url,
            headers={"Authorization": f"Bearer {access_token}"},
            timeout=5,
        )
    except Exception as ex:
        print(
            f"Error adding url {search_url} to server {server}. Exception: {ex}"
        )
        return False

    if resp.status_code == 200:
        print(f"Added context url {url}")
        return True
    elif resp.status_code == 403:
        print(
            f"Error adding url {search_url} to server {server}. Status code: {resp.status_code}. "
            "Make sure you have the read:search scope enabled for your access token."
        )
    else:
        print(
            f"Error adding url {search_url} to server {server}. Status code: {resp.status_code}"
        )
    return False


class OrderedSet:
    """An ordered set implementation over a dict"""

    def __init__(self, iterable):
        self._dict = {}
        for item in iterable:
            self.add(item)

    def add(self, item):
        if item not in self._dict:
            self._dict[item] = None

    def update(self, iterable):
        for item in iterable:
            self.add(item)

    def __contains__(self, item):
        return item in self._dict

    def __iter__(self):
        return iter(self._dict)

    def __len__(self):
        return len(self._dict)


if __name__ == "__main__":
    HELP_MESSAGE = """
Usage: ACCESS_TOKEN=XXXX python3 pull_context.py <server> <reply_interval_in_hours>

To run this script, set the ACCESS_TOKEN environment variable to your
Mastodon access token. The access token can be generated at
https://<server>/settings/applications, and must have read:search,
read:statuses and admin:read:accounts scopes.
"""

    try:
        ACCESS_TOKEN = os.environ["ACCESS_TOKEN"]
    except KeyError:
        print("ACCESS_TOKEN environment variable not set.")
        print(HELP_MESSAGE)
        sys.exit(1)

    if len(sys.argv) < 3:
        print(HELP_MESSAGE)
        sys.exit(1)

    SERVER = sys.argv[1]
    REPLY_INTERVAL_IN_HOURS = int(sys.argv[2])
    SEEN_URLS_FILE = "seen_urls"
    REPLIED_TOOT_SERVER_IDS_FILE = "replied_toot_server_ids"

    SEEN_URLS = OrderedSet([])
    if os.path.exists(SEEN_URLS_FILE):
        with open(SEEN_URLS_FILE, "r", encoding="utf-8") as f:
            SEEN_URLS = OrderedSet(f.read().splitlines())

    REPLIED_TOOT_SERVER_IDS = {}
    if os.path.exists(REPLIED_TOOT_SERVER_IDS_FILE):
        with open(REPLIED_TOOT_SERVER_IDS_FILE, "r", encoding="utf-8") as f:
            REPLIED_TOOT_SERVER_IDS = json.load(f)

    pull_context(
        SERVER,
        ACCESS_TOKEN,
        SEEN_URLS,
        REPLIED_TOOT_SERVER_IDS,
        REPLY_INTERVAL_IN_HOURS,
    )

    with open(SEEN_URLS_FILE, "w", encoding="utf-8") as f:
        f.write("\n".join(list(SEEN_URLS)[:10000]))

    with open(REPLIED_TOOT_SERVER_IDS_FILE, "w", encoding="utf-8") as f:
        json.dump(dict(list(REPLIED_TOOT_SERVER_IDS.items())[:10000]), f)
```

</details>

I hope this is useful to someone else. I'm not a Python expert, so I'm sure there are some things that could be improved. I'm also not sure if this is the best way to do this, but it seems to work well enough for me.

You can like, repost, or comment on this post on [Mastodon](https://fantastic.earth/@abnv/109971076654557910).
