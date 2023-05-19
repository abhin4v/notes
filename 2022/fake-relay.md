---
date: 2022-12-15
tags: self-hosting programming fediverse
---

# Populating Small Mastodon Instances using FakeRelay

If you are a user on a small [Mastodon](https://joinmastodon.org) instance like [me](https://fantastic.earth/@abnv), you may have noticed that there is not much interesting going on there. This is because the way Mastodon works is, only those post are delivered to your instance that are posted or reposted by the users that the users on your instance follow across the network. Since your instance will have only a few users, it will only receive a few posts.

This makes for an uninteresting experience. However, there is a solution: Relays. Relays are special servers that are designed to forward posts from other instances to your instance. This way, your instance will receive posts from even those users that your users don't follow.

It turns out, most of the big relays are not accepting new instances, and the small ones do not have much going on because they are connected to only a handful of small instances. This is where FakeRelay come in.

[FakeRelay](https://github.com/g3rv4/FakeRelay) does not forward posts from other instances. Instead, you tell it which posts you want to forward. This way, you can populate your instance with interesting content of your own choice. But how do you get that?

Mastodon instances have a public API that you can use to get the public posts on that instance tagged with a certain hashtag. For example, you can get all the posts on the [fantastic.earth](https://fantastic.earth) instance tagged with `#nixos` using the following URL: <https://fantastic.earth/tags/nixos.json>. This will return a JSON file with links to recent posts tagged with `#nixos`. You can then tell FakeRelay to forward those posts to your instance. I have written a small Python script to do this:

```python
import concurrent.futures
import json
import os
import requests
import sys
import threading

def main(fakeRelayUrl, apiKey, instanceTagsFile, seenUrlsFile, seenUrlsCount):
  instanceTags = getInstanceTags(instanceTagsFile)
  endpointUrls = getEndpointUrls(instanceTags)
  seenUrls = getSeenUrls(seenUrlsFile)
  newSeenUrls = getAndIndexStatusUrls(fakeRelayUrl, endpointUrls, seenUrls)
  seenUrls.extend(newSeenUrls)
  saveSeenUrls(seenUrlsFile, seenUrls[-seenUrlsCount:])

def getInstanceTags(instanceTagsFile):
  with open(instanceTagsFile, 'r') as f:
      return json.load(f)

def getEndpointUrls(instanceTags):
  return ('https://{}/api/v1/timelines/tag/{}'.format(instance, tag)
          for instance in instanceTags for tag in instanceTags[instance])

def getSeenUrls(seenUrlsFile):
  if not os.path.isfile(seenUrlsFile):
    with open(seenUrlsFile, 'w') as f:
      f.write('[]')
    return []

  with open(seenUrlsFile, 'r') as f:
    return json.load(f)

def getAndIndexStatusUrls(fakeRelayUrl, endpointUrls, seenUrls):
  seenUrlsSet = set(seenUrls)
  newSeenUrls = []
  for statusUrl in getAllStatusUrls(endpointUrls):
    if statusUrl not in seenUrlsSet:
      if indexStatusUrl(fakeRelayUrl, statusUrl):
        print('Sent: {}'.format(statusUrl), flush=True)
        seenUrlsSet.add(statusUrl)
        newSeenUrls.append(statusUrl)
  print('Sent {} status URLs'.format(len(newSeenUrls)), flush=True)
  return newSeenUrls

def saveSeenUrls(seenUrlsFile, seenUrls):
  with open(seenUrlsFile, 'w') as f:
    json.dump(seenUrls, f)

def getAllStatusUrls(endpointUrls):
  with concurrent.futures.ThreadPoolExecutor(max_workers=50) as executor:
    statusUrlFutures = []
    for endpointUrl in endpointUrls:
      statusUrlFutures.append(executor.submit(getStatusUrls, endpointUrl))
    for future in concurrent.futures.as_completed(statusUrlFutures):
      for statusUrl in future.result():
        yield statusUrl

def indexStatusUrl(fakeRelayUrl, statusUrl):
  try:
    with getSession().post(fakeRelayUrl,
            timeout=60,
            data={'statusUrl': statusUrl},
            headers={'Authorization': 'Bearer ' + apiKey}) as r:
      if r.status_code != 200:
        print("Failed response: {}".format(r.status_code), flush=True)
        return False

      return True
  except Exception as e:
    print('Error: {}'.format(e), flush=True)
    return False

def getStatusUrls(endpointUrl):
  print('Requesting {}'.format(endpointUrl), flush=True)
  try:
    with getSession().get(endpointUrl, timeout=60) as r:
      if r.status_code != 200:
        print("Failed response: {}".format(r.status_code), flush=True)
        return []

      data = r.json()
      for status in data:
        url = status["uri"]
        if url.startswith('http') and 'statuses' in url:
          yield url
  except Exception as e:
    print('Error: {}'.format(e), flush=True)
    return []

threadLocal = threading.local()

def getSession():
    if not hasattr(threadLocal, "session"):
        threadLocal.session = requests.Session()
    return threadLocal.session

if __name__ == '__main__':
  apiKey = os.environ.get("FAKERELAY_API_KEY")
  fakeRelayUrl = sys.argv[1] + "/index"
  instanceTagsFile = sys.argv[2]
  seenUrlsFile = sys.argv[3]
  seenUrlsCount = int(sys.argv[4])

  main(fakeRelayUrl, apiKey, instanceTagsFile, seenUrlsFile, seenUrlsCount)
```

<center><em>relay_tag_statuses.py</em></center>

You can run it like this:

```bash
FAKERELAY_API_KEY=<API_KEY> python3 relay_tag_statuses.py \
  https://fakerelay.gervas.io instance-tags.json seen-posts.json 2000
```

The `instance-tags.json` file contains a list of instances, and the hashtags you want to forward from them. For example:

```json
{
  "hachyderm.io": ["nixos", "linux"],
  "mastodon.social": ["programming", "photography"]
}
```

The `seen-posts.json` file contains a list of posts that have already been forwarded. This is used to avoid forwarding the same messages multiple times. The script keeps the last 2000 posts in this file.

I'm using the [FakeRelay server](https://fakerelay.gervas.io) run by the creator of the project. You can also run your own server. You'll need to set the `FAKERELAY_API_KEY` environment variable to the API key to access the FakeRelay server.

Finally, you can set this script to run on a schedule using Cron or Systemd timers.

That's it! Fill your Mastodon instances with interesting content and enjoy.

Like, share and reply to this post on the [fediverse](https://fantastic.earth/@abnv/109518233198510914){:class="mastodon-link"}.
