---
date: 2019-06-13
tags: code distributed-systems caching
---

# Taming the Thundering Herd with Redis

[This article][1] inspired me to write this python pseudocode to solve the thundering herd problem with [redis].

```python
def get(key, expiry):
  val = redis.GET(key)
  if val is not None:
    return val
  else:
    set = redis.SET("promise." + key, 1, "EX " + expiry, "NX")
    if set:
      val = backend.get(key)
      redis.SET(key, val, "EX " + expiry)
      return val
    else:
      while True:
        val = redit.GET(key)
        if val is None:
          time.sleep(1)
          continue
        else:
          return val
```

[1]: https://instagram-engineering.com/thundering-herds-promises-82191c8af57d
[redis]: https://redis.io/