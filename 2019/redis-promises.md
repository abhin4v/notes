---
date: 2019-06-13
tags: programming distributed-systems
---

# Taming the Thundering Herd with Redis

[This article][1] inspired me to write this python pseudo-code to solve the thundering herd problem with [redis].

```python
__RETRY = "::RETRY::"

def get(key, expiry_ms, min_wait_ms, max_wait_ms):
  val = do_get(key, expiry_ms, min_wait_ms, max_wait_ms)
  while val == __RETRY:
    val = do_get(key, expiry_ms, min_wait_ms, max_wait_ms)
  return val

def do_get(key, expiry_ms, min_wait_ms, max_wait_ms):
  val = redis.GET(key)
  if val is not None:
    return val
  else:
    set = redis.SET("promise." + key, 1, "PX " + max_wait_ms, "NX")
    if set:
      val = backend.get(key)
      redis.SET(key, val, "PX " + expiry_ms)
      return val
    else:
      val = redis.GET(key)
      if val is None:
        sleep_ms = min_wait_ms + random.randint(0, int(min_wait_ms/10))
        time.sleep(sleep_ms/1000)
        return __RETRY
      else:
        return val

val = get("some_key", 300_000, 1_000, 10_000)
```

[1]: https://instagram-engineering.com/thundering-herds-promises-82191c8af57d
[redis]: https://redis.io/
