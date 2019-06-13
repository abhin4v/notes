---
date: 2019-06-13
tags: code distributed-systems caching
---

# Taming the Thundering Herd with Redis

[This article](https://instagram-engineering.com/thundering-herds-promises-82191c8af57d) inspired me to write this python pseudocode to solve the thundering herd problem with [redis](https://redis.io/):

```python
def get(key):
  val = redis.GET(key)
  if val is None:
    set = redis.SETNX("promise." + key,
            json.serialize({"poll_url": get_node_poll_url(key)}))
    if set:
      val = backend.get(key)
      redis.SETNX(key, val)
      redis.DEL("promise." + key)
      return val
    else:
      promise = json.deserialize(redis.GET("promise." + key))
      white True:
        resp = http.get(promise.poll_url)
        if resp.code == 409:
          time.sleep(1)
          continue
        else:
          return resp.data
  else:
    return val
    
def poll(key):
  val = redis.GET(key)
  if val is None:
    return http.resp(409)
  else:
    return http.resp(200, val)
```