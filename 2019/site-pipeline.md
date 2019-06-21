---
date: 2019-06-21
tags: architecture CI
---

# CI pipeline of abhinavsarkar.net

Here's how I deploy my website [abhinavsarkar.net].

![CI pipeline of abhinavsarkar.net][image]
[CI pipeline of abhinavsarkar.net][image]

- [code.abhinavsarkar.net] runs on [Gitea].
- [notes.abhinavsarkar.net] runs on [Precis].
- hooks.abhinavsarkar.net runs on [adnanh/webhook].
- [abhinavsarkar.net] runs on [hastatic].

[abhinavsarkar.net]: https://abhinavsarkar.net
[image]: /files/site-pipeline/site.svg
[code.abhinavsarkar.net]: https://code.abhinavsarkar.net
[notes.abhinavsarkar.net]: https://notes.abhinavsarkar.net
[Gitea]: https://gitea.io/
[Precis]: https://abhin4v.github.io/precis/
[adnanh/webhook]: https://github.com/adnanh/webhook
[hastatic]: https://github.com/abhin4v/hastatic/