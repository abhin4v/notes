---
date: 2022-08-31
tags: nix self-hosting
---

# Setting up Fail2Ban with Nginx and Cloudflare on NixOS

[Fail2Ban](https://www.fail2ban.org) is a service to scan the log files of various services on servers
to find malicious IPs (by matching log lines to predefined regular expressions), and ban such IPs
using various firewalls like [iptables](https://www.netfilter.org/projects/iptables/index.html).
One of its use is to ban malicious IPs that issue bad requests on [NGINX](https://nginx.org/) servers. 

However, in my case, the NGINX server is fronted by [Cloudflare](https://www.cloudflare.com) (CF)
for resource caching etc. This means that the IPs that the NGINX server sees are that of the
CF proxies, not the real visitors. If we use Fail2Ban on them, we'll just end up banning the
CF IPs, thus breaking the websites proxied by CF.

So, we need a smarter setup than the run-of-the-mill one. Also, I automate my server setup and config
with [NixOS](https://nixos.org).

The first thing to do is to discern the requests coming through CF. CF lists its
[proxy IPs](https://www.cloudflare.com/ips) publicly. We consume the list and use it to
configure NGINX.

```nix
{ modulesPath, pkgs, lib, ... }:

let
  cloudflareIPs = builtins.fetchurl "https://www.cloudflare.com/ips-v4";
  setRealIpFromConfig =
    lib.concatMapStrings (ip: "set_real_ip_from ${ip};\n") 
      (lib.strings.splitString "\n" (builtins.readFile "${cloudflareIPs}"));
in {
  services.nginx = {
    enable = true;
    appendHttpConfig = ''
      ${setRealIpFromConfig}
      real_ip_header CF-Connecting-IP;
    '';
  };
}
```

<center><em>nginx.nix</em></center>

We also set the `real_ip_header` setting so that NGINX extracts the real visitor IPs from the
`CF-Connecting-IP` header that CF sends, and prints them in the access log instead of the
CF proxy IPs.

Fail2Ban now sees the real visitor IPs through the access log. However, banning those IPs in the
server does no good because they are being proxied through CF. We need to ban them at
CF as well.

CF comes with a tool called [WAF](https://www.cloudflare.com/waf/) for the same, and
it can be accessed via HTTP APIs. Even better, Fail2Ban already comes with a
[cloudflare action](https://github.com/fail2ban/fail2ban/blob/master/config/action.d/cloudflare.conf)
with the API calls builtin. All we need to do is to configure our Fail2Ban jails to use this action.

```nix
{ lib, config, pkgs, ... }:

let
  cfEmail = "cfadmin@example.net";
  cfApiKey = "xxxxxx";
in {
  services.fail2ban = {
    enable = true;
    extraPackages = [ pkgs.curl ];

    jails.DEFAULT = ''
      bantime  = 1d
      findtime = 1h
    '';

    jails.nginx-noagent = ''
      enabled  = true
      port     = http,https
      filter   = nginx-noagent
      backend  = auto
      maxretry = 1
      logpath  = %(nginx_access_log)s
      action   = cloudflare[cfuser="${cfEmail}", cftoken="${cfApiKey}"]
                 iptables-multiport[port="http,https"]
    '';

    environment.etc."fail2ban/filter.d/nginx-noagent.conf".text = ''
      [Definition]

      failregex = ^<HOST> -.*"-" "-"$

      ignoreregex =
    '';
  };
}
```

<center><em>fail2ban.nix</em></center>

The code above shows an example jail called `nginx-noagent` that bans all IPs requesting without an
[HTTP user-agent](https://developer.mozilla.org/en-US/docs/Web/HTTP/Headers/User-Agent). We use
the `cloudflare` action (with CF account's email and API key) to ban in the CF WAF, and 
the `iptables-multiport` action to ban in the server iptables. The `curl` package is needed to make
the CF API calls.

That's it! You can add more jails that use different regexes to catch different malicious actions,
and ban the bad IPs. Safe secure and serve (HTTP requests).
