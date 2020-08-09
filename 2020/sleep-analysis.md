---
date: 2020-08-09
tags: personal health observations
---

# Sleep Analysis

Last summer I got a Garmin 245 smartwatch which tracks sleep and I've been wearing it to bed every night. It's been a year since so I decided to play around with the data captured till now. I downloaded my data from the Garmin Connect website by hitting the right API using [curl](https://en.wikipedia.org/wiki/CURL), did some analysis using [jq](https://stedolan.github.io/jq/) and [termgraph](https://github.com/mkaz/termgraph/) and made some charts using [Google Sheets](https://docs.google.com/spreadsheets/).

I like to sleep. I sleep quite a lot. So first chart is the histogram of my sleep duration over last one year:

![Sleep duration histogram](/files/sleep-analysis/duration.svg "Sleep duration histogram")
[Sleep duration histogram](/files/sleep-analysis/duration.svg)

As expected, it is a [Normal Distribution](https://en.wikipedia.org/wiki/Normal_distribution), with a peak around eight hours.

Next is the histogram of my sleep and wakeup times:

![Sleep/wakeup time histogram](/files/sleep-analysis/sleep-wakeup.svg "Sleep/wakeup time histogram")
[Sleep/wakeup time histogram](/files/sleep-analysis/sleep-wakeup.svg)

Again a Normal Distribution, indicating that I usually fall asleep around 10:30 PM and wake up around 7:00 AM.

Here's how my sleep of last one year looks like broken up by sleep stages:

![Sleep timeline](/files/sleep-analysis/timeline.svg "Sleep timeline")
[Sleep timeline](/files/sleep-analysis/timeline.svg)

You can see that I'm a pretty light sleeper. Most of my sleep is either light or [REM](https://en.wikipedia.org/wiki/Rapid_eye_movement_sleep) stages.

Looking at the distribution for sleep duration by stages, here's light and REM stages:

![Light/REM duration histogram](/files/sleep-analysis/light-rem.svg "Light/REM duration histogram")
[Light/REM duration histogram](/files/sleep-analysis/light-rem.svg)

And here's the histogram for deep and awake stage durations:

![Deep/awake duration histogram](/files/sleep-analysis/deep-awake.svg "Deep/awake duration histogram")
[Deep/awake duration histogram](/files/sleep-analysis/deep-awake.svg)

So, I usually have five hours of light sleep and three hours or REM sleep. My deep sleep duration is almost zero and I'm usually awake for 5–10 minutes.

It is interesting to get these insights into my sleep patterns. I hope to use it make my sleep even better. Until next time, happy napping!
