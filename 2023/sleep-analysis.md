---
date: 2023-04-27
tags: personal observations health
---

# Sleep Analysis

I posted [a note](/2020/sleep-analysis) on analysis of my sleep patterns in 2020. Last year I had a lot of changes in my life, so I wanted to do the analysis again this year to see how things have changed. I downloaded the sleep data from my [Garmin watch](https://www.garmin.com/en-US/p/628939) using [Garmin Connect](https://connect.garmin.com/), and used Google sheets to analyze the data and create the charts.

Let's get started. First is the histogram of sleep duration over the last year:

![Sleep duration histogram](/files/sleep-analysis-2/duration_histogram.png "Sleep duration histogram")
[Sleep duration histogram](/files/sleep-analysis-2/duration_histogram.png "Sleep duration histogram")

As before, my sleep duration is mostly between 7.5 hours and 8.25 hours. An interesting things to notice is to a smaller second peak at 9 hours. This is probably due to the fact that I have been sleeping in occasionally.

Next is the weekly average sleep duration over the last year:

![Weekly average sleep duration](/files/sleep-analysis-2/duration_average.png "Weekly average sleep duration")
[Weekly average sleep duration](/files/sleep-analysis-2/duration_average.png "Weekly average sleep duration")

A major life event happened in the third week of July 2022, which you can notice as a sudden drop in the chart. I had a baby! The sleep durations have been low since then, but it is slowly increasing.

The same thing reflects in the weekly sleep duration spread:

![Weekly sleep duration spread](/files/sleep-analysis-2/duration_spread.png "Weekly sleep duration spread")
[Weekly sleep duration spread](/files/sleep-analysis-2/duration__spread.png "Weekly sleep duration spread")

The spreads are much higher since July 2022. Some days I get less than 6 hours of sleep, and some days I get more than 9 hours of sleep.

I went in the details this time, and made a box plot of the sleep durations bucketed weekly:

![Weekly sleep duration box plot](/files/sleep-analysis-2/duration_box.png "Weekly sleep duration box plot")
[Weekly sleep duration box plot](/files/sleep-analysis-2/duration_box.png "Weekly sleep duration box plot")

Again, it is easy notice to how the sleep duration has reduced, and the spread has increased since July 2022. The week when my baby was born stands out clearly.

To zoom in a little more, here are the box plots of my sleep time (when I go to bed) and wake time (when I wake up) bucketed weekly:

![Weekly sleep time box plot](/files/sleep-analysis-2/sleep_time_box.png "Weekly sleep time box plot")
[Weekly sleep time box plot](/files/sleep-analysis-2/sleep_time_box.png "Weekly sleep time box plot")

![Weekly wake time box plot](/files/sleep-analysis-2/wake_time_box.png "Weekly wake time box plot")
[Weekly wake time box plot](/files/sleep-analysis-2/wake_time_box.png "Weekly wake time box plot")

I've been going to bed later and waking up later since July 2022. I am not sure if this is due to the baby, or due to the fact that I was on leave and then I've been mostly working from home and don't have to commute to work. I am guessing it is a combination of both. Also, notice the increased spread in both the charts.

Here is the same data but as a histogram for the entire year:

![Sleep and wake time histogram](/files/sleep-analysis-2/sleep_wake_time_histogram.png "Sleep and wake time histogram")

The histogram shows that my most usual sleep time is 11 pm, and most usual wake time is 8:30 am. Both of them have spreads of about 2.5 hours and 1.5 hours towards midnight and away from it respectively.

Finally, here is a chart to show my sleep debt over time:

![Sleep debt](/files/sleep-analysis-2/debt.png "Sleep debt")
[Sleep debt](/files/sleep-analysis-2/debt.png "Sleep debt")

I take 8 hours to be the ideal sleep duration. For calculating the sleep debt, I take the difference between the actual sleep duration and the ideal sleep duration, and add it to the sleep debt. Only the last 30 days of sleep are considered for calculating the sleep debt at any point in time. Note that the sleep debt is negative when the actual sleep duration is more than the ideal sleep duration.

As the chart shows, I started quickly accumulating sleep debt in July 2022, reaching a peak of 20 hours in mid August. It took about one month to come back to zero. Even after that I went and came out of sleep debt every few months (as the baby's sleep schedule changed), but the peaks are not as high as the first one.

That's it for this year. I may do the analysis again next year, and see how things have changed.

Like, share or comment on this post on [Mastodon](https://fantastic.earth/@abnv/110270757135255035){:class="mastodon-link"}.
