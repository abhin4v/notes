---
date: 2020-07-05
tags: health programming
---

# COVID-19 Charts for India on Terminal

[covid19india.org](https://www.covid19india.org) makes its data available via an API at [api.covid19india.org](https://api.covid19india.org/). One of the endpoints provides the district-wise daily data for all of India. Since I could not find any district-wise COVID-19 dashboard on internet, I decided to draw some charts on my own.

First fetch the district-wise data and save the JSON file:

```
> curl https://api.covid19india.org/districts_daily.json -o districts_daily.json
```

Now, we can list the states using [jq](https://stedolan.github.io/jq/):

```
> cat districts_daily.json | jq '.districtsDaily | keys'
[
  "Andaman and Nicobar Islands",
  "Andhra Pradesh",
  "Arunachal Pradesh",
  "Assam",
  "Bihar",
  "Chandigarh",
  "Chhattisgarh",
  "Dadra and Nagar Haveli",
  "Dadra and Nagar Haveli and Daman and Diu",
  "Delhi",
  "Goa",
  "Gujarat",
  "Haryana",
  "Himachal Pradesh",
  "Jammu and Kashmir",
  "Jharkhand",
  "Karnataka",
  "Kerala",
  "Ladakh",
  "Lakshadweep",
  "Madhya Pradesh",
  "Maharashtra",
  "Manipur",
  "Meghalaya",
  "Mizoram",
  "Nagaland",
  "Odisha",
  "Puducherry",
  "Punjab",
  "Rajasthan",
  "Sikkim",
  "State Unassigned",
  "Tamil Nadu",
  "Telangana",
  "Tripura",
  "Uttar Pradesh",
  "Uttarakhand",
  "West Bengal"
]
```

And the districts of a particular state:

```
> cat districts_daily.json | jq '.districtsDaily["Karnataka"] | keys'
[
  "Bagalkote",
  "Ballari",
  "Belagavi",
  "Bengaluru",
  "Bengaluru Rural",
  "Bengaluru Urban",
  "Bidar",
  "Chamarajanagara",
  "Chikkaballapura",
  "Chikkamagaluru",
  "Chitradurga",
  "Dakshina Kannada",
  "Davanagere",
  "Dharwad",
  "Gadag",
  "Hassan",
  "Haveri",
  "Kalaburagi",
  "Kodagu",
  "Kolar",
  "Koppal",
  "Mandya",
  "Mysuru",
  "Other State",
  "Raichur",
  "Ramanagara",
  "Shivamogga",
  "Tumakuru",
  "Udupi",
  "Unknown",
  "Uttara Kannada",
  "Vijayapura",
  "Yadgir"
]
```

Next, we can find out the active counts time-series for a particular district:

```
> cat /tmp/districts_daily.json | \
  jq '.districtsDaily["Karnataka"]["Bengaluru Urban"][] | [.date, .active] | join(", ")' | \
  tail -10
"2020-06-26, 1326"
"2020-06-27, 1912"
"2020-06-28, 2691"
"2020-06-29, 3426"
"2020-06-30, 3915"
"2020-07-01, 4648"
"2020-07-02, 5504"
"2020-07-03, 6296"
"2020-07-04, 7249"
"2020-07-05, 7249"
```

And finally, we can make some charts on the terminal using [Termgraph](https://github.com/mkaz/termgraph/):

### Count of active COVID-19 cases in Bengaluru Urban over last 30 days

```
> cat /tmp/districts_daily.json | \
  jq '.districtsDaily["Karnataka"]["Bengaluru Urban"][] | [.date, .active] | join(", ")' | \
  tail -31 | head -30 | sed 's/"//g' | \
  termgraph --width 50

2020-06-05: ▇ 150.00
2020-06-06: ▇ 162.00
2020-06-07: ▇ 161.00
2020-06-08: ▇ 176.00
2020-06-09: ▇ 204.00
2020-06-10: ▇ 244.00
2020-06-11: ▇ 258.00
2020-06-12: ▇▇ 290.00
2020-06-13: ▇▇ 319.00
2020-06-14: ▇▇ 330.00
2020-06-15: ▇▇ 362.00
2020-06-16: ▇▇ 372.00
2020-06-17: ▇▇ 413.00
2020-06-18: ▇▇ 408.00
2020-06-19: ▇▇▇ 531.00
2020-06-20: ▇▇▇▇ 621.00
2020-06-21: ▇▇▇▇▇ 796.00
2020-06-22: ▇▇▇▇▇▇ 919.00
2020-06-23: ▇▇▇▇▇▇ 996.00
2020-06-24: ▇▇▇▇▇▇▇ 1123.00
2020-06-25: ▇▇▇▇▇▇▇▇ 1206.00
2020-06-26: ▇▇▇▇▇▇▇▇▇ 1326.00
2020-06-27: ▇▇▇▇▇▇▇▇▇▇▇▇▇ 1912.00
2020-06-28: ▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇ 2691.00
2020-06-29: ▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇ 3426.00
2020-06-30: ▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇ 3915.00
2020-07-01: ▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇ 4648.00
2020-07-02: ▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇ 5504.00
2020-07-03: ▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇ 6296.00
2020-07-04: ▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇ 7249.00
```

### Count of COVID-19 cases in Bengaluru Urban over last 7 days

```
> cat districts_daily.json | \
  jq '.districtsDaily["Karnataka"]["Bengaluru", "Bengaluru Urban"][] | [.date, .confirmed, .active, .recovered, .deceased] | join(", ")' | \
  tail -8 | head -7 | { echo "@ Confirmed, Active, Recovered, Deceased";  sed 's/"//g'; } | \
  termgraph --color {red,blue,magenta,black} --width 100
```

![](/files/covid-india-charts/cases.png)

That's all for now folks. Stay safe and healthy.
