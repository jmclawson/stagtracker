Simple Shiny app for real-time tracking CTA arrivals at a station.

This app shows a small UI designed to run on a Raspberry Pi using a small touchscreen [like this one](https://www.amazon.com/dp/B0CXTFN8K9?ref=ppx_yo2ov_dt_b_fed_asin_title&th=1). I'm running it on a Raspberry Pi 4, since that's the hobby kit I had sitting around.

## Setup

In app_options.R, set the following values: 

* `api_key` - Provided by CTA upon [approval](https://www.transitchicago.com/developers/traintrackerapply/)
* `home_station` - Which station should the app default to showing?
* `home_line` - During commute time, which line should focus by default?
* `direction_order` - Between "▼" and "▲" (translating as 5 and 1), which should come first and be chosen as the default?

Clicking on the table makes it run full screen. 

Sample output:

