# Real-time train tracking

Shiny app with minimal UI for arrivals at a CTA station, designed for a Raspberry Pi with a small touchscreen [like this one](https://www.amazon.com/dp/B0CXTFN8K9?ref=ppx_yo2ov_dt_b_fed_asin_title&th=1).

## Setup

In app_options.R, set the following values: 

* `api_key` - Provided by CTA upon [approval](https://www.transitchicago.com/developers/traintrackerapply/)
* `home_station` - Which station should the app default to showing?
* `commute` - During commute time, which line should focus by default?
* `show_rows` - How many rows should the screen show?
* `direction_order` - Between "▼" and "▲" (translating as 5 and 1), which should come first and be chosen as the default?

## Interface

At the top of the screen, a table shows the incoming trains for one station and one direction. Underneath, a picker sets the station and a three-way toggle sets the direction. The station set as `home_station` will also show a toggle allowing focus on a single `commute` line. At the bottom right a map button replaces the train time table with a map of incoming train location.

Every sixty seconds, the app requests updated data using the API provided. During set times on weekday mornings, the app will by default focus on the `commute` line. During set times in the evenings, the tracker will show arrival times for trains coming in either direction. 

Clicking on the table makes it run full screen. 

## Images

<dl>
<dt>Southbound from Belmont</dt>
<dd><img width="1490" height="1041" alt="belmont_southbound" src="https://github.com/user-attachments/assets/d3f26019-aa16-483e-9f87-415af3adecf9" /></dd>

<dt>Map at Belmont with popup</dt>
<dd><img width="1142" height="813" alt="belmont_map_popup_16" src="https://github.com/user-attachments/assets/d22f23ce-22fe-4998-8d25-1c8122271f8c" /></dd>

<dt>Clark and Lake</dt>
<dd><img width="1370" height="1005" alt="clark_and_lake_times" src="https://github.com/user-attachments/assets/527b1848-22ef-4fbe-b6fa-46b3a7e90b2a" /></dd>

<dt>Map at Clark and Lake</dt>
<dd><img width="1189" height="812" alt="clark_and_lake_map_city" src="https://github.com/user-attachments/assets/bde97aca-2614-4e6c-a31c-986d9e00c3bf" /></dd>

<dt>Belmont commute</dt>
<dd><img width="1649" height="1216" alt="belmont_commute_new" src="https://github.com/user-attachments/assets/e739388e-e10f-4251-81f5-8580357025bd" /></dd>

<dt>Station picker with large nub for scrolling</dt>
<dd><img width="1246" height="896" alt="station_picker_new" src="https://github.com/user-attachments/assets/5aceccf2-1fee-4f7c-82a0-ccc2f17cd109" /></dd>

</dl>

## Stagtracker?

In honor of [The Last Stag](https://hollowknight.fandom.com/wiki/Last_Stag).
