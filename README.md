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

At the top of the screen, a table shows the incoming trains for one station and one direction. Underneath, a toggle sets the direction, and a picker sets the stations. The station set as `home_station` will also show a toggle at the bottom right, allowing focus on a single line.

Every sixty seconds, the app requests updated data using the API provided. During set times on weekday mornings, the app will by default focus on the `commute` line. During set times in the evenings, the tracker will show arrival times for trains coming in either direction. 

Clicking on the table makes it run full screen. 

## Images

<dl>
<dt>Southbound from Belmont</dt>
<dd><img width="770" height="447" alt="Screenshot 2025-10-07 at 8 14 19 AM" src="https://github.com/user-attachments/assets/cbbfa29c-b612-4f4b-9389-09cfcceed1d0" /></dd>

<dt>Clark and Lake on device</dt>
<dd><img width="4032" height="3024" alt="clark_lake" src="https://github.com/user-attachments/assets/40df18af-2c8e-4024-9f6a-75bbbcac5a21" /></dd>

<dt>Belmont commute</dt>
<dd><img width="4032" height="3024" alt="belmont_commute" src="https://github.com/user-attachments/assets/9e785c23-4575-4ed9-8e4f-d23656f4cf5f" /></dd>

<dt>Station Picker</dt>
<dd><img width="3534" height="2601" alt="station_picker" src="https://github.com/user-attachments/assets/ffa4447a-1d41-4e81-bb72-44ac43034004" /></dd>

</dl>
