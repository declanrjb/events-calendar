R tool to automatically compile the weekly blotter and events calendar at Reed College. Built for the student paper,
The Reed College Quest.

## Compiling the Blotter
1) Make sure your device has an internet connection.

2) Run the contents of `scripts/blotter.R`. Data scraped from
the [Reed campus police blotter](https://www.reed.edu/community_safety/past-blotters-activity.html) will be automatically
stored in `data/blotter.csv`. 

3) Knit `md-output/blotter.Rmd` to HTML.

4) Open the generated `md-output/blotter.html` file in a browser. Copy the contents into Adobe InDesign.

## Compiling the Events Calendar
1) Make sure your device has an internet connection.

2) Run the contents of `scripts/events_calendar.R`. Data scraped from
the [Reed events calendar](https://events.reed.edu/) will be automatically
stored in `data/events-calendar.csv`. 

3) Knit `md-output/events_calendar.Rmd` to HTML.

4) Open the generated `md-output/events_calendar.html` file in a browser. Copy the contents into Adobe InDesign.