# COVID-19: international cases & deaths plot generator
Generating plots from JHU and UN data.

# Data sources
* COVID-19 cases and deaths: [John Hopkins University CSSE](https://github.com/CSSEGISandData/COVID-19/tree/master/csse_covid_19_data/csse_covid_19_time_series). 
* Country populations in 2020: [UN Population Division Total Population](https://population.un.org/wup/DataQuery/) database. 

# Process
COVID-19 cases and deaths are loaded from John Hopkins University CSSE Github page when the app is launched. 
Calculations are run in server.R and depend on the selections made by the user. 

# Options
There are three tabs. 
They contain plots of cumulative cases over time, cumulative deaths over time, or a rolling average of new cases and deaths over time. 
