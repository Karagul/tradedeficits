# tradedeficits
Interactive R Shiny app exploring the US trade balance with other countries over time, 1998-present 

Developer: Dave Fitzpatrick

Live URL: https://tradetracker.shinyapps.io/ustrade/

Overview: This app explores the trade balance between the U.S. and it's top trade partners from 1998-present. The user can investigate trade balance by year, by product category, observe trends over time, and visualize these trends on a map.

Quick Start:

1. Download this repository to your local machine
2. Open R Studio and run server.r or ui.r to start the app

- Make sure you have the 'shiny' package installed

Components of the app:

User inputs: choose a range of years, comparison type, and product categories
Deficit Histogram: shows the U.S. trade deficit broken out by product category over time, as a % of U.S. GDP
World Map: Based on inputs, countries are color coded
Data Table: % of GDP, Prior year, period over period change, and share of deficit for top trading partners
