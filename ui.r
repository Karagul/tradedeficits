shinyUI(fluidPage(
  sidebarPanel(
    sliderInput("year", label="Year Range", min = 1998, max = 2016, value = c(2012,2016)),
    radioButtons("relative","Color Code: ",c("% of US GDP"="GDP","Change in % of US GDP"="Chg"),selected="GDP"),
    checkboxGroupInput("sitc.codes", "Products:",
                       c("Food and Live Animals" = 0,
                         "Beverages and Tobacco" = 1,
                         "Crude Materials, except Fuels" = 2,
                         "Fuels, Lubricants, Related" = 3,
                         "Animal/Veg Oils, Fats, Waxes" = 4,
                         "Chemicals and Related" = 5,
                         "Manufactured Materials" = 6,
                         "Machinery and Transport" = 7,
                         "Misc. Manufactured Articles" = 8,
                         "Commodities and Other" = 9),
              selected = c(0:9)),
    plotOutput("deficit")
  ),
  mainPanel(
      textOutput("Map"),
      plotOutput("map", click ="map_click"),
      #               hover = hoverOpts("plot_hover", delay = 100, delayType = "debounce")),
      #uiOutput("hover_info"),
      DT::dataTableOutput("table"),
      bsModal("countrySelect","","go",size="large",plotOutput("country")),
      bsModal("mapSelect","","go",size="large",plotOutput("largemap"))
    )
  )
)