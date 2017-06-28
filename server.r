source('libraries.R') # load required libraries
source('load_data.R') # load data and do some pre-processing
 
# data.all: Monthly imports/exports by year, product, country
# data.gdp: (Year, GDP) US 
# sitc: (SITC, Category)

# create Country, Year level data w/ month vars inclusive of all SITC codes
data.allproducts <- aggregate(data.all[,c(5:30)],by=list(data.all[,"Year"],data.all[,"Country"]),sum)
names(data.allproducts)[1:2] <- c("Year","Country")

# function to compute trade deficits by year
deficits_by_year <- function (data.input, keys) {
  data.byyear <- data.input[,c("Year",keys,"ExportsYtdDec","ImportsYtdDec")]
  data.byyear <- transform(data.byyear, Deficit = ImportsYtdDec - ExportsYtdDec)
  data.byyear <- data.byyear[,c("Year",keys,"Deficit")]
  data.byyear <- reshape(data.byyear[,c("Year",keys,"Deficit")], v.names="Deficit", idvar=keys, timevar="Year", direction="wide")
  return(data.byyear)
}

# compute trade deficits by year at product/country and country level
data.sitc.byyear <- deficits_by_year(data.all, c("SITC","Country"))
data.allproducts.byyear <- deficits_by_year(data.allproducts, c("Country"))

# extract World totals from Country level dataset
data.world.byyear <- data.all[,c("Year","SITC","Category","Country","ExportsYtdDec","ImportsYtdDec")]
data.world.byyear <- merge(data.world.byyear,data.gdp,by="Year")
data.world.byyear <- transform(data.world.byyear,ImportsPct = ImportsYtdDec / GDP)
data.world.byyear <- transform(data.world.byyear,ExportsPct = ExportsYtdDec / GDP)

data.countries.exports <- data.world.byyear[,c("SITC","Category","Year","Country","ExportsPct")]
data.countries.imports <- data.world.byyear[,c("SITC","Category","Year","Country","ImportsPct")]

data.world.byyear <- subset(data.world.byyear,Country=="WorldTotal")
data.world.exports <- data.world.byyear[,c("SITC","Year","ExportsPct")]
data.world.imports <- data.world.byyear[,c("SITC","Year","ImportsPct")]

# base world map
mp <- NULL
mapWorld <- borders("world", colour="black", fill="gray50") # create a layer of borders
mp_init <- ggplot() + mapWorld
map.world <- map_data(map="world")

# calculate % of GDP, change, and Deficit share based on user inputs and add to base map
tradeData <- function (year,relative,sitc.codes) {
  # get data based on user selection
  data.selected <- subset(data.sitc.byyear,SITC %in% sitc.codes)
  data.selected <- aggregate(data.selected[,3:ncol(data.selected)],by=list(data.selected$Country),sum)
  names(data.selected)[1] <- "Country"
  data.selected.world <- subset(data.selected,Country == "WorldTotal")
  
  # append yearly deficit totals to base map
  map.year <- merge(map.world,data.selected,by.x="region",by.y="Country",all.x=T)
  map.year <- map.year[,c("long","lat","group","order","region","subregion",names(data.selected[2:ncol(data.selected)]))]
  map.year <- map.year[order(map.year$order),]
  
  var.years <- names(map.year)[names(map.year) >= paste("Deficit",year[1],sep=".") & names(map.year) <= paste("Deficit",year[2],sep=".")]
  var.priors <- names(map.year)[names(map.year) < paste("Deficit",year[1],sep=".") & names(map.year) >= paste("Deficit",2*year[1]-1-year[2],sep=".")]
  
  deficitTotal <- sum(data.selected.world[,var.years])
  deficitPrior <- sum(data.selected.world[,var.priors])
  GDPTotal <- sum(subset(data.gdp,Year >= year[1] & Year <= year[2])[,"GDP"])
  GDPPrior <- sum(subset(data.gdp,Year < year[1] & Year >= 2*year[1]-1-year[2] & Year >= 1996)[,"GDP"])
  
  map.year <- transform(map.year, DeficitGDP = rowSums(map.year[,var.years]) / GDPTotal)
  map.year <- transform(map.year, DeficitShare = rowSums(map.year[,var.years]) / deficitTotal)
  map.year <- transform(map.year, DeficitPrior = rowSums(map.year[,var.priors]) / GDPPrior)
  map.year <- transform(map.year, DeficitChg = DeficitGDP / DeficitPrior - 1)
  map.year[,"Deficit"] <- map.year[,paste("Deficit",relative,sep="")]
  map.year <- transform(map.year,DeficitFill = Deficit * as.numeric(DeficitShare >= 0.01))
  return(map.year)
}

# output the map with countries shaded appropriately
tradeMap <- function (mapData, year, relative) {
  title <- paste("U.S. Trade Deficits, ", year[1], "-", year[2], sep="")
  if (relative == "Chg") {
    priors = c(year[1] - 1- (year[2] - year[1]), year[1] - 1)
    title <- paste("Change in ",title," vs. ",priors[1],"-",priors[2],sep="")
  }
  map.year <- mapData
  mp <- mp_init + geom_map(data=map.year, map=map.year, aes(map_id=region, fill=DeficitFill))
  mp <- mp + scale_fill_gradient2(name="",low="green",mid="gray80",high="red",midpoint=0, guide = "colourbar", labels = scales::percent)
  mp <- mp + theme(legend.position = "bottom")
  mp <- mp + guides(fill = guide_colorbar(barwidth = 30, barheight = 1.5))
  mp <- mp + coord_equal()
  mp <- mp + borders("world", colour="black")
  mp <- mp + ggtitle(title) + theme(plot.title = element_text(size=24, face="bold"))
  return(mp)
}

shinyServer(function(input, output, session) {
  
  dataInput <- reactive({
    tradeData(input$year,input$relative,input$sitc.codes)
  })
  
  dataOutput <- reactive({
    tradeOutput <- dataInput()[,c("region","DeficitGDP","DeficitPrior","DeficitChg","DeficitShare")]
    tradeOutput <- subset(tradeOutput,!is.na(DeficitGDP) & DeficitShare >= 0.01)
    names(tradeOutput) <- c("Country","% of GDP","Prior","Change","% of Deficit")
    unique(tradeOutput)
  })
  
  worldImports <- reactive({
    subset(data.world.imports,SITC %in% input$sitc.codes)
  })
  
  worldExports <- reactive({
    subset(data.world.exports,SITC %in% input$sitc.codes)
  })
  
  countrySelected <- reactive({
    dataOutput()[input$table_rows_selected,"Country"]
  })
  
  countryImports <- reactive({
    subset(data.countries.imports,Country == countrySelected())
  })
  
  countryExports <- reactive({
    subset(data.countries.exports,Country == countrySelected())
  })
  
  output$deficit <- renderPlot({
    ggplot() + 
      geom_bar(data = worldExports(), aes(x=Year, y=-1*ExportsPct, fill=SITC),stat = "identity") +
      geom_bar(data = worldImports(), aes(x=Year, y=ImportsPct, fill=SITC),stat = "identity") +
      scale_fill_brewer(type = "qual", palette = "Paired") + 
      scale_y_continuous(name="% of GDP", labels = scales::percent, limits=c(-0.15,0.15)) + 
      scale_x_discrete(breaks = seq(1996, 2016, by = 2))
  })
  
  output$country <- renderPlot({
    ggplot() + 
      geom_bar(data = countryExports(), aes(x=Year, y=-1*ExportsPct, fill=Category),stat = "identity") +
      geom_bar(data = countryImports(), aes(x=Year, y=ImportsPct, fill=Category),stat = "identity") +
      scale_fill_brewer(type = "qual", palette = "Paired") + 
      ggtitle(paste("Trade with",countrySelected(),sep=" ")) + 
      theme(legend.position = "bottom") + 
      scale_y_continuous(name="% of U.S. GDP", labels = scales::percent) +
      theme(plot.title = element_text(size=22)) + 
      theme(plot.title = element_text(hjust = 0.5))
  })
  
  output$map <- renderPlot({ 
    tradeMap(dataInput(), input$year, input$relative)
  })
  
  output$largemap <- renderPlot({ 
    tradeMap(dataInput(), input$year, input$relative)
  })
  
  # Show country data when hovering (work in progress)
  # output$hover_info <- renderUI({
  #   hover <- input$plot_hover
  #   point <- nearPoints(map.world, hover, threshold = 5, maxpoints = 1, addDist = TRUE)
  #   if (nrow(point) == 0) return(NULL)
  #   
  #   hover.data <- subset(dataOutput(),Country == point$region)
  #   
  #   # calculate point position INSIDE the image as percent of total dimensions
  #   # from left (horizontal) and from top (vertical)
  #   left_pct <- (hover$x - hover$domain$left) / (hover$domain$right - hover$domain$left)
  #   top_pct <- (hover$domain$top - hover$y) / (hover$domain$top - hover$domain$bottom)
  #   
  #   # calculate distance from left and bottom side of the picture in pixels
  #   left_px <- hover$range$left + left_pct * (hover$range$right - hover$range$left)
  #   top_px <- hover$range$top + top_pct * (hover$range$bottom - hover$range$top)
  #   
  #   # create style property fot tooltip
  #   # background color is set so tooltip is a bit transparent
  #   # z-index is set so we are sure are tooltip will be on top
  #   style <- paste0("position:absolute; z-index:100; background-color: rgba(245, 245, 245, 0.85); ",
  #                   "left:", left_px + 2, "px; top:", top_px + 2, "px;")
  #   
  #   # actual tooltip created as wellPanel
  #   wellPanel(
  #     style = style,
  #     p(HTML(paste0("<b> Country: </b>", hover.data$Country, "<br/>",
  #                   "<b> Deficit: </b>", hover.data[,2], "<br />",
  #                   "<b> Change: </b>", hover.data[,3], "<br />",
  #                   "<b> Share: </b>", hover.data[,4])))
  #   )
  # })
  
  output$table <- DT::renderDataTable({
    datatable(dataOutput(), selection='single',rownames=F, caption='Click a country to drill down',
              options=list(searching=F,pageLength=20,paging=F,order=list(list(2,'desc')))) %>% 
      formatPercentage('Change', 2) %>% 
      formatPercentage('% of GDP',2) %>% 
      formatPercentage('Prior',2) %>%
      formatPercentage('% of Deficit',2)
  })
  
  observeEvent(input$table_rows_selected, {
    toggleModal(session,"countrySelect")
  })
  observeEvent(input$map_click, {
    toggleModal(session,"mapSelect")
  }) 
}
)