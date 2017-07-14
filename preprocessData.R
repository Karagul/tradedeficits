# Purpose: Reads in raw data files and processes them into data to be used by the app, to save on data loading time

# read in raw data
data.gdp <- read.csv("data/gdp.csv",stringsAsFactors=F)
data.all <- read.csv("data/sitc1digit.csv",stringsAsFactors=F)
#data.all3 <- read.csv("data\\sitc3digit.csv",stringsAsFactors=F)

# SITC codes: https://www.census.gov/foreign-trade/reference/codes/sitc/sitc.txt
sitc <- as.data.frame(cbind(c(seq(0,9,1)),c("Food/Animals",
                                            "Beverages/Tobacco",
                                            "Crudes ex Fuels",
                                            "Fuels, related",
                                            "Oils, Fats, Waxes",
                                            "Chemicals, related",
                                            "Manuf. Materials",
                                            "Machinery/Transport",
                                            "Misc. Manufactured",
                                            "Commodities/Transact")))
names(sitc) <- c("SITC","Category")

# merge SITC codes onto raw data
data.all <- merge(data.all,sitc,by="SITC",all.x=T)

# create Country, Year, SITC level dataset w/ month vars
data.all$Year <- factor(data.all$Year)
data.all$SITC <- factor(data.all$SITC)
data.all <- data.all[,-grep("GenImportsCIF",names(data.all))]
data.all <- data.all[,-grep("CTY_CODE",names(data.all))]
names(data.all) <- gsub("ExportsFASValueBasis","Exports",names(data.all))
names(data.all) <- gsub("GenImportsCustomsValBasis","Imports",names(data.all))
data.all[,5:30] <- data.all[,5:30] / 1e9

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

# create separate datasets for country exports and imports
data.countries.exports <- data.world.byyear[,c("SITC","Category","Year","Country","ExportsPct")]
data.countries.imports <- data.world.byyear[,c("SITC","Category","Year","Country","ImportsPct")]

# create separate datasets for entire world totals
data.world.byyear <- subset(data.world.byyear,Country=="WorldTotal")
data.world.exports <- data.world.byyear[,c("SITC","Year","ExportsPct")]
data.world.imports <- data.world.byyear[,c("SITC","Year","ImportsPct")]

# save processed data
write.csv(data.sitc.byyear,'processed/data_sitc_byyear.csv',row.names=F)
write.csv(data.gdp,'processed/data_gdp.csv',row.names=F)
write.csv(data.world.exports,'processed/data_world_exports.csv',row.names=F)
write.csv(data.world.imports,'processed/data_world_imports.csv',row.names=F)
write.csv(data.countries.exports,'processed/data_countries_exports.csv',row.names=F)
write.csv(data.countries.imports,'processed/data_countries_imports.csv',row.names=F)
