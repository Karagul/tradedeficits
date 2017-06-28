# read in data
data.gdp <- read.csv("data\\gdp.csv",stringsAsFactors=F)
data.all <- read.csv("data\\sitc1digit.csv",stringsAsFactors=F)
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

data.all <- merge(data.all,sitc,by="SITC",all.x=T)

# create Country, Year, SITC level dataset w/ month vars
data.all$Year <- factor(data.all$Year)
data.all$SITC <- factor(data.all$SITC)
data.all <- data.all[,-grep("GenImportsCIF",names(data.all))]
data.all <- data.all[,-grep("CTY_CODE",names(data.all))]
names(data.all) <- gsub("ExportsFASValueBasis","Exports",names(data.all))
names(data.all) <- gsub("GenImportsCustomsValBasis","Imports",names(data.all))
data.all[,5:30] <- data.all[,5:30] / 1e9