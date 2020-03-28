#SV Scarpino
#March 2020
#PDF to CSV for MA DPH COVID19

###########
#libraries#
###########
library(pdftools)
library(lubridate)
library(glue)

#########
#Globals#
#########
start <- as.POSIXct(strptime("2020-03-09", format = "%Y-%m-%d"))
end <- as.POSIXct(strptime(substr(Sys.time(), 1, 10), format = "%Y-%m-%d")) 
dates <- seq(start, end, by = 60*60*24)

###############
#Acc funcitons#
###############
find_county <- function(x, county){
  find <- which(x == county)
  if(length(find) == 0){
    find <- NA
  }
  return(find)
}
######
#Data#
######
county_census <- read.csv("../Data/Meta_data/whichCounty.csv", header = FALSE, stringsAsFactors = FALSE)

###################
#Loop through PDFs#
###################
counties <- unique(county_census$V2)
counties <- gsub(pattern = " County", replacement = "", counties)
files <- list.files("../Data/PDFs/")
data <- matrix(NA, nrow = length(dates), ncol = length(counties))
rownames(data) <- as.character(dates)
colnames(data) <- counties
data <- as.data.frame(data)

for(i in 1:length(files)){
  date.i <- strptime(paste0(strsplit(files[i], ".pdf")[[1]], "-2020"), format = "%B-%d-%Y")
  mt.i <- which(rownames(data) == date.i)
  if(length(mt.i) != 1){
    stop("Date match error")
  }
  pdf.i <- pdf_text(paste0("../Data/PDFs/", files[i]))
  data_split.i <- strsplit(pdf.i, "\n")
  county.i <- strsplit(data_split.i[[1]], " ")
  for(c in 1:ncol(data)){
    
    county.c.i <- unlist(lapply(county.i, FUN = find_county, county = colnames(data)[c]))
    find.c.i <- which(county.c.i == 1)
    
    if(length(find.c.i) == 0){
      next
    }else{
      if(length(find.c.i) > 1){
        if(colnames(data)[c] == "Berkshire" & length(grep("Berkshire Medical", data_split.i[[1]][find.c.i])) == 1){
          find.c.i <- find.c.i[1]
        }else{
          stop("County match error")
        }
      }
    }
  
    raw.c.i <- unlist(strsplit(data_split.i[[1]][find.c.i], " "))
    num.c.i <- na.omit(as.numeric(raw.c.i))
    if(length(num.c.i) != 1){
      if(length(num.c.i) == 2){
        num.c.i <- num.c.i[1]
      }else{
        stop("Case count error")
      }
    }
    data[mt.i, c] <- num.c.i
  }
}

data$Dukes_And_Nantucket <- data$Dukes #as of Mar 27th, state is reporting combined counts for these two counties
data$Dukes <- rep(NA, nrow(data))

write.csv(data, file = "../Data/Case Counts/MA_DPH_COVID19.csv", quote = FALSE)