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
#Download PDFs#
###############
dates_format <- tolower(format(dates, format = "%B"))
dates_format <- glue("{dates_format}-{day(dates)}")
missed <- c()
for(i in dates_format){
  if(i %in% c("march-9",  "march-10", "march-11", "march-12")){
    file.i <- paste0("https://www.mass.gov/doc/covid-19-cases-in-massachusetts-", i, "-2020/download")
  }else{
    if(i == "march-19"){
      file.i <- "https://www.mass.gov/doc/covid-19-cases-in-massachusetts-as-of-march-19-2020-x-updated4pm/download"
    }else{
      file.i <- paste0("https://www.mass.gov/doc/covid-19-cases-in-massachusetts-as-of-", i, "-2020/download")
    }
  }
  
  dest.i <- paste0("../Data/PDFs/", i, ".pdf")
  try.i <- try(download.file(url = file.i, destfile = dest.i), silent = TRUE)
  if(length(grep("error", try.i, ignore.case = TRUE)) > 0){
    missed <- c(missed, i)
    }
}
