#Plot Testing by State
#SV Scarpino
#Mar 18 2020

###########
#libraries#
###########
library(ggplot2)

######
#Data#
######
dat <- read.csv("covid_tracking_addedPolitics.csv")
dat$date <- as.POSIXct(strptime(dat$date, format = "%Y%m%d"))

##########
#Analysis#
##########
states <- unique(dat$state)
new_tests <- rep(NA, nrow(dat))
new_pos_tests <- rep(NA, nrow(dat))
for(i in states){
  use.i <- which(dat$state == i)
  ord.i <- order(dat$date[use.i])
  diff.i <- diff(dat$totalTestResults[use.i][ord.i])
  diff2.i <- diff(dat$positive[use.i][ord.i])
  new_tests[use.i][ord.i][-1] <- diff.i
  new_pos_tests[use.i][ord.i][-1] <- diff2.i
}
dat$new_tests <- new_tests
dat$new_pos_tests <- new_pos_tests

######
#Plot#
######
ggplot(dat, aes(as.factor(date), new_pos_tests/new_tests, fill = gov_party)) + geom_boxplot() + scale_y_log10()
