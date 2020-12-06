#SV Scarpino
#Dec 2020
#COVID MA

###########
#libraries#
###########
library(zoo)

#########
#Globals#
#########


######
#Data#
######
test <- read.csv("covid-19-dashboard_12-05-2020/Testing2.csv")
pos <- read.csv("covid-19-dashboard_12-05-2020/TestingByDate.csv")

test$Date <- as.POSIXct(strptime(test$Date , format = "%m/%d/%y"))
pos$Date <- as.POSIXct(strptime(pos$Date , format = "%m/%d/%y"))

per_first_time_pos <- pos$Molecular.Positive.New/pos$First.Molecular.Test.per.person
per_repeat_pos_no_uni <- (pos$All.Positive.Molecular.Tests_MA.without.Higher.ED - pos$Molecular.Positive.New)/(pos$All.Molecular.Tests_MA.without.Higher.ED - pos$First.Molecular.Test.per.person)
per_all_pos_no_uni <- pos$All.Positive.Molecular.Tests_MA.without.Higher.ED/pos$All.Molecular.Tests_MA.without.Higher.ED
per_all_pos_uni <- pos$All.Positive.Molecular.Tests_Higher.Ed.ONLY/pos$All.Molecular.Tests_Higher.Ed.ONLY

#######
#Plots#
#######
use <- which(pos$Date > as.POSIXct(strptime("10/01/20", format = "%m/%d/%y")))
use2 <- which(pos$Date > as.POSIXct(strptime("09/01/20", format = "%m/%d/%y")))

quartz()
plot(pos$Date[use], per_first_time_pos[use], type = "p", pch = 16, xlab = "2020 (daily data)", ylab = "Prop. positive (7 day avg.)", main = "~20% of 1st time COVID-19 tests are positives in MA!", bty = "n", lwd = 2, ylim = c(0, 0.22), col = "#4d4d4d00")
points(pos$Date[use2][-c(1:6)], rollmean(per_first_time_pos[use2], 7), type = "l", col = "#b2182b", lwd = 4, lty = 1)
points(pos$Date[use2][-c(1:6)], rollmean(per_all_pos_no_uni[use2], 7), type = "l", col = "#4d4d4d", lwd = 3, lty = 5)
points(pos$Date[use2][-c(1:6)], rollmean(per_repeat_pos_no_uni[use2], 7), type = "l", col = "#4575b4", lwd = 3, lty = 4)
points(pos$Date[use2][-c(1:6)], rollmean(per_all_pos_uni[use2], 7), type = "l", col = "#fee090", lwd = 3, lty = 6)

legend(as.POSIXct(strptime("10/05/20", format = "%m/%d/%y")), 0.18, col = c("#b2182b", "#4d4d4d", "#4575b4","#fee090"), lty = c(1, 5, 4, 6), lwd = 3, legend = c("1st time tested", "1st & repeat (no higher-ed)", "Repeat only (no higher-ed)", "Higher-ed only"), bty = "n")


###########
#Model fit#
###########

quartz()
plot(pos$Date[use], per_first_time_pos[use], type = "p", pch = 16, xlab = "2020 (daily data)", ylab = "Prop. positive (7 day avg.)", main = "COVID-19 postive test rates are as predicted in MA!", bty = "n", lwd = 2, ylim = c(0, 0.22), col = "#4d4d4d00")
points(pos$Date[use2][-c(1:6)], rollmean(per_first_time_pos[use2], 7), type = "l", col = "#b2182b", lwd = 4, lty = 1)
points(pos$Date[use2][-c(1:6)], rollmean(per_all_pos_no_uni[use2], 7), type = "l", col = "#4d4d4d", lwd = 3, lty = 5)
points(pos$Date[use2][-c(1:6)], rollmean(per_repeat_pos_no_uni[use2], 7), type = "l", col = "#4575b4", lwd = 3, lty = 4)
points(pos$Date[use2][-c(1:6)], rollmean(per_all_pos_uni[use2], 7), type = "l", col = "#fee090", lwd = 3, lty = 6)

legend(as.POSIXct(strptime("10/05/20", format = "%m/%d/%y")), 0.2, col = c("#b2182b", "#4d4d4d", "#4575b4","#fee090"), lty = c(1, 5, 4, 6), lwd = 3, legend = c("1st time tested", "1st & repeat (no higher-ed)", "Repeat only (no higher-ed)", "Higher-ed only"), bty = "n")

train <- 44:71
test <- (train[length(train)]+1):length(use2)
x <- (train[-c(1:6)]-min(train))
mod <- lm(log(rollmean(per_all_pos_no_uni[use2[train]], 7)) ~ x)
eval <- mod$coefficients[1] + mod$coefficients[2]*(test-min(train))
points(pos$Date[use2[train+1][-c(1:6)]], exp(mod$fitted.values), col = "#4d4d4d")
points(pos$Date[use2[test+1]], exp(eval), pch = 15, col = "#4d4d4d")

train <- 44:71
test <- (train[length(train)]+1):length(use2)
x <- (train[-c(1:6)]-min(train))
mod <- lm(log(rollmean(per_first_time_pos[use2[train]], 7)) ~ x)
eval <- mod$coefficients[1] + mod$coefficients[2]*(test-min(train))
points(pos$Date[use2[train+1][-c(1:6)]], exp(mod$fitted.values), col = "#b2182b")
points(pos$Date[use2[test+1]], exp(eval), pch = 17, col = "#b2182b")

legend(as.POSIXct(strptime("10/05/20", format = "%m/%d/%y")), 0.15, col = c("#b2182b", "#b2182b", "#4d4d4d", "#4d4d4d"), legend = c("Model train (1st time tested)", "Out-of-sample", "Model train (1st & repeat no higher-ed)", "Out-of-sample"), pch = c(1, 17, 1, 15), bty = "n")
