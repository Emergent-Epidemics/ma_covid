#SV Scarpino
#Dec 2020
#COVID MA

###########
#libraries#
###########
library(zoo)
library(glmulti)

#########
#Globals#
#########
do_plots <- FALSE

######
#Data#
######
test <- read.csv("covid-19-dashboard_12-14-2020/Testing2.csv")
pos <- read.csv("covid-19-dashboard_12-14-2020/TestingByDate.csv")
hosp <- read.csv("covid-19-dashboard_12-14-2020/Hospitalization from Hospitals.csv")

test$Date <- as.POSIXct(strptime(test$Date , format = "%m/%d/%y"))
pos$Date <- as.POSIXct(strptime(pos$Date , format = "%m/%d/%y"))
hosp$Date <- as.POSIXct(strptime(hosp$Date , format = "%m/%d/%y"))
hosp$roll_new_7 <- c(rep(NA, 6), rollmean(hosp$Net.new.number.of.confirmed.COVID.patients.in.hospital.today, 7))
mt_hosp_date <- match(pos$Date, hosp$Date)
hosps <- hosp$Total.number.of.confirmed.COVID.patients.in.hospital.today[mt_hosp_date]
  
repeat_tests_non_higher_ed <- pos$All.Molecular.Tests-pos$First.Molecular.Test.per.person-pos$All.Molecular.Tests_Higher.Ed.ONLY

per_first_time_pos <- pos$Molecular.Positive.New/pos$First.Molecular.Test.per.person
per_first_time_pos_mod <- (pos$Molecular.Positive.New-(0.01*repeat_tests_non_higher_ed))/pos$First.Molecular.Test.per.person
per_repeat_pos_no_uni <- (pos$All.Positive.Molecular.Tests_MA.without.Higher.ED - pos$Molecular.Positive.New)/(pos$All.Molecular.Tests_MA.without.Higher.ED - pos$First.Molecular.Test.per.person)
per_all_pos_no_uni <- pos$All.Positive.Molecular.Tests_MA.without.Higher.ED/pos$All.Molecular.Tests_MA.without.Higher.ED
per_all_pos_uni <- pos$All.Positive.Molecular.Tests_Higher.Ed.ONLY/pos$All.Molecular.Tests_Higher.Ed.ONLY

##########
#Hosp pre#
##########
use_hosp <- which(pos$Date > as.POSIXct(strptime("09/01/20", format = "%m/%d/%y")))

glmulti.hosp <-
  glmulti(hosps[use_hosp][-c(1:14)] ~ per_first_time_pos[use_hosp][-c(81:94)] + per_repeat_pos_no_uni[use_hosp][-c(81:94)] + per_first_time_pos_mod[use_hosp][-c(81:94)] + per_all_pos_no_uni[use_hosp][-c(81:94)] + per_all_pos_uni[use_hosp][-c(81:94)],
          level = 1,               # 2 = pairwise interaction considered
          method = "h",            # h = Exhaustive approach
          crit = "bic",            # bic = BIC as criteria
          confsetsize = 1,        # Keep # best models
          plotty = F, report = F,  # Plot or interim reports
          fitfunction = lm)
best.mod.hosp <- slot(object = glmulti.hosp, name = "objects")[[1]]
summary(best.mod.hosp)

quartz()
layout(matrix(1:4, nrow = 2))
  plot(rollmean(per_first_time_pos[use_hosp][-c(81:94)],14), rollmean(hosps[use_hosp][-c(1:14)],14), xlab = "14 day lagged Prop. pos. (all assumed 1st time pos, 14 day avg)", ylab = "State-wide admissions (14 day avg)", bty = "n", main = "All assumed first time tested vs. hospital admissions (COVID-19 in MA)", pch = 16, col = "#b2182b")
  
  plot(rollmean(per_first_time_pos_mod[use_hosp][-c(81:94)], 14), rollmean(hosps[use_hosp][-c(1:14)], 14), xlab = "14 day lagged Prop. pos. (est. first time tested, 14 day avg)", ylab = "State-wide admissions (14 day avg)", bty = "n", main = "Est. first time tested vs. hospital admissions (COVID-19 in MA)", pch = 16, col = "#2166ac")
  
  plot(rollmean(per_all_pos_no_uni[use_hosp][-c(81:94)], 14), rollmean(hosps[use_hosp][-c(1:14)], 14), xlab = "14 day lagged Prop. pos. (all tested, no higher-ed, 14 day avg)", ylab = "State-wide admissions (14 day avg)", bty = "n", main = "All tested, no higher-ed vs. hospital admissions (COVID-19 in MA)", pch = 16, col = "#f4a582")
  
  plot(rollmean(per_all_pos_uni[use_hosp][-c(81:94)], 14), rollmean(hosps[use_hosp][-c(1:14)], 14), xlab = "14 day lagged Prop. pos. (only higher-ed, 14 day avg)", ylab = "State-wide admissions (14 day avg)", bty = "n", main = "Only higher-ed testing vs. hospital admissions (COVID-19 in MA)", pch = 16, col = "#4d4d4d")

summary(lm(hosps[use_hosp][-c(1:14)] ~ per_first_time_pos[use_hosp][-c(81:94)] ))
summary(lm(hosps[use_hosp][-c(1:14)] ~ per_first_time_pos_mod[use_hosp][-c(81:94)] ))

#######
#Plots#
#######
if(do_plots == TRUE){
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
}
