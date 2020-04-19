#SV Scarpino
#March 2020
#Testing and Cases for MA DPH COVID19

###########
#libraries#
###########


#########
#Globals#
#########


######
#Data#
######
tests <- c(1296, 1751, 2271, 3132, 4091, 5207, 6004, 8922, 13749, 19794, 23621, 29371, 35049, 39066, 42793, 46935, 51738, 56608, 62962, 68800,71937,76429,81344,87511,94958,102372, 108776,116730, 122049, 126551, 132023, 140773, 148744, 156806) #taken from PDFs, last updated for Apr 17th

source("growth_rates.R")

start <- 9
stop <- nrow(diff_mat)-1

#######
#Plots#
#######
plot(as.POSIXct(strptime(diff_mat[start:stop,1], format = "%Y-%m-%d")) , rowSums(diff_mat[,-1], na.rm = T)[start:stop], type = "b", pch = 16, lwd = 2, bty = "n", xlab = "", ylab = "Number of new cases", main = "COVID-19 daily cases. in MA")

plot(as.POSIXct(strptime(diff_mat[start:stop,1], format = "%Y-%m-%d")) , diff(tests), type = "b", pch = 16, lwd = 2, bty = "n", xlab = "", ylab = "Number of new COVID19 tests", main = "COVID-19 daily tests in MA")

plot(as.POSIXct(strptime(diff_mat[start:stop,1], format = "%Y-%m-%d")) , rowSums(diff_mat[,-1], na.rm = T)[start:stop]/diff(tests), type = "b", ylim = c(0, 0.35), pch = 16, lwd = 2, bty = "n", xlab = "", ylab = "Proportion of tests positive", main = "COVID-19 positive test prop. in MA")

abline(lm(as.numeric( rowSums(diff_mat[,-1], na.rm = T)[start:stop]/diff(tests))~as.POSIXct(strptime(diff_mat[start:stop,1], format = "%Y-%m-%d"))), lty = 3, lwd = 2, col = "red")
