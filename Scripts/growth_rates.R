#SV Scarpino
#March 2020
#PDF to CSV for MA DPH COVID19

###########
#libraries#
###########
library(ggplot2)
library(dplyr)
library(tidyr)
library(wesanderson)
library(scales)
library(lme4)

#########
#Globals#
#########


######
#Data#
######
data <- read.csv("../Data/Case Counts/MA_DPH_COVID19.csv", stringsAsFactors = FALSE)

##########
#Plotting#
##########
plot.df <- data %>% gather(location, cases, Middlesex:Dukes_And_Nantucket) 
colnames(plot.df) <- c("Date", "County", "Cumulative")
plot.df$Date <- as.POSIXct(strptime(plot.df$Date, format = "%Y-%m-%d"))

cols <- sample(wes_palette(name = "Zissou1", n = length(unique(plot.df$County)), type = "continuous"), length(unique(plot.df$County)))

##############
#Growth rates#
##############
dates <- as.POSIXct(strptime(data[,1], format = "%Y-%m-%d")) 
diff_mat <- data
for(i in 2:ncol(diff_mat)){
  diff_mat[-1,i] <- diff(diff_mat[,i])
}

diff.df <- diff_mat %>% gather(location, cases, Middlesex:Dukes_And_Nantucket) 
colnames(diff.df) <- c("Date", "County", "New")
diff.df$Date <- as.POSIXct(strptime(diff.df$Date, format = "%Y-%m-%d"))

doubling_prov <- list()
doubling_fixed <- list()
for(i in 1:(length(dates)-6)){
  use.i <- which(diff.df$Date >= dates[i] & diff.df$Date < dates[i+6])
  
  data.i <- diff.df[use.i, ]
  
  data.i$DATE <- as.numeric(data.i$Date - dates[i], unit = "days")
  mod3.i <- try(lmer(data = data.i, log(New + 1) ~ DATE + (DATE|County)), silent = TRUE)
  
  if(is(mod3.i)[1] == "try-error"){
    fixed.i <- NA
    doubling.i <- NA
    mob.i <- NA
    names.i <- NA
  }else{
    fixed.i <-   fixef(mod3.i)["DATE"]
    doubling.i <- ranef(mod3.i)$County$DATE+fixed.i
    names.i <- rownames(ranef(mod3.i)$County)
  }
  
  doubling_prov[[i]] <- doubling.i
  names(doubling_prov[[i]]) <- names.i
  doubling_fixed[[i]] <- fixed.i
}

rates <- unlist(lapply(doubling_prov, function(x) x))
prov <- unlist(lapply(doubling_prov, function(x) names(x)))
times <- rep(dates[1:(length(dates)-6)], times = unlist(lapply(doubling_prov, function(x) length(x))))
dat.plot <- data.frame(rates, times, prov)

quartz(width = 8, height = 6)
ggplot(dat.plot, (aes(x = times, y = rates, color = prov))) + geom_line() + geom_line(size = 1.2) + scale_color_manual(values = cols) + xlab("2020") + ylab("COVID19 growth rate (country-level)") + theme(legend.position = "right", legend.key = element_rect(fill = "#f0f0f0"), legend.background = element_rect(fill = "#ffffffaa", colour = "black"), panel.background = element_rect(fill = "white", colour = "black"), axis.text.y = element_text(colour = "black", size = 14), axis.text.x = element_text(colour = "black", size = 10), axis.title = element_text(colour = "black", size = 16), panel.grid.minor = element_line(colour = "#00000000",linetype = 3), panel.grid.major = element_line(colour = "#00000000", linetype = 3)) + scale_y_continuous(expand = c(0.01,0.01)) + labs(color = "Country")

quartz(width = 10, height = 10)
ggplot(dat.plot, (aes(x = times, y = rates, color = prov))) + geom_line() + facet_wrap(~prov) + scale_color_manual(values = cols) + xlab("2020") + ylab("COVID19 growth rate (country-level)") + theme(legend.position = "none", legend.key = element_rect(fill = "#f0f0f0"), legend.background = element_rect(fill = "#ffffffaa", colour = "black"), panel.background = element_rect(fill = "white", colour = "black"), axis.text.y = element_text(colour = "black", size = 14), axis.text.x = element_text(colour = "black", size = 10), axis.title = element_text(colour = "black", size = 16), panel.grid.minor = element_line(colour = "#00000000",linetype = 3), panel.grid.major = element_line(colour = "#00000000", linetype = 3)) + scale_y_continuous(expand = c(0.01,0.01)) + labs(color = "Country")
