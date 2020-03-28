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

cols <- wes_palette(name = "Rushmore1", n = length(unique(plot.df$County)), type = "continuous")

quartz(width = 10, height = 6)
ggplot(plot.df, aes(x = Date, y = Cumulative, color = County)) + geom_line(size = 1.2) + scale_color_manual(values = cols) + xlab("2020") + ylab("COVID19 cases (cumulative)") + theme(legend.position = "right", legend.key = element_rect(fill = "#f0f0f0"), legend.background = element_rect(fill = "#ffffffaa", colour = "black"), panel.background = element_rect(fill = "white", colour = "black"), axis.text.y = element_text(colour = "black", size = 14), axis.text.x = element_text(colour = "black", size = 10), axis.title = element_text(colour = "black", size = 16), panel.grid.minor = element_line(colour = "#00000000",linetype = 3), panel.grid.major = element_line(colour = "#00000000", linetype = 3)) + scale_y_continuous(trans = log_trans(), limits = c(1, 1000)) + labs(color = "MA County")