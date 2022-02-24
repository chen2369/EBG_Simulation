library(shiny)
library(tidyverse)
library(R6)
library(reshape2)
library(gt)
library(gghighlight)
source("market.R")
source("player.R")
source("game.R")



# for ui.R
# 4種type
ratiolist <- c("90 / 85 / 80 / 50", "100 / 85 / 70 / 50")
typelist <- c("Herd", "Inversive", "Hedge", "Noise", "Best")
settinglist <- c("probability", "internal goal")
rulelist <- c("Balance", "Bubble", "Burst")

# upload to shiny
# library(rsconnect)
# rsconnect::setAccountInfo(name='chenebg',
#                           token='D8BE70F42A78CEA94AB02F30A9587722',
#                           secret='im/zLyArLibgSOnOt05cLG9pIcqOSdNauKNZwIXY')
# rsconnect::deployApp("D:/Files/College/Lab/EBG Sim/shiny")
