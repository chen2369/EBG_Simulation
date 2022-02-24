library(shiny)
library(tidyverse)
library(R6)
library(reshape2)
library(gt)
library(gghighlight)
source("market_2.R")
source("player_2.R")
source("game_2.R")


# for ui.R
# 4種type
ratiolist <- c("90 / 85 / 80 / 50", "100 / 85 / 70 / 50")
typelist <- c("Herd", "Inversive", "Hedge", "Noise", "Best")
settinglist <- c("probability", "internal goal")
rulelist <- c("Balance", "Bubble", "Burst")

# library(rsconnect)
# rsconnect::deployApp("D:/文件/大學/Lab/EBG Sim/shiny")
