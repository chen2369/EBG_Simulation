# library(tidyverse)
# library(R6)
# Market Class
Market_2 <- R6Class("Market",
                  public = list(
                    total = NA,     #總回合數
                    up_down_s = NA,
                    up_down_l = NA,
                    price = NULL,   #價格
                    dprice = NULL,  #價格波動
                    trial = 1,      #當前回合
                    board = matrix(c(0.05, 0.03, 0, 0.03, 0, -0.03, 0, -0.03, -0.05), nrow = 3, ncol = 3),  #漲跌幅陣列
                    # 初始化變項
                    initialize = function(total, up_down_s, up_down_l){
                      self$total <- total
                      self$up_down_s <- up_down_s
                      self$up_down_l <- up_down_l
                      self$price <- vector("numeric", self$total+1)
                      self$dprice <- vector("numeric", self$total+1)
                      self$price[1] <- 100
                      self$dprice[1] <- 0
                    },
                    # 市場情況
                    condition = function(con){
                      self$board = switch (con,
                                           Balance={c(0.05, 0.03, 0, -0.03, -0.05)},
                                           Bubble ={c(0.10, 0.06, 0, -0.03, -0.05)},
                                           Burst  ={c(0.05, 0.03, 0, -0.06, -0.10)})
                    },
                    # 遊戲進行
                    game = function(decisiondata){
                      buyvol <- sum(decisiondata == "B")
                      sellvol <- sum(decisiondata == "S")
                      net = (buyvol - sellvol) / (buyvol + sellvol)
                      if (net > self$up_down_l){
                          change = self$board[1]
                      } else if (net > self$up_down_s){
                          change = self$board[2]
                      } else if (net > self$up_down_s * -1){
                          change = self$board[3]
                      } else if (net > self$up_down_l * -1){
                          change = self$board[4]
                      } else {
                          change = self$board[5]
                      }
                      
                      self$dprice[self$trial+1] = self$price[self$trial] * change
                      self$price[self$trial+1] = self$price[self$trial] + self$dprice[self$trial+1]
                      self$trial = self$trial + 1
                    },
                    
                    lock_objects = F
                  )
                )