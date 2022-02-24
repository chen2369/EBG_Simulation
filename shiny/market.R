# library(tidyverse)
# library(R6)
# Market Class
Market <- R6Class("Market",
                  public = list(
                    total = NA,     #總回合數
                    price = NULL,   #價格
                    dprice = NULL,  #價格波動
                    trial = 1,      #當前回合
                    mrow = NA,      #P1抉擇
                    mcol = NA,      #P2抉擇
                    change = NA,    #漲跌幅
                    board = matrix(c(0.05, 0.03, 0, 0.03, 0, -0.03, 0, -0.03, -0.05), nrow = 3, ncol = 3),  #漲跌幅陣列
                    # 初始化變項
                    initialize = function(total){
                      stopifnot(is.numeric(total), length(total) == 1)
                      self$total <- total
                      self$price <- vector("numeric", self$total+1)
                      self$dprice <- vector("numeric", self$total+1)
                      self$price[1] <- 100
                      self$dprice[1] <- 0
                    },
                    # 市場情況
                    condition = function(con){
                      self$board = switch (con,
                                           Balance={matrix(c(0.05, 0.03, 0, 0.03, 0, -0.03, 0, -0.03, -0.05), nrow = 3, ncol = 3)},
                                           Bubble={matrix(c(0.10, 0.06, 0, 0.06, 0, -0.03, 0, -0.03, -0.05), nrow = 3, ncol = 3)},
                                           Burst={matrix(c(0.05, 0.03, 0, 0.03, 0, -0.06, 0, -0.06, -0.10), nrow = 3, ncol = 3)})
                    },
                    # 遊戲進行
                    game = function(p1act, p2act){
                      self$mrow = switch(p1act, B={1}, N={2}, S={3})
                      self$mcol = switch(p2act, B={1}, N={2}, S={3})
                      self$change = self$board[self$mrow, self$mcol]
                      self$dprice[self$trial+1] = self$price[self$trial] * self$change
                      self$price[self$trial+1] = self$price[self$trial] + self$dprice[self$trial+1]
                      self$trial = self$trial + 1
                    },
                    lock_objects = F
                  )
                )