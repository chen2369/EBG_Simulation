Game <- R6Class("Game",
        public = list(
          times = NA,
          p1type = NULL,
          p2type = NULL,
          p1setting = NULL,
          p2setting = NULL,
          p1anchoring = NULL,
          p2anchoring = NULL,
          anchortrial = NA,
          cash = NA,
          stock = NA,
          herdb = NA,
          herds = NA,
          inversiveb = NA,
          inversives = NA,
          downratio = NA,
          nobuy = NA,
          game = NULL,
          P1 = NULL,
          P2 = NULL,
          fst_con = NULL,
          snd_con = NULL,
          trd_con = NULL,
          simulation_data = NULL,
          thing = c("Price", "Dprice", "P1Cash", "P2Cash", "P1Stock", "P2Stock", "P1Value", "P2Value", "P1Asset", "P2Asset"),
          market = Market$new(total=100),
          # 初始化變項
          initialize = function(P1type, P2type, P1setting, P2setting, P1anchoring, P2anchoring, anchortrial, Cash, Stock,
                                herdb, herds, inversiveb, inversives, fst_con, snd_con, trd_con, downratio, nobuy){
            self$p1type = P1type
            self$p2type = P2type
            self$p1setting = P1setting
            self$p2setting = P2setting
            self$p1anchoring = P1anchoring
            self$p2anchoring = P2anchoring
            self$anchortrial = anchortrial
            self$cash = Cash
            self$stock = Stock
            self$herdb = herdb
            self$herds = herds
            self$inversiveb = inversiveb
            self$inversives = inversives
            self$fst_con = fst_con
            self$snd_con = snd_con
            self$trd_con = trd_con
            self$downratio = downratio
            self$nobuy = nobuy
          },
          playing = function(x){
            self$P1 <- Player$new(1,self$cash,self$stock,self$p1type,self$p1setting,self$p1anchoring,self$anchortrial,100,self$herdb,self$herds,self$inversiveb,self$inversives,self$downratio,self$nobuy)
            self$P2 <- Player$new(2,self$cash,self$stock,self$p2type,self$p2setting,self$p2anchoring,self$anchortrial,100,self$herdb,self$herds,self$inversiveb,self$inversives,self$downratio,self$nobuy)
            self$market = Market$new(total=100)                    
            for (i in 1:self$market$total) {
              self$P1$decide(self$market)
              self$P2$decide(self$market)
              if(i <= 20){
                self$market$condition(self$fst_con)
              } else if (i <= 60){
                self$market$condition(self$snd_con)
              } else {
                self$market$condition(self$trd_con)
              }
              self$market$game(self$P1$decision[i],self$P2$decision[i])
              self$P1$ending(self$market)
              self$P2$ending(self$market)
            }
            data <- list(times = x, trials = 1:101,
                         price = self$market$price, deltaPrice = self$market$dprice, 
                         p1_cash = self$P1$cash, p2_cash = self$P2$cash,
                         p1_stock = self$P1$stock, p2_stock = self$P2$stock,
                         p1_value = self$P1$value, p2_value = self$P2$value,
                         p1_asset = self$P1$asset, p2_asset = self$P2$asset,
                         p1_decision = self$P1$decision, p2_decision = self$P2$decision)
            return(data)
          },
          simulate = function(sim_times){
            self$simulation_data <- sapply(1:sim_times, self$playing, simplify = FALSE, USE.NAMES = TRUE)
          },
          lock_objects = F
        ))

