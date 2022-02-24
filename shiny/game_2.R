Game_2 <- R6Class("Game",
        public = list(
          setting = NULL, # probability or internal gaol
          anchoring = NULL,
          anchortrial = NA, # 考慮期數
          agent_amount = NA, # 玩家數量
          n_herd = NA,
          n_inversive = NA,
          n_hedge = NA,
          n_noise = NA,
          up_down_s = NA, # 小波動
          up_down_l = NA, # 大波動
          cash = NA,
          stock = NA,
          herdb = NA,
          herds = NA,
          inversiveb = NA,
          inversives = NA,
          nobuy = NA,
          game = NULL,
          players = NULL,
          fst_con = NULL,
          snd_con = NULL,
          trd_con = NULL,
          market = NULL,
          simulation_data = NULL,
          # 初始化變項
          initialize = function(agent_amount, herd_ratio, inversive_ratio, hedge_ratio, noise_ratio, up_down1, up_down2,
                                herdb, herds, inversiveb, inversives, setting, anchoring, anchortrial, fst_con, snd_con, trd_con, nobuy){
            self$agent_amount = agent_amount
            t = herd_ratio + inversive_ratio + hedge_ratio + noise_ratio
            self$n_herd = round(herd_ratio/t * agent_amount, 0)
            self$n_inversive = round(inversive_ratio/t * agent_amount, 0)
            self$n_hedge = round(hedge_ratio/t * agent_amount, 0)
            self$n_noise = agent_amount - self$n_herd - self$n_inversive - self$n_hedge
            self$up_down_s = up_down1
            self$up_down_l = up_down2
            self$cash = 10000
            self$stock = 10
            self$herdb = herdb
            self$herds = herds
            self$inversiveb = inversiveb
            self$inversives = inversives
            self$setting = setting
            self$anchoring = anchoring
            self$anchortrial = anchortrial
            self$fst_con = fst_con
            self$snd_con = snd_con
            self$trd_con = trd_con
            self$nobuy = nobuy
          },
          # 模擬開始
          playing = function(x){
            # 建立玩家 & 市場
            playersTypes <- rep(c("Herd", "Inversive", "Hedge", "Noise"),
                                c(self$n_herd, self$n_inversive, self$n_hedge, self$n_noise))
            self$players <- lapply(playersTypes, function(playerType){
              Player_2$new(self$cash, self$stock, playerType, self$setting, self$anchoring,
                         self$anchortrial, 100, self$herdb, self$herds, self$inversiveb,
                         self$inversives, self$nobuy)
            })
            self$market = Market_2$new(100, self$up_down_s, self$up_down_l)
            
            # 每回合
            for (i in 1:self$market$total){
              # 所有玩家決策 
              for (j in 1:self$agent_amount){
                self$players[[j]]$decide(self$market)
              }
              
              # 市場情況
              if(i <= 20){
                self$market$condition(self$fst_con)
              } else if (i <= 60){
                self$market$condition(self$snd_con)
              } else {
                self$market$condition(self$trd_con)
              }
              # 獲得所有玩家本回合決策
              self$market$game(self$getPlayersData(self$players, "decision", self$market$trial))
              
              # 結算玩家情況
              for (k in 1:self$agent_amount) {
                self$players[[k]]$ending(self$market)
              }
            }
            
            data <- list(times = x, trials = 1:101,
                         price = self$market$price, deltaPrice = self$market$dprice)
                         # p1_cash = self$P1$cash, p2_cash = self$P2$cash,
                         # p1_stock = self$P1$stock, p2_stock = self$P2$stock,
                         # p1_value = self$P1$value, p2_value = self$P2$value,
                         # p1_asset = self$P1$asset, p2_asset = self$P2$asset,
                         # p1_decision = self$P1$decision, p2_decision = self$P2$decision)
            return(data)
          },
          
          # 取得玩家資訊
          getPlayersData = function(Players, data, trial){
            
            playerdata <- lapply(Players, function(player){
              if(length(player[[data]]) > 1){
                player[[data]][trial]
              } else{
                player[[data]][1]
              }
            })
            return(playerdata)
          },
          
          # 模擬sim_times次
          simulate = function(sim_times){
            self$simulation_data <- sapply(1:sim_times, self$playing, simplify = FALSE, USE.NAMES = TRUE)
          },
          
          lock_objects = F
        ))