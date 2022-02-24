# library(tidyverse)
# library(R6)
#Player Class
Player_2 <- R6Class("Player",
                  public = list(
                    type = NULL,     #玩家類型
                    setting = NULL,
                    anchoring = NULL,
                    anchortrial = NA,
                    cash = NULL,     #現金
                    stock = NULL,    #股票
                    value = NULL,    #股票價值
                    asset = NULL,    #總資產
                    decision = NULL, #決策
                    total = NA,      #總回合數
                    herdb = NA,
                    herds = NA,
                    inversiveb = NA,
                    inversives = NA,
                    nobuy = NA,      #N期後降低買股比率
                    # 初始化變項
                    initialize = function(cash,stock,type,setting,anchoring,anchortrial,total,herdb,herds,inversiveb,inversives,nobuy){
                      # 初始值設定
                      self$cash = vector("numeric", total)
                      self$stock = vector("numeric", total)
                      self$value = vector("numeric", total)
                      self$asset = vector("numeric", total)
                      self$decision = vector("character", total)
                      self$type = type
                      self$setting = setting
                      self$anchoring = anchoring
                      self$anchortrial = anchortrial
                      self$herdb = herdb
                      self$herds = herds
                      self$inversiveb = inversiveb
                      self$inversives = inversives
                      self$nobuy = nobuy
                      self$cash[1] = cash 
                      self$stock[1] = stock
                      self$value[1] = stock * 100
                      self$asset[1] = cash + stock * 100
                      self$decision[101] = "N"
                      self$decision[101] = "N"
                    },
                    # 決策
                    decide = function(Market){
                      # 價格波動變化
                      ## Noise
                      if (self$type == "Noise"){
                        Prob = rmultinom(1, size = 1, prob = c(0.3, 0.3, 0.3))
                      } else {
                        # anchoring
                        fairh = 1
                        fairl = 1
                        if(self$anchoring){
                          if(Market$trial <= self$anchortrial+1){
                            fair = 100
                          }
                          else {
                            s = Market$trial-self$anchortrial  # start
                            e = Market$trial-1   # end
                            fair = mean(Market$price[s:e])
                          }
                          if(Market$price[Market$trial] > fair){
                            fairh = 0.8
                          } else {
                            fairl = 0.8
                          }
                        }
                        ## probability
                        if(self$setting == "probability"){
                          if(Market$dprice[Market$trial] > 0){
                            Prob = switch(self$type,
                                          Herd=     {rmultinom(1, size = 1, prob = c(self$herdb*fairh+(1-self$herds)*(1-fairl),self$herds-self$herdb,(1-self$herds)*fairl+self$herdb*(1-fairh)))},
                                          Inversive={rmultinom(1, size = 1, prob = c((1-self$inversives)*fairh+self$inversiveb*(1-fairl),self$inversives-self$inversiveb,self$inversiveb*fairl+(1-self$inversives)*(1-fairh)))},
                                          Hedge=    {rmultinom(1, size = 1, prob = c(0.1*fairh+0.7*(1-fairl),0.2,0.7*fairl+0.1*(1-fairh)))}) 
                          } else if(Market$dprice[Market$trial] == 0){
                            Prob = switch(self$type,
                                          Herd=     {rmultinom(1, size = 1, prob = c(1/3*(fairh+1-fairl),1/3,1/3*(fairl+1-fairh)))},
                                          Inversive={rmultinom(1, size = 1, prob = c(1/3*(fairh+1-fairl),1/3,1/3*(fairl+1-fairh)))},
                                          Hedge=    {rmultinom(1, size = 1, prob = c(0.4*fairh+0.2*(1-fairl),0.4,0.2*fairl+0.4*(1-fairh)))}) 
                          } else{
                            Prob = switch(self$type,
                                          Herd=     {rmultinom(1, size = 1, prob = c((1-self$herds)*fairh,self$herds-self$herdb,self$herdb*fairl))},
                                          Inversive={rmultinom(1, size = 1, prob = c(self$inversiveb*fairh,self$inversives-self$inversiveb,(1-self$inversives)*fairl))},
                                          Hedge=    {rmultinom(1, size = 1, prob = c(0.45*fairh+0.1*(1-fairl),0.45,0.1*fairl+0.45*(1-fairh)))})
                          }
                        } else {
                          ## internal goal
                          if(Market$trial == 1){
                            Prob = rmultinom(1, size = 1, prob = c(0.4, 0.2, 0.4))
                          } else {
                            probasset = fairh
                            probcash  = fairl 
                            # 60期後買股意願下降
                            if (Market$trial >= self$nobuy){
                              probasset = probasset * (0.3+0.00000028*(Market$trial-100)^4)
                              probcash = probcash + probasset * (1-(0.3+0.00000028*(Market$trial-100)^4))
                            }
                            # 價格變動影響
                            if(Market$dprice[Market$trial]/Market$price[Market$trial-1] > 0.061){
                              ins1 = 90
                              ins2 = 10
                            } else if(Market$dprice[Market$trial]/Market$price[Market$trial-1] > 0.031){
                              ins1 = 85
                              ins2 = 15
                            } else if(Market$dprice[Market$trial]/Market$price[Market$trial-1] > 0){
                              ins1 = 80
                              ins2 = 20
                            } else if(Market$dprice[Market$trial]/Market$price[Market$trial-1] == 0){
                              ins1 = 50
                              ins2 = 50
                            } else if(Market$dprice[Market$trial]/Market$price[Market$trial-1] > -0.031){
                              ins1 = 20
                              ins2 = 80
                            } else if(Market$dprice[Market$trial]/Market$price[Market$trial-1] > -0.061){
                              ins1 = 15
                              ins2 = 85
                            } else {
                              ins1 = 10
                              ins2 = 90
                            }
                            
                            # BNS機率
                            ps = switch(self$type,
                                        Herd=probcash*ins2,
                                        Inversive=probcash*ins1,
                                        Hedge=probcash*ins1)
                            pb = switch(self$type,
                                        Herd=probasset*ins1,
                                        Inversive=probasset*ins2,
                                        Hedge=0.7*probasset*ins2)
                            pn = max(100 - ps - pb, 0)
                            Prob = rmultinom(1, size = 1, prob = c(pb,pn,ps))
                            }
                          }
                      }
                      # 判斷變動
                      if(Prob[1,1]){
                        if(self$cash[Market$trial] > Market$price[Market$trial]){
                          self$decision[Market$trial] = "B"
                          self$cash[Market$trial+1] = self$cash[Market$trial] - Market$price[Market$trial]
                          self$stock[Market$trial+1] = self$stock[Market$trial] + 1
                        } else {
                          self$decision[Market$trial] = "N"
                          self$cash[Market$trial+1] = self$cash[Market$trial]
                          self$stock[Market$trial+1] = self$stock[Market$trial]
                        }
                      } else if(Prob[2,1]){
                        self$decision[Market$trial] = "N"
                        self$cash[Market$trial+1] = self$cash[Market$trial]
                        self$stock[Market$trial+1] = self$stock[Market$trial]
                      } else {
                        if(self$stock[Market$trial] > 0){
                          self$decision[Market$trial] = "S"
                          self$cash[Market$trial+1] = self$cash[Market$trial] + Market$price[Market$trial]
                          self$stock[Market$trial+1] = self$stock[Market$trial] - 1
                        }  else {
                          self$decision[Market$trial] = "N"
                          self$cash[Market$trial+1] = self$cash[Market$trial]
                          self$stock[Market$trial+1] = self$stock[Market$trial]
                        }
                      }
                    },
                    # 結算
                    ending = function(Market){
                      self$value[Market$trial] = self$stock[Market$trial-1] * Market$price[Market$trial-1]
                      self$asset[Market$trial] = self$cash[Market$trial-1] + self$value[Market$trial-1]
                    },
                    
                    lock_objects = F
                  ))