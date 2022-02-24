library(writexl)
# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  ## Simulation 
  simulation <- reactiveValues(data = NULL)
  simulation2 <- reactiveValues(data = NULL)
  
  #-----------------------twoplayer simulation--------------------------------------------------------------#
  

  observeEvent(input$run, {
    showNotification("Simulating ...", duration = NULL, id = "sim")
  
    game <- Game$new(input$p1_type, input$p2_type, input$p1_setting, input$p2_setting, input$p1_anchoring, input$p2_anchoring, input$anchortrial, input$cash_amount, input$stock_amount,
                     input$Herd_prob[1], input$Herd_prob[2], input$Inversive_prob[1], input$Inversive_prob[2],
                     input$fst_con, input$snd_con, input$trd_con, input$down_ratio, input$no_buy)
    
    set.seed(408)
    game$simulate(input$sim_times)
    removeNotification(id = "sim")
    showNotification("DONE!", duration = 3)
    
    simulation$data <- game$simulation_data
  })
  # Result
  
  output$text1 <- renderText({ 
    paste0("player: ",input$p1_type, "(", input$p1_setting, ") VS ", input$p2_type, "(", input$p2_setting, ")\n")
    })
  output$text2 <- renderText({ 
    paste0("simulation times: ", input$sim_times, ", Cash: ", input$cash_amount, ", stock: ", input$stock_amount)
    
    })
  output$text3 <- renderText({ 
    paste0("anchoring: p1(", input$p1_anchoring, "), p2(", input$p2_anchoring, ")")
    })
  
  ### Price data
  price_data <- eventReactive(input$update,{
    .data <- lapply(simulation$data, function(x){
      x <- as_tibble(x) %>% 
        select(times, trials, price) %>%
        filter(trials >= input$trial_range[1], trials <= input$trial_range[2]) %>% 
        mutate(maxp = which.max(price))
    })
    return(.data)
  })
  
  ### Price plot
  output$price_plot <- renderPlot({
    g <- do.call(rbind, price_data())
    
    outputdata <- g %>% 
      mutate(type = paste0(input$p1_type, "_", input$p2_type)) %>% 
      select(type, times, trials, price) %>% 
      pivot_wider(names_from = trials, values_from = price)
        
    write_xlsx(outputdata, paste0(input$p1_type, "_", input$p2_type, ".xlsx"))
    
    g_wider <- g %>% 
      pivot_wider(id_cols = trials, 
                  names_from = times,
                  names_prefix = "sim",
                  values_from = price)
    g_wider <- g_wider %>% 
      mutate(mean = rowMeans(g_wider %>% select(-trials)))
    
    write_xlsx(g_wider %>% select(-mean), paste0(input$p1_type, "_", input$p2_type, "_wider.xlsx"))
    
    q1 <- c()
    q2 <- c()
    q3 <- c()
    for (i in 1:101) {
      q1 <- append(q1, quantile(g_wider[i,] %>% select(-trials), 1/4)[[1]])
      q2 <- append(q2, quantile(g_wider[i,] %>% select(-trials), 2/4)[[1]])
      q3 <- append(q3, quantile(g_wider[i,] %>% select(-trials), 3/4)[[1]])
    }
    g_wider$one = q1
    g_wider$two = q2
    g_wider$thr = q3
    
    g <- g %>% 
      inner_join(g_wider %>% select(trials, mean, one, two, thr), by="trials") %>% 
      # left_join(.s %>% select(trials, mean, sd), by="trials") %>% 
      ggplot(aes(x = trials, y = price, group = times)) +
      # geom_line(aes(x = trials, y = mean), alpha = I(0.75), color = "red") +
      # geom_line(aes(x = trials, y = mean + sd), color = "blue") +
      # geom_line(aes(x = trials, y = mean - sd), color = "blue") +
      geom_line(alpha = I(0.75), color = "grey") +
      geom_line(aes(x = trials, y = mean), color = "red") +
      geom_line(aes(x = trials, y = one), color = "blue") +
      geom_line(aes(x = trials, y = two), color = "blue") +
      geom_line(aes(x = trials, y = thr), color = "blue") +
      coord_cartesian(xlim=c(0,100), ylim=c(0, 350)) +
      scale_x_continuous(breaks = c(0,20,40,60,80,100)) +
      theme_bw() +
      theme(axis.text=element_text(size=14),
            axis.title=element_text(size=16, face = "bold"))
    g
  })

  ### interval price data
  output$int_interval_table <- render_gt({
    g <- do.call(rbind, price_data())
    g_wider <- g %>% 
      pivot_wider(id_cols = trials, 
                  names_from = times,
                  names_prefix = "sim",
                  values_from = price)
    g_wider <- g_wider %>% 
      mutate(mean = rowMeans(g_wider %>% select(-trials)))
    q1 <- c()
    q2 <- c()
    q3 <- c()
    for (i in 1:101) {
      q1 <- append(q1, quantile(g_wider[i,] %>% select(-trials), 1/4)[[1]])
      q2 <- append(q2, quantile(g_wider[i,] %>% select(-trials), 2/4)[[1]])
      q3 <- append(q3, quantile(g_wider[i,] %>% select(-trials), 3/4)[[1]])
    }
    g_wider$one = q1
    g_wider$two = q2
    g_wider$thr = q3
    a <- g %>% 
      inner_join(g_wider %>% select(trials, mean, one, two, thr), by="trials") %>% 
      mutate(interval = ceiling(trials/20)) %>%
      filter(interval <= 5) %>% 
      group_by(interval) %>% 
      summarise(`q1`= round(mean(one),0),
                `q2`= round(mean(two),0),
                `q3`= round(mean(thr),0),
                mean = round(mean(mean),0)) %>% 
      select(-interval) %>% 
      ungroup()
    as.data.frame(t(a)) %>% 
      rename("1~20"="V1","21~40"="V2","41~60"="V3","61~80"="V4","81~100"="V5") %>% 
      mutate(quartile = c("q1","q2","q3","mean")) %>% 
      select(quartile, everything()) %>% 
      gt() %>% 
      tab_options(table.font.size = pct(125)) %>% 
      tab_header(title = "Quartile")
  })
  
  # maxprice data
  maxprice_data <- eventReactive(input$update, {
    max_list <- lapply(simulation$data, function(x){
      .x <- as_tibble(x) %>% 
        select(times, trials, price) %>%
        mutate(maxp = which.max(price)) %>% 
        filter(trials == maxp)
    })
  })
  
  # maxprice time plot
  output$maxtime_plot <- renderPlot({
    g <- do.call(rbind, maxprice_data()) %>%
      ggplot(aes(x = maxp)) +
      geom_bar() +
      coord_cartesian(xlim=c(0,100)) +
      scale_x_continuous(breaks = c(0,20,40,60,80,100)) +
      theme_bw() +
      theme(axis.text=element_text(size=14),
            axis.title=element_text(size=16, face = "bold"))
    g
  })  
  
  # maxprice plot
  output$maxprice_plot <- renderPlot({
    g <- do.call(rbind, maxprice_data()) %>% 
      ggplot(aes(y = price)) +
      geom_boxplot(outlier.colour="red",
                   outlier.size=1.5) +
      xlim(-1,1) +
      theme_bw() +
      xlab("") +
      ggtitle("Maxprice plot") +
      theme(axis.text=element_text(size=14),
            axis.title=element_text(size=16, face = "bold"),
            title=element_text(size=18, face = "bold"))
    g
  })
  
  ### win rate data
  winRate_data <- eventReactive(input$update, {
    win_list <- lapply(simulation$data, function(x){
      .x <- as_tibble(x) %>% 
        select(times, trials, ends_with("cash")) %>% 
        filter(trials == 101) %>% 
        mutate(delta_cash = p1_cash - p2_cash)
    })
  })
  
  ### win rate table
  output$winRate_table <- render_gt({
    do.call(rbind, winRate_data()) %>% 
      summarise(p1_win = sum(delta_cash > 0),
                p1_tie = sum(delta_cash == 0), 
                p1_loss = sum(delta_cash < 0),
                p1_Rate = paste0(round(p1_win/n()*100,0), "%"),
                mean = round(mean(delta_cash), 0), 
                sd = round(sd(delta_cash), 0)) %>% 
      gt() %>% 
      tab_options(table.font.size = pct(125)) %>% 
      tab_header(title = "delta_cash = p1_cash - p2_cash")
  })
  
  # decision data
  bns_data <- eventReactive(input$update, {
    .data <- lapply(simulation$data, function(x){
      x <- as_tibble(x) %>% 
        select(times, trials, ends_with("decision")) %>% 
        filter(trials >= input$trial_range[1], trials <= input$trial_range[2])
    })
    return(.data)
  })
  
  # decision table
  output$bns_table <- render_gt({
    g <- do.call(rbind, bns_data()) 
    g %>% 
      pivot_wider(id_cols = trials, 
                  names_from = times,
                  names_prefix = "sim",
                  values_from = c(p1_decision, p2_decision)) %>% 
      write_xlsx(paste0(input$p1_type, "_", input$p2_type, "_choice.xlsx"))
    
    ss <- g %>% 
      filter(p1_decision == "S") %>% 
      filter(p2_decision == "S") %>% nrow()
    sn <- g %>% 
      filter(p1_decision == "S") %>% 
      filter(p2_decision == "N") %>% nrow()
    sb <- g %>% 
      filter(p1_decision == "S") %>% 
      filter(p2_decision == "B") %>% nrow()
    ns <- g %>% 
      filter(p1_decision == "N") %>% 
      filter(p2_decision == "S") %>% nrow()
    nn <- g %>% 
      filter(p1_decision == "N") %>% 
      filter(p2_decision == "N") %>% nrow()
    nb <- g %>% 
      filter(p1_decision == "N") %>% 
      filter(p2_decision == "B") %>% nrow()
    bs <- g %>% 
      filter(p1_decision == "B") %>% 
      filter(p2_decision == "S") %>% nrow()
    bn <- g %>% 
      filter(p1_decision == "B") %>% 
      filter(p2_decision == "N") %>% nrow()
    bb <- g %>% 
      filter(p1_decision == "B") %>% 
      filter(p2_decision == "B") %>% nrow()
    tot <- bb+nb+sb+bn+nn+sn+bs+ns+ss
    data.frame("player1/2"=c("Buy","NoTrade", "Sell", "Total"),
               "Buy"=round(c(bb/tot, nb/tot, sb/tot, (bb+nb+sb)/tot), 3)*100,
               "NoTrade"=round(c(bn/tot, nn/tot, sn/tot, (bn+nn+sn)/tot), 3)*100,
               "Sell"=round(c(bs/tot, ns/tot, ss/tot, (bs+ns+ss)/tot), 3)*100,
               "Total"=round(c((bb+bn+bs)/tot,(nn+nb+ns)/tot,(sn+sb+ss)/tot,tot/tot), 3)*100) %>% 
      gt() %>%
      tab_options(table.font.size = pct(125)) %>% 
      tab_header(title = "Contingency table of B/N/S")
  })
  
  ## Player plot
  ### cash data
  cash_data <- eventReactive(input$update, {
    .data <- lapply(simulation$data, function(x){
      x <- as_tibble(x) %>% 
        select(times, trials, ends_with("cash")) %>%
        filter(trials >= input$trial_range[1], trials <= input$trial_range[2])
    })
    return(.data)
  })
  
  ### cash plot
  output$cash_plot <- renderPlot({
    g <- do.call(rbind, cash_data()) %>% 
      pivot_longer(cols = contains("_"), names_to = "player", values_to = "value") %>% 
      ggplot(aes(x = trials, y = value, group = times, color = player)) +
      geom_line(alpha = I(0.75)) +
      facet_grid(as.factor(player) ~ .) +
      theme_bw() +
      theme(legend.position = "bottom")
    g
  })
  
  ### stock data
  stock_data <- eventReactive(input$update, {
    .data <- lapply(simulation$data, function(x){
      x <- as_tibble(x) %>% 
        select(times, trials, ends_with("stock")) %>% 
        filter(trials >= input$trial_range[1], trials <= input$trial_range[2])
    })
    return(.data)
  })
  
  ### stock plot
  output$stock_plot <- renderPlot({
    
    do.call(rbind, stock_data()) %>% 
      pivot_wider(id_cols = trials, 
                  names_from = times,
                  names_prefix = "sim",
                  values_from = c(p1_stock, p2_stock)) %>% 
      write_xlsx(paste0(input$p1_type, "_", input$p2_type, "_stock.xlsx"))
    
    
    g <- do.call(rbind, stock_data()) %>% 
      pivot_longer(cols = contains("_"), names_to = "player", values_to = "value") %>% 
      ggplot(aes(x = trials, y = value, group = times, color = player)) +
      geom_line(alpha = I(0.75)) +
      facet_grid(as.factor(player) ~ .) +
      theme_bw() +
      theme(legend.position = "bottom")
    g
  })
  
  ### Asset plot
  # asset_data <- eventReactive(input$update, {
  #   .data <- lapply(simulation$data, function(x){
  #     x <- as_tibble(x) %>% 
  #       select(times, trials, ends_with("asset")) %>% 
  #       filter(trials >= input$trial_range[1], trials <= input$trial_range[2])
  #   })
  #   return(.data)
  # })
  # 
  # output$asset_plot <- renderPlot({
  #   g <- do.call(rbind, asset_data()) %>% 
  #     pivot_longer(cols = contains("_"), names_to = "player", values_to = "value") %>% 
  #     ggplot(aes(x = trials, y = value, group = times, color = player)) +
  #     geom_line(alpha = I(0.75)) +
  #     facet_grid(as.factor(player) ~ .) +
  #     theme_bw() +
  #     theme(legend.position = "bottom")
  #   g
  # })
  
  #-----------------------multiplayer simulation--------------------------------------------------------------#
  
  observeEvent(input$run_2, {
    showNotification("Simulating ...", duration = NULL, id = "sim_2")
    
    game <- Game_2$new(input$agent_amount_2, input$herd_ratio_2, input$inversive_ratio_2, input$hedge_ratio_2, input$noise_ratio_2, input$up_down_2[1], input$up_down_2[2],
                     input$Herd_prob_2[1], input$Herd_prob_2[2], input$Inversive_prob_2[1], input$Inversive_prob_2[2], input$setting_2, input$anchoring_2, input$anchortrial_2,
                     input$fst_con_2, input$snd_con_2, input$trd_con_2, input$no_buy_2)
    
    game$simulate(input$sim_times_2)
    removeNotification(id = "sim_2")
    showNotification("DONE!", duration = 3)
    simulation2$data <- game$simulation_data
  })
  ### Price data
  price_data_2 <- eventReactive(input$update_2,{
    .data <- lapply(simulation2$data, function(x){
      x <- as_tibble(x) %>% 
        select(times, trials, price) %>%
        filter(trials >= input$trial_range_2[1], trials <= input$trial_range_2[2]) %>% 
        mutate(maxp = which.max(price))
    })
    return(.data)
  })
  
  ### Price plot
  output$price_plot_2 <- renderPlot({
    g <- do.call(rbind, price_data_2())
    g_wider <- g %>% 
      pivot_wider(id_cols = trials, 
                  names_from = times,
                  names_prefix = "sim",
                  values_from = price)
    g_wider <- g_wider %>% 
      mutate(mean = rowMeans(g_wider %>% select(-trials)))
    q1 <- c()
    q2 <- c()
    q3 <- c()
    for (i in input$trial_range_2[1]:input$trial_range_2[2]) {
      q1 <- append(q1, quantile(g_wider[i,] %>% select(-trials), 1/4)[[1]])
      q2 <- append(q2, quantile(g_wider[i,] %>% select(-trials), 2/4)[[1]])
      q3 <- append(q3, quantile(g_wider[i,] %>% select(-trials), 3/4)[[1]])
    }
    g_wider$one = q1
    g_wider$two = q2
    g_wider$thr = q3
    
    g <- g %>% 
      inner_join(g_wider %>% select(trials, mean, one, two, thr), by="trials") %>% 
      # left_join(.s %>% select(trials, mean, sd), by="trials") %>% 
      ggplot(aes(x = trials, y = price, group = times)) +
      # geom_line(aes(x = trials, y = mean), alpha = I(0.75), color = "red") +
      # geom_line(aes(x = trials, y = mean + sd), color = "blue") +
      # geom_line(aes(x = trials, y = mean - sd), color = "blue") +
      geom_line(alpha = I(0.75), color = "grey") +
      geom_line(aes(x = trials, y = mean), color = "red") +
      geom_line(aes(x = trials, y = one), color = "blue") +
      geom_line(aes(x = trials, y = two), color = "blue") +
      geom_line(aes(x = trials, y = thr), color = "blue") +
      coord_cartesian(xlim=c(0,100), ylim=c(0, 350)) +
      scale_x_continuous(breaks = c(0,20,40,60,80,100)) +
      theme_bw() +
      theme(axis.text=element_text(size=14),
            axis.title=element_text(size=16, face = "bold"))
    g
  })
})