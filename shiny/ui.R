# loading all the resource
source("global.R")
source("global_2.R")

# Define UI for application that draws a histogram
shinyUI(fluidPage
  (navbarPage(
    
    title = "Simulation of EBG",
    
    tabPanel(title = "Two Players",
      # Sidebar with a slider input for number of bins
      sidebarLayout(
        
        sidebarPanel(
          
          h3("Simulation setting"),

          selectInput("p1_type", "Player1 type:", 
                      choices = typelist, selected = "Herd"),
          selectInput("p1_setting", "Player1 setting:", 
                      choices = settinglist, selected = "probability"),
          checkboxInput("p1_anchoring", "Player1 anchoring",
                        value = FALSE),
          selectInput("p2_type", "Player2 type:", 
                      choices = typelist, selected = "Inversive"),
          selectInput("p2_setting", "Player2 setting:", 
                      choices = settinglist, selected = "probability"),
          checkboxInput("p2_anchoring", "Player2 anchoring",
                        value = FALSE),
          sliderInput("anchortrial", "考慮期數: ",
                      min = 1, max = 10, value = 3, step = 1),
          sliderInput("sim_times", "Simulation times",
                      min = 10, max = 300, value = 50, step = 10),
          sliderInput("cash_amount", "Cash amount",
                      min = 5000, max = 50000, value = 10000, step = 1000),
          sliderInput("stock_amount", "Stock amount",
                      min = 0, max = 100, value = 10, step = 10),
          sliderInput("Herd_prob", "Herd B/N/S(以漲為例，跌則相反)",
                      min = 0, max = 1, value = c(0.7,0.9), step = 0.05),
          sliderInput("Inversive_prob", "Inversive S/N/B(以漲為例，跌則相反)",
                      min = 0, max = 1, value = c(0.7,0.9), step = 0.05),
          sliderInput("down_ratio", "對方不動作時降低行動之比率",
                      min = 0, max = 0.85, value = 0.7, step = 0.05),
          sliderInput("no_buy", "N期後降低買股比率",
                      min = 0, max = 100, value = 60, step = 5),
          selectInput("fst_con", "1-20期市場條件", 
                      choices = rulelist, selected = "Balance"),
          selectInput("snd_con", "21-60期市場條件", 
                      choices = rulelist, selected = "Bubble"),
          selectInput("trd_con", "61-100期市場條件", 
                      choices = rulelist, selected = "Burst"),
          actionButton("run", "計算"),
          hr(),
          h3("Result"),
          sliderInput("trial_range", "Trial range",
                      min = 1, max = 101, value = c(1, 101)),
          actionButton("update", "畫面更新"),

          width = 2
        ),
        
        # Show a plot of the generated distribution
        mainPanel(
          
          textOutput('text1'),
          textOutput('text2'),
          textOutput('text3'),
          
          tags$head(
            tags$style("#text1{
                          font-size: 30px;
                          font-weight: 700;
                          }
                       #text2, #text3{
                       font-size: 20px;
                       font-weight: 500;
                       }"
                      )
          ),
          
          plotOutput("price_plot"),
          plotOutput("maxtime_plot"),
          fluidRow(
            column(4, gt_output("int_interval_table")),
            column(4, plotOutput("maxprice_plot")),
            column(4, gt_output("winRate_table"),
                      br(),
                      gt_output("bns_table"))
          ),
          plotOutput("cash_plot"),
          plotOutput("stock_plot"),

          width = 10
        )
      )  # sidebarlayout
    ),  
    
    tabPanel(title = "MultiPlayers",
               # Sidebar with a slider input for number of bins
               sidebarLayout(
                 sidebarPanel(
    
                   h3("Simulation setting"),
                   
                   sliderInput("agent_amount_2", "agent數",
                               min = 10, max = 200, value = 100, step = 10),
                   sliderInput("herd_ratio_2", "herd比率",
                               min = 0, max = 1, value = 0.5, step = 0.01),
                   sliderInput("inversive_ratio_2", "inversive比率",
                               min = 0, max = 1, value = 0.15, step = 0.01),
                   sliderInput("hedge_ratio_2", "hedge比率",
                               min = 0, max = 1, value = 0.05, step = 0.01),
                   sliderInput("noise_ratio_2", "noise比率",
                               min = 0, max = 1, value = 0.3, step = 0.01),
                   sliderInput("sim_times_2", "Simulation times",
                               min = 1, max = 50, value = 10, step = 1),
                   sliderInput("up_down_2", "漲跌幅標準(左為小波動、右為大波動)",
                               min = 0, max = 1, value = c(0.2,0.7), step = 0.05),
                   sliderInput("Herd_prob_2", "Herd B/N/S(以漲為例，跌則相反)",
                               min = 0, max = 1, value = c(0.7,0.9), step = 0.05),
                   sliderInput("Inversive_prob_2", "Inversive S/N/B(以漲為例，跌則相反)",
                               min = 0, max = 1, value = c(0.7,0.9), step = 0.05),
                   selectInput("setting_2", "Player setting:", 
                               choices = settinglist, selected = "probability"),
                   checkboxInput("anchoring_2", "anchoring",
                                 value = FALSE),
                   sliderInput("anchortrial_2", "合理價格考慮期數",
                               min = 1, max = 10, value = 3, step = 1),
                   sliderInput("no_buy_2", "N期後降低買股比率",
                               min = 0, max = 100, value = 60, step = 5),
                   selectInput("fst_con_2", "1-20期市場條件", 
                               choices = rulelist, selected = "Balance"),
                   selectInput("snd_con_2", "21-60期市場條件", 
                               choices = rulelist, selected = "Bubble"),
                   selectInput("trd_con_2", "61-100期市場條件", 
                               choices = rulelist, selected = "Burst"),
                   actionButton("run_2", "計算"),
                   hr(),
                   h3("Result"),
                   sliderInput("trial_range_2", "Trial range",
                               min = 1, max = 101, value = c(1, 101)),
                   actionButton("update_2", "畫面更新"),

                   width = 2
                 ),
                 
                 # Show a plot of the generated distribution
                 mainPanel(
                   
                   plotOutput("price_plot_2"),
                   
                   width = 10
                 )
               )  # sidebarlayout
      )
    )  # navbar
))  # fluid & shiny