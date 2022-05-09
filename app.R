#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
rm(list=ls()) #clear global environment
library(shiny)
library(tidyr)
library(DT)
library(ggplot2)
library(dplyr)
library(shinyWidgets)
source("etl_functions.R")

safeway <- read.csv("datasets/safeway.csv")
walmart <- read.csv("datasets/walmart.csv")
s.market <- read.csv("datasets/streets_market.csv")


ui <- bootstrapPage(
  ######## CSS ######## 
    tags$head(
      tags$style(HTML("
    #weight {
      text-align: center;
      width: 100px;
    }
    #diet {
      text-align: center;
      width: 100px;
    }
  "))),
  navbarPage("Data Challenge",
    tabPanel("Compare Stores",  
      # Sidebar with a slider input for number of bins 
      sidebarLayout(
          sidebarPanel(
              fluidRow(
                column(6,h4("Desired Calories:"),
                       numericInput(inputId="cals", label=NULL, 5000, min = 2000, max = 50000)),
                column(2,offset=2,actionButton("sample_bakset", "Sample Basket"), 
                ),
              ),
              ######## User Input ######## 
              h4("Food Group Weighting:"),
              div(style="display:inline-block",id="weight", numericInput(label="Vegtables", inputId="w_veg",step=0.05,value = 0.2,min = 0,max=1 )),
              div(style="display:inline-block",id="weight",numericInput(label="Fruit", inputId="w_fruit",step=0.05, value = 0.2,min = 0,max=1)),
              br(),
              div(style="display:inline-block",id="weight",numericInput(label="Protein", inputId="w_protein",step=0.05, value = 0.2,min = 0,max=1)),
              div(style="display:inline-block",id="weight",numericInput(label="Dairy", inputId="w_dairy",step=0.05, value = 0.2,min = 0,max=1)),
              div(style="display:inline-block",id="weight",numericInput(label="Grain", inputId="w_grain",step=0.05, value = 0.2,min = 0,max=1)),
              verbatimTextOutput("warning"),
              
              ######## ANOVA Test ######## 
              h4("ANOVA Test Results"),
              strong("Determine whether store tiers influence grocery prices:", style = "font-family: 'times'; font-si16pt"),
              p("H0(null): The mean price is the same for Local (Streets Market), Regional (Safeway), and National (Walmart) stores.", style = "font-family: 'times'; font-si16pt"),
              p("H1(Alt): At least one pair of means are different from each other ", style = "font-family: 'times'; font-si16pt"),
              verbatimTextOutput("anovaTable"),
              br(),
              
              ######## T-Test on Fetched Basket ######## 
              h4("Lower Tailed Test Results"),
              strong("Walmart vs. Safeway", style = "font-family: 'times'; font-si16pt"),
              verbatimTextOutput("t_test_walmart.safeway"),
              
              strong("Walmart vs. Streets Market", style = "font-family: 'times'; font-si16pt"),
              verbatimTextOutput("t_test_walmart.streetsmarket"),
              
              strong("Safeway vs. Streets Market", style = "font-family: 'times'; font-si16pt"),
              verbatimTextOutput("t_test_safeway.streetsmarket"),
              
              width = 4
          ),
  
          mainPanel(
            ######## Dataframe of Fetched Basket - Box Plot ######## 
            h3("Sampled Basket"),
            fluidRow(
              column(DT::dataTableOutput("table"), width = 11)
            ),
            fluidRow(
              column(5,
                     h4("Summary Statistics"),
                     tableOutput("sum_total"),
                     verbatimTextOutput("boxplot_summary"),
              ),
              column(6,
                     plotOutput("boxplot_prices"),
              ),
            ),
            ######## Compare Diets - Regression Plots ######## 
            fluidRow(
              h3("Compare Diets"),
              column(5,
                     div(style = "display:inline-block; float:right", actionButton("refresh", "Refresh")),
                     br(),
                     div(style="display:inline-block",id="diet", checkboxInput("keto", "Keto", FALSE) ),
                     div(style="display:inline-block",id="diet",checkboxInput("vegan", "Vegan", FALSE)),
                     div(style="display:inline-block",id="diet",checkboxInput("carb", "High-Carb", FALSE)),
                     numericRangeInput(
                       inputId = "cal_range", label = "Calories Range",
                       value = c(10000, 30000)
                     ),
                     numericInput(inputId = "cal_step", label="Step", 5000, min = 2000, max = 10000),
                     h4("Coefficents:"),
                     verbatimTextOutput("lm_user.summary"),
                     verbatimTextOutput("lm_keto.summary"),
                     verbatimTextOutput("lm_vegan.summary"),
                     verbatimTextOutput("lm_carb.summary"),
              ),
              column(6,
                     plotOutput("linePlot"),
              ),
            )
          )
      )
    ),
    ######## Compare Products ######## 
    navbarMenu("Compare Products",
               
      ######## Raw vs Processed ######## 
      tabPanel("Raw vs Processed",
         sidebarLayout(
           sidebarPanel(
             h3("Lower Tailed Test Results"),
             p("Determine whether it is cheaper to purchase (and cook) raw products instead of purchasing the product in processed form:", style = "font-family: 'times'; font-si16pt"),
             p("H0(null): The mean difference in price between raw and processed products is equal or greater than 0. I.e., raw products are not less expensive than processed products", style = "font-family: 'times'; font-si16pt"),
             p("H1(Alt): The mean difference in price between raw and processed products is less than 0. I.e., raw products are less expensive than processed products", style = "font-family: 'times'; font-si16pt"),
             verbatimTextOutput("t_test.raw.processed"),
           ),
          
           mainPanel(
             h3("Raw vs Processed Products"),
             fluidRow(
               column(DT::dataTableOutput("processed.raw_table"), width = 11)
             ),
           )
         )
      ),
      
      ######## Private vs National ######## 
      tabPanel("Private vs National",
         sidebarLayout(
           sidebarPanel(
             h3("Lower Tailed Test Results"),
             p("Determine whether it is cheaper to purchase private brand products over national brand products:", style = "font-family: 'times'; font-si16pt"),
             p("H0(null): The mean difference in price between private and national brand products is equal to or greater than 0. I.e., private brand products are not less expensive than national brand products.", style = "font-family: 'times'; font-si16pt"),
             p("H1(Alt): The mean difference in price between private and national brand products is less than 0. I.e., private brand products are less expensive than national brand products.", style = "font-family: 'times'; font-si16pt"),
             verbatimTextOutput("t_test.private.national"),
           ),
           
           mainPanel(
             h3("Private vs National Products"),
             fluidRow(
               column(DT::dataTableOutput("private.national_table"), width = 11)
             ),
           )
         )
      ),
    )
  )
)

server <- function(input, output) {
    
    # Initalize - create sample basket with settings
    init_cals <- 5000
    init_weight<- c("Vegtable" = 0.2, "Fruit" = 0.2, "Protein" = 0.2, "Dairy" = 0.2, "Grain" = 0.2)
    init.basket <- get.basket(init_cals, init_weight) 
    init.basket <- get.competing.prices(init.basket)
    
    # Intialize - regression on default diet 
    cals.l <- seq(10000, 30000, 5000)
    user <- get_diet_cals(cals.l, init_weight)
    df.diets<- data.frame(cals.l, user)
    names(df.diets)<-c("calories", "user")
    
    # Initalize T-Test: Processed vs Raw, Private vs National
    # Static data - does not change given user input
    df.processed.raw <- compare_processed.raw()
    df.private.national <- compare_private.national()
    
    data <- reactiveValues(weight_group = init_weight, calories = init_cals,
                           basket=init.basket, diets=df.diets)
    
    # "Sample Basket" button; - Create Sample Basket
    observeEvent(input$sample_bakset, {
      
      # Ensure food group wegihting equals one
      weight <- input$w_veg + input$w_fruit + input$w_protein + input$w_dairy + input$w_grain
      if(isFALSE(all.equal(weight,1))){
        output$warning <- renderText({ "Weighting of Food Groups must equal 1" })
      }
      else{
        output$warning <- renderText({ NULL }) # Clear warning 
        
        # Fetch input and create sample basket 
        data$weight_group = c("Vegtable" = input$w_veg, "Fruit" = input$w_fruit, "Protein" = input$w_protein, "Dairy" = input$w_dairy, "Grain" = input$w_grain)
        data$calories = input$cals
        df.basket <- get.basket(data$calories, data$weight_group) # Create sample basket via safeway 
        data$basket <- get.competing.prices(df.basket)# Fetch competing prices - walmart, streets market
      }
    })
    
    # Output: Sampled Bakset
    output$table <- DT::renderDataTable({
      df.basket = data$basket
      col_order <- c("Food Group","Item Name", "Servings","Walmart", "Safeway",
                     "Streets Market", "Calories")
      df.basket <- df.basket[, col_order]
      
      DT::datatable(df.basket, rownames = FALSE, options = list(pageLength = 5))
    })
    # Output: ANOVA Test
    output$anovaTable <- renderPrint({
      df.basket = data$basket
      keycol <- "store"
      valuecol <- "price"
      gathercols <- c("Safeway", "Walmart","Streets Market")
      sampled.basket <- gather_(df.basket, keycol, valuecol, gathercols)
      aov.model <- aov(sampled.basket$price~factor(sampled.basket$store))
      summary(aov.model)
    })
    
    # Lower-tail T-Test: Walmart vs Safeway
    output$t_test_walmart.safeway <- renderPrint({
      df.basket = data$basket
      walmart.s <- df.basket$Walmart
      safeway.s <- df.basket$Safeway
      res <- t.test(walmart.s, safeway.s, alternative='less')
      res
    })
    #Lower-tail T-Test: Walmart vs Streets Market
    output$t_test_walmart.streetsmarket <- renderPrint({
      df.basket = data$basket
      walmart.s <- df.basket$Walmart
      streetmkt.s <- df.basket["Streets Market"]
      res <- t.test(walmart.s, streetmkt.s, alternative='less')
      res
    })
    # Lower-tail T-Test: Walmart vs Safeway
    output$t_test_safeway.streetsmarket <- renderPrint({
      df.basket = data$basket
      swy <- df.basket$Safeway
      sm <- df.basket["Streets Market"]
      res <- t.test(swy, sm, alternative='less')
      res
    })
  
    # Output: Boxplot of store prices
    output$boxplot_prices <- renderPlot({
      df.basket = data$basket
      ggplot( stack(df.basket[c("Safeway", "Walmart","Streets Market")]), aes(x = ind, y = values) ) + 
        geom_boxplot() + xlab("Store") + ylab("Price") + ggtitle("Boxplot of Price by Store")
    })
    # Output: Boxplot sumamry statistics
    output$sum_total <- renderTable({
      df.basket = data$basket
      df.display <- df.basket[c("Food Group", "Safeway", "Walmart", "Streets Market", "Calories")]
      # Group by Food Group and sum
      df.display <- df.display %>% group_by(df.display['Food Group']) %>%summarise_all(.funs = sum,na.rm=T)
      # Add Total row
      df.display <- df.display %>% bind_rows(summarise(.,across(where(is.numeric), sum), across(where(is.character), ~"Total")))
      df.display <- as.data.frame(df.display)
      rownames(df.display) <- df.display$`Food Group`
      df.display <- df.display[ , !(names(df.display) %in% 'Food Group')]
      col_order <- c("Walmart", "Safeway","Streets Market", "Calories")
      df.display[, col_order]
    },rownames = TRUE) 
    
    # Output: Basket Statistics
    output$boxplot_summary <- renderPrint({
      df.basket = data$basket
     summary(df.basket[c("Safeway", "Walmart","Streets Market")])
    })
    
    
    
    # "Refresh" button - Refresh regression for compare diets 
    observeEvent(input$refresh, {
      weight <- input$w_veg + input$w_fruit + input$w_protein + input$w_dairy + input$w_grain
      if(isFALSE(all.equal(weight,1))){
        output$warning <- renderText({ "Invalid Weighting!" })
      }
      else{
        data$weight_group = c("Vegtable" = input$w_veg, "Fruit" = input$w_fruit, "Protein" = input$w_protein, "Dairy" = input$w_dairy, "Grain" = input$w_grain)
        
        # Range of calories specified by user
        cals.range <- input$cal_range
        cal_step <- input$cal_step
        cals.l <- seq(cals.range[1], cals.range[2], cal_step)
        
        # Get prices for user diet
        user <- get_diet_cals(cals.l, data$weight_group)
        df.diets<- data.frame(cals.l, user)
        names(df.diets)<-c("calories", "user")
        
        # Get prices for keto diet
        if(input$keto == TRUE){
          weight.keto <- c("Vegtable" = 0.2, "Fruit" = 0, "Protein" = 0.5, "Dairy" = 0.2, "Grain" = 0.1)
          keto <- get_diet_cals(cals.l, weight.keto)
          df.diets$keto <- keto
        }
        
        # Get prices for vegan diet
        if(input$vegan == TRUE){
          weight.vegan <- c("Vegtable" = 0.3, "Fruit" = 0.3, "Protein" = 0, "Dairy" = 0.2, "Grain" = 0.2)
          vegan <- get_diet_cals(cals.l, weight.vegan)
          df.diets$vegan <- vegan
        }
        
        # Get prices for high carb diet
        if(input$carb == TRUE){
          weight.highcarb <- c("Vegtable" = 0.2, "Fruit" = 0.1, "Protein" = 0.1, "Dairy" = 0.1, "Grain" = 0.5)
          high_carb <- get_diet_cals(cals.l, weight.highcarb)
          df.diets$high_carb <- high_carb
        }
        data$diets <- df.diets
      }
    })
    

    # Regression Plot
    output$linePlot <- renderPlot({
      df.diets = data$diets
      gathercols <- colnames(df.diets)
      gathercols <- gathercols[gathercols != "calories"]
      df.diets <- gather_(df.diets, "diet", "price", gathercols)
      ggplot(df.diets, aes(x = calories, y = price, color = diet, shape = diet)) + 
        geom_point()  + geom_smooth(method = "lm", fill = NA) + xlab("Calories") + ylab("Price") + ggtitle("Rate of Price increase by Calories")
    })
    
    
    # Output Linear Regression Coefficient: User, keto, vegan, high-carb
    output$lm_user.summary <- renderPrint({
      df.diets = data$diets
      lm(df.diets$user ~ df.diets$calories)
    })
    output$lm_keto.summary <- renderPrint({
      df.diets = data$diets
      if("keto" %in% colnames(df.diets))
      {
        lm(df.diets$keto ~ df.diets$calories)
      }
    })
    output$lm_vegan.summary <- renderPrint({
      df.diets = data$diets
      if("vegan" %in% colnames(df.diets))
      {
        lm(df.diets$vegan ~ df.diets$calories)
      }
    })
    output$lm_carb.summary <- renderPrint({
      df.diets = data$diets
      if("high_carb" %in% colnames(df.diets)){
        lm(df.diets$high_carb ~ df.diets$calories)
      }
    })
    
######## Compare Products - Raw CVS Processed ######## 
    # Paired T-Test
    output$t_test.raw.processed <- renderPrint({
      df.processed.raw
      before <- df.processed.raw$`Raw Price`
      after <- df.processed.raw$`Processed Price`
      res <- t.test(before, after, alternative='less')
      res
    })
    output$processed.raw_table <- DT::renderDataTable({
      DT::datatable(df.processed.raw, rownames = FALSE, options = list(pageLength = 10))
    })

######## Compare Products - Private VS National ######## 
    # Paired T-Test
    output$t_test.private.national <- renderPrint({
      df.processed.raw
      before <- df.private.national$`Private Price`
      after <- df.private.national$`National Price`
      res <- t.test(before, after, alternative='less')
      res
    })
    output$private.national_table <- DT::renderDataTable({
      DT::datatable(df.private.national, rownames = FALSE, options = list(pageLength = 10))
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
