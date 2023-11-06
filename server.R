# import library
library(shiny)
library(shinydashboard)
library(dplyr)
library(DT)
library(data.table)

#droping first column as it is not needed
my_data <- read.csv("data/AppleStore.csv")
my_data <- subset(my_data, select = -c(1))


shinyServer(
  function(input,output,session){
    
################################################################################################# DATA CODE ##############################################################################################    
    output$datatable <- renderDataTable( #rendering using DT
      my_data,  # below options are used to display,filter,select table dynamically
      options = list(paging = TRUE,    ## paginate the output
                     pageLength = 7,  ## number of rows to output for each page
                     scrollX = TRUE,   ## enable scrolling on X axis
                     scrollY = TRUE,   ## enable scrolling on Y axis
                     autoWidth = TRUE, ## use smart column width handling
                     server = FALSE,   ## use client-side processing
                     dom = 'Bfrtip',
                     buttons = c('csv', 'excel'),
                     columnDefs = list(list(targets = '_all', className = 'dt-center'),
                                       list(targets = c(0, 8, 9), visible = TRUE))
      ),
      extensions = 'Buttons',
      selection = 'single', ## enable selection of a single row
      filter = 'bottom',              ## include column filters at the bottom
      rownames = FALSE                ## don't show row numbers/names
    )
    
    output$summary <- renderPrint( #renderprint for summary
      
      my_data %>% 
        summary()
    )
    
    output$structure <- renderPrint( #renderprint for structure
      my_data %>% 
        str()
    )
    
########################################################################################### STAT CODE(INFO,VALUE BOX) #########################################################################################
    
    output$gen <- renderText(  # returns heading for stats page 
                             paste("Complete Stats of all apps in", input$g1, sep = " "))
    
    infobox_data <- reactive({  # reactive function that can be used anywhere
      filter(my_data, prime_genre == input$g1)
    }
    )
    #infobox_data <- filter(my_data, prime_genre == g1()) #problem with reactive
    
    #yellow2. Valid colors are: red, yellow, aqua, blue, light-blue, green, navy, teal, olive, lime, orange, fuchsia, purple, maroon, black
    
    output$avg_size <- renderValueBox(  #valuebox(we dont have ttile in this. this is the only difference b/w both)
      valueBox( 
              value = paste(round(mean(infobox_data()$size_bytes)* 0.000001, digits = 2), "Mb", sep = " "),
              subtitle = "Average size of apps",
              icon('circle')
              ) 
      
    )
    
    output$avg_price <- renderInfoBox( #infobox
      infoBox(title = "Avg_price", 
              value = paste(round(mean(infobox_data()$price), digits = 2), "$", sep = " "),
              subtitle = "Average price of apps",
              icon = icon('usd'),
              color = 'yellow',
              fill = TRUE)
    )
    
    output$supp_dev <- renderInfoBox(
      infoBox(title = "Supported devices", 
              value = round(mean(infobox_data()$sup_devices.num)),
              subtitle = "Supported devices by apps in this genre",
              icon = icon('television'),
              color = 'maroon',
              fill = TRUE)
    )
    
    output$avg_rat <- renderInfoBox(
      infoBox(title = "Avg_user_rating", 
              value = round(mean(infobox_data()$user_rating), digits = 2),
              subtitle = "Average rating by user for all versions",
              icon = icon('smile'),
              color = "orange",
              fill = TRUE)
    )
    
    output$avg_rat_curr <- renderInfoBox(
      valueBox( 
              value = round(mean(infobox_data()$user_rating_ver), digits = 2),
              subtitle = "Average rating by user for current version",
              icon = icon('smile'),
              color = 'teal',
              )
    )
    
    output$avg_user_cnt <- renderInfoBox(
      infoBox(title = "Avg_rating_count", 
              value = round(mean(infobox_data()$rating_count_tot)),
              subtitle = "Average count of user given rating",
              icon = icon('star'),
              color = 'olive',
              fill = TRUE)
    )
    
    output$avg_user_cnt_ver <- renderInfoBox(
      valueBox(
              value = round(mean(infobox_data()$rating_count_ver)),
              subtitle = "Average count of user given rating for current version",
              icon = icon('star'),
              color = "purple"
              )
    )
    
    ##################################################################################### PARAMETER TRENDS CODE ###############################################################################################################
    
    t <- reactive({  # REACTIVE FUNC
      input$tre
    })
    
    output$gt <- renderText(  # returns heading for stats page 
      paste("Trend of", t(), sep = " "))
    
    output$partrends <- renderPlotly(
      if (t() == "size_bytes")
        fig <- plot_ly(
          data = agg_t_size,
          x =~ prime_genre,
          y =~ size_bytes,
          #color =~ prime_genre,
          type = 'bar',
          order = "ascending"
        )
      else if(t() == 'price')
         fig <- plot_ly(
           data = agg_t_price,
           x =~ prime_genre,
           y =~ price,
           type = 'bar'
         )
      else if(t() == "user_rating")
        fig <- plot_ly(
          data = agg_t_user_rating,
          x =~ prime_genre,
          y =~ user_rating,
          type = 'bar'
        )
      else if(t() == 'user_rating_ver')
        fig <- plot_ly(
          data = agg_t_user_rating_ver,
          x =~ prime_genre,
          y =~ user_rating_ver,
          type = 'bar'
        )
      else if(t() == 'rating_count_tot')
        fig <- plot_ly(
          data = agg_t_rating_count_tot,
          x =~ prime_genre,
          y =~ rating_count_tot,
          type = 'bar'
        )
      else if(t() == 'rating_count_ver')
        fig <- plot_ly(
          data = agg_t_rating_count_ver,
          x =~ prime_genre,
          y =~ rating_count_ver,
          type = 'bar'
        )
      
    )
    
    # Rendering the box header  
    output$head1 <- renderText(
      paste("Top 5 of", input$tre)
    )
    
    # Rendering the box header 
    output$head2 <- renderText(
      paste("Last 5 of", input$tre)
    )
    
    # Rendering table with 5 states with high arrests for specific genre type
    output$top5 <- renderTable({
      
      if (t() == "size_bytes")
        agg_t_size[order(agg_t_size$size_bytes, decreasing = TRUE),] %>% 
          head(5)
      else if(t() == "price")
        agg_t_price[order(agg_t_price$price, decreasing = TRUE),] %>% 
        head(5)
      else if(t() == "user_rating")
        agg_t_user_rating[order(agg_t_user_rating$user_rating, decreasing = TRUE),] %>% 
        head(5)
      else if(t() == "user_rating_ver")
        agg_t_user_rating_ver[order(agg_t_user_rating_ver$user_rating_ver, decreasing = TRUE),] %>% 
        head(5)
      else if(t() == "rating_count_tot")
        agg_t_rating_count_tot[order(agg_t_rating_count_tot$rating_count_tot, decreasing = TRUE),] %>% 
        head(5)
      else if(t() == "rating_count_ver")
        agg_t_rating_count_ver[order(agg_t_rating_count_ver$rating_count_ver, decreasing = TRUE),] %>% 
        head(5)
      
    })
    
    # Rendering table with 5 states with low arrests for specific crime type
    output$low5 <- renderTable({
      
      if (t() == "size_bytes")
        agg_t_size[order(agg_t_size$size_bytes, decreasing = FALSE),] %>% 
        head(5)
      else if(t() == "price")
        agg_t_price[order(agg_t_price$price, decreasing = FALSE),] %>% 
        head(5)
      else if(t() == "user_rating")
        agg_t_user_rating[order(agg_t_user_rating$user_rating, decreasing = FALSE),] %>% 
        head(5)
      else if(t() == "user_rating_ver")
        agg_t_user_rating_ver[order(agg_t_user_rating_ver$user_rating_ver, decreasing = FALSE),] %>% 
        head(5)
      else if(t() == "rating_count_tot")
        agg_t_rating_count_tot[order(agg_t_rating_count_tot$rating_count_tot, decreasing = FALSE),] %>% 
        head(5)
      else if(t() == "rating_count_ver")
        agg_t_rating_count_ver[order(agg_t_rating_count_ver$rating_count_ver, decreasing = FALSE),] %>% 
        head(5)

    })
    
    
    ######################################################################################### PRICE TRENDS CODE #############################################################################################
    
    p <- reactive({
      input$pri_tre
    })
    
    output$pricetrends <- renderPlotly(
      if (p() == 'user_rating')
        fig <- plot_ly(
          data = df_fil,
          x =~ user_rating,
          y =~ price,
          mode = 'markers'
        )
      else if(p() == 'user_rating_ver')
        fig <- plot_ly(
          data = df_fil,
          x =~ user_rating_ver,
          y =~ price,
          mode = 'markers'
        )
      else if(p() == 'size_bytes')
        fig <- plot_ly(
          data = df_fil,
          x =~ size_bytes,
          y =~ price,
          mode = 'markers'
        )
      else if(p() == 'cont_rating')
        fig <- plot_ly(
          data = agg_p_cont,
          x =~ cont_rating,
          y =~ price,
          type = 'bar',
          colors  = "orange"
        )
    )
    
########################################################################################### PIE CHARTS CODE ###################################################################################################################
    
    output$freepie <- renderPlotly(
      fig <- plot_ly(agg_f, labels = ~prime_genre, values = ~Count_f, type = 'pie')
    )
    
    output$paidpie <- renderPlotly(
      fig <- plot_ly(agg_p, labels = ~prime_genre, values = ~Count_p, type = 'pie')
    )
    
    g <- reactive({
      input$genrepar
    })
    
    output$selbar <- renderText(paste("Distribution of apps in", g(), sep = " "))
    output$selpie <- renderText(paste("Distribution of apps in", g(), sep = " "))
    
    output$selbargen <- renderPlotly(
      fig <- plot_ly(
        data = df_1,
        x = ~rownames(df_1),
        y = ~ get(g()),
        type = 'bar',
        yaxis = c( 0 , 3000 )
      ) %>%
        layout(xaxis = list(title = "Apps"),
               yaxis = list(title = "genre"))
    )
    
    output$selpiegen <- renderPlotly(
      fig <- plot_ly(
        data = df_1,
        labels = ~rownames(df_1),
        values = ~get(g()),
        type = 'pie'
      )
    )
    
    output$e1 <- renderPrint(
      paste("Select atleast 1 type of plot")
    )
    
    bp <- reactive({
      input$barorpie
    })
    
    #if (bp() == "Bar" && bp() == "Pie")
    output$baropie1 <- renderUI(
      if (bp() == "Bar" & bp() == "Pie")
        box(box(tags$h3(textOutput("selpie")), align = 'center',
                withSpinner(plotlyOutput("selpiegen")),
                width = 4.5),
            box(tags$h3(textOutput("selbar")), align = 'center',
                withSpinner(plotlyOutput("selbargen")),
                width = 4.5), width = 9)
      )
    #else if (bp() == "Bar")
    output$baropie2 <- renderUI(
      if (bp() == "Bar")
        box(tags$h3(textOutput("selbar")), align = 'center',
            withSpinner(plotlyOutput("selbargen")),
            width = 9)
      )
    #else if (bp() == "Pie")
    output$baropie3 <- renderUI(
      if (bp() == "Pie")
        box(tags$h3(textOutput("selpie")), align = 'center',
            withSpinner(plotlyOutput("selpiegen")),
            width = 9)
      )
    
    
  }
)