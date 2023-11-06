# import library
library(shiny)
library(shinydashboard)
library(DT)
library(plotly)
library(shinycssloaders) # to add a loader while graph is populating
library(data.table)

#tit <- tags$h1(icon("apple"), "AppleStore")

shinyUI(
  dashboardPage(skin = "blue",
    
    #header
    dashboardHeader(title = "Apple Store",
                    tags$li(class="dropdown",tags$a(href="https://github.com/vinay-git2412/AppleStore-Shinyapp.git", icon("github"), "Source Code", target="_blank")),
                    
                    # Dropdown menu for tasks, with progress bar
                    dropdownMenu(type = "tasks", badgeStatus = "danger",
                                 taskItem(value = 80, color = "green",
                                          "Design "
                                 ),
                                 taskItem(value = 60, color = "yellow",
                                          "Plots enhancment"
                                 ),
                                 taskItem(value = 20, color = "red",
                                          "Writing documentation"
                                 )
                    )
    ),
    
    #sidebar
    dashboardSidebar(
      
      #menu and menu items
      sidebarMenu(id = 'sidebar', #sidebar id is used to call sidebarmenu 
        menuItem("About", tabName = "abt", icon = icon("folder")),  # describes about data 
        menuItem("Data", tabName = "data", icon = icon("database")), # shows data and stats
        menuItem("Statistics", tabName = "stat", icon = icon("id-card"), badgeLabel = "new", badgeColor = "green"), # Stats with data
        # Conditional Panel for conditional widget appearance
        # Filter should appear only for the visualization menu and selected tabs within it
        conditionalPanel("input.sidebar == 'stat'", selectInput(inputId = "g1" , label ="Select the Genre" , choices = my_data_genre)),
        menuItem("Visualization", tabName = 'viz', icon = icon('area-chart'))
      )
      
    ),
    
    #main body
    dashboardBody(
      tabItems(  # tabitems alligned with menu items
        tabItem(tabName = "abt",  # About menu item
                fluidRow(
                  column(width = 5,tags$img(src="applestore.jpg", height="100%", width="100%", align="right")),
                  column(width = 7,tags$img(src="app1.jpeg", height="70%", width="70%", align="left"))
                  
                ),
                tags$br(), # used to give space between fluid rows
                
                fluidRow(
                  # box function used to make the text look beautiful
                  box(tags$h1("Mobile App Statistics (Apple iOS app store)"),
                      tags$br(),
                      tags$h5("The ever-changing mobile landscape is a challenging space to navigate. The percentage of mobile over desktop is only increasing. Android holds about 53.2% of the smartphone market, while iOS is 43%. To get more people to download your app, you need to make sure they can easily find your app. Mobile app analytics is a great way to understand the existing strategy to drive growth and retention of future user."),
                      tags$br(),
                      tags$h5("With million of apps around nowadays, the following data set has become very key to getting top trending apps in iOS app store. This data set contains more than 7000 Apple iOS mobile application details. The data was extracted from the iTunes Search API at the Apple Inc website. R and linux web scraping tools were used for this study."),
                      width = 12, status = "primary"
                      )
                )
        ),
        
        tabItem(tabName = "data",  # about data
                tabBox(id = "tb1", width = 12, 
                       tabPanel("Data", icon = icon("table"), withSpinner(dataTableOutput("datatable"))), #display data with spinner effect while loading
                       tabPanel("Summary", icon = icon("chart-pie"), withSpinner(verbatimTextOutput("summary"))), # display summary of data
                       tabPanel("Structure", icon = icon("uncharted"), withSpinner(verbatimTextOutput("structure"))))), # display structure of data
        
        tabItem(tabName = 'stat', # stats menu
                
                fluidRow(box(status = 'info', solidHeader = F,background = "red", # heading of this page in a box
                  tags$h1(textOutput("gen")), align = 'center',width = 800, tags$br())
                  ),
                
                # info and value boxes
                fluidRow(
                  valueBoxOutput("avg_size", width = 4), infoBoxOutput("avg_price", width = 4), infoBoxOutput("supp_dev", width = 4)),
                
                fluidRow(
                  infoBoxOutput("avg_rat", width = 4), valueBoxOutput("avg_rat_curr", width = 4), infoBoxOutput("avg_user_cnt", width = 4)),
                
                fluidRow(
                  valueBoxOutput("avg_user_cnt_ver", width = 4))
                ),
        
        tabItem(tabName = 'viz', # visulatization menu
                tabBox(id = 'tb2', width = 12,
                       tabPanel("Genre Trends",value = "trends",icon = icon("bar-chart"),  # create tabpanels inside tab
                                selectInput("tre","Select Parameter", choices = c1, width = 250),
                                box(status = 'success', soildHeader = T,background = 'green',# heading of this page in a box
                                    tags$h1(textOutput("gt")), align = 'center',width = 12),  # below code displays top and bottom 5 values
                                fluidRow(tags$div(align="center", box(tableOutput("top5"), title = textOutput("head1") , collapsible = TRUE, status = "warning",  collapsed = TRUE, solidHeader = TRUE)),
                                         tags$div(align="center", box(tableOutput("low5"), title = textOutput("head2") , collapsible = TRUE, status = "danger",  collapsed = TRUE, solidHeader = TRUE))
                                ),
                                withSpinner(plotlyOutput("partrends"))),  # loads plots with spinner display
                       
                       tabPanel("Price Trends", icon = icon('line-chart'),  # second panel
                                selectInput("pri_tre", "Select Parameter", choices = c2, width = 250),
                                withSpinner(plotlyOutput("pricetrends"))),
                       
                       tabPanel("Free vs Paid", icon = icon("pie-chart"),
                                fluidRow(
                                  box(tags$h1("Distribution of Free apps"),
                                      tags$br(),
                                      withSpinner(plotlyOutput("freepie")),
                                      width = 6),
                                  box(tags$h1("Distribution of Paid apps"),
                                      tags$br(),
                                      withSpinner(plotlyOutput("paidpie")),
                                      width = 6)
                                ),
                                fluidRow(
                                  box(
                                      checkboxGroupInput(
                                        "barorpie",
                                        "Select type:",
                                        choices = c("Bar", "Pie"),
                                        selected = "Bar",
                                        inline = FALSE
                                      ),
                                      radioButtons(
                                        "genrepar",
                                        "Select type of Genre:",
                                        choices = my_data_genre,
                                        inline = TRUE
                                      ),
                                      #actionButton(inputId = "alert", label = "Preview"),
                                      width = 3
                                      ),
                                  # box(tags$h3(textOutput("selpie")), align = 'center',
                                  #     withSpinner(plotlyOutput("selpiegen")),
                                  #     width = 4.5),
                                  # box(tags$h3(textOutput("selbar")), align = 'center',
                                  #     withSpinner(plotlyOutput("selbargen")),
                                  #     width = 4.5)
                                  
                                  uiOutput("baropie1"),
                                  uiOutput("baropie2"),
                                  uiOutput("baropie3"),
                                ))
                                )
                       )
                )
        
        
      )
    )
  )

