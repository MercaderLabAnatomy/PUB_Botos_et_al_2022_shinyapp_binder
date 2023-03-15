library("rlang")
library("sodium")
library("shinymanager")
library("DT")
library("dplyr")
library("fontawesome")
library("shinydashboard")
library("shinycssloaders")
library("igraph")
library("markdown")
library("ggplot2")
library("visNetwork")
library("heatmaply")
library("plotly")
library("brew")
library("RColorBrewer")
library("curl")


rm(list=ls())
gc()




#################################
#                               #
#   Data Load                   #
#                               #     
#################################


ui <-dashboardPage(skin = "purple",
                               dashboardHeader(title = "Heart Injuries"),
                               dashboardSidebar(width = 300,
                                                sidebarMenu(
                                                  menuItem("Home",tabName = "home",icon = icon("home")),
                                                  menuItem("Resection vs Sham",tabName = "resected", icon = icon("scissors",lib = "glyphicon")),#,icon = icon("calendar",lib = "glyphicon")),
                                                  menuItem("Ablation vs Sham",tabName = "ablated",icon = icon("tag",lib = "glyphicon")),
                                                  menuItem("Uninjured vs Sham",tabName = "uninjured",icon = icon("ok",lib = "glyphicon")),
                                                  menuItem("Heart Core Regeneration Common Genes ",tabName = "heartcore",icon = icon("heart-empty",lib = "glyphicon")),
                                                  menuItem("PubMed Query",tabName = "kuk",icon = icon("list",lib = "glyphicon")),
                                                  menuItem("Core Regeneration Log2FC",tabName = "core_l2fc",icon = icon("bookmark",lib = "glyphicon")),
                                                  menuItem("Genes Seeker",tabName = "gs",icon = icon("search",lib = "glyphicon"))
                                                  
                                                )),
                               dashboardBody(
                                 tabItems(
                                   tabItem(tabName = "home",
                                           #home section and markdown
                                           includeMarkdown("./www/markdown_home.md")),
                                   # #next tab
                                   
                                   #https://stackoverflow.com/questions/70689513/how-to-have-shiny-dashboard-box-fill-the-entire-width-of-my-dashbaord
                                   #next tab
                                   tabItem(tabName = "resected",
                                           fluidRow(
                                             column(width = 12, textOutput("verb_amp")),#tags$head(tags$style("#text1{color: red;font-size: 20px;font-style: italic;}")) %>% withSpinner()),
                                             column(width = 12, visNetworkOutput(outputId = "resected_net",width = "auto", height = "600px") %>% withSpinner()),
                                             column(width = 12, dataTableOutput(outputId = "goTable_amp") %>% withSpinner()),
                                             column(width = 12,valueBoxOutput(width = "100%",outputId = "genes_go_amp") %>% withSpinner()),
                                             column(width = 12,plotlyOutput(outputId = "amphm",width = "100%",height = "100%") %>% withSpinner())
                                             
                                           )
                                   ),
                                   #next tab
                                   tabItem(tabName = "ablated",
                                           fluidRow(
                                             column(width = 12, textOutput("verb_abl")),#tags$head(tags$style("#text1{color: red;font-size: 20px;font-style: italic;}")) %>% withSpinner()),
                                             column(width = 12, visNetworkOutput(outputId = "ablated_net",width = "auto",height = "600px") %>% withSpinner()),
                                             column(width = 12, dataTableOutput(outputId = "goTable_abl") %>% withSpinner()),
                                             column(width = 12,valueBoxOutput(width = 12,outputId = "genes_go_abl") %>% withSpinner()),
                                             column(width = 12,plotlyOutput(outputId = "ablhm",width = "100%",height = "100%") %>% withSpinner())
                                             
                                           )
                                   ),
                                   #next tab
                                   tabItem(tabName = "uninjured",
                                           fluidRow(
                                             column(width = 12, textOutput("verb_unj")),#tags$head(tags$style("#text1{color: red;font-size: 20px;font-style: italic;}")) %>% withSpinner()),
                                             column(width = 12, visNetworkOutput(outputId = "uninjured_net",width = "auto",height = "600px") %>% withSpinner()),
                                             column(width = 12, dataTableOutput(outputId = "goTable_unj") %>% withSpinner()),
                                             column(width = 12,valueBoxOutput(width = 12,outputId = "genes_go_unj") %>% withSpinner()),
                                             column(width = 12,plotlyOutput(outputId = "unjhm",width = "100%",height = "100%") %>% withSpinner())
                                             
                                           )
                                   ),
                                   #next tab
                                   tabItem(tabName = "heartcore",
                                           fluidRow(
                                             column(width = 12, textOutput("verb_core") %>% withSpinner()),
                                             column(width = 12, visNetworkOutput(outputId = "heartcore_net",width = "auto",height = "600px") %>% withSpinner()),
                                             column(width = 12, dataTableOutput(outputId = "goTable_core") %>% withSpinner()),
                                             column(width = 12,valueBoxOutput(width = "auto",outputId = "genes_go_core") %>% withSpinner()),
                                             column(width = 12,plotlyOutput(outputId = "hchm",width = "100%",height = "100%") %>% withSpinner())
                                             
                                           )
                                   ),
                                   #next tab
                                   tabItem(tabName = "kuk",
                                           fluidRow(
                                             column(width = 12,plotlyOutput(outputId = "known", width = "100%", height = "1024px") %>% withSpinner()),
                                             column(width = 12,plotlyOutput(outputId = "unknown", width = "100%", height = "1024px") %>% withSpinner())         
                                           )
                                   ),
                                   #next tab
                                   tabItem(tabName = "core_l2fc",
                                           fluidRow(
                                             column(width = 12,plotlyOutput(outputId = "core_l2fc_hm", width = "100%", height = "2048px") %>% withSpinner())         
                                           )
                                           
                                   ),
                                   #next tab
                                   tabItem(tabName = "gs",
                                           fluidRow(
                                             column(width=12,selectizeInput(inputId = "gene",
                                                                           choices = NULL,
                                                                           label = "Select at least 2 Gene Symbol",
                                                                           options = list(
                                                                             placeholder= "Please select at least two genes",
                                                                             maxItems = 50),
                                                                           selected = c("Select_a_node","To_visualize_heatmaps"),
                                                                           multiple = TRUE)),
                                             column(width = 12, dataTableOutput(outputId = "geneseeker_table")),
                                             column(width = 12,plotlyOutput(outputId = "geneseeker_plot",width = "100%",height = "100%")),
                                           )
                                   
                                 )# end tabItems
                                 # Add download button 
                               )# end dashboardBody
                 )# end dashboardPage
)#end all