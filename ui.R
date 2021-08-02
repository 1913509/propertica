# Author: Rahim Bux
# Student Id: 1913509
# Course: MSc IT with BI
# Academic Year: 2020-2021
#
# 
# 
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

##install.packages("remotes")
#remotes::install_github("njtierney/ukpolice")
#install.packages("devtools")
#devtools::install_github("erzk/zooplaR")


# REQUIRED LIBRARIES
library(shiny)
library(plyr)
library(dplyr)
library(forcats)
library(leaflet)
library(ukpolice)
library(opencage)
library(highcharter)
library(e1071)
library(randomForest)
library(datasets)
library(zooplaR)
library(ggplot2)
library(Metrics)
library(stringr)
library(rpart)
library(scales)
library(lattice)
library(shinydashboard)
library(shinythemes)
library(shinyWidgets)
#library(shinydashboardPlus)



# UI for application - PROPERTICA
ui <- fluidPage(
    
    
    navbarPage(theme = shinytheme("flatly"), collapsible = TRUE,  
               HTML('<a style="text-decoration:none;cursor:default; src = rgubw1.png;color:#fcbd00;" class="active" href="#">PROPERTICA ®</a>'), id="nav",
               windowTitle = "PROPERTICA ®",
               
               
               ## Predict Section------------------------
               tabPanel("Predict",
                        div(class="outer",
                            tags$head(includeCSS("styles.css")),
                            tags$link(href = "https://fonts.googleapis.com/css?family=Oswald", rel = "stylesheet"),
                            tags$style(type = "text/css", "html, body {width:100%;height:100%; font-family: Oswald, sans-serif;}"),
                        ),
                        
                        # Background 
                        setBackgroundImage(src = "b7a.jpg", shinydashboard = FALSE),
                        
                        absolutePanel(id = "logo", class = "card", bottom = 50, left = 50, width = 50, fixed=TRUE, draggable = FALSE, height = "auto",
                                      tags$a(href='http://rgu.ac.uk', tags$img(src='rgubw1.png',height='50',width='auto'))),
                        
                        absolutePanel(id = "footer", class = "card", bottom = 10, left = 50, width = 'auto', fixed=TRUE, draggable = FALSE, height = "auto",
                                      tags$h6('Designed, coded by Rahim Bux to fulfil the requirement of MSc Project')),
                        
                        absolutePanel(id = "controls", class = "panel panel-default",
                                      top = 72, left = 55, width = 280, fixed=TRUE,
                                      draggable = FALSE, height = "auto",
                                      
                                      h2("Input Preferences"),
                                      
                                      selectInput("p_year","Select time period:", choices = 2019:as.numeric(format(Sys.Date(),"%Y"))),
                                      selectInput("postcode", "Select Postcode", c('E1'='1','E2'='13', 'E3'='14', 'E4'='15', 'E5'='16', 'E6'='17', 'E7'='18', 'E8'='19','E9'='20', 'E10'='2','E11'='3', 'E12'='4', 'E13'='5', 'E14'='6', 'E15'='7', 'E16'='8', 'E17'='9', 'E18'='10', 'E19'='11', 'E20'='12')),
                                      selectInput('p_property_type', 'Property Type', c('Detached' = '1', 'Flats/Maisonettes'= '2',  'Semi-Detached' = '3',  'Others' = '4', 'Terraced' = '5')),
                                      selectInput('p_build', 'Build', c('Old Build' = '0', 'New Build' = '1')),
                                      selectInput('p_estate_type', 'Estate Type', c('Leasehold' = '1', 'Freehold' = '0')),
                                      
                                      
                                      actionButton("go", "PREDICT!", class = "btn-primary", icon = icon('fas fa-search-dollar')),
                                      
                        ),
                        absolutePanel(
                            id = "controls", class = "panel panel-default",
                            top = 72, right = 55, width = 275, fixed=TRUE, align = "center",
                            draggable = FALSE, height = "auto",
                            tags$h2(textOutput("value")),
                            tags$p(textOutput('range'),tags$p("Price Range"),
                                   tags$p(helpText(sprintf('The prediction is based on a SVR supervised machine learning model. Furthermore, the models deliver a mean absolute error (MAE) of %s total number of Price, and a root mean squared error (RMSE) of %s total number of Price', round(mae_svm, digits = 0), round(rmse_svm, digits = 0))))),
                        )
                        
               ),
               
               
               
               
               
               ## Crime in the Area  ----------------
               tabPanel("Crime in the Area",
                        div(class="outer",
                            tags$head(includeCSS("styles.css"),
                                      tags$link(href = "https://fonts.googleapis.com/css?family=Oswald", rel = "stylesheet"),
                                      tags$style(type = "text/css", "html, body {width:100%;height:100%; font-family: Oswald, sans-serif;}"),
                            ),
                            
                            # Map 
                            leafletOutput("map", width = "100%", height = "100%"),
                            
                            # Shiny versions prior to 0.11 should use class = "modal" instead.
                            absolutePanel(id = "controls1", class = "panel panel-default",
                                          top = 72, left = 55, width = 250, fixed=TRUE,
                                          draggable = FALSE, height = "auto",
                                          
                                          h2("Search Crime"),
                                          
                                          textInput('cdate', 'Year:', placeholder = "eg: 2019-01"),
                                          selectInput("geocode", "Select Postcode:", c('E1', 'E2', 'E3', 'E4', 'E5' , 'E6', 'E7', 'E8', 'E9', 'E10', 'E11', 'E12', 'E13' , 'E14' , 'E15', 'E16', 'E17' , 'E18', 'E19', 'E20')),
                                          
                                          
                                          actionButton("geo", "SEARCH", class = "btn-primary", icon = icon('fas fa-search-location')),
                                          
                            ),
                            
                            absolutePanel(id = "controls1", class = "panel panel-default",
                                          top = 72, right = 55, width = 250, fixed=TRUE,
                                          draggable = FALSE, height = "auto",
                                          
                                          # Output Crime Chart
                                          highchartOutput("selectstat"),
                            ),
                            absolutePanel(id = "logo", class = "card", bottom = 50, left = 50, width = 50, fixed=TRUE, draggable = FALSE, height = "auto",
                                          tags$a(href='http://rgu.ac.uk', tags$img(src='csdmlogo.png',height='50',width='auto'))),
                            
                            
                            absolutePanel(id = 'footer', class = "card", bottom = 10, left = 50, width = 'auto', fixed=TRUE, draggable = FALSE, height = "auto",
                                          tags$p('Designed, coded by Rahim Bux to fulfil the requirement of MSc Project')),
                            
                            
                        ),
               ),
               
               ## Area Analysis--------
               tabPanel("Analysis of the Area",
                        div(class="outer",
                            tags$head(includeCSS("styles.css"),
                                      tags$link(href = "https://fonts.googleapis.com/css?family=Oswald", rel = "stylesheet"),
                                      tags$style(type = "text/css", "html, body {width:100%;height:100%; font-family: Oswald, sans-serif;}"),
                            ),
                            
                            # Map 
                            leafletOutput("areamap", width = "100%", height = "100%"),
                            
                            # Shiny versions prior to 0.11 should use class = "modal" instead.
                            absolutePanel(id = "controls1", class = "panel panel-default",
                                          top = 72, left = 55, width = 250, fixed=TRUE,
                                          draggable = FALSE, height = "auto",
                                          
                                          h2("Search Area"),
                                          
                                          selectInput("geoloc", "Select Postcode:", c('E1', 'E2', 'E3', 'E4', 'E5' , 'E6', 'E7', 'E8', 'E9', 'E10', 'E11', 'E12', 'E13' , 'E14' , 'E15', 'E16', 'E17' , 'E18', 'E19', 'E20')),
                                          #checkboxInput("my_location", "Or use your current location?"),
                                          
                                          actionButton("submit", "ANALYSIS", class = "btn-primary", icon = icon('fas fa-chart-bar')),
                                          
                            ),
                            
                            # Outputs Charts from Zoopla
                            absolutePanel
                            (id = "controls1", class = "panel panel-default", top = 72, right = 35, width = 370, fixed=TRUE, draggable = FALSE, height = "auto",
                                
                                htmlOutput("myImage"),
                                htmlOutput("myImage2"),
                                
                                
                            ),
                            absolutePanel(id = "logo", class = "card", bottom = 50, left = 50, width = 50, fixed=TRUE, draggable = FALSE, height = "auto",
                                          tags$a(href='http://rgu.ac.uk', tags$img(src='csdmlogo.png',height='50',width='auto'))),
                            
                            absolutePanel(id = 'footer', class = "card", bottom = 10, left = 50, width = 'auto', fixed=TRUE, draggable = FALSE, height = "auto",
                                          tags$p('Designed, coded by Rahim Bux to fulfil the requirement of MSc Project')),
                            
                            absolutePanel
                            (id = "controls2", class = "panel panel-default", bottom = 10, right = 170, width = 85, fixed=TRUE, draggable = FALSE, height = 120,
                                
                                valueBoxOutput("number")),
                            
                            absolutePanel
                            (id = "controls", class = "panel panel-default", bottom = 10, right = 270, width = 200, fixed=TRUE, draggable = FALSE, height = 120, align = 'middle',
                                (textOutput("average_sold_price_1year")),
                                (textOutput("number_of_sales_1year")),
                                (textOutput("average_sold_price_5year")),
                                (textOutput("number_of_sales_5year")),
                            ),
                        )
               )
    ) # Navbar Theme
    
) ## finish Navbar Panel