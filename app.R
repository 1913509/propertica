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

# Loading Datasets
R <- read.csv('data.csv')
schools <- read.csv('Lschools.csv', stringsAsFactors = TRUE)


train_set <- read.csv('y_train.csv')
test_set <- read.csv('y_test.csv')


levels(test_set$estate_type) <- levels(train_set$estate_type)
levels(test_set$year) <- levels(train_set$year)
levels(test_set$property_type) <- levels(train_set$property_type)
levels(test_set$build) <- levels(train_set$build)
levels(test_set$postcode) <- levels(train_set$postcode)


#Importing model
model <- readRDS(file = "rfmodel.rds")


# To calculate mean absolute error (MAE) and Root Mean Square Error (RMSE)
y_pred = predict(model, newdata = test_set)
mae_rf = mae(test_set[[1]], y_pred)
rmse_rf = rmse(test_set[[1]], y_pred)

#Opencage Maps & ZOOPLA API Key
Sys.setenv(OPENCAGE_KEY = "10f3bcc701d54770b5a7756c51cd3d97")
zoopla_key <- "fdg663bqfabeyvgv5cyx5xct"


## Zoopla Data Access



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
                        setBackgroundImage(src = "9.jpg", shinydashboard = FALSE),
                        
                        absolutePanel(id = "logo", class = "card", bottom = 50, left = 50, width = 50, fixed=TRUE, draggable = FALSE, height = "auto",
                                      tags$a(href='http://rgu.ac.uk', tags$img(src='rgubw1.png',height='50',width='auto'))),
                        
                        absolutePanel(id = "footer", class = "card", bottom = 10, left = 50, width = 'auto', fixed=TRUE, draggable = FALSE, height = "auto",
                                      tags$h6('Designed, coded by Rahim Bux to fulfil the requirement of MSc Project')),
                        
                       
                            
                        absolutePanel(id = "controls", class = "panel panel-default",
                                          top = 72, left = 55, width = 280, fixed=TRUE,
                                          draggable = FALSE, height = "auto",
                                          
                                          h2("Input Preferences"),
                                         
                                          selectInput("p_year","Select time period:", choices = 2017:as.numeric(format(Sys.Date(),"%Y"))),
                                          selectInput("postcode", "Select a Postcode", 
                                                      c('E1'='1','E2'='13', 'E3'='14', 'E4'='15', 'E5'='16', 'E6'='17', 'E7'='18', 'E8'='19','E9'='20', 'E10'='2','E11'='3', 'E12'='4', 'E13'='5', 'E14'='6', 'E15'='7', 'E16'='8', 'E17'='9', 'E18'='10', 'E20'='11')),
                                          selectInput('p_property_type', 'Property Type', c('Detached' = '1', 'Flats'= '2',  'Semi-Detached' = '3',  'Others' = '4', 'Terraced' = '5')),
                                          selectInput('p_build', 'Build', c('Old Build' = '0', 'New Build' = '1')),
                                          selectInput('p_estate_type', 'Estate Type', c('Leasehold' = '1', 'Freehold' = '0')),
                                         
                                          
                                          actionButton("go", "PREDICT!", class = "btn-primary", icon = icon('fas fa-search-dollar')),
                                          
                         ),
                        absolutePanel(
                            #id = "controls", class = "panel panel-default",
                            top = 50, left = 340, width = 275, fixed=TRUE, align = "center",
                            draggable = FALSE, height = "auto",
                            tags$h2(textOutput("value")),
                            tags$p(textOutput('range'),tags$p("Price Range"),
                                   tags$p(helpText(sprintf('The prediction is based on a RF supervised machine learning model. Furthermore, the models deliver a mean absolute error (MAE) of %s total number of Price, and a root mean squared error (RMSE) of %s total number of Price', round(mae_rf, digits = 0), round(rmse_rf, digits = 0))))),
                                    ),
                        absolutePanel
                        (bottom = 300, left = 300, width = 460, fixed=FALSE, draggable = FALSE, height = 100,
                            
                            highchartOutput("chart")),
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
                                          
                                          selectInput("geoloc", "Select a Postcode from E1 to E20:", c('E1', 'E2', 'E3', 'E4', 'E5' , 'E6', 'E7', 'E8', 'E9', 'E10', 'E11', 'E12', 'E13' , 'E14' , 'E15', 'E16', 'E17' , 'E18', 'E19', 'E20')),
                                        
                                        
                                          actionButton("submit", "ANALYSIS", class = "btn-primary", icon = icon('fas fa-chart-bar')),
        
                                      
                            ),
                           
                            
                            # Outputs Charts from Zoopla
                            absolutePanel
                            ( top = 72, right = 5, width = 370, fixed=TRUE, draggable = TRUE, height = "auto",
                                
                                htmlOutput("myImage"),
                                htmlOutput("myImage2"),
                            ),
                            
                            absolutePanel(id = "logo", class = "card", bottom = 50, left = 50, width = 50, fixed=TRUE, draggable = FALSE, height = "auto",
                                          tags$a(href='http://rgu.ac.uk', tags$img(src='csdmlogo.png',height='50',width='auto'))),
                            
                            absolutePanel(id = 'footer', class = "card", bottom = 10, left = 50, width = 'auto', fixed=TRUE, draggable = FALSE, height = "auto",
                                          tags$p('Designed, coded by Rahim Bux to fulfil the requirement of MSc Project')),
                            
                            absolutePanel
                            (bottom = 45, right = 120, width = 180, fixed=FALSE, draggable = FALSE, height = 100, align = 'middle',
                                
                                valueBoxOutput("number")),
                            
                            absolutePanel
                            ( id = "controls2", class = "panel panel-default",
                                bottom = 25, right =355, width = 250, fixed=FALSE, draggable = TRUE, height = 'auto', align = 'middle',
                                (textOutput("average_sold_price_1year")),
                                (textOutput("number_of_sales_1year")),
                                (textOutput("average_sold_price_5year")),
                                (textOutput("number_of_sales_5year")),
                                
                            ),
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
                                          
                                          textInput('cdate', 'Year and Month:', placeholder = "eg: 2019-01"),
                                          selectInput("geocode", "Select a Postcode from E1 to E20:", c('E1', 'E2', 'E3', 'E4', 'E5' , 'E6', 'E7', 'E8', 'E9', 'E10', 'E11', 'E12', 'E13' , 'E14' , 'E15', 'E16', 'E17' , 'E18', 'E19', 'E20')),
                                          
                                         
                                          actionButton("crime", "SEARCH", class = "btn-primary", icon = icon('fas fa-search-location')),
                                          
                                          ),
                            
                            absolutePanel(
                                #id = "controls1", class = "panel panel-default",
                                          top = 72, right = 20, width = 250, fixed=TRUE,
                                          draggable = FALSE, height = "auto",
                                         
                                          # Output Crime Chart
                                          highchartOutput("selectstat")),
                            
                            absolutePanel(id = "logo", class = "card", bottom = 50, left = 50, width = 50, fixed=TRUE, draggable = FALSE, height = "auto",
                                          tags$a(href='http://rgu.ac.uk', tags$img(src='csdmlogo.png',height='50',width='auto'))),
                            
                            
                            absolutePanel(id = 'footer', class = "card", bottom = 10, left = 50, width = 'auto', fixed=TRUE, draggable = FALSE, height = "auto",
                                          tags$p('Designed, coded by Rahim Bux to fulfil the requirement of MSc Project')),
                            
                            
                        
                        ),
                        
                        
                    )
    )
               
            ) ## finish Navbar Panel
    

# SERVER
server <- function(input, output, session) {

    
    #Prediction model
    #React value when using the action button
    a <- reactiveValues(result = NULL)
    
    observeEvent(input$go, {
        
    
        #Copy of the test data without the dependent variable
        test_pred <- test_set[-1]
        
        #Dataframe for the single prediction
        values = data.frame(postcode = as.numeric(input$postcode),
                            year = as.numeric(input$p_year),
                            property_type = as.numeric(input$p_property_type), 
                            build = as.numeric(input$p_build),
                            estate_type = as.numeric(input$p_estate_type))
        
        
        #Inclued the values into the new data
        test_pred <- rbind(test_pred,values)
        
        #Single preiction using the  model
        a$result <-  round(predict(model, newdata = test_pred[nrow(test_pred),]), digits = 0)
    
    
    
    output$value <- renderText({
        #Display the prediction value
        paste("£",
        formatC(round(as.numeric(a$result), 0), 
                big.mark = ",", 
                digits = nchar(as.character(round(as.numeric(a$result), 0)))))
                                })
    
    output$range <- renderText({
        #Display the range of prediction value using the MAE value
        input$go
        isolate(sprintf('(%s) - (%s)', 
                        round(a$result - mae_rf, digits = 0), 
                        round(a$result + mae_rf, digits = 0)))
                                })
    })
    
    ## Analysis of the Area
    output$areamap <- renderLeaflet({
        leaflet() %>%
            addProviderTiles("CartoDB.Positron") %>%
            setView(-3, 54.3, zoom = 6)
    })
    
    observeEvent(input$submit, {
        
        #--------- Zoopla data access for postcodes------------#        
        code <- input$geoloc
        
        z <- average_area_sold_price(code, "outcode", zoopla_key)
        g <- area_value_graphs (code, zoopla_key)
        s <- local_info_graphs(code, zoopla_key)
        
        # Progress bar while fetching data from Zoopla
        withProgress(
            message = 'Fetching data from Zoopla',
            value = 1/5, {
                
                
                #Display the home value graph (Zoopla)
                output$myImage <- renderText({
                    paste(
                    img (src=(g$home_values_graph_url), width = 350))

                })
                
                #Display the value trend graph (Zoopla)
                output$myImage2 <- renderText({
                    paste(
                        img (src=(g$value_trend_graph_url), width = 350))
                    
                })
                
                
                #Display number of Schools in the Area
                n <- nrow(schools[schools$POSTCODE == code,])
                
                output$number <- renderValueBox({
                    valueBox(paste0(n),'Total Schools')
                })
                
                #Display the average sold prices (Zoopla)
                output$average_sold_price_1year <- renderText({
                    paste("1 year Avg: Sold Price £",z$average_sold_price_1year)
                })
                
                output$number_of_sales_1year <- renderText({
                    paste('Number of Sells', z$number_of_sales_1year)
                })
                
                output$average_sold_price_5year <- renderText({
                    paste("5 years Avg: Sold Price £",z$average_sold_price_5year)
                })
                
                output$number_of_sales_5year <- renderText({
                    paste('Number of Sells' , z$number_of_sales_5year)
                })
 
                
                lat <- g$latitude 
                long <- g$longitude 
                
                place <- opencage_reverse(lat, long, countrycode = "GB")
                
                #Schools in the area shown on map
                tryCatch({
                    
                    pal <- c("#1B9E77", "#D95F02", "#7570B3", "#E7298A", "#66A61E", "#E6AB02")
                    leafPal <- colorFactor(pal, schools$PHASE)
                    
                    leafletProxy("areamap", data = schools) %>%
                        setView(long, lat, zoom = 14) %>% 
                        addPopups(long, lat, popup = code) %>%
                        addCircleMarkers(long, lat, color = '#f7f0d2', radius = 200, stroke = TRUE, fillOpacity = 0.5) %>%
                        clearShapes() %>% 
                        clearControls() %>% 
                        addCircles(~long, ~lat, stroke = FALSE, fill = TRUE, fillOpacity = 1, 
                                   color = ~leafPal(PHASE), label = ~PHASE, radius = 30) %>% 
                        addLegend("bottomright", pal = leafPal, values = ~PHASE, title = "School Type")
                    
                    
                    incProgress(1/5)
                },
                
                
                error = function(e) {
                    showModal(modalDialog(title = "Sorry!", 
                                          tags$p("No data found."),
                                          tags$p("Please try again!")))
                }
                )
                
                incProgress(1/5)
                
            })
        
    })
    
    ## Crime   
    output$map <- renderLeaflet({
        leaflet() %>%
            addProviderTiles("CartoDB.Positron") %>%
            setView(-3, 54.3, zoom = 6)
    })
    
    observeEvent(input$crime, {
        
        # Progress Bar
        withProgress(
            message = 'Fetching data from UK Police database..',
            value = 1/5, {
                
                #--------- Zoopla data to access postcodes------------#        
                code <- input$geocode
                g <- area_value_graphs(code, zoopla_key)
                #----------------------- end---------------------#   
                
                lat <- g$latitude 
                long <- g$longitude
                
                place <- opencage_reverse(lat, long, countrycode = "GB")
                place <- as.character(place$results$components.geocode[1])
                
                incProgress(1/5)
                
                tryCatch({
                    crimedate = input$cdate
                    crime <- ukp_crime(lat, long, date = crimedate) %>% 
                        mutate(date = as.Date(paste0(date, "-01"))) %>% 
                        mutate(date = format(date, format = "%B %Y")) %>% 
                        mutate(top5 = fct_lump(factor(category), n = 5, other_level = "hover for detail"))
                    
                    
                    crime_rank <- crime %>% #Top 5 Crimes in the area
                        count(category) %>%
                        arrange(desc(n))
                    
                },
                error = function(e) {
                    crime <- NULL
                }
                )
                
                incProgress(1/5)
                
                tryCatch({
                    #Top 5 Crimes in the area show on map
                    pal <- c("#1B9E77", "#D95F02", "#7570B3", "#E7298A", "#66A61E", "#E6AB02","#1B9E77", "#D95F02", "#7570B3", "#E7298A", "#66A61E", "#E6AB02","#1B9E77", "#D95F02")
                    leafPal <- colorFactor(pal, crime$top5)
                    
                    leafletProxy("map", data = crime) %>%
                        setView(long, lat, zoom = 14) %>% 
                        clearShapes() %>% 
                        clearControls() %>% 
                        addCircles(~long, ~lat, stroke = FALSE, fill = TRUE, fillOpacity = .7, 
                                   color = ~leafPal(top5), label = ~category, radius = 30)
                       #addLegend("bottomright", pal = leafPal, values = ~top5, title = "Category")
                    
                    incProgress(1/5)
                    
                    output$selectstat <- renderHighchart({
                        
                        hchart(crime_rank, "bar", hcaes(category, n, color = c("#1B9E77", "#D95F02", "#7570B3", "#E7298A", "#66A61E", "#E6AB02","#1B9E77", "#D95F02", "#7570B3", "#E7298A", "#66A61E", "#E6AB02","#1B9E77", "#D95F02"))) %>% 
                        #    hc_colors('category') %>% 
                            hc_title(text = paste("Crimes data in", isolate(code))) %>% 
                            hc_subtitle(text = unique(crime$date)) %>% 
                            hc_xAxis(title = list(text = ""), gridLineWidth = 0, minorGridLineWidth = 0) %>% 
                            hc_yAxis(title = list(text = "Incidents"), gridLineWidth = 0, minorGridLineWidth = 0) %>%
                            hc_legend(enabled = FALSE) %>% 
                            hc_tooltip(pointFormat = "Incidents: <b>{point.y}</b>") %>% 
                            hc_plotOptions(series = list(cursor = "default")) %>% 
                            hc_add_theme(hc_theme_economist()) %>% 
                            hc_chart(backgroundColor = "transparent")
                        
                    })
                },
                
                error = function(e) {
                    showModal(modalDialog(title = "Sorry!", 
                                          tags$p("No data found."),
                                          tags$p("Please try again!")))
                }
                )
                
                incProgress(1/5)
                
            })
    })
    
}


# Run the application 
shinyApp(ui = ui, server = server)
