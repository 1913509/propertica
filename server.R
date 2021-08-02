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
library(shiny)

#Importing model
model <- readRDS(file = "svmmodel.rds")

# Loading Datasets
R <- read.csv('data.csv')
school <- read.csv('Lschools.csv', stringsAsFactors = TRUE)


train_set <- read.csv('y_train.csv')
test_set <- read.csv('y_test.csv')

levels(test_set$postcode) <- levels(train_set$postcode)
levels(test_set$estate_type) <- levels(train_set$estate_type)
levels(test_set$year) <- levels(train_set$year)
levels(test_set$property_type) <- levels(train_set$property_type)
levels(test_set$build) <- levels(train_set$build)


# To calculate mean absolute error (MAE) and Root Mean Square Error (RMSE)
y_pred = predict(model, newdata = test_set)
mae_svm = mae(test_set[[1]], y_pred)
rmse_svm = rmse(test_set[[1]], y_pred)


#Opencage Maps & ZOOPLA API Key
Sys.setenv(OPENCAGE_KEY = "10f3bcc701d54770b5a7756c51cd3d97")
zoopla_key <- "fdg663bqfabeyvgv5cyx5xct"

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
        #test_pred <- rbind.fill(test_pred,values)
        
        
        #Single preiction using the  model
        a$result <-  round(predict(model, newdata = test_pred[nrow(test_pred),]), digits = 0)
    })
    
    
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
                        round(a$result - mae_svm, digits = 0), 
                        round(a$result + mae_svm, digits = 0)))
    })  
    
    
    
    ## Crime   
    output$map <- renderLeaflet({
        leaflet() %>%
            addProviderTiles("CartoDB.Positron") %>%
            setView(-3, 54.3, zoom = 5)
    })
    
    observeEvent(input$geo, {
        
        # Progress Bar
        withProgress(
            message = 'Fetching data from UK Police database..',
            value = 1/5, {
                
                #--------- Zoopla data to access postcodes------------#        
                code <- input$geoloc
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
                    pal <- c("#1B9E77", "#D95F02", "#7570B3", "#E7298A", "#66A61E", "#E6AB02")
                    leafPal <- colorFactor(pal, crime$top5)
                    
                    leafletProxy("map", data = crime) %>%
                        setView(long, lat, zoom = 14) %>% 
                        clearShapes() %>% 
                        clearControls() %>% 
                        addCircles(~long, ~lat, stroke = FALSE, fill = TRUE, fillOpacity = .7, 
                                   color = ~leafPal(top5), label = ~category, radius = 30) %>% 
                        addLegend("bottomright", pal = leafPal, values = ~top5, title = "Category")
                    
                    incProgress(1/5)
                    
                    output$selectstat <- renderHighchart({
                        
                        hchart(crime_rank, "bar", hcaes(category, n)) %>% 
                            hc_colors("SteelBlue") %>% 
                            hc_title(text = paste("Crimes in", isolate(place))) %>% 
                            hc_subtitle(text = unique(crime$date)) %>% 
                            hc_xAxis(title = list(text = ""), gridLineWidth = 0, minorGridLineWidth = 0) %>% 
                            hc_yAxis(title = list(text = "Incidents"), gridLineWidth = 0, minorGridLineWidth = 0) %>%
                            hc_legend(enabled = FALSE) %>% 
                            hc_tooltip(pointFormat = "Incidents: <b>{point.y}</b>") %>% 
                            hc_plotOptions(series = list(cursor = "default")) %>% 
                            hc_add_theme(hc_theme_smpl()) %>% 
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
    
    ## Analysis of the Area
    output$areamap <- renderLeaflet({
        leaflet() %>%
            addProviderTiles("CartoDB.Positron") %>%
            setView(-3, 54.3, zoom = 5)
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
                n <- nrow(school[school$POSTCODE == code,])
                
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
                    
                    pal <- c("#E7298A", "#66A61E", "#E6AB02","#1B9E77", "#D95F02", "#7570B3")
                    leafPal <- colorFactor(pal, school$PHASE)
                    
                    leafletProxy("areamap", data = school) %>%
                        setView(long, lat, zoom = 14) %>% 
                        addPopups(long, lat, popup = code) %>%
                        addCircleMarkers(long, lat, color = '#f7f0d2', radius = 200, stroke = TRUE, fillOpacity = 0.5) %>%
                        clearShapes() %>% 
                        clearControls() %>% 
                        addCircles(~long, ~lat, stroke = TRUE, fill = TRUE, fillOpacity = .7, 
                                   color = ~leafPal(PHASE), label = ~PHASE, radius = 20) %>% 
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
    
    
}