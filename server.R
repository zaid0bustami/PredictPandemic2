library(tidyverse)

Sys.setlocale("LC_TIME", "C")

#importing symptom trends data from Naver
symptoms1 <- as.data.frame(read.csv("data/chills_cough_eyepain_aguesia_anosmia.csv"))[-(1:5),]
colnames(symptoms1) = symptoms1[1,]
symptoms1 <- symptoms1[-1,] %>%
  mutate_at(c("Chills", "Cough", "Eye Pain", "Anosmia", "Aguesia"), as.double)

symptoms2 <- as.data.frame(read.csv("data/headache_fever_nasalcongestion_shortnessofbreath_rhinorrhea.csv"))[-(1:5),]
colnames(symptoms2) = symptoms2[1,]
symptoms2 <- symptoms2[-1,] %>%
  mutate_at(c("Headache", "Fever", "Nose congestion", "Rhinorrhea", "Shortness of breath"), as.double)

symptoms3 <- as.data.frame(read.csv("data/sorethroat.csv"))[-(1:5),]
colnames(symptoms3) = symptoms3[1,]
symptoms3 <- symptoms3[-1,] %>%
  mutate_at(c("Sore throat"), as.double)

# importing Korean COVID-19 data from Our World in Data
covidData <- as.data.frame(read.csv("data/Korea-Covid-Data.csv")) %>% 
  separate(col = "date", sep = "/", into = c("day", "month", "year")) %>% 
  relocate(year, .before = "day")

# Reformating dates to match subsequent code
for (r in 1:nrow(covidData)){
  n <- covidData$day[r]
  m <- covidData$month[r]
  covidData$day[r] <- ifelse(nchar(n) < 2, paste0("0", n), n)
  covidData$month[r] <- ifelse(nchar(m) < 2, paste0("0", m), m)
}

covidData <- covidData %>% 
  unite(year, day, month, col = "Date", sep = "-") %>% 
  select(Date, 
         total_cases, new_cases_smoothed, 
         total_deaths, new_deaths_smoothed,
         new_cases_smoothed_per_million, total_cases_per_million,  
         new_deaths_smoothed_per_million, total_deaths_per_million)

# Joining symptoms data frames into a singular data frame
data2 <- symptoms1 %>% 
  left_join(symptoms2, by = "Date") %>% 
  left_join(symptoms3, by = "Date") %>% 
  mutate("STATE" = "Korea, South") %>% 
  relocate(STATE, .before = "Date")

# Joining symptoms data frame with COVID-19 data frame
data3 <- data2 %>% 
  left_join(covidData, by = "Date") %>% 
  mutate(day = 1:nrow(data2)) %>% 
  relocate(day, .after = "Date")

# data3 <- as.data.frame(read.csv("/home/adminuser/python/files_output/all_time_series__johns-hopkins__ihme.csv", header=TRUE, sep=","))

data3$STATE <- gsub("_", " ", data3$STATE, fixed=TRUE)
data3$STATE <- gsub("US", "United States of America", data3$STATE, fixed=TRUE)
data3 <- data3 %>% arrange(STATE)
countries3 <- unique(data3$STATE)
codes3 <- data3$CODE_2
CODES_3 <- data3$CODE_3
codes3 <- tolower(codes3)
codes3 <- unique(codes3)
flags3 <- c(paste0("https://cdn.rawgit.com/lipis/flag-icon-css/master/flags/4x3/",codes3,".svg"))
# data3 <- data3[,-c(3,4,5,22,23,24,25,26,31,32)] #remove codes and population data
data3[is.na(data3)] <- 0
colnames(data3)<- c("STATE", "DATE", "DAY", "Ageusia", "Anosmia", "Chills", 
                    "Cough", "Eye Pain", "Fever", "Headache", "Nasal Cong.", 
                    "Rhinorrea", "Short Breath", "Sore Throat", "Cases Cum.", 
                    "Cases", "Deaths Cum.", "Deaths", "Cases Norm.", 
                    "Cases Cum. Norm.", "Deaths Norm.", "Deaths Cum. Norm.")
data3$DATE <- as.Date(data3$DATE)
# data3 <- cbind(data3[,1:14],abs(data3[,15:22]))

server <- function(input, output) { 
    
#### view in home ####    
    
    output$countries_box_home <- renderUI({
        countries = countries3
        codes = codes3
        flags = flags3
        
        box(title = "Select the country of interest:", 
            width = 12, status = "primary", solidHeader = TRUE, align = "center",
            #???style = "overflow-x: scroll",
            shiny::flowLayout(
                cellArgs = list(#min-width: 500px;
                    style = "
                     min-width: 100px;
                     width: auto;
                     height: auto;
                     border: 1px solid white;
                     padding: 10px;
                     margin: 10px;"),
                pickerInput("countries_box_home",# "Pick a country:", #multiple = TRUE, selected = NULL,
                            choices = countries,
                            selected = "Italy", #options = list(height = 10)
                            options = pickerOptions(width = 220, liveSearch=T, `actions-box` = FALSE, `none-selected-text` = "Pick your country of interest"),
                            choicesOpt = list(content = mapply(countries, flags, FUN = function(country, flagUrl) {
                                HTML(paste(
                                    tags$img(src=flagUrl, width=20, height=15),
                                    country
                                ))
                            }, SIMPLIFY = FALSE, USE.NAMES = FALSE)
                            )))
        )
    })
    
    output$flag_home <- renderUI({
        #req(!is.null(input$countries_box_home))
        countries3 <- unique(data3$STATE)
        n <- match(input$countries_box_home,countries3)
        HTML(paste(
            tags$img(src=flags3[n], width=100, height=70)#,
            #"Country"
        ))
    })
    
    output$plot_home <- renderUI({
        no_countries = c("Brunei", "Central African Republic", "Fiji","Holy See", "Marshall Islands", "Mauritius",
                         "Singapore", "Tajikistan", "Tanzania", "Vanuatu", "Vietnam")
        
        if (is.null(input$countries_box_home)) {
            tags$div(
                tags$h2(style="color:red", "Please wait, we're building the model")
            )
            
        } else if (input$countries_box_home %in% no_countries) {
            tags$div(
                tags$h2(style="color:red", "Please select another country. Data are insufficient.")
            )
        } else {
        mydata <- as.data.frame(data3)
        reactive_data <- as.data.frame(mydata %>% filter(STATE == input$countries_box_home)) #input$countries_box
        reactive_data <- reactive_data[, colSums(reactive_data != 0) > 0]
        reactive_data[sapply(reactive_data, function(x) length(unique(na.omit(x)))) > 1]
        test <- reactive_data    #() %>% filter(DATE >= input$date_input[1] & DATE <=input$date_input[2])
        reactiveDf <- test
        data <- reactiveDf[,-1]
        # data <- data[,-1]########edit#############
        data <- data[,sapply(data, function(v) var(v, na.rm=TRUE)!=0)]
        my_vars <- colnames(data[,-c(1:2)])
        data <- as.data.frame(cbind(data[,c(1:2)],zoo::rollmean(data[,-c(1,2)], 1, fill = "extend")))# input$ma_selection
        data2 <- data[,-c(1:2)]
        colnames(data2) <- my_vars #
        data <- as.data.frame(cbind(data[,c(1:2)],data2)) #
        data <- as.data.frame(data)
        data <- data[complete.cases(data[,]),] #
        my_ts_def <- data
        C <- colnames(my_ts_def)
        nums <- nrow(my_ts_def)
        prova <- my_ts_def[c((nums-30):nums),]
        prova <- prova[, colSums(prova != 0) > 0]
        D <- colnames(prova)
        aa<-setdiff(C,D)
        my_ts_def<-my_ts_def[, !(colnames(my_ts_def) %in% aa), drop = FALSE]
        pca.model <- prcomp(my_ts_def[,-c(1:2)], scale=TRUE, center=TRUE, rank.=2)
        scores<-pca.model$x
        scores <- as.data.frame(scores)
        colnames(scores)=c("PC1","PC2")
        scores_old <- scores     #PCA_model()$scores
        
        
        pca2 <- prcomp(my_ts_def[c((nums-30):nums),-c(1:2)], scale=TRUE, center=TRUE, rank.=2) #input$refine_prediction
        my_new_ts <- my_ts_def[c((nums-30):nums),] #input$refine_prediction
        scores <- pca2$x
        
        datz <- my_ts_def[c((nums-30):nums),]
        pred_data <- as_tibble(scores)
        startDate <- as.Date(min(datz$DATE))
        endDate <- as.Date(max(datz$DATE))
        days <- seq(startDate, endDate, "7 days") ####edit####
        pred_data <- cbind(days,pred_data)
        pred_data <- as_tsibble(pred_data)
        
        fit <- pred_data %>%
            model(
                ets = ETS(PC1)#, #~ trend("A")),
            )
        
        fit2 <- pred_data %>%
            model(
                ets = ETS(PC2)#, # ~ trend("A")),
            )
        
        num_days <- 14  #input$predict_days
        h_pred <- as.character(paste(num_days,"days"))

        ####PCA Plot####
        # k means clustering on PC1 and PC2
        df <- kmeans(pred_data %>% column_to_rownames("days"), centers = 3)$cluster %>%
          as.data.frame() %>%
          cbind(pred_data) %>%
          mutate(cluster = as.character(.))
        
        #scatterplot
        p <- df %>%
          ggplot(aes(x = PC1, y = PC2, color = cluster)) +
            geom_point(size = 3) +
            labs(title = "Association of PC1 with PC2 for Naver Data +
                          South Korean Data on COVID-19 Cases/Fatalities") +
            scale_color_viridis_d(end = 0.75) +
            theme_bw()
        print(p)
        
        #bar plot on symptoms
        q <- df %>% 
          left_join(data3, by = c("days" = "DATE")) %>% 
          pivot_longer(cols = c("Ageusia", "Anosmia", "Chills", "Cough", "Eye Pain", "Fever", "Headache", "Nasal Cong.", "Rhinorrea", "Short Breath", "Sore Throat"), 
                       names_to = "Symptom", values_to = "Value") %>% 
          ggplot(aes(x = days, y = Value, fill = cluster)) + 
            geom_col() + scale_fill_viridis_d(end = 0.75) + 
            facet_wrap("Symptom", scales = "free_y")
        print(q)
        
        #bar plot on incidence and mortality
        r <- df %>% 
          left_join(data3, by = c("days" = "DATE")) %>% 
          pivot_longer(cols = c("Cases Cum.", 
                                "Cases", "Deaths Cum.", "Deaths", "Cases Norm.", 
                                "Cases Cum. Norm.", "Deaths Norm.", "Deaths Cum. Norm."), 
                       names_to = "Statistic", values_to = "Value") %>% 
          ggplot(aes(x = days, y = Value, fill = cluster)) + 
          geom_col() + scale_fill_viridis_d(end = 0.75) + 
          facet_wrap("Statistic", scales = "free_y")
        print(r)
        ####end PCA Plot####

        fc <- forecast(fit, h = h_pred)
        
        fc2 <- fit2 %>% forecast(h = h_pred)
        
        days_etsPC1 <- fc %>% 
            as.data.frame(fc)%>%
            filter(.model == "ets")%>%
            select(days, .mean)
        
        etsPC2 <- fc2 %>% 
            as.data.frame(fc2)%>%
            filter(.model == "ets")%>%
            select(.mean)
        
        ets_means <- cbind(days_etsPC1,etsPC2)       
        colnames(ets_means) <- c("Date","PC1","PC2")
        
        ets_prediction <- ets_means
        
        ets_pred <- as.data.frame(t(t(as.matrix(ets_prediction[,c(2,3)]) %*% t(pca2$rotation)) * pca2$scale + pca2$center))
        model <- c(rep("ets",nrow(ets_pred)))
        Date2 <- c(ets_prediction$Date)
        ets_pred <- cbind(ets_pred, ets_prediction[,c(2,3)])
        reversed_prediction <- ets_pred
        reversed_prediction <- cbind(Date2, model, reversed_prediction)
        colnames(reversed_prediction) <- c("DATE", "model", colnames(ets_pred))
        new <- my_ts_def[c((nums-30):nums),]
        new <- cbind(new,scores)  #PCA_model()$
        colnames(new)[2] <- "model"
        ets_reversed <- subset(reversed_prediction, model == "ets")
        data.table::setDT(ets_reversed)[Cases < 0, Cases := 0]
        link_ets <- rbind(new[nrow(new),], ets_reversed[1,])
        data.table::setDT(link_ets)[Cases < 0, Cases := 0]
        data.table::setDT(my_ts_def)[Cases < 0, Cases := 0]
        
        min_Date <- max(my_ts_def$DATE)-270
        max_Date <- max(my_ts_def$DATE)+30
        
        p <- plot_ly(x = my_ts_def$DATE, y = my_ts_def[["Cases"]], type = 'scatter', mode = 'lines',
                     line = list(color = 'green'), #input$predicted_new_variables
                     showlegend = TRUE, name = paste0('Measured ', "Cases"))%>% 
            add_trace(x = link_ets$DATE, y = link_ets[["Cases"]], type = 'scatter', mode = 'lines',
                      line = list(color = 'blue'), showlegend = FALSE)%>%
            add_trace(x = ets_reversed$DATE, y = ets_reversed[["Cases"]], type = 'scatter', mode = 'lines',
                      line = list(color = 'blue'), showlegend = TRUE, name = paste0('Predicted future cases'))%>% 
            layout(#title = "Predicted Cases", 
                   xaxis = list(range = c(min_Date, max_Date),showgrid = F), 
                   yaxis = list(showgrid = F,range = c(0, max(my_ts_def[["Cases"]]*1.1))),
                   dragmode = "pan", height = 400, width = 1400)
        
        box(title = paste("View the prediction on new cases for", input$countries_box_home), 
            width = 12, status = "danger", solidHeader = TRUE, align = "center", collapsible = TRUE, collapsed = FALSE,
            style = "overflow-x: scroll",
            shiny::flowLayout(
                cellArgs = list(
                    style = "min-width: 790px;
                     min-height: 400px;
                     width: auto;
                     height: auto;
                     border: 1px solid white;
                     padding: 10px;
                     margin: 10px;"),
                renderPlotly(p))
        )
        }
    })
    
#### view time series ####
    
    reactive_data <- reactive({
        mydata <- as.data.frame(data3)#
        reactive_data <- as.data.frame(mydata %>% filter(STATE == input$countries_box))#
        reactive_data <- reactive_data[, colSums(reactive_data != 0) > 0]
        reactive_data[sapply(reactive_data, function(x) length(unique(na.omit(x)))) > 1]
        C <- colnames(reactive_data)
        nums <- nrow(reactive_data)
        prova <- reactive_data[c((nums-60):nums),]
        prova <- prova[, colSums(prova != 0) > 0]
        D <- colnames(prova)
        aa<-setdiff(C,D)
        reactive_data<-reactive_data[, !(colnames(reactive_data) %in% aa), drop = FALSE]
        reactive_data
    })
    
    output$countries_box <- renderUI({
        countries = countries3
        codes = codes3
        flags = flags3
        
        box(title = "Select the country of interest:", 
            width = 12, status = "primary", solidHeader = TRUE, align = "center",
            #style = "overflow-x: scroll",
            shiny::flowLayout(
                cellArgs = list(#min-width: 500px;
                    style = "
                     width: auto;
                     height: auto;
                     border: 1px solid white;
                     padding: 10px;
                     margin: 10px;"),
                pickerInput("countries_box", #"Pick a country:", #multiple = TRUE, selected = NULL,
                            choices = countries,
                            selected = input$countries_box_home,
                            options = pickerOptions(width = 220, liveSearch=T, `actions-box` = FALSE, `none-selected-text` = "Pick your country of interest"),
                            choicesOpt = list(content = mapply(countries, flags, FUN = function(country, flagUrl) {
                                HTML(paste(
                                    tags$img(src=flagUrl, width=20, height=15),
                                    country
                                ))
                            }, SIMPLIFY = FALSE, USE.NAMES = FALSE)
                            )))
        )
    })
    
    output$date_input <- renderUI({
        #req(!is.null(input$countries_box))
        no_countries = c("Brunei", "Central African Republic", "Fiji","Holy See", "Marshall Islands", "Mauritius",
                         "Singapore", "Tajikistan", "Tanzania", "Vanuatu", "Vietnam")
        
        if (is.null(input$countries_box)) {
            return(NULL)
        } else if (input$countries_box %in% no_countries) {
            return(NULL)
        } else {
        dates<-as.Date(reactive_data()$DATE)
        min_date <- min(dates)
        max_date <- max(dates)
        
        box(title = "Refine the time range:", 
            width = 12, status = "warning", solidHeader = TRUE, align = "center",
            style = "overflow-x: scroll",
            shiny::flowLayout(
                cellArgs = list(#min-width: 500px;
                    style = "
                     width: auto;
                     height: auto;
                     border: 1px solid white;
                     padding: 10px;
                     margin: 10px;"),
                dateRangeInput('date_input',
                               label = NULL,
                               start = min_date, end = max_date, width = 400))
        )
        }
    })
    
    reactiveDf <- reactive({
        #req(!is.null(input$date_input))
        test <- reactive_data() %>% filter(DATE >= input$date_input[1] & DATE <=input$date_input[2])
        reactiveDf <- test
    })
    
    output$tbl = renderUI({
        no_countries = c("Brunei", "Central African Republic", "Fiji","Holy See", "Marshall Islands", "Mauritius",
                         "Singapore", "Tajikistan", "Tanzania", "Vanuatu", "Vietnam")
        
        if (is.null(input$countries_box)) {
            tags$div(
                tags$h2(style="color:red", "Please wait, we're building the model")
            )
            
        } else if (input$countries_box %in% no_countries) {
            tags$div(
                tags$h2(style="color:red", "Please select another country. Data are insufficient.")
            )
        } else {
            box(title = paste0("Dataset of ", my_ts()[1,1]), 
                width = 12, status = "success", solidHeader = TRUE, collapsible = TRUE, collapsed = TRUE,
                style = "overflow-x: scroll", align = "center",
                shiny::flowLayout(
                    cellArgs = list(
                        style = "min-width: auto;
                     width: auto;
                     height: auto;
                     border: 1px solid white;
                     padding: 10px;
                     margin: 10px;"),
                    renderDT(reactiveDf()))
            )
        }
    })
    
    my_ts <- reactive({
        #req(!is.null(input$countries_box))
        my_ts <- reactiveDf()
        my_ts$DATE <- as.Date(my_ts$DATE)
        my_ts
    })
    
    output$data_select_ts <- renderUI({
        #req(!is.null(input$date_input))
        no_countries = c("Brunei", "Central African Republic", "Fiji","Holy See", "Marshall Islands", "Mauritius",
                         "Singapore", "Tajikistan", "Tanzania", "Vanuatu", "Vietnam")
        
        if (is.null(input$countries_box)) {
            return(NULL)
        } else if (input$countries_box %in% no_countries) {
            return(NULL)
        } else {
        
        ts_variables = c("Cases", "Cases Cum.", "Cases Norm.", "Cases Cum. Norm.", "Deaths", "Deaths Cum.", "Deaths Norm.", "Deaths Cum. Norm.")
        
        # box(title = "Select conventional metrics:", 
        #     width = 12, status = "info", solidHeader = TRUE,  style = "overflow-x: scroll", align = "center",
        #     shiny::flowLayout(
        #         cellArgs = list(
        #             style = "
        #              width: auto;
        #              height: auto;
        #              border: 1px solid white;
        #              padding: 10px;
        #              margin: 10px;"),
                pickerInput(
                    inputId = "data_select_ts",
                    label = "Select conventional metrics:",
                    choices = ts_variables,
                    selected = ts_variables[1],
                    multiple = FALSE,
                    #options = list(`live-search` = TRUE)
                    )
        #)
        }
    })
    
    output$data_select_ts_y2 <- renderUI({
        req(!is.null(input$data_select_ts))
        no_countries = c("Brunei", "Central African Republic", "Fiji","Holy See", "Marshall Islands", "Mauritius",
                         "Singapore", "Tajikistan", "Tanzania", "Vanuatu", "Vietnam")
        
        if (is.null(input$countries_box)) {
            return(NULL)
        } else if (input$countries_box %in% no_countries) {
            return(NULL)
        } else {
        ts_variables_2 = c("Ageusia", "Anosmia", "Chills", "Cough", "Eye Pain", "Fever", "Headache", "Nasal Cong.", "Rhinorrea", "Short Breath", "Sore Throat")
        
        # box(title = HTML(paste0("Select Google Trends topics:")), 
        #     width = 12, status = "info", solidHeader = TRUE, style = "overflow-x: scroll;overflow-y: scroll", align = "center",
        #     shiny::flowLayout(
        #         cellArgs = list(
        #             style = "
        #              width: auto;
        #              height: auto;
        #              border: 1px solid white;
        #              padding: 10px;
        #              margin: 10px;"),
                pickerInput(
                    inputId = "data_select_ts_y2",
                    label = "Select Google Trends topics:",
                    choices = ts_variables_2,
                    selected = NULL,
                    multiple = TRUE,
                    #options = list( `live-search` = TRUE)
                    )#)
        #)
        }
    })
    
    output$ma_selection <- renderUI({
        #req(!is.null(input$data_select_ts))
        no_countries = c("Brunei", "Central African Republic", "Fiji","Holy See", "Marshall Islands", "Mauritius",
                         "Singapore", "Tajikistan", "Tanzania", "Vanuatu", "Vietnam")
        
        if (is.null(input$countries_box)) {
            return(NULL)
        } else if (input$countries_box %in% no_countries) {
            return(NULL)
        } else {
        
        # box(title = "Define moving average:", 
        #     width = 12, status = "warning", solidHeader = TRUE, align = "center",
        #     style = "overflow-x: scroll",
        #     shiny::flowLayout(
        #         cellArgs = list(
        #             style = "min-width: 200px;
        #              width: auto;
        #              height: auto;
        #              border: 1px solid white;
        #              padding: 10px;
        #              margin: 10px;"),
                #chooseSliderSkin("Flat", color = "blue"),
                sliderTextInput(
                    inputId = "ma_selection",
                    label = "Define moving average (days):", #"Select moving average:", 
                    choices = c(seq(1,30)),
                    width = "300px",
                    selected = 7,
                    grid = TRUE)#)
        #)
        }
    })
    
    output$plot_ts <- renderUI({
        #req(!is.null(input$data_select_ts))
        no_countries = c("Brunei", "Central African Republic", "Fiji","Holy See", "Marshall Islands", "Mauritius",
                         "Singapore", "Tajikistan", "Tanzania", "Vanuatu", "Vietnam")
        
        if (is.null(input$countries_box)) {
            return(NULL)
        } else if (input$countries_box %in% no_countries) {
            return(NULL)
        } else {
        
        my_ts1 <- my_ts()[4:20]
        my_ts1 <- cbind(my_ts()$DATE,my_ts1)
        colnames(my_ts1)[1] <- "DATE"
        
        my_ts2 <- my_ts()[4:20]
        my_ts2 <- cbind(my_ts()$DATE,my_ts2)
        colnames(my_ts2)[1] <- "DATE"
        
        
        nums <- round(nrow(my_ts1)/7,0)
        
        if (is.null(input$data_select_ts_y2)) {
            xax <- as.Date(my_ts1$DATE)
            
            p <- plot_ly()%>%
                layout(#title = paste("Trends Over Time for", my_ts()[1,1]),
                       xaxis = list(showgrid = F,
                           type = 'date',
                           tickformat = "%d/%m/%Y", nticks = nums, tickmode = "array", tickangle = 45))#,
            
            ToAdd <- input$data_select_ts
            
            for(i in ToAdd){
                p <- p %>% add_trace(x = xax, 
                                     y = zoo::rollmean(my_ts1[[i]], input$ma_selection,fill = "extend"),
                                     name = i,
                                     type = 'scatter',
                                     mode = 'line+markers',
                                     line = list(color = i, width = 4))
                
            }
            
            if (max(my_ts1$DATE)-min(my_ts1$DATE) > 270) {
            p <- p %>%
                layout(dragmode = "pan",
                    xaxis = list(showgrid = F, type = 'date', range = c(max(my_ts1$DATE)-270, max(my_ts1$DATE)),
                                    tickformat = "%d/%m/%Y", nticks = nums, tickmode = "array", tickangle = 45), 
                    yaxis = list(title = paste("Number of", str_c(input$data_select_ts, collapse = ", ")), showgrid = FALSE),
                       legend = list(xanchor = "center", y = -0.3, x = 0.5, orientation = 'h'))
            } else if (max(my_ts1$DATE)-min(my_ts1$DATE) <= 270) {
                p <- p %>%
                    layout(dragmode = "pan",
                           xaxis = list(showgrid = F, type = 'date', range = c(min(my_ts1$DATE), max(my_ts1$DATE)),
                                        tickformat = "%d/%m/%Y", nticks = nums, tickmode = "array", tickangle = 45), 
                           yaxis = list(title = paste("Number of", str_c(input$data_select_ts, collapse = ", ")), showgrid = FALSE),
                           legend = list(xanchor = "center", y = -0.3, x = 0.5, orientation = 'h'))
            } else {
                return(NULL)
            }
           
            
        } else if (!is.null(input$data_select_ts_y2)) {
            xax <- as.Date(my_ts1$DATE)
            p <- plot_ly()%>%
                layout(#title = paste("Trends Over Time for", my_ts()[1,1]),
                    dragmode = "pan",   
                    xaxis = list(
                           type = 'date',
                           tickformat = "%d/%m/%Y", nticks = nums, tickmode = "array", tickangle = 45),
                       yaxis = list (title = c(input$data_select_ts)))
            
            ToAdd <- input$data_select_ts
            xax2 <- xax
            name <- input$data_select_ts_y2
            
            for(i in ToAdd){
                p <- p %>% add_trace(x = xax, 
                                     y = zoo::rollmean(my_ts1[[i]], input$ma_selection,fill = "extend"), 
                                     name = i,
                                     type = 'scatter',
                                     mode = 'line+markers',
                                     line = list(color = i,  width = 4))
            }
            
            ToAdd2 <- input$data_select_ts_y2
            for(i in ToAdd2 ){
                p <- p %>% add_trace(x = xax2, 
                                     y = zoo::rollmean(my_ts2[[i]], input$ma_selection,fill = "extend"),
                                     yaxis = "y2", name = i,
                                     type = 'scatter',
                                     mode = 'line+markers',
                                     line = list(color = i, width = 3))
            }
            
            if (max(my_ts1$DATE)-min(my_ts1$DATE) > 270) {
                p <- p %>%
                    layout(xaxis = list(showgrid = F, range = c(max(my_ts1$DATE)-270, max(my_ts1$DATE))), dragmode = "pan",
                           yaxis = list (title = paste("Number of", str_c(input$data_select_ts, collapse = ", ")), showgrid = F),
                           yaxis2 = list(overlaying = "y", side = "right", 
                                         title = "Google Trends interest (%)",#paste("IOT (%) of", str_c(input$data_select_ts_y2, collapse = ", ")), 
                                         showgrid = F),
                           legend = list(xanchor = "center", y = -0.3, x = 0.5, orientation = 'h'))
            } else if (max(my_ts1$DATE)-min(my_ts1$DATE) <= 270) {
                p <- p %>%
                    layout(xaxis = list(showgrid = F, range = c(min(my_ts1$DATE), max(my_ts1$DATE))), dragmode = "pan",
                           yaxis = list (title = paste("Number of", str_c(input$data_select_ts, collapse = ", ")), showgrid = F),
                           yaxis2 = list(overlaying = "y", side = "right", 
                                         title = "Google Trends interest (%)",#paste("IOT (%) of", str_c(input$data_select_ts_y2, collapse = ", ")), 
                                         showgrid = F),
                           legend = list(xanchor = "center", y = -0.3, x = 0.5, orientation = 'h'))
            } else {
                return(NULL)
            }
            
        } 
        
        box(title = paste("Time Series graph of", input$countries_box), 
            width = 12, status = "danger", solidHeader = TRUE, align = "center",
            style = "overflow-x: scroll",
            shiny::flowLayout(
                cellArgs = list(#min-width: 1150px;
                    style = "
                     min-width: 1400px;
                     width: auto;
                     height: auto;
                     border: 1px solid white;
                     padding: 10px;
                     margin: 10px;"),
                renderPlotly(p))
        ) 
        }
    })
    
    # streamgraphs ####
    
    output$stream_country_box <- renderUI({
        no_countries = c("Brunei", "Central African Republic", "Fiji","Holy See", "Marshall Islands", "Mauritius",
                         "Singapore", "Tajikistan", "Tanzania", "Vanuatu", "Vietnam")
        
        if (is.null(input$countries_box)) {
            countries = countries3
            codes = codes3
            flags = flags3
            
            box(title = "Select the country of interest:", 
                width = 12, status = "primary", solidHeader = TRUE, align = "center",
                shiny::flowLayout(
                    cellArgs = list(
                        style = "
                     width: auto;
                     height: auto;
                     border: 1px solid white;
                     padding: 10px;
                     margin: 1px;"),
                    pickerInput("stream_country_box", #"Pick a country:",
                                choices = countries,
                                selected = input$countries_box_home,
                                options = pickerOptions(width = 200, liveSearch=T, `actions-box` = FALSE, `none-selected-text` = "Pick your country of interest:"),
                                choicesOpt = list(content = mapply(countries, flags, FUN = function(country, flagUrl) {
                                    HTML(paste(
                                        tags$img(src=flagUrl, width=20, height=15),
                                        country
                                    ))
                                }, SIMPLIFY = FALSE, USE.NAMES = FALSE)
                                )))
            )
        } else if (input$countries_box %in% no_countries) {
            return(NULL)
        } else {
        countries = countries3
        codes = codes3
        flags = flags3
        
        box(title = "Select the country of interest:", 
            width = 12, status = "primary", solidHeader = TRUE, align = "center",
            shiny::flowLayout(
                cellArgs = list(
                    style = "
                     width: auto;
                     height: auto;
                     border: 1px solid white;
                     padding: 10px;
                     margin: 1px;"),
                pickerInput("stream_country_box", #"Pick a country:",
                            choices = countries,
                            selected = input$countries_box,
                            options = pickerOptions(width = 200, liveSearch=T, `actions-box` = FALSE, `none-selected-text` = "Pick your country of interest:"),
                            choicesOpt = list(content = mapply(countries, flags, FUN = function(country, flagUrl) {
                                HTML(paste(
                                    tags$img(src=flagUrl, width=20, height=15),
                                    country
                                ))
                            }, SIMPLIFY = FALSE, USE.NAMES = FALSE)
                            )))
        )
        }
    })
    
    output$stream_ma <- renderUI({
        no_countries = c("Brunei", "Central African Republic", "Fiji","Holy See", "Marshall Islands", "Mauritius",
                         "Singapore", "Tajikistan", "Tanzania", "Vanuatu", "Vietnam")
        
        if (is.null(input$countries_box)) {
            box(title = "Define moving average (days):", 
                width = 12, status = "warning", solidHeader = TRUE, align = "center", 
                #style = "overflow-x: scroll",
                shiny::flowLayout(
                    cellArgs = list(#min-width: 750px;
                        style = "
                     width: auto;
                     height: auto;
                     height: auto;
                     border: 1px solid white;
                     padding: 2px;
                     margin: 1px;"),
                    chooseSliderSkin("Flat", color = "blue"),  #112446
                    sliderTextInput(
                        inputId = "stream_ma",
                        label = NULL,#"Select moving average:", 
                        choices = c(seq(1,30)),
                        width = "300px",
                        selected = 7,
                        grid = TRUE))
            )
        } else if (input$countries_box %in% no_countries) {
            return(NULL)
        } else {
        box(title = "Define moving average (days):", 
            width = 12, status = "warning", solidHeader = TRUE, align = "center", 
            #style = "overflow-x: scroll",
            shiny::flowLayout(
                cellArgs = list(#min-width: 750px;
                    style = "
                     width: auto;
                     height: auto;
                     height: auto;
                     border: 1px solid white;
                     padding: 2px;
                     margin: 1px;"),
                chooseSliderSkin("Flat", color = "blue"),  #112446
                sliderTextInput(
                    inputId = "stream_ma",
                    label = NULL,#"Select moving average:", 
                    choices = c(seq(1,30)),
                    width = "300px",
                    selected = 7,
                    grid = TRUE))
        )
        }
    })
    
    output$plot_stream_1 <- renderUI({
        #req(!is.null(input$stream_country_box))
        
        no_countries = c("Brunei", "Central African Republic", "Fiji","Holy See", "Marshall Islands", "Mauritius",
                         "Singapore", "Tajikistan", "Tanzania", "Vanuatu", "Vietnam")
        
        if (is.null(input$countries_box)) {
            data <- data3 %>%
                filter(grepl(input$stream_country_box, STATE)) %>%#
                #filter(DATE >= input$date_input[1] & DATE <=input$date_input[2]) %>%
                select(-c("DAY","Cases Cum.", "Cases", "Deaths Cum.", "Deaths", "Cases Norm.", "Cases Cum. Norm.", "Deaths Norm.", "Deaths Cum. Norm."))
            datz <<- data
            data <- as.data.frame(cbind(data[,c(1:2)], round(zoo::rollmean(data[,-c(1,2)], input$stream_ma,fill = "extend"),2)))
            vars <- c("STATE", "DATE", "Ageusia", "Anosmia", "Chills", "Cough", "Eye Pain", "Fever", "Headache", "Nasal Cong.", "Rhinorrea", "Short Breath", "Sore Throat")
            colnames(data) <- vars
            
            p <- data %>%
                gather(Topic, IOT, all_of(vars[-c(1:2)])) %>%
                streamgraph("Topic", "IOT", "DATE", offset="silhouette",interpolate = "linear",width = 1800, height = 1200) %>%
                sg_legend(show=TRUE, label="Topics: ") %>%
                sg_axis_y(0)  %>% 
                sg_axis_x(tick_interval = "month", tick_units = 1,tick_format ="%b-%y") %>% 
                sg_fill_brewer("RdBu")#BrBG, PiYG, PRGn, PuOr, RdBu, RdGy, RdYlBu, RdYlGn, Spectral,
            #Accent, Dark2, Paired, Pastel1, Pastel2, Set1, Set2, Set3
            #Blues, BuGn, BuPu, GnBu, Greens, Greys, Oranges, OrRd, PuBu, PuBuGn, PuRd, Purples, RdPu, Reds, YlGn, YlGnBu YlOrBr, YlOrRd
            
            
            box(title = paste("Streamgraph of", input$stream_country_box), 
                width = 12, status = "danger", solidHeader = TRUE, align = "center", 
                style = "overflow-x: scroll",
                shiny::flowLayout(
                    cellArgs = list(
                        style = "min-width: 1150px;
                     width: auto;
                     height: auto;
                     border: 1px solid white;
                     padding: 10px;
                     margin: 10px;"),
                    renderStreamgraph(p)
                )
            )
        } else if (input$countries_box %in% no_countries) {
            return(NULL)
        } else {
        data <- data3 %>%
            filter(grepl(input$stream_country_box, STATE)) %>%#
            filter(DATE >= input$date_input[1] & DATE <=input$date_input[2]) %>%
            select(-c("DAY","Cases Cum.", "Cases", "Deaths Cum.", "Deaths", "Cases Norm.", "Cases Cum. Norm.", "Deaths Norm.", "Deaths Cum. Norm."))
        datz <<- data
        data <- as.data.frame(cbind(data[,c(1:2)], round(zoo::rollmean(data[,-c(1,2)], input$stream_ma,fill = "extend"),2)))
        vars <- c("STATE", "DATE", "Ageusia", "Anosmia", "Chills", "Cough", "Eye Pain", "Fever", "Headache", "Nasal Cong.", "Rhinorrea", "Short Breath", "Sore Throat")
        colnames(data) <- vars
        #datz <<- data
        p <- data %>%
            gather(Topic, IOT, all_of(vars[-c(1:2)])) %>%
            streamgraph("Topic", "IOT", "DATE", offset="silhouette",interpolate = "linear",width = 1800, height = 1200) %>%
            sg_legend(show=TRUE, label="Topics: ") %>%
            sg_axis_y(0)  %>% 
            sg_axis_x(tick_interval = "month", tick_units = 1,tick_format ="%b-%y") %>% 
            sg_fill_brewer("RdBu")#BrBG, PiYG, PRGn, PuOr, RdBu, RdGy, RdYlBu, RdYlGn, Spectral,
        #Accent, Dark2, Paired, Pastel1, Pastel2, Set1, Set2, Set3
        #Blues, BuGn, BuPu, GnBu, Greens, Greys, Oranges, OrRd, PuBu, PuBuGn, PuRd, Purples, RdPu, Reds, YlGn, YlGnBu YlOrBr, YlOrRd
        
        box(title = paste("Streamgraph of", input$stream_country_box), 
            width = 12, status = "danger", solidHeader = TRUE, align = "center", 
            style = "overflow-x: scroll",
            shiny::flowLayout(
                cellArgs = list(
                    style = "min-width: 1150px;
                     width: auto;
                     height: auto;
                     border: 1px solid white;
                     padding: 10px;
                     margin: 10px;"),
                renderStreamgraph(p)
            )
        )
        }
    })
    
    output$view_stream_2 <- renderUI({
        no_countries = c("Brunei", "Central African Republic", "Fiji","Holy See", "Marshall Islands", "Mauritius",
                         "Singapore", "Tajikistan", "Tanzania", "Vanuatu", "Vietnam")
        
        if (is.null(input$countries_box)) {
            prettySwitch(inputId = "view_stream_2", label = "View another streamgraph",  status = "success", fill = TRUE)
        } else if (input$countries_box %in% no_countries) {
            return(NULL)
        } else {
            prettySwitch(inputId = "view_stream_2", label = "View another streamgraph",  status = "success", fill = TRUE)
        }
    })
    
    
    output$button_stream_2 <- renderUI({
        req(input$view_stream_2 == TRUE)
        
        countries = countries3
        codes = codes3
        flags = flags3
        
        dropdown(
            tags$h3("Select another country:"),
            pickerInput("stream_country_box2", "Pick a country:",
                        choices = countries,
                        selected = "United States of America",
                        options = pickerOptions(liveSearch=T, `actions-box` = FALSE, `none-selected-text` = "Pick your country of interest:"),
                        choicesOpt = list(content = mapply(countries, flags, FUN = function(country, flagUrl) {
                            HTML(paste(
                                tags$img(src=flagUrl, width=20, height=15),
                                country
                            ))
                        }, SIMPLIFY = FALSE, USE.NAMES = FALSE)
                        )),
            style = "unite", icon = icon("gear"),
            status = "danger", width = "300px",
            tooltip = tooltipOptions(title = "Click to change steamgraphs comparisons!"),
            animate = animateOptions(
                enter = animations$fading_entrances$fadeInLeftBig,
                exit = animations$fading_exits$fadeOutRightBig
            )
        )
    })
    
    output$plot_stream_2 <- renderUI({
        req(input$view_stream_2 == TRUE)
        
        no_countries = c("Brunei", "Central African Republic", "Fiji","Holy See", "Marshall Islands", "Mauritius",
                         "Singapore", "Tajikistan", "Tanzania", "Vanuatu", "Vietnam")
        
        if (is.null(input$countries_box)) {
            data <- data3 %>%
                filter(grepl(input$stream_country_box2, STATE)) %>%#
                #filter(DATE >= input$date_input[1] & DATE <=input$date_input[2]) %>%
                select(-c("DAY","Cases Cum.", "Cases", "Deaths Cum.", "Deaths", "Cases Norm.", "Cases Cum. Norm.", "Deaths Norm.", "Deaths Cum. Norm."))
            
            data <- as.data.frame(cbind(data[,c(1:2)], round(zoo::rollmean(data[,-c(1,2)], input$stream_ma,fill = "extend"),2)))
            vars <- c("STATE", "DATE", "Ageusia", "Anosmia", "Chills", "Cough", "Eye Pain", "Fever", "Headache", "Nasal Cong.", "Rhinorrea", "Short Breath", "Sore Throat")
            colnames(data) <- vars
            
            p <- data %>%
                gather(Topic, IOT, all_of(vars[-c(1:2)])) %>%
                streamgraph("Topic", "IOT", "DATE", offset="silhouette",interpolate = "linear",width = 1800, height = 1200) %>%
                sg_legend(show=TRUE, label="Topics: ")  %>%
                sg_axis_y(0)  %>% 
                sg_axis_x(tick_interval = "month", tick_units = 1,tick_format ="%b-%y") %>% 
                sg_fill_brewer("RdBu")#BrBG, PiYG, PRGn, PuOr, RdBu, RdGy, RdYlBu, RdYlGn, Spectral,
            #Accent, Dark2, Paired, Pastel1, Pastel2, Set1, Set2, Set3
            #Blues, BuGn, BuPu, GnBu, Greens, Greys, Oranges, OrRd, PuBu, PuBuGn, PuRd, Purples, RdPu, Reds, YlGn, YlGnBu YlOrBr, YlOrRd
            
            box(title = paste("Streamgraph of", input$stream_country_box2), 
                width = 12, status = "danger", solidHeader = TRUE, align = "center", 
                style = "overflow-x: scroll",
                shiny::flowLayout(
                    cellArgs = list(
                        style = "min-width: 1150px;
                     width: auto;
                     height: auto;
                     border: 1px solid white;
                     padding: 10px;
                     margin: 10px;"),
                    renderStreamgraph(p))
            ) 
        } else if (input$countries_box %in% no_countries) {
            return(NULL)
        } else {
            data <- data3 %>%
                filter(grepl(input$stream_country_box2, STATE)) %>%#
                filter(DATE >= input$date_input[1] & DATE <=input$date_input[2]) %>%
                select(-c("DAY","Cases Cum.", "Cases", "Deaths Cum.", "Deaths", "Cases Norm.", "Cases Cum. Norm.", "Deaths Norm.", "Deaths Cum. Norm."))
            
            data <- as.data.frame(cbind(data[,c(1:2)], round(zoo::rollmean(data[,-c(1,2)], input$stream_ma,fill = "extend"),2)))
            vars <- c("STATE", "DATE", "Ageusia", "Anosmia", "Chills", "Cough", "Eye Pain", "Fever", "Headache", "Nasal Cong.", "Rhinorrea", "Short Breath", "Sore Throat")
            colnames(data) <- vars
            
            p <- data %>%
                gather(Topic, IOT, all_of(vars[-c(1:2)])) %>%
                streamgraph("Topic", "IOT", "DATE", offset="silhouette",interpolate = "linear",width = 1800, height = 1200) %>%
                sg_legend(show=TRUE, label="Topics: ")  %>%
                sg_axis_y(0)  %>% 
                sg_axis_x(tick_interval = "month", tick_units = 1,tick_format ="%b-%y") %>% 
                sg_fill_brewer("RdBu")#BrBG, PiYG, PRGn, PuOr, RdBu, RdGy, RdYlBu, RdYlGn, Spectral,
            #Accent, Dark2, Paired, Pastel1, Pastel2, Set1, Set2, Set3
            #Blues, BuGn, BuPu, GnBu, Greens, Greys, Oranges, OrRd, PuBu, PuBuGn, PuRd, Purples, RdPu, Reds, YlGn, YlGnBu YlOrBr, YlOrRd
            
            box(title = paste("Streamgraph of", input$stream_country_box2), 
                width = 12, status = "danger", solidHeader = TRUE, align = "center", 
                style = "overflow-x: scroll",
                shiny::flowLayout(
                    cellArgs = list(
                        style = "min-width: 1150px;
                     width: auto;
                     height: auto;
                     border: 1px solid white;
                     padding: 10px;
                     margin: 10px;"),
                    renderStreamgraph(p))
            ) 
        }
        
        
    })
    
    # world map ####
    
    output$select_world <- renderUI({
        # if (is.null(input$countries_box)) {
        #     tags$div(
        #         tags$h2(style="color:red", "Please, go to the 'Select a country' tab first")
        #     )
        # } else {
        my_vars = c("Ageusia", "Anosmia", "Cases", "Cases Cum.", "Cases Norm.", "Cases Cum. Norm.", "Chills", "Cough", 
                    "Deaths Cum.", "Deaths",  "Deaths Norm.", "Deaths Cum. Norm.",
                    "Eye Pain", "Fever", "Headache", "Nasal Cong.", "Rhinorrea", "Short Breath", "Sore Throat")
        
        box(title = "Select time series to plot:", 
            width = 12, status = "info", solidHeader = TRUE, align = "center", #???style = "overflow-x: scroll",
            shiny::flowLayout(
                cellArgs = list(
                    style = "min-width: 400px;
                     min-height: 100px;
                     width: auto;
                     height: auto;
                     border: 1px solid white;
                     padding: 10px;
                     margin: 10px;"),
                radioGroupButtons(
                    inputId = "select_world",
                    label = "Pick one variable:",
                    choices = my_vars,
                    individual = TRUE,
                    checkIcon = list(
                        yes = tags$i(class = "fa fa-circle", 
                                     style = "color: steelblue"),
                        no = tags$i(class = "fa fa-circle-o", 
                                    style = "color: steelblue"))
                )
                # pickerInput(
                #     inputId = "select_world",
                #     label = "Pick one variable:",
                #     choices = my_vars,
                #     selected = my_vars[1],
                #     multiple = FALSE,
                #     options = list(
                #         `live-search` = FALSE)
                # )
                )
        )
        #}
    })
    
    data4 <- reactive({
        data3 <- data3
        data3$DATE<-as.Date(data3$DATE)
        data3$hover <- with(data3, paste(STATE, '<br>', input$select_world, data3$DATE))#input$select_world
        data3$CODE_3 <- CODES_3
        
        if (is.null(input$stream_ma)){
            data4 <- as.data.frame(cbind(data3[,c(1,2,3,23,24)], round(zoo::rollmean(data3[,c(4:22)], 7,fill = "extend"),2)))#input$stream_ma
        } else {
            data4 <- as.data.frame(cbind(data3[,c(1,2,3,23,24)], round(zoo::rollmean(data3[,c(4:22)], input$ma_selection,fill = "extend"),2)))#input$stream_ma
        }
        
        data4
    })
    
    output$world_image <- renderUI({
        
        # data3 <- data3
        # data3$DATE<-as.Date(data3$DATE)
        # data3$hover <- with(data3, paste(STATE, '<br>', input$select_world, data3$DATE))#input$select_world
        # data3$CODE_3 <- CODES_3
        # 
        # data4 <- as.data.frame(cbind(data3[,c(1,2,3,23,24)], round(zoo::rollmean(data3[,c(4:22)], input$stream_ma,fill = "extend"),2)))#input$stream_ma
        
        data4 <- data4()
        
        m <- list(
            l = 2,
            r = 2,
            b = 1,
            t = 20,
            pad = 1
        )
        
        l <- list(color = toRGB("grey"), width = 0.5)
        
        g <- list(
            showframe = FALSE,
            showcoastlines = TRUE,
            projection = list(type = 'Mercator')
        )
        topics <- c("Ageusia", "Anosmia", "Chills", "Cough", "Eye Pain", "Fever", "Headache", "Nasal Cong.", "Rhinorrea", "Short Breath", "Sore Throat")
        
        if (input$select_world %in% topics) {
            fig <- plot_ly(data4, type='choropleth', locations=data4$CODE_3, #data3
                           z=data4[[input$select_world]], 
                           frame = as.character(data4$DATE), 
                           colorscale="Plasma",
                           text=data4$hover, 
                           hoverinfo = paste('<b>%{text}</b>'),
                           width = 1200, height = 800) %>%
                colorbar(title = input$select_world, x = 1, y = 0.7, limits = c(0, 100)) %>%
                layout(autosize = TRUE, margin = m,dragmode = "pan") %>% 
                animation_button(label = "View in sequence", x = 0.6, y = 0.15) %>%
                animation_slider(currentvalue = list(prefix = "Day: ", font = list(color="red")),
                                 x = 0, y = 0.1) %>%
                animation_opts(10, easing = "elastic", redraw = FALSE)
            
        } else {
            fig <- plot_ly(data4, type='choropleth', locations=data4$CODE_3, 
                           z=data4[[input$select_world]], 
                           frame = as.character(data4$DATE), 
                           colorscale="Plasma",
                           text=data4$hover, 
                           hoverinfo = paste('<b>%{text}</b>'),
                           width = 1200,height = 800) %>%
                colorbar(title = input$select_world, x = 1, y = 0.7) %>%
                layout(autosize = TRUE, margin = m,dragmode = "pan") %>% 
                animation_button(label = "View in sequence", x = 0.6, y = 0.15) %>% 
                animation_slider(currentvalue = list(prefix = "Day: ", font = list(color="red")),
                                 x = 0, y = 0.1)%>%
                animation_opts(10, easing = "elastic", redraw = FALSE)
        }
        
        box(title = paste('World map of', input$select_world),
            width = 12, status = "danger", solidHeader = TRUE,
            style = "overflow-x: scroll", align = "center",
            shiny::flowLayout(
                cellArgs = list(
                    style = "min-width: 800px;
                     width: auto;
                     height: 800px;
                     border: 1px solid white;
                     padding: 10px;
                     margin: 10px;"),
                renderPlotly(fig))
        )
        #}
    })
    
    #### correlation ####
    
    output$flag <- renderUI({
        if (is.null(input$countries_box)) {
            countries3 <- unique(data3$STATE)
            n <- match(input$countries_box_home,countries3)
            HTML(paste(
                tags$img(src=flags3[n], width=100, height=70)#,
                #"Country"
            ))
        } else {
            countries3 <- unique(data3$STATE)
            n <- match(input$countries_box,countries3)
            HTML(paste(
                tags$img(src=flags3[n], width=100, height=70)#,
                #"Country"
            ))   
        }
    })
    
    output$remove_topics <- renderUI({
        if (is.null(input$countries_box)) {
            mydata <- as.data.frame(data3)
            reactive_data <- as.data.frame(mydata %>% filter(STATE == input$countries_box_home)) #input$countries_box
            reactive_data <- reactive_data[, colSums(reactive_data != 0) > 0]
            reactive_data[sapply(reactive_data, function(x) length(unique(na.omit(x)))) > 1]
            test <- reactive_data    #() %>% filter(DATE >= input$date_input[1] & DATE <=input$date_input[2])
            reactiveDf <- test
            data <- reactiveDf[,-1] #()
            data <- data[,-1]########edit#############
            data <- data[,sapply(data, function(v) var(v, na.rm=TRUE)!=0)]
            my_vars <- colnames(data[,-c(1:2)])
            data <- as.data.frame(cbind(data[,c(1:2)],zoo::rollmean(data[,-c(1,2)], 1, fill = "extend")))# input$ma_selection
            data2 <- data[,-c(1:2)]
            colnames(data2) <- my_vars #
            data <- as.data.frame(cbind(data[,c(1:2)],data2)) #
            data <- as.data.frame(data)
            data <- data[complete.cases(data[,]),] #
            my_ts_def <- data
            C <- colnames(my_ts_def)
            nums <- nrow(my_ts_def)
            prova <- my_ts_def[c((nums-30):nums),]
            prova <- prova[, colSums(prova != 0) > 0]
            D <- colnames(prova)
            aa<-setdiff(C,D)
            my_ts_def<-my_ts_def[, !(colnames(my_ts_def) %in% aa), drop = FALSE]
            data<-my_ts_def[,-c(1,2)]
            choice = colnames(data)
            
            box(title = "Remove Topics for PCA model? If not, go further", 
                width = 12, status = "warning", solidHeader = TRUE, align = "center",
                style = "overflow-x: scroll",
                shiny::flowLayout(
                    cellArgs = list(
                        style = "min-width: 400px;
                     min-height: 150px;
                     width: auto;
                     height: auto;
                     border: 1px solid white;
                     padding: 10px;
                     margin: 10px;"),
                    selectInput("remove_topics","Pick topics (if necessary) to be removed before building the PCA model:",
                                choices = c(Choose ='', choice), multiple = TRUE
                    ))
            )
        } else {
            
        data<-reactiveDf()[,-1]
        choice = colnames(data)
        
        box(title = "Remove Topics for PCA model? If not, go further", 
            width = 12, status = "warning", solidHeader = TRUE, align = "center",
            style = "overflow-x: scroll",
            shiny::flowLayout(
                cellArgs = list(
                    style = "min-width: 400px;
                     min-height: 150px;
                     width: auto;
                     height: auto;
                     border: 1px solid white;
                     padding: 10px;
                     margin: 10px;"),
                selectInput("remove_topics","Pick topics (if necessary) to be removed before building the PCA model:",
                            choices = c(Choose ='', choice), multiple = TRUE
                ))
        )
        }
    })
    
    my_ts_def <- reactive({
        data <- reactiveDf()[,-1]
        data <- data[,-1]########edit#############
        data <- data[,!(colnames(data) %in% as.character(input$remove_topics))]
        data <- data[,sapply(data, function(v) var(v, na.rm=TRUE)!=0)]
        my_vars <- colnames(data[,-c(1:2)])
        data <- as.data.frame(cbind(data[,c(1:2)],zoo::rollmean(data[,-c(1,2)], input$ma_selection, fill = "extend")))# if moving average
        data2 <- data[,-c(1:2)]
        colnames(data2) <- my_vars #
        data <- as.data.frame(cbind(data[,c(1:2)],data2)) #
        data <- as.data.frame(data)
        data <- data[complete.cases(data[,]),] #
        my_ts_def <- data
        my_ts_def
    })
    
    # output$tbl2 <- renderUI({
    #     #req(!is.null(input$countries_box))
    #     if (is.null(input$countries_box)) {
    #         mydata <- as.data.frame(data3)
    #         reactive_data <- as.data.frame(mydata %>% filter(STATE == input$countries_box_home)) #input$countries_box
    #         reactive_data <- reactive_data[, colSums(reactive_data != 0) > 0]
    #         reactive_data[sapply(reactive_data, function(x) length(unique(na.omit(x)))) > 1]
    #         test <- reactive_data    #() %>% filter(DATE >= input$date_input[1] & DATE <=input$date_input[2])
    #         reactiveDf <- test
    #         data <- reactiveDf[,-1] #()
    #         data <- data[,sapply(data, function(v) var(v, na.rm=TRUE)!=0)]
    #         my_vars <- colnames(data[,-c(1:2)])
    #         data <- as.data.frame(cbind(data[,c(1:2)],zoo::rollmean(data[,-c(1,2)], 1, fill = "extend")))# input$ma_selection
    #         data2 <- data[,-c(1:2)]
    #         colnames(data2) <- my_vars #
    #         data <- as.data.frame(cbind(data[,c(1:2)],data2)) #
    #         data <- as.data.frame(data)
    #         data <- data[complete.cases(data[,]),] #
    #         my_ts_def <- data
    #         C <- colnames(my_ts_def)
    #         nums <- nrow(my_ts_def)
    #         prova <- my_ts_def[c((nums-30):nums),]
    #         prova <- prova[, colSums(prova != 0) > 0]
    #         D <- colnames(prova)
    #         aa<-setdiff(C,D)
    #         my_ts_def<-my_ts_def[, !(colnames(my_ts_def) %in% aa), drop = FALSE]
    #         box(title = "Dataset", 
    #             width = 12, status = "success", collapsible = TRUE, collapsed = TRUE, solidHeader = TRUE,
    #             style = "overflow-x: scroll",
    #             shiny::flowLayout(
    #                 cellArgs = list(
    #                     style = "min-width: 800px;
    #                  width: auto;
    #                  height: auto;
    #                  border: 1px solid white;
    #                  padding: 10px;
    #                  margin: 10px;"),
    #                 renderDT(my_ts_def))
    #         )
    #     } else {
    #     box(title = "Dataset", 
    #         width = 12, status = "success", collapsible = TRUE, collapsed = TRUE, solidHeader = TRUE,
    #         style = "overflow-x: scroll",
    #         shiny::flowLayout(
    #             cellArgs = list(
    #                 style = "min-width: 800px;
    #                  width: auto;
    #                  height: auto;
    #                  border: 1px solid white;
    #                  padding: 10px;
    #                  margin: 10px;"),
    #             renderDT(my_ts_def()))
    #     )
    #     }
    # })
    # 
    # output$corr_plot <- renderUI({
    #     if (is.null(input$countries_box)) {
    #         mydata <- as.data.frame(data3)
    #         reactive_data <- as.data.frame(mydata %>% filter(STATE == input$countries_box_home)) #input$countries_box
    #         reactive_data <- reactive_data[, colSums(reactive_data != 0) > 0]
    #         reactive_data[sapply(reactive_data, function(x) length(unique(na.omit(x)))) > 1]
    #         test <- reactive_data    #() %>% filter(DATE >= input$date_input[1] & DATE <=input$date_input[2])
    #         reactiveDf <- test
    #         data <- reactiveDf[,-1] #()
    #         data <- data[,sapply(data, function(v) var(v, na.rm=TRUE)!=0)]
    #         my_vars <- colnames(data[,-c(1:2)])
    #         data <- as.data.frame(cbind(data[,c(1:2)],zoo::rollmean(data[,-c(1,2)], 1, fill = "extend")))# input$ma_selection
    #         data2 <- data[,-c(1:2)]
    #         colnames(data2) <- my_vars #
    #         data <- as.data.frame(cbind(data[,c(1:2)],data2)) #
    #         data <- as.data.frame(data)
    #         data <- data[complete.cases(data[,]),] #
    #         my_ts_def <- data
    #         C <- colnames(my_ts_def)
    #         nums <- nrow(my_ts_def)
    #         prova <- my_ts_def[c((nums-30):nums),]
    #         prova <- prova[, colSums(prova != 0) > 0]
    #         D <- colnames(prova)
    #         aa<-setdiff(C,D)
    #         my_ts_def<-my_ts_def[, !(colnames(my_ts_def) %in% aa), drop = FALSE]
    #         corr <- round(stats::cor(my_ts_def[,-c(1,2)]),1)
    #         p.mat <- ggcorrplot::cor_pmat(my_ts_def[,-c(1,2)])
    #         corr.plot <- ggcorrplot(
    #             corr, hc.order = FALSE, type = "lower", outline.col = "white", lab = FALSE,
    #             p.mat = p.mat 
    #         )
    #         p <- ggplotly(corr.plot)
    #         p <- p%>%
    #             layout(yaxis = list(showgrid = F),dragmode = "pan")
    #         
    #         box(title = "Correlation Plot", 
    #             width = 12, status = "danger",  collapsible = TRUE, collapsed = TRUE, solidHeader = TRUE,
    #             style = "overflow-x: scroll", align = "left",
    #             shiny::flowLayout(
    #                 cellArgs = list(
    #                     style = "min-width: 800px;
    #                  width: auto;
    #                  height: auto;
    #                  border: 1px solid white;
    #                  padding: 10px;
    #                  margin: 10px;"),
    #                 renderPlotly(p))
    #         )
    #     } else {
    #     corr <- round(stats::cor(my_ts_def()[,-c(1,2)]),1)
    #     p.mat <- ggcorrplot::cor_pmat(my_ts_def()[,-c(1,2)])
    #     corr.plot <- ggcorrplot(
    #         corr, hc.order = FALSE, type = "lower", outline.col = "white", lab = FALSE,
    #         p.mat = p.mat 
    #     )
    #     p <- ggplotly(corr.plot)
    #     p <- p%>%
    #         layout(yaxis = list(showgrid = F),dragmode = "pan")
    #     
    #     box(title = "Correlation Plot", 
    #         width = 12, status = "danger",  collapsible = TRUE, collapsed = TRUE, solidHeader = TRUE,
    #         style = "overflow-x: scroll", align = "left",
    #         shiny::flowLayout(
    #             cellArgs = list(
    #                 style = "min-width: 800px;
    #                  width: auto;
    #                  height: auto;
    #                  border: 1px solid white;
    #                  padding: 10px;
    #                  margin: 10px;"),
    #             renderPlotly(p))
    #     )
    #     }
    # })
    # 
    # output$corr_table <- renderUI({
    #     if (is.null(input$countries_box)) {
    #         mydata <- as.data.frame(data3)
    #         reactive_data <- as.data.frame(mydata %>% filter(STATE == input$countries_box_home)) #input$countries_box
    #         reactive_data <- reactive_data[, colSums(reactive_data != 0) > 0]
    #         reactive_data[sapply(reactive_data, function(x) length(unique(na.omit(x)))) > 1]
    #         test <- reactive_data    #() %>% filter(DATE >= input$date_input[1] & DATE <=input$date_input[2])
    #         reactiveDf <- test
    #         data <- reactiveDf[,-1] #()
    #         data <- data[,sapply(data, function(v) var(v, na.rm=TRUE)!=0)]
    #         my_vars <- colnames(data[,-c(1:2)])
    #         data <- as.data.frame(cbind(data[,c(1:2)],zoo::rollmean(data[,-c(1,2)], 1, fill = "extend")))# input$ma_selection
    #         data2 <- data[,-c(1:2)]
    #         colnames(data2) <- my_vars #
    #         data <- as.data.frame(cbind(data[,c(1:2)],data2)) #
    #         data <- as.data.frame(data)
    #         data <- data[complete.cases(data[,]),] #
    #         my_ts_def <- data
    #         C <- colnames(my_ts_def)
    #         nums <- nrow(my_ts_def)
    #         prova <- my_ts_def[c((nums-30):nums),]
    #         prova <- prova[, colSums(prova != 0) > 0]
    #         D <- colnames(prova)
    #         aa<-setdiff(C,D)
    #         my_ts_def<-my_ts_def[, !(colnames(my_ts_def) %in% aa), drop = FALSE]
    #         corr <- round(stats::cor(my_ts_def[,-c(1,2)]),2)
    #         upper<-corr
    #         upper[upper.tri(corr)]<-""
    #         upper<-as.data.frame(upper)
    #         box(title = "Correlation Table", 
    #             width = 12, status = "primary",  collapsible = TRUE, collapsed = TRUE, solidHeader = TRUE,
    #             style = "overflow-x: scroll",
    #             shiny::flowLayout(
    #                 cellArgs = list(
    #                     style = "min-width: 800px;
    #                  width: auto;
    #                  height: auto;
    #                  border: 1px solid white;
    #                  padding: 10px;
    #                  margin: 10px;"),
    #                 renderDT(upper,  rownames = TRUE#, 
    #                          #align = 'c'
    #                 ))
    #         )
    #     } else {
    #     corr <- round(stats::cor(my_ts_def()[,-c(1,2)]),2)
    #     upper<-corr
    #     upper[upper.tri(corr)]<-""
    #     upper<-as.data.frame(upper)
    #     box(title = "Correlation Table", 
    #         width = 12, status = "primary",  collapsible = TRUE, collapsed = TRUE, solidHeader = TRUE,
    #         style = "overflow-x: scroll",
    #         shiny::flowLayout(
    #             cellArgs = list(
    #                 style = "min-width: 800px;
    #                  width: auto;
    #                  height: auto;
    #                  border: 1px solid white;
    #                  padding: 10px;
    #                  margin: 10px;"),
    #             renderDT(upper,  rownames = TRUE#, 
    #                      #align = 'c'
    #             ))
    #     )
    #     }
    # })
    
    output$dropdown_ccf <- renderUI({
        if (is.null(input$countries_box)) {
            mydata <- as.data.frame(data3)
            reactive_data <- as.data.frame(mydata %>% filter(STATE == input$countries_box_home)) #input$countries_box
            reactive_data <- reactive_data[, colSums(reactive_data != 0) > 0]
            reactive_data[sapply(reactive_data, function(x) length(unique(na.omit(x)))) > 1]
            test <- reactive_data    #() %>% filter(DATE >= input$date_input[1] & DATE <=input$date_input[2])
            reactiveDf <- test
            data <- reactiveDf[,-1] #()
            data <- data[,-1]########edit#############
            data <- data[,sapply(data, function(v) var(v, na.rm=TRUE)!=0)]
            my_vars <- colnames(data[,-c(1:2)])
            data <- as.data.frame(cbind(data[,c(1:2)],zoo::rollmean(data[,-c(1,2)], 1, fill = "extend")))# input$ma_selection
            data2 <- data[,-c(1:2)]
            colnames(data2) <- my_vars #
            data <- as.data.frame(cbind(data[,c(1:2)],data2)) #
            data <- as.data.frame(data)
            data <- data[complete.cases(data[,]),] #
            my_ts_def <- data
            C <- colnames(my_ts_def)
            nums <- nrow(my_ts_def)
            prova <- my_ts_def[c((nums-30):nums),]
            prova <- prova[, colSums(prova != 0) > 0]
            D <- colnames(prova)
            aa<-setdiff(C,D)
            my_ts_def<-my_ts_def[, !(colnames(my_ts_def) %in% aa), drop = FALSE]
            dat <- my_ts_def
            dat <- dat[,-c(1,2)]
            choice2 <- c("Cases","Cases Norm.","Cases Cum. Norm.","Deaths","Deaths Norm.","Deaths Cum. Norm.")#"Cases Cum.","Deaths Cum.",
            choice1 <- colnames(dat)
            choice1 <- choice1 [! choice1 %in% choice2]
            dropdown(
                tags$h3("List of topics for ccf"),
                pickerInput(inputId = 'input_ccf_1',
                            label = 'Select first topic:',
                            choices = choice1,
                            selected = choice1[1],
                            options = list(`style` = "btn-primary")),
                pickerInput(inputId = 'input_ccf_2',
                            label = 'Select second topic:',
                            choices = choice2,
                            selected = "Deaths",
                            options = list(`style` = "btn-info")),
                style = "unite", icon = icon("gear"),
                status = "danger", width = "300px",
                tooltip = tooltipOptions(title = "Click to change CCF comparisons!"),
                animate = animateOptions(
                    enter = animations$fading_entrances$fadeInLeftBig,
                    exit = animations$fading_exits$fadeOutRightBig
                )
            )
        } else {
        #choice1 <- c("Ageusia","Anosmia","Chills","Cough","Eye Pain","Fever","Headache","Nasal Cong.","Rhinorrea","Short Breath","Sore Throat")
        dat <- my_ts_def()
        dat <- dat[,-c(1,2)]
        choice2 <- c("Cases","Cases Norm.","Cases Cum. Norm.","Deaths","Deaths Norm.","Deaths Cum. Norm.")#"Cases Cum.","Deaths Cum.",
        choice1 <- colnames(dat)
        choice1 <- choice1 [! choice1 %in% choice2]
        dropdown(
            tags$h3("List of topics for ccf"),
            pickerInput(inputId = 'input_ccf_1',
                        label = 'Select first topic:',
                        choices = choice1,
                        selected = choice1[1],
                        options = list(`style` = "btn-primary")),
            pickerInput(inputId = 'input_ccf_2',
                        label = 'Select second topic:',
                        choices = choice2,
                        selected = "Deaths",
                        options = list(`style` = "btn-info")),
            style = "unite", icon = icon("gear"),
            status = "danger", width = "300px",
            tooltip = tooltipOptions(title = "Click to change CCF comparisons!"),
            animate = animateOptions(
                enter = animations$fading_entrances$fadeInLeftBig,
                exit = animations$fading_exits$fadeOutRightBig
            )
        )
        }
    })
    
    output$ccf_plot <- renderUI({
        if (is.null(input$countries_box)) {
            mydata <- as.data.frame(data3)
            reactive_data <- as.data.frame(mydata %>% filter(STATE == input$countries_box_home)) #input$countries_box
            reactive_data <- reactive_data[, colSums(reactive_data != 0) > 0]
            reactive_data[sapply(reactive_data, function(x) length(unique(na.omit(x)))) > 1]
            test <- reactive_data    #() %>% filter(DATE >= input$date_input[1] & DATE <=input$date_input[2])
            reactiveDf <- test
            data <- reactiveDf[,-1] #()
            data <- data[,-1]########edit#############
            data <- data[,sapply(data, function(v) var(v, na.rm=TRUE)!=0)]
            my_vars <- colnames(data[,-c(1:2)])
            data <- as.data.frame(cbind(data[,c(1:2)],zoo::rollmean(data[,-c(1,2)], 1, fill = "extend")))# input$ma_selection
            data2 <- data[,-c(1:2)]
            colnames(data2) <- my_vars #
            data <- as.data.frame(cbind(data[,c(1:2)],data2)) #
            data <- as.data.frame(data)
            data <- data[complete.cases(data[,]),] #
            my_ts_def <- data
            C <- colnames(my_ts_def)
            nums <- nrow(my_ts_def)
            prova <- my_ts_def[c((nums-30):nums),]
            prova <- prova[, colSums(prova != 0) > 0]
            D <- colnames(prova)
            aa<-setdiff(C,D)
            my_ts_def<-my_ts_def[, !(colnames(my_ts_def) %in% aa), drop = FALSE]
            data <- my_ts_def
            input1 <- ts(data[[input$input_ccf_1]])
            input2 <- ts(data[[input$input_ccf_2]])
            ccf_dat <- ccf(input1, input2) 
            
            a <- as.numeric(ccf_dat[["lag"]])
            b <- as.numeric(ccf_dat[["acf"]])
            n <- ccf_dat[["n.used"]]
            
            data_ccf <- as.data.frame(cbind(a,b))
            colnames(data_ccf) <- c("lag","CCF")
            
            p <- plot_ly(data_ccf) %>%
                add_trace(x = ~lag, y = ~CCF, type = 'bar', width = .2,
                          marker = list(color = 'black', opacity=1),
                          name = "CCF values",
                          hoverinfo = 'text',
                          hovertext = paste('<b>Lag</b>:', data_ccf$lag,
                                            '<br><b>CCF</b>:', round(data_ccf$CCF,2))) %>%
                add_segments(x = -22, xend = +22, y = (-1/n)+(2/sqrt(n)), yend = (-1/n)+(2/sqrt(n)),
                             name = "CCF threshold",
                             line = list(dash = "dash", color = 'blue')) %>%
                add_segments(x = -22, xend = +22, y = (-1/n)-(2/sqrt(n)), yend = (-1/n)-(2/sqrt(n)), 
                             name = "",
                             showlegend = FALSE,
                             line = list(dash = "dash", color = 'blue')) %>%
                layout(bargap = 0.4, title = paste(input$input_ccf_1, "vs", input$input_ccf_2),dragmode = "pan",
                       yaxis = list(title = "Cross-Correlation Function (CCF)"),
                       xaxis = list(title = "lag (days)"))
            
            box(title = paste("Cross-Correlation Function (CCF) plot for", input$countries_box), 
                width = 12, status = "danger", solidHeader = TRUE,
                style = "overflow-x: scroll",
                shiny::flowLayout(
                    cellArgs = list(
                        style = "min-width: 750px;
                     width: auto;
                     height: auto;
                     border: 1px solid white;
                     padding: 10px;
                     margin: 10px;"),
                    renderPlotly(p))
            )
        } else {
        data <- my_ts_def()
        input1 <- ts(data[[input$input_ccf_1]])
        input2 <- ts(data[[input$input_ccf_2]])
        ccf_dat <- ccf(input1, input2) 
        
        a <- as.numeric(ccf_dat[["lag"]])
        b <- as.numeric(ccf_dat[["acf"]])
        n <- ccf_dat[["n.used"]]
        
        data_ccf <- as.data.frame(cbind(a,b))
        colnames(data_ccf) <- c("lag","CCF")
        
        p <- plot_ly(data_ccf) %>%
            add_trace(x = ~lag, y = ~CCF, type = 'bar', width = .2,
                      marker = list(color = 'black', opacity=1),
                      name = "CCF values",
                      hoverinfo = 'text',
                      hovertext = paste('<b>Lag</b>:', data_ccf$lag,
                                        '<br><b>CCF</b>:', round(data_ccf$CCF,2))) %>%
            add_segments(x = -22, xend = +22, y = (-1/n)+(2/sqrt(n)), yend = (-1/n)+(2/sqrt(n)),
                         name = "CCF threshold",
                         line = list(dash = "dash", color = 'blue')) %>%
            add_segments(x = -22, xend = +22, y = (-1/n)-(2/sqrt(n)), yend = (-1/n)-(2/sqrt(n)), 
                         name = "",
                         showlegend = FALSE,
                         line = list(dash = "dash", color = 'blue')) %>%
            layout(bargap = 0.4, title = paste(input$input_ccf_1, "vs", input$input_ccf_2),dragmode = "pan",
                   yaxis = list(title = "Cross-Correlation Function (CCF)"),
                   xaxis = list(title = "lag (days)"))
        
        box(title = paste("Cross-Correlation Function (CCF) plot for", input$countries_box), 
            width = 12, status = "danger", solidHeader = TRUE,
            style = "overflow-x: scroll",
            shiny::flowLayout(
                cellArgs = list(
                    style = "min-width: 750px;
                     width: auto;
                     height: auto;
                     border: 1px solid white;
                     padding: 10px;
                     margin: 10px;"),
                renderPlotly(p))
        )
        }
    })
    
    output$ccf_table <- renderUI({
        if (is.null(input$countries_box)) {
            mydata <- as.data.frame(data3)
            reactive_data <- as.data.frame(mydata %>% filter(STATE == input$countries_box_home)) #input$countries_box
            reactive_data <- reactive_data[, colSums(reactive_data != 0) > 0]
            reactive_data[sapply(reactive_data, function(x) length(unique(na.omit(x)))) > 1]
            test <- reactive_data    #() %>% filter(DATE >= input$date_input[1] & DATE <=input$date_input[2])
            reactiveDf <- test
            data <- reactiveDf[,-1] #()
            data <- data[,-1]########edit#############
            data <- data[,sapply(data, function(v) var(v, na.rm=TRUE)!=0)]
            my_vars <- colnames(data[,-c(1:2)])
            data <- as.data.frame(cbind(data[,c(1:2)],zoo::rollmean(data[,-c(1,2)], 1, fill = "extend")))# input$ma_selection
            data2 <- data[,-c(1:2)]
            colnames(data2) <- my_vars #
            data <- as.data.frame(cbind(data[,c(1:2)],data2)) #
            data <- as.data.frame(data)
            data <- data[complete.cases(data[,]),] #
            my_ts_def <- data
            C <- colnames(my_ts_def)
            nums <- nrow(my_ts_def)
            prova <- my_ts_def[c((nums-30):nums),]
            prova <- prova[, colSums(prova != 0) > 0]
            D <- colnames(prova)
            aa<-setdiff(C,D)
            my_ts_def<-my_ts_def[, !(colnames(my_ts_def) %in% aa), drop = FALSE]
            
            data <- my_ts_def
            input1 <- ts(data[[input$input_ccf_1]])
            
            vars <- c( "Cases","Cases Cum.","Cases Norm.","Cases Cum. Norm.","Deaths","Deaths Cum.","Deaths Norm.","Deaths Cum. Norm.")
            my_ccf <- list()
            for (i in vars){
                ccf_dat <- ccf(input1, ts(data[,i])) 
                b <- as.numeric(ccf_dat[["acf"]])
                my_ccf[[i]] <- b 
            }
            data2 <- data.frame(matrix(unlist(my_ccf), ncol=length(my_ccf), byrow=FALSE))
            colnames(data2) <- names(my_ccf)
            data2 <- cbind(seq(-23,23,1),data2)
            colnames(data2)[1] <- "lag"
            data3 <- as.matrix(data2[,-1])
            row.names(data3) <- data2$lag
            
            absmax <- function(x) { x[which.max( abs(x) )]}
            vec1<-absmax(data3[,1])
            vec2<-absmax(data3[,2])
            vec3<-absmax(data3[,3])
            vec4<-absmax(data3[,4])
            vec5<-absmax(data3[,5])
            vec6<-absmax(data3[,6])
            vec7<-absmax(data3[,7])
            vec8<-absmax(data3[,8])
            res<-rbind(vec1,vec2,vec3,vec4,vec5,vec6,vec7,vec8)
            res <- round(res,3)
            row.names(res)<-colnames(data3)
            colnames(res) <- "Best CCF"
            lag <- c(names(vec1),names(vec2),names(vec3),names(vec4),names(vec5),names(vec6),names(vec7),names(vec8))
            res <- cbind(res,lag)
            colnames(res) <- c("Best CCF", "lag (days)")
            rezz <<- res
            res2 <- res[-c(2,6),]
            
            box(title = paste0("Best CCF for ", input$input_ccf_1, " (", input$countries_box_home, ")"), 
                width = 12, status = "success", solidHeader = TRUE, align="center" , collapsible = TRUE, collapsed = FALSE,
                style = "overflow-x: scroll",
                shiny::flowLayout(
                    cellArgs = list(
                        style = "min-width: 600px;
                     width: auto;
                     height: auto;
                     border: 1px solid white;
                     padding: 10px;
                     margin: 10px;"),
                    renderDT(datatable(res2,
                                       options = list(columnDefs = 
                                                          list(list(className = 'dt-center', 
                                                                    targets = "_all"))))))
            )
            
        } else {
            data <- my_ts_def()
            input1 <- ts(data[[input$input_ccf_1]])
            
            vars <- c("Cases","Cases Cum.","Cases Norm.","Cases Cum. Norm.","Deaths", "Deaths Cum.","Deaths Norm.","Deaths Cum. Norm.")
            my_ccf <- list()
            for (i in vars){
                ccf_dat <- ccf(input1, ts(data[,i])) 
                b <- as.numeric(ccf_dat[["acf"]])
                my_ccf[[i]] <- b 
            }
            data2 <- data.frame(matrix(unlist(my_ccf), ncol=length(my_ccf), byrow=FALSE))
            colnames(data2) <- names(my_ccf)
            data2 <- cbind(seq(-23,23,1),data2)
            colnames(data2)[1] <- "lag"
            data3 <- as.matrix(data2[,-1])
            row.names(data3) <- data2$lag
            
            absmax <- function(x) { x[which.max( abs(x) )]}
            vec1<-absmax(data3[,1])
            vec2<-absmax(data3[,2])
            vec3<-absmax(data3[,3])
            vec4<-absmax(data3[,4])
            vec5<-absmax(data3[,5])
            vec6<-absmax(data3[,6])
            vec7<-absmax(data3[,7])
            vec8<-absmax(data3[,8])
            res<-rbind(vec1,vec2,vec3,vec4,vec5,vec6,vec7,vec8)
            res <- round(res,3)
            row.names(res)<-colnames(data3)
            colnames(res) <- "Max CCF"
            lag <- c(names(vec1),names(vec2),names(vec3),names(vec4),names(vec5),names(vec6),names(vec7),names(vec8))
            res <- cbind(res,lag)
            colnames(res) <- c("Best CCF", "lag (days)")
            res2 <- res[-c(2,6),]
            
            # data2 <- data.frame(matrix(0, 45, ncol(data)))
            # for (i in 14:ncol(data)){
            #     ccf_dat <- ccf(input1, ts(data[,i])) 
            #     b <- as.numeric(ccf_dat[["acf"]])
            #     data2[,i] <- b 
            #     colnames(data2)[i] <- colnames(data)[i]
            # }
            # data2 <- data2[,-c(1:13)]
            # data2 <- cbind(seq(-22,22,1),data2)
            # colnames(data2)[1] <- "lag"
            # data3 <- as.matrix(data2[,-1])
            # row.names(data3) <- data2$lag
            # 
            # absmax <- function(x) { x[which.max( abs(x) )]}
            # vec1<-absmax(data3[,1])
            # vec2<-absmax(data3[,2])
            # vec3<-absmax(data3[,3])
            # vec4<-absmax(data3[,4])
            # vec5<-absmax(data3[,5])
            # vec6<-absmax(data3[,6])
            # vec7<-absmax(data3[,7])
            # vec8<-absmax(data3[,8])
            # res<-rbind(vec1,vec2,vec3,vec4,vec5,vec6,vec7,vec8)
            # res <- round(res,3)
            # row.names(res)<-colnames(data3)
            # colnames(res) <- "Max CCF"
            # lag <- c(names(vec1),names(vec2),names(vec3),names(vec4),names(vec5),names(vec6),names(vec7),names(vec8))
            # res <- cbind(res,lag)
            box(title = paste0("Max (absolute) CCF for ", input$input_ccf_1, " (", input$countries_box, ")"), 
                width = 12, status = "success", solidHeader = TRUE, align="center" , collapsible = TRUE, collapsed = FALSE,
                style = "overflow-x: scroll",
                shiny::flowLayout(
                    cellArgs = list(
                        style = "min-width: 600px;
                     width: auto;
                     height: auto;
                     border: 1px solid white;
                     padding: 10px;
                     margin: 10px;"),
                    renderDT(datatable(res2,
                                       options = list(columnDefs = 
                                                          list(list(className = 'dt-center', 
                                                                    targets = "_all"))))))
            )
        }
        
       
        
    })
    
    #### PCA model ####
    
    output$flag2 <- renderUI({
        if (is.null(input$countries_box)) {
            countries3 <- unique(data3$STATE)
            n <- match(input$countries_box_home,countries3)
            HTML(paste(
                tags$img(src=flags3[n], width=100, height=70)#,
                #"Country"
            ))
        } else {
            countries3 <- unique(data3$STATE)
            n <- match(input$countries_box,countries3)
            HTML(paste(
                tags$img(src=flags3[n], width=100, height=70)#,
                #"Country"
            ))   
        }
    })
    
    
    PCA_model <- reactive ({
        if (is.null(input$countries_box)) {
            mydata <- as.data.frame(data3)
            reactive_data <- as.data.frame(mydata %>% filter(STATE == input$countries_box_home)) #input$countries_box
            reactive_data <- reactive_data[, colSums(reactive_data != 0) > 0]
            reactive_data[sapply(reactive_data, function(x) length(unique(na.omit(x)))) > 1]
            test <- reactive_data    #() %>% filter(DATE >= input$date_input[1] & DATE <=input$date_input[2])
            reactiveDf <- test
            data <- reactiveDf[,-1] #()
            data <- data[,-1]########edit#############
            data <- data[,sapply(data, function(v) var(v, na.rm=TRUE)!=0)]
            my_vars <- colnames(data[,-c(1:2)])
            data <- as.data.frame(cbind(data[,c(1:2)],zoo::rollmean(data[,-c(1,2)], 1, fill = "extend")))# input$ma_selection
            data2 <- data[,-c(1:2)]
            colnames(data2) <- my_vars #
            data <- as.data.frame(cbind(data[,c(1:2)],data2)) #
            data <- as.data.frame(data)
            data <- data[complete.cases(data[,]),] #
            my_ts_def <- data
            C <- colnames(my_ts_def)
            nums <- nrow(my_ts_def)
            prova <- my_ts_def[c((nums-30):nums),]
            prova <- prova[, colSums(prova != 0) > 0]
            D <- colnames(prova)
            aa<-setdiff(C,D)
            my_ts_def<-my_ts_def[, !(colnames(my_ts_def) %in% aa), drop = FALSE]
        } else {
            my_ts_def <- my_ts_def()
        }
        
        pca.model <- prcomp(my_ts_def[,-c(1:2)], scale=TRUE, center=TRUE, rank.=2)
        scores<-pca.model$x
        scores <- as.data.frame(scores)
        colnames(scores)=c("PC1","PC2")
        list(pca.model = pca.model, scores = scores)
    })
    
    output$dropdown_pca_scores <- renderUI({
        #req(!is.null(input$countries_box))
        dropdown(
            tags$h3("List of PCs"),
            pickerInput(inputId = 'input_pcascores_x',
                        label = 'Select X axis:',
                        choices = c("PC1","PC2","Date"),
                        selected = "PC1",
                        options = list(`style` = "btn-primary")),
            pickerInput(inputId = 'input_pcascores_y',
                        label = 'Select Y axis:',
                        choices = c("PC1","PC2","Date"),
                        selected = "PC2",
                        options = list(`style` = "btn-info")),
            style = "unite", icon = icon("gear"),
            status = "danger", width = "300px",
            tooltip = tooltipOptions(title = "Click to change scores!"),
            animate = animateOptions(
                enter = animations$fading_entrances$fadeInLeftBig,
                exit = animations$fading_exits$fadeOutRightBig
            )
        )
    })
    
    output$pca_scores <- renderUI({
        # if (is.null(input$countries_box)) {
        #     tags$div(
        #         tags$h2(style="color:red", "Please, go to the 'Select a country' tab first")
        #     )
        # } else {
        pca.model <- PCA_model()$pca.model
        scores <- PCA_model()$scores
        
        nums <- nrow(scores)
        n_rid <- nums - 7
        n_rid2 <- nums -14
        
        if (is.null(input$countries_box)) {
            mydata <- as.data.frame(data3)
            reactive_data <- as.data.frame(mydata %>% filter(STATE == input$countries_box_home)) #input$countries_box
            reactive_data <- reactive_data[, colSums(reactive_data != 0) > 0]
            reactive_data[sapply(reactive_data, function(x) length(unique(na.omit(x)))) > 1]
            test <- reactive_data    #() %>% filter(DATE >= input$date_input[1] & DATE <=input$date_input[2])
            reactiveDf <- test
            data <- reactiveDf[,-1] #()
            data <- data[,-1]########edit#############
            data <- data[,sapply(data, function(v) var(v, na.rm=TRUE)!=0)]
            my_vars <- colnames(data[,-c(1:2)])
            data <- as.data.frame(cbind(data[,c(1:2)],zoo::rollmean(data[,-c(1,2)], 1, fill = "extend")))# input$ma_selection
            data2 <- data[,-c(1:2)]
            colnames(data2) <- my_vars #
            data <- as.data.frame(cbind(data[,c(1:2)],data2)) #
            data <- as.data.frame(data)
            data <- data[complete.cases(data[,]),] #
            my_ts_def <- data
            C <- colnames(my_ts_def)
            nums <- nrow(my_ts_def)
            prova <- my_ts_def[c((nums-30):nums),]
            prova <- prova[, colSums(prova != 0) > 0]
            D <- colnames(prova)
            aa<-setdiff(C,D)
            my_ts_def<-my_ts_def[, !(colnames(my_ts_def) %in% aa), drop = FALSE]
            dates <- as.Date(my_ts_def$DATE)
        } else {
            dates <- as.Date(my_ts_def()$DATE)
        }
        
        scores$Date <- dates
        
        mymaxs <- c(max(abs(scores[[input$input_pcascores_x]])),max(abs(scores[[input$input_pcascores_y]])))
        mymax <- max(mymaxs)
        
        minax <- min(scores[[input$input_pcascores_x]])
        maxax <- max(scores[[input$input_pcascores_x]])
        minay <- min(scores[[input$input_pcascores_y]])
        maxay <- max(scores[[input$input_pcascores_y]])
        
        points <- seq(1,n_rid2,7)
        scores_points <- scores[points,]
        scores_2week <- scores[(n_rid2):n_rid,]
        scores_2week_rid <- scores_2week[c(4,8),]
        scores_1week <- scores[(n_rid):nums,]
        scores_1week_rid <- scores_1week[c(4,7),]
        
        p <- plot_ly(x = scores[[input$input_pcascores_x]][1], y = scores[[input$input_pcascores_y]][1],#
                     #size = I(20),
                     type = 'scatter',
                     mode = 'lines+markers+text', 
                     line = list(color = 'white',  width = 0.5),
                     hoverinfo = 'text',
                     hovertext = paste('<b>Date</b>:', scores$Date[1],#
                                       '<br><b>Country</b>:', input$countries_box),#
                     marker = list(color = 'brown', symbol = "circle",
                                   size = 25), 
                     name = paste("First day:", scores$Date[1]), showlegend = TRUE) %>%
            # add_trace(x = scores[[input$input_pcascores_x]][1], y = scores[[input$input_pcascores_y]][1],#
            #           text = emo::ji("spiral_calendar"),
            #           textfont = list(size =30),
            #           type = 'scatter',
            #           mode = 'lines+markers+text', 
            #           line = list(color = 'white',  width = 0.5),
            #           hoverinfo = 'text',
            #           hovertext = paste('<b>Date</b>:', scores$Date[1],#
            #                             '<br><b>Country</b>:', input$countries_box),#
            #           textposition = "bottom left", 
            #           marker = list(color = 'red',  size = 2), 
            #           name = paste("First day:", scores$Date[1]), showlegend = FALSE) %>% 
            add_trace(x = scores[[input$input_pcascores_x]][1:n_rid2], y = scores[[input$input_pcascores_y]][1:n_rid2],#
                      type = 'scatter',
                      mode = 'lines+markers', 
                      line = list(color = 'red',  width = 2),
                      hoverinfo = 'text',
                      hovertext = paste('<b>Date</b>:', scores$Date[1:n_rid2],
                                        '<br><b>Country</b>:', input$countries_box),#
                      #size = I(15),
                      marker = list(color = 'black', size = 3), 
                      name = paste(dates[1], "to", dates[n_rid2]), showlegend = TRUE) %>%
            add_trace(x = scores_points[[input$input_pcascores_x]], y = scores_points[[input$input_pcascores_y]],#
                      text = format(scores_points$Date, format="%b %d"),
                      type = 'scatter',
                      mode = 'lines+markers+text', 
                      line = list(color = 'white',  width = 0.5),
                      hoverinfo = 'text',
                      hovertext = paste('<b>Date</b>:', scores_points$Date,#
                                        '<br><b>Country</b>:', input$countries_box),#
                      textposition = "bottom left", 
                      marker = list(color = 'red',  size = 2), 
                      name = paste(dates[1], "to", dates[n_rid2]), showlegend = FALSE) %>% 
            add_trace(x = scores_2week[[input$input_pcascores_x]], y = scores_2week[[input$input_pcascores_y]],#
                      type = 'scatter',
                      mode = 'lines+markers', 
                      line = list(color = 'darkorange',  width = 2),
                      hoverinfo = 'text',
                      hovertext = paste('<b>Date</b>:', scores_2week$Date,
                                        '<br><b>Country</b>:', input$countries_box),#
                      marker = list(color = 'black', size = 3), 
                      name = paste(dates[n_rid2+1], "to", dates[n_rid]), showlegend = TRUE) %>%
            add_trace(x = scores_2week_rid[[input$input_pcascores_x]], y = scores_2week_rid[[input$input_pcascores_y]],
                      text = format(scores_2week_rid$Date, format="%b %d"),
                      type = 'scatter',
                      mode = 'lines+markers+text', 
                      line = list(color = 'white',  width = 0.5),
                      hoverinfo = 'text',
                      hovertext = paste('<b>Date</b>:', scores_2week_rid$Date,#
                                        '<br><b>Country</b>:', input$countries_box),#
                      textposition = "top right", 
                      marker = list(color = 'black',  size = 2), 
                      name = paste(dates[n_rid2+1], "to", dates[n_rid]), showlegend = FALSE) %>% 
            add_trace(x = scores_1week[[input$input_pcascores_x]], y = scores_1week[[input$input_pcascores_y]],#
                      type = 'scatter',
                      mode = 'lines+markers', 
                      line = list(color = 'blue',  width = 2),
                      hoverinfo = 'text',
                      hovertext = paste('<b>Date</b>:', scores_1week$Date,
                                        '<br><b>Country</b>:', input$countries_box),#
                      marker = list(color = 'black', size = 3), 
                      name = paste(dates[n_rid+1], "to", dates[nums-1]), showlegend = TRUE) %>%
            add_trace(x = scores_1week_rid[[input$input_pcascores_x]], y = scores_1week_rid[[input$input_pcascores_y]],
                      text = format(scores_1week_rid$Date, format="%b %d"),
                      type = 'scatter',
                      mode = 'lines+markers+text', 
                      line = list(color = 'white',  width = 0.5),
                      hoverinfo = 'text',
                      hovertext = paste('<b>Date</b>:', scores_1week_rid$Date,#
                                        '<br><b>Country</b>:', input$countries_box),#
                      textposition = "top center", 
                      marker = list(color = 'black',  size = 2), 
                      name = paste(dates[n_rid+1], "to", dates[nums-1]), showlegend = FALSE) %>%
            add_trace(x = scores[[input$input_pcascores_x]][nums], y = scores[[input$input_pcascores_y]][nums],#
                      type = 'scatter',
                      text = format(scores$Date[nums], format="%b %d"),
                      hoverinfo = 'text',
                      hovertext = paste('<b>Date</b>:', scores$Date[nums],
                                        '<br><b>Country</b>:', input$countries_box),#
                      mode = 'lines+markers+text', 
                      line = list(color = 'blue', 
                                  width = 2),
                      textposition = "bottom center", 
                      marker = list(color = 'green', symbol = "cross-dot",
                                    size = 25),
                      name = paste("Most recent day:", dates[nums]), showlegend = TRUE) %>%
            layout(dragmode = "pan",
                xaxis = list(title = input$input_pcascores_x, showgrid = F, range = c(minax*1.2, maxax*1.2)),#
                yaxis = list(title = input$input_pcascores_y, showgrid = F, range = c(minay*1.2, maxay*1.2)),#
                hoverlabel = list(bgcolor= 'white'),
                legend = list(orientation = 'h',title=list(text='Dates')),
                height = 700, width = 700
            )
        
        # p <- plot_ly(x = scores[[input$input_pcascores_x]][1:n_rid2],
        #              text = row.names(scores)[1:n_rid2],
        #              hoverinfo = 'text',
        #              hovertext = paste('<b>Date</b>:', scores$Date[1:n_rid2],
        #                                '<br><b>Country</b>:', my_ts_def()[1,1]),
        #              size = I(15),
        #              showlegend = TRUE)  %>%
        #     add_trace(y = scores[[input$input_pcascores_y]][1:n_rid2],
        #               type = 'scatter',
        #               mode = 'lines+markers+text', 
        #               line = list(color = 'red', 
        #                           width = 2),
        #               textposition = "bottom center", 
        #               marker = list(color = 'black', 
        #                             size = 3), 
        #               name = paste(dates[1], "to", dates[n_rid2]), showlegend = TRUE) %>%
        #     add_trace(x = scores[[input$input_pcascores_x]][(n_rid2+1):n_rid], y = scores[[input$input_pcascores_y]][(n_rid2+1):n_rid],
        #               type = 'scatter',
        #               text = row.names(scores)[(n_rid2+1):n_rid],
        #               hoverinfo = 'text',
        #               hovertext = paste('<b>Date</b>:', scores$Date[(n_rid2+1):n_rid],
        #                                 '<br><b>Country</b>:', my_ts_def()[1,1]),
        #               mode = 'lines+markers+text', 
        #               line = list(color = 'orange', 
        #                           width = 2),
        #               textposition = "bottom center", 
        #               marker = list(color = 'black', 
        #                             size = 3),
        #               name = paste(dates[n_rid2+1], "to", dates[n_rid]), showlegend = TRUE) %>%
        #     add_trace(x = scores[[input$input_pcascores_x]][(n_rid+1):nums], y = scores[[input$input_pcascores_y]][(n_rid+1):nums],
        #               type = 'scatter',
        #               text = row.names(scores)[(n_rid+1):nums],
        #               hoverinfo = 'text',
        #               hovertext = paste('<b>Date</b>:', scores$Date[(n_rid+1):nums],
        #                                 '<br><b>Country</b>:', my_ts_def()[1,1]),
        #               mode = 'lines+markers+text', 
        #               line = list(color = 'blue', 
        #                           width = 2),
        #               textposition = "bottom center", 
        #               marker = list(color = 'black', 
        #                             size = 3),
        #               name = paste(dates[n_rid+1], "to", dates[nums]), showlegend = TRUE) %>%
        #     add_trace(x = scores[[input$input_pcascores_x]][nums], y = scores[[input$input_pcascores_y]][nums],
        #               type = 'scatter',
        #               text = row.names(scores)[nums],
        #               hoverinfo = 'text',
        #               hovertext = paste('<b>Date</b>:', scores$Date[nums],
        #                                 '<br><b>Country</b>:', my_ts_def()[1,1]),
        #               mode = 'lines+markers+text', 
        #               line = list(color = 'blue', 
        #                           width = 2),
        #               textposition = "bottom center", 
        #               marker = list(color = 'green', symbol = "hexagram-dot",
        #                             size = 25),
        #               name = paste("Most recent day:", dates[nums]), showlegend = TRUE) %>%
        #     layout(
        #         xaxis = list(title = input$input_pcascores_x, range = c(-mymax*1.05, mymax*1.05), showgrid = F),
        #         yaxis = list(title = input$input_pcascores_y, range = c(-mymax*1.05, mymax*1.05), showgrid = F),
        #         hoverlabel = list(bgcolor= 'white'),
        #         legend = list(orientation = 'h',title=list(text='Dates')),
        #         height = 700, width = 700
        #     )
        
        box(title = "PCA Scores", 
            width = 12, status = "danger", solidHeader = TRUE, align = "left",
            style = "overflow-x: scroll",
            shiny::flowLayout(
                cellArgs = list(
                    style = "min-width: 780px;
                     width: auto;
                     height: 780px;
                     border: 1px solid white;
                     padding: 10px;
                     margin: 10px;"),
                renderPlotly(p))
        )
        #}
    })
    
    output$dropdown_pca_loadings <- renderUI({
        #req(!is.null(input$countries_box))
        dropdown(
            tags$h3("List of PCs"),
            pickerInput(inputId = 'input_pcaloadings_x',
                        label = 'Select X axis:',
                        choices = c("PC1","PC2"),#"Variables"),
                        selected = "PC1",
                        options = list(`style` = "btn-primary")),
            pickerInput(inputId = 'input_pcaloadings_y',
                        label = 'Select Y axis:',
                        choices = c("PC1","PC2"),
                        selected = "PC2",
                        options = list(`style` = "btn-info")),
            style = "unite", icon = icon("gear"),
            status = "danger", width = "300px",
            tooltip = tooltipOptions(title = "Click to change loadings!"),
            animate = animateOptions(
                enter = animations$fading_entrances$fadeInLeftBig,
                exit = animations$fading_exits$fadeOutRightBig
            )
        )
    })
    
    output$pca_loadings <- renderUI({
        # if (is.null(input$countries_box)) {
        #     # tags$div(
        #     #     tags$h2(style="color:red", "Please, go to the 'Select a country' tab first")
        #     # )
        #     return(NULL)
        # } else {
        pca.model <- PCA_model()$pca.model
        loadings <- as.data.frame(pca.model$rotation)
        loadings$Variables <- row.names(loadings)
        
        ts_variables = c("Cases Cum.", "Cases", "Deaths Cum.", "Deaths", "Cases Norm.", "Cases Cum. Norm.", "Deaths Norm.", "Deaths Cum. Norm.")
        ts_variables_2 = c("Ageusia", "Anosmia", "Chills", "Cough", "Eye Pain", "Fever", "Headache", "Nasal Cong.", "Rhinorrea", "Short Breath", "Sore Throat")
        loadings$type = ifelse(loadings$Variables %in% ts_variables, "Values", "Topics")
        loadings$type = as.factor(loadings$type)
        
        mymaxs <- c(max(abs(loadings[[input$input_pcaloadings_x]])),max(abs(loadings[[input$input_pcaloadings_y]])))
        mymax <- max(mymaxs)
        
        if (input$input_pcaloadings_x == "Variables") {
            p <- plot_ly(x = factor(loadings$Variables,levels = loadings$Variables),
                              y = loadings[[input$input_pcaloadings_y]], name = input$input_pcaloadings_y,type = "bar")%>% 
                layout(yaxis = list(title=input$input_pcaloadings_y), dragmode = "pan",
                       legend = list(orientation = 'h',xanchor = "center", x = 0.5),
                       height = 700, width = 700)
        } else {
            # p <- plot_ly(x = loadings[[input$input_pcaloadings_x]], y = loadings[[input$input_pcaloadings_y]]) %>%
            #     add_text(
            #         name = "",
            #         text = row.names(loadings),
            #         hoverinfo = "text",
            #         size = I(15)
            #     ) %>%
            #     layout(
            #         xaxis = list(title = input$input_pcaloadings_x, range = c(-mymax*1.2, mymax*1.2), showgrid = F),
            #         yaxis = list(title = input$input_pcaloadings_y, range = c(-mymax*1.2, mymax*1.2), showgrid = F),
            #         hoverlabel = list(bgcolor= 'white'),
            #         height = 700, width = 700
            #     )
            p <- ggplot(loadings) + 
                        geom_point(aes(.data[[input$input_pcaloadings_x]], .data[[input$input_pcaloadings_y]]),size = 5, color = 'black') + 
                        geom_rangeframe() +
                        geom_vline(xintercept = 0)+
                        geom_hline(yintercept = 0)+
                        xlim(-mymax, mymax)+
                        ylim(-mymax, mymax)+
                        geom_label_repel(aes(.data[[input$input_pcaloadings_x]], .data[[input$input_pcaloadings_y]], label = .data[["Variables"]], 
                                             fill = .data[["type"]]), fontface = 'bold', color = 'white', 
                                             segment.color = 'black', size = 5, box.padding = unit(0.25, 'lines'), 
                                             point.padding = unit(0.5, 'lines')) + 
                        theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                              panel.background = element_blank(), legend.position = "none")+
                        scale_fill_manual(values = setNames(c("blue", "red"), levels(.data[["Variables"]])))
        }
        
        box(title = "PCA Loadings", 
            width = 12, status = "danger", solidHeader = TRUE, align = "left",
            style = "overflow-x: scroll",
            shiny::flowLayout(
                cellArgs = list(
                    style = "min-width: 780px;
                     width: auto;
                     height: 780px;
                     border: 1px solid white;
                     padding: 10px;
                     margin: 10px;"),
                if (input$input_pcaloadings_x == "Variables"){
                    renderPlotly(p)
                } else {
                    renderPlot(p,height=700, width = 700, units="px")
                })
        )
        #}
    })
    
    output$pca_scree_plot <- renderUI({
        # if (is.null(input$countries_box)) {
        #     # tags$div(
        #     #     tags$h2(style="color:red", "Please, go to the 'Select a country' tab first")
        #     # )
        #     return(NULL)
        # } else {
        pca.model <- PCA_model()$pca.model
        eigs <- pca.model$sdev^2
        eigs <- eigs[1:2]
        expl.variance<-rbind(
            SD = sqrt(eigs),
            Proportion = eigs/sum(eigs),
            Cumulative = cumsum(eigs)/sum(eigs))
        colnames(expl.variance)<-c(paste0("PC",1:ncol(expl.variance)))
        expl.variance<-as.data.frame(expl.variance)
        expl.variance <- round(expl.variance[,1:length(eigs)],4)
        
        ay <- list(
            tickfont = list(color = "black"),
            overlaying = "y",
            side = "right",
            title = "Cumulative Variance (%)"
        )
        #???m <- list(l=10, r=35, b=30, t=30)
        p <- plot_ly(x = factor(colnames(expl.variance),levels = colnames(expl.variance)),
                     y = t(expl.variance[2,]*100),name = "Explained Variance (%)",type = "bar")%>% 
            add_trace(x = factor(colnames(expl.variance),levels = colnames(expl.variance)),
                      y = t(expl.variance[3,]*100), type = 'scatter',mode = 'lines+markers',
                      name = "Cumulative Variance (%)", yaxis = "y2")%>% 
            layout(title = "Scree Plot of PCA", yaxis2 = ay, yaxis = list(title="Explained Variance(%)"),
                   legend = list(orientation = 'h',xanchor = "center", x = 0.5),dragmode = "pan",
                   #margin = m,
                   height = 760, width = 760)
        
        box(title = "PCA Scree Plot", 
            width = 12, status = "danger", solidHeader = TRUE, align = "left", collapsible = TRUE, collapsed = TRUE,
            style = "overflow-x: scroll",
            shiny::flowLayout(
                cellArgs = list(
                    style = "min-width: 780px;
                     width: auto;
                     height: 780px;
                     border: 1px solid white;
                     padding: 10px;
                     margin: 10px;"),
                renderPlotly(p))
        )
        #}
    })
    
    output$dropdown_pca_ts <- renderUI({
        #req(!is.null(input$countries_box))
        
        ts_pca <- c("PC1","PC2")
        
        dropdown(
            tags$h3("List of PCs"),
            pickerInput(
                inputId = "pca_select_ts",
                label = "Pick at least one PCs:",
                choices = ts_pca,
                selected = ts_pca[1],
                multiple = TRUE,
                options = list(
                    `live-search` = TRUE,
                    `style` = "btn-primary")),
            style = "unite", icon = icon("gear"),
            status = "danger", width = "300px",
            tooltip = tooltipOptions(title = "Click to change time series!"),
            animate = animateOptions(
                enter = animations$fading_entrances$fadeInLeftBig,
                exit = animations$fading_exits$fadeOutRightBig
            )
        )
    })
    
    output$pca_ts <- renderUI({
        if (is.null(input$countries_box)) {
            scores <- PCA_model()$scores
            mydata <- as.data.frame(data3)
            reactive_data <- as.data.frame(mydata %>% filter(STATE == input$countries_box_home)) #input$countries_box
            reactive_data <- reactive_data[, colSums(reactive_data != 0) > 0]
            reactive_data[sapply(reactive_data, function(x) length(unique(na.omit(x)))) > 1]
            test <- reactive_data    #() %>% filter(DATE >= input$date_input[1] & DATE <=input$date_input[2])
            reactiveDf <- test
            data <- reactiveDf[,-1] #()
            data <- data[,-1]########edit#############
            data <- data[,sapply(data, function(v) var(v, na.rm=TRUE)!=0)]
            my_vars <- colnames(data[,-c(1:2)])
            data <- as.data.frame(cbind(data[,c(1:2)],zoo::rollmean(data[,-c(1,2)], 1, fill = "extend")))# input$ma_selection
            data2 <- data[,-c(1:2)]
            colnames(data2) <- my_vars #
            data <- as.data.frame(cbind(data[,c(1:2)],data2)) #
            data <- as.data.frame(data)
            data <- data[complete.cases(data[,]),] #
            my_ts_def <- data
            C <- colnames(my_ts_def)
            nums <- nrow(my_ts_def)
            prova <- my_ts_def[c((nums-30):nums),]
            prova <- prova[, colSums(prova != 0) > 0]
            D <- colnames(prova)
            aa<-setdiff(C,D)
            my_ts_def<-my_ts_def[, !(colnames(my_ts_def) %in% aa), drop = FALSE]
            my_scores<-cbind(my_ts_def$DATE,scores)
            colnames(my_scores) <- c("Date", "PC1", "PC2")
            my_ts1 <- my_ts_def[-c(1:2)] #[4:20]
            my_ts1 <- cbind(my_ts_def$DATE,my_ts1)
            colnames(my_ts1)[1] <- "DATE"
            
            my_ts2 <- my_ts_def[-c(1:2)] #[4:20]
            my_ts2 <- cbind(my_ts_def$DATE,my_ts2)
            colnames(my_ts2)[1] <- "DATE"
            
            nums <- round(nrow(my_ts1)/7,0)
            xax <- as.Date(my_ts1$DATE)
            
            p <- plot_ly()%>%
                layout(title = paste("Trends Over Time for", input$countries_box_home),
                       xaxis = list(
                           type = 'date',
                           tickformat = "%d/%m/%Y", nticks = nums, tickmode = "array", tickangle = 45))#,
            
            ToAdd <- input$pca_select_ts
            
            for(i in ToAdd){
                p <- p %>% add_trace(x = xax, y = my_scores[[i]], name = i,
                                     type = 'scatter',
                                     mode = 'line+markers',
                                     line = list(color = i, width = 4))
                
            }
            p <- p %>%
                layout(yaxis = list (title = paste("Time series of", str_c(input$pca_select_ts, collapse = ", "))),
                       legend = list(xanchor = "center", y = -0.3, x = 0.5, orientation = 'h'),
                       height = 760, width = 760,dragmode = "pan")
            
            
            box(title = "PCA Time Series", 
                width = 12, status = "success", solidHeader = TRUE, align = "left", collapsible = TRUE, collapsed = TRUE,
                style = "overflow-x: scroll",
                shiny::flowLayout(
                    cellArgs = list(
                        style = "min-width: 780px;
                     width: auto;
                     height: 780px;
                     border: 1px solid white;
                     padding: 10px;
                     margin: 10px;"),
                    renderPlotly(p))
            )
        } else {
        scores <- PCA_model()$scores
        my_scores<-cbind(my_ts_def()$DATE,scores)
        colnames(my_scores) <- c("Date", "PC1", "PC2")
        my_ts1 <- my_ts_def()[-c(1:2)] #[4:20]
        my_ts1 <- cbind(my_ts_def()$DATE,my_ts1)
        colnames(my_ts1)[1] <- "DATE"
        
        my_ts2 <- my_ts_def()[-c(1:2)] #[4:20]
        my_ts2 <- cbind(my_ts_def()$DATE,my_ts2)
        colnames(my_ts2)[1] <- "DATE"
        
        nums <- round(nrow(my_ts1)/7,0)
        xax <- as.Date(my_ts1$DATE)
        
        p <- plot_ly()%>%
            layout(title = paste("Trends Over Time for", input$countries_box),
                   xaxis = list(
                       type = 'date',
                       tickformat = "%d/%m/%Y", nticks = nums, tickmode = "array", tickangle = 45))#,
        
        ToAdd <- input$pca_select_ts
        
        for(i in ToAdd){
            p <- p %>% add_trace(x = xax, y = my_scores[[i]], name = i,
                                 type = 'scatter',
                                 mode = 'line+markers',
                                 line = list(color = i, width = 4))
            
        }
        p <- p %>%
            layout(yaxis = list (title = paste("Time series of", str_c(input$pca_select_ts, collapse = ", "))),
                   legend = list(xanchor = "center", y = -0.3, x = 0.5, orientation = 'h'),
                   height = 760, width = 760,dragmode = "pan")
        
        
        box(title = "PCA Time Series", 
            width = 12, status = "success", solidHeader = TRUE, align = "left", collapsible = TRUE, collapsed = TRUE,
            style = "overflow-x: scroll",
            shiny::flowLayout(
                cellArgs = list(
                    style = "min-width: 780px;
                     width: auto;
                     height: 780px;
                     border: 1px solid white;
                     padding: 10px;
                     margin: 10px;"),
                renderPlotly(p))
        )
        }
    })
    
    # prediction ####
    
    # output$conf_int <- renderUI({
    #     #req(input$view_confint == TRUE)
    #     sliderTextInput(
    #         inputId = "conf_int",
    #         label = "Select % confidence interval:", 
    #         choices = c(50, 80, 90, 95, 99),
    #         selected = 95,
    #         grid = TRUE
    #     )
    # })
    
    output$flag3 <- renderUI({
        if (is.null(input$countries_box)) {
            countries3 <- unique(data3$STATE)
            n <- match(input$countries_box_home,countries3)
            HTML(paste(
                tags$img(src=flags3[n], width=100, height=70)#,
                #"Country"
            ))
        } else {
            countries3 <- unique(data3$STATE)
            n <- match(input$countries_box,countries3)
            HTML(paste(
                tags$img(src=flags3[n], width=100, height=70)#,
                #"Country"
            ))   
        }
    })
    
    pc_prediction <- reactive({
        scores <- PCA_model()$scores
        #conf_level <- (input$conf_int)/100
        conf_level <- 0.95
        alpha <- 1-conf_level
        crit_z <- qnorm(1-alpha/2)
        
        pred_data <- as_tibble(scores)
        if (is.null(input$countries_box)) {
            mydata <- as.data.frame(data3)
            reactive_data <- as.data.frame(mydata %>% filter(STATE == input$countries_box_home)) #input$countries_box
            reactive_data <- reactive_data[, colSums(reactive_data != 0) > 0]
            reactive_data[sapply(reactive_data, function(x) length(unique(na.omit(x)))) > 1]
            test <- reactive_data    #() %>% filter(DATE >= input$date_input[1] & DATE <=input$date_input[2])
            reactiveDf <- test
            data <- reactiveDf[,-1] #()
            data <- data[,-1]########edit#############
            data <- data[,sapply(data, function(v) var(v, na.rm=TRUE)!=0)]
            my_vars <- colnames(data[,-c(1:2)])
            data <- as.data.frame(cbind(data[,c(1:2)],zoo::rollmean(data[,-c(1,2)], 1, fill = "extend")))# input$ma_selection
            data2 <- data[,-c(1:2)]
            colnames(data2) <- my_vars #
            data <- as.data.frame(cbind(data[,c(1:2)],data2)) #
            data <- as.data.frame(data)
            data <- data[complete.cases(data[,]),] #
            my_ts_def <- data
            C <- colnames(my_ts_def)
            nums <- nrow(my_ts_def)
            prova <- my_ts_def[c((nums-30):nums),]
            prova <- prova[, colSums(prova != 0) > 0]
            D <- colnames(prova)
            aa<-setdiff(C,D)
            my_ts_def<-my_ts_def[, !(colnames(my_ts_def) %in% aa), drop = FALSE]
        } else {
            my_ts_def <- my_ts_def()
        }
        
        startDate <- as.Date(min(my_ts_def$DATE))
        endDate <- as.Date(max(my_ts_def$DATE))
        days <- seq(startDate, endDate, "1 day")
        pred_data <- cbind(days,pred_data)
        pred_data <- as_tsibble(pred_data)
        
        fit <- pred_data %>%
            model(
                ets = ETS(PC1), #~ trend("A")),
                arima = fable::ARIMA(PC1)#,era ARIMA(PC1)
                #nnar = NNETAR(PC1)
            )
        
        num_days <- input$predict_days
        h_pred <- as.character(paste(num_days,"days"))
        
        fc <- fit %>%
            forecast(h = h_pred)
        
        fit2 <- pred_data %>%
            model(
                ets = ETS(PC2), # ~ trend("A")),
                arima = fable::ARIMA(PC2)#,era ARIMA(PC2)
                #nnar = NNETAR(PC2)
            )
        
        fc2 <- fit2 %>%
            forecast(h = h_pred)
        
        # ets prediction ####
        days_etsPC1 <- fc %>% 
            as.data.frame(fc)%>%
            filter(.model == "ets")%>%
            select(days, .mean)
        
        etsPC2 <- fc2 %>% 
            as.data.frame(fc2)%>%
            filter(.model == "ets")%>%
            select(.mean)
        
        ets_means <- cbind(days_etsPC1,etsPC2)       
        colnames(ets_means) <- c("Date","PC1","PC2")
        
        days_etsPC1_high_low <- fc %>% 
            as.data.frame(fc)%>%
            filter(.model == "ets")%>%
            select(days, PC1)
        
        days_etsPC2_high_low <- fc2 %>% 
            as.data.frame(fc2)%>%
            filter(.model == "ets")%>%
            select(days, PC2)
        
        days_etsPC1_high_low$PC1 <- as.character(days_etsPC1_high_low$PC1)
        my<-days_etsPC1_high_low %>% separate(PC1, c("A","B"), sep = "([,])")
        my$sd<-as.numeric(gsub(")", "", my$B ))
        days_etsPC2_high_low$PC2 <- as.character(days_etsPC2_high_low$PC2)
        my2<-days_etsPC2_high_low %>% separate(PC2, c("A","B"), sep = "([,])")
        my2$sd<-as.numeric(gsub(")", "", my2$B ))
        ets_prediction <-cbind(ets_means,my$sd,my2$sd)
        ets_prediction$highPC1 <- ets_means$PC1 + crit_z*ets_prediction[,4]
        ets_prediction$lowPC1 <- ets_means$PC1 - crit_z*ets_prediction[,4]
        ets_prediction$highPC2 <- ets_means$PC2 + crit_z*ets_prediction[,5]
        ets_prediction$lowPC2 <- ets_means$PC2 - crit_z*ets_prediction[,5]
        
        
        # arima prediction ####
        days_arimaPC1 <- fc %>% 
            as.data.frame(fc)%>%
            filter(.model == "arima")%>%
            select(days, .mean)
        arimaPC2 <- fc2 %>% 
            as.data.frame(fc2)%>%
            filter(.model == "arima")%>%
            select(.mean)
        arima_means <- cbind(days_arimaPC1,arimaPC2)       
        colnames(arima_means) <- c("Date","PC1","PC2")
        
        days_arimaPC1_high_low <- fc %>% 
            as.data.frame(fc)%>%
            filter(.model == "arima")%>%
            select(days, PC1)
        
        days_arimaPC2_high_low <- fc2 %>% 
            as.data.frame(fc2)%>%
            filter(.model == "arima")%>%
            select(days, PC2)
        
        days_arimaPC1_high_low$PC1 <- as.character(days_arimaPC1_high_low$PC1)
        my<-days_arimaPC1_high_low %>% separate(PC1, c("A","B"), sep = "([,])")
        my$sd<-as.numeric(gsub(")", "", my$B ))
        days_arimaPC2_high_low$PC2 <- as.character(days_arimaPC2_high_low$PC2)
        my2<-days_arimaPC2_high_low %>% separate(PC2, c("A","B"), sep = "([,])")
        my2$sd<-as.numeric(gsub(")", "", my2$B ))
        arima_prediction<-cbind(arima_means,my$sd, my2$sd)
        arima_prediction$highPC1 <- arima_means$PC1 + crit_z*arima_prediction[,4]
        arima_prediction$lowPC1 <- arima_means$PC1 - crit_z*arima_prediction[,4]
        arima_prediction$highPC2 <- arima_means$PC2 + crit_z*arima_prediction[,5]
        arima_prediction$lowPC2 <- arima_means$PC2 - crit_z*arima_prediction[,5]
        
        # nnar prediction ####
        # days_nnarPC1 <- fc %>% 
        #   as.data.frame(fc)%>%
        #   filter(.model == "nnar")%>%
        #   select(days, .mean)
        # 
        # nnarPC2 <- fc2 %>% 
        #   as.data.frame(fc2)%>%
        #   filter(.model == "nnar")%>%
        #   select(.mean)
        # 
        # nnar_means <- cbind(days_nnarPC1,nnarPC2)       
        # colnames(nnar_means) <- c("Date","PC1","PC2")
        # 
        # bo <- fit %>%
        #   generate(times=3, h=input$predict_days) %>%
        #   filter(.model == "nnar") %>%
        #   as.data.frame()%>% 
        #   group_by(days) %>%
        #   summarise(mean = mean(.sim), sd=sd(.sim))
        # bo<-as.data.frame(bo)
        # 
        # bo2 <- fit2 %>%
        #   generate(times=3, h=input$predict_days) %>%
        #   filter(.model == "nnar") %>%
        #   as.data.frame()%>% 
        #   group_by(days) %>%
        #   summarise(mean = mean(.sim), sd=sd(.sim))
        # bo2<-as.data.frame(bo2)
        # 
        # my <- as.data.frame(bo$mean)
        # my$sd <- bo$sd
        # my2 <- as.data.frame(bo2$mean)
        # my2$sd <- bo2$sd
        # nnar_prediction<-cbind(nnar_means,my$sd, my2$sd)
        # nnar_prediction$highPC1 <- nnar_means$PC1 + crit_z*nnar_prediction[,4]
        # nnar_prediction$lowPC1 <- nnar_means$PC1 - crit_z*nnar_prediction[,4]
        # nnar_prediction$highPC2 <- nnar_means$PC2 + crit_z*nnar_prediction[,5]
        # nnar_prediction$lowPC2 <- nnar_means$PC2 - crit_z*nnar_prediction[,5]
        # 
        
        list(ets_prediction = ets_prediction, arima_prediction = arima_prediction, #nnar_prediction = nnar_prediction,
             fit = fit, fit2 = fit2, fc = fc, fc2 = fc2, arima_means = arima_means, ets_means = ets_means)
        
    })
    
    output$parameter_prediction_new_plot <- renderUI({
        #req(!is.null(input$countries_box))
        choice <- c("Cases", "Cases Cum.","Cases Norm.", "Cases Cum. Norm.", "Deaths", "Deaths Cum.",  
                     "Deaths Norm.", "Deaths Cum. Norm.", "PC1", "PC2")
        
        selectInput(#pickerInput
                inputId = "predicted_new_variables",
                label = "Predicted COVID-19 metrics:",
                choices = choice,
                selected = "Cases",
                multiple = FALSE,
                #options = list(
                    #`live-search` = TRUE#,
                    #`style` = "btn-primary"
                    )#)
            
    })
    
    output$predict_days <- renderUI({#predict_days  #refine_prediction
        #req(!is.null(input$countries_box))
        box(title = paste("How many days to predict?"),#Refine days (last) for prediction
            width = 12, status = "warning", solidHeader = TRUE, align = "center",
            style = "overflow-x: scroll",
            shiny::flowLayout(
                cellArgs = list(
                    style = "width: 200px;
                     height: auto;
                     border: 1px solid white;
                     padding: 10px;
                     margin: 10px;"),
                numericInputIcon(
                    inputId = "predict_days", #refine_prediction
                    label = NULL,#"Select days:",
                    value = 14,
                    icon = list("days")
                ))
        )
    })
    
    predicted_data <- reactive({
        
        if (is.null(input$countries_box)) {
            mydata <- as.data.frame(data3)
            reactive_data <- as.data.frame(mydata %>% filter(STATE == input$countries_box_home)) #input$countries_box
            reactive_data <- reactive_data[, colSums(reactive_data != 0) > 0]
            reactive_data[sapply(reactive_data, function(x) length(unique(na.omit(x)))) > 1]
            test <- reactive_data    #() %>% filter(DATE >= input$date_input[1] & DATE <=input$date_input[2])
            reactiveDf <- test
            data <- reactiveDf[,-1] #()
            data <- data[,-1]########edit#############
            data <- data[,sapply(data, function(v) var(v, na.rm=TRUE)!=0)]
            my_vars <- colnames(data[,-c(1:2)])
            data <- as.data.frame(cbind(data[,c(1:2)],zoo::rollmean(data[,-c(1,2)], 1, fill = "extend")))# input$ma_selection
            data2 <- data[,-c(1:2)]
            colnames(data2) <- my_vars #
            data <- as.data.frame(cbind(data[,c(1:2)],data2)) #
            data <- as.data.frame(data)
            data <- data[complete.cases(data[,]),] #
            my_ts_def <- data
            C <- colnames(my_ts_def)
            nums <- nrow(my_ts_def)
            prova <- my_ts_def[c((nums-30):nums),]
            prova <- prova[, colSums(prova != 0) > 0]
            D <- colnames(prova)
            aa<-setdiff(C,D)
            my_ts_def<-my_ts_def[, !(colnames(my_ts_def) %in% aa), drop = FALSE]
        } else {
            my_ts_def <- my_ts_def()
        }
        
        my_ts_def <- my_ts_def
        nums <- nrow(my_ts_def)
        pca2 <- prcomp(my_ts_def[c((nums-input$refine_prediction):nums),-c(1:2)], scale=TRUE, center=TRUE, rank.=2)
        my_new_ts <- my_ts_def[c((nums-input$refine_prediction):nums),]
        scores <- pca2$x
        #conf_level <- (input$conf_int)/100
        conf_level <- 0.95
        alpha <- 1-conf_level
        crit_z <- qnorm(1-alpha/2)
        
        pred_data <- as_tibble(scores)
        startDate <- as.Date(min(my_new_ts$DATE))
        endDate <- as.Date(max(my_new_ts$DATE))
        days <- seq(startDate, endDate, "1 day")
        pred_data <- cbind(days,pred_data)
        pred_data <- as_tsibble(pred_data)
        
        fit <- pred_data %>%
            model(
                ets = ETS(PC1),
                arima = fable::ARIMA(PC1)
            )
        
        num_days <- input$predict_days
        h_pred <- as.character(paste(num_days,"days"))
        
        fc <- fit %>%
            forecast(h = h_pred)
        
        fit2 <- pred_data %>%
            model(
                ets = ETS(PC2), 
                arima = fable::ARIMA(PC2)
            )
        
        fc2 <- fit2 %>%
            forecast(h = h_pred)
        
        # ets prediction ####
        days_etsPC1 <- fc %>% 
            as.data.frame(fc)%>%
            filter(.model == "ets")%>%
            select(days, .mean)
        
        etsPC2 <- fc2 %>% 
            as.data.frame(fc2)%>%
            filter(.model == "ets")%>%
            select(.mean)
        
        ets_means <- cbind(days_etsPC1,etsPC2)       
        colnames(ets_means) <- c("Date","PC1","PC2")
        
        days_etsPC1_high_low <- fc %>% 
            as.data.frame(fc)%>%
            filter(.model == "ets")%>%
            select(days, PC1)
        
        days_etsPC2_high_low <- fc2 %>% 
            as.data.frame(fc2)%>%
            filter(.model == "ets")%>%
            select(days, PC2)
        
        days_etsPC1_high_low$PC1 <- as.character(days_etsPC1_high_low$PC1)
        my<-days_etsPC1_high_low %>% separate(PC1, c("A","B"), sep = "([,])")
        my$sd<-as.numeric(gsub(")", "", my$B ))
        
        days_etsPC2_high_low$PC2 <- as.character(days_etsPC2_high_low$PC2)
        my2<-days_etsPC2_high_low %>% separate(PC2, c("A","B"), sep = "([,])")
        my2$sd<-as.numeric(gsub(")", "", my2$B ))
        
        ets_prediction <-cbind(ets_means,my$sd,my2$sd)
        ets_prediction$highPC1 <- ets_means$PC1 + crit_z*ets_prediction[,4]
        ets_prediction$lowPC1 <- ets_means$PC1 - crit_z*ets_prediction[,4]
        
        ets_prediction$highPC2 <- ets_means$PC2 + crit_z*ets_prediction[,5]
        ets_prediction$lowPC2 <- ets_means$PC2 - crit_z*ets_prediction[,5]
        # 
        # arima prediction ####
        days_arimaPC1 <- fc %>% 
            as.data.frame(fc)%>%
            filter(.model == "arima")%>%
            select(days, .mean)
        arimaPC2 <- fc2 %>% 
            as.data.frame(fc2)%>%
            filter(.model == "arima")%>%
            select(.mean)
        arima_means <- cbind(days_arimaPC1,arimaPC2)       
        colnames(arima_means) <- c("Date","PC1","PC2")
        
        days_arimaPC1_high_low <- fc %>% 
            as.data.frame(fc)%>%
            filter(.model == "arima")%>%
            select(days, PC1)
        
        days_arimaPC2_high_low <- fc2 %>% 
            as.data.frame(fc2)%>%
            filter(.model == "arima")%>%
            select(days, PC2)
        
        days_arimaPC1_high_low$PC1 <- as.character(days_arimaPC1_high_low$PC1)
        my<-days_arimaPC1_high_low %>% separate(PC1, c("A","B"), sep = "([,])")
        my$sd<-as.numeric(gsub(")", "", my$B ))
        
        days_arimaPC2_high_low$PC2 <- as.character(days_arimaPC2_high_low$PC2)
        my2<-days_arimaPC2_high_low %>% separate(PC2, c("A","B"), sep = "([,])")
        my2$sd<-as.numeric(gsub(")", "", my2$B ))
        
        arima_prediction<-cbind(arima_means,my$sd, my2$sd)
        arima_prediction$highPC1 <- arima_means$PC1 + crit_z*arima_prediction[,4]
        arima_prediction$lowPC1 <- arima_means$PC1 - crit_z*arima_prediction[,4]
        
        arima_prediction$highPC2 <- arima_means$PC2 + crit_z*arima_prediction[,5]
        arima_prediction$lowPC2 <- arima_means$PC2 - crit_z*arima_prediction[,5]
        
        arima_pred <- as.data.frame(t(t(as.matrix(arima_prediction[,c(2,3)]) %*% t(pca2$rotation)) * pca2$scale + pca2$center))
        ets_pred <- as.data.frame(t(t(as.matrix(ets_prediction[,c(2,3)]) %*% t(pca2$rotation)) * pca2$scale + pca2$center))
        model <- c(rep("arima",nrow(arima_pred)),rep("ets",nrow(ets_pred)))
        Date2 <- c(arima_prediction$Date,ets_prediction$Date)
        arima_pred <- cbind(arima_pred, arima_prediction[,c(2,3)])
        ets_pred <- cbind(ets_pred, ets_prediction[,c(2,3)])
        reversed_prediction <- rbind(arima_pred, ets_pred)
        reversed_prediction <- cbind(Date2, model, reversed_prediction)
        colnames(reversed_prediction) <- c("DATE", "model", colnames(arima_pred))
        new <- my_ts_def
        new <- cbind(new,PCA_model()$scores)
        colnames(new)[2] <- "model"
        arima_reversed <- subset(reversed_prediction, model == "arima")
        ets_reversed <- subset(reversed_prediction, model == "ets")
        link_arima <- rbind(new[nrow(new),], arima_reversed[1,])
        link_ets <- rbind(new[nrow(new),], ets_reversed[1,])
        
        arima_pred_high <- as.data.frame(t(t(as.matrix(arima_prediction[,c(6,8)]) %*% t(pca2$rotation)) * pca2$scale + pca2$center))
        ets_pred_high <- as.data.frame(t(t(as.matrix(ets_prediction[,c(6,8)]) %*% t(pca2$rotation)) * pca2$scale + pca2$center))
        model_high <- c(rep("arima",nrow(arima_pred_high)),rep("ets",nrow(ets_pred_high)))
        Date2 <- c(arima_prediction$Date,ets_prediction$Date)
        arima_pred_high <- cbind(arima_pred_high, arima_prediction[,c(2,3)])
        ets_pred_high <- cbind(ets_pred_high, ets_prediction[,c(2,3)])
        reversed_prediction_high <- rbind(arima_pred_high, ets_pred_high)
        reversed_prediction_high <- cbind(Date2, model_high, reversed_prediction_high)
        colnames(reversed_prediction_high) <- c("DATE", "model", colnames(arima_pred_high))
        new <- my_ts_def
        new <- cbind(new,PCA_model()$scores)
        colnames(new)[2] <- "model"
        arima_reversed_high <- subset(reversed_prediction_high, model == "arima")
        ets_reversed_high <- subset(reversed_prediction_high, model == "ets")
        # link_arima <- rbind(new[nrow(new),], arima_reversed[1,])
        # link_ets <- rbind(new[nrow(new),], ets_reversed[1,])
        
        arima_pred_low <- as.data.frame(t(t(as.matrix(arima_prediction[,c(7,9)]) %*% t(pca2$rotation)) * pca2$scale + pca2$center))
        ets_pred_low <- as.data.frame(t(t(as.matrix(ets_prediction[,c(7,9)]) %*% t(pca2$rotation)) * pca2$scale + pca2$center))
        model_low <- c(rep("arima",nrow(arima_pred_low)),rep("ets",nrow(ets_pred_low)))
        Date2 <- c(arima_prediction$Date,ets_prediction$Date)
        arima_pred_low <- cbind(arima_pred_low, arima_prediction[,c(2,3)])
        ets_pred_low <- cbind(ets_pred_low, ets_prediction[,c(2,3)])
        reversed_prediction_low <- rbind(arima_pred_low, ets_pred_low)
        reversed_prediction_low <- cbind(Date2, model_low, reversed_prediction_low)
        colnames(reversed_prediction_low) <- c("DATE", "model", colnames(arima_pred_low))
        new <- my_ts_def
        new <- cbind(new,PCA_model()$scores)
        colnames(new)[2] <- "model"
        arima_reversed_low <- subset(reversed_prediction_low, model == "arima")
        ets_reversed_low <- subset(reversed_prediction_low, model == "ets")
        
        list(my_ts_def = my_ts_def, arima_reversed_low = arima_reversed_low, arima_reversed_high = arima_reversed_high,
             ets_reversed_low = ets_reversed_low, ets_reversed_high = ets_reversed_high, link_arima = link_arima,
             link_ets = link_ets, arima_reversed = arima_reversed, ets_reversed = ets_reversed)
    })
    
    output$new_prediction_plot <- renderUI({
        # #req(!is.null(input$input$countries_box))
        # if (is.null(input$countries_box)) {
        #     tags$div(
        #         tags$h2(style="color:red", "Please, go to the 'Select a country' tab first")
        #     )
        # } else {
            
        my_ts_def = predicted_data()$my_ts_def
        arima_reversed_low = predicted_data()$arima_reversed_low
        arima_reversed_high = predicted_data()$arima_reversed_high
        ets_reversed_low = predicted_data()$ets_reversed_low
        ets_reversed_high = predicted_data()$ets_reversed_high
        link_arima = predicted_data()$link_arima
        link_ets = predicted_data()$link_ets
        arima_reversed = predicted_data()$arima_reversed
        ets_reversed = predicted_data()$ets_reversed
        
        data.table::setDT(my_ts_def)[Cases < 0, Cases := 0]
        data.table::setDT(my_ts_def)[Deaths < 0, Deaths := 0]
        data.table::setDT(arima_reversed_low)[Cases < 0, Cases := 0]
        data.table::setDT(arima_reversed_low)[Deaths < 0, Deaths := 0]
        data.table::setDT(arima_reversed_high)[Cases < 0, Cases := 0]
        data.table::setDT(arima_reversed_high)[Deaths < 0, Deaths := 0]
        data.table::setDT(ets_reversed_low)[Cases < 0, Cases := 0]
        data.table::setDT(ets_reversed_low)[Deaths < 0, Deaths := 0]
        data.table::setDT(ets_reversed_high)[Cases < 0, Cases := 0]
        data.table::setDT(ets_reversed_high)[Deaths < 0, Deaths := 0]
        data.table::setDT(link_arima)[Cases < 0, Cases := 0]
        data.table::setDT(link_arima)[Deaths < 0, Deaths := 0]
        data.table::setDT(link_ets)[Cases < 0, Cases := 0]
        data.table::setDT(link_ets)[Deaths < 0, Deaths := 0]
        data.table::setDT(arima_reversed)[Cases < 0, Cases := 0]
        data.table::setDT(arima_reversed)[Deaths < 0, Deaths := 0]
        data.table::setDT(ets_reversed)[Cases < 0, Cases := 0]
        data.table::setDT(ets_reversed)[Deaths < 0, Deaths := 0]
        
     
    if (input$view_confint == TRUE) {
            p <- plot_ly(x = my_ts_def$DATE, y = my_ts_def[[input$predicted_new_variables]], type = 'scatter', mode = 'lines',
                         line = list(color = 'green'),
                         showlegend = TRUE, name = paste0('Measured ', input$predicted_new_variables))%>% 
                add_trace(x = arima_reversed_low$DATE, y = arima_reversed_low[[input$predicted_new_variables]], type = 'scatter', mode = 'lines',
                          line = list(color = 'rgba(255, 80, 80,0.2)'),
                          showlegend = FALSE, name = paste0('ets ',95,"%"))%>% #input$conf_int
                add_trace(x = arima_reversed_high$DATE, y = arima_reversed_high[[input$predicted_new_variables]], type = 'scatter', mode = 'lines',
                          fill = 'tonexty', fillcolor='rgba(255, 80, 80,0.2)', line = list(color = 'rgba(255, 80, 80,0.2)'),
                          showlegend = FALSE, name = paste0('ets ',95,"%"))%>%
                add_trace(x = ets_reversed_low$DATE, y = ets_reversed_low[[input$predicted_new_variables]], type = 'scatter', mode = 'lines',
                          line = list(color = 'rgba(0, 102, 255,0.2)'),
                          showlegend = FALSE, name = paste0('ets ',95,"%"))%>% 
                add_trace(x = ets_reversed_high$DATE, y = ets_reversed_high[[input$predicted_new_variables]], type = 'scatter', mode = 'lines',
                          fill = 'tonexty', fillcolor='rgba(0, 102, 255, 0.2)', line = list(color = 'rgba(0, 102, 255,0.2)'),
                          showlegend = FALSE, name = paste0('ets ',95,"%"))%>%
                add_trace(x = link_arima$DATE, y = link_arima[[input$predicted_new_variables]], type = 'scatter', mode = 'lines',
                          line = list(color = 'red'), showlegend = FALSE)%>%
                add_trace(x = link_ets$DATE, y = link_ets[[input$predicted_new_variables]], type = 'scatter', mode = 'lines',
                          line = list(color = 'blue'), showlegend = FALSE)%>%
                add_trace(x = arima_reversed$DATE, y = arima_reversed[[input$predicted_new_variables]], type = 'scatter', mode = 'lines',
                          line = list(color = 'red'), showlegend = TRUE, name = paste0('Predicted by arima'))%>% 
                add_trace(x = ets_reversed$DATE, y = ets_reversed[[input$predicted_new_variables]], type = 'scatter', mode = 'lines',
                          line = list(color = 'blue'), showlegend = TRUE, name = paste0('Predicted by ets'))%>% 
                layout(dragmode = "pan", title = input$predicted_new_variables, 
                       xaxis = list(showgrid = F, range = c(max(my_ts_def$DATE)-270, max(my_ts_def$DATE)+input$predict_days)),
                       yaxis = list(showgrid = F),  height = 400, width = 1400)
        } else {
            p <- plot_ly(x = my_ts_def$DATE, y = my_ts_def[[input$predicted_new_variables]], type = 'scatter', mode = 'lines',
                         line = list(color = 'green'),
                         showlegend = TRUE, name = paste0('Measured ', input$predicted_new_variables))%>% 
                add_trace(x = link_arima$DATE, y = link_arima[[input$predicted_new_variables]], type = 'scatter', mode = 'lines',
                          line = list(color = 'red'), showlegend = FALSE)%>%
                add_trace(x = link_ets$DATE, y = link_ets[[input$predicted_new_variables]], type = 'scatter', mode = 'lines',
                          line = list(color = 'blue'), showlegend = FALSE)%>%
                add_trace(x = arima_reversed$DATE, y = arima_reversed[[input$predicted_new_variables]], type = 'scatter', mode = 'lines',
                          line = list(color = 'red'), showlegend = TRUE, name = paste0('Predicted by arima'))%>% 
                add_trace(x = ets_reversed$DATE, y = ets_reversed[[input$predicted_new_variables]], type = 'scatter', mode = 'lines',
                          line = list(color = 'blue'), showlegend = TRUE, name = paste0('Predicted by ets'))%>% 
                layout(dragmode = "pan", title = input$predicted_new_variables, 
                       xaxis = list(showgrid = F, range = c(max(my_ts_def$DATE)-270, max(my_ts_def$DATE)+input$predict_days)),
                       yaxis = list(showgrid = F), height = 400, width = 1400)  
        }
        
        box(title = paste(input$predicted_new_variables, "prediction using last", input$refine_prediction, "days of information"), 
            width = 12, status = "danger", solidHeader = TRUE, align = "center",
            style = "overflow-x: scroll",
            shiny::flowLayout(
                cellArgs = list(
                    style = "
                     min-width: 790px;
                     min-heigth: 430px;
                     width: auto;
                     height: auto;
                     border: 1px solid white;
                     padding: 10px;
                     margin: 10px;"),
                renderPlotly(p))
        )
        #}
    })
    
    output$select_model_prediction <- renderUI({
        #req(!is.null(input$countries_box))
        dropdown(
            tags$h3("Select model for PCA plot"),
            pickerInput(
                inputId = "select_model_prediction",
                label = "Pick one model:", 
                choices = c("ets", "arima"),#"nnar"
                selected = "ets",
                options = list(
                    title = "Select one model")
            ),
            style = "unite", icon = icon("gear"),
            status = "danger", width = "300px",
            tooltip = tooltipOptions(title = "Click to change model"),
            animate = animateOptions(
                enter = animations$fading_entrances$fadeInLeftBig,
                exit = animations$fading_exits$fadeOutRightBig
            )
        )
    })
    
    # output$select_model_prediction <- renderUI({
    #     #req(input$view_confint == TRUE)
    #     pickerInput(
    #         inputId = "select_model_prediction",
    #         label = "Select model for PCA plot:", 
    #         choices = c("ets", "arima"),#"nnar"
    #         selected = "ets",
    #         options = list(
    #             title = "Select one model")
    #     )
    # })
    
    output$predict_pca_scores_plot <- renderUI({
        # if (is.null(input$countries_box)) {
        #     # tags$div(
        #     #     tags$h2(style="color:red", "Please, go to the 'Select a country' tab first")
        #     # )
        #     return(NULL)
        # } else {
        # #req(!is.null(input$input$countries_box))
        if (is.null(input$countries_box)) {
            mydata <- as.data.frame(data3)
            reactive_data <- as.data.frame(mydata %>% filter(STATE == input$countries_box_home)) #input$countries_box
            reactive_data <- reactive_data[, colSums(reactive_data != 0) > 0]
            reactive_data[sapply(reactive_data, function(x) length(unique(na.omit(x)))) > 1]
            test <- reactive_data    #() %>% filter(DATE >= input$date_input[1] & DATE <=input$date_input[2])
            reactiveDf <- test
            data <- reactiveDf[,-1] #()
            data <- data[,-1]########edit#############
            data <- data[,sapply(data, function(v) var(v, na.rm=TRUE)!=0)]
            my_vars <- colnames(data[,-c(1:2)])
            data <- as.data.frame(cbind(data[,c(1:2)],zoo::rollmean(data[,-c(1,2)], 1, fill = "extend")))# input$ma_selection
            data2 <- data[,-c(1:2)]
            colnames(data2) <- my_vars #
            data <- as.data.frame(cbind(data[,c(1:2)],data2)) #
            data <- as.data.frame(data)
            data <- data[complete.cases(data[,]),] #
            my_ts_def <- data
            C <- colnames(my_ts_def)
            nums <- nrow(my_ts_def)
            prova <- my_ts_def[c((nums-30):nums),]
            prova <- prova[, colSums(prova != 0) > 0]
            D <- colnames(prova)
            aa<-setdiff(C,D)
            my_ts_def<-my_ts_def[, !(colnames(my_ts_def) %in% aa), drop = FALSE]
        } else {
            my_ts_def <- my_ts_def()
        }
        scores <- PCA_model()$scores
        ets_prediction <- pc_prediction()$ets_prediction
        arima_prediction <- pc_prediction()$arima_prediction
        
        ets_prediction$days <- as.numeric(seq(1,nrow(ets_prediction)))
        arima_prediction$days <- as.numeric(seq(1,nrow(arima_prediction)))
        
        arima_reversed = arima_prediction
        ets_reversed = ets_prediction
        
        
        if (input$select_model_prediction == "ets") {
            p <- plot_ly(x = scores$PC1,
                         text = row.names(scores),
                         hoverinfo = 'text',
                         hovertext = paste('<b>Date</b>:', my_ts_def$DATE,
                                           '<br><b>Country</b>:', my_ts_def[1,1]),
                         size = I(15),
                         name = paste(min(my_ts_def$DATE), "to", max(my_ts_def$DATE)),
                         showlegend = TRUE)  %>%
                add_trace(y = scores$PC2,
                          type = 'scatter',
                          mode = 'lines+markers+text', 
                          line = list(color = 'red', 
                                      width = 2),
                          textposition = "bottom center",
                          marker = list(color = 'black', 
                                        size = 8),
                          hoverlabel = list(bgcolor= 'white'))%>% 
                add_trace(x = ets_reversed$PC1,
                          y = ets_reversed$PC2,
                          type = 'scatter',
                          mode = 'markers', 
                          marker = list(
                              color= ets_reversed$days,
                              symbol= ets_reversed$days,
                              size=14,
                              opacity=.9
                          ),
                          text = ets_reversed$days,
                          hoverinfo = 'text',
                          hovertext = paste('<b>Date</b>:', ets_reversed$Date,
                                            '<br><b>Country</b>:',my_ts_def[1,1]),
                          hoverlabel = list(bgcolor= 'blue'),
                          showlegend = TRUE,
                          name = ets_reversed$Date)%>%
                layout(height = 700, width = 1400,dragmode = "pan",
                    xaxis = list(title = "Component 1", showgrid = F),
                    yaxis = list(title = "Component 2", showgrid = F),
                    showlegend = TRUE, 
                    legend=list(title=list(text='<b> ets prediction </b>')))
            
        } else if (input$select_model_prediction == "arima") {
            p <- plot_ly(x = scores$PC1,
                         text = row.names(scores),
                         hoverinfo = 'text',
                         hovertext = paste('<b>Date</b>:', my_ts_def$Date,
                                           '<br><b>Country</b>:', my_ts_def[1,1]),
                         size = I(15),
                         name = paste(min(my_ts_def$DATE), "to", max(my_ts_def$DATE)),
                         showlegend = TRUE)  %>%
                add_trace(y = scores$PC2,
                          type = 'scatter',
                          mode = 'lines+markers+text', 
                          line = list(color = 'red', 
                                      width = 2),
                          textposition = "bottom center", # here the text position
                          marker = list(color = 'black', 
                                        size = 8),
                          hoverlabel = list(bgcolor= 'white')) %>% 
                add_trace(x = arima_reversed$PC1,
                          y = arima_reversed$PC2,
                          type = 'scatter',
                          mode = 'markers', 
                          marker = list(
                              color= arima_reversed$days,
                              symbol= arima_reversed$days,
                              size=14,
                              opacity=.9
                          ),
                          text = arima_reversed$days,
                          hoverinfo = 'text',
                          hovertext = paste('<b>Date</b>:', arima_reversed$Date,
                                            '<br><b>Country</b>:',my_ts_def[1,1]),
                          hoverlabel = list(bgcolor= 'red'),
                          showlegend = TRUE,
                          name = arima_reversed$Date)%>%
                layout(height = 700, width = 1400,dragmode = "pan",
                    xaxis = list(title = "Component 1", showgrid = F),
                    yaxis = list(title = "Component 2", showgrid = F),
                    showlegend = TRUE, 
                    legend=list(title=list(text='<b> ARIMA prediction </b>')))
            
        } else {
            p <- plot_ly(x = scores$PC1,
                         text = row.names(scores),
                         hoverinfo = 'text',
                         hovertext = paste('<b>Date</b>:', my_ts_def$DATE,
                                           '<br><b>Country</b>:', my_ts_def[1,1]),
                         size = I(15),
                         name = paste(min(my_ts_def$DATE), "to", max(my_ts_def$DATE)),
                         showlegend = TRUE)  %>%
                add_trace(y = scores$PC2,
                          type = 'scatter',
                          mode = 'lines+markers+text', 
                          line = list(color = 'red', 
                                      width = 2),
                          textposition = "bottom center", # here the text position
                          marker = list(color = 'black', 
                                        size = 8),
                          hoverlabel = list(bgcolor= 'white'))%>% 
                # add_trace(x = nnar_prediction$PC1,
                #                    y = nnar_prediction$PC2,
                #                    type = 'scatter',
                #                    mode = 'markers', 
                #                    marker = list(
                #                      color= nnar_prediction$days,
                #                      symbol= nnar_prediction$days,
                #                      size=14,
                #                      opacity=.9
                #                    ),
                #                    text = row.names(nnar_prediction),
            #                    hoverinfo = 'text',
            #                    hovertext = paste('<b>Date</b>:', nnar_prediction$Date,
            #                                      '<br><b>Country</b>:', my_ts()[1,1]),
            #                    hoverlabel = list(bgcolor= 'red'),
            #                    showlegend = TRUE,
            #                    name = arima_prediction$Date)%>%
            layout(height = 700, width = 1400,dragmode = "pan",
                xaxis = list(title = "Component 1", showgrid = F),
                yaxis = list(title = "Component 2", showgrid = F),
                showlegend = TRUE, 
                legend=list(title=list(text='<b> prediction </b>')))
        }
        
        
        box(title = paste("Prediction on PCA Scores plot using", input$select_model_prediction), 
            width = 12, status = "danger", solidHeader = TRUE, align = "center", collapsible = TRUE, collapsed = TRUE,
            style = "overflow-x: scroll",
            shiny::flowLayout(
                cellArgs = list(
                    style = "min-width: 790px;
                     min-height: 730px;
                     width: auto;
                     height: auto;
                     border: 1px solid white;
                     padding: 10px;
                     margin: 10px;"),
                renderPlotly(p))
        )
        #}
    })
    
    # validation ####
    output$flag4 <- renderUI({
        if (is.null(input$countries_box)) {
            countries3 <- unique(data3$STATE)
            n <- match(input$countries_box_home,countries3)
            HTML(paste(
                tags$img(src=flags3[n], width=100, height=70)#,
                #"Country"
            ))
        } else {
            countries3 <- unique(data3$STATE)
            n <- match(input$countries_box,countries3)
            HTML(paste(
                tags$img(src=flags3[n], width=100, height=70)#,
                #"Country"
            ))   
        }
    })
    
    # validation_data <- reactive({
    #     
    #     my_ts_def <- my_ts_def()
    #     nums <- nrow(my_ts_def)
    #     pca2 <- prcomp(my_ts_def[c((nums-7-30+1):(nums-7)),-c(1:2)], scale=TRUE, center=TRUE, rank.=2)#
    #     my_new_ts <- my_ts_def[c((nums-7-30+1):(nums-7)),]#
    #     scores <- pca2$x
    #     
    #     pred_data <- as_tibble(scores)
    #     startDate <- as.Date(min(my_new_ts$DATE))
    #     endDate <- as.Date(max(my_new_ts$DATE))
    #     days <- seq(startDate, endDate, "1 day")
    #     pred_data <- cbind(days,pred_data)
    #     pred_data <- as_tsibble(pred_data)
    #     
    #     fit <- pred_data %>%
    #         model(
    #             ets = ETS(PC1),
    #             arima = fable::ARIMA(PC1)
    #         )
    #     
    #     num_days <- 7
    #     h_pred <- as.character(paste(num_days,"days"))
    #     
    #     fc <- fit %>%
    #         forecast(h = h_pred)
    #     
    #     fit2 <- pred_data %>%
    #         model(
    #             ets = ETS(PC2), 
    #             arima = fable::ARIMA(PC2)
    #         )
    #     
    #     fc2 <- fit2 %>%
    #         forecast(h = h_pred)
    #     
    #     days_etsPC1 <- fc %>% 
    #         as.data.frame(fc)%>%
    #         filter(.model == "ets")%>%
    #         select(days, .mean)
    #     
    #     etsPC2 <- fc2 %>% 
    #         as.data.frame(fc2)%>%
    #         filter(.model == "ets")%>%
    #         select(.mean)
    #     
    #     ets_means <- cbind(days_etsPC1,etsPC2)       
    #     colnames(ets_means) <- c("Date","PC1","PC2")
    #     
    #     days_arimaPC1 <- fc %>% 
    #         as.data.frame(fc)%>%
    #         filter(.model == "arima")%>%
    #         select(days, .mean)
    #     arimaPC2 <- fc2 %>% 
    #         as.data.frame(fc2)%>%
    #         filter(.model == "arima")%>%
    #         select(.mean)
    #     arima_means <- cbind(days_arimaPC1,arimaPC2)       
    #     colnames(arima_means) <- c("Date","PC1","PC2")
    #     
    #     arima_pred <- as.data.frame(t(t(as.matrix(arima_means[,c(2,3)]) %*% t(pca2$rotation)) * pca2$scale + pca2$center))
    #     ets_pred <- as.data.frame(t(t(as.matrix(ets_means[,c(2,3)]) %*% t(pca2$rotation)) * pca2$scale + pca2$center))
    #     model <- c(rep("arima",nrow(arima_pred)),rep("ets",nrow(ets_pred)))
    #     Date2 <- c(arima_means$Date,ets_means$Date)
    #     arima_pred <- cbind(arima_pred, arima_means[,c(2,3)])
    #     ets_pred <- cbind(ets_pred, ets_means[,c(2,3)])
    #     reversed_prediction <- rbind(arima_pred, ets_pred)
    #     reversed_prediction <- cbind(Date2, model, reversed_prediction)
    #     colnames(reversed_prediction) <- c("DATE", "model", colnames(arima_pred))
    #     
    #     list(reversed_prediction = reversed_prediction, nums = nums)
    # })

    output$dropdown_fit <- renderUI({
        # if (is.null(input$countries_box)) {
        #     return(NULL)
        # } else {
        dropdown(
            tags$h3("List of inputs"),
            pickerInput(
                inputId = "select_graph",
                label = "Select type of plot:",
                choices = c("scatter", "series"),#"nnar"
                selected = "series",
                options = list(
                    title = "Select one graph:",
                    `style` = "btn-success")
            ),
            pickerInput(
                inputId = "select_model_validation",
                label = "Select model for fitting:",
                choices = c("ets", "arima"),#"nnar"
                selected = "ets",
                options = list(
                    title = "Select one model:",
                    `style` = "btn-warning")
            ),
            pickerInput(inputId = 'input_fit_topic',
                        label = 'Select topic:',
                        #choices = c(colnames(data3[,-c(1:3)])),
                        choices = c("Cases", "Deaths"),
                        selected = "Cases",
                        options = list(`style` = "btn-primary")),
            style = "unite", icon = icon("gear"),
            status = "danger", width = "300px",
            tooltip = tooltipOptions(title = "Click to see inputs!"),
            animate = animateOptions(
                enter = animations$fading_entrances$fadeInLeftBig,
                exit = animations$fading_exits$fadeOutRightBig
            )
        )
        #}
    })
    
    validation_data <- reactive({
        if (is.null(input$countries_box)) {
            mydata <- as.data.frame(data3)
            reactive_data <- as.data.frame(mydata %>% filter(STATE == input$countries_box_home)) #input$countries_box
            reactive_data <- reactive_data[, colSums(reactive_data != 0) > 0]
            reactive_data[sapply(reactive_data, function(x) length(unique(na.omit(x)))) > 1]
            test <- reactive_data    #() %>% filter(DATE >= input$date_input[1] & DATE <=input$date_input[2])
            reactiveDf <- test
            data <- reactiveDf[,-1] #()
            data <- data[,-1]########edit#############
            data <- data[,sapply(data, function(v) var(v, na.rm=TRUE)!=0)]
            my_vars <- colnames(data[,-c(1:2)])
            data <- as.data.frame(cbind(data[,c(1:2)],zoo::rollmean(data[,-c(1,2)], 1, fill = "extend")))# input$ma_selection
            data2 <- data[,-c(1:2)]
            colnames(data2) <- my_vars #
            data <- as.data.frame(cbind(data[,c(1:2)],data2)) #
            data <- as.data.frame(data)
            data <- data[complete.cases(data[,]),] #
            my_ts_def <- data
            C <- colnames(my_ts_def)
            nums <- nrow(my_ts_def)
            prova <- my_ts_def[c((nums-30):nums),]
            prova <- prova[, colSums(prova != 0) > 0]
            D <- colnames(prova)
            aa<-setdiff(C,D)
            my_ts_def<-my_ts_def[, !(colnames(my_ts_def) %in% aa), drop = FALSE]
        } else {
            my_ts_def <- my_ts_def()
        }
        my_ts_def <- my_ts_def
        nums <- nrow(my_ts_def)
        
        C <- colnames(my_ts_def)
        prova <- my_ts_def[c((nums-input$validate_days-input$refine_prediction+1):(nums-input$validate_days)),]
        prova <- prova[, colSums(prova != 0) > 0]
        D <- colnames(prova)
        aa<-setdiff(C,D)
        
        if (length(aa) > 0){
            pca2 <- prcomp(my_ts_def[c((nums-input$validate_days-input$refine_prediction-30+1):(nums-input$validate_days)),-c(1:2)], scale=TRUE, center=TRUE, rank.=2)#
            my_new_ts <- my_ts_def[c((nums-input$validate_days-input$refine_prediction-30+1):(nums-input$validate_days)),]#
            scores <- pca2$x
        } else {
            pca2 <- prcomp(my_ts_def[c((nums-input$validate_days-input$refine_prediction+1):(nums-input$validate_days)),-c(1:2)], scale=TRUE, center=TRUE, rank.=2)#
            my_new_ts <- my_ts_def[c((nums-input$validate_days-input$refine_prediction+1):(nums-input$validate_days)),]#
            scores <- pca2$x
        }
        
        pred_data <- as_tibble(scores)
        startDate <- as.Date(min(my_new_ts$DATE))
        endDate <- as.Date(max(my_new_ts$DATE))
        days <- seq(startDate, endDate, "1 day")
        pred_data <- cbind(days,pred_data)
        pred_data <- as_tsibble(pred_data)
        
        fit <- pred_data %>%
            model(
                ets = ETS(PC1),
                arima = fable::ARIMA(PC1)
            )
        
        num_days <- input$validate_days
        h_pred <- as.character(paste(num_days,"days"))
        
        fc <- fit %>%
            forecast(h = h_pred)
        
        fit2 <- pred_data %>%
            model(
                ets = ETS(PC2), 
                arima = fable::ARIMA(PC2)
            )
        
        fc2 <- fit2 %>%
            forecast(h = h_pred)
        
        days_etsPC1 <- fc %>% 
            as.data.frame(fc)%>%
            filter(.model == "ets")%>%
            select(days, .mean)
        
        etsPC2 <- fc2 %>% 
            as.data.frame(fc2)%>%
            filter(.model == "ets")%>%
            select(.mean)
        
        ets_means <- cbind(days_etsPC1,etsPC2)       
        colnames(ets_means) <- c("Date","PC1","PC2")
        
        days_arimaPC1 <- fc %>% 
            as.data.frame(fc)%>%
            filter(.model == "arima")%>%
            select(days, .mean)
        arimaPC2 <- fc2 %>% 
            as.data.frame(fc2)%>%
            filter(.model == "arima")%>%
            select(.mean)
        arima_means <- cbind(days_arimaPC1,arimaPC2)       
        colnames(arima_means) <- c("Date","PC1","PC2")
        
        arima_pred <- as.data.frame(t(t(as.matrix(arima_means[,c(2,3)]) %*% t(pca2$rotation)) * pca2$scale + pca2$center))
        ets_pred <- as.data.frame(t(t(as.matrix(ets_means[,c(2,3)]) %*% t(pca2$rotation)) * pca2$scale + pca2$center))
        model <- c(rep("arima",nrow(arima_pred)),rep("ets",nrow(ets_pred)))
        Date2 <- c(arima_means$Date,ets_means$Date)
        arima_pred <- cbind(arima_pred, arima_means[,c(2,3)])
        ets_pred <- cbind(ets_pred, ets_means[,c(2,3)])
        reversed_prediction <- rbind(arima_pred, ets_pred)
        reversed_prediction <- cbind(Date2, model, reversed_prediction)
        colnames(reversed_prediction) <- c("DATE", "model", colnames(arima_pred))
        
        list(reversed_prediction = reversed_prediction, nums = nums)
    })
    
    output$pc1_fit <- renderUI({
        # if (is.null(input$countries_box)) {
        #     tags$div(
        #         tags$h2(style="color:red", "Please, go to the 'Select a country' tab first")
        #     )
        # } else {
        reversed_prediction <- validation_data()$reversed_prediction
        data.table::setDT(reversed_prediction)[Cases < 0, Cases := 0]
        data.table::setDT(reversed_prediction)[Deaths < 0, Deaths := 0]
        
        nums <- validation_data()$nums
        
        if (is.null(input$countries_box)) {
            mydata <- as.data.frame(data3)
            reactive_data <- as.data.frame(mydata %>% filter(STATE == input$countries_box_home)) #input$countries_box
            reactive_data <- reactive_data[, colSums(reactive_data != 0) > 0]
            reactive_data[sapply(reactive_data, function(x) length(unique(na.omit(x)))) > 1]
            test <- reactive_data    #() %>% filter(DATE >= input$date_input[1] & DATE <=input$date_input[2])
            reactiveDf <- test
            data <- reactiveDf[,-1] #()
            data <- data[,-1]########edit#############
            data <- data[,sapply(data, function(v) var(v, na.rm=TRUE)!=0)]
            my_vars <- colnames(data[,-c(1:2)])
            data <- as.data.frame(cbind(data[,c(1:2)],zoo::rollmean(data[,-c(1,2)], 1, fill = "extend")))# input$ma_selection
            data2 <- data[,-c(1:2)]
            colnames(data2) <- my_vars #
            data <- as.data.frame(cbind(data[,c(1:2)],data2)) #
            data <- as.data.frame(data)
            data <- data[complete.cases(data[,]),] #
            my_ts_def <- data
            C <- colnames(my_ts_def)
            nums <- nrow(my_ts_def)
            prova <- my_ts_def[c((nums-30):nums),]
            prova <- prova[, colSums(prova != 0) > 0]
            D <- colnames(prova)
            aa<-setdiff(C,D)
            my_ts_def<-my_ts_def[, !(colnames(my_ts_def) %in% aa), drop = FALSE]
            my_ts_def<-my_ts_def[,-1]
        } else {
            my_ts_def <- reactiveDf()[,-1]
        }
        
        data <- my_ts_def#[,-c(1,2)] #reactiveDf()[,-1]
        data <- data[,!(colnames(data) %in% as.character(input$remove_topics))]
        data <- data[,sapply(data, function(v) var(v, na.rm=TRUE)!=0)]
        my_ts_def <- data
        
        measured_def <- my_ts_def[c((nums-input$validate_days+1):(nums)),]#
        predicted_def <- reversed_prediction %>% filter(model == input$select_model_validation) #
        topic <- input$input_fit_topic#
        rmsep = round(Metrics::rmse(measured_def[[topic]], predicted_def[[topic]]),2)
        
        fit <- lm(predicted_def[[topic]] ~ measured_def[[topic]])
        r2<-round(summary(fit)$r.squared,2)
        intercept <- round(fit[["coefficients"]][[1]],2)
        slope <- round(fit[["coefficients"]][[2]],2)
        fv <- lm(predicted_def[[topic]] ~ measured_def[[topic]]) %>% fitted.values()
        
        
        if (input$select_graph == "scatter") {
            p <- plot_ly(x = measured_def[[topic]], y = predicted_def[[topic]], 
                         type = "scatter", mode = "markers",
                         name = paste0("Measured vs Predicted (", topic,")"), 
                         hoverinfo = 'text',
                         text = ~paste('</br> Date: ', predicted_def$Date,
                                       '</br> Measured: ', round(measured_def[[topic]],2),
                                       '</br> Predicted: ', round(predicted_def[[topic]],2))) %>% 
                add_trace(x = measured_def[[topic]], y = fv, mode = "lines", 
                          name = paste0("Regression line (", topic, ")"), showlegend = T,
                          line = list(color= "red", dash = "dot")#,
                          #hoverinfo = 'text',
                          #text = ~paste('</br> Fitted: ', round(fv,2))
                ) %>%
                add_annotations(x= 0.9, y= 0.1, xref = "paper", yref = "paper",
                                text = sprintf("<b>%s</b>",paste0("R<sup>2</sup> = ",r2,"<br>f(x) = (",slope,")*x + ","(",intercept,")",
                                                                  "<br>RMSEP = ",rmsep)),
                                showarrow = F, font=list(size=15))%>% 
                layout(dragmode = "pan", showlegend = T, xaxis = list(title = paste0("Measured (", topic,")"), showgrid = F, zeroline = TRUE, showline = TRUE),
                       yaxis = list(title = paste0("Predicted (", topic,")"), showgrid = F, zeroline = TRUE, showline = TRUE),
                       legend = list(xanchor = "center", y = -0.2, x = 0.5, orientation = 'h'))
        } else {
            Date <- predicted_def$DATE
            Measured <- measured_def[[topic]]
            Predicted <- predicted_def[[topic]]
            mydata <- cbind(Predicted, Measured)
            mydata <- as.data.frame(mydata)
            mydata <- cbind(Date,mydata)
            vals <- length(Date)
            
            xax <- as.Date(Date)
            p <- plot_ly()#%>%
            ToAdd <- c("Predicted", "Measured")
            for(i in ToAdd){
                p <- p %>% add_trace(x = xax, y = mydata[[i]], name = i,
                                     type = 'scatter',
                                     mode = 'line+markers',
                                     line = list(color = i, width = 4))
            }
            p <- p %>%
                layout(dragmode = "pan",
                       yaxis = list (title = paste("Measured vs Predicted"),showgrid = F, showline = TRUE),
                       xaxis = list (title = paste("Prediction dates"),type = 'date',showgrid = F, showline = TRUE,
                                     tickformat = "%d/%m/%Y", nticks = vals, tickmode = "array", tickangle = 45), 
                       legend = list(xanchor = "center", y = -0.3, x = 0.5, orientation = 'h'))%>%
                add_annotations(x= 0.9, y= 0.9, xref = "paper", yref = "paper",showarrow = F,
                                text = sprintf("<b>%s</b>",paste0("RMSEP = ",rmsep)))
            
        }  
        
        box(title = paste("Fitting Time Series over", input$input_fit_topic, "for", input$select_model_validation, "model"), 
            width = 12, status = "primary", solidHeader = TRUE,align = "center",
            style = "overflow-x: scroll",
            shiny::flowLayout(
                cellArgs = list(
                    style = "min-width: 790px;
                     width: auto;
                     height: auto;
                     border: 1px solid white;
                     padding: 10px;
                     margin: 10px;"),
                renderPlotly(p))
        )
        #}
    })
    
    results_validation <- reactive({
        reversed_prediction <- validation_data()$reversed_prediction
        
        data.table::setDT(reversed_prediction)[Cases < 0, Cases := 0]
        data.table::setDT(reversed_prediction)[Deaths < 0, Deaths := 0]
        
        nums <- validation_data()$nums
        
        if (is.null(input$countries_box)) {
            mydata <- as.data.frame(data3)
            reactive_data <- as.data.frame(mydata %>% filter(STATE == input$countries_box_home)) #input$countries_box
            reactive_data <- reactive_data[, colSums(reactive_data != 0) > 0]
            reactive_data[sapply(reactive_data, function(x) length(unique(na.omit(x)))) > 1]
            test <- reactive_data    #() %>% filter(DATE >= input$date_input[1] & DATE <=input$date_input[2])
            reactiveDf <- test
            data <- reactiveDf[,-1] #()
            data <- data[,-1]########edit#############
            data <- data[,sapply(data, function(v) var(v, na.rm=TRUE)!=0)]
            my_vars <- colnames(data[,-c(1:2)])
            data <- as.data.frame(cbind(data[,c(1:2)],zoo::rollmean(data[,-c(1,2)], 1, fill = "extend")))# input$ma_selection
            data2 <- data[,-c(1:2)]
            colnames(data2) <- my_vars #
            data <- as.data.frame(cbind(data[,c(1:2)],data2)) #
            data <- as.data.frame(data)
            data <- data[complete.cases(data[,]),] #
            my_ts_def <- data
            C <- colnames(my_ts_def)
            nums <- nrow(my_ts_def)
            prova <- my_ts_def[c((nums-30):nums),]
            prova <- prova[, colSums(prova != 0) > 0]
            D <- colnames(prova)
            aa<-setdiff(C,D)
            my_ts_def<-my_ts_def[, !(colnames(my_ts_def) %in% aa), drop = FALSE]
        } else {
            my_ts_def <- my_ts_def()
        }
        
        data <- my_ts_def[,-c(1,2)] #reactiveDf()[,-1]
        
        #data <- reactiveDf()[,-1]
        data <- data[,!(colnames(data) %in% as.character(input$remove_topics))]
        data <- data[,sapply(data, function(v) var(v, na.rm=TRUE)!=0)]
        my_ts_def <- data
        
        measured_def <- my_ts_def[c((nums-input$validate_days+1):(nums)),]#
        predicted_def <- reversed_prediction %>% filter(model == input$select_model_validation) #
        topic <- input$input_fit_topic#
        rmsep = round(Metrics::rmse(measured_def[[topic]], predicted_def[[topic]]),2)
        
        fit <- lm(predicted_def[[topic]] ~ measured_def[[topic]])
        r2<-round(summary(fit)$r.squared,2)
        intercept <- round(fit[["coefficients"]][[1]],2)
        slope <- round(fit[["coefficients"]][[2]],2)
        fv <- lm(predicted_def[[topic]] ~ measured_def[[topic]]) %>% fitted.values()

        pred_res <- predicted_def[[topic]]
        meas_res <- measured_def[[topic]]
        predicted <- predicted_def

        list(pred_res = pred_res, meas_res = meas_res, predicted = predicted)
    })
    
    output$view_results <- renderUI({
        # if (is.null(input$input$countries_box)) {
        #     return(NULL)
        # } else {
        actionBttn(inputId = "view_results", label = "View results", color = "success", style = "material-flat",
                   size = "md", icon = icon("sliders"), block = FALSE )
        #}
    })

    observeEvent(input$view_results, {
        pred_res <- as.data.frame(round(results_validation()$pred_res,2))
        meas_res <- as.data.frame(round(results_validation()$meas_res,2))
        predicted <- as.data.frame(results_validation()$predicted)
        results <- as.data.frame(cbind(predicted$DATE, meas_res, pred_res))
        Difference <- round((meas_res - pred_res),2)
        results$Difference <- Difference
        greeks=c(Delta='\u0394')
        colnames(results) <- c("Dates", "Measured", "Predicted", greeks['Delta'])
        showModal(modalDialog(
            title = paste("Differences of", input$input_fit_pc),
            DT::renderDataTable(results,
                                rownames= FALSE)
        ))
    })
    
    ts_validation <- reactive({
        if (is.null(input$countries_box)) {
            mydata <- as.data.frame(data3)
            reactive_data <- as.data.frame(mydata %>% filter(STATE == input$countries_box_home)) #input$countries_box
            reactive_data <- reactive_data[, colSums(reactive_data != 0) > 0]
            reactive_data[sapply(reactive_data, function(x) length(unique(na.omit(x)))) > 1]
            test <- reactive_data    #() %>% filter(DATE >= input$date_input[1] & DATE <=input$date_input[2])
            reactiveDf <- test
            data <- reactiveDf[,-1] #()
            data <- data[,-1]########edit#############
            data <- data[,sapply(data, function(v) var(v, na.rm=TRUE)!=0)]
            my_vars <- colnames(data[,-c(1:2)])
            data <- as.data.frame(cbind(data[,c(1:2)],zoo::rollmean(data[,-c(1,2)], 1, fill = "extend")))# input$ma_selection
            data2 <- data[,-c(1:2)]
            colnames(data2) <- my_vars #
            data <- as.data.frame(cbind(data[,c(1:2)],data2)) #
            data <- as.data.frame(data)
            data <- data[complete.cases(data[,]),] #
            my_ts_def <- data
            C <- colnames(my_ts_def)
            nums <- nrow(my_ts_def)
            prova <- my_ts_def[c((nums-30):nums),]
            prova <- prova[, colSums(prova != 0) > 0]
            D <- colnames(prova)
            aa<-setdiff(C,D)
            my_ts_def<-my_ts_def[, !(colnames(my_ts_def) %in% aa), drop = FALSE]
        } else {
            my_ts_def <- my_ts_def()
        }
        nums <- nrow(my_ts_def)
        pca2 <- prcomp(my_ts_def[c((nums-input$validate_days-input$refine_prediction+1):(nums)),-c(1:2)], scale=TRUE, center=TRUE, rank.=2)#
        my_new_ts <- my_ts_def[c((nums-input$validate_days-input$refine_prediction+1):(nums)),]#
        scores <- as.data.frame(pca2$x)
        scores<-cbind(my_new_ts$DATE,scores)
        colnames(scores) <- c("Date", "PC1", "PC2")
        scores$Date <- as.Date(scores$Date)
        scores <- as_tsibble(scores)
        
        pred_data <- scores
        
        fit <- pred_data %>%
            model(
                ets = ETS(PC1),
                arima = fable::ARIMA(PC1)
            )
        
        test <- scores %>%
            filter(Date > (max(scores$Date)-input$validate_days+1) & Date <= max(scores$Date))#
        fc <- fit %>% forecast(test)
        fca <- fc
        
        results <- forecast::accuracy(fca, scores)
        fc <- as.data.frame(fc)
        colnames(fc) <- c(".model","Date","model","PC1","PC2")
        
        fit2 <- pred_data %>%
            model(
                ets = ETS(PC2),
                arima = fable::ARIMA(PC2)
            )
        test2 <- scores %>%
            filter(Date > (max(scores$Date)-input$validate_days+1) & Date <= max(scores$Date))#
        fc2 <- fit2 %>% forecast(test2)
        fca2 <- fc2
        results2 <- forecast::accuracy(fca2, scores)
        fc2 <- as.data.frame(fc2)
        colnames(fc2) <- c(".model","Date","model","PC2","PC1")
        
        list(results = results, fc = fc, scores = scores, results2 = results2, fc2 = fc2)
    })
    
    output$accuracy_table <- renderUI({
        # if (is.null(input$countries_box)) {
        #     return(NULL)
        # } else {
        results <- ts_validation()$results
        results <- as.data.frame(results)
        row.names(results) <- results[,1]
        results <- results[,-c(1:2)]
        results <- round(results,3)
        colors <- apply(col2rgb(rainbow(n=ncol(results))),2,function(x)paste0("rgb(",paste(x,collapse=","),")"))
        data <- datatable(results)
        sapply(c(1:ncol(results)),function(x){
            data <<- data %>% formatStyle(colnames(results)[[x]],backgroundColor = styleEqual(max(results[[x]]), colors[3]))#
        })
        
        box(title = "Performance Time Series PC1", 
            width = 12, status = "success", solidHeader = TRUE, align="center" , collapsible = TRUE, collapsed = TRUE,
            style = "overflow-x: scroll",
            shiny::flowLayout(
                cellArgs = list(
                    style = "min-width: 600px;
                     width: auto;
                     height: auto;
                     border: 1px solid white;
                     padding: 10px;
                     margin: 10px;"),
                renderDT(data)
            ))
            #         DT::datatable( data = results, 
            #                        extensions = 'Buttons', 
            #                        options = list(dom = "Blfrtip", 
            #                                       scrollX = TRUE,
            #                                       buttons = 
            #                                           list("copy", list(
            #                                               extend = "collection"
            #                                               , buttons = c("csv", "excel", "pdf")
            #                                               , text = "Download"
            #                                           )),
            #                                       lengthMenu = list(c(10, 20, 50, -1), 
            #                                                         c(10, 20, 50, "All") # declare titles
            #                                       ), # end of lengthMenu customization
            #                                       pageLength = 10
            #                        ) # end of options
            #         ) # end of datatables)
            #     )
            # ))
        #}
    })
    
    output$accuracy_table2 <- renderUI({
        # if (is.null(input$countries_box)) {
        #     return(NULL)
        # } else {
        results <- ts_validation()$results2
        results <- as.data.frame(results)
        row.names(results) <- results[,1]
        results <- results[,-c(1:2)]
        results <- round(results,3)
        colors <- apply(col2rgb(rainbow(n=ncol(results))),2,function(x)paste0("rgb(",paste(x,collapse=","),")"))
        data <- datatable(results)
        sapply(c(1:ncol(results)),function(x){
            data <<- data %>% formatStyle(colnames(results)[[x]],backgroundColor = styleEqual(max(results[[x]]), colors[2]))
        })
        
        box(title = "Performance Time Series PC2", 
            width = 12, status = "warning", solidHeader = TRUE, align="center", collapsible = TRUE, collapsed = TRUE,
            style = "overflow-x: scroll",
            shiny::flowLayout(
                cellArgs = list(
                    style = "min-width: 600px;
                     width: auto;
                     height: auto;
                     border: 1px solid white;
                     padding: 10px;
                     margin: 10px;"),
                renderDT(data)
        #             DT::datatable( data = results, 
        #                            extensions = 'Buttons', 
        #                            options = list(dom = "Blfrtip", 
        #                                           scrollX = TRUE,
        #                                           buttons = 
        #                                               list("copy", list(
        #                                                   extend = "collection"
        #                                                   , buttons = c("csv", "excel", "pdf")
        #                                                   , text = "Download"
        #                                               )),
        #                                           lengthMenu = list(c(10, 20, 50, -1), 
        #                                                             c(10, 20, 50, "All") # declare titles
        #                                           ), # end of lengthMenu customization
        #                                           pageLength = 10
        #                            ) # end of options
        #             ) # end of datatables)
        #         )
        # ))
            ))
        #}
    })
    
    # About ####
    output$last_update <- renderUI({
        div(
            tags$strong(paste("Last data update:"), style = "display: inline; font-size: 25px; 
                           font-style: bold;"),
            h3(paste(max(data3$DATE)), style = "display: inline; font-size: 25px;")
        )
    })
    
    
} # end server
    
    