library(shinydashboard)
library(shinyWidgets)
library(shiny)
library(shinycustomloader)
library(DT)
library(dplyr)
library(plotly)
library(ggcorrplot)
library(ggplot2)
library(ggrepel)
library(ggthemes)
library(tidyr)
library(fable)
library(kableExtra)
library(stringr)
library(tidyverse)
library(tsibble)
library(lubridate)
library(streamgraph)
library(forecast)
library(zoo)
library(htmltools)
library(Metrics)
library(data.table)
#library(feasts)


ui <- dashboardPage(
    dashboardHeader(title = "PredictPandemic.org"), #header
    dashboardSidebar(
        sidebarMenu( id = "sidebarid",
                     menuItem("Home", tabName = "home", icon = icon("home")),
                     menuItem("Tutorial", tabName = "tutorial", icon = icon("youtube")),
                     menuItem("Select a country", tabName = "specific_country", icon = icon("flag")),
                     conditionalPanel(
                         'input.sidebarid == "specific_country"',
                         br(),
                         #        uiOutput("data_select_ts"),
                         #        uiOutput("data_select_ts_y2"),
                         #        uiOutput("ma_selection"))#,
                         uiOutput("data_select_ts"),
                         uiOutput("data_select_ts_y2"),
                         uiOutput("ma_selection")
                     ),
                     menuItem("View the data", tabName = "time_series", icon = icon("eye"),startExpanded = FALSE,
                              # menuSubItem("Specific country",
                              #             tabName = "specific_country", icon = icon("flag")),
                              menuSubItem("Streamgraph",
                                          tabName = "streamgraph_tab", icon = icon("area-chart")),         
                              menuSubItem("Whole world",
                                          tabName = "whole_world", icon = icon("globe"))),
                     menuItem("Build the models", tabName = "models", icon = icon("gears"),
                              menuSubItem("Correlation",
                                          tabName = "correlation", icon = icon("arrows-alt")),
                              menuSubItem("PCA model",
                                          tabName = "pca_analysis", icon = icon("wrench"))),
                     menuItem("Predict future data", tabName = "predict_ts", icon = icon("magic")),
                     conditionalPanel(
                         'input.sidebarid == "predict_ts"',
                         br(),
                         numericInputIcon(
                             inputId = "refine_prediction",
                             label = "Refine days (last) for prediction:",
                             size = "sm",
                             value = 30,
                             icon = list("How many days?")
                         ),
                         br(),
                         awesomeCheckbox(
                             inputId = "view_confint",
                             label = "Confidence intervals (95%)?", 
                             value = TRUE,
                             status = "danger"
                         ),
                         br(),
                         #uiOutput("conf_int"),
                         #br(),
                         uiOutput("parameter_prediction_new_plot")
                         
                     ),
                     menuItem("Validate the models", tabName = "validation", icon = icon("search")),
                     conditionalPanel(
                         'input.sidebarid == "validation"',
                         br(),
                         numericInputIcon(
                             inputId = "validate_days",
                             label = "Validate prediction:",
                             size = "sm",
                             value = 7,
                             icon = list("How many days?")
                         ),
                         br()
                     ),# conditionalPanel "validation"
                     menuItem("About", tabName = "about", icon = icon("info"))
        )# sidebarMenu
    ), # dashboardSidebar
    dashboardBody(
        tags$head(tags$style(HTML("
                                #final_text {
                                   text-align: center;
                                }
                                div.box-header {
                                   text-align: center;
                                }
                                .content-wrapper {
                                   overflow: auto;
                                }
                                .box-header h3 {
                                   font-weight: bold;
                                }
                                "))),
        tabItems(
            tabItem(tabName = "home",
                    br(),
                    fluidRow(
                        column(12,offset=0,
                               div(style = "font-size: 10px; padding: 14px 0px; margin-top:-4em; ",
                                   fluidRow(align = "center",
                                            h2(strong("PredictPandemic.org"), "evaluates", 
                                               em("Google Trends"), "and", em("conventional COVID-19 metrics"), "via an interactive framework")
                                   )
                               )#.,
                               # div(style = "font-size: 10px; padding: 0px 0px; margin-top:-4em", 
                               #     fluidRow(align = "center",
                               #              h2("involving", strong("time series"), 
                               #                 "and", strong("Principal Component Analysis (PCA)"))
                               #     )                                      
                               # )
                        )
                    ),
                    br(),
                    fluidRow(
                        column(width = 3,),
                        column(width = 6, uiOutput("countries_box_home")),
                        column(width = 2,),
                        column(1, uiOutput("flag_home"))
                    ),
                    br(),
                    fluidRow(
                        column(width = 12, 
                               withLoader(uiOutput("plot_home"), type = "html", loader="loader3"))
                    )#,
                    # fluidRow(column = 12,
                    #     align = "center",
                    #     h2(strong("Do you want to see more? Visit our website!"))
                    # )
            ), #tabItem = "home"
            tabItem(tabName = "tutorial",
                    br(),
                    br(),
                    fluidRow(
                        column(2, ),
                        column(8,tags$iframe(width="1120", height="630", src="https://www.youtube.com/embed/SPvcL0JK4QA", frameborder="0", allow="accelerometer; autoplay; encrypted-media; gyroscope; picture-in-picture", allowfullscreen=NA)),
                        column(2,)
                    )
            ), #tabItem = "tutorial"
            tabItem(tabName = "specific_country",    
                    br(),
                    fluidRow(
                        column(12,offset=0,
                               div(style = "font-size: 10px; padding: 14px 0px; margin-top:-4em; ",
                                   fluidRow(align = "center",
                                            h2(strong("Selection:"), "Prime the app"))))
                    ),
                    br(),
                    fluidRow(
                        column(width = 6,uiOutput("countries_box")),
                        column(width = 6, uiOutput("date_input"),
                               chooseSliderSkin("Flat", color = "red"))
                    ),
                    br(),
                    fluidRow(
                        uiOutput('tbl')
                    ),
                    br(),#,
                    fluidRow(
                        column(width = 12, 
                               withLoader(uiOutput("plot_ts"), type = "html", loader="loader3")),
                        # column(width = 3, 
                        #        uiOutput("data_select_ts"),
                        #        uiOutput("data_select_ts_y2"),
                        #        uiOutput("ma_selection"))#,
                    )#,
                    #br()
                    #  )#fluidPage
            ), #tabItem = "specific_country"
            tabItem(tabName = "streamgraph_tab",
                    br(),
                    fluidRow(
                        column(12,offset=0,
                               div(style = "font-size: 10px; padding: 14px 0px; margin-top:-4em; ",
                                   fluidRow(align = "center",
                                            h2(strong("Streamgraphs:"), "See how trends flow over time"))))
                    ),
                    br(),
                    fluidRow(
                        column(width = 6,uiOutput("stream_country_box")),
                        column(width = 6,uiOutput("stream_ma"),
                               chooseSliderSkin("Flat", color = "red"))
                    ),
                    br(),
                    fluidRow(
                        column(width = 12,
                               withLoader(uiOutput("plot_stream_1"), type = "html", loader="loader3"))
                    ),
                    fluidRow(
                        column(width = 1, withLoader(uiOutput("view_stream_2"), type = "html", loader="loader3"))
                    ),
                    fluidRow(
                        column(width = 1, withLoader(uiOutput("button_stream_2"), type = "html", loader="loader3"))
                    ),
                    fluidRow(
                        column(width = 12,
                               withLoader(uiOutput("plot_stream_2"), type = "html", loader="loader3"))
                    )
            ), #tabItem = "streamgraph_tab"
            tabItem(tabName = "whole_world",
                    br(),
                    fluidRow(
                        column(12,offset=0,
                               div(style = "font-size: 10px; padding: 14px 0px; margin-top:-4em; ",
                                   fluidRow(align = "center",
                                            h2(strong("World map:"), "View worldwide trends over time"))))
                    ),
                    br(),
                    fluidRow(
                        #column(width = 4, ),
                        #uiOutput("slider_date_input"),
                        column(width = 12, uiOutput("select_world"))#,
                        #column(width = 4, )
                    ),
                    br(),
                    fluidRow(
                        withLoader(uiOutput("world_image"),type = "text",
                                   loader=list(marquee("Preparing the map...", behavior = "alternate", 
                                                       scrollamount = 5)))
                    )
            ), #tabItem = "specific_country"
            tabItem(tabName = "correlation",
                    br(),
                    fluidRow(
                        column(11,offset=0,
                               div(style = "font-size: 10px; padding: 14px 0px; margin-top:-4em; ",
                                   fluidRow(align = "center",
                                            h2(strong("Correlations:"), "Explore relationships among data")))),
                        column(1, uiOutput("flag"))
                    ),
                    br(),
                    fluidRow(
                        column(1, uiOutput("dropdown_ccf")),
                        column(6,)
                    ),
                    br(),
                    fluidRow(
                        column(6, uiOutput("ccf_plot")),
                        column(6, uiOutput("ccf_table"))
                    ),
                    br(),
                    fluidRow(
                        column(4, ),
                        column(width = 4, 
                               uiOutput("remove_topics")),
                        # column(width = 7, 
                        #        uiOutput('tbl2'),
                        #        uiOutput("corr_plot"),
                        #        uiOutput("corr_table")),
                        column(4, )
                    )
            ), #tabItem = "correlation"
            tabItem(tabName = "pca_analysis",
                    br(),
                    fluidRow(
                        column(11,offset=0,
                               div(style = "font-size: 10px; padding: 14px 0px; margin-top:-4em; ",
                                   fluidRow(align = "center",
                                            h2(strong("PCA model:"), "Understand how data are analyzed")))),
                        column(1, uiOutput("flag2"))
                    ),
                    br(),
                    fluidRow(
                        column(1, uiOutput("dropdown_pca_scores")),
                        column(5,),
                        column(1,uiOutput("dropdown_pca_loadings"))
                    ),
                    fluidRow(
                        column(6, uiOutput("pca_scores")),
                        column(6, uiOutput("pca_loadings")),
                    ),
                    br(),
                    fluidRow(
                        column(6,),
                        column(1,uiOutput("dropdown_pca_ts"))
                    ),
                    fluidRow(
                        column(6, uiOutput("pca_scree_plot")),
                        column(6, uiOutput("pca_ts"))
                    ),
                    br()
            ), #tabItem = "pca_analysis"
            tabItem(tabName = "predict_ts",
                    br(),
                    fluidRow(
                        column(11,offset=0,
                               div(style = "font-size: 10px; padding: 14px 0px; margin-top:-4em; ",
                                   fluidRow(align = "center",
                                            h2(strong("Predictions:"), "View how COVID-19 is expected to change")))),
                        column(1, uiOutput("flag3"))
                    ),
                    #fluidRow(uiOutput("check_prediction")),
                    br(),
                    fluidRow(
                        #column(1,uiOutput("dropdown_prediction_plot"))
                        column(3, ),
                        column(6, uiOutput("predict_days")), #refine_prediction
                        column(3, )#,
                        #column(1, uiOutput("flag3"))#,
                    ),
                    fluidRow(
                        #column(6, withLoader(uiOutput("prediction_plot"), type = "html", loader="loader6")),
                        column(12, withLoader(uiOutput("new_prediction_plot"), type = "html", loader="loader6")),
                        column(1,uiOutput("select_model_prediction"))
                    ),
                    br(),
                    fluidRow(
                        #column(6, withLoader(uiOutput("prediction_plot"), type = "html", loader="loader6")),
                        column(12, withLoader(uiOutput("predict_pca_scores_plot"),type = "text",
                                             loader=list(marquee("Calculating predicted PCA scores", behavior = "alternate", 
                                                                 scrollamount = 5))))
                    )
            ), #tabItem = "predict_ts"
            tabItem(tabName = "validation",
                    br(),
                    fluidRow(
                        column(11,offset=0,
                               div(style = "font-size: 10px; padding: 14px 0px; margin-top:-4em; ",
                                   fluidRow(align = "center",
                                            h2(strong("Validation:"), "Test how accurate are predictions")))),
                        column(1, uiOutput("flag4"))
                    ),
                    br(),
                    fluidRow(
                        column(1, ),
                        column(1, uiOutput("dropdown_fit")),
                        column(7, withLoader(uiOutput("pc1_fit"),type = "html", loader="loader2")),
                        column(2, uiOutput("view_results")),
                        column(1, )#,
                        #column(1, uiOutput("flag4"))
                    ),
                    br(),
                    fluidRow(
                        column(6, withLoader(uiOutput("accuracy_table"),type = "text",
                                             loader=list(marquee("Calculating the figures of merit", behavior = "alternate", 
                                                                 scrollamount = 5)))),
                        column(6, withLoader(uiOutput("accuracy_table2"),type = "text",
                                             loader=list(marquee("Calculating the figures of merit", behavior = "alternate", 
                                                                 scrollamount = 5))))
                    )
            ),#, #tabItem = "validation"
            tabItem(tabName = "about",
                    h3(strong("Authors of the app:")),
                    fluidRow(
                        column(3,
                               HTML('<center><img src="alessandro-marchese.jpeg" height = "200" width = "200"></center>'),
                               h3(strong(a("Alessandro Marchese", 
                                    href = "https://www.linkedin.com/in/alessandro-marchese-b37358124/")),align = "center")),
                       column(3,
                              HTML('<center><img src="alessandro-rabiolo.jpeg" height = "200" width = "200"></center>'),
                              h3(strong(a("Alessandro Rabiolo", 
                                          href = "https://www.linkedin.com/in/alessandro-rabiolo-48390a141/")),align = "center")),
                       column(3,
                              HTML('<center><img src="esteban-morales.jpeg" height = "200" width = "200"></center>'),
                              h3(strong(a("Esteban Morales", 
                                          href = "https://www.linkedin.com/in/esjmorales/")),align = "center")),
                       column(3,
                              HTML('<center><img src="eugenio-alladio.jpeg" height = "200" width = "200"></center>'),
                              h3(strong(a("Eugenio Alladio", 
                                          href = "https://www.linkedin.com/in/eugenio-alladio-7173a9119/")),align = "center"))
                    ),
                    # h3(strong(a("Alessandro Marchese", 
                    #            href = "https://www.linkedin.com/in/alessandro-marchese-b37358124/"))),", ",
                    #          a("Alessandro Rabiolo", 
                    #            href = "https://www.linkedin.com/in/alessandro-rabiolo-48390a141/"),", ",
                    #          a("Esteban Morales", 
                    #            href = "https://www.linkedin.com/in/esjmorales/"),", ",
                    #          a("Eugenio Alladio", 
                    #            href = "https://www.linkedin.com/in/eugenio-alladio-7173a9119/"))),
                    # fluidRow(
                    #     img(src = "alessandro-marchese.jpeg", height = 200, width = 200),
                    #     img(src = "alessandro-rabiolo.jpeg", height = 200, width = 200),
                    #     img(src = "esteban-morales.jpeg", height = 200, width = 200),
                    #     img(src = "eugenio-alladio.jpeg", height = 200, width = 200)),
                    br(),
                    h3(strong("Funding:"), strong(a("PredictPandemic.org", href = "https://predictpandemic.org/")), "was supported by the", strong(a("EOSCsecretariat.eu", href = "https://eoscsecretariat.eu/")),", 
                       which has received funding from the European Union's Horizon Programme call H2020-INFRAEOSC-05-2018-2019, grant Agreement number 831644."),
                    br(),
                    h3(strong("Primary data source:"), strong(a("Google Trends", 
                               href = "https://www.google.com/trends"),", ",
                             a("COVID-19 Map - Johns Hopkins Coronavirus Resource Center", 
                               href = "https://github.com/CSSEGISandData/COVID-19"),".")),
                    br(),
                    h3(strong("Acknowledgement:"),"Abdelmonem Afifi, Francesco Bandello, 
                             Andrew McNaught, Alessandro Currao, Davide Corsico, Pietro Rabiolo, Sunil Mamtora."),
                    br(),
                    uiOutput("last_update"),
                    br(),
                    h3(strong("Powered by:"), "ARO"),
                    br(),
                    h3(strong("Hosted by:"), strong(a("ArticHost", 
                                                      href = "https://www.artichost.com"))),
                    br(),
                    h3(strong("Preprint:"), strong(a("Read our article", 
                                                      href = "https://www.medrxiv.org/content/10.1101/2021.03.09.21253186v1")))
                    # fluidRow(
                    #     img(src = "abdelmonem-afifi.jpeg", height = 200, width = 200),
                    #     img(src = "francesco-bandello.jpg", height = 200, width = 200),
                    #     img(src = "andrew-mcnaught.jpg", height = 200, width = 200))
            ) #tabItem = "about"
        ) #tabItems
    ) #dashboard body
)