shinyUI(navbarPage("Rainfall in Colorado",
                   
                   # Analysis Tab                   
                   tabPanel("Analysis", 
                            div(class="outer",
                                tags$head(
                                  # Include our custom CSS on this Panel
                                  includeCSS("www/styles.css")
                                ),
                                fluidPage(
                                  fluidRow(
                                    column(8,
                                           includeHTML("index.html"),
                                           plotOutput("plots3", height = "200px"),
                                           plotOutput("plots5", height = "200px"),
                                           plotOutput("plots2", height = "400px"),
                                           verbatimTextOutput("rl")
                                    )
                                  )
                                ),
                                tags$head(tags$style(
                                  type="text/css",
                                  "#mapImage img {max-width: 100%; width: 100%; height: 100%}"
                                )),
                                absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                                              draggable = FALSE, top = 80, left = "auto", right = 30, bottom = "auto",
                                              width = "30%", height = "auto",
                                              h2("Data input"),
                                              selectInput("station", 
                                                          label = "Choose a weather station:",
                                                          choices = c("Weather station 1", "Weather station 2", "Weather station 3", "Weather station 4", "Weather station 5", "Weather station 6", "Weather station 7", "Weather station 8", "Weather station 9", "Weather station 10",
                                                                      "Weather station 11", "Weather station 12", "Weather station 13", "Weather station 14", "Weather station 15", "Weather station 16", "Weather station 17", "Weather station 18", "Weather station 19", "Weather station 20",
                                                                      "Weather station 21", "Weather station 22", "Weather station 23", "Weather station 24", "Weather station 25", "Weather station 26", "Weather station 27", "Weather station 28", "Weather station 29", "Weather station 30",
                                                                      "Weather station 31", "Weather station 32", "Weather station 33", "Weather station 34", "Weather station 35", "Weather station 36", "Weather station 37", "Weather station 38", "Weather station 39", "Weather station 40",
                                                                      "Weather station 41", "Weather station 42", "Weather station 43", "Weather station 44", "Weather station 45", "Weather station 46", "Weather station 47", "Weather station 48", "Weather station 49", "Weather station 50",
                                                                      "Weather station 51", "Weather station 52", "Weather station 53", "Weather station 54", "Weather station 55", "Weather station 56"),
                                                          selected = "Weather station 1"),
                                              sliderInput("threshold", "Threshold:", 
                                                          min = 80, max = 100, value = 95),
                                              helpText("Note: this is the quantile at which the threshold is set. "),
                                              imageOutput("mapImage")
                                ),
                                tags$div(id="cite",
                                         'Summer project,', tags$em('by Wuji and Ryan.'))
                            )
                   ),
                   
                   
                   
                   #  Simulation Tab
                   
                   
                   tabPanel("Simulation",
                            div(class="outer",
                                tags$head(
                                  # Include our custom CSS on this Panel
                                  includeCSS("www/styles.css")
                                ),
                                fluidPage(
                                  fluidRow(
                                    column(8,
                                           h1("Simulation"),
                                           plotOutput("plots4", height = "300px"),
                                           actionButton("goButton", "Re-Run simulation", icon("random")),
                                           helpText("Generate a QQ plot of quantiles from model-simulated data against the data.")
                                    )
                                  )
                                ),
                                tags$head(tags$style(
                                  type="text/css",
                                  "#mapImage2 img {max-width: 100%; width: 100%; height: 100%}"
                                )),
                                absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                                              draggable = FALSE, top = 80, left = "auto", right = 30, bottom = "auto",
                                              width = "30%", height = "auto",
                                              h2("Data input"),
                                              selectInput("station2", 
                                                          label = "Choose a weather station:",
                                                          choices = c("Weather station 1", "Weather station 2", "Weather station 3", "Weather station 4", "Weather station 5", "Weather station 6", "Weather station 7", "Weather station 8", "Weather station 9", "Weather station 10",
                                                                      "Weather station 11", "Weather station 12", "Weather station 13", "Weather station 14", "Weather station 15", "Weather station 16", "Weather station 17", "Weather station 18", "Weather station 19", "Weather station 20",
                                                                      "Weather station 21", "Weather station 22", "Weather station 23", "Weather station 24", "Weather station 25", "Weather station 26", "Weather station 27", "Weather station 28", "Weather station 29", "Weather station 30",
                                                                      "Weather station 31", "Weather station 32", "Weather station 33", "Weather station 34", "Weather station 35", "Weather station 36", "Weather station 37", "Weather station 38", "Weather station 39", "Weather station 40",
                                                                      "Weather station 41", "Weather station 42", "Weather station 43", "Weather station 44", "Weather station 45", "Weather station 46", "Weather station 47", "Weather station 48", "Weather station 49", "Weather station 50",
                                                                      "Weather station 51", "Weather station 52", "Weather station 53", "Weather station 54", "Weather station 55", "Weather station 56"),
                                                          selected = "Weather station 1"),
                                              sliderInput("threshold2", "Threshold:", 
                                                          min = 80, max = 100, value = 95),
                                              helpText("Note: this is the quantile at which the threshold is set. "),
                                              imageOutput("mapImage2")
                                ),
                                tags$div(id="cite",
                                         'Summer project,', tags$em('by Wuji and Ryan.'))
                            )
                   )
))                   