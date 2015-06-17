shinyUI(navbarPage("Rainfall in Colorado",
                   tabPanel("Analysis",
                            sidebarLayout(
                              sidebarPanel(
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
                                            min = 90, max = 100, value = 95, step = 0.5, animate=TRUE),
                                helpText("Note: this is the quantile at which the threshold is set. ")
                              ),
                              mainPanel(
                                plotOutput("plots1"),
                                br(),
                                plotOutput("plots2")
                              )
                            )
                   )
))