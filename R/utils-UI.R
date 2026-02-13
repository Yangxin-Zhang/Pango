
#' R/utils-UI.R

#' shiny user interface

.shiny_user_interface_pango <- function(stock_db){

  ui <- fluidPage(titlePanel("Test"),
                  sidebarLayout(sidebarPanel(width = 3,
                                             textInput(inputId = "Stock_Code",
                                                       label = "Stock Code",
                                                       value = "000001",
                                                       updateOn = "blur"),
                                             textInput(inputId = "Year",
                                                       label = "Year",
                                                       value = "2025",
                                                       updateOn = "blur"),
                                             textInput(inputId = "Interval",
                                                       label = "Interval",
                                                       value = "1,200",
                                                       updateOn = "blur"),
                                             uiOutput("slider_location"),
                                             uiOutput("slider_width")),
                                mainPanel(width = 9,
                                          plotOutput("trend_graph",
                                                     height = "600px"))))

  return(ui)

}
