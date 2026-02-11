
#' R/utils-UI.R

#' shiny user interface

.shiny_user_interface_pango <- function(){

  ui <- fluidPage(titlePanel("Test"),
                  sidebarLayout(sidebarPanel(textInput(inputId = "Stock_Code",
                                                       label = "Stock Code",
                                                       value = "000001",
                                                       updateOn = "blur"),
                                             textInput(inputId = "Year",
                                                       label = "Year",
                                                       value = "2025",
                                                       updateOn = "blur")),
                                mainPanel(plotOutput("trend_graph"))))

  return(ui)

}
