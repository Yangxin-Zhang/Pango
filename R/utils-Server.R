
# R/utils-Server.R

#' shiny server

.shiny_server_pango <- function() {


  server <- function(input,output) {

    db_path <- "/home/youngxin/Desktop/Pango_R/Pango_Project/.DataBase/Stock_Database.db"

    stock_db <- Pango::Stock_Database_Pango(db_path = db_path)

    Years <- reactive({
      years <- strsplit(input$Year,split = ",")
      years <- years[[1]]
      return(as.numeric(years))
    })

    stock_blocks <- reactive({Pango::Stock_Block_Pango(stock_database = stock_db,
                                                       stock_symbol = input$Stock_Code,
                                                       year = Years())})

    output$trend_graph <- renderPlot(Pango::Trend_Graph.Pango(dataset = stock_blocks()@graphic_matrix))

    output$test <- renderPrint(Years())

  }

  return(server)

}
