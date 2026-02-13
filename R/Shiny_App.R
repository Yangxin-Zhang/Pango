
# R/Shiny_App.R

#' the shiny app
#'
#' @param database_path the database path
#'
#' @export

Shiny_App_Pango <- function(database_path) {

  stock_db <- Pango::Stock_Database_Pango(db_path = database_path)

  shiny_app <- shinyApp(ui = Pango:::.shiny_user_interface_pango(),
                        server = Pango:::.shiny_server_pango(stock_db = stock_db))

  return(shiny_app)

}
