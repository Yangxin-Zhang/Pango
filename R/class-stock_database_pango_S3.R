
# R/class-stock_database_pango_S3.R

#' Stock Database Pango version S3 class
#'
#' @param db_path the database path
#'
#' @export

Stock_Database_Pango <- function(db_na = NULL,
                                 db_path = NULL) {


  if (is.null(db_path)) {

    if (is.null(db_na)) {

      db_na <- "Stock_Database.db"

    }

    db_path <- paste(getwd(),
                     paste(".DataBase",db_na,sep = "/"),
                     sep = "/")

  }

  db_cnn <- dbConnect(SQLite(),db_path)
  cat("connect successfully\n")
  on.exit(dbDisconnect(db_cnn),add = TRUE)
  on.exit(cat("disconnect successfully\n"),add = TRUE)

  obj <- list(db_cnn = db_cnn,
              db_path = db_path)

  class(obj) <- "Stock_Database_Pango"

  return(obj)

}
