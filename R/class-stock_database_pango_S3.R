
# R/class-stock_database_pango_S3.R

#' Stock Database Pango version S3 class
#'
#' @param db_path the database path
#'
#' @export

Stock_Database_Pango <- function(db_na = NULL,
                                 db_path = NULL,
                                 activate = FALSE,
                                 message_connect = TRUE,
                                 message_disconnect = TRUE) {


  if (is.null(db_path)) {

    if (is.null(db_na)) {

      db_na <- "Stock_Database.db"

    }

    db_path <- paste(getwd(),
                     paste(".DataBase",db_na,sep = "/"),
                     sep = "/")

  }

  db_cnn <- dbConnect(SQLite(),db_path)

  if (message_connect) {

    cat("connect successfully\n")

  }

  if (!activate) {

    on.exit(dbDisconnect(db_cnn),add = TRUE)

    if (message_disconnect) {

      on.exit(cat("disconnect successfully\n"),add = TRUE)

    }

  }

  obj <- list(db_na = db_na,
              db_cnn = db_cnn,
              db_path = db_path,
              table = dbListTables(db_cnn))

  class(obj) <- "Stock_Database_Pango"

  return(obj)

}
