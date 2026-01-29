
# R/generic-stock_database_pango.R

#' download stock dataset pango
#'
#' @export

download_stock_dataset_pango <- function(x,...)
  {

  UseMethod("download_stock_dataset_pango")

}

#' download stock dataset pango
#'
#' @param stock_symbol the stock symbol
#' @param year the year to download
#' @param period the data period
#' @param adjust the data adjust
#' @param market the market symbol
#' @export

download_stock_dataset_pango.Stock_Database_Pango <- function(x,
                                                              stock_symbol,
                                                              year,
                                                              period = "daily",
                                                              adjust = "qfq",
                                                              market = "A",
                                                              ...)
  {

  stock_db <- x

  ak <- reticulate::import("akshare",convert = FALSE)
  sql <- RSQLite::import("sqlite3",convert = FALSE)

  if (market == "A") {

    sheet_na <- paste(stock_symbol,year,sep = "-")

    stock_dataset_ls <- list(year)
    names(stock_dataset_ls) <- sheet_na

    stock_dataset_ls <- lapply(stock_dataset_ls,function(year,
                                                         stock_symbol,
                                                         adjust,
                                                         period,
                                                         ak){

      start_date <- paste(year,"0101",sep = "")
      end_date <- paste(year,"1231",sep = "")
      stock_dataset <- ak$stock_zh_a_hist(symbol = stock_symbol,
                                          period = period,
                                          start_date = start_date,
                                          end_date = end_date,
                                          adjust = adjust)

      return(stock_dataset)

    },
    stock_symbol = stock_symbol,
    adjust = adjust,
    period = period,
    ak = ak)

    py_cnn <- sql$connect(stock_db$db_path)
    mapply(function(stock_dataset,
                    stock_db,
                    ak,
                    py_cnn,
                    sheet_na){

      stock_dataset$to_sql(sheet_na,py_cnn,
                           if_exists = 'replace',
                           index = FALSE)

      cat("write successfuly",sheet_na)

    },
    stock_dataset_ls,
    stock_db,
    ak,
    py_cnn,
    sheet_na)
    py_cnn$close()

  }

}
