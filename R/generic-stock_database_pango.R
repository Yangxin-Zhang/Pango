
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

  stock_db <- Pango::Stock_Database_Pango(db_na = x$db_na,
                                          db_path = x$db_path,
                                          activate = TRUE)

  ak <- reticulate::import("akshare",convert = FALSE)
  sql <- reticulate::import("sqlite3",convert = FALSE)

  if (market == "A") {

    sheet_na <- paste(stock_symbol,year,sep = "-") %>%
      paste(period,sep = "_") %>%
      paste(adjust,sep = "-") %>%
      paste(market,sep = "-")
    names(sheet_na) <- year

    stock_dataset_ls <- as.list(year)
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

      stock_dataset$columns <- c("Date", "Stock_Code", "Open", "Close", "High", "Low",
                                 "Volume", "Turnover", "Volatility", "Return",
                                 "Price_Change", "Turnover_Rate")

      return(stock_dataset)

    },
    stock_symbol = stock_symbol,
    adjust = adjust,
    period = period,
    ak = ak)

    py_cnn <- sql$connect(stock_db$db_path)

    for (i in 1:length(sheet_na)) {

      stock_dataset <- stock_dataset_ls[[sheet_na[i]]]

      stock_dataset$to_sql(sheet_na[i],py_cnn,
                           if_exists = 'replace',
                           index = FALSE)

      cat("write successfuly:",sheet_na[i],"\n")

    }

    py_cnn$close()

  }

  dbDisconnect(stock_db$db_cnn)

  stock_db <- Pango::Stock_Database_Pango(db_na = x$db_na,
                                          db_path = x$db_path,
                                          activate = TRUE,
                                          message_connect = FALSE)

  # for (i in 1:length(year)) {
  #
  #   st_dt <- Pango::extract_stock_dataset_pango(stock_db,
  #                                               stock_symbol = stock_symbol,
  #                                               year = year[i],
  #                                               period = period,
  #                                               adjust = adjust,
  #                                               market = market) %>%
  #     Pango:::.generate_additional_dataset()
  #
  #   dbWriteTable(conn = stock_db$db_cnn,
  #                name = sheet_na[year[i]],
  #                value = st_dt,
  #                overwrite = TRUE)
  #
  # }
  #
  # dbDisconnect(stock_db$db_cnn)
  #
  # stock_db <- Pango::Stock_Database_Pango(db_na = x$db_na,
  #                                         db_path = x$db_path,
  #                                         activate = FALSE,
  #                                         message_connect = FALSE)

  return(stock_db)

}

#' extract stock dataset pango
#'
#' @export

extract_stock_dataset_pango <- function(x,...)
{

  UseMethod("extract_stock_dataset_pango")

}

#' extract stock dataset pango
#'
#' @param stock_symbol the stock symbol
#' @param year the year to download
#' @param period the data period
#' @param adjust the data adjust
#' @param market the market symbol
#' @export

extract_stock_dataset_pango.Stock_Database_Pango <- function(x,
                                                             stock_symbol,
                                                             year,
                                                             period = "daily",
                                                             adjust = "qfq",
                                                             market = "A",
                                                             ...)
  {

  stock_db <- Pango::Stock_Database_Pango(db_na = x$db_na,
                                          db_path = x$db_path,
                                          activate = TRUE)

  sheet_na <- paste(stock_symbol,year,sep = "-") %>%
    paste(period,sep = "_") %>%
    paste(adjust,sep = "-") %>%
    paste(market,sep = "-")

  stock_dataset_ls <- vector("list",length = length(sheet_na))
  names(stock_dataset_ls) <- sheet_na

  for (i in 1:length(sheet_na)) {

    db_cmd <- paste("SELECT * FROM",paste("'","'",sep = sheet_na[i]),sep = " ")

    stock_dataset_ls[sheet_na[i]] <- dbGetQuery(stock_db$db_cnn,
                                                db_cmd )%>%
      list()

  }

  dbDisconnect(stock_db$db_cnn)

  cat("disconnect successfully\n")

  return(rbindlist(stock_dataset_ls))

}
