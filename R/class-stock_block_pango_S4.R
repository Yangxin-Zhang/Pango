
# R/stock_block_pango_S4.R

#' the class of Stock Block Pango
#'
#' @param change_blocks the block list
#' @param information_matrix information matrix
#' @param graphic_matrix the matrix for plotting
#'
#' @export

setClass(Class = "Stock_Block_Pango",
         slots = list(
           change_blocks = "list",
           information_matrix = "data.table",
           graphic_matrix = "data.table",
           trend_matrix = "data.table",
           original_matrix = "data.table",
           moving_average_symbol = "character"
         ))

#' initialize Stock_Block_Pango
#'
#' @param original_dataset the original dataset
#' @param ma_symbol the moving average symbol

setMethod("initialize",
          signature = c(.Object = "Stock_Block_Pango"),
          definition = function(.Object,
                                original_dataset,
                                ma_symbol){

            on.exit(gc())

            .Object@moving_average_symbol <- ma_symbol
            .Object@original_matrix <- original_dataset

            .Object@change_blocks <- Pango:::.generate_stock_block(origi_dataset = .Object@original_matrix,
                                                                   ma_symbol = .Object@moving_average_symbol)

            .Object@information_matrix <- Pango:::.generate_block_information_matrix(.Object@change_blocks) %>%
              Pango:::.generate_trend_state()

            .Object@graphic_matrix <- Pango:::.generate_graphic_matrix(change_blocks = .Object@change_blocks,
                                                                       information_matrix = .Object@information_matrix)

            .Object@trend_matrix <- Pango:::.generate_trend_matrix(graphic_matrix = .Object@graphic_matrix,
                                                                   ma_col = .Object@moving_average_symbol)

            return(.Object)

          })

#' constructor
#'
#' @param stock_database the stock database
#' @param stock_symbol the stock symbol
#' @param year the year to download
#' @param period the data period
#' @param adjust the data adjust
#' @param market the market symbol
#'
#' @export

Stock_Block_Pango <- function(stock_database,
                              stock_symbol,
                              year,
                              period = "daily",
                              adjust = "qfq",
                              market = "A",
                              ma_symbol = "sma_d5")
  {

  original_dataset <- Pango::extract_stock_dataset_pango(stock_database,
                                                         stock_symbol = stock_symbol,
                                                         year = year,
                                                         period = period,
                                                         adjust = adjust,
                                                         market = market)

  obj <- new("Stock_Block_Pango",
             original_dataset =original_dataset,
             ma_symbol = ma_symbol)

  return(obj)

}
