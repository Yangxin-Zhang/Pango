
# R/change_block_pango_S3.R

#' Change Block Pango version S3 class
#'
#' @param stock_symbol the stock symbol
#' @param start_date the start date
#' @param end_date the end date
#' @param block_period the block period
#' @param block_dataset the block dataset
#' @param inflection_point the inflection point
#' @param block_volatility the block volatility
#' @param block_order the block order
#' @param block_direction the block direction
#' @param block_location block_location
#' @param change_state the change state
#'
#' @export

Change_Block_Pango <- function(stock_symbol,
                               start_date,
                               end_date,
                               block_period,
                               block_dataset,
                               inflection_point,
                               block_volatility,
                               block_order,
                               block_direction,
                               block_location,
                               change_state)
  {

  obj <- list(stock_symbol = stock_symbol,
              start_date = start_date,
              end_date = end_date,
              block_period = block_period,
              block_dataset = block_dataset,
              inflection_point = inflection_point,
              block_volatility = block_volatility,
              block_order = block_order,
              block_direction = block_direction,
              block_location = block_location,
              change_state = change_state)

  class(obj) <- "Change_Block_Pango"

  return(obj)

}
