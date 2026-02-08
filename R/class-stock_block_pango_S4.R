
# R/stock_block_pango_S4.R

#' the class of Stock Block Pango
#'
#' @param change_blocks the block list
#' @param information_matrix information matrix
#'
#' @export

setClass(Class = "Stock_Block_Pango",
         slots = list(
           change_blocks = "list",
           information_matrix = "data.table"
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

            .Object@change_blocks <- Pango:::.generate_stock_block(origi_dataset = original_dataset,
                                                                   ma_symbol = ma_symbol)

            .Object@information_matrix <- Pango:::.generate_block_information_matrix(.Object@change_blocks)

            return(.Object)

          })

#' constructor
#'
#' @param original_dataset the original dataset
#' @param ma_symbol the moving average symbol
#'
#' @export

Stock_Block_Pango <- function(original_dataset,
                              ma_symbol)
  {

  obj <- new("Stock_Block_Pango",
             original_dataset =original_dataset,
             ma_symbol = ma_symbol)

  return(obj)

}
