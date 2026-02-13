
# R/utils-stock_block

#' generate block sequence
#'
#' @param seq_breaks the sequence breaks

.generate_block_sequence <- function(seq_breaks)
  {

  on.exit(gc())

  block_na <- paste("block",seq((length(seq_breaks)-1)),sep = "_")
  seq_ls <- vector("list",length = length(block_na))
  names(seq_ls) <- block_na

  for (i in 1:length(block_na)) {

    seq_ls[block_na[i]] <- list(seq(from = seq_breaks[i],
                                    to = (seq_breaks[i+1]-1),
                                    by = 1))

  }

  return(seq_ls)

}

#' generate block location sequence
#'
#' @param origi_dataset the original dataset
#' @param ma_symbol the moving average symbol

.generate_block_location_sequence <- function(origi_dataset,
                                              ma_symbol)
  {

  on.exit(gc())

  origi_dataset_cp <- copy(origi_dataset)

  ma_value <- origi_dataset_cp[,..ma_symbol]
  origi_dataset_cp[,ma := ma_value]

  ma_dir_na <- paste(ma_symbol,"dir",sep = "_")
  ma_dir_value <- origi_dataset_cp[,..ma_dir_na]
  origi_dataset_cp[,ma_dir := ma_dir_value]

  ma_inf_na <- paste(ma_symbol,"inf",sep = "_")
  ma_inf_value <- origi_dataset_cp[,..ma_inf_na]
  origi_dataset_cp[,ma_inf := ma_inf_value]

  loc_inf <- which(origi_dataset_cp$ma_inf != 0)
  loc_dir <- origi_dataset_cp$ma_inf[loc_inf]

  i = 1
  while (i < length(loc_inf)) {

    if (!is.na(loc_dir[i]) & loc_dir[i] == loc_dir[i+1]) {

      loc_inf[i+1] <- NA

      i <- i+1

    } else {

      i <- i+1

    }
  }

  loc <- loc_inf[!is.na(loc_inf)]

  return(loc)

}

#' generate stock block
#'
#' @param origi_dataset the original dataset
#' @param ma_symbol the moving average symbol

.generate_stock_block <- function(origi_dataset,
                                  ma_symbol)
  {

  on.exit(gc())

  origi_dataset_cp <- copy(origi_dataset)

  ma_value <- origi_dataset_cp[,..ma_symbol]
  origi_dataset_cp[,ma := ma_value]

  ma_dir_na <- paste(ma_symbol,"dir",sep = "_")
  ma_dir_value <- origi_dataset_cp[,..ma_dir_na]
  origi_dataset_cp[,ma_dir := ma_dir_value]

  ma_inf_na <- paste(ma_symbol,"inf",sep = "_")
  ma_inf_value <- origi_dataset_cp[,..ma_inf_na]
  origi_dataset_cp[,ma_inf := ma_inf_value]

  block_sequence_ls <- Pango:::.generate_block_location_sequence(origi_dataset = origi_dataset_cp,
                                                                 ma_symbol = ma_symbol) %>%
    Pango:::.generate_block_sequence()

  block_na <- names(block_sequence_ls)

  change_blocks <- lapply(block_sequence_ls, function(block_sequence,origi_dt_cp,origi_dt){

    sub_dt <- origi_dt_cp[block_sequence]

    if (unique(sub_dt[ma_inf != 0,ma_inf]) == 1) {

      block_volatility <- (max(sub_dt[,ma])-min(sub_dt[,ma]))/min(sub_dt[,ma])

    }

    if (unique(sub_dt[ma_inf != 0,ma_inf]) == -1) {

      block_volatility <- (max(sub_dt[,ma])-min(sub_dt[,ma]))/max(sub_dt[,ma])

    }

    ch_blo <- Pango::Change_Block_Pango(stock_symbol = unique(sub_dt$Stock_Code),
                                        start_date = origi_dt_cp[block_sequence[1],Date],
                                        end_date = origi_dt_cp[block_sequence[length(block_sequence)],Date],
                                        block_period = length(block_sequence),
                                        block_dataset = origi_dt[block_sequence],
                                        inflection_point = sub_dt[ma_inf != 0,Date],
                                        block_volatility = block_volatility,
                                        block_order = numeric(),
                                        block_direction = unique(sub_dt[ma_inf != 0,ma_inf]),
                                        block_location = sub_dt[,Date],
                                        change_state = numeric())

    return(ch_blo)

  },
  origi_dt_cp = origi_dataset_cp,
  origi_dt = origi_dataset)

  for (i in 1:length(block_na)) {

    blo_ord <- strsplit(block_na[i],split = "_")
    blo_ord <- blo_ord[[1]][2] %>%
      as.numeric()

    change_blocks[[block_na[i]]]$block_order <- blo_ord

    if (change_blocks[[block_na[i]]]$block_volatility <= 0.01) {

      change_blocks[[block_na[i]]]$change_state <- 0

    } else {

      change_blocks[[block_na[i]]]$change_state <- change_blocks[[block_na[i]]]$block_direction

    }
  }

  return(change_blocks)

}

#' generate block information matrix
#'
#' @param block_ls the stock block list

.generate_block_information_matrix <- function(block_ls)
  {

  on.exit()

  block_order <- numeric()
  stock_code <- character()
  start_date <- character()
  end_date <- character()
  block_period <- numeric()
  block_volatility <- numeric()
  block_direction <- numeric()
  change_state <- numeric()

  for (i in 1:length(block_ls)) {

    block_order <- c(block_order,block_ls[[i]]$block_order)
    stock_code <- c(stock_code,block_ls[[i]]$stock_symbol)
    start_date <- c(start_date,block_ls[[i]]$start_date)
    end_date <- c(end_date,block_ls[[i]]$end_date)
    block_period <- c(block_period,block_ls[[i]]$block_period)
    block_volatility <- c(block_volatility,block_ls[[i]]$block_volatility)
    block_direction <- c(block_direction,block_ls[[i]]$block_direction)
    change_state <- c(change_state,block_ls[[i]]$change_state)

  }

  blo_info_mat <- list("Block_Order" = block_order,
                       "Stock_Code" = stock_code,
                       "Start_Date" = start_date,
                       "End_Date" = end_date,
                       "Block_Period" = block_period,
                       "Block_Volatility" = block_volatility,
                       "Block_Direction" = block_direction,
                       "Change_State" = change_state) %>%
    as.data.table()

  return(blo_info_mat)

}

#' generate trend state
#'
#' @param block_info_mat the stock block

.generate_trend_state <- function(block_info_mat) {

  on.exit()

  block_info_mat_cp <- copy(block_info_mat)
  trend_state_value <- block_info_mat_cp$Change_State
  block_info_mat_cp[,Trend_State := trend_state_value]

  block_num <- nrow(block_info_mat)

  i=2
  while (i < block_num) {

    if (block_info_mat_cp$Change_State[i] == 0 & block_info_mat_cp$Change_State[i-1] !=0 & block_info_mat_cp$Change_State[i+1] != 0) {

      block_info_mat_cp[i,Trend_State := block_info_mat_cp$Change_State[i-1]]

    }

    i <- i+1

  }

  i=1
  while (i <= block_num) {

    k=1
    while (i+k <= block_num) {

      if (block_info_mat_cp$Trend_State[i] == block_info_mat_cp$Trend_State[i+k]) {

        k <- k+1

      } else {

        break

      }
    }

    if (k > 1) {

      block_info_mat_cp[c(i:(i+k-1)),Trend_State := block_info_mat_cp$Trend_State[i]]

    } else {

      if (block_info_mat_cp$Block_Volatility[i] < 0.02) {

        block_info_mat_cp[i,Trend_State := 0]

      }
    }

    i <- i+k

  }


  return(block_info_mat_cp)

}

#' generate graphic matrix
#'
#' @param change_blocks the change blocks
#' @param information_matrix the information matrix

.generate_graphic_matrix <- function(change_blocks,
                                     information_matrix)
  {

  on.exit(gc())

  block_na <- names(change_blocks)

  graphic_matrix_ls <- list()
  for (i in 1:length(block_na)) {

    block_order <- strsplit(block_na[i],split = "_")
    block_order <- block_order[[1]][2] %>%
      as.numeric()

    gra_mat <- change_blocks[[block_na[i]]]$block_dataset

    trend_state <- information_matrix[Block_Order == block_order,Trend_State]

    gra_mat[,Trend_State := trend_state]

    graphic_matrix_ls <- append(graphic_matrix_ls,list(gra_mat))

  }

  graphic_matrix <- rbindlist(graphic_matrix_ls)

  return(graphic_matrix)

}

#' generate trend matrix
#'
#' @param graphic_matrix the graphic matrix

.generate_trend_matrix <- function(graphic_matrix,
                                   ma_col = "sma_d5")
  {

  on.exit(gc())

  graphic_num <- nrow(graphic_matrix)

  trend_matrix_ls <- list()
  i=1
  trend_order <- 1
  while (i <= graphic_num) {

    k = 1
    while (i+k <= graphic_num) {

      if (graphic_matrix$Trend_State[i+k] == graphic_matrix$Trend_State[i]) {

        k <- k+1

      } else {

        break

      }
    }

    trend_na <- paste("Trend",trend_order,sep = "_")
    trend_matrix_ls[trend_na] <- list("trend_order" = trend_order,
                                      "start_date" = graphic_matrix$Date[i],
                                      "end_date" = graphic_matrix$Date[i+k-1],
                                      "start_close" = graphic_matrix$Close[i],
                                      "end_close" = graphic_matrix$Close[i+k-1],
                                      "trend_direction" = graphic_matrix$Trend_State[i],
                                      "start_ma" = unlist(graphic_matrix[i,..ma_col]),
                                      "end_ma" = unlist(graphic_matrix[i+k-1,..ma_col])) %>%
      list()

    i <- i+k
    trend_order <- trend_order+1

  }

  trend_na <- names(trend_matrix_ls)
  trend_order <- numeric()
  start_date <- character()
  end_date <- character()
  start_close <- numeric()
  end_close <- numeric()
  trend_direction <- numeric()
  start_ma <- numeric()
  end_ma <- numeric()
  for (i in 1:length(trend_na)) {

    trend_order <- c(trend_order,trend_matrix_ls[[trend_na[i]]]$trend_order)
    start_date <- c(start_date,trend_matrix_ls[[trend_na[i]]]$start_date)
    end_date <- c(end_date,trend_matrix_ls[[trend_na[i]]]$end_date)
    trend_direction <- c(trend_direction,trend_matrix_ls[[trend_na[i]]]$trend_direction)
    start_close <- c(start_close,trend_matrix_ls[[trend_na[i]]]$start_close)
    end_close <- c(end_close,trend_matrix_ls[[trend_na[i]]]$end_close)
    start_ma <- c(start_ma,trend_matrix_ls[[trend_na[i]]]$start_ma)
    end_ma <- c(end_ma,trend_matrix_ls[[trend_na[i]]]$end_ma)

  }

  trend_matrix <- list("Trend_Symbol" = trend_na,
                       "Trend_Order" = trend_order,
                       "Start_Date" = start_date,
                       "End_Date" = end_date,
                       "Trend_Direction" = trend_direction,
                       "Start_Close" = start_close,
                       "End_Close" = end_close,
                       "Start_Ma" = start_ma,
                       "End_Ma" = end_ma) %>%
    as.data.table()

  trend_matrix[,Close_Volatility := (End_Close-Start_Close)/Start_Close]
  trend_matrix[,Ma_Volatility := (End_Ma-Start_Ma)/Start_Ma]
  trend_matrix[,Trend_Interval := as.numeric(difftime(End_Date,Start_Date))]

  return(trend_matrix)

}
