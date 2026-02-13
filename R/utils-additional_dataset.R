
# R/additional_dataset.R

#' generate additional dataset
#'
#' @param origi_dataset the original dataset

.generate_additional_dataset <- function(origi_dataset)
  {

  on.exit(gc())

  # moving average

  setorder(origi_dataset,Date)
  origi_dataset[,Date_Order := seq(nrow(origi_dataset))]

  origi_dataset <- Pango:::.add_moving_average_information(origi_dataset = origi_dataset,
                                                           ma_col = "sma_d5") %>%
    Pango:::.add_moving_average_information(ma_col = "sma_d4") %>%
    Pango:::.add_moving_average_information(ma_col = "sma_d3")

  return(origi_dataset)

}

#' add moving average information
#'
#' @param origi_dataset the original dataset
#' @param ma_col the moving average column

.add_moving_average_information <- function(origi_dataset,
                                            ma_col)
  {

  on.exit(gc())

  splited_ma_col <- strsplit(ma_col,split = "_")

  ma_symbol <- splited_ma_col[[1]][1]
  day <- strsplit(splited_ma_col[[1]][2],split = "d")
  day <- day[[1]][2] %>%
    as.numeric()

  if (ma_symbol == "sma") {

    ma <- SMA(origi_dataset$Close,day)
    ma <- list(ma)
    names(ma) <- ma_col
    ma <- as.data.table(ma)

    origi_dataset <- bind_cols(origi_dataset,ma)

  }

  origi_dataset <- Pango:::.annotate_moving_average_location(origi_dataset = origi_dataset,
                                                             ma_col = ma_col) %>%
    Pango:::.annotate_moving_average_direction(ma_col = ma_col) %>%
    Pango:::.annotate_moving_average_inflection_point(ma_col = ma_col)

  return(origi_dataset)

}
#' annotate moving average location
#'
#' @param origi_dataset the original dataset
#' @param ma_col the moving average column

.annotate_moving_average_location <- function(origi_dataset,
                                               ma_col)
  {

  on.exit(gc())

  origi_dataset_cp <- copy(origi_dataset)

  ma_value <- origi_dataset_cp[,..ma_col]
  origi_dataset_cp[,ma := ma_value]

  ma_loc <- (origi_dataset_cp$ma-origi_dataset_cp$Low)/(origi_dataset_cp$High-origi_dataset_cp$Low)

  col_na <- paste(ma_col,"loc",sep = "_")
  loc_col <- list(ma_loc)
  names(loc_col) <- col_na
  loc_col <- as.data.table(loc_col)

  origi_dataset_cp <- bind_cols(origi_dataset,loc_col)

  return(origi_dataset_cp)

}

#' annotate moving average direction
#'
#' @param origi_dataset the original dataset
#' @param ma_col the moving average column

.annotate_moving_average_direction <- function(origi_dataset,
                                               ma_col)
  {

  on.exit(gc())

  interval <- strsplit(ma_col,split = "")
  interval <- interval[[1]][length(interval[[1]])] %>%
    as.numeric()

  origi_dataset_cp <- copy(origi_dataset)

  ma_dir <- lapply(as.list((interval+1):nrow(origi_dataset_cp)), function(x,origi_dataset_cp,interval){

    if (origi_dataset_cp$Close[x] > origi_dataset_cp$Close[x-interval]) {

      return(1)

    }
    if (origi_dataset_cp$Close[x] < origi_dataset_cp$Close[x-interval]) {

      return(-1)

    }
    if (origi_dataset_cp$Close[x] == origi_dataset_cp$Close[x-interval]) {

      return(0)

    }

  },
  origi_dataset = origi_dataset_cp,
  interval = interval)

  col_na <- paste(ma_col,"dir",sep = "_")
  dir_col <- list(c(rep(NA,interval),unlist(ma_dir)))
  names(dir_col) <- col_na
  dir_col <- as.data.table(dir_col,
                           na.rm = FALSE)

  origi_dataset_cp <- bind_cols(origi_dataset,dir_col)

  return(origi_dataset_cp)

}

#' annotate moving average inflection point
#'
#' @param origi_dataset the original dataset
#' @param ma_col the moving average column

.annotate_moving_average_inflection_point <- function(origi_dataset,
                                                      ma_col)
{

  on.exit(gc())

  interval <- strsplit(ma_col,split = "")
  interval <- interval[[1]][length(interval[[1]])] %>%
    as.numeric()

  origi_dataset_cp <- copy(origi_dataset)

  ma_value <- origi_dataset_cp[,..ma_col]
  origi_dataset_cp[,ma := ma_value]

  ma_col_dir <- paste(ma_col,"dir",sep = "_")
  ma_dir_value <- origi_dataset_cp[,..ma_col_dir]
  origi_dataset_cp[,ma_dir := ma_dir_value]

  ma_inf <- lapply(as.list((interval+2):(nrow(origi_dataset_cp)-1)), function(x,origi_dt,interval){

    if (origi_dt$ma_dir[x] == 0) {

      return(0)

    }

    lower = 1
    if (origi_dt$ma_dir[x-lower] == 0){

      if ((x-lower) == (interval+1)) {

        return(0)

      }

      while (origi_dt$ma_dir[x-lower] == 0) {

        lower <- lower+1

      }
    }

    upper = 1
    if (origi_dt$ma_dir[x+upper] == 0){

      if ((x+upper) == nrow(origi_dt)) {

        return(0)

      }

      while (origi_dt$ma_dir[x+upper] == 0) {

        upper <- upper+1

      }
    }

    if (origi_dt$ma_dir[x-lower] == origi_dt$ma_dir[x]) {

      return(0)

    } else {

      if (origi_dt$ma_dir[x+upper] != origi_dt$ma_dir[x]) {

        return(0)

      } else {

        return(origi_dt$ma_dir[x])

      }
    }
  },
  origi_dt = origi_dataset_cp,
  interval = interval)

  col_na <- paste(ma_col,"inf",sep = "_")
  inf_col <- list(c(rep(NA,(interval+1)),unlist(ma_inf),NA))
  names(inf_col) <- col_na
  inf_col <- as.data.table(inf_col,
                           na.rm = FALSE)

  origi_dataset_cp <- bind_cols(origi_dataset,inf_col)

  return(origi_dataset_cp)

}

#' annotate last inflection point and interval
#'
#' @param stock_block the stock block
#' @param ma_col the moving average column

.annotate_last_inflection_point_and_interval <- function(stock_block,
                                                         ma_col){

  on.exit(gc())

  last_ma_inf <- paste("last_inf",ma_col,sep = "_")
  last_ma_inf_int <- paste("last_inf_int",ma_col,sep = "_")

  trend_matrix <- copy(stock_block@trend_matrix)
  trend_matrix[,last_inf_ma := character()]
  trend_matrix[,last_inf_int_ma := numeric()]

  origi_dataset <- copy(stock_block@original_matrix) %>%
    setorder(Date_Order)

  ma_inf_na <- paste(ma_col,"inf",sep = "_")
  ma_inf <- origi_dataset[,..ma_inf_na]
  origi_dataset[,ma_inf := ma_inf]

  for (i in 2:nrow(trend_matrix)) {

    order1 <- origi_dataset[Date == trend_matrix[i-1,Start_Date],Date_Order]
    order2 <- origi_dataset[Date == trend_matrix[i-1,End_Date],Date_Order]

    origi_mat <- origi_dataset[Date_Order %in% seq(order1,order2),]

    last_inf <- origi_mat[ma_inf != 0]
    if (nrow(last_inf) == 0 ) {

      next

    } else {

      last_inf <- last_inf[nrow(last_inf),Date]

      last_inf_interval <- difftime(origi_dataset[Date_Order %in% order2,Date],last_inf) %>%
        as.numeric()

      trend_matrix[i,last_inf_ma := last_inf]
      trend_matrix[i,last_inf_int_ma := last_inf_interval]

    }

  }

  tr_mt_ls <- list(trend_matrix[,last_inf_ma],
                   trend_matrix[,last_inf_int_ma])
  names(tr_mt_ls) <- c(last_ma_inf,
                       last_ma_inf_int)

  tr_mt_ls <- as.data.table(tr_mt_ls)

  stock_block@trend_matrix <- bind_cols(stock_block@trend_matrix,tr_mt_ls)

  return(stock_block)

}

#' verify year group
#'
#' @param years the year group to verify

.verify_year_group <- function(years)
  {

  on.exit(gc())

  ref_year <- seq(from = min(years),
                  to = max(years))

  if (length(ref_year) == length(years)) {

    years <- list("Year_Group_1" = years)

    return(years)

  } else {

    year_loc <- ref_year %in% years

    years <-list()
    k <- numeric()
    for (i in 1:length(ref_year)) {

      if (i == 1) {

        k <- c(k,ref_year[i])

      }

      if (year_loc[i] == TRUE & i != 1) {

        k <- c(k,ref_year[i])

      }

      if (year_loc[i] == FALSE & length(k) != 0) {

        years <- append(years,list(k))
        k <- numeric()

      }

      if (i == length(ref_year)) {

        years <- append(years,list(k))

      }

    }

    year_gp_num <- length(years)
    gp_na <- paste("Year_Group",seq(year_gp_num),sep = "_")
    names(years) <- gp_na

    return(years)

  }
}
