
# R/additional_dataset.R

#' generate additional dataset
#'
#' @param origi_dataset the original dataset

.generate_additional_dataset <- function(origi_dataset)
  {

  on.exit(gc())

  # moving average
  sma_d5 <- SMA(origi_dataset$Close,5)
  origi_dataset[,sma_d5 := sma_d5]

  origi_dataset <- Pango:::.annotate_moving_average_location(origi_dataset = origi_dataset,
                                                             ma_col = "sma_d5") %>%
    Pango:::.annotate_moving_average_direction(ma_col = "sma_d5") %>%
    Pango:::.annotate_moving_average_inflection_point(ma_col = "sma_d5")

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

  ma_value <- origi_dataset_cp[,..ma_col]
  origi_dataset_cp[,ma := ma_value]

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
