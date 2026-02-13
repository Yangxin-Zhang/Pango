

# R/utils-description_analysis.R

#' generate barchart
#'
#' @param dataset the dataset
#' @param aim_col the aim col
#' @param col_num the number of cols
#' @export

Barchart.Pango <- function(dataset,
                          aim_col,
                          col_num,
                          log2_trans = FALSE)
  {

  on.exit(gc())

  if (log2_trans) {

    col_data <- dataset[,..aim_col] %>%
      unlist() %>%
      as.numeric() %>%
      log2()

  } else {

    col_data <- dataset[,..aim_col] %>%
      unlist() %>%
      as.numeric()

  }

  col_data <- col_data[!is.na(col_data)]
  col_data <- col_data[is.finite(col_data)]

  col_width <- (max(col_data)-min(col_data))/col_num

  mid_value <- (min(col_data)+((2*seq(col_num)-1)*(col_width/2))) %>%
    round(digits = 2)
  mid_value <- as.character(mid_value)

  splited_data <- split(col_data,
                        cut(col_data,
                            breaks = col_num)) %>%
    lengths() %>%
    as.data.frame(row.names = mid_value) %>%
    rownames_to_column()

  colnames(splited_data) <- c("interval","counts")

  splited_data <- mutate(splited_data,
                         interval = factor(interval,levels = interval))

  plot <- ggplot() +
    geom_col(data = splited_data,
             mapping = aes(x = interval,
                           y = counts)) +
    labs(title = "interval: ") +
    theme(panel.background = element_blank(),
          panel.grid.major.y = element_line(color = "grey90",
                                            linewidth = 0.3),
          axis.text.x = element_text(colour = "black",
                                     angle = 60,
                                     hjust = 1),
          axis.line.x = element_line(color = "black",
                                     linewidth = 0.5),
          axis.line.y = element_line(color = "black",
                                     linewidth = 0.5),
          plot.background = element_blank())

  return(plot)

}

#' trend plot
#'
#' @param stock_block the stock block class
#' @param interval the description interval
#' @param interval_width the interval width
#' @export

Trend_Graph.Pango <- function(stock_block,
                              interval = numeric(),
                              interval_width = 100,
                              ma_symbol = "sma_d5",
                              linewidth = 0.3)
  {

  on.exit(gc())

  dataset_graphic <- copy(stock_block@graphic_matrix) %>%
    Pango:::.generate_plotting_cols(ma_symbol = ma_symbol)
  dataset_graphic[,Data_Symbol := "graphic"]
  date_graphic <- dataset_graphic[,Date]

  dataset_original <- copy(stock_block@original_matrix) %>%
    Pango:::.generate_plotting_cols(ma_symbol = ma_symbol)
  dataset_original[,Data_Symbol := "original"]
  date_original <- dataset_original[,Date]

  dataset <- bind_rows(dataset_original[!date_original %in% date_graphic],
                       dataset_graphic) %>%
    setorder(Date)

  dataset[Data_Symbol == "original", Trend_State := ma_dir]

  if (length(interval) == 2) {

    if (sum(!is.na(interval)) == 2) {

      interval <- seq(from = interval[1],
                      to = interval[2],
                      by = 1)

    } else {

      if (is.na(interval[1])) {

        interval <- seq(from = 1,
                        to = interval[2],
                        by = 1)

      } else {

        interval <- seq(from = interval[1],
                        to = nrow(dataset),
                        by = 1)

      }

    }
  } else {

    if (interval > interval_width) {

      interval <- seq(from = (interval-interval_width-1),
                      to = interval,
                      by = 1)

    } else {

      interval <- seq(from = 1,
                      to = interval,
                      by = 1)

    }
  }

  plotting_dataset <- dataset[interval]

  plotting_dataset[Close > Open, linecolor := "red"]
  plotting_dataset[Close == Open, linecolor := "green"]
  plotting_dataset[Close < Open, linecolor := "blue"]

  plot <- ggplot() +
    geom_point(data = plotting_dataset,
               mapping = aes(x = Date,
                             y = ma,
                             colour = factor(Trend_State),
                             size = factor(ma_inf))) +
    scale_colour_manual(values = c("1" = "red",
                                   "-1" = "blue",
                                   "0" = "green"),
                        na.value = "black") +
    new_scale_colour() +
    geom_segment(data = plotting_dataset,
                 mapping = aes(x = Date,
                               xend = Date,
                               y = High,
                               yend = Low,
                               colour = linecolor),
                 linewidth = linewidth) +
    geom_errorbar(data = plotting_dataset,
                 mapping = aes(ymin = Close,
                               ymax = Open,
                               x = Date,
                               colour = linecolor),
                 linewidth = linewidth,
                 width = 0.5) +
    scale_colour_manual(values = c("red" = "red",
                                   "blue" = "blue",
                                   "green" = "green")) +
    scale_size_manual(values = c("1" = 2,
                                 "-1" = 2,
                                 "0" = 1),
                      na.value = 1) +
    theme(axis.ticks.x = element_blank(),
          axis.title.x = element_blank(),
          axis.text.x = element_blank(),
          axis.title.y = element_blank(),
          panel.background = element_blank(),
          axis.line.x = element_line(colour = "black",
                                     linewidth = 0.5),
          axis.line.y = element_line(colour = "black",
                                     linewidth = 0.5),
          panel.grid.major.x = element_line(colour = "grey90",
                                            linewidth = 0.5),
          panel.grid.major.y = element_line(colour = "grey90",
                                            linewidth = 0.5),
          legend.position = "none")

  return(plot)

}

#' generate plotting cols
#'
#' @param original_dataset the original dataset
#' @param ma_symbol the moving average symbol

.generate_plotting_cols <- function(original_dataset,
                                    ma_symbol) {

  on.exit(gc())

  dataset <- copy(original_dataset)
  ma_value <- dataset[,..ma_symbol]
  dataset[,ma := ma_value]

  ma_dir_col <- paste(ma_symbol,"dir",sep = "_")
  ma_dir_value <- dataset[,..ma_dir_col]
  dataset[,ma_dir := ma_dir_value]

  ma_inf_col <- paste(ma_symbol,"inf",sep = "_")
  ma_inf_value <- dataset[,..ma_inf_col]
  dataset[,ma_inf := ma_inf_value]

  return(dataset)

}
