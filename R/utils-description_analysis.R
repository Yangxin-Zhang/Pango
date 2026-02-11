

# R/utils-description_analysis.R

#' generate barchart
#'
#' @param dataset the dataset
#' @param aim_col the aim col
#' @param col_num the number of cols
#' @export

Barchart.Pango <- function(dataset,
                          aim_col,
                          col_num)
  {

  on.exit(gc())

  col_data <- dataset[,..aim_col] %>%
    unlist() %>%
    as.numeric()

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
    labs(title = "interval: ")
    theme(panel.background = element_blank(),
          panel.grid.major.y = element_line(color = "black",
                                            linewidth = 0.5),
          axis.text.x = element_text(colour = "black",
                                     angle = 60,
                                     hjust = 1),
          axis.line.x = element_line(color = "black",
                                     linewidth = 0.5),
          axis.line.y = element_line(color = "black",
                                     linewidth = 0.5))

  return(plot)

}

#' trend plot
#'
#' @param dataset the dataset
#' @param interval the description interval
#' @export

Trend_Graph.Pango <- function(dataset,
                              interval = c(1,nrow(dataset)),
                              ma_symbol = "sma",
                              ma_interval = 5)
  {

  on.exit(gc())

  dataset_cp <- copy(dataset)

  ma_col <- paste(ma_symbol,ma_interval,sep = "_d")
  ma_value <- dataset_cp[,..ma_col]
  dataset_cp[,ma := ma_value]

  ma_dir_col <- paste(ma_col,"dir",sep = "_")
  ma_dir_value <- dataset_cp[,..ma_dir_col]
  dataset_cp[,ma_dir := ma_dir_value]

  ma_inf_col <- paste(ma_col,"inf",sep = "_")
  ma_inf_value <- dataset_cp[,..ma_inf_col]
  dataset_cp[,ma_inf := ma_inf_value]

  interval <- seq(from = interval[1],
                  to = interval[2],
                  by = 1)

  plotting_dataset <- dataset_cp[interval]

  plot <- ggplot() +
    geom_point(data = plotting_dataset,
               mapping = aes(x = Date,
                             y = ma,
                             colour = factor(Trend_State),
                             size = factor(ma_inf))) +
    geom_segment(data = plotting_dataset,
                 mapping = aes(x = Date,
                               xend = Date,
                               y = High,
                               yend = Low)) +
    scale_colour_manual(values = c("1" = "red",
                                   "-1" = "blue",
                                   "0" = "green"),
                        na.value = "black") +
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
