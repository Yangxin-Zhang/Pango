
# R/utils-Server.R

#' shiny server

.shiny_server_pango <- function(stock_db,
                                interval_width = 100) {

  server <- function(input,output) {

    Years <- reactive({
      years <- strsplit(input$Year,split = ",")
      years <- years[[1]]
      return(as.numeric(years))
    })

    Interval <- reactive({
      interval <- strsplit(input$Interval,split = ",")
      interval <- interval[[1]]
      return(as.numeric(interval))
    })

    stock_blocks <- reactive({Pango::Stock_Block_Pango(stock_database = stock_db,
                                                       stock_symbol = input$Stock_Code,
                                                       year = Years())})


    output$slider_location <- renderUI(

      if (length(Interval()) == 1) {

        slider_max <- nrow(stock_blocks()@original_matrix)
        if (slider_max > Interval()) {

            slider_min <- Interval()

            slider <- sliderInput(inputId = "Interval_Location",
                                  label = "Interval_Location",
                                  min = slider_min,
                                  max = slider_max,
                                  value = Interval(),
                                  step = 1)

          return(slider)

        } else {

          return(NULL)

        }
      } else {

        return(NULL)

      })

    output$slider_width <- renderUI(

      if (length(Interval()) == 1) {

        slider_max <- nrow(stock_blocks()@original_matrix)

        slider_min <- 1

        slider <- sliderInput(inputId = "Interval_Width",
                              label = "Interval_Width",
                              min = slider_min,
                              max = slider_max,
                              value = interval_width,
                              step = 1)

        return(slider)

      } else {

        return(NULL)

      })

    Interval_Location <- reactive({

      if (is.null(input$Interval_Location)) {

        return(Interval())

      } else {

        return(input$Interval_Location)

      }

    })

    Interval_Width <- reactive({

      if (is.null(input$Interval_Width)) {

        return(interval_width)

      } else {

        return(input$Interval_Width)

      }

    })

    output$trend_graph <- renderPlot(Pango::Trend_Graph.Pango(stock_block = stock_blocks(),
                                                              ma = "sma_d5",
                                                              interval = Interval_Location(),
                                                              interval_width = Interval_Width()))

    output$test <- renderPrint(Years())

  }

  return(server)

}
