#' @importFrom caret trainControl train
#' @importFrom DT renderDataTable datatable
#' @importFrom ggplot2 ggplot aes geom_point geom_smooth theme_classic position_dodge stat_summary ylab xlab ggtitle geom_hline scale_linetype_manual guide_legend theme element_text geom_col theme_bw
#' @importFrom graphics plot points segments
#' @importFrom htmltools tagList
#' @importFrom imager load.image grabRect
#' @importFrom performance check_model
#' @importFrom readxl read_xlsx read_excel
#' @importFrom shiny reactiveValues observeEvent showModal modalDialog textInput actionButton modalButton reactiveVal removeModal req renderPlot reactiveFileReader updateTextInput updateRadioButtons updateNumericInput stopApp
#' @importFrom shinycssloaders showSpinner
#' @importFrom stats na.omit lm predict
#' @importFrom writexl write_xlsx
####
# Functions
####

options(shiny.maxRequestSize = 50 * 1024 ^ 2)

wd <- getwd()

# Define the server
server <- function(input, output, session) {
  # Initialize the measurements data frame
  measured_animals <- shiny::reactiveValues(length_measurement = data.frame(),
                                            width_measurements = data.frame(),
  )

  newdata_10 <- shiny::reactiveValues(
    ID = character(),
    score = character(),
    Date = character(),
    F_Alt = numeric(),
    TO_Alt = numeric(),
    Calti = numeric(),
    BL = numeric(),
    width10 = numeric(),
    width20 = numeric(),
    width30 = numeric(),
    width40 = numeric(),
    width50 = numeric(),
    width60 = numeric(),
    width70 = numeric(),
    width80 = numeric(),
    width90 = numeric(),
    fw = numeric()
  )

  newdata_05 <- shiny::reactiveValues(
    ID = character(),
    score = character(),
    Date = character(),
    F_Alt = numeric(),
    TO_Alt = numeric(),
    Calti = numeric(),
    BL = numeric(),
    width05 = numeric(),
    width10 = numeric(),
    width15 = numeric(),
    width20 = numeric(),
    width25 = numeric(),
    width30 = numeric(),
    width35 = numeric(),
    width40 = numeric(),
    width45 = numeric(),
    width50 = numeric(),
    width55 = numeric(),
    width60 = numeric(),
    width65 = numeric(),
    width70 = numeric(),
    width75 = numeric(),
    width80 = numeric(),
    width85 = numeric(),
    width90 = numeric(),
    width95 = numeric(),
    fw = numeric()
  )

  cur_dir <- shiny::reactiveVal(value = as.character())
  user_dir <- shiny::reactiveVal(value = as.character())

  #################################
  # Setting the working directory #
  #################################

  shiny::observeEvent(input$path, {
    shiny::showModal(shiny::modalDialog(
      shiny::textInput("wd", "Enter the directory path:", ""),
      footer = htmltools::tagList(
        shiny::actionButton("confirmBtn", "Confirm"),
        shiny::modalButton("Cancel")
      )
    ))
  })

  shiny::observeEvent(input$confirmBtn, {
    setwd(input$wd)

    user_dir(input$wd)

    shiny::removeModal()  # close dialog box
  })

  ###############################
  # Creating a start data frame #
  ###############################

  shiny::observeEvent(input$create, {

    data_in <- list("10%_interval", "05%_interval") %in%
      list.files()

    if (input$segments == 1 && data_in[1] == F) {

      dir.create("./10%_interval")

      cur_dir(paste(user_dir(), "/10%_interval", sep = ""))

      p10 <- paste(cur_dir(), "/Measurements_10.xlsx", sep = "")
      p10.1 <- paste(cur_dir(), "/Measurements_10_1.xlsx", sep = "")

      measurements <- create_data(segments = 1,
                                  path = p10,
                                  path2 = p10.1)

      shiny::showModal(
        shiny::modalDialog(
          title = "Dataframe created",
          "10% intervals",
          footer = shiny::modalButton("OK"),
          easyClose = TRUE
        )
      )

      # Display measurements table
      output$mTable <- DT::renderDataTable({
        measurements <- readxl::read_xlsx(path = p10.1, col_names = T)

        DT::datatable(measurements)
      })
    } else {
      shiny::showModal(
        shiny::modalDialog(
          title = "Dataframe already exist in directory",
          "Import the data",
          footer = shiny::modalButton("OK"),
          easyClose = TRUE
        )
      )
    }

    if (input$segments == 2 && data_in[2] == F) {

      dir.create("./05%_interval")

      cur_dir(paste(user_dir(), "/05%_interval", sep = ""))

      p05 <- paste(cur_dir(), "/Measurements_05.xlsx", sep = "")
      p05.1 <- paste(cur_dir(), "/Measurements_05_1.xlsx", sep = "")

      measurements <- create_data(segments = 2,
                                  path = p05,
                                  path2 = p05.1)

      shiny::showModal(
        shiny::modalDialog(
          title = "Dataframe created",
          "5% intervals",
          footer = shiny::modalButton("OK"),
          easyClose = TRUE
        )
      )

      # Display measurements table
      output$mTable <- DT::renderDataTable({
        measurements <- readxl::read_xlsx(path = p05.1, col_names = T)

        DT::datatable(measurements)
      })
    } else {
      shiny::showModal(
        shiny::modalDialog(
          title = "Dataframe already exist in directory",
          "Import the data",
          footer = shiny::modalButton("OK"),
          easyClose = TRUE
        )
      )
    }
  })

  shiny::observeEvent(input$import, {

    data_in <- list("10%_interval", "05%_interval") %in%
      list.files()

    if (input$segments == 1) {
      if (data_in[1] == T) {

        cur_dir(paste(user_dir(), "/10%_interval", sep = ""))

        p10 <- paste(cur_dir(), "/Measurements_10.xlsx", sep = "")
        p10.1 <- paste(cur_dir(), "/Measurements_10_1.xlsx", sep = "")

        shiny::showModal(
          shiny::modalDialog(
            title = "Dataframe already exist in directory",
            paste("Dataframe imported: 10% interval", sep = ""),
            footer = shiny::modalButton("OK"),
            easyClose = TRUE
          )
        )

        # Display measurements table
        output$mTable <- DT::renderDataTable({
          measurements <- readxl::read_xlsx(path = p10.1, col_names = T)

          DT::datatable(measurements)
        })
      } else {
        shiny::showModal(
          shiny::modalDialog(
            title = "Dataframe not exist in directory",
            paste("Create a 10% interval dataframe before", sep = ""),
            footer = shiny::modalButton("OK"),
            easyClose = TRUE
          )
        )
      }
    }

    if (input$segments == 2) {
      if (data_in[2] == T) {

        cur_dir(paste(user_dir(), "/05%_interval", sep = ""))

        p05 <- paste(cur_dir(), "/05%_interval/Measurements_05.xlsx", sep = "")
        p05.1 <- paste(cur_dir(), "/05%_interval/Measurements_05_1.xlsx", sep = "")

        shiny::showModal(
          shiny::modalDialog(
            title = "Dataframe already exist in directory",
            paste("Dataframe imported: 5% interval", sep = ""),
            footer = shiny::modalButton("OK"),
            easyClose = TRUE
          )
        )

        # Display measurements table

        output$mTable <- DT::renderDataTable({
          measurements <- readxl::read_xlsx(path = p05.1, col_names = T)

          DT::datatable(measurements)
        })
      } else {
        shiny::showModal(
          shiny::modalDialog(
            title = "Dataframe not exist in directory",
            paste("Create a 5% interval dataframe before", sep = ""),
            footer = shiny::modalButton("OK"),
            easyClose = TRUE
          )
        )
      }
    }
  })

  #########################
  # Render the image plot #
  #########################

  shiny::observeEvent(input$file, {
    # Store the image name
    newdata_10$ID = input$ImageID
    newdata_05$ID = input$ImageID

    shiny::req(input$file)

    if (!is.null(input$file) && !is.null(input$file$datapath)) {
      img1 <- imager::load.image(input$file$datapath) %>%
        imager::grabRect(output = "im")
    }

    output$imagePlot <- shiny::renderPlot({
      img1 %>%
        graphics::plot(x = img1, main = input$file$name)

      # Add red dots for length points
      if (!is.null(measured_animals$length_measurement) &&
          nrow(measured_animals$length_measurement <= 3)) {
        graphics::points(
          measured_animals$length_measurement$Length_X,
          measured_animals$length_measurement$Length_Y,
          col = "red",
          cex = 1.5
        )
      }

      # Connect length points and draw the line passing through the 3 points
      if (nrow(measured_animals$length_measurement) == 3 &&
          input$segments == 1) {
        x <- measured_animals$length_measurement$Length_X
        y <- measured_animals$length_measurement$Length_Y

        # Draw the line passing through the 3 points
        graphics::segments(x[1], y[1], x[2], y[2], col = "red")
        graphics::segments(x[2], y[2], x[3], y[3], col = "red")

        # Calculate the perpendicular direction
        dx <- x[3] - x[1]
        dy <- y[3] - y[1]
        perpendicular_direction <- c(dy, -dx) / sqrt(dx ^ 2 + dy ^ 2)

        # Calculate the length as the sum of distances between the points
        length_pixels <- sum(sqrt(diff(x) ^ 2 + diff(y) ^ 2))

        # Draw parallel lines at 10% intervals
        for (i in 1:9) {
          length_fraction <- i / 10
          x_parallel <- x[1] + (x[3] - x[1]) * length_fraction
          y_parallel <- y[1] + (y[3] - y[1]) * length_fraction
          line_length <- length_pixels * length_fraction * 2  # Adjust the length of the parallel lines

          # Calculate the coordinates of the parallel lines
          x1 <- x_parallel - line_length * perpendicular_direction[1]
          y1 <- y_parallel - line_length * perpendicular_direction[2]
          x2 <- x_parallel + line_length * perpendicular_direction[1]
          y2 <- y_parallel + line_length * perpendicular_direction[2]

          graphics::segments(x1, y1, x2, y2, col = "blue", lty = "dashed")
        }

        # Add yellow dots for width measurements
        if (!is.null(measured_animals$width_measurements)
            && nrow(measured_animals$length_measurement == 3)) {
          graphics::points(
            measured_animals$width_measurements$Width_X,
            measured_animals$width_measurements$Width_Y,
            col = "yellow",
            pch = 4,
            cex = 2
          )
        }

        if (input$segments == 1 &&
            nrow(measured_animals$width_measurements == 21)) {
          x <- measured_animals$width_measurements$Width_X
          y <- measured_animals$width_measurements$Width_Y

          # Draw the line passing through fluke width
          graphics::segments(x[20], y[20], x[21], y[21], col = "green")
        }
      }
      # Connect length points and draw the line passing through the 3 points
      if (nrow(measured_animals$length_measurement) == 3 &&
          input$segments == 2) {
        x <- measured_animals$length_measurement$Length_X
        y <- measured_animals$length_measurement$Length_Y

        # Draw the line passing through the 3 points
        graphics::segments(x[1], y[1], x[2], y[2], col = "red")
        graphics::segments(x[2], y[2], x[3], y[3], col = "red")

        # Calculate the perpendicular direction
        dx <- x[3] - x[1]
        dy <- y[3] - y[1]
        perpendicular_direction <- c(dy, -dx) / sqrt(dx ^ 2 + dy ^ 2)

        # Calculate the length as the sum of distances between the points
        length_pixels <- sum(sqrt(diff(x) ^ 2 + diff(y) ^ 2))

        # Draw parallel lines at 10% intervals
        for (i in 1:19) {
          length_fraction <- i / 20
          x_parallel <- x[1] + (x[3] - x[1]) * length_fraction
          y_parallel <- y[1] + (y[3] - y[1]) * length_fraction
          line_length <- length_pixels * length_fraction * 2  # Adjust the length of the parallel lines

          # Calculate the coordinates of the parallel lines
          x1 <- x_parallel - line_length * perpendicular_direction[1]
          y1 <- y_parallel - line_length * perpendicular_direction[2]
          x2 <- x_parallel + line_length * perpendicular_direction[1]
          y2 <- y_parallel + line_length * perpendicular_direction[2]

          graphics::segments(x1, y1, x2, y2, col = "blue", lty = "dashed")
        }

        # Add yellow dots for width measurements
        if (!is.null(measured_animals$width_measurements)
            && nrow(measured_animals$length_measurement == 3)) {
          graphics::points(
            measured_animals$width_measurements$Width_X,
            measured_animals$width_measurements$Width_Y,
            col = "yellow",
            pch = 4,
            cex = 2
          )
        }

        if (input$segments == 2 &&
            nrow(measured_animals$width_measurements == 41)) {
          x <- measured_animals$width_measurements$Width_X
          y <- measured_animals$width_measurements$Width_Y

          # Draw the line passing through fluke width
          graphics::segments(x[40], y[40], x[41], y[41], col = "green")

        }
      }
    })
  })

  # shiny::observeEvent(input$crop, {
  #
  #   if (!is.null(input$file) && !is.null(input$file$datapath)) {
  #     img1 <- imager::load.image(input$file$datapath) %>%
  #       imager::grabRect(output = "im")
  #   }
  #
  #   output$output2 = shiny::renderImage({
  #     bbox <- c(input$crop$xmin = {input$crop$xmin}
  #               input$crop$ymin = {input$crop$ymin}
  #               input$crop$xmax = {input$crop$xmax}
  #               input$crop$ymax = {input$crop$ymax}
  #     )
  #   })
  # })

  #########################################
  # Capture length and width measurements #
  #########################################

  shiny::observeEvent(input$plot_click, {
    ##########################
    # Calculating plot cords #
    ##########################

    if (!is.null(input$file) &&
        nrow(measured_animals$length_measurement) < 3) {
      x <- input$plot_click$x
      y <- input$plot_click$y

      measured_animals$length_measurement <- rbind(measured_animals$length_measurement,
                                                   data.frame(Length_X = x, Length_Y = y))

      length_measurement <- sum(sqrt(
        diff(measured_animals$length_measurement$Length_X) ^ 2 + diff(measured_animals$length_measurement$Length_Y) ^
          2
      ))

      newdata_10$BL = length_measurement
      newdata_05$BL = length_measurement

      if (nrow(measured_animals$length_measurement) == 3) {
        shiny::showModal(
          shiny::modalDialog(
            title = "Measure Length",
            "Length measurements completed. Take the widths.",
            footer = shiny::modalButton("OK"),
            easyClose = TRUE
          )
        )
      }
    }

    if (!is.null(input$file) &&
        nrow(measured_animals$length_measurement) == 3 &&
        input$segments == 1) {
      if (nrow(measured_animals$width_measurements) <= 21) {
        x1 <- input$plot_click$x
        y1 <- input$plot_click$y

        measured_animals$width_measurements <- rbind(measured_animals$width_measurements,
                                                     data.frame(Width_X = x1, Width_Y = y1))

        if (nrow(measured_animals$width_measurements) == 19) {
          shiny::showModal(
            shiny::modalDialog(
              title = "Width measurements completed",
              "Take Fluke width.",
              footer = shiny::modalButton("OK"),
              easyClose = TRUE
            )
          )
        }

        if (nrow(measured_animals$width_measurements) == 21) {
          shiny::showModal(
            shiny::modalDialog(
              title = "Fluke width",
              "Fluke width measurements taken",
              footer = shiny::modalButton("OK"),
              easyClose = TRUE
            )
          )

          ##########################################
          # Assigning values to reactive variables #
          ##########################################

          coord1 <- measured_animals$width_measurements[seq(2, nrow(measured_animals$width_measurements), by = 2), ]


          coord2 <- measured_animals$width_measurements[seq(3, nrow(measured_animals$width_measurements), by = 2), ]

          distances <- numeric(nrow(coord1)) / 2

          for (i in 1:nrow(measured_animals$width_measurements)) {
            x1 <- coord1$Width_X[i]
            y1 <- coord1$Width_Y[i]

            x2 <- coord2$Width_X[i]
            y2 <- coord2$Width_Y[i]

            # Calculate the distance using the Euclidean distance formula
            distance <- sqrt((x2 - x1) ^ 2 + (y2 - y1) ^ 2)

            distances[i] <- distance

          }

          width_measurements <- stats::na.omit(distances)

          # Store user input
          newdata_10$F_Alt = input$alt
          newdata_10$TO_Alt = input$takeof
          newdata_10$Date = input$Date

          newdata_10$fw = width_measurements[10]
          newdata_10$width90 = width_measurements[9]
          newdata_10$width80 = width_measurements[8]
          newdata_10$width70 = width_measurements[7]
          newdata_10$width60 = width_measurements[6]
          newdata_10$width50 = width_measurements[5]
          newdata_10$width40 = width_measurements[4]
          newdata_10$width30 = width_measurements[3]
          newdata_10$width20 = width_measurements[2]
          newdata_10$width10 = width_measurements[1]
          newdata_10$Calti = newdata_10$F_Alt + newdata_10$TO_Alt
        }
      }
    }

    if (!is.null(input$file) &&
        nrow(measured_animals$length_measurement) == 3 &&
        input$segments == 2) {
      if (nrow(measured_animals$width_measurements) <= 41) {
        x1 <- input$plot_click$x
        y1 <- input$plot_click$y

        measured_animals$width_measurements <- rbind(measured_animals$width_measurements,
                                                     data.frame(Width_X = x1, Width_Y = y1))

        if (nrow(measured_animals$width_measurements) == 39) {
          shiny::showModal(
            shiny::modalDialog(
              title = "Width measurements completed",
              "Take Fluke width.",
              footer = shiny::modalButton("OK"),
              easyClose = TRUE
            )
          )
        }

        if (nrow(measured_animals$width_measurements) == 41) {
          shiny::showModal(
            shiny::modalDialog(
              title = "Fluke width",
              "Fluke width measurements taken",
              footer = shiny::modalButton("OK"),
              easyClose = TRUE
            )
          )

          ##########################################
          # Assigning values to reactive variables #
          ##########################################

          coord1 <- measured_animals$width_measurements[seq(2, nrow(measured_animals$width_measurements), by = 2), ]


          coord2 <- measured_animals$width_measurements[seq(3, nrow(measured_animals$width_measurements), by = 2), ]

          distances <- numeric(nrow(coord1)) / 2

          for (i in 1:nrow(measured_animals$width_measurements)) {
            x1 <- coord1$Width_X[i]
            y1 <- coord1$Width_Y[i]

            x2 <- coord2$Width_X[i]
            y2 <- coord2$Width_Y[i]

            # Calculate the distance using the Euclidean distance formula
            distance <- sqrt((x2 - x1) ^ 2 + (y2 - y1) ^ 2)

            distances[i] <- distance

          }

          width_measurements <- stats::na.omit(distances)

          # Store user input
          newdata_05$F_Alt = input$alt
          newdata_05$TO_Alt = input$takeof
          newdata_05$Date = input$Date

          newdata_05$fw = width_measurements[20]
          newdata_05$width95 = width_measurements[19]
          newdata_05$width90 = width_measurements[18]
          newdata_05$width85 = width_measurements[17]
          newdata_05$width80 = width_measurements[16]
          newdata_05$width75 = width_measurements[15]
          newdata_05$width70 = width_measurements[14]
          newdata_05$width65 = width_measurements[13]
          newdata_05$width60 = width_measurements[12]
          newdata_05$width55 = width_measurements[11]
          newdata_05$width50 = width_measurements[10]
          newdata_05$width45 = width_measurements[9]
          newdata_05$width40 = width_measurements[8]
          newdata_05$width35 = width_measurements[7]
          newdata_05$width30 = width_measurements[6]
          newdata_05$width25 = width_measurements[5]
          newdata_05$width20 = width_measurements[4]
          newdata_05$width15 = width_measurements[3]
          newdata_05$width10 = width_measurements[2]
          newdata_05$width05 = width_measurements[1]
          newdata_05$Calti = newdata_05$F_Alt + newdata_05$TO_Alt
        }
      }
    }
  })

  shiny::observeEvent(input$score, {
    if (input$score == 1) {
      newdata_10$score = "Good"
      newdata_05$score = "Good"
    }

    if (input$score == 2) {
      newdata_10$score = "Moderate"
      newdata_05$score = "Moderate"
    }

    if (input$score == 3) {
      newdata_10$score = "Bad"
      newdata_05$score = "Bad"
    }

    if (input$score == 4) {
      newdata_10$score = "Not assingned"
      newdata_05$score = "Not assingned"
    }
  })

  shiny::observeEvent(input$saveBtn, {
    if (input$segments == 1) {
      nd <- data.frame(
        "Drone" = input$drone,
        "Obs" = input$obs,
        "Species" = input$Species,
        "Date" = newdata_10$Date,
        "Measured_Date" = as.character(Sys.time()),
        "ID" = newdata_10$ID,
        "Frame_Score" = newdata_10$score,
        "F_Alt" = newdata_10$F_Alt,
        "TO_Alt" = newdata_10$TO_Alt,
        "C_Alt" = newdata_10$Calti,
        "BL" = newdata_10$BL,
        "WD_10" = newdata_10$width10,
        "WD_20" = newdata_10$width20,
        "WD_30" = newdata_10$width30,
        "WD_40" = newdata_10$width40,
        "WD_50" = newdata_10$width50,
        "WD_60" = newdata_10$width60,
        "WD_70" = newdata_10$width70,
        "WD_80" = newdata_10$width80,
        "WD_90" = newdata_10$width90,
        "WD_F" = newdata_10$fw,
        "cGSD" = 0,
        "EstLength" = 0,
        "Comments" = input$comments
      )

      # Specify the absolute path to save the file

      p10 <- paste(cur_dir(), "/Measurements_10.xlsx", sep = "")

      measurements <- shiny::reactiveFileReader(
        session = session,
        intervalMillis = 1000,
        filePath = p10,
        readFunc = readxl::read_excel
      )

      measurements <- measurements()

      measurements[nrow(measurements) + 1, ] = nd

      writexl::write_xlsx(measurements, path = p10)

      writexl::write_xlsx(dtfilter(measurements),
                          path = paste(cur_dir(), "/Measurements_10_1.xlsx", sep = ""))

      shiny::showModal(
        shiny::modalDialog(
          title = "Saved",
          "Measurements are saved",
          footer = shiny::modalButton("OK"),
          easyClose = TRUE
        )
      )

      shinycssloaders::showSpinner("mTable", {
        slow_function()
      })

      # Display measurements table
      output$mTable <- DT::renderDataTable({
        DT::datatable(dtfilter(measurements))

      })
    }

    if (input$segments == 2) {
      nd <- data.frame(
        "Drone" = input$drone,
        "Obs" = input$obs,
        "Species" = input$Species,
        "Date" = newdata_05$Date,
        "Measured_Date" = as.character(Sys.time()),
        "ID" = newdata_05$ID,
        "Frame_Score" = newdata_05$score,
        "F_Alt" = newdata_05$F_Alt,
        "TO_Alt" = newdata_05$TO_Alt,
        "C_Alt" = newdata_05$Calti,
        "BL" = newdata_05$BL,
        "WD_05" = newdata_05$width05,
        "WD_10" = newdata_05$width10,
        "WD_15" = newdata_05$width15,
        "WD_20" = newdata_05$width20,
        "WD_25" = newdata_05$width25,
        "WD_30" = newdata_05$width30,
        "WD_35" = newdata_05$width35,
        "WD_40" = newdata_05$width40,
        "WD_45" = newdata_05$width45,
        "WD_50" = newdata_05$width50,
        "WD_55" = newdata_05$width55,
        "WD_60" = newdata_05$width60,
        "WD_65" = newdata_05$width65,
        "WD_70" = newdata_05$width70,
        "WD_75" = newdata_05$width75,
        "WD_80" = newdata_05$width80,
        "WD_85" = newdata_05$width85,
        "WD_90" = newdata_05$width90,
        "WD_95" = newdata_05$width95,
        "WD_F" = newdata_05$fw,
        "cGSD" = 0,
        "EstLength" = 0,
        "Comments" = input$comments
      )

      # Specify the absolute path to save the file

      p05 <- paste(cur_dir(), "/Measurements_05.xlsx", sep = "")

      measurements <- shiny::reactiveFileReader(
        session = session,
        intervalMillis = 1000,
        filePath = p05,
        readFunc = readxl::read_excel
      )

      measurements <- measurements()

      measurements[nrow(measurements) + 1, ] = nd

      writexl::write_xlsx(measurements, path = p05)

      writexl::write_xlsx(dtfilter(measurements),
                          path = paste(cur_dir(), "/Measurements_05_1.xlsx", sep = ""))

      shiny::showModal(
        shiny::modalDialog(
          title = "Saved",
          "Measurements are saved",
          footer = shiny::modalButton("OK"),
          easyClose = TRUE
        )
      )

      shinycssloaders::showSpinner("mTable", {
        slow_function()
      })

      # Display measurements table
      output$mTable <- DT::renderDataTable({
        DT::datatable(dtfilter(measurements))

      })
    }
  })

  # Clear measurements
  shiny::observeEvent(input$clearBtn, {
    measured_animals$length_measurement <- data.frame()
    measured_animals$width_measurements <- data.frame()
    shiny::updateTextInput(inputId = "comments", value = "")
    shiny::updateTextInput(ImageId = "ImageId", value = "")
    shiny::updateRadioButtons(inputId = "score", selected = 4)
    shiny::updateNumericInput(inputId = "alt", value = 20)

  })

  ###########################################
  # Adding the model calibration data frame #
  ###########################################

  shiny::observeEvent(input$calib, {

    calib_path <- input$calib_path

    mdata <- calib(calib_path)

    mdata <- stats::na.omit(mdata)

    m1 <- stats::lm(eGSD ~ as.numeric(C_Alt), data = mdata)

    train.control <- caret::trainControl(method = "repeatedcv",
                                         number = 10,
                                         repeats = 5)
    # Train the model
    model1 <- caret::train(
      eGSD ~ as.numeric(C_Alt),
      data = mdata,
      method = "lm",
      trControl = train.control
    )

    mdata$cGSD <- stats::predict(model1, mdata)

    mdata$LcGSD <- mdata$Pixel * mdata$cGSD

    output$mplot <- shiny::renderPlot({

      ggplot2::ggplot(mdata,
                      ggplot2::aes(
                        x = as.numeric(C_Alt),
                        y = eGSD)) +
        ggplot2::geom_point() +
        ggplot2::geom_smooth(method = "lm",
                             se = T) +
        ggplot2::theme_classic()
      })

    output$checkm <- shiny::renderPlot({

      performance::check_model(
        m1, check = c("linearity", "qq",
                      "homogeneity", "outliers")
        )
      })

    output$meanplot <- shiny::renderPlot({
      mdata <- stats::na.omit(mdata)

      pd <- ggplot2::position_dodge(0.1)

      ggplot2::ggplot(mdata,
                            ggplot2::aes(
                              x = as.numeric(C_Alt),
                              y = LcGSD)) +
        ggplot2::stat_summary(
          fun.data = "mean_sdl",
          fun.args = list(mult = 1),
          geom = "errorbar",
          color = "black",
          width = 0.1
        ) +
        ggplot2::stat_summary(fun = mean,
                              geom = "point",
                              color = "black") +
        ggplot2::theme_classic(base_family = "serif",
                               base_size = 14) +
        ggplot2::ylab("Measurements (meters)") +
        ggplot2::xlab("Altitude (m)") +
        ggplot2::ggtitle(
          "Mean and RMSE values estimated",
          subtitle = paste(
            "RMSE = ",
            round(model1$results$RMSE, digits = 3),
            "R² = ",
            round(model1$results$Rsquared, digits = 3),
            "MAE = ",
            round(model1$results$MAE, digits = 3)
          )
        ) +
        ggplot2::geom_hline(
          ggplot2::aes(
            yintercept = ObjLength / 100,
            linetype = paste("Reference object ",
                             ObjLength, " centimeters")
          ),
          colour = 'blue',
          linewidth = 1
        ) +
        ggplot2::geom_hline(
          ggplot2::aes(yintercept = mean(LcGSD),
                       linetype = "Mean estimated length"),
          colour = 'red',
          linewidth = 1
        ) +
        ggplot2::scale_linetype_manual(
          name = "Reference object",
          values = c(2, 2),
          guide = ggplot2::guide_legend(override.aes =
                                          list(color = c("red", "blue")))
        ) +
        ggplot2::theme(
          axis.text = ggplot2::element_text(size = 14),
          legend.title = ggplot2::element_text(size = 14),
          legend.position = "top"
        )
      })

    if(input$save_plot == "Y"){

      p1 <- ggplot2::ggplot(mdata,
                            ggplot2::aes(
                              x = as.numeric(C_Alt),
                              y = eGSD)) +
        ggplot2::geom_point() +
        ggplot2::geom_smooth(method = "lm",
                             se = T) +
        ggplot2::theme_classic()

      p2 <- performance::check_model(
        m1, check = c("linearity", "qq",
                      "homogeneity", "outliers"),
        panel = T
      )
      setwd(cur_dir())
      png("Diagnostic_plot.png",
          width = 720, height = 480,
          units = "px")
      print(p2)
      dev.off()
      setwd(user_dir())

      p3 <- ggplot2::ggplot(mdata,
                            ggplot2::aes(
                              x = as.numeric(C_Alt),
                              y = LcGSD)) +
        ggplot2::stat_summary(
          fun.data = "mean_sdl",
          fun.args = list(mult = 1),
          geom = "errorbar",
          color = "black",
          width = 0.1
        ) +
        ggplot2::stat_summary(fun = mean,
                              geom = "point",
                              color = "black") +
        ggplot2::theme_classic(base_family = "serif",
                               base_size = 14) +
        ggplot2::ylab("Measurements (meters)") +
        ggplot2::xlab("Altitude (m)") +
        ggplot2::ggtitle(
          "Mean and RMSE values estimated",
          subtitle = paste(
            "RMSE = ",
            round(model1$results$RMSE, digits = 3),
            "R² = ",
            round(model1$results$Rsquared, digits = 3),
            "MAE = ",
            round(model1$results$MAE, digits = 3)
          )
        ) +
        ggplot2::geom_hline(
          ggplot2::aes(
            yintercept = ObjLength / 100,
            linetype = paste("Reference object ",
                             ObjLength, " centimeters")
          ),
          colour = 'blue',
          linewidth = 1
        ) +
        ggplot2::geom_hline(
          ggplot2::aes(yintercept = mean(LcGSD),
                       linetype = "Mean estimated length"),
          colour = 'red',
          linewidth = 1
        ) +
        ggplot2::scale_linetype_manual(
          name = "Reference object",
          values = c(2, 2),
          guide = ggplot2::guide_legend(override.aes =
                                          list(color = c("red", "blue")))
        ) +
        ggplot2::theme(
          axis.text = ggplot2::element_text(size = 14),
          legend.title = ggplot2::element_text(size = 14),
          legend.position = "top"
        )

      ggplot2::ggsave(filename = "Regression_plot.png",
                      plot = p1,
                      path = cur_dir(),
                      width = 720, height = 480,
                      units = "px")

      ggplot2::ggsave(filename = "Variance_plot.png",
                      plot = p3,
                      path = cur_dir(),
                      width = 720, height = 480,
                      units = "px")
    }

    file <- ifelse(input$segments == 1,
                   "Measurements_10_1.xlsx",
                   "Measurements_05_1.xlsx")

    if (file %in% list.files(path = cur_dir())) {
      m1 <- stats::lm(eGSD ~ as.numeric(C_Alt), data = mdata)

      file_path <- paste(cur_dir(), file, sep = "/")

      measurements <- readxl::read_xlsx(path = file_path, col_names = T)

      measurements$cGSD <- stats::predict(m1, measurements)
      measurements$EstLength <- measurements$cGSD * measurements$Pixel

      measurements[, 11] <- round(measurements[, 11], digits = 4)
      measurements[, 12] <- round(measurements[, 12], digits = 2)
      measurements[, 15] <- round(measurements[, 15], digits = 2)

      writexl::write_xlsx(measurements, path = paste(
        cur_dir(),
        ifelse(
          input$segments == 1,
          "/Measurements_10_1.xlsx",
          "/Measurements_05_1.xlsx"
        ),
        sep = ""
      ))

      # Display measurements table
      output$mTable <- DT::renderDataTable({
        file_path <- paste(
          cur_dir(),
          ifelse(
            input$segments == 1,
            "/Measurements_10_1.xlsx",
            "/Measurements_05_1.xlsx"
          ),
          sep = ""
        )

        DT::datatable(measurements)
      })
    }
  })

  ########################
  # Whales measured plot #
  ########################

  shiny::observeEvent(input$n_whales, {
    if (input$n_whales != 0) {
      file_path <- paste(
        cur_dir(),
        ifelse(
          input$segments == 1,
          "/Measurements_10_1.xlsx",
          "/Measurements_05_1.xlsx"
        ),
        sep = ""
      )

      measurements <- readxl::read_excel(path = file_path, col_names = T)

      c <- levels(as.factor(measurements$ID))

      c1 <- sum(measurements$ID %in% c[1:input$n_whales])

      output$lplot <- shiny::renderPlot({
        ggplot2::ggplot(measurements[1:c1, ],
                        ggplot2::aes(
                          x = Segments,
                          y = Pixel,
                          fill = ID
                        )) +
          ggplot2::geom_col(position = ggplot2::position_dodge(width = 0.8),
                            show.legend = T) +
          ggplot2::theme_classic()

      })

      output$mwhale <- shiny::renderPlot({
        ggplot2::ggplot(measurements[1:c1, ],
                        ggplot2::aes(
                          x = Segments,
                          y = EstLength,
                          fill = ID
                        )) +
          ggplot2::geom_col(position = ggplot2::position_dodge(width = 0.8),
                            show.legend = T) +
          ggplot2::theme_bw()

      })
    }
  })

  ################
  # Close button #
  ################

  shiny::observeEvent(input$closeBtn, {
    shiny::stopApp()
  })
}
