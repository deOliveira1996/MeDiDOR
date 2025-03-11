####
# Functions
####
#' @importFrom caret trainControl train
#' @importFrom DT renderDataTable datatable
#' @importFrom ggplot2 ggplot aes geom_point geom_smooth theme_classic position_dodge stat_summary ylab xlab ggtitle geom_hline scale_linetype_manual guide_legend theme element_text geom_col theme_bw
#' @importFrom graphics plot points segments
#' @importFrom htmltools tagList
#' @importFrom performance check_model
#' @importFrom readxl read_xlsx read_excel
#' @importFrom shiny reactiveValues observeEvent showModal modalDialog textInput actionButton modalButton reactiveVal removeModal req renderPlot reactiveFileReader updateTextInput updateRadioButtons updateNumericInput stopApp
#' @importFrom shinycssloaders showSpinner
#' @importFrom stats na.omit lm predict
#' @importFrom writexl write_xlsx
#' @importFrom tidyverse %>%
#' @export

options(shiny.maxRequestSize = 50 * 1024^2)

# Define server
server <- function(input, output, session) {
  # Reactive values ----
  rv <- shiny::reactiveValues(
    # Controle de segmentos
    segments = NULL,

    # Medições
    length_measurements = data.frame(Length_X = numeric(),
                                     Length_Y = numeric()),
    width_measurements = data.frame(Width_X = numeric(),
                                    Width_Y = numeric()),
    fw_measurements = data.frame(Width_X = numeric(),
                                 Width_Y = numeric()),

    # Dados principais
    main_data = NULL,

    # Diretórios
    user_dir = getwd(),
    current_dir = getwd(),

    # Novos dados
    new_id = character(),
    new_score = "Not assigned",
    new_date = character(),
    new_f_alt = numeric(),
    new_to_alt = numeric(),
    new_calti = numeric(),
    new_bl = numeric(),
    new_widths = numeric(),
    new_fw = numeric(),

    # Imagem
    current_image = NULL,
    crop_status = FALSE,

    # Dimensões e ranges
    img_width = 0,
    img_height = 0,
    plot_ranges_x = NULL,
    plot_ranges_y = NULL,

    # Calibração
    calib_path = NULL,
    calib_data = NULL,
    calib_model = NULL
  )

  observeEvent(input$segments, {
    rv$segments <- input$segments
  })

  # Helper functions
  handle_fluke_measurements <- function(x , y) {

    if(nrow(rv$fw_measurements) != 2) {
      rv$fw_measurements <- rbind(rv$fw_measurements,
                                  data.frame(Width_X = x, Width_Y = y))
    }

    if (nrow(rv$fw_measurements) == 2) {
      show_measurement_modal("Measurements complete")
      # process_measurements()
    }
  }

  handle_width_measurement <- function(x, y) {
    target <- ifelse(input$segments == 1, 18, 38)

    # Adiciona pontos apenas se ainda não atingiu o target
    if (nrow(rv$width_measurements) != target) {
      rv$width_measurements <- rbind(rv$width_measurements,
                                     data.frame(Width_X = x, Width_Y = y))
    }

    # Lógica para transição entre width e fw measurements
    if (nrow(rv$width_measurements) == target) {
      show_measurement_modal("Width measurements completed. Take Fluke width.")
      handle_fluke_measurements(x, y)
    }
  }

  process_measurements <- function() {
    coords <- rv$width_measurements
    pairs <- data.frame(
      x1 = coords$Width_X[seq(2, nrow(coords), 2)],
      y1 = coords$Width_Y[seq(2, nrow(coords), 2)],
      x2 = coords$Width_X[seq(3, nrow(coords), 2)],
      y2 = coords$Width_Y[seq(3, nrow(coords), 2)]
    )

    widths <- sqrt((pairs$x2 - pairs$x1)^2 + (pairs$y2 - pairs$y1)^2)
    rv$new_widths <- widths[1:(length(widths))]
    rv$new_fw <- sqrt((rv$fw_measurements$Width_X[2] -rv$fw_measurements$Width_X[1])^2 + (rv$fw_measurements$Width_Y[2] - rv$fw_measurements$Width_X[1])^2)
    rv$new_id = input$ImageID
    rv$new_date = input$Date
    rv$new_f_alt = input$alt
    rv$new_to_alt = input$takeof
    rv$new_calti = input$alt + input$takeof
    rv$new_bl = sum(sqrt(
      diff(rv$length_measurements$Length_X)^2 +
        diff(rv$length_measurements$Length_Y)^2
    ))
  }

  create_new_entry <- function(interval) {
    if (interval == 10) {
      data.frame(
        Drone = input$drone,
        Obs = input$obs,
        Species = input$Species,
        rv$newdata$metadata,
        WD_10 = rv$new_widths[1],
        WD_20 = rv$new_widths[2],
        WD_30 = rv$new_widths[3],
        WD_40 = rv$new_widths[4],
        WD_50 = rv$new_widths[5],
        WD_60 = rv$new_widths[6],
        WD_70 = rv$new_widths[7],
        WD_80 = rv$new_widths[8],
        WD_90 = rv$new_widths[9],
        WD_F = rv$new_fw,
        cGSD = 0,
        EstLength = 0,
        Comments = input$comments
      )
    } else {
      data.frame(
        Drone = input$drone,
        Obs = input$obs,
        Species = input$Species,
        rv$newdata$metadata,
        WD_05 = rv$new_widths[1],
        WD_10 = rv$new_widths[2],
        WD_15 = rv$new_widths[3],
        WD_20 = rv$new_widths[4],
        WD_25 = rv$new_widths[5],
        WD_30 = rv$new_widths[6],
        WD_35 = rv$new_widths[7],
        WD_40 = rv$new_widths[8],
        WD_45 = rv$new_widths[9],
        WD_50 = rv$new_widths[10],
        WD_55 = rv$new_widths[11],
        WD_60 = rv$new_widths[12],
        WD_65 = rv$new_widths[13],
        WD_70 = rv$new_widths[14],
        WD_75 = rv$new_widths[15],
        WD_80 = rv$new_widths[16],
        WD_85 = rv$new_widths[17],
        WD_90 = rv$new_widths[18],
        WD_95 = rv$new_widths[19],
        WD_F = rv$new_fw,
        cGSD = 0,
        EstLength = 0,
        Comments = input$comments
      )
    }
  }

  # Measurement line drawer
  draw_measurement_lines <- function() {
    req(
      rv$current_image,
      rv$length_measurements,
      rv$segments
    )

    lp <- rv$length_measurements
    if (nrow(lp) < 3) return()

    start <- data.frame(x = lp$Length_X[1], y = lp$Length_Y[1])
    mid <- data.frame(x = lp$Length_X[2], y = lp$Length_Y[2])
    end <- data.frame(x = lp$Length_X[3], y = lp$Length_Y[3])

    # Desenha linha principal
    graphics::segments(start$x,
                       start$y,
                       mid$x,
                       mid$y,
                       col = "red",
                       lwd = 1.5)

    graphics::segments(mid$x,
                       mid$y,
                       end$x,
                       end$y,
                       col = "red",
                       lwd = 1.5)

    # Cálculo vetorial no sistema de coordenadas original
    original_dx <- end$x - start$x
    original_dy <- end$y - start$y
    perpendicular <- c(-original_dy, original_dx) / sqrt(original_dx^2 + original_dy^2)

    # Parâmetros dos intervalos
    intervals <- ifelse(input$segments == 1, 9, 19)

    total_length <- sum(sqrt(diff(lp$Length_X)^2 + diff(lp$Length_Y)^2))

    for (i in 1:intervals) {
      fraction <- i / (intervals + 1)

      # Ponto na linha original
      original_x <- start$x + original_dx * fraction
      original_y <- start$y + original_dy * fraction

      # Converte ponto central
      center <- data.frame(x = original_x, y = original_y)

      # Calcula extremidades da linha perpendicular
      line_length <- total_length * 2
      offset_x <- perpendicular[1] * line_length / 2
      offset_y <- perpendicular[2] * line_length / 2

      # Desenha linha perpendicular
      graphics::segments(
        center$x - offset_x,
        center$y - offset_y,
        center$x + offset_x,
        center$y + offset_y,
        col = "blue",
        lty = "dashed"
      )
    }
  }

  # Path  block

  # Directory handling ----
  shiny::observeEvent(input$path, {
    shiny::showModal(
      shiny::modalDialog(
        title = "Set Working Directory",
        shiny::textInput("wd", "Enter directory path:", value = getwd()),
        footer = tagList(
          shiny::actionButton("confirmBtn", "Confirm", class = "btn-primary"),
          shiny::modalButton("Cancel")
        )
      )
    )
  })

  # Confirmation  block

  shiny::observeEvent(input$confirmBtn, {
    req(input$wd)
    setwd(input$wd)
    rv$user_dir <- input$wd
    shiny::removeModal()
  })

  # Create  block

  # Data frame management
  shiny::observeEvent(input$create, {
    req(rv$user_dir, rv$segments)

    dir_path <- get_directory_path(rv$segments, rv$user_dir)
    paths <- get_file_paths(dir_path, rv$segments)

    if (!dir.exists(dir_path)) {
      dir.create(dir_path)
      create_data(
        segments = rv$segments,
        path = paths$main,
        path2 = paths$secondary
      )
      shiny::showModal(shiny::modalDialog(
        title = "Success",
        paste(
          ifelse(input$segments == 1, "10%", "5%"),
          "interval dataframe created"
        ),
        footer = shiny::modalButton("OK")
      ))

      rv$main_data <- readxl::read_xlsx(paths$secondary)

      output$mTable <- DT::renderDataTable({
        DT::datatable(readxl::read_xlsx(paths$secondary))
      })

    } else {
      shiny::showModal(
        modalDialog(
          title = "Info",
          "Dataframe already exists, please IMPORT instead",
          footer = shiny::modalButton("OK")
        )
      )
    }
  })

  # Import  block

  shiny::observeEvent(input$import, {
    req(rv$user_dir, rv$segments)
    dir_path <- get_directory_path(rv$segments, rv$user_dir)
    paths <- get_file_paths(dir_path, rv$segments)

    if (file.exists(paths$secondary)) {
      rv$main_data <- readxl::read_xlsx(paths$secondary)

      shiny::showModal(
        shiny::modalDialog(
          title = "Success",
          "Dataframe imported",
          footer = shiny::modalButton("OK")
        )
      )
      output$mTable <- DT::renderDataTable({
        DT::datatable(readxl::read_xlsx(paths$secondary))
      })

    } else {
      shiny::showModal(
        shiny::modalDialog(
          title = "Error",
          "Dataframe not found, please CREATE instead",
          footer = shiny::modalButton("OK")
        )
      )
    }
  })

  # File  block

  # Image processing ----
  shiny::observeEvent(input$file, {
    req(input$file)
    img <- imager::load.image(input$file$datapath)
    rv$current_image <- img
    rv$img_width <- width(img)
    rv$img_height <- height(img)
  })

  # Crop  block

  # Crop button logic
  shiny::observeEvent(input$crop, {
    req(input$file)
    req(input$plot_brush)

    # Set crop area

    rv$plot_ranges_x <- c(input$plot_brush$xmin, input$plot_brush$xmax)
    rv$plot_ranges_y <- c(input$plot_brush$ymax, input$plot_brush$ymin)

    # Update states
    rv$crop_status <- TRUE
    updateActionButton(session, "crop", disabled = TRUE)
  })

  # UI feedback for crop state
  output$crop_status <- shiny::renderUI({
    req(input$file)
    if (!rv$crop_status == TRUE) {
      div(
        class = "alert alert-info",
        "Step 1: Select the area of interest on the image using the selection tool and click 'Crop'"
      )
    } else {
      div(class = "alert alert-success",
          "Area selected! You can now mark the measurement points.")
    }
  })

  # Image plot  block

  output$imagePlot <- shiny::renderPlot({
    req(rv$current_image,
        rv$segments)

    if (rv$crop_status == TRUE) {

      plot(
        rv$current_image,
        xlim = rv$plot_ranges_x,
        ylim = rv$plot_ranges_y,
        main = input$file$name,
        axes = T
      )

      if (nrow(rv$length_measurements) > 0) {
        points_df <- rv$length_measurements %>%
          dplyr::mutate(
            x_plot = rv$length_measurements$Length_X,
            y_plot = rv$length_measurements$Length_Y
          )
        graphics::points(
          points_df$x_plot,
          points_df$y_plot,
          col = "red",
          cex = 1.5)
      }

      if (nrow(rv$width_measurements) > 0) {

        points_df <- rv$width_measurements %>%
          dplyr::mutate(
            x_plot = rv$width_measurements$Width_X,
            y_plot = rv$width_measurements$Width_Y
          )
        graphics::points(
          points_df$x_plot,
          points_df$y_plot,
          col = "yellow",
          pch = 4,
          cex = 2
        )
      }

      if (nrow(rv$fw_measurements) > 0) {

        points_df <- rv$fw_measurements %>%
          dplyr::mutate(
            x_plot = rv$fw_measurements$Width_X,
            y_plot = rv$fw_measurements$Width_Y
          )

        graphics::points(
          points_df$x_plot,
          points_df$y_plot,
          col = "darkgreen",
          pch = 4,
          cex = 2
        )

        graphics::segments(rv$fw_measurements$Width_X[1],
                           rv$fw_measurements$Width_Y[1],
                           rv$fw_measurements$Width_X[2],
                           rv$fw_measurements$Width_Y[2],
                           col = "lightgreen",
                           lwd = 1.5)
      }

      if (nrow(rv$length_measurements) == 3) {
        draw_measurement_lines()
      }
    } else {
      plot(rv$current_image, main = "Select area of interest", axes = T)
    }
  })
  # Plot click  block

  # Measurement handling ----
  shiny::observeEvent(input$plot_click, {
    req(input$crop,
        rv$crop_status)

    if (nrow(rv$length_measurements) != 3) {
      rv$length_measurements <- rbind(
        rv$length_measurements,
        data.frame(
          Length_X = input$plot_click$x,
          Length_Y = input$plot_click$y
        )
      )
      if (nrow(rv$length_measurements) == 3) {
        show_measurement_modal("Length measurements completed. Take widths.")
      }
    } else {
      handle_width_measurement(input$plot_click$x, input$plot_click$y)
    }
  })

  # Clear  block

  # Measurement Reset Warning
  shiny::observeEvent(input$clearBtn, {
    shiny::showModal(
      shiny::modalDialog(
        title = "Confirm Reset",
        "Are you sure you want to clear all current measurements?",
        footer = tagList(
          shiny::actionButton("confirm_reset", "Yes", class = "btn-danger"),
          shiny::modalButton("Cancel")
        )
      )
    )
  })

  shiny::observeEvent(input$confirm_reset, {
    rv$length_measurements <- data.frame()
    rv$width_measurements <- data.frame()
    # Novos dados
    rv$new_id = character()
    rv$new_score = "Not assigned"
    rv$new_date = character()
    rv$new_f_alt = numeric()
    rv$new_calti = numeric()
    rv$new_bl = numeric()
    rv$new_widths = numeric()
    rv$new_fw = numeric()
    rv$crop_status = FALSE
    shiny::updateActionButton(session, "crop", disabled = FALSE)
    shiny::updateTextInput(inputId = "comments", value = "")
    shiny::updateTextInput(inputId = "ImageId", value = "")
    shiny::updateRadioButtons(inputId = "score", selected = 4)
    shiny::updateNumericInput(inputId = "alt", value = 20)
    rv$plot_ranges_x = NULL
    rv$plot_ranges_y = NULL

    shiny::removeModal()
  })

  # Calibration  block

  # Refactored Calibration Handler
  shiny::observeEvent(input$calib, {
    req(input$calib_path)

    rv$calib_path <- input$calib_path

    rv$calib_data <- calib(rv$calib_path)

    calib <- stats::na.omit(rv$calib_data)

    m1 <- stats::lm(eGSD ~ as.numeric(C_Alt), data = calib)

    train.control <- caret::trainControl(method = "repeatedcv",
                                         number = 10,
                                         repeats = 5)
    # Train the model
    m1 <- caret::train(
      eGSD ~ as.numeric(C_Alt),
      data = calib,
      method = "lm",
      trControl = train.control
    )

    rv$calib_model <- m1

    calib$cGSD <- stats::predict(m1, calib)

    calib$LcGSD <- calib$Pixel * calib$cGSD

    rv$calib_data <- calib

    # Save plots if requested
    if (input$save_plot == "Y") {
      save_calibration_plots(create_calibration_plots(rv$calib_data,
                                                      rv$calib_model),
                             rv$current_dir)

    }

    # Update measurements data
    updated_measurements <- update_measurements(rv$calib_model,
                                                rv$segments)

    # Update data table if successful
    if (!is.null(updated_measurements)) {
      update_data_table(paths$secondary)
    }
  })

  # Model plot  block

  output$meanplot <- shiny::renderPlot({
    req(input$calib, rv$calib_data, rv$calib_model)

    ggplot2::ggplot(
      as.data.frame(rv$calib_data),
      ggplot2::aes(x = as.numeric(C_Alt), y = eGSD) +
        ggplot2::geom_point() +
        ggplot2::geom_smooth(method = "lm", se = TRUE) +
        ggplot2::theme_classic() +
        ggplot2::labs(x = "Altitude (m)", y = "eGSD")
    )
  })

  output$checkm <- shiny::renderPlot({
    req(input$calib, rv$calib_data, rv$calib_model)
    performance::check_model(rv$calib_model,
                             check = c("linearity", "qq", "homogeneity", "outliers"))
  })

  output$variance <- shiny::renderPlot({
    req(input$calib, rv$calib_data, rv$calib_model)
    ggplot2::ggplot(
      as.data.frame(rv$calib_data),
      ggplot2::aes(x = as.numeric(C_Alt), y = LcGSD) +
        ggplot2::stat_summary(
          fun.data = "mean_sdl",
          fun.args = list(mult = 1),
          geom = "errorbar",
          color = "black",
          width = 0.1
        ) +
        ggplot2::stat_summary(
          fun = mean,
          geom = "point",
          color = "black"
        ) +
        ggplot2::theme_classic(base_family = "serif", base_size = 14) +
        ggplot2::labs(x = "Altitude (m)", y = "Measurements (meters)") +
        ggplot2::ggtitle(
          "Mean and RMSE values estimated",
          subtitle = sprintf(
            "RMSE = %.3f, R² = %.3f, MAE = %.3f",
            rv$calib_model$results$RMSE,
            rv$calib_model$results$Rsquared,
            rv$calib_model$results$MAE
          )
        ) +
        ggplot2::geom_hline(
          ggplot2::aes(
            yintercept = unique(ObjLength) / 100,
            linetype = paste("Reference object", unique(ObjLength), "centimeters")
          ),
          colour = 'blue',
          linewidth = 1
        ) +
        ggplot2::geom_hline(
          ggplot2::aes(yintercept = mean(LcGSD)),
          colour = 'red',
          linewidth = 1
        ) +
        ggplot2::scale_linetype_manual(
          name = "Reference",
          values = c(2, 2),
          guide = ggplot2::guide_legend(override.aes = list(color = c("red", "blue")))
        )
    )
  })

  # Save  block

  # Save system ----
  shiny::observeEvent(input$saveBtn, {
    req(rv$current_dir, rv$segments)
    dir_path <- get_directory_path(input$segments, rv$current_dir)
    paths <- get_file_paths(dir_path, input$segments)

    # Calculate distances
    width_points <- rv$width_measurements
    distances <- sqrt(
      (
        rv$width_measurements$Width_X[seq(2, nrow(width_points), 2)] -
          rv$width_measurements$Width_X[seq(3, nrow(width_points), 2)]
      )^2 +
        (
          rv$width_measurements$Width_Y[seq(2, nrow(width_points), 2)] -
            rv$width_measurements$Width_Y[seq(3, nrow(width_points), 2)]
        )^2
    )

    # Create new data entry
    new_entry <- data.frame(
      Drone = input$drone,
      Obs = input$obs,
      Species = input$Species,
      Date = input$Date,
      Measured_Date = Sys.time(),
      ID = input$ImageID,
      Frame_Score = switch(
        input$score,
        "1" = "Good",
        "2" = "Moderate",
        "3" = "Bad",
        "Not assigned"
      ),
      F_Alt = input$alt,
      TO_Alt = input$takeof,
      C_Alt = input$alt + input$takeof,
      BL = sum(sqrt(
        diff(rv$length_measurementsLength_X)^2 +
          diff(rv$length_measurementsLength_Y)^2
      )),
      Comments = input$comments
    )

    # Add width measurements
    if (input$segments == 1) {
      new_entry[, 12:21] <- c(distances[1:9], distances[10])
    } else {
      new_entry[, 12:31] <- c(distances[1:19], distances[20])
    }

    # Save to file
    measurements <- readxl::read_xlsx(paths$secondary)
    measurements <- rbind(measurements, new_entry)

    rv$main_data <- measurements

    writexl::write_xlsx(measurements, paths$secondary)

    shiny::showModal(
      shiny::modalDialog(
        title = "Saved",
        "Measurements stored successfully",
        footer = shiny::modalButton("OK")
      )
    )

    update_data_table(paths$secondary)
  })

  # Observe actions

  shiny::observe({
    req(rv$current_image)

    if (nrow(rv$length_measurements) == 3) {
      shiny::showModal(
        shiny::modalDialog(
          title = "Measurement Progress",
          HTML(
            "<b>Length measurement complete!</b><br>
             Click along the perpendicular lines to measure widths."
          ),
          footer = shiny::modalButton("Continue"),
          easyClose = TRUE
        )
      )
    }
    # Disable crop button when measurements started
    if (nrow(rv$length_measurements) > 0) {
      updateActionButton(session, "crop", disabled = TRUE)
    } else {
      updateActionButton(session, "crop", disabled = FALSE)
    }
  })

  ################
  # Close button #
  ################

  shiny::observeEvent(input$closeBtn, {
    shiny::stopApp()
  })
}
