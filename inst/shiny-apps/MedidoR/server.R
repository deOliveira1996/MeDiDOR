
options(shiny.maxRequestSize = 50 * 1024^2)

# Define server
server <- function(input, output, session) {

  calib_log <- reactiveVal(data.frame(Timestamp = character(),
                                      Message = character()))

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
    current_data = NULL,
    main = NULL,
    secondary = NULL,
    dir_path = NULL,

    # Diretórios
    user_dir = getwd(),
    current_dir = getwd(),

    # Novos dados
    new_id = character(),
    new_score = character(),
    new_date = character(),
    new_f_alt = numeric(),
    new_to_alt = numeric(),
    new_calti = numeric(),
    new_bl = numeric(),
    new_widths = numeric(),
    new_fw = numeric(),
    new_drone = character(),

    # Imagem
    current_image = NULL,
    crop_status = FALSE,
    add_status = TRUE,
    click_save = FALSE,

    # Dimensões e ranges
    img_width = 0,
    img_height = 0,
    plot_ranges_x = NULL,
    plot_ranges_y = NULL,

    # Calibração
    calib_path = NULL,
    calib_data = NULL,
    calib_model = NULL,
    calib_train = NULL
  )

  ############################ Path  block ############################

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

  # Assign segments
  observeEvent(input$segments, {
    rv$segments <- input$segments
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

    dir_path <- MedidoR:::get_directory_path(rv$segments, rv$user_dir)
    paths <- MedidoR:::get_file_paths(dir_path, rv$segments)

    rv$main <- paths$main
    rv$secondary <- paths$secondary
    rv$dir_path <- dir_path

    if (!dir.exists(dir_path)) {
      dir.create(dir_path)
      MedidoR:::create_data(
        segments = rv$segments,
        path = rv$main,
        path2 = rv$secondary
      )
      shiny::showModal(shiny::modalDialog(
        title = "Success",
        paste(
          ifelse(rv$segments == 1, "10%", "5%"),
          "interval dataframe created"
        ),
        footer = shiny::modalButton("OK")
      ))

      rv$current_dir <- dir_path

      rv$main_data <- readxl::read_xlsx(path = rv$main, col_names = T)

      output$mTable <- DT::renderDataTable({
        MedidoR:::update_data_table(rv$secondary)
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

  shiny::observeEvent(input$score, {
    req(input$file)
    if (input$score == 1) {
      rv$new_score <- "Good"
    }
    if (input$score == 2) {
      rv$new_score <- "Moderate"
    }
    if (input$score == 3) {
      rv$new_score <- "Bad"
    }
    if (input$score == 4) {
      rv$new_score <- "Not assigned"
    }
  })

  # Import  block

  shiny::observeEvent(input$import, {
    req(rv$user_dir, rv$segments)
    dir_path <- MedidoR:::get_directory_path(rv$segments, rv$user_dir)
    paths <- MedidoR:::get_file_paths(dir_path, rv$segments)

    rv$main <- paths$main
    rv$secondary <- paths$secondary
    rv$dir_path <- dir_path

    if (file.exists(rv$secondary)) {
      rv$main_data <- readxl::read_xlsx(rv$main)

      shiny::showModal(
        shiny::modalDialog(
          title = "Success",
          "Dataframe imported",
          footer = shiny::modalButton("OK")
        )
      )

      output$mTable <- DT::renderDataTable({
        MedidoR:::update_data_table(rv$secondary)
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

  ############################ Image block ############################

  # Image processing ----
  shiny::observeEvent(input$file, {
    req(input$file)
    img <- imager::load.image(input$file$datapath)
    rv$current_image <- img
    rv$img_width <- imager::width(img)
    rv$img_height <- imager::height(img)
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
    shiny::updateActionButton(session, "crop", disabled = TRUE)
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
          "Area selected!")
    }
  })

  ############################ Image plot  block ############################

  # Measurement line drawer functions

  process_measurements <- function() {
    coords <- rv$width_measurements

    # Garantir que há um número par de pontos
    if (nrow(coords) %% 2 != 0) {
      showNotification("Odd number of stitches for widths!", type = "error")
      return()
    }

    # Criar pares consecutivos (1-2, 3-4, etc.)
    pairs <- data.frame(
      x1 = coords$Width_X[seq(1, nrow(coords) - 1, 2)],
      # Índices ímpares (1,3,5...)
      y1 = coords$Width_Y[seq(1, nrow(coords) - 1, 2)],
      x2 = coords$Width_X[seq(2, nrow(coords), 2)],
      # Índices pares (2,4,6...)
      y2 = coords$Width_Y[seq(2, nrow(coords), 2)]
    )

    widths <- sqrt((pairs$x2 - pairs$x1)^2 + (pairs$y2 - pairs$y1)^2)
    rv$new_widths <- widths[1:(length(widths))]
    rv$new_fw <- as.numeric(sqrt((rv$fw_measurements$Width_X[2] - rv$fw_measurements$Width_X[1])^2 + (rv$fw_measurements$Width_Y[2] - rv$fw_measurements$Width_Y[1])^2))
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

  #  Length measurements
  draw_measurement_lines <- function() {
    req(rv$current_image, rv$length_measurements, rv$segments)

    lp <- rv$length_measurements
    if (nrow(lp) < 3)
      return()

    start <- data.frame(x = lp$Length_X[1], y = lp$Length_Y[1])
    mid <- data.frame(x = lp$Length_X[2], y = lp$Length_Y[2])
    end <- data.frame(x = lp$Length_X[3], y = lp$Length_Y[3])

    # Main lines
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

    # Vectorial calculations
    original_dx <- end$x - start$x
    original_dy <- end$y - start$y
    perpendicular <- c(-original_dy, original_dx) / sqrt(original_dx^2 + original_dy^2)

    # Interval parameters
    intervals <- ifelse(rv$segments == 1, 9, 19)

    total_length <- sum(sqrt(diff(lp$Length_X)^2 + diff(lp$Length_Y)^2))

    for (i in 1:intervals) {
      fraction <- i / (intervals + 1)

      # Main line point
      original_x <- start$x + original_dx * fraction
      original_y <- start$y + original_dy * fraction

      # Central point convert
      center <- data.frame(x = original_x, y = original_y)

      # Calcs perpendicular summ
      line_length <- total_length * 2
      offset_x <- perpendicular[1] * line_length / 2
      offset_y <- perpendicular[2] * line_length / 2

      # Draw perpendicular lines
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

  # Width measurements
  handle_width_measurement <- function(x, y) {
    target <- ifelse(rv$segments == 1, 18, 38)

    if (nrow(rv$width_measurements) < target) {
      rv$width_measurements <- rbind(rv$width_measurements,
                                     data.frame(Width_X = x, Width_Y = y))
    }

    if (nrow(rv$width_measurements) == target) {
      MedidoR:::show_measurement_modal("Width measurements completed. Take Fluke width.")
    }
  }

  # Fluke measurements
  handle_fluke_measurements <- function(x, y) {
    if (nrow(rv$fw_measurements) < 2) {
      rv$fw_measurements <- rbind(rv$fw_measurements,
                                  data.frame(Width_X = x, Width_Y = y))
    }

    if (nrow(rv$fw_measurements) == 2) {
      MedidoR:::show_measurement_modal("Measurements complete")
      process_measurements()
    }
  }

  # Rendering plot
  output$imagePlot <- shiny::renderPlot({
    req(rv$current_image, rv$segments)

    if (rv$crop_status == TRUE) {
      plot(
        rv$current_image,
        xlim = rv$plot_ranges_x,
        ylim = rv$plot_ranges_y,
        main = input$file$name,
        axes = T
      )

      if (nrow(rv$length_measurements) > 0) {
        points_df <- rv$length_measurements |>
          dplyr::mutate(
            x_plot = rv$length_measurements$Length_X,
            y_plot = rv$length_measurements$Length_Y
          )
        graphics::points(points_df$x_plot,
                         points_df$y_plot,
                         col = "red",
                         cex = 1.5)
      }

      if (nrow(rv$width_measurements) > 0) {
        points_df <- rv$width_measurements |>
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
        points_df <- rv$fw_measurements |>
          dplyr::mutate(
            x_plot = rv$fw_measurements$Width_X,
            y_plot = rv$fw_measurements$Width_Y
          )

        graphics::points(
          points_df$x_plot,
          points_df$y_plot,
          col = "purple3",
          pch = 4,
          cex = 2
        )

        graphics::segments(
          rv$fw_measurements$Width_X[1],
          rv$fw_measurements$Width_Y[1],
          rv$fw_measurements$Width_X[2],
          rv$fw_measurements$Width_Y[2],
          col = "purple",
          lwd = 1.5
        )
      }

      if (nrow(rv$length_measurements) == 3) {
        draw_measurement_lines()
      }
    } else {
      plot(rv$current_image, main = "Select area of interest", axes = T)
    }
  })

  ############################ Plot click  block ############################
  # Measurement handling ----
  shiny::observeEvent(input$plot_click, {
    req(input$crop, rv$crop_status)

    if (nrow(rv$length_measurements) != 3) {
      rv$length_measurements <- rbind(
        rv$length_measurements,
        data.frame(
          Length_X = input$plot_click$x,
          Length_Y = input$plot_click$y
        )
      )
      if (nrow(rv$length_measurements) == 3) {
        MedidoR:::show_measurement_modal("Length measurements completed. Take widths.")
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
    } else {
      target <- ifelse(rv$segments == 1, 18, 38)
      if (nrow(rv$width_measurements) < target) {
        handle_width_measurement(input$plot_click$x, input$plot_click$y)
      } else {
        handle_fluke_measurements(input$plot_click$x, input$plot_click$y)
        rv$add_status <- FALSE
      }
    }
  })

  shiny::observe({
    req(input$file)

    if (nrow(rv$length_measurements) > 0) {
      shiny::updateActionButton(session, "crop", disabled = TRUE)
    } else {
      shiny::updateActionButton(session, "crop", disabled = FALSE)
    }

    if (rv$add_status == TRUE) {
      shiny::updateActionButton(session, "saveBtn", disabled = TRUE)
    } else {
      shiny::updateActionButton(session, "saveBtn", disabled = FALSE)
    }
  })

  ############################ Save  block ############################

  # Create New Entry function
  create_new_entry <- function(segments) {
    req(input$saveBtn)

    base_df <- data.frame(
      Drone = as.character(input$drone),
      Obs = as.character(input$obs),
      Species = as.character(input$Species),
      Date = as.character(rv$new_date),
      Measured_Date = format(as.POSIXct(Sys.time()),
                             "%Y-%m-%d %H:%M:%S"),
      ID = as.character(rv$new_id),
      Frame_Score = as.character(rv$new_score),
      F_Alt = as.numeric(rv$new_f_alt),
      TO_Alt = as.numeric(rv$new_to_alt),
      C_Alt = as.numeric(rv$new_calti),
      cGSD = NA_real_,
      EstLength = NA_real_,
      Comments = as.character(input$comments),
      BL = as.numeric(rv$new_bl)
    )

    # Width columns
    if (segments == 1) {
      widths <- setNames(as.list(rv$new_widths[1:9]),
                         paste0("WD_", seq(10, 90, by = 10)))
    } else {
      widths <- setNames(
        as.list(rv$new_widths[1:19]),
        paste0("WD_", sprintf("%02d", seq(5, 95, by = 5)))
      )
    }

    widths <- lapply(widths, as.numeric)

    # Add Fluke Width (WD_F)
    widths$WD_F <- rv$new_fw

    # Combine all columns
    new_entry <- cbind(base_df, as.data.frame(widths))

    return(new_entry)
  }

  # Save data function
  save_data <- function(new_entry) {
    tryCatch({

      rv$main_data <- readxl::read_xlsx(rv$main, col_names = T) |>
        dplyr::mutate(
          dplyr::across(c(F_Alt, TO_Alt, C_Alt, BL, starts_with("WD_"),
                          cGSD, EstLength), as.numeric),
          dplyr::across(c(Drone, Obs, Species, Date, Measured_Date, ID,
                          Frame_Score, Comments), as.character)
        )

      rv$current_data <- dplyr::bind_rows(rv$main_data, new_entry)

      writexl::write_xlsx(rv$current_data, rv$main)

      filtered_data <- dtfilter(rv$current_data)

      writexl::write_xlsx(x = filtered_data, path = rv$secondary)

      rv$add_status <- TRUE

      shiny::showModal(
        modalDialog(
          title = "Saved",
          "Measurements stored successfully",
          footer = modalButton("OK")
        )
      )
      return(TRUE)
    }, error = function(e) {
      showNotification(paste("Error when saving:", e$message), type = "error")
      return(FALSE)
    })
  }

  # Save system ----
  shiny::observeEvent(input$saveBtn, {
    req(rv$main_data, rv$new_fw)

    # Criar nova entrada
    new_entry <- create_new_entry(rv$segments)

    save_data(new_entry = new_entry)

    rv$click_save <- TRUE

    output$mTable <- DT::renderDataTable({
      MedidoR:::update_data_table(rv$secondary)
    })
  })

  # UI feedback for add state
  output$add_status <- shiny::renderUI({
    req(input$file)
    if (rv$click_save == FALSE) {
      div(
        class = "alert alert-info",
        "Step 2: After completing the measurements, click on 'Add-IN button' to save"
      )
    } else {
      div(class = "alert alert-success",
          "The measurements have been saved!")
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
    rv$fw_measurements <- data.frame()
    # Novos dados
    rv$new_id = character()
    rv$new_score = character()
    rv$new_date = character()
    rv$new_f_alt = numeric()
    rv$new_calti = numeric()
    rv$new_bl = numeric()
    rv$new_widths = numeric()
    rv$new_fw = numeric()
    rv$crop_status = FALSE
    rv$add_status = TRUE
    rv$click_status = FALSE
    shiny::updateActionButton(session, "crop", disabled = FALSE)
    shiny::updateActionButton(session, "saveBtn", disabled = TRUE)
    shiny::updateTextInput(inputId = "comments", value = "")
    shiny::updateTextInput(inputId = "ImageId", value = "")
    shiny::updateRadioButtons(inputId = "score", selected = 4)
    shiny::updateNumericInput(inputId = "alt", value = 20)
    rv$plot_ranges_x = NULL
    rv$plot_ranges_y = NULL

    shiny::removeModal()
  })

  #################################### Calibration  block ####################################

  # Re factored Calibration Handler
  shiny::observeEvent(input$calib, {
    req(input$calib_path, rv$main, rv$secondary)

    tryCatch({
      rv$calib_path <- input$calib_path

      # Carregar e processar dados de calibração
      rv$calib_data <- MedidoR:::calib(rv$calib_path) |>
        stats::na.omit()

      # Treinar modelo
      rv$calib_train <- caret::train(
        as.numeric(eGSD) ~ as.numeric(C_Alt),
        data = rv$calib_data,
        method = "lm",
        trControl = caret::trainControl(method = "repeatedcv",
                                        number = 10,
                                        repeats = 5)
      )

      rv$calib_model <- stats::lm(formula = as.numeric(eGSD) ~ as.numeric(C_Alt),
                           data = rv$calib_data)

      rv$calib_data$cGSD <- stats::predict(rv$calib_model, rv$calib_data)
      rv$calib_data$LcGSD <- rv$calib_data$cGSD * rv$calib_data$Pixel

      # Atualizar medições
      updated <- MedidoR:::update_measurements(
        main_path = rv$main,
        secondary_path = rv$secondary,
        model = rv$calib_model,
        calib_data = rv$calib_data,
        current_data = NULL
      )

      if (!is.null(updated)) {
        output$mTable <- DT::renderDataTable({
          MedidoR:::update_data_table(rv$secondary)
        })
        showNotification("Calibration applied successfully", type = "message")
      }
    }, error = function(e) {
      showNotification(paste("Calibration failed:", e$message), type = "error")

      calib_log(rbind(calib_log(),
                      data.frame(Timestamp = Sys.time(),
                                 Message = paste("Error:", e$message))))
    })

    if (input$save_plot == "Y") {
      dir.create(file.path(rv$user_dir, "Model-Plots"))
    }
  })

  #################################### Model Plot Block ####################################

  output$checkm <- shiny::renderPlot({
    req(input$calib, rv$calib_model)

    tryCatch({
      # Configurar área de plotagem
      old_par <- par(no.readonly = TRUE)
      on.exit(par(old_par))
      par(mfrow = c(2, 2))

      # Plotar diagnósticos
      plot(rv$calib_model)

      # Salvar se necessário
      if (input$save_plot == "Y") {
        png(file.path(rv$user_dir,"Model-Plots", "Diagnostic_plot.png"),
            width = 10, height = 6, units = "in", res = 300)
        par(mfrow = c(2, 2))
        plot(rv$calib_model)
        dev.off()
      }

    }, error = function(e) {
      showNotification(paste("Erro ao gerar gráficos de diagnóstico:", e$message),
                       type = "error")
      return(NULL)
    })
  })

  output$variance <- shiny::renderPlot({
    req(input$calib, rv$calib_data, rv$calib_train)

    tryCatch({
      if (!"C_Alt" %in% names(rv$calib_data) || !"LcGSD" %in% names(rv$calib_data) || !"ObjLength" %in% names(rv$calib_data)) {
        stop("Calibration data does not contain required columns")
      }

      p <- ggplot2::ggplot(
        as.data.frame(rv$calib_data),
        ggplot2::aes(x = as.factor(round(C_Alt, 0)), y = LcGSD)
      ) +
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
            rv$calib_train$results$RMSE,
            rv$calib_train$results$Rsquared,
            rv$calib_train$results$MAE
          )
        ) +
        ggplot2::geom_hline(
          ggplot2::aes(yintercept = unique(ObjLength) / 100,
                       linetype = paste("Object length:", round(unique(ObjLength)/100, 2), "m")),
          colour = 'blue',
          linewidth = 1
        ) +
        ggplot2::geom_hline(
          ggplot2::aes(yintercept = mean(LcGSD),
                       linetype = paste("Mean estimated length:", round(mean(LcGSD), 2), "m")),
          colour = 'red',
          linewidth = 1
        ) +
        ggplot2::scale_linetype_manual(
          name = "Reference Lines",
          values = c(1, 1),
          guide = ggplot2::guide_legend(
            override.aes = list(
              colour = c("red", "blue"),
              linewidth = 1
            )
          )
        ) +
        ggplot2::theme(
          legend.position = "bottom",
          legend.box = "horizontal",
          legend.title = ggplot2::element_text(face = "bold")
        )

      if (input$save_plot == "Y") {
        ggplot2::ggsave(filename = "Variance_plot.png",
                        plot = p,
                        path = file.path(rv$user_dir, "Model-Plots"),
                        width = 10,
                        height = 6)
      }

      print(p)

    }, error = function(e) {
      showNotification(paste("Error generating variance graph:", e$message), type = "error")
      return(NULL)
    })
  })

  output$mplot <- shiny::renderPlot({
    req(input$calib, rv$calib_data)

    tryCatch({
      if (!"C_Alt" %in% names(rv$calib_data) || !"eGSD" %in% names(rv$calib_data)) {
        stop("Dados de calibração não contêm colunas necessárias")
      }

      p <- ggplot2::ggplot(
        as.data.frame(rv$calib_data),
        ggplot2::aes(x = as.numeric(C_Alt), y = eGSD)
      ) +
        ggplot2::geom_point() +
        ggplot2::geom_smooth(method = "lm", formula = y ~ x, se = TRUE) +
        ggplot2::theme_classic() +
        ggplot2::labs(x = "Altitude (m)", y = "eGSD")

      # Salvar o gráfico se necessário
      if (input$save_plot == "Y") {
        ggplot2::ggsave(filename = "Regression_plot.png",
                        plot = p,
                        path = file.path(rv$user_dir, "Model-Plots"),
                        width = 10,
                        height = 6)
      }

      print(p)

    }, error = function(e) {
      showNotification(paste("Error generating regression graph:", e$message), type = "error")
      return(NULL)
    })
  })

  shiny::observeEvent(input$n_whales, {
    req(rv$segments, rv$user_dir)

    if (input$n_whales != 0) {
      tryCatch({
        file_name <- ifelse(rv$segments == 1,
                            "Measurements_10_1.xlsx",
                            "Measurements_05_1.xlsx")
        file_path <- file.path(rv$dir_path, file_name)

        if (!file.exists(file_path)) {
          stop("Measurement file not found")
        }

        measurements <- readxl::read_excel(path = file_path,
                                           col_names = TRUE) |>
          dplyr::mutate(
            Date_mmd = format(as.Date(Date), "%m-%d"),
            ID_combined = paste(ID, " (", Date_mmd, ")", sep = "")
          )

        if (!"ID" %in% names(measurements) || !"Date" %in% names(measurements)) {
          stop("Columns 'ID' or 'Date' not found in data")
        }

        unique_ids <- unique(measurements$ID_combined)

        if (length(unique_ids) == 0) {
          stop("No data available for display")
        }

        selected_ids <- unique_ids[1:min(input$n_whales, length(unique_ids))]
        filtered_data <- measurements[measurements$ID_combined %in% selected_ids, ]

        output$lplot <- shiny::renderPlot({
          ggplot2::ggplot(filtered_data,
                          ggplot2::aes(x = Segments, y = Pixel, fill = ID_combined)) +
            ggplot2::geom_col(position = ggplot2::position_dodge(width = 0.8),
                              show.legend = TRUE) +
            ggplot2::theme_classic() +
            ggplot2::labs(title = "Pixel Measurements per Segment",
                          x = "Segments", y = "Pixels",
                          fill = "ID (Date)") +
            ggplot2::scale_fill_viridis_d() +
            ggplot2::theme(legend.position = "bottom")
        })

        output$mwhale <- shiny::renderPlot({
          ggplot2::ggplot(filtered_data,
                          ggplot2::aes(x = Segments, y = EstLength, fill = ID_combined)) +
            ggplot2::geom_col(position = ggplot2::position_dodge(width = 0.8),
                              show.legend = TRUE) +
            ggplot2::theme_classic() +
            ggplot2::labs(title = "Estimated Lengths (meters)",
                          x = "Segments", y = "Length (m)",
                          fill = "ID (Date)") +
            ggplot2::scale_fill_viridis_d() +
            ggplot2::theme(legend.position = "bottom")
        })

      }, error = function(e) {
        showNotification(paste("Error generating graphs:", e$message),
                         type = "error")
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
