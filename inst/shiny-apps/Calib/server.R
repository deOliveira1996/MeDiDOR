
options(shiny.maxRequestSize = 50 * 1024^2)

# Define server
server <- function(input, output, session) {

  # Reactive values ----
  rv <- shiny::reactiveValues(

    # Medições
    scale_measurements = data.frame(Length_X = numeric(),
                                    Length_Y = numeric()),

    # Dados principais
    main_data = NULL,
    main = NULL,
    dir_path = NULL,

    # Diretórios
    user_dir = getwd(),

    # Novos dados
    new_id = character(),
    new_res = character(),
    new_date = character(),
    new_f_alt = numeric(),
    new_to_alt = numeric(),
    new_calti = numeric(),
    new_objL = numeric(),
    new_objP = numeric(),
    new_iw = numeric(),
    new_sw = numeric(),
    new_flen = numeric(),
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
    req(rv$user_dir)

    dir_path <- file.path(rv$user_dir)
    paths <- file.path(dir_path, paste0("calib", ".xlsx"))

    rv$main <- paths
    rv$dir_path <- dir_path

    tryCatch({
      if (!file.exists(rv$main)) {
        MedidoR:::create_data2(
          path = rv$main
        )
        shiny::showModal(shiny::modalDialog(
          title = "Success",
          "Scale calibration dataframe created",
          footer = shiny::modalButton("OK")
        ))

        rv$main_data <- readxl::read_xlsx(path = rv$main,
                                          col_names = T)

        output$mTable <- DT::renderDataTable({
          rv$main_data
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
      return(TRUE)
    }, error = function(e){
      showNotification(paste("Error:", e$message), type = "error")
      return(FALSE)
    })
  })

  # Import  block

  shiny::observeEvent(input$import, {
    req(rv$user_dir)

    dir_path <- file.path(rv$user_dir)
    paths <- file.path(dir_path, paste0("calib", ".xlsx"))

    rv$main <- paths
    rv$dir_path <- dir_path

    tryCatch({
      if (file.exists(rv$main)) {
        rv$main_data <- readxl::read_xlsx(rv$main)

        shiny::showModal(
          shiny::modalDialog(
            title = "Success",
            "Scale calibration dataframe imported",
            footer = shiny::modalButton("OK")
          )
        )

        output$mTable <- DT::renderDataTable({
          rv$main_data
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
      return(TRUE)
    }, error = function(e) {
      showNotification(paste("Error:", e$message), type = "error")
      return(FALSE)
    })
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

  #  Length measurements
  draw_measurement_lines <- function() {
    req(rv$current_image, rv$scale_measurements)

    lp <- rv$scale_measurements
    if (nrow(lp) < 2)
      return()

    start <- data.frame(x = lp$Length_X[1], y = lp$Length_Y[1])
    end <- data.frame(x = lp$Length_X[2], y = lp$Length_Y[2])

    # Main lines
    graphics::segments(start$x,
                       start$y,
                       end$x,
                       end$y,
                       col = "red",
                       lwd = 1.5)

    # Vectorial calculations
    original_dx <- end$x - start$x
    original_dy <- end$y - start$y
    perpendicular <- c(-original_dy, original_dx) / sqrt(original_dx^2 + original_dy^2)

    total_length <- sum(sqrt(diff(lp$Length_X)^2 + diff(lp$Length_Y)^2))
    rv$new_objP <- total_length

  }

  # Rendering plot
  output$imagePlot <- shiny::renderPlot({
    req(rv$current_image)

    if (rv$crop_status == TRUE) {
      plot(
        rv$current_image,
        xlim = rv$plot_ranges_x,
        ylim = rv$plot_ranges_y,
        main = input$file$name,
        axes = T
      )

      if (nrow(rv$scale_measurements) > 0) {
        points_df <- rv$scale_measurements |>
          dplyr::mutate(
            x_plot = rv$scale_measurements$Length_X,
            y_plot = rv$scale_measurements$Length_Y
          )
        graphics::points(points_df$x_plot,
                         points_df$y_plot,
                         col = "red",
                         cex = 1.5)
      }

      if (nrow(rv$scale_measurements) == 2) {
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

    tryCatch({
      if (nrow(rv$scale_measurements) != 2) {
        rv$scale_measurements <- rbind(
          rv$scale_measurements,
          data.frame(
            Length_X = input$plot_click$x,
            Length_Y = input$plot_click$y
          )
        )
        if (nrow(rv$scale_measurements) == 2) {
          MedidoR:::show_measurement_modal("Length measurements completed.")
          shiny::showModal(
            shiny::modalDialog(
              title = "Measurement Progress",
              HTML(
                "<b>Scale measurement complete!</b><br>
             Click on ADD-IN button to proceed."
              ),
              footer = shiny::modalButton("Continue"),
              easyClose = TRUE
            )
          )
          rv$add_status <- FALSE
        }
      }
      return(TRUE)
    }, error = function(e){
      showNotification(paste("Error:", e$message), type = "error")
      return(FALSE)
    })
  })

  shiny::observe({
    req(input$file)

    if (nrow(rv$scale_measurements) > 0) {
      shiny::updateActionButton(session, "crop", disabled = TRUE)
    } else {
      shiny::updateActionButton(session, "crop", disabled = FALSE)
    }

    if (rv$add_status == TRUE) {
      shiny::updateActionButton(session, "saveBtn", disabled = TRUE)
    } else {
      shiny::updateActionButton(session, "saveBtn", disabled = FALSE)
    }

    rv$new_res = as.character(input$ImageRES)
    rv$new_iw = as.numeric(rv$img_width)
    rv$new_sw = as.numeric(input$sw)
    rv$new_flen = as.numeric(input$flen)
    rv$new_id = as.character(input$ImageID)
    rv$new_date = as.character(input$Date)
    rv$new_f_alt = as.numeric(input$alt)
    rv$new_to_alt = as.numeric(input$takeof)
    rv$new_calti = as.numeric(input$alt) + as.numeric(input$takeof)
    rv$new_id = as.character(input$ImageID)
    rv$new_drone = as.character(input$drone)
    rv$new_objL = as.numeric(input$objL)
  })

  ############################ Save  block ############################

  # Create New Entry function
  create_new_entry2 <- function() {

    new_entry <- data.frame(
      Drone = as.character(rv$new_drone),
      Resolution = as.character(rv$new_res),
      ID = as.character(rv$new_id),
      Obs = as.character(input$obs),
      Date = as.character(rv$new_date),
      Measured_Date = as.character(format(as.POSIXct(Sys.time()),
                                          "%Y-%m-%d %H:%M:%S")),
      TO_Alt = as.numeric(rv$new_to_alt),
      F_Alt = as.numeric(rv$new_f_alt),
      C_Alt = as.numeric(rv$new_calti),
      OBJ_L = as.numeric(rv$new_objL),
      OBJ_P = round(as.numeric(rv$new_objP), 2),
      sw = as.numeric(rv$new_sw),
      iw = as.numeric(rv$new_iw),
      flen = as.numeric(rv$new_flen),
      Comments = as.character(input$comments)
    )

    return(new_entry)
  }

  # Save data function
  save_data <- function(new_entry) {
    tryCatch({

      rv$main_data <- readxl::read_xlsx(rv$main, col_names = T) |>
        dplyr::mutate(
          dplyr::across(c(F_Alt, TO_Alt, C_Alt, OBJ_L,
                          OBJ_P, sw, iw, flen), as.numeric),
          dplyr::across(c(Drone, Obs, Resolution, Date,
                          Measured_Date, ID, Comments), as.character)
        )

      rv$current_data <- dplyr::bind_rows(rv$main_data, new_entry)

      writexl::write_xlsx(rv$current_data, rv$main)

      rv$add_status <- TRUE

      shiny::showModal(
        modalDialog(
          title = "Saved",
          "Scale measurements stored successfully",
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
    req(rv$main_data)
    tryCatch({
      # Criar nova entrada
      new_entry <- create_new_entry2()

      save_data(new_entry = new_entry)

      rv$click_save <- TRUE

      output$mTable <- DT::renderDataTable({
        rv$current_data
      })
      return(TRUE)
    }, error = function(e) {
      showNotification(paste("Error when saving:", e$message), type = "error")
      return(FALSE)
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
          "The scale measurements have been saved!")
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
    rv$scale_measurements <- data.frame()

    # Novos dados
    rv$new_id = character()
    rv$new_objL = numeric()
    rv$new_objP = numeric()
    rv$new_date = character()
    rv$new_f_alt = numeric()
    rv$new_calti = numeric()
    rv$new_sw = numeric()
    rv$new_iw = numeric()
    rv$new_flen = numeric()
    rv$crop_status = FALSE
    rv$add_status = TRUE
    rv$click_status = FALSE
    shiny::updateActionButton(session, "crop", disabled = FALSE)
    shiny::updateActionButton(session, "saveBtn", disabled = TRUE)
    shiny::updateTextInput(inputId = "comments", value = "")
    shiny::updateTextInput(inputId = "ImageID", value = "")
    shiny::updateNumericInput(inputId = "alt", value = 20)
    rv$plot_ranges_x = NULL
    rv$plot_ranges_y = NULL

    shiny::removeModal()
  })

  ################
  # Close button #
  ################

  shiny::observeEvent(input$closeBtn, {
    shiny::stopApp()
  })
}
