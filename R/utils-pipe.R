
#' @title dtfilter
#' @importFrom tidyr gather
#' @importFrom dplyr arrange
#' @export

dtfilter <- function(x) {
  x <- x |>
    tidyr::gather(
      key = "Segments",
      value = "Pixel",
      -c(
        ID,
        Frame_Score,
        F_Alt,
        TO_Alt,
        C_Alt,
        Measured_Date,
        Species,
        cGSD,
        EstLength,
        Obs,
        Comments,
        Drone,
        Date
      )
    ) |>
    dplyr::arrange(by_group = ID)
  return(x)
} # Function to filter the data for the data.table output

#' @title Generate modal dialog
#' @importFrom shiny showModal modalDialog
#' @keywords internal

# Helper functions ----
show_measurement_modal <- function(message) {
  shiny::showModal(
    shiny::modalDialog(
      title = "Measurement Update",
      message,
      footer = shiny::modalButton("OK"),
      easyClose = TRUE
    )
  )
}

#' @title Update the data table in UI
#' @importFrom DT datatable
#' @keywords internal

update_data_table <- function(path) {
  if(file.exists(path)) {
    DT::datatable(
      readxl::read_xlsx(path),
      options = list(
        pageLength = 11,
        autoWidth = TRUE,
        serverSide = TRUE
      )
    )
  }
}

#' @title Get the directory paths
#' @keywords internal

# Unified directory handler
get_directory_path <- function(segments, user_dir) {
  dir_name <- ifelse(segments == 1, "10%_interval", "05%_interval")
  normalizePath(file.path(user_dir, dir_name), mustWork = FALSE)
}

#' @title Generate the file paths for data
#' @keywords internal

# Unified file path generator
get_file_paths <- function(dir_path, segments) {
  base_name <- ifelse(segments == 1, "Measurements_10", "Measurements_05")
  list(
    main = file.path(dir_path, paste0(base_name, ".xlsx")),
    secondary = file.path(dir_path, paste0(base_name, "_1.xlsx"))
  )
}

#' @title Update the data with calibration model estimates
#' @keywords internal
#' @importFrom writexl write_xlsx
#' @importFrom readxl read_xlsx
#' @importFrom dplyr mutate across

# Update measurements with model predictions
update_measurements <- function(model, segments) {

  file_path <- file.path(rv$secondary)

  if (file.exists(file_path)) {
    measurements <- readxl::read_xlsx(file_path)
    measurements$cGSD <- stats::predict(model, measurements)
    measurements$EstLength <- as.numeric(measurements$cGSD) * as.numeric(measurements$Pixel)

    # Apply rounding
    measurements <- measurements |>
      dplyr::mutate(
        dplyr::across(c(11, 15), ~ round(., 2)),
        dplyr::across(12, ~ round(., 2))
      )

    writexl::write_xlsx(measurements, file_path)
    return(measurements)
  }
  return(NULL)
}

#' @title Save the calibration plots
#' @keywords internal
#' @importFrom ggplot2 ggsave

# Helper to save plots
save_calibration_plots <- function(plots, save_dir) {
  ggplot2::ggsave(
    filename = "Regression_plot.png",
    plot = plots$regression,
    path = save_dir,
    width = 10,
    height = 6
  )

  ggplot2::ggsave(
    filename = "Variance_plot.png",
    plot = plots$variance,
    path = save_dir,
    width = 10,
    height = 6
  )

  print(plots$diagnostic)
  grDevices::png(
    file.path(save_dir, "Diagnostic_plot.png"),
    width = 720,
    height = 480,
    units = "px"
  )
  grDevices::dev.off()
}

#' @title Import calibration data
#' @importFrom readxl read_excel
#' @importFrom dplyr select mutate across
#' @import tidyverse


calib <- function(file = "") {
  readxl::read_excel(file) |>
    dplyr::select(Date, Pixel, ObjLength,
                  GPSAlt, TO_Alt) |>
    dplyr::mutate(
      Date = as.factor(Date),
      across(c(Pixel, ObjLength, GPSAlt, TO_Alt), as.numeric),
      C_Alt = GPSAlt + TO_Alt,
      eGSD = round((ObjLength/100)/Pixel, 4)
    )
}

#' @title Create measurement template
#' @description Generates interval measurement templates
#' @importFrom writexl write_xlsx
#' @keywords internal

create_data <- function(segments, path, path2) {
  if(segments == 1) {
    dt <- data.frame(
      Drone = character(0), Obs = character(0), Species = character(0),
      Date = character(0), Measured_Date = character(0), ID = character(0),
      Frame_Score = character(0), F_Alt = numeric(0), TO_Alt = numeric(0),
      C_Alt = numeric(0), BL = numeric(0),
      WD_10 = numeric(0), WD_20 = numeric(0), WD_30 = numeric(0),
      WD_40 = numeric(0), WD_50 = numeric(0), WD_60 = numeric(0),
      WD_70 = numeric(0), WD_80 = numeric(0), WD_90 = numeric(0),
      WD_F = numeric(0), cGSD = numeric(0), EstLength = numeric(0),
      Comments = character(0)
    )
    writexl::write_xlsx(dt, path)
    writexl::write_xlsx(dtfilter(dt), path2)

  } else {
    dt <- data.frame(
      Drone = character(0), Obs = character(0), Species = character(0),
      Date = character(0), Measured_Date = character(0), ID = character(0),
      Frame_Score = character(0), F_Alt = numeric(0), TO_Alt = numeric(0),
      C_Alt = numeric(0), BL = numeric(0),
      WD_05 = numeric(0), WD_10 = numeric(0), WD_15 = numeric(0),
      WD_20 = numeric(0), WD_25 = numeric(0), WD_30 = numeric(0),
      WD_35 = numeric(0), WD_40 = numeric(0), WD_45 = numeric(0),
      WD_50 = numeric(0), WD_55 = numeric(0), WD_60 = numeric(0),
      WD_65 = numeric(0), WD_70 = numeric(0), WD_75 = numeric(0),
      WD_80 = numeric(0), WD_85 = numeric(0), WD_90 = numeric(0),
      WD_95 = numeric(0), WD_F = numeric(0), cGSD = numeric(0),
      EstLength = numeric(0), Comments = character(0)
    )
    writexl::write_xlsx(dt, path)
    writexl::write_xlsx(dtfilter(dt), path2)

  }
}
