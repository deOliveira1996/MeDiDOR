#' @title dtfilter
#' @importFrom tidyr gather
#' @importFrom dplyr arrange
#' @keywords internal
#' @export

dtfilter <- function(x) {
  x <- x %>%
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
    ) %>%
    dplyr::arrange(by_group = ID)
  return(x)
} # Function to filter the data for the data.table output

#' @title Generate modal dialog
#' @keywords internal
#' @export

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

#' @title Save the data in user path
#' @keywords internal
#' @export

save_data <- function(new_entry, paths) {
  full_path <- file.path(rv$current_dir, paths$main)
  current_data <- readxl::read_xlsx(full_path)
  updated_data <- rbind(current_data, new_entry)
  writexl::write_xlsx(updated_data, full_path)
  writexl::write_xlsx(dtfilter(updated_data),
                      file.path(rv$current_dir, paths$filtered))
}

#' @title Update the data table in UI
#' @keywords internal
#' @export

update_data_table <- function(path) {
  output$mTable <- DT::renderDataTable({
    DT::datatable(readxl::read_xlsx(path))
  })
}

#' @title Get the directory paths
#' @keywords internal
#' @export

# Unified directory handler
get_directory_path <- function(segments, user_dir) {
  dir_name <- ifelse(segments == 1, "10%_interval", "05%_interval")
  file.path(user_dir, dir_name)
}

#' @title Generate the file paths for data
#' @keywords internal
#' @export

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
#' @export

# Update measurements with model predictions
update_measurements <- function(model, segments) {
  file_name <- ifelse(segments == 1,
                      "Measurements_10_1.xlsx",
                      "Measurements_05_1.xlsx")
  file_path <- file.path(rv$current_dir, file_name)

  if (file.exists(file_path)) {
    measurements <- readxl::read_xlsx(file_path)
    measurements$cGSD <- stats::predict(model, measurements)
    measurements$EstLength <- measurements$cGSD * measurements$Pixel

    # Apply rounding
    measurements <- measurements %>%
      dplyr::mutate(
        dplyr::across(c(11, 15), ~ round(., 4)),
        dplyr::across(12, ~ round(., 2))
      )

    writexl::write_xlsx(measurements, file_path)
    return(measurements)
  }
  return(NULL)
}

#' @title Save the calibration plots
#' @keywords internal
#' @export

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
