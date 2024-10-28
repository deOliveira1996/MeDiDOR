#' @title Used to open the MedidoR API
#' Opens a Shiny application developed to conduct aerial photogrammetry analysis. MedidoR allows you to extract and convert pixel measurements from aerial images collected of marine animals.
#' @importFrom shiny runApp
#' @export

medidor <- function() {
  shiny::runApp(appDir = system.file("shiny-apps", "MedidoR", package = "MedidoR"))

}

#' @title Function to import the measurement spreadsheet (.xlsx)
#' @param file Full path of the file (.xlsx)
#' @importFrom dplyr as_tibble
#' @importFrom readxl read_xlsx
#' @export

import_data <- function(file = "") {
  readxl::read_xlsx(path = "", col_names = TRUE) %>%
    dplyr::as_tibble()
}

#' @title dtfilter
#' @importFrom tidyr gather
#' @importFrom dplyr arrange
#' @keywords internal

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

#' @title slow_function
#' @keywords internal

slow_function <- function() {
  Sys.sleep(2)
}

#' @title Function to import the calib spreadsheet (.xlsx)
#' @param file Full path of the file (.xlsx)
#' @importFrom readxl read_excel
#' @importFrom dplyr select
#' @export

calib <- function(file = "") {
  mdata <- readxl::read_excel(path = file, col_names = T) %>%
    dplyr::select(c("Date", "Pixel", "ObjLength",
                    "GPSAlt", "TO_Alt"))

  mdata$Data <- as.factor(mdata$Date)
  mdata$Pixel <- as.numeric(mdata$Pixel)
  mdata$ObjLength <- as.numeric(mdata$ObjLength)
  mdata$GPSAlt <- as.numeric(mdata$GPSAlt)
  mdata$TO_Alt <- as.numeric(mdata$TO_Alt)
  mdata$C_Alt <- as.numeric(mdata$GPSAlt) + as.numeric(mdata$TO_Alt)

  mdata$eGSD <- as.numeric((mdata$ObjLength / 100) / mdata$Pixel)
  mdata$eGSD <- round(mdata$eGSD, digits = 4)

  return(mdata)
} # Open the calibration data frame

#' @title create_data
#' @importFrom dplyr select
#' @importFrom writexl write_xlsx
#' @importFrom shiny showModal
#' @importFrom shiny modalDialog
#' @importFrom shiny modalButton

create_data <- function(segments = 2,
                        path = NULL,
                        path2 = NULL) {
  if (segments == 1) {
    df <- data.frame(
      "Drone" = character(0),
      "Obs" = character(0),
      "Species" = character(0),
      "Date" = character(0),
      "Measured_Date" = character(0),
      "ID" = character(0),
      "Frame_Score" = character(0),
      "F_Alt" = numeric(0),
      "TO_Alt" = numeric(0),
      "C_Alt" = numeric(0),
      "BL" = numeric(0),
      "WD_10" = numeric(0),
      "WD_20" = numeric(0),
      "WD_30" = numeric(0),
      "WD_40" = numeric(0),
      "WD_50" = numeric(0),
      "WD_60" = numeric(0),
      "WD_70" = numeric(0),
      "WD_80" = numeric(0),
      "WD_90" = numeric(0),
      "WD_F" = numeric(0),
      "cGSD" = numeric(0),
      "EstLength" = numeric(0),
      "Comments" = character(0)
    )

    writexl::write_xlsx(df, path = path)
    writexl::write_xlsx(path = path2, dtfilter(df))

    shiny::showModal(
      shiny::modalDialog(
        title = "Dataframe created",
        "Done",
        footer = shiny::modalButton("OK"),
        easyClose = TRUE
      )
    )
  }

  if (segments == 2) {
    df <- data.frame(
      "Drone" = character(0),
      "Obs" = character(0),
      "Species" = character(0),
      "Date" = character(0),
      "Measured_Date" = character(0),
      "ID" = character(0),
      "Frame_Score" = character(0),
      "F_Alt" = numeric(0),
      "TO_Alt" = numeric(0),
      "C_Alt" = numeric(0),
      "BL" = numeric(0),
      "WD_05" = numeric(0),
      "WD_10" = numeric(0),
      "WD_15" = numeric(0),
      "WD_20" = numeric(0),
      "WD_25" = numeric(0),
      "WD_30" = numeric(0),
      "WD_35" = numeric(0),
      "WD_40" = numeric(0),
      "WD_45" = numeric(0),
      "WD_50" = numeric(0),
      "WD_55" = numeric(0),
      "WD_60" = numeric(0),
      "WD_65" = numeric(0),
      "WD_70" = numeric(0),
      "WD_75" = numeric(0),
      "WD_80" = numeric(0),
      "WD_85" = numeric(0),
      "WD_90" = numeric(0),
      "WD_95" = numeric(0),
      "WD_F" = numeric(0),
      "cGSD" = numeric(0),
      "EstLength" = numeric(0),
      "Comments" = character(0)
    )

    writexl::write_xlsx(df, path)
    writexl::write_xlsx(path = path2, dtfilter(df))

    shiny::showModal(
      shiny::modalDialog(
        title = "Dataframe created",
        "Done",
        footer = shiny::modalButton("OK"),
        easyClose = TRUE
      )
    )
  }
  return(df)
} # create data frame
