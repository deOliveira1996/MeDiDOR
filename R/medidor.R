#' @title Used to open the MedidoR API
#' @description Opens a Shiny application developed to conduct aerial photogrammetry analysis. MedidoR allows you to extract and convert pixel measurements from aerial images collected of marine animals.
#' @importFrom shiny runApp
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
#' @export

medidor <- function() {
  shiny::runApp(
    appDir = system.file("shiny-apps", "MedidoR",
                         package = "MedidoR"))
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

#' @title Function to import the measurement spreadsheet (.xlsx)
#' @param file Full path of the file (.xlsx)
#' @importFrom dplyr as_tibble
#' @importFrom readxl read_xlsx
#' @export

import_data <- function(file = "") {
  readxl::read_xlsx(path = "", col_names = TRUE) %>%
    dplyr::as_tibble()
}
