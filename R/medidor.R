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

#' @title Open MedidoR API
#' @description Launches Shiny app for aerial photogrammetry analysis
#' @importFrom shiny runApp
#' @export

medidor <- function() {
  shiny::runApp(
    appDir = system.file("shiny-apps", "MedidoR", package = "MedidoR"))
}

#' @title Import calibration data
#' @param file Path to calibration file (.xlsx)
#' @importFrom readxl read_excel
#' @importFrom dplyr select mutate across
#' @export

calib <- function(file = "") {
  readxl::read_excel(file) %>%
    dplyr::select(Date, Pixel, ObjLength, GPSAlt, TO_Alt) %>%
    dplyr::mutate(
      Date = as.factor(Date),
      across(c(Pixel, ObjLength, GPSAlt, TO_Alt), as.numeric),
      C_Alt = GPSAlt + TO_Alt,
      eGSD = round((ObjLength/100)/Pixel, 4)
    )
}

#' @title Create measurement template
#' @description Generates interval measurement templates
#' @keywords internal
#' @export

create_data<- function(segments, path, path2) {
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
