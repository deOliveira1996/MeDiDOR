
#' @title Open MedidoR API
#' @description Opens a Shiny application developed to conduct aerial photogrammetry analysis. MedidoR allows you to extract and convert pixel measurements from aerial images collected of marine animals.
#' @importFrom shiny runApp
#' @importFrom caret trainControl train
#' @importFrom DT renderDataTable datatable
#' @importFrom ggplot2 ggplot aes geom_point geom_smooth theme_classic position_dodge stat_summary ylab xlab ggtitle geom_hline scale_linetype_manual guide_legend theme element_text geom_col theme_bw
#' @importFrom graphics plot points segments
#' @importFrom htmltools tagList
#' @importFrom performance check_model
#' @importFrom readxl read_xlsx read_excel
#' @importFrom shiny reactiveValues observeEvent showModal modalDialog textInput actionButton modalButton reactiveVal removeModal req renderPlot reactiveFileReader updateTextInput updateRadioButtons updateNumericInput stopApp runApp
#' @importFrom shinycssloaders showSpinner
#' @importFrom stats na.omit lm predict
#' @importFrom writexl write_xlsx
#' @importFrom imager load.image width height
#' @importFrom dplyr bind_rows
#' @export

medidor <- function() {
  shiny::runApp(
    appDir = system.file("shiny-apps", "MedidoR", package = "MedidoR"))
}
