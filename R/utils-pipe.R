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

#' @title Function to create the spreadsheet (.xlsx)
#' @description
#' Create a 5% or 10% interval spreadsheet
#' @importFrom dplyr select
#' @importFrom writexl write_xlsx
#' @importFrom shiny showModal
#' @importFrom shiny modalDialog
#' @importFrom shiny modalButton
#' @keywords internal
#' @export

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
