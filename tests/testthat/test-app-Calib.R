library(shinytest2)

test_that("{shinytest2} recording: calib_gui_test", {
  local_app_support(test_path("../../inst/shiny-apps/Calib"))
  app <- AppDriver$new(test_path("../../inst/shiny-apps/Calib"), name = "calib_gui_test",
      seed = 123, height = 911, width = 1619)
  app$click("path")
  app$click("confirmBtn")
  app$set_inputs(wd = "G:/Meu Drive/Drive_Work/_Lucas Oliveira/REPOS/MeDiDOR/inst/shiny-apps/Calib")
  app$set_inputs(wd = "G:\\Meu Drive\\Drive_Work\\_Lucas Oliveira\\REPOS\\MeDiDOR\\inst\\extdata\\calib_test")
  app$click("confirmBtn")
  app$click("import")
  rlang::warn(paste0("`file` should be the path to the file, relative to the app's tests/testthat directory.\n",
      "Remove this warning when the file is in the correct location."))
  app$upload_file(file = "Molde_15M_1.png")
  app$set_inputs(plot_click = character(0), allow_no_input_binding_ = TRUE)
  app$set_inputs(plot_brush = character(0), allow_no_input_binding_ = TRUE)
  app$set_inputs(objL = "2")
  app$set_inputs(ImageID = "15m_1")
  app$set_inputs(ImageRES = "4k")
  app$set_inputs(sw = "13.2")
  app$set_inputs(flen = "8.8")
  app$set_inputs(drone = "P4P")
  app$set_inputs(alt = 10)
  app$set_inputs(alt = 14.7)
  app$set_inputs(takeof = 0)
  app$set_inputs(takeof = 1.2)
  app$set_inputs(takeof = character(0))
  app$set_inputs(takeof = 1.8)
  app$set_inputs(Date = "2019-08-13")
  app$set_inputs(obs = "Observer 1")
  app$set_inputs(plot_click = c(1851.80384720203, 1002.98662904251, 506.8125, 333.046875,
      506.8125, 333.046875, 1, 1, 0.999999999999773, 4096, 2294.71511627907, -133.71511627907,
      59.04, 1049.76, 645.56, 58.04, character(0), character(0)), allow_no_input_binding_ = TRUE,
      priority_ = "event")
  app$set_inputs(plot_brush = c(1851.803847202, 2587.5414925509, 1002.9866290425,
      1457.6559604379, 506.8125, 684.8125, 333.046875, 443.046875, 506.8125, 684.8125,
      333.046875, 443.046875, 1, 1, 0.999999999999773, 4096, 2294.71511627907, -133.71511627907,
      59.04, 1049.76, 645.56, 58.04, character(0), character(0), "xy", "plot_brush",
      "imagePlot"), allow_no_input_binding_ = TRUE)
  app$click("crop")
  app$set_inputs(plot_click = c(2054.3818102982, 1206.52210019011, 340.8125, 321.046875,
      340.8125, 321.046875, 1, 1, 1836.32401791562, 2603.02132183728, 1457.6559604379,
      1002.9866290425, 59.0400000000002, 1049.76, 645.56, 58.04, character(0), character(0)),
      allow_no_input_binding_ = TRUE, priority_ = "event")
  app$set_inputs(plot_click = c(2370.124401545, 1203.42658458965, 748.8125, 317.046875,
      748.8125, 317.046875, 1, 1, 1836.32401791562, 2603.02132183728, 1457.6559604379,
      1002.9866290425, 59.0400000000002, 1049.76, 645.56, 58.04, character(0), character(0)),
      allow_no_input_binding_ = TRUE, priority_ = "event")
  app$click("saveBtn")
  app$click("clearBtn")
  app$click("confirm_reset")
  app$click("confirm_reset")
  app$set_inputs(mTable_rows_current = 1, allow_no_input_binding_ = TRUE)
  app$set_inputs(mTable_rows_all = 1, allow_no_input_binding_ = TRUE)
  app$set_inputs(mTable_state = c(1782743336559, 0, 10, "", TRUE, FALSE, TRUE, c(TRUE,
      "", TRUE, FALSE, TRUE), c(TRUE, "", TRUE, FALSE, TRUE), c(TRUE, "", TRUE, FALSE,
      TRUE), c(TRUE, "", TRUE, FALSE, TRUE), c(TRUE, "", TRUE, FALSE, TRUE), c(TRUE,
      "", TRUE, FALSE, TRUE), c(TRUE, "", TRUE, FALSE, TRUE), c(TRUE, "", TRUE, FALSE,
      TRUE), c(TRUE, "", TRUE, FALSE, TRUE), c(TRUE, "", TRUE, FALSE, TRUE), c(TRUE,
      "", TRUE, FALSE, TRUE), c(TRUE, "", TRUE, FALSE, TRUE), c(TRUE, "", TRUE, FALSE,
      TRUE), c(TRUE, "", TRUE, FALSE, TRUE), c(TRUE, "", TRUE, FALSE, TRUE), c(TRUE,
      "", TRUE, FALSE, TRUE), c(TRUE, "", TRUE, FALSE, TRUE)), allow_no_input_binding_ = TRUE)
  app$set_inputs(mTable_state = c(1782743341653, 0, 100, "", TRUE, FALSE, TRUE, c(TRUE,
      "", TRUE, FALSE, TRUE), c(TRUE, "", TRUE, FALSE, TRUE), c(TRUE, "", TRUE, FALSE,
      TRUE), c(TRUE, "", TRUE, FALSE, TRUE), c(TRUE, "", TRUE, FALSE, TRUE), c(TRUE,
      "", TRUE, FALSE, TRUE), c(TRUE, "", TRUE, FALSE, TRUE), c(TRUE, "", TRUE, FALSE,
      TRUE), c(TRUE, "", TRUE, FALSE, TRUE), c(TRUE, "", TRUE, FALSE, TRUE), c(TRUE,
      "", TRUE, FALSE, TRUE), c(TRUE, "", TRUE, FALSE, TRUE), c(TRUE, "", TRUE, FALSE,
      TRUE), c(TRUE, "", TRUE, FALSE, TRUE), c(TRUE, "", TRUE, FALSE, TRUE), c(TRUE,
      "", TRUE, FALSE, TRUE), c(TRUE, "", TRUE, FALSE, TRUE)), allow_no_input_binding_ = TRUE)
  app$expect_values()
})
