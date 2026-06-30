library(shinytest2)

test_that("{shinytest2} recording: medidor_gui_test", {
  local_app_support(test_path("../../inst/shiny-apps/MedidoR"))
  app <- AppDriver$new(test_path("../../inst/shiny-apps/MedidoR"), name = "medidor_gui_test",
      seed = 321, height = 911, width = 1619)
  app$click("path")
  app$wait_for_idle()
  medidor_wd <- system.file("extdata", "medidor_test", package = "MedidoR")
  app$set_inputs(wd = medidor_wd)
  app$click("confirmBtn")
  app$wait_for_idle()
  app$set_inputs(app_mode = "free")
  app$set_inputs(app_mode = "morpho")
  app$click("create")
  app$set_inputs(segments = "1")
  app$click("create")
  app$click("import")
  app$set_inputs(Species = "Eubalaena australis")
  app$set_inputs(ImageID = "RW-1")
  app$set_inputs(ImageRES = "2.7k")
  app$set_inputs(alt = character(0))
  app$set_inputs(alt = 22.5)
  app$set_inputs(takeof = 0.5)
  app$set_inputs(Date = "2022-08-01")
  app$set_inputs(obs = "Observer 1")
  app$set_inputs(sw = "6.4")
  app$set_inputs(flen = "7.6")
  app$set_inputs(drone = "MA2")
  app$set_inputs(score = "1")
  img_whale_path <- system.file("extdata", "medidor_test", "example_whale.png", package = "MedidoR")
  if (img_whale_path == "") stop("Whale test image not found in the extdata folder.")
  app$upload_file(file = img_whale_path)
  app$wait_for_idle(timeout = 10000)

  app$set_inputs(plot_click = c(2091.46522478601, 553.75689713037, 829.8125, 277.046875,
                                829.8125, 277.046875, 1, 1, 1, 2688, 1553.22674418605, -40.2267441860467, 59.04,
                                1049.76, 645.56, 58.04, character(0), character(0)), allow_no_input_binding_ = TRUE,
                 priority_ = "event")
  app$click("crop", wait_ = FALSE)
  app$wait_for_idle(timeout = 6000)
  app$set_inputs(plot_brush = c(735.38076096172, 2091.465224786, 553.75689713037,
      1107.0393583707, 329.8125, 829.8125, 277.046875, 481.046875, 329.8125, 829.8125,
      277.046875, 481.046875, 1, 1, 1, 2688, 1553.22674418605, -40.2267441860467,
      59.04, 1049.76, 645.56, 58.04, character(0), character(0), "xy", "plot_brush",
      "imagePlot"), allow_no_input_binding_ = TRUE)
  app$click("crop")
  app$set_inputs(plot_click = c(774.764179319464, 815.679392133953, 87.8125, 341.046875,
      87.8125, 341.046875, 1, 1, 735.38076096172, 2091.465224786, 1232.49293969843,
      428.303315802638, 59.04, 1049.76, 645.56, 58.04, character(0), character(0)),
      allow_no_input_binding_ = TRUE, priority_ = "event")
  app$set_inputs(plot_click = c(1512.54026741831, 852.636635879164, 626.8125, 368.046875,
      626.8125, 368.046875, 1, 1, 735.38076096172, 2091.465224786, 1232.49293969843,
      428.303315802638, 59.04, 1049.76, 645.56, 58.04, character(0), character(0)),
      allow_no_input_binding_ = TRUE, priority_ = "event")
  app$click("undo")
  app$set_inputs(plot_click = c(1505.69633339142, 858.111783100676, 621.8125, 372.046875,
      621.8125, 372.046875, 1, 1, 735.38076096172, 2091.465224786, 1232.49293969843,
      428.303315802638, 59.04, 1049.76, 645.56, 58.04, character(0), character(0)),
      allow_no_input_binding_ = TRUE, priority_ = "event")
  app$set_inputs(plot_click = c(1940.97053750168, 833.473620603869, 939.8125, 354.046875,
      939.8125, 354.046875, 1, 1, 735.38076096172, 2091.465224786, 1232.49293969843,
      428.303315802638, 59.04, 1049.76, 645.56, 58.04, character(0), character(0)),
      allow_no_input_binding_ = TRUE, priority_ = "event")
  app$set_inputs(plot_click = c(891.795451179299, 749.977625475799, 173.3125, 293.046875,
      173.3125, 293.046875, 1, 1, 735.38076096172, 2091.465224786, 1232.49293969843,
      428.303315802638, 59.04, 1049.76, 645.56, 58.04, character(0), character(0)),
      allow_no_input_binding_ = TRUE, priority_ = "event")
  app$set_inputs(plot_click = c(889.057877568543, 903.281747678157, 171.3125, 405.046875,
      171.3125, 405.046875, 1, 1, 735.38076096172, 2091.465224786, 1232.49293969843,
      428.303315802638, 59.04, 1049.76, 645.56, 58.04, character(0), character(0)),
      allow_no_input_binding_ = TRUE, priority_ = "event")
  app$set_inputs(plot_click = c(1009.51111644182, 736.289757422018, 259.3125, 283.046875,
      259.3125, 283.046875, 1, 1, 735.38076096172, 2091.465224786, 1232.49293969843,
      428.303315802638, 59.04, 1049.76, 645.56, 58.04, character(0), character(0)),
      allow_no_input_binding_ = TRUE, priority_ = "event")
  app$set_inputs(plot_click = c(1006.77354283107, 940.238991423368, 257.3125, 432.046875,
      257.3125, 432.046875, 1, 1, 735.38076096172, 2091.465224786, 1232.49293969843,
      428.303315802638, 59.04, 1049.76, 645.56, 58.04, character(0), character(0)),
      allow_no_input_binding_ = TRUE, priority_ = "event")
  app$set_inputs(plot_click = c(1127.22678170435, 711.65159492521, 345.3125, 265.046875,
      345.3125, 265.046875, 1, 1, 735.38076096172, 2091.465224786, 1232.49293969843,
      428.303315802638, 59.04, 1049.76, 645.56, 58.04, character(0), character(0)),
      allow_no_input_binding_ = TRUE, priority_ = "event")
  app$set_inputs(plot_click = c(1121.75163448284, 962.139580309419, 341.3125, 448.046875,
      341.3125, 448.046875, 1, 1, 735.38076096172, 2091.465224786, 1232.49293969843,
      428.303315802638, 59.04, 1049.76, 645.56, 58.04, character(0), character(0)),
      allow_no_input_binding_ = TRUE, priority_ = "event")
  app$set_inputs(plot_brush = c(1121.7516344828, 1123.1204212882, 962.13958030942,
      962.13958030942, 341.3125, 342.3125, 448.046875, 448.046875, 341.3125, 342.3125,
      448.046875, 448.046875, 1, 1, 735.38076096172, 2091.465224786, 1232.49293969843,
      428.303315802638, 59.04, 1049.76, 645.56, 58.04, character(0), character(0),
      "xy", "plot_brush", "imagePlot"), allow_no_input_binding_ = TRUE)
  app$set_inputs(plot_click = c(1243.57366016149, 723.970676173614, 430.3125, 274.046875,
      430.3125, 274.046875, 1, 1, 735.38076096172, 2091.465224786, 1232.49293969843,
      428.303315802638, 59.04, 1049.76, 645.56, 58.04, character(0), character(0)),
      allow_no_input_binding_ = TRUE, priority_ = "event")
  app$set_inputs(plot_click = c(1238.09851293998, 967.614727530932, 426.3125, 452.046875,
      426.3125, 452.046875, 1, 1, 735.38076096172, 2091.465224786, 1232.49293969843,
      428.303315802638, 59.04, 1049.76, 645.56, 58.04, character(0), character(0)),
      allow_no_input_binding_ = TRUE, priority_ = "event")
  app$set_inputs(plot_click = c(1358.55175181326, 733.552183811261, 514.3125, 281.046875,
      514.3125, 281.046875, 1, 1, 735.38076096172, 2091.465224786, 1232.49293969843,
      428.303315802638, 59.04, 1049.76, 645.56, 58.04, character(0), character(0)),
      allow_no_input_binding_ = TRUE, priority_ = "event")
  app$set_inputs(plot_click = c(1355.81417820251, 971.721087947066, 512.3125, 455.046875,
      512.3125, 455.046875, 1, 1, 735.38076096172, 2091.465224786, 1232.49293969843,
      428.303315802638, 59.04, 1049.76, 645.56, 58.04, character(0), character(0)),
      allow_no_input_binding_ = TRUE, priority_ = "event")
  app$set_inputs(plot_click = c(1476.26741707579, 765.03428033496, 600.3125, 304.046875,
      600.3125, 304.046875, 1, 1, 735.38076096172, 2091.465224786, 1232.49293969843,
      428.303315802638, 59.04, 1049.76, 645.56, 58.04, character(0), character(0)),
      allow_no_input_binding_ = TRUE, priority_ = "event")
  app$set_inputs(plot_click = c(1473.52984346503, 949.820499061015, 598.3125, 439.046875,
      598.3125, 439.046875, 1, 1, 735.38076096172, 2091.465224786, 1232.49293969843,
      428.303315802638, 59.04, 1049.76, 645.56, 58.04, character(0), character(0)),
      allow_no_input_binding_ = TRUE, priority_ = "event")
  app$set_inputs(plot_click = c(1591.24550872755, 791.041229637145, 684.3125, 323.046875,
      684.3125, 323.046875, 1, 1, 735.38076096172, 2091.465224786, 1232.49293969843,
      428.303315802638, 59.04, 1049.76, 645.56, 58.04, character(0), character(0)),
      allow_no_input_binding_ = TRUE, priority_ = "event")
  app$set_inputs(plot_click = c(1591.24550872755, 908.75689489967, 684.3125, 409.046875,
      684.3125, 409.046875, 1, 1, 735.38076096172, 2091.465224786, 1232.49293969843,
      428.303315802638, 59.04, 1049.76, 645.56, 58.04, character(0), character(0)),
      allow_no_input_binding_ = TRUE, priority_ = "event")
  app$set_inputs(plot_click = c(1707.5923871847, 818.416965744709, 769.3125, 343.046875,
      769.3125, 343.046875, 1, 1, 735.38076096172, 2091.465224786, 1232.49293969843,
      428.303315802638, 59.04, 1049.76, 645.56, 58.04, character(0), character(0)),
      allow_no_input_binding_ = TRUE, priority_ = "event")
  app$set_inputs(plot_click = c(1707.5923871847, 875.906011570593, 769.3125, 385.046875,
      769.3125, 385.046875, 1, 1, 735.38076096172, 2091.465224786, 1232.49293969843,
      428.303315802638, 59.04, 1049.76, 645.56, 58.04, character(0), character(0)),
      allow_no_input_binding_ = TRUE, priority_ = "event")
  app$set_inputs(plot_click = c(1825.30805244722, 806.097884496305, 855.3125, 334.046875,
      855.3125, 334.046875, 1, 1, 735.38076096172, 2091.465224786, 1232.49293969843,
      428.303315802638, 59.04, 1049.76, 645.56, 58.04, character(0), character(0)),
      allow_no_input_binding_ = TRUE, priority_ = "event")
  app$set_inputs(plot_click = c(1825.30805244722, 874.537224765215, 855.3125, 384.046875,
      855.3125, 384.046875, 1, 1, 735.38076096172, 2091.465224786, 1232.49293969843,
      428.303315802638, 59.04, 1049.76, 645.56, 58.04, character(0), character(0)),
      allow_no_input_binding_ = TRUE, priority_ = "event")
  app$set_inputs(plot_click = c(1986.82489548185, 592.567142857308, 973.3125, 178.046875,
      973.3125, 178.046875, 1, 1, 735.38076096172, 2091.465224786, 1232.49293969843,
      428.303315802638, 59.04, 1049.76, 645.56, 58.04, character(0), character(0)),
      allow_no_input_binding_ = TRUE, priority_ = "event")
  app$set_inputs(plot_click = c(2045.68272811311, 1056.58586988051, 1016.3125, 517.046875,
      1016.3125, 517.046875, 1, 1, 735.38076096172, 2091.465224786, 1232.49293969843,
      428.303315802638, 59.04, 1049.76, 645.56, 58.04, character(0), character(0)),
      allow_no_input_binding_ = TRUE, priority_ = "event")
  app$click("saveBtn")
  app$click("clearBtn")
  app$click("confirm_reset")
  app$click("confirm_reset")
  app$set_inputs(mTable_rows_current = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11), allow_no_input_binding_ = TRUE)
  app$set_inputs(mTable_rows_all = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11), allow_no_input_binding_ = TRUE)
  app$set_inputs(mTable_state = c(1782767371973, 0, 11, "", TRUE, FALSE, TRUE, c(TRUE,
      "", TRUE, FALSE, TRUE), c(TRUE, "", TRUE, FALSE, TRUE), c(TRUE, "", TRUE, FALSE,
      TRUE), c(TRUE, "", TRUE, FALSE, TRUE), c(TRUE, "", TRUE, FALSE, TRUE), c(TRUE,
      "", TRUE, FALSE, TRUE), c(TRUE, "", TRUE, FALSE, TRUE), c(TRUE, "", TRUE, FALSE,
      TRUE), c(TRUE, "", TRUE, FALSE, TRUE), c(TRUE, "", TRUE, FALSE, TRUE), c(TRUE,
      "", TRUE, FALSE, TRUE), c(TRUE, "", TRUE, FALSE, TRUE), c(TRUE, "", TRUE, FALSE,
      TRUE), c(TRUE, "", TRUE, FALSE, TRUE), c(TRUE, "", TRUE, FALSE, TRUE), c(TRUE,
      "", TRUE, FALSE, TRUE), c(TRUE, "", TRUE, FALSE, TRUE), c(TRUE, "", TRUE, FALSE,
      TRUE), c(TRUE, "", TRUE, FALSE, TRUE), c(TRUE, "", TRUE, FALSE, TRUE), c(TRUE,
      "", TRUE, FALSE, TRUE), c(TRUE, "", TRUE, FALSE, TRUE)), allow_no_input_binding_ = TRUE)
  app$set_inputs(calib_path = "calib.xlsx")
  app$click("calib")
  app$set_inputs(n_whales = 1)
  app$set_inputs(n_whales = 0)
  app$click("closeBtn")
  app$expect_values()
})
