library(shinytest2)

test_that("{shinytest2} recording: calib_gui_test", {
  local_app_support(test_path("../../inst/shiny-apps/Calib"))
  app <- AppDriver$new(test_path("../../inst/shiny-apps/Calib"), name = "calib_gui_test",
                       seed = 123, height = 911, width = 1619)

  # 1. Diretório
  app$click("path")
  app$wait_for_idle()
  calib_wd <- system.file("extdata", "calib_test", package = "MedidoR")
  app$set_inputs(wd = calib_wd)
  app$click("confirmBtn")
  app$wait_for_idle()

  # 2. Inicialização e Imagem
  app$click("import")
  img_calib_path <- system.file("extdata", "calib_test", "Molde_15M_1.png", package = "MedidoR")
  if (img_calib_path == "") stop("Calibration test image not found in the extdata folder.")
  app$upload_file(file = img_calib_path)
  app$wait_for_idle(timeout = 5000)

  # 3. Metadados
  app$set_inputs(objL = "2")
  app$set_inputs(ImageID = "15m_1")
  app$set_inputs(ImageRES = "4k")
  app$set_inputs(sw = "13.2")
  app$set_inputs(flen = "8.8")
  app$set_inputs(drone = "P4P")
  app$set_inputs(alt = 14.7)
  app$set_inputs(takeof = 1.8)
  app$set_inputs(Date = "2019-08-13")
  app$set_inputs(obs = "Observer 1")

  # 4. Simulação de medição (Encurtada para eficiência)
  app$set_inputs(plot_brush = c(1851.8, 2587.5, 1002.9, 1457.6, 506.8, 684.8, 333.0, 443.0, 506.8, 684.8, 333.0, 443.0, 1, 1, 0.9, 4096, 2294.7, -133.7, 59.0, 1049.7, 645.5, 58.0, character(0), character(0), "xy", "plot_brush", "imagePlot"), allow_no_input_binding_ = TRUE)
  app$click("crop", wait_ = FALSE)
  app$wait_for_idle()

  app$set_inputs(plot_click = c(2054.3, 1206.5, 340.8, 321.0, 340.8, 321.0, 1, 1, 1836.3, 2603.0, 1457.6, 1002.9, 59.0, 1049.7, 645.5, 58.0, character(0), character(0)), allow_no_input_binding_ = TRUE, priority_ = "event")
  app$set_inputs(plot_click = c(2370.1, 1203.4, 748.8, 317.0, 748.8, 317.0, 1, 1, 1836.3, 2603.0, 1457.6, 1002.9, 59.0, 1049.7, 645.5, 58.0, character(0), character(0)), allow_no_input_binding_ = TRUE, priority_ = "event")

  # 5. Salvar e Limpar
  app$click("saveBtn")
  app$wait_for_idle()

  app$click("clearBtn")
  app$wait_for_idle()

  app$click("confirm_reset")
  app$wait_for_idle()

  app$expect_values()
})
