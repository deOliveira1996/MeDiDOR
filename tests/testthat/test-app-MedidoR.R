library(shinytest2)

test_that("{shinytest2} recording: medidor_gui_test", {
  local_app_support(test_path("../../inst/shiny-apps/MedidoR"))
  app <- AppDriver$new(test_path("../../inst/shiny-apps/MedidoR"), name = "medidor_gui_test",
                       seed = 321, height = 911, width = 1619)

  # 1. Diretório
  app$click("path")
  app$wait_for_idle()
  medidor_wd <- system.file("extdata", "medidor_test", package = "MedidoR")
  app$set_inputs(wd = medidor_wd)
  app$click("confirmBtn")
  app$wait_for_idle()

  # 2. Inicialização e Imagem
  app$set_inputs(app_mode = "morpho")
  app$set_inputs(segments = "1")
  app$click("import")

  img_whale_path <- system.file("extdata", "medidor_test", "example_whale.png", package = "MedidoR")
  if (img_whale_path == "") stop("Whale test image not found in the extdata folder.")
  app$upload_file(file = img_whale_path)
  app$wait_for_idle(timeout = 10000)

  # 3. Metadados
  app$set_inputs(Species = "Eubalaena australis")
  app$set_inputs(ImageID = "RW-1")
  app$set_inputs(ImageRES = "2.7k")
  app$set_inputs(alt = 22.5)
  app$set_inputs(takeof = 0.5)
  app$set_inputs(Date = "2022-08-01")
  app$set_inputs(obs = "Observer 1")
  app$set_inputs(sw = "6.4")
  app$set_inputs(flen = "7.6")
  app$set_inputs(drone = "MA2")
  app$set_inputs(score = "1")

  # 4. Simulação de medição básica (Apenas os cliques cruciais)
  app$set_inputs(plot_brush = c(735.3, 2091.4, 553.7, 1107.0, 329.8, 829.8, 277.0, 481.0, 329.8, 829.8, 277.0, 481.0, 1, 1, 1, 2688, 1553.2, -40.2, 59.04, 1049.7, 645.5, 58.04, character(0), character(0), "xy", "plot_brush", "imagePlot"), allow_no_input_binding_ = TRUE)
  app$click("crop", wait_ = FALSE)
  app$wait_for_idle(timeout = 5000)

  app$set_inputs(plot_click = c(774.7, 815.6, 87.8, 341.0, 87.8, 341.0, 1, 1, 735.3, 2091.4, 1232.4, 428.3, 59.04, 1049.7, 645.5, 58.04, character(0), character(0)), allow_no_input_binding_ = TRUE, priority_ = "event")
  app$set_inputs(plot_click = c(1512.5, 852.6, 626.8, 368.0, 626.8, 368.0, 1, 1, 735.3, 2091.4, 1232.4, 428.3, 59.04, 1049.7, 645.5, 58.04, character(0), character(0)), allow_no_input_binding_ = TRUE, priority_ = "event")
  app$set_inputs(plot_click = c(1940.9, 833.4, 939.8, 354.0, 939.8, 354.0, 1, 1, 735.3, 2091.4, 1232.4, 428.3, 59.04, 1049.7, 645.5, 58.04, character(0), character(0)), allow_no_input_binding_ = TRUE, priority_ = "event")

  # 5. Salvar e Limpar
  app$click("saveBtn", wait_ = FALSE)
  app$wait_for_idle()

  app$click("clearBtn")
  app$wait_for_idle()

  app$click("confirm_reset")
  app$wait_for_idle()

  app$click("closeBtn")
  app$expect_values()
})
