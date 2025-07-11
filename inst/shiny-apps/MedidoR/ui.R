# Define the UI

ui <- shiny::fluidPage(
  htmltools::includeCSS("photogrammetry.css"),
  shiny::titlePanel("MedidoR"),
  shiny::sidebarLayout(
    shiny::sidebarPanel(
      shiny::wellPanel(
        shiny::fluidRow(shiny::strong("Input Data")),
        width = 2,
        shiny::actionButton("path",
                            "Change working directory",
                            width = "100%"),
        shiny::fileInput(
          "file",
          "Select an image file",
          accept = c(".png", ".jpeg", ".jpg",
                     ".bmp", ".gif", ".tiff")
        ),
        shiny::radioButtons(
          "segments",
          "Select the desired width interval to CREATE or IMPORT the dataset:",
          choices =
            list("10% interval" = 1, "05% interval" = 2),
          selected = 2
        ),
        shiny::fluidRow(
          column(width = 6,
                 actionButton("create",
                              "CREATE",
                              width = "100%")),
          column(width = 6,
                 actionButton("import",
                              "IMPORT",
                              width = "100%")),
          shiny::helpText("The", strong("CREATE"), "button creates a data frame
                        (for measurements every 5% or 10% of the body length)"),
          shiny::helpText("The", strong("IMPORT"), "button imports a data frame
                        (for measurements every 5% or 10% of the body length)
                        already existing in the directory.")
        )
      ),
      shiny::wellPanel(
        shiny::fluidRow(
          shiny::strong("Image parameters"),
          shiny::p(),
          shiny::textInput("Species", "Species name:",
                           placeholder = "Given Species name"),
          shiny::textInput("ImageID", "Image-ID:",
                           placeholder = "Given the Image-ID"),
        )
      ),
        shiny::fluidRow(
          shiny::column(
            width = 6,
            shiny::numericInput(
              "alt",
              "Fligth Altitude (m)",
              20,
              min = 5,
              max = 150,
              step = 5
            ),
            shiny::br(),
            shiny::numericInput("takeof",
                                "Take-off Altitude (m)",
                                value = 0)
          ),
          shiny::column(
            width = 6,
            shiny::radioButtons(
              "score",
              "Frame Score:",
              choices =
                list(
                  "Good" = 1,
                  "Moderate" = 2,
                  "Bad" = 3,
                  "Not assingned" = 4
                ),
              selected = 4
            ),
            shiny::textInput("drone",
                             "Drone model:",
                             value = "")
          ),
        ),

        shiny::fluidRow(
          shiny::column(
            width = 6,
            shiny::textInput("Date",
                             "Image collection date: YYYY-MM-DD",
                             value = "")
          ),
          shiny::column(width = 6,
                        shiny::textInput("obs",
                                         "Observer:",
                                         value = "")),
          shiny::column(
            width = 12,
            shiny::textAreaInput(
              "comments",
              "Comments here:",
              value = "",
              resize = "none"
            )
          )
        ),

        shiny::actionButton("closeBtn",
                            "Close application",
                            width = "100%")
      ),

    shiny::mainPanel(
      width = 8,
      shiny::tabsetPanel(
        type = "tabs",
        shiny::tabPanel(
          "Instructions",
          shiny::h1("MedidoR User Guide", style = "color: #000000;"),  # Black

          # Introduction Section
          shiny::div(
            style = "background-color: #696969; padding: 15px; border-radius: 5px; margin-bottom: 20px;",  # Darkgrey
            shiny::h2("Introduction", style = "color: #0047AB;"),  # Blue
            shiny::p(
              shiny::strong(shiny::em("MedidoR")),
              "is an interactive photogrammetry tool for marine megafauna research that enables:"
            ),
            shiny::tags$ul(
              shiny::tags$li("Precise morphometric measurements from drone imagery"),
              shiny::tags$li("Pixel-to-real-world conversion through calibration"),
              shiny::tags$li("Standardized data collection and quality control")
            )
          ),
          # Workflow Overview
          shiny::div(
            style = "border-left: 4px solid #0066cc; padding-left: 15px; margin-bottom: 20px; background-color: #696969;",  # Darkgrey
            shiny::h2("Workflow Overview", style = "color: #0047AB;"),  # Blue
            shiny::h3("1. Initial Setup", style = "color: #000000;"),  # Black
            shiny::tags$ul(
              shiny::tags$li("Set working directory via the interface"),
              shiny::tags$li("Choose segment interval (5% or 10% density)")
            ),

            shiny::h3("2. Data Management", style = "color: #000000;"),  # Black
            shiny::tags$ul(
              shiny::tags$li(shiny::strong("First-time users:"), "Create new measurement templates"),
              shiny::tags$li(shiny::strong("Returning users:"), "Import existing datasets"),
              shiny::tags$li("Data stored in standardized Excel formats")
            ),

            shiny::h3("3. Image Processing", style = "color: #000000;"),  # Black
            shiny::tags$ul(
              shiny::tags$li("Load drone images (JPG/PNG)"),
              shiny::tags$li("Interactive crop tool for ROI selection")
            )
          ),

          # Measurement Section
          shiny::div(
            style = "background-color: #696969; padding: 15px; border-radius: 5px; margin-bottom: 20px;",  # Darkgrey
            shiny::h2("Measurement Protocol", style = "color: #0047AB;"),  # Blue

            shiny::h3("Length Measurement", style = "color: #000000;"),  # Black
            shiny::tags$ol(
              shiny::tags$li("Click 3 points along body axis (rostrum to caudal notch)"),
              shiny::tags$li("Automatic generation of:",
                             shiny::tags$ul(
                               shiny::tags$li("Main measurement line (red)"),
                               shiny::tags$li("Perpendicular guides (blue dashed)")
                             )
              )
            ),

            shiny::h3("Width Measurement", style = "color: #000000;"),  # Black
            shiny::tags$ul(
              shiny::tags$li(shiny::strong("10% intervals:"), "18 width points (9 segments)"),
              shiny::tags$li(shiny::strong("5% intervals:"), "38 width points (19 segments)"),
              shiny::tags$li("Click point pairs on each perpendicular line")
            ),

            shiny::h3("Fluke Measurement", style = "color: #000000;"),  # Black
            shiny::tags$ul(
              shiny::tags$li("Final 2 clicks to measure tail fluke width"),
              shiny::tags$li("Highlighted in purple with special marker")
            ),

            shiny::div(
              style = "background-color: #ff9933; padding: 10px; border-radius: 5px; margin-top: 10px;",  # Orange
              shiny::h4("Pro Tips:", style = "color: #000000;"),  # Black text on orange
              shiny::tags$ul(
                shiny::tags$li("Use high-contrast images for better point selection"),
                shiny::tags$li("Zoom in for precise fluke width measurement"),
                shiny::tags$li("Save frequently with", shiny::strong("Add-IN"))
              )
            )
          ),

          # Data & Calibration Section
          shiny::div(
            style = "border-top: 2px solid #0066cc; padding-top: 15px; background-color: #696969;",  # Darkgrey
            shiny::h2("Data Management & Calibration", style = "color: #0047AB;"),  # Blue

            shiny::h3("Metadata Entry", style = "color: #000000;"),  # Black
            shiny::tags$ul(
              shiny::tags$li("Complete all fields in", shiny::em("Image Parameters")),
              shiny::tags$li("Essential fields:",
                             shiny::tags$ul(
                               shiny::tags$li("Species identification"),
                               shiny::tags$li("Flight altitude (m)"),
                               shiny::tags$li("Frame quality score (Good/Moderate/Bad)")
                             )
              )
            ),

            shiny::h3("Calibration Process", style = "color: #000000;"),  # Black
            shiny::tags$ol(
              shiny::tags$li("Prepare", shiny::strong("calib.xlsx"), "with:"),
              shiny::tags$ul(
                shiny::tags$li("Reference object measurements (pixels)"),
                shiny::tags$li("Actual lengths (meters)"),
                shiny::tags$li("Flight altitude data")
              ),
              shiny::tags$li("Upload in", shiny::strong("Calibration"), "tab"),
              shiny::tags$li("Review diagnostic plots for model validation")
            ),

            shiny::div(
              style = "background-color: #48494B; padding: 10px; border-radius: 5px; margin: 10px 0;",  # Dimgrey (darker)
              shiny::h4("File Structure:", style = "color: #000000;"),  # Black
              shiny::tags$ul(
                shiny::tags$li(shiny::strong("Measurements.xlsx:"), "Raw collected data"),
                shiny::tags$li(shiny::strong("Measurements_1.xlsx:"), "Model-adjusted values"),
                shiny::tags$li("Use *_1.xlsx for final analysis")
              )
            )
          ),

          # Visualization Section
          shiny::div(
            style = "margin-top: 20px; background-color: #696969; padding: 15px; border-radius: 5px;",  # Darkgrey
            shiny::h2("Results Visualization", style = "color: #0047AB;"),  # Blue

            shiny::h3("Measurement Explorer", style = "color: #000000;"),  # Black
            shiny::tags$ul(
              shiny::tags$li("View/compare individuals in", shiny::strong("Measured whale")),
              shiny::tags$li("Adjust sample size with", shiny::em("Number of whales"))
            ),

            shiny::h3("Data Tables", style = "color: #000000;"),  # Black
            shiny::tags$ul(
              shiny::tags$li("Full dataset available in", shiny::strong("Dataframe"), "tab"),
              shiny::tags$li("Search and filter capabilities")
            )
          ),
          # Footer
          shiny::div(
            style = "text-align: center; margin-top: 30px; padding: 10px; background-color: #696969;",  # Darkgrey
            shiny::p(
              style = "font-size: 1.1em; color: #000000;",  # Black
              "Now that you're familiar with",
              shiny::strong(shiny::em("MedidoR")),
              ", start exploring!"
            ),
            shiny::p(
              style = "font-size: 0.9em; color: #000000;",  # Black
              "For advanced usage, refer to the package documentation"
            )
          )
        ),
        shiny::tabPanel(
          "Image plot",
          uiOutput("crop_status"),
          fluidRow(
            shiny::plotOutput(
              "imagePlot",
              height = "720",
              width = "1080",
              click = "plot_click",
              brush = brushOpts(
                id = "plot_brush",
                resetOnNew = T,
                opacity = 0.1,
                clip = T
              )
            )),
          uiOutput("add_status"),
          # Modified button layout
          div(style = "margin-top: 20px;",
              shiny::actionButton("crop", "CROP", width = "32%"),
              shiny::actionButton("saveBtn", "ADD IN", width = "32%"),
              shiny::actionButton("clearBtn", "CLEAR", width = "32%")
          )
        ),

        shiny::tabPanel(
          "Dataframe",
          shiny::p(),
          shinycssloaders::withSpinner(
            DT::dataTableOutput("mTable"),
            type = getOption("spinner.type",
                             default = 4)
          )
        ),

        shiny::tabPanel(
          "Calibration",
          shiny::textInput(inputId = "calib_path",
                           label = "Calibration data path",
                           value = "",
                           placeholder = "Set the desired calibration data path (.xlsx)",
                           width = "75%"
                           ),
          shiny::p(),
          shiny::actionButton("calib",
                              "RUN Calibration",
                              width = "50%"),
          shiny::radioButtons("save_plot",
                              "Save plots ?",
                              choices =
                                list("Yes" = "Y",
                                     "No" = "N"),
                              selected = "Y"
                              ),
          shiny::p(),
          shiny::wellPanel(
            shiny::fluidRow(shiny::strong("Diagnostic plot")),
            shiny::p(),
            shinycssloaders::withSpinner(
              shiny::plotOutput("checkm",
                                height = "600"),
              type = getOption("spinner.type",
                               default = 4)
            )
          ),
          shiny::p(),
          shiny::wellPanel(
            shiny::fluidRow(shiny::strong("Accuracy plot")),
            shiny::p(),
            shinycssloaders::withSpinner(
              shiny::plotOutput("variance",
                                height = "600"),
              type = getOption("spinner.type",
                               default = 4)
            )
          ),
          shiny::wellPanel(
            shiny::fluidRow(shiny::strong("Regression plot")),
            shiny::p(),
            shinycssloaders::withSpinner(
              shiny::plotOutput("mplot",
                                height = "600"),
              type = getOption("spinner.type",
                               default = 4)
            )
          )
        ),

        shiny::tabPanel(
          "Measured Whales",
          shiny::h1("Measurement distribution histogram"),
          shiny::sliderInput(
            "n_whales",
            "How many samples to show ?",
            min = 0,
            max = 100,
            value = 0
          ),
          shiny::p(),
          shiny::wellPanel(
            shiny::fluidRow(
              shiny::strong("Pixel measurements")),
            shiny::p(),
            shinycssloaders::withSpinner(
              shiny::plotOutput("lplot"),
              type = getOption("spinner.type",
                               default = 4)
              )
          ),
          shiny::wellPanel(
            shiny::fluidRow(
              shiny::strong("Estimated lengths (meters)")),
            shiny::p(),
            shinycssloaders::withSpinner(
              shiny::plotOutput("mwhale"),
              type = getOption("spinner.type",
                               default = 4)
            )
          )
        )
      )
    )
  )
)
