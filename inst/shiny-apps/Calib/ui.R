# Define the UI

ui <- shiny::fluidPage(
  htmltools::includeCSS("photogrammetry.css"),
  shiny::titlePanel("MedidoR - Scale measurements API"),
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
          "Select an SCALE IMAGE",
          accept = c(".png", ".jpeg", ".jpg",
                     ".bmp", ".gif", ".tiff")
        ),
        shiny::fluidRow(
          shiny::column(width = 6,
                 actionButton("create",
                              "CREATE",
                              width = "100%")),
          shiny::column(width = 6,
                 actionButton("import",
                              "IMPORT",
                              width = "100%")),
          shiny::helpText("The", strong("CREATE"),
                          "button creates a calib.xlsx data frame"),
          shiny::helpText("The", strong("IMPORT"),
                          "button imports a existing calib.xlsx data frame already existing
                          in the directory.")
        )
      ),
      shiny::wellPanel(
        shiny::fluidRow(
          shiny::strong("Scale parameters"),
          shiny::p(),
          shiny::column(
            width = 6,
            shiny::textInput("objL", "Scale length (m):",
                             placeholder = "True scale length (m)",
                             value = NULL),
            shiny::textInput("ImageID", "Image-ID:",
                             placeholder = "Given the Image-ID",
                             value = NULL),
            shiny::textInput("ImageRES", "Image Resolution:",
                             placeholder = "Given the Resolution",
                             value = NULL)
          ),
          shiny::column(
            width = 6,
            shiny::textInput("sw", "Camera sensor width (mm):",
                             placeholder = "Sensor width (mm)",
                             value = NULL),
            shiny::textInput("flen", "Camera focal length (mm):",
                             placeholder = "Focal length (mm)",
                             value = NULL),
            shiny::textInput("drone",
                             "Drone model:",
                             value = NULL)
          )
        )
      ),
      shiny::fluidRow(
        shiny::column(
          width = 6,
          shiny::numericInput(
            "alt",
            "Fligth Altitude (m)",
            10,
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
          shiny::textInput("Date",
                           "Image collection date: YYYY-MM-DD",
                           value = ""),
          shiny::textInput("obs",
                           "Observer:",
                           value = "")
        )
      ),
      shiny::fluidRow(
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
            shiny::h1("MedidoR Scale Measurements User Guide", style = "color: #00A8FF;"),
            shiny::div(
              style = "max-width: 800px; margin: auto;",

              shiny::h3("Application Purpose"),
              shiny::p("This application helps you measure calibration objects in aerial images to establish accurate scale references for photogrammetric analysis."),

              shiny::h3("Step-by-Step Guide"),

              shiny::h4("1. Set Working Directory"),
              shiny::tags$ol(
                shiny::tags$li("Click the 'Set Directory' button"),
                shiny::tags$li("Enter the path where your calibration file will be stored/accessed"),
                shiny::tags$li("Confirm your selection")
              ),

              shiny::h4("2. Initialize Data"),
              shiny::tags$ul(
                shiny::tags$li(shiny::strong("Create:"), "Click to create a new calibration file (if none exists)"),
                shiny::tags$li(shiny::strong("Import:"), "Click to import an existing calibration file")
              ),

              shiny::h4("3. Load and Process Image"),
              shiny::tags$ol(
                shiny::tags$li("Upload your aerial image using the file selector"),
                shiny::tags$li("Select the area containing your calibration object by clicking and dragging on the image"),
                shiny::tags$li("Click 'Crop' to focus on the selected area")
              ),

              shiny::h4("4. Measure Calibration Object"),
              shiny::tags$ol(
                shiny::tags$li("Click on both ends of your known-length calibration object (e.g., scale bar)"),
                shiny::tags$li("A red line will appear connecting your measurement points"),
                shiny::tags$li("Verify the measurement matches the physical object length")
              ),

              shiny::h4("5. Enter Metadata"),
              shiny::p("Fill in all required information:"),
              shiny::tags$ul(
                shiny::tags$li("Image ID and date"),
                shiny::tags$li("Drone model and camera specifications"),
                shiny::tags$li("Flight altitude and ground sampling distance"),
                shiny::tags$li("Actual length of the calibration object (in meters)")
              ),

              shiny::h4("6. Save Measurements"),
              shiny::tags$ol(
                shiny::tags$li("Click 'Add-IN' to save your measurements to the calibration file"),
                shiny::tags$li("Verify the data appears in the table below"),
                shiny::tags$li("Add any additional comments if needed")
              ),

              shiny::h4("Additional Features"),
              shiny::tags$ul(
                shiny::tags$li(shiny::strong("Clear:"), "Reset current measurements without saving"),
                shiny::tags$li(shiny::strong("Close:"), "Exit the application")
              ),

              shiny::h3("Best Practices"),
              shiny::tags$ul(
                shiny::tags$li("Use high-contrast calibration objects for better measurement accuracy"),
                shiny::tags$li("Measure objects that are on the same plane as your subject"),
                shiny::tags$li("Include multiple scale measurements in different image locations"),
                shiny::tags$li("Verify your camera parameters (focal length, sensor width) are accurate")
              ),

              shiny::h3("Troubleshooting"),
              shiny::tags$ul(
                shiny::tags$li("If measurements don't appear, ensure you've completed the crop step"),
                shiny::tags$li("Check that your working directory has write permissions"),
                shiny::tags$li("Verify image format compatibility (JPEG, PNG, TIFF)")
              )
            )
          ) ,
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
          )
        )
      )
    )
  )
