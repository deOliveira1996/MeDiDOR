# Define the UI

ui <- shiny::fluidPage(
  htmltools::includeCSS("photogrammetry.css"),
  shiny::titlePanel("MeDiDOR"),
  shiny::sidebarLayout(
    shiny::sidebarPanel(
      shiny::wellPanel(
        shiny::fluidRow(shiny::strong("Input Data")),
        width = 2,
        shiny::actionButton("path", "Change working directory", width = "100%"),
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
            list("10% interval" = 1, "5% interval" = 2),
          selected = 2
        ),
        shiny::fluidRow(
          column(width = 6, actionButton("create", "CREATE", width = "100%")),
          column(width = 6, actionButton("import", "IMPORT", width = "100%"))
        ),
        shiny::helpText("The", strong("CREATE"), "button creates a data frame
                        (for measurements every 5% or 10% of the body length)"),
        shiny::helpText("The", strong("IMPORT"), "button imports a data frame
                        (for measurements every 5% or 10% of the body length)
                        already existing in the directory.")
      ),
      shiny::wellPanel(
        shiny::fluidRow(
          shiny::strong("Image parameters"),
          shiny::textInput("Species", "Species name:", placeholder = "Given Species name")
        ),
        shiny::textInput("ImageID", "Image-ID:", placeholder = "Given the Image-ID")
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
            shiny::numericInput("takeof", "Take-off Altitude (m)", 0)
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
            shiny::textInput("drone", "Drone model:", value = "")
          ),
        ),

        shiny::fluidRow(
          shiny::column(
            width = 6,
            shiny::textInput("Date", "Image collection date: YYYY-MM-DD", value = "")
          ),
          shiny::column(width = 6, shiny::textInput("obs", "Observer:", value = "")),
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

        shiny::actionButton("closeBtn", "Close application", width = "100%"),
      )
    ),

    shiny::mainPanel(
      width = 8,
      shiny::tabsetPanel(
        type = "tabs",
        shiny::tabPanel(
          "Instructions",
          shiny::h1("How to use the app ..."),
          shiny::p(
            "The",
            shiny::strong(shiny::em("MeDiDOR")),
            "API is a user
                    interface that was built using the Shiny App, a supplementary
                    package that enables creating an interactive environment for data
                    presentation and collection."
          ),
          shiny::p(
            shiny::strong(shiny::em("MeDiDOR")),
            "was developed with the aim of
                      optimizing
                    the analysis of aerial photogrammetry data using pixel scale
                    calibration models. The target audience for this API primarily
                    consists of researchers working on monitoring morphometric
                    patterns of marine megafauna."
          ),
          shiny::h2("First steps"),
          shiny::p(
            "If the application is opened in an R environment, the working
                  directory will be set to the directory where the file was stored.
                    You can change the working directory by clicking",
            shiny::strong(shiny::em("Change working directory")),
            "in the",
            shiny::strong(shiny::em("Input data")),
            "box."
          ),
          shiny::p(
            shiny::strong(
              "IMPORTANT !!! When the user opens the application for the
                           first time, the",
              shiny::strong("START button"),
              "must be pressed."
            )
          ),
          shiny::p(
            "Once the directory is set, clicking the START button for the first
                  time will create a spreadsheet (Measurements.xlsx).
                    This spreadsheet will be used throughout the applications usage to
                    store and analyze the collected measurements."
          ),
          shiny::p(
            "If the application has been started at least once before and/or if
                  the Measurements.xlsx file is in the working directory,
                    clicking the",
            shiny::strong("START"),
            "button will import the Measurements
                    .xlsx spreadsheet."
          ),
          shiny::p(
            "The",
            shiny::strong("INPUT"),
            "button should be used to upload the
                    desired image."
          ),
          shiny::p(shiny::strong("IMPORTANT !!!")),
          shiny::p(
            "Once the image is selected, a new window will open outside the R
                  environment with the image plotted in its original dimensions.
                    At this point, the user should select the area of interest by
                    clicking and dragging the mouse cursor on the image."
          ),
          shiny::p(
            "In the",
            shiny::strong(shiny::em("Image parameters")),
            "box, the user can add
                    some information to the image, such as the",
            shiny::strong(shiny::em("Species name")),
            ",",
            shiny::strong(shiny::em("Fligth Altitude (m)")),
            ",",
            shiny::strong(shiny::em("Take-off Altitude (m)")),
            "and",
            shiny::strong(shiny::em("Frame Score"))
          ),
          shiny::p(
            "Once the image is plotted, you can start collecting measurements.
                  The first three points should be marked along the body of the animal,
                  starting from the tip of the rostrum and extending to the caudal notch
                  (or the end of the body if the measured animal is not a cetacean)"
          ),
          shiny::p(
            "Next, red lines will connect the points, and perpendicular lines
                    (in blue) will define intervals at every 10% of the measured body
                    length. These lines will serve as guides to define the widths along
                    the body of the animal. Lastly, the ",
            shiny::strong(shiny::em("MeDiDOR")),
            " Will request the user to define
                    the fluke width measurements, this measurement will be highlighted
                    with a green line. Each marked point for width
                    definition will be identified with a yellow star."
          ),
          shiny::p(
            "The",
            shiny::strong("ADD IN"),
            "button will save the measurements to
                    the Measurements.xlsx spreadsheet, and the",
            shiny::strong("Clear Measurements"),
            "button will clear the collected
                    measurements to start a new measurement."
          ),
          shiny::p(
            "After the measurements have been collected and added to the
                    spreadsheet, it is possible to construct the scale calibration model
                    to estimate the measurements in meters of the animals in the",
            shiny::strong(" Calibration "),
            "tab. For this, it is necessary that the
                    working directory contains a spreadsheet named 'calib.xlsx.' This
                    spreadsheet must contain the pixel measurements of the object used
                    as a reference scale, the  collection date, the flight altitude,
                    takeoff altitude, and the actual length of the object. With this
                    information, the",
            shiny::strong(shiny::em('MeDiDOR')),
            "will be able
                    to construct the model."
          ),
          shiny::p(
            "When the model is built, three plots containing the model results
                    will be generated. The first plot will display diagnostic graphs,
                    allowing evaluation of whether the model has been statistically
                    validated. The second graph shows the accuracy results of the model,
                    including R², RMSE, and MAE values, as well as the distribution and
                    mean of the estimates of the length of the reference object
                    considering the model calibration. Finally, the correlation graph
                    between the model-predicted values and flight altitudes is
                    presented."
          ),
          shiny::p(
            "In the",
            shiny::strong(' Measured whale '),
            "tab, the user can visualize
                    the distribution of collected measurements for each segment and each
                    measured animal. Additionally, in this tab, it is possible to define
                    the number of samples (Number of whales) that the user wants to see
                    in the histograms."
          ),
          shiny::p(
            "The collected measurements are automatically stored and updated in
                    the 'Dataframe' tab, where the user can view, search, and define how
                    many rows will be displayed."
          ),
          shiny::p(shiny::strong("IMPORTANT !!!")),
          shiny::p(
            "Upon clicking the start button, PhotogrammetrGUI creates the
                    'Measurements.xlsx' spreadsheet, where the collected measurements
                    are stored (blank spreadsheet). As the user begins measuring the
                    animals, another spreadsheet is created ('Measurements_1.xlsx'), and
                    only in this spreadsheet will the measurements estimated by the
                    model be contained. This is the spreadsheet that should be used for
                    data analysis."
          ),
          shiny::p(
            "Now that you've learned how to use",
            shiny::strong(shiny::em(" PhotogrammetryGUI")),
            ", have fun!"
          ),
        ),
        shiny::tabPanel(
          "Image plot",
          shiny::plotOutput("imagePlot", height = "600",
                            click = "plot_click", brush = "crop"),
          shiny::actionButton("cropBtn", "CROP", width = "33%"),
          shiny::actionButton("saveBtn", "ADD IN", width = "33%"),
          shiny::actionButton("clearBtn", "Clear Measurements", width = "33%")
        ),

        shiny::tabPanel(
          "Dataframe",
          shiny::p(),
          shinycssloaders::withSpinner(
            DT::dataTableOutput("mTable"),
            type = getOption("spinner.type", default = 4)
          )
        ),
        shiny::tabPanel(
          "Calibration",
          shiny::p(),
          shiny::actionButton("calib", "Run model calibration"),
          shiny::p(),
          shiny::wellPanel(
            shiny::fluidRow(shiny::strong("Diagnostic plot")),
            shiny::p(),
            shinycssloaders::withSpinner(
              shiny::plotOutput("checkm", height = "600"),
              type = getOption("spinner.type", default = 4)
            )
          ),
          shiny::p(),
          shiny::wellPanel(
            shiny::fluidRow(shiny::strong("Accuracy plot")),
            shiny::p(),
            shinycssloaders::withSpinner(
              shiny::plotOutput("meanplot", height = "600"),
              type = getOption("spinner.type", default = 4)
            )
          ),
          shiny::wellPanel(
            shiny::fluidRow(shiny::strong("Regression plot")),
            shiny::p(),
            shinycssloaders::withSpinner(
              shiny::plotOutput("mplot", height = "600"),
              type = getOption("spinner.type", default = 4)
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
            shiny::fluidRow(shiny::strong("Pixel measurements")),
            shiny::p(),
            shinycssloaders::withSpinner(shiny::plotOutput("lplot"), type = getOption("spinner.type", default = 4))
          ),
          shiny::wellPanel(
            shiny::fluidRow(shiny::strong("Estimated lengths (meters)")),
            shiny::p(),
            shinycssloaders::withSpinner(
              shiny::plotOutput("mwhale"),
              type = getOption("spinner.type", default = 4)
            )
            ,
          )
        ),
      )
    )
  )
)
