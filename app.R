library(shiny)
library(shinyFeedback)
library(shinythemes)
library(shinyWidgets)
library(dplyr, warn.conflicts = FALSE)
library(thematic)
library(stringr)
library(glue)
library(ggplot2)
library(hrbrthemes)
library(shinycustomloader)
library(shinyjs)

ui <- fluidPage(
  useShinyFeedback(),
  useShinyjs(),
  theme = shinythemes::shinytheme("spacelab"),
  #shinythemes::themeSelector(),
  titlePanel("PiedPiper", windowTitle = "PiedPiper"),
  fluidRow(
    column(width = 3,
           wellPanel(
             textInput("name", "Rat name", placeholder = "Blue4"),
             numericInput("weight", "Weight (g)", value = 0, min = 0),
             textAreaInput("observations", "Observations from run", rows = 5,
                           placeholder = "Good hits. Good misses.\nOnly real FAs.\n2 CRs back to back.\nBreak 35-47m with no jams."),
             textInput("exclude_trials", "Trials to skip", placeholder = "2, 120-126, 201 (or blank)")
           ),
           wellPanel(
             textInput("scientist", "Your Name", placeholder = "Noëlle"),
           ),
    ),
    column(width = 9,
      fillRow(height = "100px", width = "1250px", # want 625 for file input plus margin, so 625 x2 = 1250
        fileInput("matfile", "Select .mat file:", buttonLabel = "Browse...", accept = c(".mat"), width = "600px"),
        actionButton("btnAnalyze", span("Analyze", id = "UpdateAnimate", class = ""), class = "btn btn-primary", style = "margin-top: 25px;", width = "150px"),
      ),
      textOutput("requirements"),
      textOutput("text1"),
      textOutput("text2"),
      withLoader(tableOutput("warnings"), type = "html", loader = "dnaspin"),
      plotOutput("plotWeight"),
      conditionalPanel(
        condition = "output.plotWeight",
        span(tags$div(
          "Does this graph look OK?", tags$br(),
          "Are there problems with weight or trial trends?", tags$br(),
          "Does today's weight look like it belongs?", tags$br(),
        ), style="font-weight:bold"),
        fluidRow(
          column(width = 3,
            actionButton("weightOK", "OK", width = "100%"),
          ),
          column(width = 2,
            actionButton("weightProblem", "Problem", width = "100%"),
          ),
          column(width = 8,
            conditionalPanel(
              condition = "output.weightProblem",
              span(textInput("weightAction", NULL, placeholder = "To correct [description of problem], I will [take these actions]...", width = "95%"), style = "margin-top: -20px;")
            ),
          )
        ),
      ),
      plotOutput("plotRxn"),
    ),
  ),
  tags$style(HTML("tbody { color: #DD0000; font-family: monospace; white-space: pre}")),
  # button animation
  tags$head(tags$style(type="text/css", '
            .loading {
                display: inline-block;
                overflow: hidden;
                height: 1.3em;
                margin-top: -0.3em;
                line-height: 1.5em;
                vertical-align: text-bottom;
                box-sizing: border-box;
            }
            .loading.dots::after {
                text-rendering: geometricPrecision;
                content: "⠋\\A⠙\\A⠹\\A⠸\\A⠼\\A⠴\\A⠦\\A⠧\\A⠇\\A⠏";
                animation: spin10 1s steps(10) infinite;
                animation-duration: 1s;
                animation-timing-function: steps(10);
                animation-delay: 0s;
                animation-iteration-count: infinite;
                animation-direction: normal;
                animation-fill-mode: none;
                animation-play-state: running;
                animation-name: spin10;
            }
            .loading::after {
                display: inline-table;
                white-space: pre;
                text-align: left;
            }
            @keyframes spin10 { to { transform: translateY(-15.0em); } }
            '))
)

server <- function(input, output, session) {
  # Name validation
  rats = rat_archive$Rat_name %>% str_to_upper()
  name_clean <- reactive({
    input$name %>% str_trim %>% str_replace_all(" ", "") %>% str_to_upper()
  })
  name_good <- reactive({
    req(name_clean() %in% rats, cancelOutput = TRUE)
    name_clean()
  })
  id_good <- reactive({
    rat_archive %>% dplyr::filter(str_to_upper(Rat_name) == name_good()) %>% .$Rat_ID
  })
  known_rat <- reactive ({
    name_clean() %in% rats
  })
  observeEvent(input$name, {
    alnum = name_clean() %>% str_detect("^[:alnum:]*$")
    if(!alnum) {
      hideFeedback("name")
      showFeedbackWarning("name", "Must contain only letters, numbers, and spaces.")
    } else if (known_rat()) {
      hideFeedback("name")
      showFeedbackSuccess("name", "Rat exists in Rat Archive.")
    } else {
      hideFeedback("name")
    }
  })

  # Weight validation
  last_weight <- reactive({
    run_archive %>% dplyr::filter(rat_ID == id_good()) %>% dplyr::arrange(date) %>% tail(1) %>% .$weight
  })
  weight_change_percent <- reactive({
    if(is.na(input$weight) || input$weight == 0) -1 else abs((input$weight - last_weight()) / last_weight())
  })
  observeEvent(weight_change_percent(), {
    if(weight_change_percent() == -1) {
      hideFeedback("weight")
    } else if(weight_change_percent() > user_settings$maximum_weight_change_daily_percent) {
      hideFeedback("weight")
      showFeedbackWarning("weight", glue("Differs by {round(weight_change_percent() * 100, digits = 0)}% from prior {last_weight()}g."))
    } else {
      hideFeedback("weight")
      showFeedbackSuccess("weight", glue("Within {user_settings$maximum_weight_change_daily_percent * 100}% of prior {last_weight()}g."))
    }
  })

  # File validation
  matname_clean <- reactive({
    stringr::str_match_all(input$matfile$name, pattern="^.+?(?=_)") %>%
      unlist(recursive = TRUE) %>%
      tail (n = 1) %>%
      str_trim %>% str_replace_all(" ", "") %>% str_to_upper()
  })

  observeEvent(ignoreInit = TRUE, c(input$name, input$matfile), {
    req(input$name)
    req(input$matfile)
    if(tools::file_ext(input$matfile$datapath) != "mat") {
      hideFeedback("matfile")
      showFeedbackDanger("matfile", "The file must be .mat format.")
    } else if(name_clean() == matname_clean()) {
      hideFeedback("matfile")
      showFeedbackSuccess("matfile", "Filename agrees with rat name.")
    } else {
      hideFeedback("matfile")
      showFeedbackWarning("matfile", glue("This file appears to belong to {matname_clean()}, not {name_clean()}."))
    }
  })

  observeEvent(ignoreInit = TRUE, input$observations, {
    if (input$observations != "") hideFeedback("observations")
  })

  observeEvent(input$matfile, {
    if(tools::file_ext(input$matfile$datapath) != "mat") {
      hideFeedback("matfile")
      showFeedbackDanger("matfile", "The file must be .mat format.")
    }
    else {
      hideFeedback("matfile")
    }
  })

  # Submit validation
  v <- reactiveValues(pushed = FALSE, row = NULL, finished = FALSE, weightProblem = FALSE)

  requirements <- reactive({
    if(input$name == "") {
      hideFeedback("name")
      showFeedbackDanger("name", "Required.")
    }
    if(is.na(input$weight) || input$weight < 1) {
      hideFeedback("weight")
      showFeedbackDanger("weight", "Must be >0.")
    }
    if(input$observations == "") {
      hideFeedback("observations")
      showFeedbackDanger("observations", "Required.")
    }
    if(is.null(input$matfile)) {
      hideFeedback("matfile")
      showFeedbackDanger("matfile", "No file selected!")
    }
    validate(
      need(input$name, "Rat name is required."),
      need(input$weight > 0, "Weight must be greater than zero."),
      need(input$observations, "Observations must be provided."),
      need(input$matfile, "No file selected!")
    )
    "Ready for analysis."
  })

  observeEvent(input$btnAnalyze, {
    v$pushed = FALSE
    if (requirements() == "Ready for analysis.") {
      v$pushed = TRUE

      # start animation and disable inputs
      shinyjs::addClass(id = "UpdateAnimate", class = "loading dots")
      shinyjs::disable("btnAnalyze")
      shinyjs::disable("name")
      shinyjs::disable("weight")
      shinyjs::disable("observations")
      shinyjs::disable("exclude_trials")
      shinyjs::disable("matfile")
    }
  })

  output$requirements <- renderText({
    req(input$btnAnalyze)
    requirements()
    if(isolate(v$pushed)) {
      v$pushed = FALSE
      v$row = Process_File(file_to_load = input$matfile$datapath, name = input$name, weight = input$weight, observations = input$observations, exclude_trials = input$exclude_trials, file_name_override = input$matfile$name, use_shiny = TRUE)
      "Finished."
      # stop animation and reenable button
      shinyjs::removeClass(id = "UpdateAnimate", class = "loading dots")
      # shinyjs::enable("btnAnalyze")
    }
    else(requirements())
  })

  # output$text2 <- renderText({
  #   input$weightYes
  # })

  # output$text3 <- renderText({
  #   req(input$btnAnalyze)
  #   req(v$row)
  #   showModal(modalDialog(
  #     title = "Important message",
  #     "This is an important message!"
  #   ))
  # })

  output$warnings <- renderTable(striped = TRUE, hover = TRUE, sanitize.text.function = identity,
  {
    req(input$btnAnalyze)
    req(v$row)
    v$row %>% .$warnings_list %>% unlist() %>% data.frame(Warnings = .) %>% mutate(Warnings = str_replace_all(Warnings, "\\n", "<br> "))
  })

  output$plotWeight <- renderPlot({
    req(input$btnAnalyze)
    req(v$row) #TODO at the moment I'm constructing a fake vrow to feed to warnings for when the file is already loaded, but this thinks it has a real one and dies. Check existence of columns or something and skip evaluation if they're absent.
    rat_name = v$row %>% .$rat_name
    rat_ID = v$row %>% .$rat_ID
    Generate_Weight_Trials_Graph(rat_name, rat_ID)
  })

  output$weightProblem <- reactive({v$weightProblem})
  outputOptions(output, "weightProblem", suspendWhenHidden = FALSE)
  observeEvent(input$weightOK, {v$weightProblem = FALSE})
  observeEvent(input$weightProblem, {v$weightProblem = TRUE})

  output$plotWeight <- renderPlot({
    req(input$btnAnalyze)
    req(v$row) #TODO at the moment I'm constructing a fake vrow to feed to warnings for when the file is already loaded, but this thinks it has a real one and dies. Check existence of columns or something and skip evaluation if they're absent.
    rat_name = v$row %>% .$rat_name
    rat_ID = v$row %>% .$rat_ID
    Generate_Weight_Trials_Graph(rat_name, rat_ID)
  })


}


source(paste0(projects_folder, "main.R"))
options(shiny.host = "127.0.0.1") #setting this to an external IP address forces browser instead of rstudio window
shinyApp(ui, server)
