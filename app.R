library(shiny)
library(shinyFeedback)
library(shinythemes)
library(shinyWidgets)
library(shinycustomloader)
library(shinyjs)
library(rlang)
library(dplyr, warn.conflicts = FALSE)
library(stringr)
library(glue)
library(ggplot2)
library(hrbrthemes)

ui <- fluidPage(
  useShinyFeedback(),
  useShinyjs(),
  theme = shinythemes::shinytheme("spacelab"),
  #shinythemes::themeSelector(),
  titlePanel("PiedPiper", windowTitle = "PiedPiper"),
  #textOutput("text2"),
  fluidRow(
    column(width = 2,
           div(id = "form1",
               wellPanel(
                 textInput("name", "Rat name", placeholder = "Blue4"),
                 numericInput("weight", "Weight (g)", value = 0, min = 0),
                 textAreaInput("observations", "Observations from run", rows = 5,
                               placeholder = "Good hits. Good misses.\nOnly real FAs.\n2 CRs back to back.\nBreak 35-47m with no jams."),
                 textInput("exclude_trials", "Trials to skip", placeholder = "2, 120-126, 201 (or blank)")
               ),
           ),
           wellPanel(
             textInput("scientist", "Your Name", placeholder = "Noëlle"),
           ),
    ),
    div(id = "form2",
                column(width = 10,
               fluidRow(
                 column(width = 5,
                        fileInput("matfile", "Select .mat file:", buttonLabel = "Browse...", accept = c(".mat"), width = "600px"),
                 ),
                 column(width = 2,
                        actionButton("btnAnalyze", span("Analyze", id = "UpdateAnalyze", class = ""), class = "btn btn-primary", style = "margin-top: 25px;", width = "150px"),
                 ),
                 column(width = 5,
                        withLoader(tableOutput("warnings"), type = "html", loader = "dnaspin", proxy.height = "100px"),
               ),
               ),
               textOutput("requirements"),
               conditionalPanel(
                 condition = "output.plotWeight",
                 h3(textOutput("displaycomment")),
               ),
               conditionalPanel(
                  condition = "output.fatal_error == 'TRUE'",
                 actionButton("btnReset", label = "Reset Form", class = "btn btn-danger", width = "250px"),
               ),
               fluidRow(
                 column(width = 6,
                        br(),br(),
                        plotOutput("plotWeight", height = "500px"),
                        conditionalPanel(
                          condition = "output.plotWeight",
                          span(tags$div(
                            "Is there a problem with these trends?", #tags$br(),
                            "Does today's weight look correct?", #tags$br(),
                          ), style="font-weight:bold"),
                          fluidRow(
                            column(width = 3,
                                   radioGroupButtons(
                                     inputId = "weightCheck",
                                     label = NULL,
                                     choices = c("OK", "Problem"), #changing this text changes the conditional below and in server
                                     selected = character(0),
                                     justified = TRUE,
                                     status = "primary",
                                   ),
                            ),
                            column(width = 9,
                                   conditionalPanel(
                                     condition = "input.weightCheck == 'Problem'",
                                     span(textInput("weightProblem", NULL, placeholder = "To correct [description of problem], I will [take these actions]...", width = "90%"), style = "margin-top: -20px;")
                                   ),
                            ),
                          ),
                        ),
                 ),
                 column(width = 6,
                        conditionalPanel(
                          condition = "output.plotWeight",
                          tabsetPanel(type = "tabs",
                                      tabPanel("Reaction Speed", withLoader(plotOutput("plotRxn", height = "500px"), type = "html", loader = "dnaspin"),),
                                      tabPanel("Sensitivity", withLoader(plotOutput("plotDprime", height = "500px"), type = "html", loader = "dnaspin"),),
                          ),
                          conditionalPanel(
                            condition = "output.plotWeight",
                            span(tags$div(
                              "Does it look like the rat performed as usual today?", #tags$br(),
                            ), style="font-weight:bold"),
                            fluidRow(
                              column(width = 3,
                                     radioGroupButtons(
                                       inputId = "rxnCheck",
                                       label = NULL,
                                       choices = c("OK", "Problem"), #changing this text changes the conditional below and in server
                                       selected = character(0),
                                       justified = TRUE,
                                       status = "primary",
                                     ),
                              ),
                              column(width = 9,
                                     conditionalPanel(
                                       condition = "input.rxnCheck == 'Problem'",
                                       span(textInput("rxnProblem", NULL, placeholder = "Today's data is concerning because....", width = "90%"), style = "margin-top: -20px;")
                                     ),
                              ),
                            ),
                          ),
                        ),
                 ),
               ),
               conditionalPanel(
                 condition = "(input.weightCheck != null) && (input.rxnCheck != null)",
                 actionButton("btnSave", span("Save Run", id = "UpdateSave", class = ""), class = "btn btn-success", style = "margin-top: 25px;", width = "450px"),
                 textOutput("requirements_for_save")
               ),
        ),
    ),
    div(id = "form3",
                 column(width = 2,
                        actionButton("btnClose", span("Exit", id = "CloseAnalyze", class = ""), 
                                     icon = icon("xmark"),
                                     class = "btn btn-danger", style = "margin-top: 5px;", width = "75px"),
                 )
             )
  ),
  
  # file input reset
  tags$script('
    Shiny.addCustomMessageHandler("resetFileInputHandler", function(x) {
        var id = "#" + x + "_progress";      # name of progress bar is file1_progress
        var idBar = id + " .bar";
        $(id).css("visibility", "hidden");   # change visibility
        $(idBar).css("width", "0%");         # reset bar to 0%
    });
  '),
  
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
            .greenbody tbody {
              color: #00BB00;
              font-family: monospace;
              white-space: pre
            }
            .redbody tbody {
              color: #DD0000;
              font-family: monospace;
              white-space: pre
            }
            '),
            
  ),
)

server <- function(input, output, session) {
  # increase maximum file size to greater than the largest file ever seen. Currently 5,122 KB.
  options(shiny.maxRequestSize = 6 * 1024^2)
  
  # Name validation
  rats = rat_archive$Rat_name %>% str_to_upper()
  name_clean <- reactive({
    req(input$name, cancelOutput = TRUE)
    input$name %>% str_trim %>% str_replace_all(" ", "") %>% str_to_upper()
  })
  known_rat <- reactive ({
    req(name_clean(), cancelOutput = TRUE)
    name_clean() %in% rats
  })
  name_good <- reactive({
    req(known_rat(), cancelOutput = TRUE)
    name_clean()
  })
  id_good <- reactive({
    req(name_good(), cancelOutput = TRUE)
    rat_archive %>% dplyr::filter(str_to_upper(Rat_name) == name_good() & is.na(end_date)) %>% .$Rat_ID
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
    w = run_archive %>% dplyr::filter(rat_ID %in% id_good()) %>% dplyr::arrange(date) %>% tail(1) %>% .$weight
    if (rlang::is_empty(w)) -1 else w
  })
  weight_change_percent <- reactive({
    if(is.na(input$weight) || input$weight == 0 || last_weight() == -1) -1 else abs((input$weight - last_weight()) / last_weight())
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
    req(v$pickedFile)
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
    v$pickedFile = TRUE
  })
  
  # Submit validation
  v <- reactiveValues(pushedAnalyze = FALSE, readyAnalyze = FALSE, row = NULL, weightPlotted = FALSE, pushedSave = FALSE, readySave = FALSE, pickedFile = FALSE)
  
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
    if(is.null(input$matfile) || !v$pickedFile) {
      hideFeedback("matfile")
      showFeedbackDanger("matfile", "No file selected!")
    }
    if(input$scientist == "") {
      hideFeedback("scientist")
      showFeedbackDanger("scientist", "Required.")
    }
    validate(
      need(input$name, "Rat name is required."),
      need(input$weight > 0, "Weight must be greater than zero."),
      need(input$observations, "Observations must be provided."),
      need(input$matfile, "No file selected!"),
      need(input$scientist, "Your name is required.")
    )
    "Ready for analysis."
  })
  
  observeEvent(input$btnAnalyze, {
    v$pushedAnalyze = TRUE
    v$readyAnalyze = FALSE
    if (requirements() == "Ready for analysis.") {
      v$readyAnalyze = TRUE
      
      # start animation and disable inputs
      shinyjs::addClass(id = "UpdateAnalyze", class = "loading dots")
      shinyjs::disable("btnAnalyze")
      shinyjs::disable("name")
      shinyjs::disable("weight")
      shinyjs::disable("observations")
      shinyjs::disable("exclude_trials")
      shinyjs::disable("matfile")
    }
  })
  
  observeEvent(input$btnClose, {
    v$pushedClose = TRUE
    v$readyClose = TRUE
    
    stopApp(returnValue = 'Pied Piper closed by user interface. Hit \"Run App\" to re-open.')
  })
  
  output$show <- reactive({
    output$requirements == "Finished."
  })
  
  
  output$requirements <- renderText({
    req(v$pushedAnalyze)
    #requirements()
    if(isolate(v$readyAnalyze)) {
      v$readyAnalyze = FALSE
      v$row = Process_File(file_to_load = input$matfile$datapath, name = input$name, weight = input$weight, observations = input$observations, exclude_trials = input$exclude_trials, file_name_override = input$matfile$name, use_shiny = TRUE)
      "Finished."
      # stop animation and reenable button
      shinyjs::removeClass(id = "UpdateAnalyze", class = "loading dots")
      # shinyjs::enable("btnAnalyze")
    }
    else if(isolate(v$pushedAnalyze)) {
      v$pushedAnalyze = FALSE
      requirements()
    }
  })
  
  output$displaycomment <- renderText({
    req(id_good)
    if(id_good() > 0) {
      rat_archive %>% dplyr::filter(Rat_ID %in% id_good()) %>% .$Display_Comment
    } else {
      id_good()
    }
  })
  
  # output$text3 <- renderText({
  #   req(input$btnAnalyze)
  #   req(v$row)
  #   showModal(modalDialog(
  #     title = "Important message",
  #     "This is an important message!"
  #   ))
  # })
  
  no_fatal_error <- reactive({
    !fatal_error()
  })
  fatal_error <- eventReactive(v$row, {
    (length(v$row) == 2)
  })
  output$fatal_error <- renderText({
    fatal_error()
  })
  outputOptions(output, "fatal_error", suspendWhenHidden = FALSE)
  
  output$warnings <- renderTable(striped = TRUE, hover = TRUE, sanitize.text.function = identity,
                                 {
                                   req(v$pushedAnalyze)
                                   req(v$row)
                                   warns = v$row %>% .$warnings_list %>% unlist() %>% data.frame(Warnings = .)
                                   if(nrow(warns) == 0) {
                                     shinyjs::addClass(id = "warnings", class = "greenbody")
                                     #data.frame(File_Checks_Complete = "No warnings!")
                                   } else {
                                     shinyjs::addClass(id = "warnings", class = "redbody")
                                     warns = warns %>% mutate(Warnings = str_replace_all(Warnings, "\\n", "<br> "))
                                   }
                                 })
  
  output$plotWeight <- renderPlot({
    req(v$pushedAnalyze)
    req(v$row) #TODO at the moment I'm constructing a fake vrow to feed to warnings for when the file is already loaded, but this thinks it has a real one and dies. Check existence of columns or something and skip evaluation if they're absent.
    req(no_fatal_error())
    rat_name = v$row %>% .$rat_name
    rat_ID = v$row %>% .$rat_ID
    Generate_Weight_Trials_Graph(rat_name, rat_ID)
  }, height = 500)
  
  extra_graphs  <- reactive({
    req(v$pushedAnalyze)
    req(v$row) #TODO at the moment I'm constructing a fake vrow to feed to warnings for when the file is already loaded, but this thinks it has a real one and dies. Check existence of columns or something and skip evaluation if they're absent.
    req(no_fatal_error())
    rat_name = v$row %>% .$rat_name
    rat_ID = v$row %>% .$rat_ID
    Generate_Extra_Graphs(rat_name, rat_ID)
  })
  
  output$plotRxn <- renderPlot({
    extra_graphs()$rxn_graph
  }, height = 500)
  
  output$plotDprime <- renderPlot({
    extra_graphs()$dprime_graph
  }, height = 500)
  
  
  
  requirements_for_save <- reactive({
    req(input$weightCheck)
    req(input$rxnCheck)
    r = "Ready for save."
    if(input$weightCheck != "OK") {
      if(is.null(input$weightProblem) || input$weightProblem == "") {
        hideFeedback("weightProblem")
        showFeedbackDanger("weightProblem", "Required.")
        r = "Missing required values."
      } else {
        hideFeedback("weightProblem")
      }
    }
    if(input$rxnCheck != "OK") {
      if(is.null(input$rxnProblem) || input$rxnProblem == "") {
        hideFeedback("rxnProblem")
        showFeedbackDanger("rxnProblem", "Required.")
        r = "Missing required values."
      } else {
        hideFeedback("rxnProblem")
      }
    }
    if(is.null(input$scientist) || input$scientist == "") {
      hideFeedback("scientist")
      showFeedbackDanger("scientist", "Required.")
      r = "Missing required values."
    }
    validate(
      need(input$weightCheck, "Must OK or Reject weight graph."),
      need(input$rxnCheck, "Must OK or Reject rxn graph."),
      need(input$scientist, "Must enter your name."),
    )
    r
  })
  
  observeEvent(ignoreInit = TRUE, input$weightProblem, {
    if (input$weightProblem != "") hideFeedback("weightProblem")
  })
  
  observeEvent(ignoreInit = TRUE, input$rxnProblem, {
    if (input$rxnProblem != "") hideFeedback("rxnProblem")
  })
  
  observeEvent(ignoreInit = TRUE, input$scientist, {
    if (input$scientist != "") hideFeedback("scientist")
  })
  
  resetForm <- function() {
    reset("form1")
    reset("form2")
    v$pushedAnalyze = FALSE
    v$readyAnalyze = FALSE
    v$row = NULL
    v$weightPlotted = FALSE
    v$pushedSave = FALSE
    v$readySave = FALSE
    v$pickedFile = FALSE
    # updateTextInput(session,
    #                 inputId = "inText",
    #                 label = paste("New", c_label),
    #                 value = paste("New text", c_num)
    # )
    session$sendCustomMessage(type = "resetFileInputHandler", "file1")
    updateRadioGroupButtons(inputId = "weightCheck", selected = character(0))
    updateRadioGroupButtons(inputId = "rxnCheck", selected = character(0))
    shinyjs::enable("btnAnalyze")
    shinyjs::enable("name")
    shinyjs::enable("weight")
    shinyjs::enable("observations")
    shinyjs::enable("exclude_trials")
    shinyjs::enable("matfile")
    shinyjs::enable("btnSave")
    shinyjs::enable("weightCheck")
    shinyjs::enable("weightProblem")
    shinyjs::enable("rxnCheck")
    shinyjs::enable("rxnProblem")
    shinyjs::enable("scientist")
    hideFeedback("weightProblem")
    hideFeedback("rxnProblem")
    hideFeedback("scientist")
    hideFeedback("name")
    hideFeedback("weight")
    hideFeedback("observations")
    hideFeedback("matfile")
  }
  
  observeEvent(input$btnReset, {
    resetForm()
  })
  
  #output$requirements_for_save <- renderText({
  observeEvent(v$readySave, {
    req(v$pushedSave)
    if(isolate(v$readySave)) {
      r = v$row
      r$scientist = input$scientist
      r$weightProblem = if(input$weightCheck != "OK") input$weightProblem else ""
      r$rxnProblem = if(input$rxnCheck != "OK") input$rxnProblem else ""
      Write_To_Archives(r)
      "Saved."
      shinyjs::removeClass(id = "UpdateSave", class = "loading dots") # stop animation
      resetForm()
    }
    else(requirements_for_save())
  })
  
  observeEvent(input$btnSave, {
    v$pushedSave = TRUE
    v$readySave = FALSE
    if (requirements_for_save() == "Ready for save.") {
      v$readySave = TRUE
      
      # start animation and disable inputs
      shinyjs::addClass(id = "UpdateSave", class = "loading dots")
      shinyjs::disable("btnSave")
      shinyjs::disable("weightCheck")
      shinyjs::disable("weightProblem")
      shinyjs::disable("rxnCheck")
      shinyjs::disable("rxnProblem")
      shinyjs::disable("scientist")
    }
  })
}

#sink()
#sink(paste0(projects_folder, paste0(Sys.Date() %>% format("%Y-%m-%d"), ".log")), append=TRUE, split=TRUE)
source(paste0(projects_folder, "main.R"))
options(shiny.host = "127.0.0.1") #setting this to an external IP address forces browser instead of rstudio window
shinyApp(ui, server)
#sink()
