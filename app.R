library(shiny)
library(shinyFeedback)
library(shinythemes)
library(dplyr, warn.conflicts = FALSE)
library(thematic)
library(stringr)
library(glue)
library(ggplot2)
library(hrbrthemes)

ui <- fluidPage(
  useShinyFeedback(),
  theme = shinythemes::shinytheme("cerulean"),
  titlePanel("PiedPiper", windowTitle = "PiedPiper"),
  verticalLayout(
    textInput("name", "Rat name", placeholder = "Blue4"),
    numericInput("weight", "Weight (g)", value = 0, min = 0),
    textAreaInput("observations", "Observations from run", rows = 3,
                  placeholder = "Good hits. Good misses. Only real FAs.\n2 CRs back to back.\nBreak from 35-47m, with no jams."),
    textInput("skip", "Trials to skip (e.g. not performed by rat)", placeholder = "2, 120-126, 201 (may leave blank)"),
    fileInput("matfile", "Select .mat file:", buttonLabel = "Browse...", accept = c(".mat"), width = "600px"),
  ),
  textOutput("text1"),
  textOutput("text2")
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
  observeEvent(input$name, {
    alnum = name_clean() %>% str_detect("^[:alnum:]*$")
    knownrat = name_clean() %in% rats
    if(!alnum) {
      hideFeedback("name")
      showFeedbackWarning("name", "Must contain only letters, numbers, and spaces.")
    } else if (knownrat) {
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


  # output$text2 <- renderText({
  #   req(input$name)
  #   req(input$matfile)
  #
  #   if(tools::file_ext(input$matfile$datapath) != "mat") {
  #     hideFeedback("matfile")
  #     showFeedbackDanger("matfile", "The file must be .mat format.")
  #   } else if(name_clean() == matname_clean()) {
  #     hideFeedback("matfile")
  #     showFeedbackSuccess("matfile", "Filename appears to match rat name.")
  #   } else {
  #     hideFeedback("matfile")
  #     showFeedbackWarning("matfile", glue("Is this the correct file for {input$name}?"))
  #   }
  # })

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

  observeEvent(input$matfile, {
    if(tools::file_ext(input$matfile$datapath) != "mat") {
      hideFeedback("matfile")
      showFeedbackDanger("matfile", "The file must be .mat format.")
    }
    else {
      hideFeedback("matfile")
    }
  })

  # debug output
  output$text1 <- renderText({
    name_clean()
  })
  # output$text2 <- renderText({
  #   matname_clean()
  # })

}

source(paste0(projects_folder, "main.R"))
shinyApp(ui, server)
