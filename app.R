library(shiny)
library(shinyFeedback)
library(shinythemes)
library(dplyr, warn.conflicts = FALSE)
library(thematic)
library(stringr)
browseVignettes(package = "shinyFeedback")

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
  rats = rat_archive$Rat_name %>% str_to_lower()
  name_clean <- reactive({
    input$name %>% str_trim %>% str_replace_all(" ", "") %>% str_to_lower()
  })
  name_good <- reactive({
    req(name_clean() %in% rats, cancelOutput = TRUE)
    name_clean()
  })
  id_good <- reactive({
    rat_archive %>% dplyr::filter(str_to_lower(Rat_name) == name_good()) %>% .$Rat_ID
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





}

source(paste0(projects_folder, "main.R"))
shinyApp(ui, server)
