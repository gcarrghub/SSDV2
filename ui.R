library(shiny)
library(shinyjs)
library(shinyalert)
library(shinyWidgets)
library(rhandsontable)# only if for table output
library(markdown)# for PDF/markdown output
library(rmarkdown)
library(knitr)
library(htmlwidgets)
library(sortable)
library(magrittr)
library(isotone)
library(parallel)
library(formattable)
library(DT)
library(RColorBrewer)
library(multcomp)
jsResetCode <- "shinyjs.reset = function() {history.go(0)}" # Define the js method that resets the page
shinyUI(
  fluidPage(
    shinyalert::useShinyalert(),  # Sets up shinyalert
    titlePanel("ES + BMD Tools"),
    sidebarLayout(
      sidebarPanel(
        actionBttn(
          inputId="reset_button",
          label = "Reset Tool",
          icon = icon("redo"),
          style = "pill",
          color = "default",
          size = "md",
          block = FALSE,
          no_outline = TRUE
        ),
        #actionButton("reset_button", "Reset Page",icon = icon("redo")),
        splitLayout(
          radioButtons("analysisType",label = "Select Analysis",selected = "SSD",
                       choiceValues = list("Count","BMD","SK","Continuous","SSD"),
                       choiceNames=list("LCx","Binary BMD","Spearman-Karber","BV","SSD")
          ),
          fluidPage(
            wellPanel(uiOutput("defaultVars"))
          )
        ),
        # in the server, these SSD inputs are NULLed out if the analysis is not SSD
        uiOutput("SSDoptshead"),
        splitLayout(uiOutput("SSD.2.1"),uiOutput("SSD.2.2"),uiOutput("SSD.2.3"),#cellWidths = "33%",
                    cellArgs = list(style = c("align: left","align: center","align: right"))),
        uiOutput("effectSelects"),
        textAreaInput("pasteData",label="Paste data with column labels here:",rows=3),
        ### always need a response variable
        ### all of these will initally be set to None
        uiOutput("varSelects"),
        #uiOutput("responsesVar"),
        #uiOutput("sizesVar"),
        #uiOutput("dosesVar"),
        #uiOutput("speciesVar"),
        #sliderInput("ECXvalue", "Effect Level", 0.05, .95, 0.50, step = 0.05),
        uiOutput("scaleSelect"),
        uiOutput("varLabels"),
        uiOutput("setupButton"),
        #uiOutput("xLabBox"),
        #uiOutput("yLabBox"),
        #textInput("xLab",label="Exposure label",value="Exposure Concentration"),
        #textInput("yLab",label="Response label",value="Mortality Rate"),
        uiOutput("graphOpts"),
        # this puts out the species customizations only if SSD is chosen.
        # otherwise, NULLed out
        splitLayout(uiOutput("SSD.1.1"),uiOutput("SSD.1.2"),uiOutput("SSD.1.3"),
                    cellArgs = list(style = c("align: left","align: right"))),
        splitLayout(uiOutput("SSD.2.4"),uiOutput("SSD.2.5"),#cellWidths = "33%",
                    cellArgs = list(style = c("align: left","align: right"))),
        uiOutput("runButton"),
        h3("Results:"),
        ### idea is only to offer output when an analysis is complete.
        ### otherwise, old files could get posted.  Another option
        ### is to use a different output file (xls and pdf) for
        ### each analysis, but that's not implemented yet.
        splitLayout(
          uiOutput("Excelbutton"),
          uiOutput("PDFbutton")
        ),
        # https://stackoverflow.com/questions/25062422/restart-shiny-session
        shinyjs::useShinyjs(),                                           # Include shinyjs in the UI
        shinyjs::extendShinyjs(text = jsResetCode, functions = "reset") # Add the js code to the page




      ),
      mainPanel(
        ### I think this should work for any analysis:  a view of the input data before selections,
        ### after selections, and a preview plot, and that's it for now.
        h3("Input data:"),
        DTOutput("DTtableRaw",width = "75%"),
        h3("Analysis data:"),
        DTOutput("DTtable"),
        h3("Preview plot:"),
        conditionalPanel(condition="output.setupComplete",plotOutput("basePlot"))#,
        ### only for BMD, and that should probably be
        ### simplified since my old version somehow works
        ### outside of shiny, but not inside it.  Go figure.
        #uiOutput("markdown")
      )
    )
  )
)
