packages = unique(c("devtools","shiny","shinyjs","shinyalert","shinyWidgets",# only if for table output
                    "htmlwidgets","magrittr","parallel","formattable","DT",
                    "RColorBrewer","multcomp","openxlsx","ADGofTest"))
packageTests <- sapply(packages,FUN = require,character.only=TRUE)
if(all(packageTests)){
  cat("\n",paste(rep("#",100),collapse = ""),
      "\n  All required packages are present.",
      "\n",paste(rep("#",100),collapse = ""),"\n")
}
if(sum(!packageTests)>0){
  cat("\n",paste(rep("#",100),collapse = ""),
      "\n  Please wait while these required packages and their dependencies are installed:",
      "\n   ",paste(names(packageTests[!packageTests]),collapse = " "),
      "\n  Requires internet access and sufficient rights to install R packages on your system.",
      "\n",paste(rep("#",100),collapse = ""),"\n")
  install.packages(packages[!packageTests], repos = "https://cran.rstudio.com/", dependencies=TRUE)
  ### In one case, needed to add this to a users install.packages call:  INSTALL_opts = c('--no-lock')
  # recheck for packages
  packageTests <- sapply(packages,FUN = require,character.only=TRUE)
  if(all(packageTests)){
    cat("\n",paste(rep("#",100),collapse = ""),
        "\n  All required packages were successfully installed.",
        "\n",paste(rep("#",100),collapse = ""),"\n")
  }
  if(!all(packageTests)){
    cat("\n",paste(rep("#",100),collapse = ""),
        "\n  Not all packages were successfully installed:",
        "\n   ",paste(names(packageTests[!packageTests]),collapse = " "),
        "\n",paste(rep("#",100),collapse = ""),"\n")
  }
}


library(shiny)
library(shinyjs)
library(shinyalert)
library(shinyWidgets)
#library(sortable)
library(htmlwidgets)
#library(rhandsontable)# only if for table output
library(formattable)
library(DT)
#library(markdown)# for PDF/markdown output
#library(rmarkdown)
#library(knitr)
library(magrittr)
#library(isotone)
library(parallel)
library(RColorBrewer)
library(multcomp)
library(openxlsx)

jsResetCode <- "shinyjs.reset = function() {history.go(0)}" # Define the js method that resets the page

### just in case, purge files that might be left over from a previous run
### the code attempts to prevent this by resetting things, but it's all limits of the
### imagination for the order things are entered, changed, etc.  The user
### must assume the ultimate responsibility.  Any critical analysis should be run
### from a reset tool, in the proper order.
if(file.exists("SSDplotOutput.pdf"))unlink("SSDplotOutput.pdf")
if(file.exists("SSD Analysis.pdf"))unlink("SSD Analysis.pdf")
if(file.exists("SSDoutput.xlsx"))unlink("SSDoutput.xlsx")
if(file.exists("SSD Analysis.xlsx"))unlink("SSD Analysis.xlsx")

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
        ### before, we assigned default vars but current version does not work on that idea
        radioButtons("analysisType",label = "Select Analysis",selected = "SSD",
                     choiceValues = list("Count","BMD","SK","Continuous","SSD"),
                     choiceNames=list("LCx","Binary BMD","Spearman-Karber","BV","SSD")
        ),
        ### splitLayout(
        ###   radioButtons("analysisType",label = "Select Analysis",selected = "SSD",
        ###               choiceValues = list("Count","BMD","SK","Continuous","SSD"),
        ###               choiceNames=list("LCx","Binary BMD","Spearman-Karber","BV","SSD")
        ###  ),
        ###  fluidPage(
        ###    wellPanel(uiOutput("defaultVars"))
        ###  )
        ###),
        # in the server, these SSD inputs are NULLed out if the analysis is not SSD
        uiOutput("SSDoptshead"),
        splitLayout(uiOutput("SSD.2.1"),uiOutput("SSD.2.2"),uiOutput("SSD.2.3"),#cellWidths = "33%",
                    cellArgs = list(style = c("align: left","align: center","align: right"))),
        uiOutput("effectSelects"),
        textAreaInput("pasteData",label="Data with column labels:",rows=3,
                      placeholder = "Click inside this box and paste data (copied from Excel or similar)."),
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
        uiOutput("setupButton"),
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
        tabsetPanel(type="tabs",id = "outputTabActive",
                    tabPanel("Output",
                      h3("Input data:"),
                      DTOutput("DTtableRaw",width = "75%"),
                      h3("Analysis data:"),
                      DTOutput("DTtable"),
                      h3("Preview plot (Nonparametric fit):"),
                      conditionalPanel(condition="output.setupComplete",plotOutput("basePlot"))#,
                    ),
                    tabPanel("Help",includeHTML(path = "SSD Analysis Tool.html"))
                    )
        ### only for BMD, and that should probably be
        ### simplified since my old version somehow works
        ### outside of shiny, but not inside it.  Go figure.
        #uiOutput("markdown")
      )
    )
  )
)
