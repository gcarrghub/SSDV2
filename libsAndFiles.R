
### SSD only setup

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

# library(snow)


### just in case, purge files that might be left over from a previous run
### the code attempts to prevent this by resetting things, but it's all limits of the
### imagination for the order things are entered, changed, etc.  The user
### must assume the ultimate responsibility.  Any critical analysis should be run
### from a reset tool, in the proper order.
if(file.exists("SSDplotOutput.pdf"))unlink("SSDplotOutput.pdf")
if(file.exists("SSD Analysis.pdf"))unlink("SSD Analysis.pdf")
if(file.exists("SSDoutput.xlsx"))unlink("SSDoutput.xlsx")
if(file.exists("SSD Analysis.xlsx"))unlink("SSD Analysis.xlsx")

### set this object for resetting the web page with a button click
jsResetCode <- "shinyjs.reset = function() {history.go(0)}" # Define the js method that resets the page
