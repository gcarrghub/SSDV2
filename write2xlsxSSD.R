wb <- openxlsx::createWorkbook()
addWorksheet(wb = wb, sheetName = "Data Listing", gridLines = FALSE)

# Output the raw data
unitSTR <- paste0("(",input$units,")")
if(!input$doGrps){
  writeData(wb = wb,sheet = 1,x = testData[,c("species","responses")],startCol = 1,startRow = 3,colNames = FALSE)
  writeData(wb = wb,sheet = 1,x = data.frame(X1="Species",X2="Response"),
            startCol = 1,startRow = 1,colNames = FALSE)
  writeData(wb = wb,sheet = 1,x = data.frame(X1="",X2=unitSTR),
            startCol = 1,startRow = 2,colNames = FALSE)
}

if(input$doGrps){
  writeData(wb = wb,sheet = 1,x = testData[,c("species","responses","groups")],startCol = 1,startRow = 3,colNames = FALSE)
  writeData(wb = wb,sheet = 1,x = data.frame(X1="Species",X2="Response",X3="Group"),
            startCol = 1,startRow = 1,colNames = FALSE)
  writeData(wb = wb,sheet = 1,x = data.frame(X1="",X2=unitSTR),
            startCol = 1,startRow = 2,colNames = FALSE)
}

headerStyle <- createStyle(fontSize = 20,textDecoration = "bold")
speciesStyle <- createStyle(
  fontSize = 18,
  textDecoration = c("italic")
)

getPlacesFMT <- function(x){
  x <- abs(na.omit(x))
  #print(x)
  places2print <- floor(min(log10(x)))-1
  #print(places2print)
  places2print <- ifelse(places2print<0,yes = abs(places2print),no = 0)
  #print(places2print)
  paste0("0.",paste0(rep("0",places2print),collapse = ""))
}
#checking
#getPlacesFMT(testData$responses)
#getPlacesFMT(testData$responses/1000)
#getPlacesFMT(testData$responses*1000)

numFMT <- openxlsx::createStyle(fontSize=18,numFmt = getPlacesFMT(testData$responses))

### Format the numeric values (decimal places) and italics for species
addStyle(wb = wb,sheet = 1,style = speciesStyle,rows = 2+(1:nrow(testData)),
         cols = rep(1,nrow(testData)))
addStyle(wb = wb,sheet = 1,style = numFMT,rows = 2+(1:nrow(testData)),
         cols = rep(2,nrow(testData)))
if(input$doGrps)addStyle(wb = wb,sheet = 1,style = speciesStyle,rows = 2+(1:nrow(testData)),
         cols = rep(3,nrow(testData)))


SC.res <- 6
RES.DF <- as.data.frame(resultsTable)
RES.DF <- data.frame(Distribution=c("Normal","Logistic","Non-parametric"),RES.DF)
#Write the actual results starting in row 3, header added next
#8 columns with params, 6 without
RES.DF.noparms <- RES.DF[,-(6:7)]
RES.parms <- RES.DF[-nrow(RES.DF),(6:7)]
writeData(wb = wb,sheet = 1,x = RES.DF.noparms,startCol = SC.res,startRow = 3,colNames = FALSE)
writeData(wb = wb,sheet = 1,x = as.data.frame(rbind(c("Distribution","P",names(RES.DF.noparms)[3],"LowerCL","UpperCL","AD GOF"))),
          startCol = SC.res,startRow = 1,colNames = FALSE)
writeData(wb = wb,sheet = 1,x = as.data.frame(rbind(c("","",unitSTR,unitSTR,unitSTR,"p-value"))),
          startCol = SC.res,startRow = 2,colNames = FALSE)

#params to right, for completeness
writeData(wb = wb,sheet = 1,x = RES.parms,startCol = SC.res+10,startRow = 3,colNames = FALSE)
writeData(wb = wb,sheet = 1,x = as.data.frame(rbind(c("Parameters:","Location","Scale"))),
          startCol = SC.res+9,startRow = 1,colNames = FALSE)
writeData(wb = wb,sheet = 1,x = as.data.frame(rbind(c("(log scale)","(log scale)"))),
          startCol = SC.res+10,startRow = 2,colNames = FALSE)

### format numeric output
for(i in SC.res + 1:(ncol(RES.DF.noparms)-1)){
  colStyle <- createStyle(
    fontSize = 18,
    numFmt = getPlacesFMT(RES.DF.noparms[,i-(SC.res-1)])
  )
  addStyle(wb = wb,sheet = 1,style = colStyle,rows = 2+(1:nrow(RES.DF.noparms)),cols = i)
}
print(RES.parms)
for(i in (1:2)){
  colStyle <- createStyle(
    fontSize = 18,
    numFmt = getPlacesFMT(RES.parms[,i])
  )
  addStyle(wb = wb,sheet = 1,style = colStyle,rows = 2+(1:nrow(RES.parms)),cols = i+SC.res+10-1)
}

# row label of results
addStyle(wb = wb,sheet = 1,style = createStyle(fontSize=18),
         rows = 2+(1:nrow(RES.DF)),cols = SC.res,gridExpand = TRUE,stack = TRUE)
# all first row is bold/size
addStyle(wb = wb,sheet = 1,style = headerStyle,rows=1,cols = 1:20,stack = TRUE)
# right-align numeric headers
addStyle(wb = wb,sheet = 1,style = createStyle(halign = "right"),rows=1,cols = c(2,(SC.res+1):20),stack = TRUE)
# right-align units in 2nd row
addStyle(wb = wb,sheet = 1,style = createStyle(fontSize=18,halign = "right"),rows=2,cols = 1:20,stack = TRUE)

setColWidths(
  wb = wb,
  sheet = 1,
  cols = 1:20,
  widths = "auto")

if(input$doGrps){
  addWorksheet(wb = wb, sheetName = "Group Analysis", gridLines = TRUE)
  outStyle <- createStyle(
    fontName = "Courier",
    fontSize = 18
    )
  writeData(wb,
            x=readLines("siminf.txt"),
            sheet="Group Analysis",
            startRow=2)
  addStyle(wb = wb,sheet = "Group Analysis",style = outStyle,cols = 1:100,rows=1:100,gridExpand = TRUE)
  writeData(wb,
            x=structure(confintDF,names=c("Estimate","LCL.95","UCL.95")),
            sheet="Group Analysis",
            startRow=2,startCol = 5,rowNames = TRUE)
  print(confintDF)
  for(i in 1:3){
    colStyle <- createStyle(
      fontSize = 18,
      numFmt = getPlacesFMT(confintDF[,i])
    )
    addStyle(wb = wb,sheet = "Group Analysis",style = colStyle,rows = 3:(2+nrow(confintDF)),cols = i+5,stack = TRUE)
  }

  setColWidths(
    wb = wb,
    sheet = "Group Analysis",
    cols = 1,
    widths = "auto")
  setColWidths(
    wb = wb,
    sheet = "Group Analysis",
    cols = 2:9,
    widths = rep(20,3))
  setColWidths(
    wb = wb,
    sheet = "Group Analysis",
    cols = 5,
    widths = 40)
  addStyle(wb = wb,sheet = "Group Analysis",style = createStyle(halign = "right"),rows = 3:(2+nrow(confintDF)),cols = 5,stack = TRUE)
  addStyle(wb = wb,sheet = "Group Analysis",style = createStyle(halign = "right"),rows = 2,cols = 6:8,stack = TRUE)

  writeData(wb,
            x=siminfDF,
            sheet="Group Analysis",
            startRow=11,startCol = 5,rowNames = TRUE)
  for(i in 1:3){
    colStyle <- createStyle(
      fontSize = 18,
      numFmt = getPlacesFMT(siminfDF[,i])
    )
    addStyle(wb = wb,sheet = "Group Analysis",style = colStyle,rows = 11+(1:nrow(siminfDF)),cols = i+5,stack = TRUE)
  }
  addStyle(wb = wb,sheet = "Group Analysis",style = createStyle(
    fontSize = 18,
    numFmt = "0.0000"),
    rows = 11+(1:nrow(siminfDF)),cols = i+6,stack = TRUE)
  addStyle(wb = wb,sheet = "Group Analysis",style = createStyle(halign = "right"),rows = 11+(1:nrow(siminfDF)),cols = 5,stack = TRUE)
  addStyle(wb = wb,sheet = "Group Analysis",style = createStyle(halign = "right"),rows = 11,cols = 6:9,stack = TRUE)

}

if(doLeaveOneOut){
addWorksheet(wb = wb,sheetName = "Normal Leave Out")
writeData(wb,
          x=data.frame(Normal=c("Normal","Leave One Out Analysis")),
          sheet="Normal Leave Out",
          startRow=1)
#writeWorksheet(wb.out,data.frame(Normal=c(tagString,"Leave One Out Analysis")),sheet=tagString,startRow=rowCount)
writeData(wb = wb,sheet = "Normal Leave Out",x = as.data.frame(rbind(names(fit.out.norm))),
          startCol = 1,startRow = 4,colNames = FALSE)
writeData(wb = wb,sheet = "Normal Leave Out",x = data.frame(X1="",X2=unitSTR,X3="",X4=unitSTR,X5=unitSTR,X6=unitSTR),
          startCol = 1,startRow = 5,colNames = FALSE)
writeData(wb,
          x=fit.out.norm,
          sheet="Normal Leave Out",
          startRow=6,
          startCol=1,
          colNames = FALSE)

addWorksheet(wb = wb,sheetName = "Logistic Leave Out")
writeData(wb,
          x=data.frame(Logistic=c("Logistic","Leave One Out Analysis")),
          sheet="Logistic Leave Out",
          startRow=1)
#writeWorksheet(wb.out,data.frame(Normal=c(tagString,"Leave One Out Analysis")),sheet=tagString,startRow=rowCount)
writeData(wb = wb,sheet = "Logistic Leave Out",x = as.data.frame(rbind(names(fit.out.logis))),
          startCol = 1,startRow = 4,colNames = FALSE)
writeData(wb = wb,sheet = "Logistic Leave Out",x = data.frame(X1="",X2=unitSTR,X3="",X4=unitSTR,X5=unitSTR,X6=unitSTR),
          startCol = 1,startRow = 5,colNames = FALSE)
writeData(wb,
          x=fit.out.logis,
          sheet="Logistic Leave Out",
          startRow=6,
          startCol=1,
          colNames = FALSE)
}

if(doAddOneIn){
  addWorksheet(wb,sheetName = "AddOneIn")
  writeData(wb,x = data.frame(AddOne=c("AddOneIn","Add One In Analysis")),
            sheet="AddOneIn",startRow=1)
  #writeWorksheet(wb.out,data.frame(AddOne=c(tagString,"Add One In Analysis")),sheet=tagString,startRow=rowCount)
  writeData(wb = wb,sheet = "AddOneIn",x = as.data.frame(rbind(names(fit.out[,-1]))),
            startCol = 1,startRow = 5,colNames = FALSE)
  writeData(wb = wb,sheet = "AddOneIn",x = data.frame(X1="",X2="",X3=unitSTR,X4=unitSTR),
            startCol = 1,startRow = 6,colNames = FALSE)
  writeData(wb,x = fit.out[,-1],
            sheet="AddOneIn",startRow=7,startCol=1,colNames = FALSE)
}

if(input$doAvg){
  addWorksheet(wb,sheetName = "ModelAvg")
  writeData(wb,x = distDF,
            sheet="ModelAvg",startRow=1)
}

saveWorkbook(wb = wb,file = "SSDoutput.xlsx",overwrite = TRUE)
