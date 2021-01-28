rvs$SSD.complete <- 0
pdf(file = "SSDplotOutput.pdf",width = input$figW,height = input$figH)
testData <- rvs$finalDF
if(input$doGrps){
  print("Inside of doGrps block, for emmeans analyses")
  stepProgress <- stepProgress + 1
  setProgress(value = stepProgress/Nsteps, detail="Anova")
  lmData <- testData
  lmData$y <- testData$responses
  lmData$groups <- as.factor(lmData$groups)
  lmData$log10.y <- log10(lmData$y)


  #process to reorder grouping factor in mean order
  lmOBJ <- lm(log10.y ~ groups,data=lmData)
  predVals <- predict(lmOBJ,newdata=data.frame(groups=levels(lmData$groups)))
  lmData$groups <- factor(as.character(lmData$groups),levels = levels(lmData$groups)[order(predVals)])


  nGroups <- length(levels(lmData$groups))
  lmOBJ <- lm(log10.y ~ groups,data=lmData)
  lmOBJnoint <- lm(log10.y ~ -1 + groups,data=lmData)
  confOBJ<-confint(multcomp::glht(lmOBJnoint),calpha = univariate_calpha())
  cexLAB <- input$labelSize # size of xlab and ylab
  cexAXIS <- input$axisSize  # tick annotations size on axis
  cexLWD <- input$lineSize  # line width
  cexPCH <- ifelse(cexLWD<1,1,cexLWD*.75)  # plot symbol size
  #since group labels on the left, adjust up if number of characters is large?
  max(nchar(levels(lmData$groups)))
  par(mai=c(input$figH*0.15, input$figW*0.30, 0, 0)+0.1,omi=rep(0,4))

  plot(y=c(1:length(levels(lmData$groups))),x=10^confOBJ$confint[,1],
       xlim=10^range(c(range(confOBJ$confint),lmData$log10.y)),
       ylim=c(1,nGroups)+c(-0.5,0.5),
       axes=FALSE,type="n",log='x',
       xlab="",ylab="")
  axis(side=1,cex.axis=cexAXIS,lwd=cexLWD)
  axis(side=2,at=c(1:length(levels(lmData$groups))),
       labels = FALSE,las=1,cex.axis=cexAXIS,lwd=cexLWD,tck=0.02)
  mtext(text = levels(lmData$groups),side=2,las=1,outer=FALSE,
        cex=cexAXIS,at = c(1:length(levels(lmData$groups))),
        adj=1.02,line=0)
  ###################
  print(c(at=10^mean(par("usr")[1:2])))
  mtext(side=1,outer=FALSE,line=par("mar")[1]-1.25,cex = cexLAB,padj=0,
        at=10^mean(par("usr")[1:2]),
        text = paste0(input$xLab," (",input$units,")"))
  sapply(1:length(levels(lmData$groups)),FUN = function(i){
    yVal <- (1:length(levels(lmData$groups)))[i]
    lines(x=10^confOBJ$confint[i,2:3],y=yVal[c(1,1)],col="gray",lwd=10,lend="butt")
    points(x=10^confOBJ$confint[i,1],y=yVal[c(1)],pch=16,cex=1.5*cexPCH)
    with(subset(lmData,groups==levels(lmData$groups)[i]),
         points(x=10^log10.y,y=log10.y*0 + yVal,cex=cexPCH))
  })
  #adjustment of letters depends on device height, apparently...
  lettADJ <- 0.025
  if(input$figH<8)lettADJ <- lettADJ+0.05*(8-input$figH)/8
  pairwiseOBJ <- summary(glht(lmOBJ, linfct = mcp(groups = "Tukey")),adjusted(type = c("hochberg")))
  text(y=c(1:length(levels(lmData$groups)))+diff(par("usr")[3:4])*lettADJ,
       x=10^confOBJ$confint[,1],
       col="red",cex=cexPCH,
       labels = cld(pairwiseOBJ)$mcletters$Letters
  )
  capture.output(pairwiseOBJ,file="siminf.txt")
  confintDF <- 10^as.data.frame(confOBJ$confint)
  rownames(confintDF) <- levels(lmData$groups)
  siminfDF <- as.data.frame(pairwiseOBJ$test[3:6])
  rownames(siminfDF) <- paste(gsub(pattern = " - ",replacement = " / ",fixed = TRUE,x = rownames(siminfDF)),"== 1")
  names(siminfDF) <- c("Ratio","SE","tvalue","pvalue")
  siminfDF$Ratio <- 10^siminfDF$Ratio
  siminfDF$SE <- siminfDF$Ratio/siminfDF$tvalue
  }

inputList <- input

# Reproduce the figure shown on web page
par(mai=c(input$figH*.15, input$figW*.15, 0, input$figW*input$speciesMargin)+0.1,
    omi=rep(0,4))
input2plot <- testData
input2plot$doses <- testData$responses
input2plot$logDose <- log10(input2plot$doses)
newLims <- 10^(log10(range(input2plot$responses,na.rm = TRUE))-c(0.1,0)*diff(log10(range(input2plot$responses,na.rm = TRUE))))
useFIT <- FALSE
xlimsFORCE <- newLims
yMaxFORCE <- NULL
speciesTF <- TRUE # setting this to TRUE effectively is to allow default plotting setup.
# when FALSE species will NOT be plotted not matter other settings.
source("SSDplotCodeCDF.R",local = TRUE)

#SSDplotFUN(testData = testData,inputList = input)
HCx.NonPar <- 10^quantile(log10(testData$responses),prob=input$ECXvalue.SSD,type=8)
#abline(v=HCx.NonPar)

data4legend <- testData
xLegend <- HCx.NonPar
source("addLegend.R",local = TRUE)


# Reset the layout for plots without species in margins
#par(mai=c(input$figH*.15, input$figW*.15, 0, 0)+0.1,omi=rep(0,4))
par(mai=rep(0.1,4),omi=rep(0,4))
pageBreakPDF("Normal\ndistribution fit")
par(mai=c(input$figH*.15, input$figW*.15, 0, input$figW*input$speciesMargin)+0.1,
    omi=rep(0,4))
stepProgress <- stepProgress+1
setProgress(value = stepProgress/Nsteps, detail="Normal fit")
normFitResults <- SSDfitFUN(
  pllData=testData$responses,
  speciesLabels=testData$species,
  distName="norm",
  xlabString=paste0(input$xLab," (",input$units,")"),
  effectLevel=input$ECXvalue.SSD,
  gridSize=50,
  titleString=NULL,
  italicFont=3,
  xlimVals=NULL,
  printSpeciesLabels=TRUE,
  showUpper=FALSE,
  roundTo=NULL,
  doPlots=FALSE,
  par1.LB=-Inf,
  par2.LB=-Inf,
  confLevel=0.95,
  logTransform=TRUE,
  startVals=NULL,
  quietTF=TRUE,
  inputList = inputList)
print(normFitResults$fit)


### the CDF and PDF plot versions are done in SSDplotCode.R
### create input2plot so the code is generically useful, especially for the add one in plots
input2plot <- testData
input2plot$doses <- testData$responses
input2plot$logDose <- log10(input2plot$doses)
fitOBJ <- normFitResults
useFIT <- TRUE
xlimsFORCE <- NULL
yMaxFORCE <- NULL
speciesTF <- TRUE # setting this to TRUE effectively is to allow default plotting setup.
                  # when FALSE species will NOT be plotted not matter other settings.
source("SSDplotCodeCDF.R",local = TRUE)
xLegend <- fitOBJ$fit[2]
data4legend <- testData
source("addLegend.R",local = TRUE)
source("SSDplotCodePDF.R",local = TRUE)



par(mai=rep(0.1,4),omi=rep(0,4))
pageBreakPDF("Logistic\ndistribution fit")
stepProgress <- stepProgress+1
setProgress(value = stepProgress/Nsteps, detail="Logistic fit")
logisFitResults <- SSDfitFUN(
  pllData=testData$responses,
  speciesLabels=testData$species,
  distName="logis",
  xlabString=paste0(input$xLab," (",input$units,")"),
  effectLevel=input$ECXvalue.SSD,
  gridSize=50,
  titleString=NULL,
  italicFont=3,
  xlimVals=NULL,
  printSpeciesLabels=TRUE,
  showUpper=FALSE,
  roundTo=NULL,
  doPlots=FALSE,
  par1.LB=-Inf,
  par2.LB=-Inf,
  confLevel=0.95,
  logTransform=TRUE,
  startVals=NULL,
  quietTF=TRUE,
  inputList = inputList)

### same data for plotting, just need to change the fit result
fitOBJ <- logisFitResults
useFIT <- TRUE
source("SSDplotCodeCDF.R",local = TRUE)
xLegend <- fitOBJ$fit[2]
data4legend <- testData
source("addLegend.R",local = TRUE)
source("SSDplotCodePDF.R",local = TRUE)

#leave one out is easy, but computationally expensive
doLeaveOneOut <- input$doLOO
HC.primary <- input$ECXvalue.SSD
concentrationLabel <- paste0(input$xLab," (",input$units,")")
if(doLeaveOneOut){
  ### This tries to speed up computations.  It depends on the
  ### number of cpus in useres computer.  Should still work on any
  ### computer, but might not speed up results
  library(parallel)
  ### There is issue with R v4... where cluster startup hangs.  setup_strategy = "sequential"
  ### is the fix found here:
  ### https://github.com/rstudio/rstudio/issues/6692
  snowCluster <- makePSOCKcluster(4, setup_strategy = "sequential")
  #on.exit(expr = {stopCluster(snowCluster)})
  #leave-one-out analyses
  clusterExport(snowCluster,varlist=c("genericPLL.AddOne", "rriskFitdist.GJC"),envir = serverEnv)


  #pageBreakPDF("Normal\nleave-one-out")
  stepProgress <- stepProgress+2
  setProgress(value = stepProgress/Nsteps, detail="Normal LOO")
  LOO.results.norm <- clusterApplyLB(snowCluster,x=0:nrow(testData),fun=function(i){
    if(i==0)result <- genericPLL.AddOne(testData$responses,speciesLabels=testData$species,distName="norm",doPlots=FALSE,effectLevel = HC.primary)
    if(i>=1)result <- genericPLL.AddOne(testData$responses[-i],speciesLabels=testData$species[-i],distName="norm",doPlots=FALSE,effectLevel = HC.primary)
    result
  })
  print("LOO for normal completed!!!!!!!")

  #pageBreakPDF("Logistic\nleave-one-out")

  stepProgress <- stepProgress+2
  setProgress(value = stepProgress/Nsteps, detail="Logistic LOO")
  LOO.results.logis <- clusterApplyLB(snowCluster,x=0:nrow(testData),fun=function(i){
    if(i==0)result <- genericPLL.AddOne(testData$responses,speciesLabels=testData$species,distName="logis",doPlots=FALSE,effectLevel = HC.primary)
    if(i>=1)result <- genericPLL.AddOne(testData$responses[-i],speciesLabels=testData$species[-i],distName="logis",doPlots=FALSE,effectLevel = HC.primary)
    result
  })
  print("LOO for logistic completed!!!!!!!")
  stopCluster(snowCluster)

  #logistic LOO
  fit.out.logis <- cbind(structure(data.frame(c("NONE",as.character(testData$species)),c(NA,testData$responses)),names=c("Species.Out",names(testData)[2])),
                         do.call(rbind,lapply(LOO.results.logis,FUN=function(resList){
                           resList[[1]]
                         })))
  if(FALSE){
    writeData(wb.out,
              x = data.frame(Logistic=c(tagString,"Leave One Out Analysis")),
              sheet=tagString,startRow=rowCount)
    #writeWorksheet(wb.out,data.frame(Logistic=c(tagString,"Leave One Out Analysis")),sheet=tagString,startRow=rowCount)
    rowCount <- rowCount+3
    writeData(wb.out,x = fit.out,
              sheet=tagString,
              startRow=rowCount,
              startCol=1)
    #writeWorksheet(wb.out,fit.out,sheet=tagString,startRow=rowCount,startCol=1)
    rowCount <- rowCount+nrow(fit.out)+3
  }


  #normal LOO
  fit.out.norm <- cbind(structure(data.frame(c("NONE",as.character(testData$species)),c(NA,testData$responses)),names=c("Species.Out",names(testData)[2])),
                        do.call(rbind,lapply(LOO.results.norm,FUN=function(resList){
                          resList[[1]]
                        })))
  if(FALSE){
    writeData(wb.out,
              x=data.frame(Normal=c(tagString,"Leave One Out Analysis")),
              sheet=tagString,
              startRow=rowCount)
    #writeWorksheet(wb.out,data.frame(Normal=c(tagString,"Leave One Out Analysis")),sheet=tagString,startRow=rowCount)
    rowCount <- rowCount+3
    writeData(wb.out,
              x=fit.out,
              sheet=tagString,
              startRow=rowCount,
              startCol=1)
    #writeWorksheet(wb.out,fit.out,sheet=tagString,startRow=rowCount,startCol=1)
    rowCount <- rowCount+nrow(fit.out)+3
  }
  # plotting of leave-one-out results is contained separately (in-line code, not a function, so source it each time)
  input2plot <- testData
  input2plot$doses <- testData$responses
  input2plot$logDose <- log10(input2plot$doses)
  fitOBJ <- normFitResults
  useFIT <- TRUE
  xMins <- qnorm(.001,mean=fit.out.norm[,"mean"],sd=fit.out.norm[,"sd"])
  xMaxs <- qnorm(.999,mean=fit.out.norm[,"mean"],sd=fit.out.norm[,"sd"])
  yMaxs <- dnorm(fit.out.norm[,"mean"],mean=fit.out.norm[,"mean"],sd=fit.out.norm[,"sd"])
  xlimsFORCE <- exp(range(c(xMins,xMaxs,input2plot$logDose)))
  yMaxFORCE <- max(c(max(yMaxs)*log(10),dnorm(x = fitOBJ$distPars$location,fitOBJ$distPars$location,fitOBJ$distPars$scale)))

  speciesTF <- FALSE # setting this to TRUE effectively is to allow default plotting setup.
  # when FALSE species will NOT be plotted not matter other settings.
  par(mai=rep(0.1,4),omi=rep(0,4))
  pageBreakPDF("Leave-One-Out\nAnalysis")
  pageBreakPDF("A:\nNormal\nLeave-One-Out")
  source("SSDplotCodeCDF.R",local = TRUE)
  for(i in 2:nrow(fit.out.norm)){
    xVals <- seq(xMins[i],xMaxs[i],length=1000)
    lines(x=exp(xVals),y=pnorm(xVals,mean=fit.out.norm[i,"mean"],sd=fit.out.norm[i,"sd"]),col=rgb(0.65,0.65,0.65))
  }
  if(TRUE){#redraw the model lines since LOO lines will obscure them
    lines(y=fitOBJ$fitLines[,1],x=fitOBJ$fitLines[,4],col=lineColors[1],lwd=cexLWD*1.7)
    lines(y=fitOBJ$fitLines[,1],x=fitOBJ$fitLines[,2],col=lineColors[2],lwd=cexLWD)
    lines(y=fitOBJ$fitLines[,1],x=fitOBJ$fitLines[,3],col=lineColors[2],lwd=cexLWD)
    if(input$doGrays){
      lines(y=fitOBJ$fitLines[,1],x=fitOBJ$fitLines[,2],col=lineColors[3],lwd=cexLWD,lty=2)
      lines(y=fitOBJ$fitLines[,1],x=fitOBJ$fitLines[,3],col=lineColors[3],lwd=cexLWD,lty=2)
    }
  }
  data4legend <- testData
  xLegend <- fitOBJ$fit[2]
  source("addLegend.R",local = TRUE)

  source("SSDplotCodePDF.R",local = TRUE)
  for(i in 2:nrow(fit.out.norm)){
    xVals <- seq(xMins[i],xMaxs[i],length=1000)
    lines(x=exp(xVals),y=log(10)*dnorm(xVals,mean=fit.out.norm[i,"mean"],sd=fit.out.norm[i,"sd"]),col=rgb(0.65,0.65,0.65))
  }
  #repeat fit pdf so on top of LOO lines
  dRange <- qnorm(p = c(0.0001,0.9999),fitOBJ$distPars$location[1],fitOBJ$distPars$scale[1])
  xVals <- seq(dRange[1],dRange[2],length=1000)
  lines(y=dnorm(xVals,fitOBJ$distPars$location[1],fitOBJ$distPars$scale[1]),x=10^(xVals),col=lineColors[1],lwd=cexLWD*1.7)

  fitOBJ <- logisFitResults
  useFIT <- TRUE
  xMins <- qlogis(.001,location = fit.out.logis[,"location"],scale = fit.out.logis[,"scale"])
  xMaxs <- qlogis(.999,location = fit.out.logis[,"location"],scale = fit.out.logis[,"scale"])
  yMaxs <- dlogis(fit.out.logis[,"location"],location=fit.out.logis[,"location"],scale=fit.out.logis[,"scale"])
  xlimsFORCE <- exp(range(c(xMins,xMaxs,input2plot$logDose)))
  yMaxFORCE <- max(c(max(yMaxs)*log(10),dlogis(x = fitOBJ$distPars$location,fitOBJ$distPars$location,fitOBJ$distPars$scale)))
  speciesTF <- FALSE # setting this to TRUE effectively is to allow default plotting setup.
  # when FALSE species will NOT be plotted not matter other settings.
  par(mai=rep(0.1,4),omi=rep(0,4))
  pageBreakPDF("B:\nLogistic\nLeave-One-Out")
  source("SSDplotCodeCDF.R",local = TRUE)
  for(i in 2:nrow(fit.out.logis)){
    xVals <- seq(xMins[i],xMaxs[i],length=1000)
    lines(x=exp(xVals),y=plogis(xVals,location=fit.out.logis[i,"location"],scale=fit.out.logis[i,"scale"]),col=rgb(0.65,0.65,0.65))
  }
  if(TRUE){#redraw the model lines since LOO lines will obscure them
    lines(y=fitOBJ$fitLines[,1],x=fitOBJ$fitLines[,4],col=lineColors[1],lwd=cexLWD*1.7)
    lines(y=fitOBJ$fitLines[,1],x=fitOBJ$fitLines[,2],col=lineColors[2],lwd=cexLWD)
    lines(y=fitOBJ$fitLines[,1],x=fitOBJ$fitLines[,3],col=lineColors[2],lwd=cexLWD)
    if(input$doGrays){
      lines(y=fitOBJ$fitLines[,1],x=fitOBJ$fitLines[,2],col=lineColors[3],lwd=cexLWD,lty=2)
      lines(y=fitOBJ$fitLines[,1],x=fitOBJ$fitLines[,3],col=lineColors[3],lwd=cexLWD,lty=2)
    }
  }
  data4legend <- testData
  xLegend <- fitOBJ$fit[2]
  source("addLegend.R",local = TRUE)

  source("SSDplotCodePDF.R",local = TRUE)
  for(i in 2:nrow(fit.out.logis)){
    xVals <- seq(xMins[i],xMaxs[i],length=1000)
    lines(x=exp(xVals),y=log(10)*dlogis(xVals,location=fit.out.logis[i,"location"],scale=fit.out.logis[i,"scale"]),col=rgb(0.65,0.65,0.65))
  }
  #repeat fit pdf so on top of LOO lines
  dRange <- qlogis(p = c(0.0001,0.9999),fitOBJ$distPars$location[1],fitOBJ$distPars$scale[1])
  xVals <- seq(dRange[1],dRange[2],length=1000)
  lines(y=dlogis(xVals,fitOBJ$distPars$location[1],fitOBJ$distPars$scale[1]),x=10^(xVals),col=lineColors[1],lwd=cexLWD*1.7)
  #source("LOOplotting.R",local = TRUE)
}
doAddOneIn <- input$doAOI
if(doAddOneIn){
  #add-one-in is much more difficult to program
  #add-one-in analyses -- plots directly from the function evaluation

  print("inside of AOI")
  if(!input$doGrps){
    #In add-one, always need a grouping variable.  If none there, add one
    #because the added value always needs to be distinct from data values
    #(it wound not be there if input data do not have it/not asked for)
    testData$groups <- rep("Obs",nrow(testData))
    print(head(testData))
  }
  par(mai=rep(0.1,4),omi=rep(0,4))
  pageBreakPDF("A:  Normal Add-One-In")
  stepProgress <- stepProgress+1.5
  setProgress(value = stepProgress/Nsteps, detail="Normal AOI")
  fit.out2 <- try(addOnePLL(effectValues = testData$responses,
                            speciesLabels=testData$species,
                            distName="norm",
                            xlabString=concentrationLabel,
                            dataTag="norm AOI",
                            effectLevel = HC.primary))
  for(iAOI in 1:nrow(fit.out2)){
    AOIdata <- testData[,c("species","responses","groups")]
    print("head(AOIdata)")
    print(head(AOIdata))
    AOIdata <- rbind(data.frame(
      species="ADD ONE",
      responses=fit.out2[iAOI,"addOneValue"],
      groups=paste("ADD ONE to",fit.out2[iAOI,"target"]),
      stringsAsFactors = FALSE),AOIdata,stringsAsFactors=FALSE)
    xVals <- quantile(log10(AOIdata$responses),probs = seq(0.0001,0.9999,length=10000),type=8)
    yVals <- seq(0.0001,0.9999,length=10000)
    pointIDs <- sapply(log10(AOIdata$responses),FUN = function(x)which.min(abs(x-xVals)))
    pointIDs[1] <- max(which(xVals==xVals[1]))
    AOIdata$yVals <- yVals[pointIDs]
    #print(AOIdata)
    #save(list="AOIdata",file="AOIdata.Rdata")

    normAOI.2 <- SSDfitFUN(
      pllData=AOIdata$responses,
      speciesLabels=AOIdata$species,
      distName="norm",
      xlabString=paste0(input$xLab," (",input$units,")"),
      effectLevel=input$ECXvalue.SSD,
      gridSize=50,
      titleString=NULL,
      italicFont=3,
      xlimVals=NULL,
      printSpeciesLabels=TRUE,
      showUpper=FALSE,
      roundTo=NULL,
      doPlots=FALSE,
      par1.LB=-Inf,
      par2.LB=-Inf,
      confLevel=0.95,
      logTransform=TRUE,
      startVals=NULL,
      quietTF=TRUE,
      inputList = inputList)
    #print(logisAOI.2)
    input2plot <- AOIdata
    input2plot$doses <- AOIdata$responses
    input2plot$logDose <- log10(input2plot$doses)
    #print(input2plot)
    fitOBJ <- normAOI.2
    useFIT <- TRUE
    xlimsFORCE <- NULL
    yMaxFORCE <- NULL
    speciesTF <- TRUE # setting this to TRUE effectively is to allow default plotting setup.
    # when FALSE species will NOT be plotted not matter other settings.
    source("SSDplotCodeCDF.R",local = TRUE)
    data4legend <- AOIdata
    xLegend <- fitOBJ$fit[2]
    source("addLegend.R",local = TRUE)
    #source("SSDplotCodePDF.R",local = TRUE)
  }
  #Create final plot that includes all of the AOI levels
  #the last one done is most extreme so use for xlims
  xlimsFORCE <- range(input2plot$doses)
  fitOBJ <- normFitResults
  useFIT <- TRUE
  input2plot <- testData
  input2plot$doses <- testData$responses
  input2plot$logDose <- log10(input2plot$doses)
  source("SSDplotCodeCDF.R",local = TRUE)
  mtext(side=4,at=0,text = "ADD ONE",adj=0,las=1,cex=input$speciesSize)
  for(iAOI in 1:nrow(fit.out2)){
    points(x=fit.out2[iAOI,"addOneValue"],y=0,pch=18,cex=cexPCH*sqrt(2))
    text(x=fit.out2[iAOI,"addOneValue"],y=0.02,adj=0,srt=90,
         labels = paste(fit.out2[iAOI,"oneInFormated"],"to get",fit.out2[iAOI,"target"]),
         cex=input$speciesSize)
    addOnePars <- SSDfitFUN(
      pllData=c(fit.out2[iAOI,"addOneValue"],testData$responses),
      speciesLabels=c("ADD ONE",testData$species),
      distName="norm",
      parsONLY=TRUE,
      doPlots=FALSE)

    xRange <- qnorm(c(.001,.999),addOnePars[1],addOnePars[2])
    xVals <- seq(xRange[1],xRange[2],length=1000)
    yVals <- pnorm(xVals,addOnePars[1],addOnePars[2])
    lines(x=10^xVals,y=yVals)
  }
  data4legend <- AOIdata
  legendSTR <- data4legend$groups[data4legend$species=="ADD ONE"]
  slashLOC <- regexpr("/",legendSTR,fixed = TRUE)
  data4legend$groups[data4legend$species=="ADD ONE"] <- paste0(substring(legendSTR,1,slashLOC),"X")
  xLegend <- 10^par("usr")[1]
  source("addLegend.R",local = TRUE)

  par(mai=rep(0.1,4),omi=rep(0,4))
  pageBreakPDF("Add-One-In Analysis")
  pageBreakPDF("B:  Logistic Add-One-In")
  stepProgress <- stepProgress+1.5
  setProgress(value = stepProgress/Nsteps, detail="Logistic AOI")
  fit.out1 <- try(addOnePLL(effectValues = testData$responses,
                            speciesLabels=testData$species,
                            distName="logis",
                            xlabString=concentrationLabel,
                            dataTag="logis AOI",
                            effectLevel = HC.primary))
  for(iAOI in 1:nrow(fit.out1)){
    AOIdata <- testData[,c("species","responses","groups")]
    AOIdata <- rbind(data.frame(
      species="ADD ONE",
      responses=fit.out1[iAOI,"addOneValue"],
      groups=paste("ADD ONE to",fit.out1[iAOI,"target"]),
      stringsAsFactors = FALSE),AOIdata,stringsAsFactors=FALSE)
    xVals <- quantile(log10(AOIdata$responses),probs = seq(0.0001,0.9999,length=10000),type=8)
    yVals <- seq(0.0001,0.9999,length=10000)
    pointIDs <- sapply(log10(AOIdata$responses),FUN = function(x)which.min(abs(x-xVals)))
    pointIDs[1] <- max(which(xVals==xVals[1]))
    AOIdata$yVals <- yVals[pointIDs]
    #print(AOIdata)
    #save(list="AOIdata",file="AOIdata.Rdata")

    logisAOI.2 <- SSDfitFUN(
      pllData=AOIdata$responses,
      speciesLabels=AOIdata$species,
      distName="logis",
      xlabString=paste0(input$xLab," (",input$units,")"),
      effectLevel=input$ECXvalue.SSD,
      gridSize=50,
      titleString=NULL,
      italicFont=3,
      xlimVals=NULL,
      printSpeciesLabels=TRUE,
      showUpper=FALSE,
      roundTo=NULL,
      doPlots=FALSE,
      par1.LB=-Inf,
      par2.LB=-Inf,
      confLevel=0.95,
      logTransform=TRUE,
      startVals=NULL,
      quietTF=TRUE,
      inputList = inputList)
    #print(logisAOI.2)
    input2plot <- AOIdata
    input2plot$doses <- AOIdata$responses
    input2plot$logDose <- log10(input2plot$doses)
    #print(input2plot)
    fitOBJ <- logisAOI.2
    useFIT <- TRUE
    xlimsFORCE <- NULL
    yMaxFORCE <- NULL
    speciesTF <- TRUE # setting this to TRUE effectively is to allow default plotting setup.
    # when FALSE species will NOT be plotted not matter other settings.
    source("SSDplotCodeCDF.R",local = TRUE)
    data4legend <- AOIdata
    xLegend <- fitOBJ$fit[2]
    source("addLegend.R",local = TRUE)
    #source("SSDplotCodePDF.R",local = TRUE)
  }
  #Create final plot that includes all of the AOI levels
  #the last one done is most extreme so use for xlims
  xlimsFORCE <- range(input2plot$doses)
  fitOBJ <- logisFitResults
  useFIT <- TRUE
  input2plot <- testData
  input2plot$doses <- testData$responses
  input2plot$logDose <- log10(input2plot$doses)
  source("SSDplotCodeCDF.R",local = TRUE)
  mtext(side=4,at=0,text = "ADD ONE",adj=0,las=1,cex=input$speciesSize)
  for(iAOI in 1:nrow(fit.out1)){
    points(x=fit.out1[iAOI,"addOneValue"],y=0,pch=18,cex=cexPCH*sqrt(2))
    text(x=fit.out1[iAOI,"addOneValue"],y=0.02,adj=0,srt=90,
         labels = paste(fit.out1[iAOI,"oneInFormated"],"to get",fit.out1[iAOI,"target"]),
         cex=input$speciesSize)
    addOnePars <- SSDfitFUN(
      pllData=c(fit.out1[iAOI,"addOneValue"],testData$responses),
      speciesLabels=c("ADD ONE",testData$species),
      distName="logis",
      parsONLY=TRUE,
      doPlots=FALSE)

    xRange <- qlogis(c(.001,.999),addOnePars[1],addOnePars[2])
    xVals <- seq(xRange[1],xRange[2],length=1000)
    yVals <- plogis(xVals,addOnePars[1],addOnePars[2])
    lines(x=10^xVals,y=yVals)
  }
  data4legend <- AOIdata
  legendSTR <- data4legend$groups[data4legend$species=="ADD ONE"]
  slashLOC <- regexpr("/",legendSTR,fixed = TRUE)
  data4legend$groups[data4legend$species=="ADD ONE"] <- paste0(substring(legendSTR,1,slashLOC),"X")
  xLegend <- 10^par("usr")[1]
  source("addLegend.R",local = TRUE)

  fit.out <- rbind(fit.out1,fit.out2)
  print(fit.out)
  #writeWorksheet(wb.out,fit.out[,-1],sheet=tagString,startRow=rowCount,startCol=1)

  #rgl.quit()#clean up the rgl stuff
}

dev.off()
### Main results
resultsTable <- rbind(normFitResults$fit,logisFitResults$fit,
                      c(input$ECXvalue.SSD,
                        10^quantile(log10(testData$responses),probs = input$ECXvalue.SSD,type = 8),
                        rep(NA,length(normFitResults$fit)-2)))
#save(list="resultsTable",file="resTab.RData")
print(resultsTable)
source("write2xlsxSSD.R",local = TRUE)
print("SSD analyses complete")
print("SSD analyses complete")
print("SSD analyses complete")
rvs$SSD.complete <- 1
