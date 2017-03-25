# By default the file size limit is 5MB. Here limit is 70MB.
options(shiny.maxRequestSize = 70*1024^2)

# Increase memory limit
memory.size(max = FALSE)

source("helpers.R")

shinyServer(function(input, output, session){

hide("selectstage")
hide("selectestimaterisk")
hide("selectdetectclusters")
hide("selectmakemapsOESIR")

################################################
# INI populate selectInput columns data
################################################

  observe({
    if (is.null(names(rv$datosOriginal)))
      xd <- character(0)

    xd<-names(rv$datosOriginal)

    if (is.null(xd))
      xd <- character(0)

    xd2<- c("-", xd)

    # Can also set the label and select items
    #label = paste("Select input label", length(x)),
    updateSelectInput(session, "columnidareaindata", choices = xd, selected = head(xd, 1))
    updateSelectInput(session, "columndateindata",   choices = xd, selected = head(xd, 1))
    updateSelectInput(session, "columnpopindata",    choices = xd, selected = head(xd, 1))
    updateSelectInput(session, "columncasesindata",  choices = xd, selected = head(xd, 1))
    updateSelectInput(session, "columncov1indata",   choices = xd2, selected = head(xd2, 1))
    updateSelectInput(session, "columncov2indata",   choices = xd2, selected = head(xd2, 1))
    updateSelectInput(session, "columncov3indata",   choices = xd2, selected = head(xd2, 1))
    updateSelectInput(session, "columncov4indata",   choices = xd2, selected = head(xd2, 1))

  })



################################################
# END populate selectInput columns data
################################################

################################################
# INI populate selectInput columns map
################################################

observe({
    x <- names(rv$map)
    xd<-c("-",x)
    # Can use character(0) to remove all choices
    if (is.null(x)){
      x <- character(0)
      xd<-x
    }

    updateSelectInput(session, "columnidareainmap",        choices = x,  selected = head(x, 1))
    updateSelectInput(session, "columnnameareainmap",      choices = x,  selected = head(x, 1))
    updateSelectInput(session, "columnnamesuperareainmap", choices = xd, selected = head(xd, 1))

})

################################################
# END populate selectInput columns map
################################################





################################################
# INI show options selected in the analysis
################################################

dr<-reactive({
  dd<-switch(input$temporalUnitButton, "Year" = substring(input$daterange, 1, 4), "Month" = substring(input$daterange, 1, 7), "Day" = input$daterange)
  return(paste(dd[1],"to",dd[2]))
})

output$daterangeValue<-renderText({
 print(dr())
})

sorst<-reactive({
  if(input$SorSTButton=="S"){
    return("Spatial")}else{
      return("Spatio-Temporal")
    }
})

output$SorSTButtonValue<-renderText(
  print(sorst())
)

output$temporalUnitButtonValue<-renderText(input$temporalUnitButton)

################################################
# END show options selected in the analysis
################################################



################################################
# INI update labels that say I have done the analyses (detectclusters, estimaterisk, makemapsOESIR)
################################################

observeEvent(input$detectclustersButton , {
  updateTextInput(session, "selectdetectclusters", value = 'done')

})

observeEvent(input$estimateriskButton , {
  updateTextInput(session, "selectestimaterisk", value = 'done')

})

observeEvent(input$makemapsOESIRButton , {
  updateTextInput(session, "selectmakemapsOESIR", value = 'done')

})

################################################
# END update labels that say I have done the analyses (detectclusters, estimaterisk, makemapsOESIR)
################################################



################################################
# INI update selectstage label that says if I am in stagehelp or stageuploaddata
################################################

observeEvent(input$helpButton , {
  rv$lastselectstage<-input$selectstage
  updateTextInput(session, "selectstage", value = 'stagehelp')
  toggle("helpButton")

})

observeEvent(input$returnButton , {
  updateTextInput(session, "selectstage", value = rv$lastselectstage)
  toggle("helpButton")

})

observeEvent(input$editInputsButton , {
  updateTextInput(session, "selectstage", value = 'stageuploaddata')
  updateTextInput(session, "selectestimaterisk", value = 'notdone')
  updateTextInput(session, "selectdetectclusters", value = 'notdone')
  updateTextInput(session, "selectmakemapsOESIR", value = 'notdone')
  updateSelectInput(session, "vblePintar", label = "Variable to display",
                    choices = c("Population","Observed cases", "Expected cases","SIR"), selected = "Observed cases")
})

################################################
# END update selectstage label that says if I am in stagehelp or stageuploaddata
################################################


################################################
# INI function fnMakeMapsWithProgress
################################################
fnMakeMapsWithProgress<-function(vecVblePintar, discretedata="no", labelstext=NULL, labelsx=NULL, labelsy=NULL, allmapssamelegend="no"){

  #Make maps
  datosP<-rv$datoswithvaluesforeachidandtime
  #vecVblePintar<-c("Risk","LowerLimitCI","UpperLimitCI")
  vecTimes<-unique(datosP$time)
  n<-length(vecTimes)

  withProgress(message = 'In progress. Trends.', value = 0, {
  ### INI Plots temporal trends. 1 plot for each vble
  for(vp in 1:length(vecVblePintar)){
    nombreGuardar<-paste0("TemporalTrend",vecVblePintar[vp])
    valor<-vecVblePintar[vp]
    g<-ggplot(datosP, aes_string(x = "timeformatted", y = valor, group = "id")) + geom_point(color="darkorange2") + geom_line(color="darkorange2") +
      xlab("") + theme_minimal() + theme(legend.position="none")
    png(paste("plots/",nombreGuardar,".png",sep=""), width=5, height=5, units="in", res=300)
    plot(g)
    dev.off()
  }
  ### END Plots temporal trends
  })


  withProgress(message = 'In progress. Maps.', value = 0, {
    if(allmapssamelegend=="yes"){
      minim<-min(datosP[,vecVblePintar])
      maxim<-max(datosP[,vecVblePintar])
      vecMinlegend<-rep(minim,length(vecVblePintar))
      vecMaxlegend<-rep(maxim,length(vecVblePintar))
    }
    if(allmapssamelegend=="no"){
    #min and max for all times
    if(length(vecVblePintar)>1){
      vecMinlegend<-apply(datosP[,vecVblePintar], 2, FUN=min)
      vecMaxlegend<-apply(datosP[,vecVblePintar], 2, FUN=max)
    }else{
      vecMinlegend<-min(datosP[,vecVblePintar])
      vecMaxlegend<-max(datosP[,vecVblePintar])
    }
    }

    # INI Plots maps
    for(vt in 1:n){
      datostime<-datosP[which(datosP$time==vecTimes[vt]),]
      fnCreateMaps(datostime,vecTimes[vt],rv$map,vecVblePintar,vecMinlegend,vecMaxlegend,discretedata,
                   labelstext,labelsx,labelsy)
      incProgress(1/n, detail = paste("Part", vt,"/",n))
      Sys.sleep(0.1)
    }
  })
}

################################################
# END function fnMakeMapsWithProgress
################################################



output$errorinlaorsatscannotinstalled<-renderText({""})

observeEvent(input$detectclustersButton, {

executablepath<-"ss/SaTScanBatch64.exe"

if(Sys.info()["sysname"]=="Linux"){
executablepath<-"ss/SaTScanBatch64"
}


if(!file.exists(executablepath)){
updateTextInput(session, "selectdetectclusters", value = 'notdone')
msgerrorsatscan <- paste0("To detect clusters SaTScan needs to be installed, and the SaTScanBatch64 executable needs to be placed in the SpatialEpiApp/SpatialEpiApp/ss folder which is located in the R library path.")
output$errorinlaorsatscannotinstalled <- renderText({ print(msgerrorsatscan) })
}

if(file.exists(executablepath)){
    output$errorinlaorsatscannotinstalled <- renderText({""})
    fnDetectClustersSatscanInstalled(executablepath)
}
})


fnDetectClustersSatscanInstalled<-function(executablepath){
  shinyjs::disable("detectclustersButton")
  withProgress(message = 'In progress. Detect clusters.', value = 0, {
    rv$datoswithvaluesforeachidandtime<-fnDetectClusters(rv$datossatscan,rv$map,rv$datoswithvaluesforeachidandtime, rv$usedcovs, executablepath)
    })

  datosP<-rv$datoswithvaluesforeachidandtime
  vecVblePintar<-c("Clusters")
  vecTimes<-unique(datosP$time)
  n<-length(vecTimes)

  withProgress(message = 'In progress. Trends.', value = 0, {

    ### INI Plots temporal trends. 1 plot for each vble
    for(vp in 1:length(vecVblePintar)){
      nombreGuardar<-paste0("TemporalTrend",vecVblePintar[vp])
      valor<-vecVblePintar[vp]
      datosPNA<-datosP
      datosPNA[which(datosPNA$Clusters==0),"Clusters"]<-NA
      g<-ggplot(datosPNA, aes_string(x = "timeformatted", y = valor, group = "id")) + geom_point(color="darkorange2") + geom_line(color="darkorange2") +
        xlab("") + theme_minimal() + theme(legend.position="none") + ylim(0, max(datosP$Clusters))
      png(paste("plots/",nombreGuardar,".png",sep=""), width=5, height=5, units="in", res=300)
      plot(g)
      dev.off()
    }
    ### END Plots temporal trends
  })



  #Make maps
  withProgress(message = 'In progress. Maps.', value = 0, {

    #min and max for all times
    if(length(vecVblePintar)>1){
    vecMinlegend<-apply(datosP[,vecVblePintar], 2, FUN=min)
    vecMaxlegend<-apply(datosP[,vecVblePintar], 2, FUN=max)
    }else{
      vecMinlegend<-min(datosP[,vecVblePintar])
      vecMaxlegend<-max(datosP[,vecVblePintar])
    }

    # Tabla con el informacion para cada cluster
    tabla<-read.table("ss/satscan.col.txt")


    for(vt in 1:n){
      datostime<-datosP[which(datosP$time==vecTimes[vt]),]

      if(ncol(tabla)>1){
      #start date V6, end date V7
      filasparavt<-which(tabla[,6]<=vt & vt<=tabla[,7])
      #Cluster number, lat and long
      labelstext<-tabla[filasparavt,1]
      labelsx<-tabla[filasparavt,3]
      labelsy<-tabla[filasparavt,4]
      }
      if(ncol(tabla)==1){
        labelstext<-NULL
        labelsx<-NULL
        labelsy<-NULL
      }

      fnCreateMaps(datostime,vecTimes[vt],rv$map,vecVblePintar,vecMinlegend,vecMaxlegend,discretedata="yes",
                   labelstext,labelsx,labelsy)

      incProgress(1/n, detail = paste("Part", vt,"/",n))
      Sys.sleep(0.1)
    }
  })


  if(input$selectestimaterisk=="done"){
    updateSelectInput(session, "vblePintar", label = "Variable to display",
                      choices = c("Population","Observed cases", "Expected cases","SIR","Risk","Clusters"),selected = "Observed cases")
  }else{
    updateSelectInput(session, "vblePintar", label = "Variable to display",
                      choices = c("Population","Observed cases", "Expected cases","SIR","Clusters"),selected = "Observed cases")

  }
}



observeEvent(input$makemapsOESIRButton, {
  shinyjs::disable("makemapsOESIRButton")
  vecVblePintar<-c("Population","Observed","Expected","SIR")
  fnMakeMapsWithProgress(vecVblePintar,discretedata="no",labelstext=NULL,labelsx=NULL,labelsy=NULL,allmapssamelegend="no")
})

#############################################################


observeEvent(input$estimateriskButton, {

  inlainstalled<-require(INLA)
  #inlainstalled<-FALSE
  if(!inlainstalled){
    updateTextInput(session, "selectestimaterisk", value = 'notdone')
    msgerrorinla <- paste0("To estimate risk the R-INLA package needs to be installed, http://www.r-inla.org.")
    output$errorinlaorsatscannotinstalled <- renderText({ print(msgerrorinla) })
  }

  if(inlainstalled){
    output$errorinlaorsatscannotinstalled <- renderText({""})
    fnEstimateRiskINLAInstalled()
  }
})

fnEstimateRiskINLAInstalled<-function(){

  shinyjs::disable("estimateriskButton")

  #Estimate risk
  withProgress(message = 'In progress. Estimate risk.', value = 0, {
    rv$datoswithvaluesforeachidandtime<-fnEstimateRisk(rv$datoswithvaluesforeachidandtime,rv$map)
  })

  #Make maps
  vecVblePintar<-c("Risk","LowerLimitCI","UpperLimitCI")
  fnMakeMapsWithProgress(vecVblePintar,discretedata="no",labelstext=NULL,labelsx=NULL,labelsy=NULL,allmapssamelegend="yes")

  if(input$selectdetectclusters=="done"){
    updateSelectInput(session, "vblePintar", label = "Variable to display",
                      choices = c("Population","Observed cases", "Expected cases","SIR","Risk","Clusters"),selected = "Observed cases")
  }else{
    updateSelectInput(session, "vblePintar", label = "Variable to display",
                      choices = c("Population","Observed cases", "Expected cases","SIR","Risk"),selected = "Observed cases")

  }

}





################################################
# INI reactiveValues
################################################

rv <- reactiveValues(
columnidareainmap=NULL,  columnnameareainmap=NULL, columnnamesuperareainmap=NULL,
idpolyhighlighted = NULL, posinmapFilteredIdpolyhighlighted=NULL, colores=NULL,
minrisk=0, maxrisk=1,
vblePintar="Risk", textareareactive="NULL",messageCheckDataText="",
map=NULL,datosOriginal=NULL,
datoswithvaluesforeachidandtime=NULL,
datossatscan=NULL,
lastselectstage=NULL,
usedcovs=NULL,
selectstage='stageuploaddata')




vv<-reactive({

  # When this is updated, the plots are updated.
  # I want this to be updated each time I change dates
  # and also each time I click the buttons to do maps, estimate risk, detect clusters
  aa1<-input$makemapsOESIRButton
  aa2<-input$estimateriskButton
  aa3<-input$detectclustersButton

  if(input$daterange[1]<=input$daterange[2]){
  #calculate this in case timeperiodSlideryear is month

  primerafecha<-paste0(substring(as.Date(input$daterange[1]),1,7),"-01")
  ultimafecha<-paste0(substring(as.Date(input$daterange[2]),1,7),"-01")
  fechas2<-seq(as.Date(primerafecha),as.Date(ultimafecha), "month")

  numfechas <-  1:length(fechas2)


  returnfecha<-switch(input$temporalUnitButton,
                 "Year" = as.Date(paste0(input$timeperiodSlideryear, "-01-01")),
                 "Month" = fechas2[input$timeperiodSlidermonth] ,
                 "Day" = input$timeperiodSliderday )


  return(returnfecha)
  }
})


################################################
# END reactiveValues
################################################



################################################
# INI Update sliderInput
################################################

# Change daterange with min and max of datos

observe({
  #updates timeperiodSlider year, month or day

  if(input$daterange[1]<=input$daterange[2]){
  if(input$temporalUnitButton== "Year" ){
    fechas<-substring(as.Date(input$daterange[1]),1,4):substring(as.Date(input$daterange[2]),1,4)
    if(input$SorSTButton=="S"){ fechas<-fechas[1] }
    updateSliderInput(session, "timeperiodSlideryear",value = fechas[1],  min=fechas[1], max=fechas[length(fechas)])
  }
  if(input$temporalUnitButton== "Month" ){

    primerafecha<-paste0(substring(as.Date(input$daterange[1]),1,7),"-01")
    ultimafecha<-paste0(substring(as.Date(input$daterange[2]),1,7),"-01")
    fechas<-seq(as.Date(primerafecha),as.Date(ultimafecha), "month")

    numfechas <-  1:length(fechas)
    if(input$SorSTButton=="S"){ numfechas<-numfechas[1] }
    updateSliderInput(session, "timeperiodSlidermonth",value = numfechas[1],  min=numfechas[1], max=numfechas[length(numfechas)])
  }
  if(input$temporalUnitButton== "Day" ){
    fechas<-seq(as.Date(input$daterange[1]),as.Date(input$daterange[2]), "day")
    if(input$SorSTButton=="S"){ fechas<-fechas[1] }
    updateSliderInput(session, "timeperiodSliderday",value = fechas[1],  min=fechas[1], max=fechas[length(fechas)])
  }
  }

})
################################################
# END Update sliderInput
################################################


################################################
# INI When I change values in the slider I change minrisk and maxrisk
################################################

observe({
  rv$minrisk<-input$risk[1]
  rv$maxrisk<-input$risk[2]
})

################################################
# END When I change values in the slider I change minrisk and maxrisk
################################################




############################################
# INI input - contents. show map and data that are uploaded
############################################


output$uploadmapmap <- renderPlot({
  if (is.null(rv$map))
    return(NULL)
  plot(rv$map)
})

output$uploadmapsummary <- renderPrint({
  if (!is.null(rv$map)){
    print(summary(rv$map@data))
  }
})

output$uploaddatasummary <- renderPrint({
  if (!is.null(rv$datosOriginal)){
  print(summary(rv$datosOriginal))
  }
})

output$uploadmaptable  <- renderDataTable({
  if (is.null(rv$map))
    return(NULL)
  rv$map@data

})

output$uploaddatatable  <- renderDataTable({
  if (is.null(rv$datosOriginal))
    return(NULL)
  rv$datosOriginal

})

############################################
# END input - contents. show map and data that are uploaded
############################################





################################################
# INI DATA - filter areas in map depending of state and risk
################################################

# Upload shapefile
observe({
  shpdf <- input$filemap
  if(is.null(shpdf)){
    return()
  }
  previouswd <- getwd()
  uploaddirectory <- dirname(shpdf$datapath[1])
  setwd(uploaddirectory)
  for(i in 1:nrow(shpdf)){
      file.rename(shpdf$datapath[i], shpdf$name[i])
    }
  setwd(previouswd)
  #map <- readShapePoly(paste(uploaddirectory, shpdf$name[grep(pattern="*.shp", shpdf$name)], sep="/"),  delete_null_obj=TRUE)
  map <- readOGR(paste(uploaddirectory, shpdf$name[grep(pattern="*.shp", shpdf$name)], sep="/"))#,  delete_null_obj=TRUE)
  map <- spTransform(map, CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"))

  rv$map<-map

})

# Upload data
observe({
inFile <- input$file1
  if (is.null(inFile))
    return(invisible())
rv$datosOriginal<-read.csv(inFile$datapath)

})


# EXECUTE WHEN input$startAnalysisButton

observeEvent(input$startAnalysisButton, {
  shinyjs::disable("startAnalysisButton")

#############################
#DELETE input$useSampleData 4
if(input$useSampleData){
rv$datosOriginal<-read.csv("data/Ohio/dataohiocomplete.csv")
rv$map<-readShapePoly("data/Ohio/fe_2007_39_county/fe_2007_39_county", delete_null_obj=TRUE)
}else{

if (is.null(rv$map)){
  rv$messageCheckDataText<-"Error: Map is not uploaded."
  shinyjs::enable("startAnalysisButton")
  return(NULL)
}

if (is.null(rv$datosOriginal)){
    rv$messageCheckDataText<-"Error: Data are not uploaded."
    shinyjs::enable("startAnalysisButton")
    return(NULL)
}

#This line is in case user clicks starts and names are not already loaded
if(input$columnidareaindata=="" || input$columndateindata=="" || input$columncasesindata=="" || input$columnpopindata==""){
  shinyjs::enable("startAnalysisButton")
  return(NULL)
}

#DELETE input$useSampleData 1
}

#############################

# When I start I remove all files in plots
# In Linux file.remove also removes the folder not only the files.
# So I need to create it again
file.remove(paste0("plots/",list.files("plots")) )
if(!dir.exists("plots")){
dir.create("plots")
}


map<-rv$map
datosOriginal<-rv$datosOriginal



# Names of the columns in the map
rv$columnidareainmap<-input$columnidareainmap
rv$columnnameareainmap<-input$columnnameareainmap
rv$columnnamesuperareainmap<-input$columnnamesuperareainmap

# DELETE input$useSampleData 5
if(input$useSampleData){
rv$columnidareainmap<-"NAME"
rv$columnnameareainmap<-"NAME"
rv$columnnamesuperareainmap<-"-"
}

# Populate selectInput names superarea

xregions<-c("All")
if(rv$columnnamesuperareainmap!="-"){
  xregions<-unique(rv$map@data[, rv$columnnamesuperareainmap])
}
updateSelectInput(session, "superarea", choices = xregions, selected = xregions[1])



# Names of the columns in data
id<-input$columnidareaindata
timeraw<-input$columndateindata
cases<-input$columncasesindata
pop<-input$columnpopindata
cov1<-input$columncov1indata
cov2<-input$columncov2indata
cov3<-input$columncov3indata
cov4<-input$columncov4indata
idinmap<-input$columnidareainmap
temporalunit<-switch(input$temporalUnitButton, "Year" = "year", "Month" = "month", "Day" = "day")

#DELETE input$useSampleData 12
if(input$useSampleData){
  id<-"NAME"
  timeraw<-"year"
  cases<-"y"
  pop<-"n"
  cov1<-"gender"
  cov2<-"race"
  cov3<-"-"
  cov4<-"-"
  idinmap<-"NAME"
  temporalunit<-"year"
}





#Check cases and population are numeric
if(!is.numeric(datosOriginal[,pop]) || !is.numeric(datosOriginal[,cases])){
  rv$messageCheckDataText<-"Error: Population and cases should be numbers."
  shinyjs::enable("startAnalysisButton")
  return(NULL)
}

rv$messageCheckDataText<-""


################################
# INI Construct data with correct names
################################

# Select columns id, timeraw, pop, cases, covs
namesdatos<-c(id,timeraw,pop,cases)
if(cov1!="-"){ namesdatos<-c(namesdatos,cov1)}
if(cov2!="-"){ namesdatos<-c(namesdatos,cov2)}
if(cov3!="-"){ namesdatos<-c(namesdatos,cov3)}
if(cov4!="-"){ namesdatos<-c(namesdatos,cov4)}

datosOriginal2<-data.frame(datosOriginal[,namesdatos])

# Put new names
namesdatosnew<-c("id","timeraw","pop","cases")
usedcovs<-c("cov1","cov2","cov3","cov4")[which((c(cov1,cov2,cov3,cov4)!="-"))]
rv$usedcovs<-usedcovs
if(length(usedcovs)>0){
  namesdatosnew<-c(namesdatosnew,usedcovs)
}
names(datosOriginal2)<-namesdatosnew



################################
# END Construct data with correct names
################################




if(input$daterange[1]>input$daterange[2]){
  rv$messageCheckDataText<-"Error: In Date Range maximum date should be greater or equal than minimum date."
  shinyjs::enable("startAnalysisButton")
  return(NULL)
}



rv$messageCheckDataText<-""


#Check time and create column time adding "-01-01"
datosOriginal2<-fnCheckColumnTime(datosOriginal2, temporalunit)



if(class(datosOriginal2)=="character"){
  rv$messageCheckDataText<-datosOriginal2
  shinyjs::enable("startAnalysisButton")
  return(NULL)
}



rv$messageCheckDataText<-""

#Create daterangeentered according to the temporal unit, ends in "-01-01"
daterangeentered<-switch(input$temporalUnitButton,
                         "Year" = paste0(substring(as.Date(input$daterange),1,4),"-01-01"),
                         "Month" = paste0(substring(as.Date(input$daterange),1,7),"-01"),
                         "Day" = as.Date(input$daterange))

#Update daterange with dates ending in "01-01"
updateDateRangeInput(session, "daterange", start = as.Date(daterangeentered[1]), end = as.Date(daterangeentered[2]))

# Compare ending in "01-01"
# If min date entered in daterange < minimum date in the dataset change the min date in the daterange to the min in the data
if(daterangeentered[1]< min(datosOriginal2$time)){
  updateDateRangeInput(session, "daterange", start = as.Date(min(datosOriginal2$time)))
}
# The same with maximum date
if(daterangeentered[2]>max(datosOriginal2$time)){
  updateDateRangeInput(session, "daterange", end = as.Date(max(datosOriginal2$time)))
}





#Select rows with dates within daterange

#Need to filter by converted input$daterange[1], input$daterange[2]
#If it is year consider year-01-01, if it is month consider year-month-01, if it is day consider the whole date year-month-day
datosOriginal2<-fnFilteredWithinDateRange(datosOriginal2, as.Date(daterangeentered[1]), as.Date(daterangeentered[2]))


if(is.null(datosOriginal2)){
  print("There is no data with those specifications")
  rv$messageCheckDataText<-"There is no data with those specifications"
  shinyjs::enable("startAnalysisButton")
  return(NULL)
}


rv$messageCheckDataText<-""

resCheckData<-fnCheckData(datosOriginal2, usedcovs, map, idinmap, temporalunit)

if(class(resCheckData)=="character"){
  print(resCheckData)
  rv$messageCheckDataText<-resCheckData
  shinyjs::enable("startAnalysisButton")
  return(NULL)
}

  rv$messageCheckDataText<-""

  msgerror<-resCheckData[[1]]
  datos<-resCheckData[[2]]
  numstrata<-resCheckData[[3]]
  vecid<-resCheckData[[4]]
  vectime<-resCheckData[[5]]
  print(msgerror)
  timeformattedIfSpatial=""


  ################################
  # INI if Spatial S
  ################################
  if(input$SorSTButton=="S"){
    #If analysis is spatial. Sum cases and population for each combination of area id and covariates.
    #Poner la fecha igual a la primera fecha

    minmaxdatesindatos<-c( min(datos$timeraw), max(datos$timeraw))

    dd<-switch(input$temporalUnitButton,
               "Year" = substring(minmaxdatesindatos,1,4),
               "Month" = substring(minmaxdatesindatos,1,7),
               "Day" = minmaxdatesindatos)

    timeformattedIfSpatial<-paste(dd[1],"to",dd[2])

    vblesagg<-"id"
    if(length(usedcovs)>0){
      vblesagg<-rev(c(vblesagg,usedcovs))
    }

    listvblesagg<-list()
      for(i in 1:length(vblesagg)){
        listvblesagg[[vblesagg[i]]]<-datos[, vblesagg[i]]
      }


    idspatialAux<-aggregate(x=datos$cases, by=listvblesagg, FUN="sum")

    #datosAux tiene id, y covariables
    datosAux<-idspatialAux[,-which(names(idspatialAux)=="x")]
    casesspatial<- aggregate(x=datos$cases, by=listvblesagg, FUN="sum")[,"x"]
    popspatial<- aggregate(x=datos$pop, by=listvblesagg, FUN="sum")[,"x"]
    timerawspatial<-rep(datos$timeraw[1],length(casesspatial))
    timespatial<-rep(datos$time[1],length(casesspatial))
    #if datosAux does not have covariates, is a vector. I put it as a data.frame
    if(is.null(dim(datosAux))){
      datosAux<-data.frame(id=datosAux)
    }
    datos<-cbind(datosAux, timeraw=timerawspatial, pop=popspatial, cases=casesspatial, time=timespatial)
    vecid<-unique(datos$id)
    vectime<-as.Date(datos$time[1])

  }

  ################################
  # END if Spatial S
  ################################

  # data with id, time, pop, O, E, SIR
  datos2<-fnConstructData(datos, numstrata, vecid, vectime, temporalunit, timeformattedIfSpatial)

  # Create integer idtime for fitting INLA
  uniquetimes<-sort(unique(datos2$time))

  datos2[,"idtime"]<-match(datos2[, "time"], uniquetimes)
  #Create integer id. It is the order where is in the map
  datos2[,"id"]<-match(datos2[, "id"], map@data[, idinmap])

  #Keep the id and name from the beginning. I want to show this information instead of the new created id
  map@data[,"idArea"]<- map@data[,idinmap]
  map@data[,"nameArea"]<- map@data[,rv$columnnameareainmap]
  #idAreaInteger is the order in the map. Auglaize is 1. Scioto is 81
  map@data[,"idAreaInteger"]<-1:nrow(map@data)
  rv$columnidareainmap<-"idAreaInteger"

  ###########################################################################


  # INI datos SaTScan.
  # It has covariates pop, cases, time (o timeraw?) o create idtime new?, id o create id new?
  # Call SaTScan with datos. Add results to  rv$datoswithvaluesforeachidandtime
  datossatscan<-datos
  datossatscan[,"idtime"]<-match(as.Date(datossatscan[, "time"]), uniquetimes)
  datossatscan[,"id"]<-match(datossatscan[, "id"], map@data[,idinmap])
  # END SaTScan


  ###########################################################################

  rv$map<-map
  rv$datoswithvaluesforeachidandtime<-datos2
  rv$datossatscan<-datossatscan

  ###########################################################################

  updateTextInput(session, "selectstage", value = 'stageanalysis')
  shinyjs::enable("startAnalysisButton")
  shinyjs::enable("estimateriskButton")
  shinyjs::enable("detectclustersButton")
  shinyjs::enable("makemapsOESIRButton")

})



output$messageCheckData<-renderText(
 paste(rv$messageCheckDataText)
)


#########################################################################
# INI Construct map@data with columns for each period of time
#########################################################################
# idareamap nameareamap idsuperareamap namesuperareamap | Otime1 Otime2 Otime3


mapWithData<-reactive({

datos<-rv$datoswithvaluesforeachidandtime
map<-rv$map

print(sum(datos$Observed))
print(sum(datos$Expected))

if(is.null(map)){
  return()
}


# 1. Add columns idareamap nameareamap idsuperareamap namesuperareamap
if(rv$columnnamesuperareainmap=="-"){
map@data[,c(rv$columnnamesuperareainmap)]<-rep("All",nrow(map@data))
}
map@data<-map@data[,c(rv$columnidareainmap,rv$columnnameareainmap,"idArea","nameArea",rv$columnnamesuperareainmap)]


# check columnidareainmap and columnidsuperareainmap are unique
# check all elements of columnidareaindata are in columnidareainmap.
# if there is some id in data that is not in map in the analyisis will be NA?


# 2. Add columns for each time .. Otime1 Otime2 Otime3 ... Columns are in datostime
vecTimes<-unique(datos$time)
for(vt in 1:length(vecTimes)){
  datostime<-datos[which(datos$time==vecTimes[vt]),]
  orden<-match(map@data[,rv$columnidareainmap], datostime[, "id"])
  names(datostime)<-paste0(names(datos),"time",vecTimes[vt])
  map@data<-cbind(map@data, datostime[orden,])

}


return(map)
})

#########################################################################
# END Construct map@data with columns for each period of time
#########################################################################




#########################################################################
# INI mapFiltered Filter columns rows by superarea and by range of values
#########################################################################

# I want to modify it when mapWithData() changes and when input$superarea, input$vblePintar vv() time changes
mapFilteredAuxAllTimes<-reactive({

map<-mapWithData()

if(is.null(map)){
  return()
}

# Filter rows by superarea
mapFiltered<-map[which(map@data[, rv$columnnamesuperareainmap] == input$superarea),]
#if(rv$columnnamesuperareainmap=="-" ){
 # hide("superarea")
#}


rv$vblePintar <- switch(input$vblePintar,
                        "Observed cases" = "Observed",
                        "Expected cases" = "Expected",
                        "SIR" = "SIR",
                        "Risk"="Risk",
                        "Clusters"="Clusters",
                        "Population"="Population")

# Calculate min and max of values to put them in the sliderInput
# If I render the leaflet again put idpolyhighlighted so it does not show anything

rv$idpolyhighlighted <-NULL

#updateSliderInput with min and max risk in the filtered dataset
#if(nrow(mapFiltered)>0){
valmin<-max(0,round(min(mapFiltered@data[,paste0(rv$vblePintar,"time", vv())], na.rm=TRUE),2)-0.01)
valmax<-round(max(mapFiltered@data[,paste0(rv$vblePintar,"time", vv())], na.rm=TRUE),2)+0.01
#}

rv$minrisk<-valmin
rv$maxrisk<-valmax
valstep<- (valmax-valmin)/10
updateSliderInput(session, "risk", value = c(valmin,valmax), min = valmin, max = valmax, step = valstep)

return(mapFiltered)


})

#########################################################################
# END mapFiltered Filter columns rows by superarea and by range of values
#########################################################################

mapFilteredAuxWithDates<-reactive({

  mapFiltered<-mapFilteredAuxAllTimes()
  if(is.null(mapFiltered)){
    return()
  }

  # If I render the leaflet again put idpolyhighlighted so it does not show anything
  rv$idpolyhighlighted <-NULL

   #Filter rows by range of values
  #rv$minrisk and rv$maxrisk is input$risk[1] y input$risk[2]
  mapFiltered<-mapFiltered[which(mapFiltered@data[, paste0(rv$vblePintar,"time",vv())]>= rv$minrisk &
                                 mapFiltered@data[, paste0(rv$vblePintar,"time",vv())]<= rv$maxrisk),]


  return(mapFiltered)

})



#########################################################
# INI mapFiltered Filter columns by time
#########################################################

#  mapFilteredAuxAllTimes() returns data filtered by superarea and range of values. It contains all times
#  This function mapFilteredAux() returns just the 4 first columns for id and name of area and superarea and only the time selected
#  Tt will be used for doing the maps


# Execute when mapFilteredAuxAllTimes() changes
# isolate rv$vblePintar and vv
# mapFilteredAux<-eventReactive(mapFilteredAuxPrevio(), {
mapFilteredAux<-reactive({

mapFiltered<-mapFilteredAuxWithDates()
if(is.null(mapFiltered)){
  return()
}


# Columns corresponding to the time selected
sufijotime<-paste0("time",vv())
indexColumnsTime<-grep(sufijotime,names(mapFiltered@data))

# Data of mapFiltered are the 4 first columns of map (id and name areas and superareas) and columns of datos of the time selected
mapFilteredByTimeDatos<-mapFiltered@data[,c(1:5, indexColumnsTime)]
#1:4

# In column names I delete suffix "timei"
names(mapFilteredByTimeDatos)<-c(unlist(strsplit(names(mapFilteredByTimeDatos), sufijotime )))
mapFiltered@data<-mapFilteredByTimeDatos

return(mapFiltered)
})

#########################################################
# END mapFiltered Filter columns by time
#########################################################


################################################
# END DATA - filter areas in map depending of state and risk
################################################




#########################################################
# INI LEAFLET -  create leaflet
#########################################################

# I want to enter when mapFilteredAux() changes. Put isolate in the rest of reactive values rv$

output$leafletMap <- renderLeaflet({

mapFiltered<-mapFilteredAux()

#mapFiltered<-mapFilteredAux()
if(is.null(mapFiltered)){
  return()
}


values<-mapFiltered@data[, isolate(rv$vblePintar)]


pal <- colorNumeric(palette = "Oranges", domain = values)

rv$colores<-pal(values)

# I need the ids for highlight
ids<-as.integer(as.vector(mapFiltered@data[, "id"]))

leaflet(mapFiltered) %>%
        addProviderTiles("OpenStreetMap.BlackAndWhite", options = providerTileOptions(noWrap = TRUE))  %>%
        addPolygons(data=mapFiltered, layerId=ids, fillColor = ~pal(values), smoothFactor = 0.2, fillOpacity = 0.9, stroke=FALSE)  %>%
        addLegend("bottomright", pal = pal, values = values, title = input$vblePintar, labFormat = labelFormat(prefix = ""), opacity = 1)

})




#########################################################
# END LEAFLET -  create leaflet
#########################################################

#########################################################
# INI LEAFLET -  show information of highlighted polygon
#########################################################

output$areaInfoPanel <- renderUI({
if (is.null(input$leafletMap_shape_mouseover) | is.null(rv$idpolyhighlighted)) {
return()
}

mapFiltered<-mapFilteredAux()
if(nrow(mapFiltered)==0){
return(tags$div(tags$strong("No areas with the specifications selected")))
}

selected <- mapFiltered[which(mapFiltered@data[, "id"]==rv$idpolyhighlighted),]

dateshowninmap<-switch(input$temporalUnitButton,
            "Year" = substring(vv(),1,4),
            "Month" = substring(vv(),1,7),
            "Day" = vv())



if(nrow(selected)>0){

  areariskinfo <- paste0("<strong>&nbsp;", selected@data[, "nameArea"], #rv$columnnameareainmap
                         "</strong><br><strong>&nbsp;", selected@data[,"timeformatted"],
                       "</strong><br><strong>&nbsp;Obs.: </strong>" ,round(selected@data[,"Observed"],2),
                       "</strong> <strong>&nbsp;Exp.: </strong>" ,round(selected@data[,"Expected"],2),
                       "</strong> <strong>&nbsp;SIR: </strong>" ,round(selected@data[,"SIR"],2),
                       "</strong>")

#If Risk, LowerLimitCI and UpperLimitCI are calculated
if("Risk" %in% names(selected@data) ){
areariskinfo <- paste0(areariskinfo, "<br><strong>&nbsp;Risk: </strong>" ,round(selected@data[,"Risk"],2),
                         ", 95% CI: (", round(selected@data[,"LowerLimitCI"],2),",", round(selected@data[,"UpperLimitCI"],2),")&nbsp;")
}

if("Clusters" %in% names(selected@data) ){
    areariskinfo <- paste0(areariskinfo, "<br><strong>&nbsp;Clusters: </strong>" ,selected@data[,"Clusters"],"&nbsp;")
}

return(tags$div(HTML(areariskinfo)))}
})

#########################################################
# END LEAFLET -  show information of highlighted polygon
#########################################################



#########################################################
# INI LEAFLET - highlights polygon when mouse hovers
#########################################################

observeEvent(input$leafletMap_shape_mouseover$id,{
if(is.null(input$leafletMap_shape_mouseover)){
return()
}

mapFiltered<-mapFilteredAux()
if(nrow(mapFiltered)>0){
if(input$leafletMap_shape_mouseover$id != "Selected"){
rv$idpolyhighlighted <-input$leafletMap_shape_mouseover$id
rv$posinmapFilteredIdpolyhighlighted<-which(mapFiltered@data[, "id"] == input$leafletMap_shape_mouseover$id)

selected <- mapFiltered[which(mapFiltered@data[, "id"] == input$leafletMap_shape_mouseover$id),]
proxy <- leafletProxy("leafletMap")
proxy %>% removeShape(layerId = "Selected")
proxy %>% addPolygons(data = selected, fillOpacity = 0, color = "black", weight = 3, stroke = TRUE, layerId = "Selected")
}}
})

#########################################################
# END LEAFLET - highlights polygon when mouse hovers
#########################################################

#########################################################
# INI DYGRAPH
#########################################################


# I want to enter when mapFilteredAuxAllTimes() changes. Put isolate in the rest of reactive values rv$

output$dygraphTemporalTrend<-renderDygraph({


mapFilteredAllTimes<-mapFilteredAuxWithDates()
if(is.null(mapFilteredAllTimes)){
  return()
}


if(input$SorSTButton=="S"){
  return()
}

indexColumnsVblePintar<-grep(paste0(isolate(rv$vblePintar),"time"), names(mapFilteredAllTimes))
valuesxts<-mapFilteredAllTimes@data[, indexColumnsVblePintar]

timesxts<-unlist(lapply(
  names(mapFilteredAllTimes)[indexColumnsVblePintar],
  function(x,y){unlist(strsplit(x,y))[[2]]},
  paste0(isolate(rv$vblePintar),"time")))

if(length(timesxts)==1){
  return()
}

datosxts<-NULL
for(lt in 1:nrow(valuesxts)){
  datosxtsrisk<-xts(as.numeric(valuesxts[lt,]), as.Date(strptime(timesxts, format = "%Y-%m-%d")))
  datosxts<-cbind(datosxts, datosxtsrisk)
}

# ids have to coincide with ids of leaflet. In leaflet I have put rv$columnidareaindata. Put the same
ids<-as.integer(as.vector(mapFilteredAllTimes@data[, isolate(rv$columnidareainmap)]))
colnames(datosxts)<-ids


rvcolores<-isolate(rv$colores)


FUNC_JSFormatNumberYear <- "function(x) {return x.getFullYear()}"
FUNC_JSFormatNumberYearMonth <- "function(x) {return x.toString().substring(4,8) + x.getFullYear()}"

#d1<-dygraph(datosxts, elementId="trends")%>%
d1<-dygraph(datosxts)%>%
  dyRangeSelector() %>%
    dyHighlight(highlightCircleSize = 3, highlightSeriesBackgroundAlpha = 0.3, hideOnMouseOut = FALSE)%>%
    #dyLegend(labelsDiv = "legendDivID")%>%
    dyEvent(vv(), strokePattern="dashed") %>%
    dyCSS("dygraph.css") %>%
    dyOptions(drawGrid = FALSE, colors="gray")

#dyOptions( drawGrid = FALSE, colors=rvcolores)


if(input$temporalUnitButton=="Year"){
  d1<- d1 %>%  dyAxis("x", axisLabelFormatter=JS(FUNC_JSFormatNumberYear))
}
if(input$temporalUnitButton=="Month"){
  d1<- d1 %>%  dyAxis("x", axisLabelFormatter=JS(FUNC_JSFormatNumberYearMonth))
}

if(is.null(rv$idpolyhighlighted)){
d1
}else{
  #Add dySeries of the highlighted area
  colorpolyhightlighted<-rvcolores[isolate(rv$posinmapFilteredIdpolyhighlighted)]
  #I can put in dyOptions color=rvcolores so they have the same colours as in the map
  #I want to put in dySeries colorpolyhightlighted but it does not work
  nameseries<-as.character(isolate(rv$idpolyhighlighted))
  d1<- d1 %>%  dySeries(nameseries, strokeWidth=6, label=nameseries)

  }

})




#########################################################
# END DYGRAPH
#########################################################



############################################
# INI table
############################################


datasetInput <- reactive({

  if(is.null(mapFilteredAux())){
    return()
  }
  tablaparamostrar<-mapFilteredAux()@data
  nt<-names(tablaparamostrar)
  tablaparamostrar<-tablaparamostrar[,-which(nt=="time")]
  #I delete 1 and 2 id and name in the map that I put in the beginning because I change them and put them as integer
  #put in columns 1 and 2 timeformatted and id area first. Change names columns at the end
  tablaparamostrar[,1]<-tablaparamostrar[,"timeformatted"]
  tablaparamostrar[,2]<-tablaparamostrar[,"id"]
  #delete id (area), date, idtime
  tablaparamostrar<-tablaparamostrar[,-c(6,11,12)]
  if(rv$columnnamesuperareainmap=="-"){
    tablaparamostrar<-tablaparamostrar[,-5]
  }


  nt<-names(tablaparamostrar)
  nt[1]<- "Date"
  nt[2]<- "ID area numeric"
  nt<-replace(nt, which(nt=="idArea"), "ID area")
  nt<-replace(nt, which(nt=="nameArea"), "Name area")
  names(tablaparamostrar)<-nt

  tablaparamostrar

})


output$tabla <- renderDataTable({
  datasetInput()
})


output$downloadtabla <- downloadHandler(
  filename = function() { paste("table", '.csv', sep='') },
  content = function(file) {
    write.csv(datasetInput(), file)
  }
)


############################################
# END table
############################################



############################################
# INI mainPanel - tabPanel ModelResults
############################################



staticdate<-reactive({
  dd<-NULL
  if(input$SorSTButton=="S"){
    dd<-switch(input$temporalUnitButton,
                    "Year" = substring(input$daterange,1,4),
                    "Month" = substring(input$daterange,1,7),
                    "Day" = input$daterange)
    dd<- paste(dd[1],"to",dd[2])

  }else{
    dd<-switch(input$temporalUnitButton,
               "Year" = substring(vv(),1,4),
               "Month" = substring(vv(),1,7),
               "Day" = vv())
  }
  if(vv()=="0001-01-01"){
    ""
  }else{
    paste("Date:",dd)
  }

})


output$staticdate1<-renderText({
    staticdate()

})

output$staticdate2<-renderText({
  staticdate()
})
output$staticdate3<-renderText({
  staticdate()
})


output$plot1 <- renderImage({
filename <- normalizePath(file.path(paste("plots/Map",vv(),"Population.png",sep="")))
list(src = filename, width = 300, height = 300)}, deleteFile = FALSE)

output$plot2 <- renderImage({
filename <- normalizePath(file.path(paste("plots/Map",vv(),"Observed.png",sep="")))
list(src = filename, width = 300, height = 300)}, deleteFile = FALSE)

output$plot3 <- renderImage({
filename <- normalizePath(file.path(paste("plots/Map",vv(),"Expected.png",sep="")))
list(src = filename, width = 300, height = 300)}, deleteFile = FALSE)

output$plot4 <- renderImage({
filename <- normalizePath(file.path(paste("plots/Map",vv(),"SIR.png",sep="")))
list(src = filename, width = 300, height = 300)}, deleteFile = FALSE)


output$plot5 <- renderImage({
  filename <- normalizePath(file.path(paste("plots/Map",vv(),"Risk.png",sep="")))
  list(src = filename, width = 300, height = 300)}, deleteFile = FALSE)

output$plot6 <- renderImage({
  filename <- normalizePath(file.path(paste("plots/Map",vv(),"LowerLimitCI.png",sep="")))
  list(src = filename, width = 300, height = 300)}, deleteFile = FALSE)

output$plot7 <- renderImage({
  filename <- normalizePath(file.path(paste("plots/Map",vv(),"UpperLimitCI.png",sep="")))
  list(src = filename, width = 300, height = 300)}, deleteFile = FALSE)


output$plot1t <- renderImage({
  fecha<-vv()
  filename <- normalizePath(file.path(paste("plots/TemporalTrend","Population.png",sep="")))
  list(src = filename, width = 300, height = 300)}, deleteFile = FALSE)

output$plot2t <- renderImage({
  fecha<-vv()
  filename <- normalizePath(file.path(paste("plots/TemporalTrend","Observed.png",sep="")))
  list(src = filename, width = 300, height = 300)}, deleteFile = FALSE)

output$plot3t <- renderImage({
  fecha<-vv()
  filename <- normalizePath(file.path(paste("plots/TemporalTrend","Expected.png",sep="")))
  list(src = filename, width = 300, height = 300)}, deleteFile = FALSE)

output$plot4t <- renderImage({
  fecha<-vv()
  filename <- normalizePath(file.path(paste("plots/TemporalTrend","SIR.png",sep="")))
  list(src = filename, width = 300, height = 300)}, deleteFile = FALSE)


output$plot5t <- renderImage({
  fecha<-vv()
  filename <- normalizePath(file.path(paste("plots/TemporalTrend","Risk.png",sep="")))
  list(src = filename, width = 300, height = 300)}, deleteFile = FALSE)

output$plot6t <- renderImage({
  fecha<-vv()
  filename <- normalizePath(file.path(paste("plots/TemporalTrend","LowerLimitCI.png",sep="")))
  list(src = filename, width = 300, height = 300)}, deleteFile = FALSE)

output$plot7t <- renderImage({
  fecha<-vv()
  filename <- normalizePath(file.path(paste("plots/TemporalTrend","UpperLimitCI.png",sep="")))
  list(src = filename, width = 300, height = 300)}, deleteFile = FALSE)




output$plotclusters <- renderImage({
  filename <- normalizePath(file.path(paste("plots/Map",vv(),"Clusters.png",sep="")))
  list(src = filename, width = 300, height = 300)}, deleteFile = FALSE)

output$plotclusterst <- renderImage({
  fecha<-vv()
  filename <- normalizePath(file.path(paste("plots/TemporalTrend","Clusters.png",sep="")))
  list(src = filename, width = 300, height = 300)}, deleteFile = FALSE)


############################################
# END mainPanel - tabPanel ModelResults
############################################



fechasformatocorrecto<-reactive({

  if(input$temporalUnitButton== "Year" ){
    fechas<-substring(as.Date(input$daterange[1]),1,4):substring(as.Date(input$daterange[2]),1,4)
    }
  if(input$temporalUnitButton== "Month" ){
    primerafecha<-paste0(substring(as.Date(input$daterange[1]),1,7),"-01")
    ultimafecha<-paste0(substring(as.Date(input$daterange[2]),1,7),"-01")
    fechas<-seq(as.Date(primerafecha),as.Date(ultimafecha), "month")

    fechas<-substring(fechas,1,7)
  }
  if(input$temporalUnitButton== "Day" ){
    fechas<-seq(as.Date(input$daterange[1]),as.Date(input$daterange[2]), "day")
  }
  if(input$SorSTButton=="S"){
    fechas<-paste(fechas[1] ,"to", fechas[length(fechas)])
  }
  return(fechas)

})




tablaclusterscol<- reactive({
  tabla<-read.table("ss/satscan.col.txt")

  if(is.null(tabla)){
    return()
  }



  if(ncol(tabla)>1){

  #Write fechas
  fechas<-fechasformatocorrecto()
  #start and end dates are 1, 2,.... Change by actual dates
  fechastart<-tabla[,6]
  fechaend<-tabla[,7]

  tabla$s<-fechas[fechastart]
  tabla$e<-fechas[fechaend]

  #Write names areas
  tabla$namesareas<-tabla[,1]

  vecIdclusters<-tabla[,1]
  gis<-read.table("ss/satscan.gis.txt")
  #put i to the rows corresponding to each cluster
  for(i in 1:length(vecIdclusters)){
    areascluster<-gis[which(gis[,2]==vecIdclusters[i]), 1]
    ac<-which( rv$map@data$idAreaInteger %in% areascluster)
    tabla[i,"namesareas"]<-paste(rv$map@data[ac,"nameArea"],collapse=", ")

    #Central location
    ac<-which( rv$map@data$idAreaInteger %in% tabla[i,2])
    tabla[i,2]<-paste(rv$map@data[ac,"nameArea"],collapse=", ")

  }
  }

  if(ncol(tabla)==1){
    tabla<-data.frame(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18)
    tabla<-tabla[-1,]
  }


  colnames(tabla)<-c("Cluster", "Central area", "lat", "long", "radius", "Start date", "End date", "No. areas", "LLR", "p-value",
                     "Observed", "Expected", "SIR", "Risk in / Risk out","nose","Start date","End date","Areas")

   # if it is spatial do not put columns start, end

  return(tabla[,c(1,2,8,16,17,14,9,10,18)])

})




output$tablaclusters<- renderDataTable({
  tabla<-tablaclusterscol()
  if(is.null(tabla)){
    return()
  }
  print(tabla)
})



output$staticsummary <- renderPrint({
  if(is.null(rv$datoswithvaluesforeachidandtime)){
    return()
  }
  datosP<-rv$datoswithvaluesforeachidandtime
  datostime<-datosP[which(datosP$time==vv()),]
  vecVbles<-c("Population","Observed","Expected","SIR")
  if(input$selectestimaterisk=="done"){
  vecVbles<-c(vecVbles,"Risk","LowerLimitCI","UpperLimitCI")
  }
  s<-datostime[, vecVbles]
    print(summary(s))

})



########################################################
# INI download report with the disease mapping results
########################################################


vecVblesPintarReport<-reactive({
  vecVbles<-NULL
  if(input$selectmakemapsOESIR=="done"){
    vecVbles<-c("Population","Observed","Expected","SIR")
  }
  if(input$selectestimaterisk=="done"){
    vecVbles<-c(vecVbles,"Risk","LowerLimitCI","UpperLimitCI")
  }
  if(input$selectdetectclusters=="done"){
    vecVbles<-c(vecVbles,"Clusters")
  }
  return(vecVbles)
})

vecVblesTablaReport<-reactive({
  vecVbles<-NULL
    vecVbles<-c("Population","Observed","Expected","SIR")
  if(input$selectestimaterisk=="done"){
    vecVbles<-c(vecVbles,"Risk","LowerLimitCI","UpperLimitCI")
  }
  if(input$selectdetectclusters=="done"){
    vecVbles<-c(vecVbles,"Clusters")
  }
  return(vecVbles)
})



output$report <- downloadHandler(
  # For PDF output, change this to "report.pdf"
  filename = "report.pdf",
  content = function(file) {
    # Copy the report file to a temporary directory before processing it, in
    # case we don't have write permissions to the current working dir (which
    # can happen when deployed).
    tempReport <- file.path(tempdir(), "report.Rmd")
    file.copy("report.Rmd", tempReport, overwrite = TRUE)
    # Set up parameters to pass to Rmd document
    #params <- list(n = input$risk)
    datosP<-rv$datoswithvaluesforeachidandtime

    vpr<-vecVblesPintarReport()[which(vecVblesPintarReport() %in% input$rmaps)]
    vtr<-vecVblesTablaReport()[which(vecVblesTablaReport() %in% input$rtables)]
    #there is just one option in input$rtablesclusters. that is why I do not compare with "Clusters"
    if(!is.null(input$rtablesclusters) & input$selectdetectclusters=="done"){
      tc<-tablaclusterscol()
      if(nrow(tc)==0){
        tc<-NULL
      }
    }else{
      tc<-NULL
    }


    params <- list(daterange = dr(), typeofanalysis=sorst() , temporalunit=input$temporalUnitButton, datosP=datosP,
                   fechasformatocorrecto=fechasformatocorrecto(),
                   vecVblesPintar=vpr, vecVblesTabla=vtr, tablaClusters=tc)
    print(params)
    tempReport <- file.path("report.Rmd")

    # Knit the document, passing in the `params` list, and eval it in a
    # child of the global environment (this isolates the code in the document
    # from the code in this app).
    rmarkdown::render(tempReport,  output_file = file,
                      params = params,
                      envir = new.env(parent = globalenv())

    )
  }
)
########################################################
# END download report with the disease mapping results
########################################################

onSessionEnded(function() {
  stopApp()
})

})

