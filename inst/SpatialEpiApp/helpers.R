#' Check dates are correct and create a new column with dates in format yyyy-mm-dd that is added to the data
#' 
#' This function creates a new column time = as.character(as.Date(fechas)) that is added to the data.
#' By executing as.Date we check input dates are correct. If as.Date gives error there is something wrong in the input files.
#' Also, the new column time will have dates with month and days with two digits (2000-01-01).
#' In this way we avoid some dates have zeros and others not
#' 
#' @param datos data.frame with the data
#' @param temporalunit character that can be "year", "month", "day"
#' @return Data.frame with data including a new column time
#' 
fnCheckColumnTime<-function(datos, temporalunit){
  
  incorrecto<-sapply(datos$timeraw, fnCheckTimeRaw, temporalunit)
  if(sum(incorrecto)>0){
    return(paste("Incorrect file input. Dates or temporal unit are not correct."))
  }
  
  
  #Add days from year, month, day
  datos$timeraw<-switch(temporalunit,
                        "year"= paste0(datos$timeraw,"-01-01"),
                        "month"= paste0(datos$timeraw,"-01"),
                        "day"= paste0(datos$timeraw))


  #Convert times to dates. Create new column time 
  convertedtimes<-tryCatch(as.character(as.Date(datos$timeraw)), error=function(e) NA)
  if(sum(is.na(convertedtimes))>0){
    return(paste("Incorrect file input. Dates or temporal unit are not correct."))
  }else{
    datos$time<-convertedtimes
  }
  return(datos)
}


#' It checks length of the dates is consistent with the temporal unit
#' 
#' This function is used inside fnCheckColumnTime function. All dates are checked
#' 
#' @param ffecha date to be checked
#' @param temporalunit temporal unit to be checked
#' @return  locical value that is TRUE if dates and temporal unit are incorrect or FALSE if they are OK
fnCheckTimeRaw<-function(ffecha, temporalunit){
lfecha<-nchar(paste0(ffecha))
incorrecto<- (lfecha <=3 | lfecha ==5  | lfecha>=11) ||  (lfecha == 4 & temporalunit!="year")  ||   
    (6 <= lfecha & lfecha <= 7 & temporalunit!="month") || (8 <= lfecha & lfecha <= 10 & temporalunit!="day")
  return(incorrecto)
} 


#' Select data rows with dates within a date range
#' 
#' @param datos data.frame with the data
#' @param daterangeminentered minimum date in the date range
#' @param daterangemaxentered maximum date in the date range
#' @return Data.frame with filtered data
#'
fnFilteredWithinDateRange<-function(datos, daterangeminentered, daterangemaxentered){
  indicesdatos<-which(datos$time>=daterangeminentered & datos$time<=daterangemaxentered)
  if(length(indicesdatos)>0){
  datos<-datos[indicesdatos,]
  }else{
    datos<-NULL
  }
  return(datos)
}



#' Check data are correct
#' 
#' 1. Check rows are unique
#' 2. Check all idareaindatos are in idareainmap
#' 3. Create data.frame dataAllAttributes with all combinations of id, timeraw, cov1, cov2, cov3, cov4
#' 4. Compare datos with dataAllAttributes. Return error if there are elements in dataAllAttributes that are not in datos
#' 
#' @param datos data.frame with the data
#' @param usedcovs vector with the names of the covariates
#' @param map shapefile with the map
#' @param idinmap
#' @param temporalunit character that can be "year", "month", "day"
#' @return list with elements paste("There are no errors"), datos, numstrata (number of strata),
#'         vecid (unique ids), vectime (consecutive dates between min and max)
fnCheckData<-function(datos, usedcovs, map, idinmap, temporalunit){

colsAttributes<-c("id","timeraw")
if(length(usedcovs)>0){
colsAttributes<-c(colsAttributes,usedcovs)
}
  
  
################################
# INI Check rows are unique, all idareaindatos are in idareainmap
################################



#1 Check rows in datos are unique.
numduplicatedrows<-nrow(datos[,colsAttributes])- nrow(unique(datos[,colsAttributes]))
if(numduplicatedrows>0){
  return(paste("Incorrect file input. Data have", numduplicatedrows, "rows with the same attributes"))
  #datos<-datos[-which(duplicated(datos[,colsAttributes])==TRUE),]
}

 
#2.1 Check idareaindata are all in map@data$id
areasindatanotinmap<- length(which(!unique(datos$id)%in% unique(map@data[,idinmap])))
if(areasindatanotinmap>0){
  return(paste("Incorrect file input. There are", areasindatanotinmap, "areas in the data file that are not in the map file"))
}

 


# I have added lines to remove areas in map that are not in dataset. Remove those lines?
#2.2 Check  map@data$id are all in idareaindata
areasinmapnotindata<- length(which(!unique(map@data[,idinmap])%in% unique(datos$id)))
if(areasinmapnotindata>0){
  indii=unique(map@data[,idinmap])[which(!unique(map@data[,idinmap])%in% unique(datos$id))]
  #print(map@data[which(map@data[,idinmap]%in% indii), ])
  map<-map[-which(map@data[,idinmap]%in% indii),]
  return(paste("Incorrect file input. There are", areasinmapnotindata, "areas in the map file that are not in the data file"))
}


################################
# END Check rows are unique, all idareaindatos are in idareainmap
################################


################################
# INI Create dataAllAttributes
################################

# Create data.frame dataAllAttributes with all combinations of id, timeraw, cov1, cov2, cov3, cov4

#Select unique id
vecid<-unique(datos$id)

#Calculate all consecutive dates between min and max
vectime<-seq(as.Date(min(datos$time)),as.Date(max(datos$time)),by=temporalunit)
length(vectime)

#Select unique cov1, cov2, cov3, cov4
# and create listexpand to pass as argument in expand.grid
listveccov<-list(cov1=1, cov2=1, cov3=1, cov4=1)
# Write cov1=1 instead of cov1=NULL because I do length(listveccov[[3]]) and want length to be 1 if there are no covariates

#listexpand<-list(id=vecid, time=vectime, cov1=unique(datos[, usedcovs[1]]), cov2=unique(datos[, usedcovs[2]]))



listexpand<-list(id=vecid, time=vectime)
if(length(usedcovs)>0){
  for(i in 1:length(usedcovs)){
    listveccov[[i]]<-unique(datos[, usedcovs[i]])
    listexpand[[usedcovs[i]]]<-listveccov[[i]]
  }
}

# Create dataAllAttributes. data.frame with all combinations of id, timeraw, cov1, cov2
dataAllAttributes <- expand.grid(listexpand)

################################
# END Create dataAllAttributes
################################


################################
# INI Compare datos with dataAllAttributes
################################

#Compare datos with dataAllAttributes. STOP If there are elements in dataAllAttributes that are not in datos

#colsAttributes<-c("id","time","cov1","cov2")
#Put time instead of timeraw
colsAttributes<-replace(colsAttributes, which(colsAttributes=="timeraw"), "time")

# Sort datos and dataAllAttributes
datos<-datos[ do.call("order", datos[colsAttributes]), ]
dataAllAttributes<-dataAllAttributes[ do.call("order", dataAllAttributes[colsAttributes]), ]


fnReturnRowsInANotinB <- function(x.1,x.2,...){
  x.1p <- do.call("paste", x.1)
  x.2p <- do.call("paste", x.2)
  x.1[! x.1p %in% x.2p, ]
}

# Print rows in dataAllAttributes but not in datos
rowsinAnotinB<-fnReturnRowsInANotinB(dataAllAttributes[,colsAttributes], datos[,colsAttributes])
if(nrow(rowsinAnotinB)>0){
  return(paste("Incorrect file input. There are not data corresponding to ",nrow(rowsinAnotinB)," attributes. For example: ",
               paste(do.call("paste", head(rowsinAnotinB)),collapse=" , ")))
}



################################
# END Compare datos with dataAllAttributes
################################

################################
# INI Return paste("No errors") if there are no errors
################################


#numstrata will be used to calculate expected
numstrata<-length(listveccov[[1]])*length(listveccov[[2]])*length(listveccov[[3]])*length(listveccov[[4]])

return(list(paste("There are no errors"), datos, numstrata, vecid, vectime))

################################
# END Return paste("No errors") if there are no errors
################################
}

 
#' Construct data to be merged with map with columns id, time, pop, O, E, SIR, timeformatted
#' 
#' @param datos data.frame with the data
#' @param numstrata number of strata, calculated multiplying the number of categories in each covariate
#' @param vecid vector with the unique datos$id
#' @param vectime vector with all consecutive dates between min and max
#' @param temporalunit character that can be "year", "month", "day"
#' @param timeformattedIfSpatial
#' @return Data.frame with columns id, time, pop, O, E, SIR, timeformatted
#'
#'
fnConstructData<-function(datos, numstrata, vecid, vectime, temporalunit, timeformattedIfSpatial){


# To have this configuration as expand.grid first I need to aggregate time, id
#id1 time1
#id1 time2
#id1 time3
#id2 time1
#id2 time2
#id2 time3
  
  
#Calculate O
observados<- aggregate(x=datos$cases, by=list(time = datos$time, id = datos$id), FUN="sum")[,"x"]
#Calculate pop
poblacion<- aggregate(x=datos$pop, by=list(time = datos$time, id = datos$id), FUN="sum")[,"x"]
#Calculate E. id is ID and time
#numstrata<-length(listveccov[[1]])*length(listveccov[[2]])*length(listveccov[[3]])*length(listveccov[[4]])
esperados<-expected(datos$pop, datos$cases, numstrata)

#Create data.frame id, time, pop, O, E, SIR
datos2 <- expand.grid(id=vecid, time=vectime)
datos2<-datos2[ with(datos2, order(id, time)), ]
head(datos2)
datos2$Population<-poblacion
datos2$Observed<-observados
datos2$Expected<-esperados
datos2$SIR<-datos2$O/datos2$E 

#Add  "Mean", "2.5Percentile", "97.5Percentile"?

datos2$timeformatted<-switch(temporalunit,
                            "year"= substring(as.character(datos2$time),1,4),
                            "month"= substring(as.character(datos2$time),1,7),
                            "day"= as.character(datos2$time))


if(timeformattedIfSpatial!=""){
    datos2$timeformatted<-rep(timeformattedIfSpatial, nrow(datos2))
}


return(datos2)

}



#' Creates plots showing the values of the variables to plot in each of the areas of the map
#' 
#' @param maparea map with areas to be plotted. Calculated using fortify and left_join in function fnCreateMaps
#' @param vblePintar string with the name of the variable to be plotted
#' @param nombreGuardar character with the name of the plot to be saved
#' @param minlegend minimum value of the legend in the plot
#' @param maxlegend maximum value of the legend in the plot
#' @param discretedata character denoting if the variable is discrete ("yes") or not ("no")
#' @param labelstext label argument of the aesthetics geom_text
#' @param labelsx x argument of the aesthetics geom_text
#' @param labelsy y argument of the aesthetics geom_text
#' @return Creates maps that are saved in png format
#' 
fnPlotMap<-function(maparea, vblePintar, nombreGuardar, minlegend, maxlegend, discretedata, labelstext, labelsx, labelsy){
  
  myPalette <- colorRampPalette(rev(brewer.pal(9, "Oranges")))
  sc<-scale_fill_gradientn(colours = rev(myPalette(100)), limits=c(minlegend, maxlegend))
  gt<-NULL
  leyendaquitar<-NULL
  
  if(discretedata=="yes"){
    # maxlegend+1 (num clusters+0) colours
    valorescolor<- colorRampPalette(brewer.pal(8, "Spectral"))(maxlegend+1)
    #Pick colours form the number of the cluster. This is for making cluster colours to coincide in time
    #Sum 1 because position 0 does not exist
    valorescolor2<-valorescolor[1+ sort(unique(maparea[,vblePintar]))]
    valorescolor2[1]<-"#D3D3D3"
    sc<-scale_fill_manual(values = valorescolor2)
    
    gt<-geom_text(aes(label = labelstext, x = labelsx, y = labelsy))
    leyendaquitar<- guides(fill=FALSE)
    
    maparea[,vblePintar]<-as.factor(maparea[,vblePintar])
    
   }
  
    g <- ggplot() + geom_polygon(data = maparea, aes_string(x = "long", y = "lat", group = "group", fill=vblePintar)) +
            # delete map projection
            # coord_map() +
      theme_minimal() + sc + gt + leyendaquitar

   png(paste("plots/",nombreGuardar,".png",sep=""), width=5, height=5, units="in", res=300)
   plot(g)
   dev.off()
    
}
  
  

  


#' Function to create maps
#' 
#' @param datostime data corresponding just a given date
#' @param vectimesvt date of the values to be plotted
#' @param map shapefile with the map
#' @param vecVblePintar vector containing the variables to be plotted. E.g. vecVblePintar<-c("Population","Observed","Expected","SIR")
#' @param vecMinlegend vector containing the minimum values of the legends in the plots for each variable
#' @param vecMaxlegend vector containing the maximum values of the legends in the plots for each variable
#' @param discretedata character denoting if the variable is discrete ("yes") or not ("no")
#' @param labelstext label argument of the aesthetics geom_text
#' @param labelsx x argument of the aesthetics geom_text
#' @param labelsy y argument of the aesthetics geom_text
#' @return Calls function fnPlotMap that creates maps that are saved in png format
#' 
fnCreateMaps<-function(datostime, vectimesvt, map, vecVblePintar, vecMinlegend, vecMaxlegend, discretedata, labelstext, labelsx, labelsy){
  ##datos<-rv$datoswithvaluesforeachidandtime
  ##map<-rv$map
  
  #pongofuera vecTimes<-unique(datos$time)
  #pongofuera for(vt in 1:length(vecTimes)){
  #pongofuera  datostime<-datos[which(datos$time==vecTimes[vt]),]
    
    orden<-match(map@data[,"idAreaInteger"], datostime[, "id"])
    #names(datostime)<-paste0(names(datos),"time",vecTimes[vt])
    
    #map@data<-cbind(map@data, datostime[orden,])
    

    #Plot
    dd<-data.frame(datostime[orden,])
    dd$id<-as.character(dd$id)
    # Convert the shapefile to a dataframe for use in ggplot2
    maparea <- fortify(map, region="idAreaInteger")
    maparea <- left_join(maparea, dd, by = c("id" = "id"))
    
    
    for(vp in 1:length(vecVblePintar)){
    fnPlotMap(maparea, vecVblePintar[vp], nombreGuardar=paste0("Map", vectimesvt, vecVblePintar[vp]), vecMinlegend[vp], vecMaxlegend[vp],
              discretedata, labelstext, labelsx, labelsy)
    
        #pongofuera }
    
  }}




 
#' Detect clusters by using the SaTScan software
#' 
#' This function creates the files needed to run SaTScan, calls SaTScan, and postprocesses the output files to
#' add a new column Clusters to the data with the clusters detected
#' Entries in column Clusters are 0 if areas do not pertain to a cluster.
#' Entries corresponding to areas that pertain to cluster i have value i, i=1, 2, ...
#' 
#' @param datossatscan data.frame with the data
#' @param map shapefile with the map
#' @param datos data.frame with the data
#' @param usedcovs vector with the names of the covariates
#' @param executablepath path to the SaTScanBatch executable. The path is different in Windows and Linux
#' @return Data.frame with data including new column Clusters with the number of the detected clusters
#' 
fnDetectClusters <- function(datossatscan, map, datos, usedcovs, executablepath) {
  
  # 1. Create files to call SaTScan using datossatscan. idtime (1,2,...)
  #.cas
  columnas<-c("id","cases","idtime")
  if(length(usedcovs)>0){columnas<-c(columnas,usedcovs)}
  write.table(datossatscan[, columnas], "ss/satscan.cas", row.names=FALSE, col.names=FALSE)
  #.pop
  columnas<-c("id","idtime","pop")
  if(length(usedcovs)>0){columnas<-c(columnas,usedcovs)}
  write.table(datossatscan[, columnas], "ss/satscan.pop", row.names=FALSE, col.names=FALSE)
  #.geo
  write.table(cbind(map@data[,"idAreaInteger"], coordinates(map)), "ss/satscan.geo", row.names=FALSE, col.names=FALSE)
  
  
  # 2. Create .prm
  aa=readLines("ss/satscanplantilla.prm")
  mind<-min(datossatscan$idtime)
  maxd<-max(datossatscan$idtime)
  aa[9]<-paste0("StartDate=",mind)
  aa[11]<-paste0("EndDate=",maxd)
  if (mind != maxd){
    aa[25]<-paste0("AnalysisType=",3)
    
  } 
  aa[119]<-paste0("IntervalStartRange=",mind,",",maxd)
  aa[121]<-paste0("IntervalEndRange=",mind,",",maxd)
  #aa[151]<-paste0("ProspectiveStartDate=",maxd)
  
  writeLines(aa,"ss/satscan.prm")
  
  print('start SaTScan')
  # 3. Call SaTScan
  #system(paste('"ss/SaTScanBatch64.exe"', 'ss/satscan.prm'), wait = TRUE)
  system(paste(executablepath, 'ss/satscan.prm'), wait = TRUE)
  print('finish SaTScan')
  
  # 4. Select the clusters with p-value less 0.05
  
  # 4. Add Results to data
  #Areas that do not pertain to a cluster have 0. The areas that pertain to cluster i have value i, i=1,2,...
  datos$Clusters<-0
  
  #col: V1 number cluster, V6 start date, V7 end date
  col<-tryCatch(read.table("ss/satscan.col.txt"), error=function(e) NA)
  
  if(is.na(col)){
    #If there are no clusters satscan.col.txt is empty. I assign col a data.frame(NULL) with 0 columns
    col<-data.frame(NULL)
  }else{
    #pvalue less than 0.05
    col<-col[which(col[,10]<0.05),]
  }
  

  if(nrow(col)==0){
    write.table(NA,"ss/satscan.col.txt")
  }
    
  if(nrow(col)>0){
  write.table(col,"ss/satscan.col.txt")
    
  vecIdclusters<-col[,1]
  vecStartdateclusters<-col[,6]
  vecFinishdateclusters<-col[,7]
  
  #gis: V1 area id, V2 number cluster
  gis<-read.table("ss/satscan.gis.txt")
  #put i to the rows corresponding to each cluster
  for(i in 1:length(vecIdclusters)){
    areascluster<-gis[which(gis[,2]==vecIdclusters[i]), 1]
    startdate<-vecStartdateclusters[i]
    finishdate<-vecFinishdateclusters[i]
    datos[which(datos$idtime >=startdate &  datos$idtime <=finishdate & datos$id %in% areascluster), "Clusters"]<-i
  }
  }
  
  return(datos)
}
 


#' Estimate risk, and 95% CI by fitting a Bayesian model using INLA
#' 
#' The model fitted is the BYM model is there is one period of time,
#' and the model introduced by Bernardinelli et al. 1995 if there is more the one period of time
#' 
#' @param datos data.frame with the data. It has columns O, E, id, idtime
#' @param map shapefile with the map. It is needed to construct the neighbourhood matrix
#' @return Data.frame with data including new columns Risk, lower and upper limits of the 95% CI
#' 
fnEstimateRisk <- function(datos, map) {

  datos1<-datos
  
  #create adjacency matrix
  map.nb <- poly2nb(map)
  
  #Convert the adjacency matrix into a file in the INLA format
  nb2INLA("map.adj", map.nb)
  g = inla.read.graph(filename="map.adj")
  
  #indices in datos id and idtime
  #idAreaInteger in index in map
  
  #create indices random effects area
  datos1$ID.area<-datos1$id
  datos1$ID.area1<-datos1$id
  
  #create indices random effects time
  datos1$ID.time<-datos1$idtime
  datos1$ID.time1<-datos1$idtime
  
  #Formula spatial
  formula = Observed ~ f(ID.area, model="besag", graph=g, adjust.for.con.comp=TRUE) + f(ID.area1, model="iid")
  
  #Formula spatio-temporal
  if(length(unique(datos1$idtime))>1){
  # Bernardinelli
   formula = Observed ~ f(ID.area, model="bym", graph=g, adjust.for.con.comp=TRUE) + f(ID.area1, ID.time, model="iid",constr=TRUE) + ID.time
  }
  
  print('start INLA')
  res  =  inla(formula, family="poisson", data=datos1, E=Expected,
               control.predictor=list(compute=TRUE), control.compute=list(dic=TRUE), quantiles=c(0.025,0.975))
  print('finish INLA')
   
  #Get Results
  datos$Risk<-res$summary.fitted.values[,"mean"]
  datos$LowerLimitCI<-res$summary.fitted.values[,"0.025quant"]
  datos$UpperLimitCI<-res$summary.fitted.values[,"0.975quant"]
  
  return(datos)
  
}

  
