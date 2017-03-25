library(shiny)
library(shinyjs)
library(htmlwidgets)
library(leaflet)
library(dygraphs)
library(rmarkdown)
library(knitr)
#library(INLA)
library(SpatialEpi)
library(maptools)
library(spdep)
library(dplyr)
library(xts)
library(ggplot2)
library(RColorBrewer)
library(rgeos)
library(mapproj)
library(rgdal)



shinyUI(fluidPage(
useShinyjs(),

###########################################################################
###########################################################################
# TITLE
###########################################################################
###########################################################################

wellPanel(
  titlePanel(
    fluidRow(
      column(10,
             div("SpatialEpiApp", style="margin-left:10px;" ),
             div("Shiny Web Application for the Analysis of Spatial and Spatio-Temporal Disease Data", style="padding:10px; font-size:70%;")
             )))),

div(style="display: inline-block;vertical-align:middle;", actionButton("helpButton", strong("Help") )),


###########################################################################
###########################################################################
# STAGEHELP
###########################################################################
###########################################################################

conditionalPanel(condition = "input.selectstage == 'stagehelp'",

div(style="display: inline-block;vertical-align:middle;", h3("Help")),
div(style="display: inline-block;vertical-align:middle;", h3(" ")),
div(style="display: inline-block;vertical-align:middle;", actionButton("returnButton", icon = icon("reply"),label="")),
fluidRow(column(12,HTML("<br>"))),

tabsetPanel(
tabPanel("Application", column(12, includeMarkdown("helpinput.md"))),
tabPanel("Methodology", column(12, includeMarkdown("helpanalysis.md"))),
tabPanel("Manual", tags$iframe(src="manual.pdf", width="100%", height="600px", scrolling="auto")),
tabPanel("About", column(12, includeMarkdown("helpabout.md"))),

textInput("selectstage", label="", value="stageuploaddata", width='300px'),
textInput("selectestimaterisk", label="", value="notdone", width='300px'),
textInput("selectdetectclusters", label="", value="notdone", width='300px'),
textInput("selectmakemapsOESIR", label="", value="notdone", width='300px')
)),


###########################################################################
###########################################################################
# STAGEUPLOADDATA
###########################################################################
###########################################################################

conditionalPanel(condition = "input.selectstage == 'stageuploaddata'",
                 div(style="display: inline-block;vertical-align:middle;", h3("Inputs")),
                 div(style="display: inline-block;vertical-align:middle;", h3(" ")),


fluidRow(
  column(4, style = "height:750px;  margin:10px; border-style: solid;border-color:gray",

         h3("1. Upload map (shapefile)"),

         helpText("Upload all map files at once: shp, dbf, shx and prj."),
         fileInput("filemap", "", accept=c('.shp','.dbf','.sbn','.sbx','.shx',".prj"), multiple=TRUE),

         helpText("Select columns id and name of the areas in the map."),
         fluidRow(column(6, selectInput("columnidareainmap",   label = "area id",   choices = c(""), selected = "")),
                  column(6, selectInput("columnnameareainmap", label = "area name", choices = c(""), selected = ""))),

         fluidRow(column(12, helpText(div("Optional. Select column name of the regions in the map."),
                                      div("If the number of areas is big, the leaflet map will not render. By specifying regions containing a small number of areas,
                                          only areas within the selected region will be shown in the interactive results.")))),
         fluidRow(column(6, selectInput("columnnamesuperareainmap", label = "region name", choices = c(""), selected = "")))),



  column(4, style = "height:750px;  margin:10px; border-style: solid;border-color:gray",

         h3("2. Upload data (.csv file)"),

         helpText(div("File needs to have columns",  strong("<area id><date><population><cases>"),"."),
                  div("Optional. It can also include columns with up to four covariates",strong("<covariate1>...<covariate4>"),".")),
         fileInput("file1", "", accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv")),

         helpText("Select columns id, date, population and cases in the data."),
         fluidRow(column(6, selectInput("columnidareaindata",  label = "area id",  choices = c(""), selected = "")),
                  column(6, selectInput("columndateindata", label = "date", choices = c(""), selected = ""))),
         fluidRow(column(6, selectInput("columnpopindata", label = "population", choices = c(""), selected = "")),
                  column(6, selectInput("columncasesindata", label = "cases", choices = c(""), selected = ""))),

         helpText(div("Optional. Select columns covariate 1, covariate 2, covariate 3, covariate 4."),
                  div("Leave the boxes with - if the data do not contain covariates.")),
         fluidRow(column(6, selectInput("columncov1indata", label = "covariate 1", choices = c(""), selected = "")),
                  column(6, selectInput("columncov2indata", label = "covariate 2", choices = c(""), selected = ""))),
         fluidRow(column(6, selectInput("columncov3indata", label = "covariate 3", choices = c(""), selected = "")),
                  column(6, selectInput("columncov4indata", label = "covariate 4", choices = c(""), selected = ""))),

         helpText(div("Note: Area id is a unique identifier of the area. Area id in the data should be the same as area id in the map.
                       Dates can be written in year (yyyy), month (yyyy-mm) or day (yyyy-mm-dd) format. Dates should be consecutive.
                       Data should contain the population and cases for all combinations of area id, date and covariates."))),


  column(3, style = "height:750px;  margin:10px; border-style: solid;border-color:gray",

         h3("3. Select analysis"),

         helpText(div("Select the temporal unit in the data. It can be year, month or day depending on the format of the dates in the data file." )),
         radioButtons("temporalUnitButton", "Temporal unit", inline=TRUE, c("Year (yyyy)" = "Year", "Month  (yyyy-mm)" = "Month", "Day (yyyy-mm-dd)" = "Day")),
         hr(),
         helpText(div("Select minimum and maximum dates of the analysis.  Only data with date within the date range will be used in the analysis ")),
         #Do not set min and max. Any date can be set.
         dateRangeInput("daterange", "Date range", start="1981-01-01", end="1984-01-01", format = "yyyy-mm-dd", separator = " to "),
         hr(),
         radioButtons("SorSTButton", "Type of analysis", inline=TRUE, c("Spatial" = "S", "Spatio-temporal" = "ST"), selected="ST"),
         hr(),

         #DELETE not input$useSampleData 2
         #fluidRow(column(4),column(2,
         #actionButton("startAnalysisButton",strong( "Start analysis"), style='background-color: #F1A8A8'))),

         #DELETE input$useSampleData 2
         fluidRow(column(4, offset=3, actionButton("startAnalysisButton",strong( "Start analysis"), style='background-color: #F1A8A8')),
                   column(4, checkboxInput("useSampleData", "Use sample data", TRUE))),

         hr(),
         fluidRow(verbatimTextOutput("messageCheckData")))),


fluidRow(style = "margin:10px",
         h3("Contents map"),
         plotOutput("uploadmapmap"),
         verbatimTextOutput("uploadmapsummary"),
         div(dataTableOutput('uploadmaptable'),  style = "font-size:80%")),

fluidRow(style = "margin:10px",
         h3("Contents data"),
         verbatimTextOutput("uploaddatasummary"),
         div(dataTableOutput('uploaddatatable'),  style = "font-size:80%"))
),



###########################################################################
###########################################################################
# STAGEANALYSIS
###########################################################################
###########################################################################

conditionalPanel(condition = "input.selectstage == 'stageanalysis'",
                 div(style="display: inline-block;vertical-align:middle;", h3("Analysis")),
                 div(style="display: inline-block;vertical-align:middle;", h3(" ")),

#######################
# INI type of analysis
#######################

fluidRow(column(2, h5("Date range")),
         column(2, h5("Type of analysis")),
         column(2, h5("Temporal unit")),
         column(2)),

fluidRow(column(2, verbatimTextOutput("daterangeValue")),
         column(2, verbatimTextOutput("SorSTButtonValue")),
         column(2, verbatimTextOutput("temporalUnitButtonValue")),
         column(1, actionButton("editInputsButton", strong( "Edit Inputs"), style='background-color: #F1A8A8')),
         column(2, actionButton("makemapsOESIRButton", strong( "Maps Pop O E SIR"), style='background-color: #D0F5A9')),
         column(1, actionButton("estimateriskButton", strong( "Estimate risk"), style='background-color: #D0F5A9')),
         column(1, actionButton("detectclustersButton", strong( "Detect clusters"), style='background-color: #D0F5A9'))),

fluidRow(column(6, offset=6, verbatimTextOutput("errorinlaorsatscannotinstalled"))),



hr(),

#######################
# END type of analysis
#######################

sidebarLayout(


#######################
# INI sidebarPanel
#######################

sidebarPanel(

helpText("Choose a variable to display. Tab 'Interactive' will be updated."),
selectInput("vblePintar", label = "Variable", choices = c("Population","Observed cases", "Expected cases","SIR"), selected = "Observed cases"),

helpText("Choose a time period to display. Tabs 'Interactive', 'Maps' and 'Clusters' will be updated."),

conditionalPanel("input.temporalUnitButton=='Day'",
sliderInput("timeperiodSliderday", label="Day", min = as.Date(1), max =as.Date(2), value =as.Date(1), step=1, animate=animationOptions(interval = 1000))),

conditionalPanel("input.temporalUnitButton=='Month'",
sliderInput("timeperiodSlidermonth", label="Month", min = 1, max =2, value =1, step=1, animate=animationOptions(interval = 1000))),

conditionalPanel("input.temporalUnitButton=='Year'",
sliderInput("timeperiodSlideryear", label="Year", min = 1, max =2, value =1, step=1, animate=animationOptions(interval = 1000))),

hr(),

helpText("Choose a region and a range of values to display. Tab 'Interactive' will be updated."),
selectInput("superarea", label = "Region", choices = c("All"), selected = "All"),
sliderInput("risk", label = "Range of values", min = 0, max = 1, value =c(0,1), step=0.01)
),

#######################
# END sidebarPanel
#######################


#######################
# INI mainPanel
#######################

conditionalPanel(condition = "input.selectstage == 'stageanalysis'",
mainPanel(
tabsetPanel(

tabPanel("Interactive",
         HTML("<br>"),
         fluidRow(column(12, verbatimTextOutput("staticdate1"))),
         fluidRow(column(8, leafletOutput("leafletMap"),
                         #Information for each highlighted area
                         absolutePanel(right = 17, top = 1, class = "panel panel-default", uiOutput("areaInfoPanel"))),
                  column(4, dygraphOutput("dygraphTemporalTrend"), textOutput("legendDivID"))),
         HTML("<br>"),
         fluidRow(column(5,downloadButton('downloadtabla', 'Download table'))),
         HTML("<br>"),
         fluidRow(column(5,div( dataTableOutput('tabla'),  style = "font-size:80%")))),


tabPanel("Maps",
         HTML("<br>"),
         fluidRow(column(12, verbatimTextOutput("staticdate2"))),
         fluidRow(column(12, verbatimTextOutput("staticsummary"))),

         conditionalPanel(condition = "input.selectmakemapsOESIR == 'done'",
                          fluidRow(column(3,h4("Population")), column(3,h4("Observed")), column(3,h4("Expected")), column(3,h4("SIR"))),
                          fluidRow(column(3,plotOutput("plot1")), column(3,plotOutput("plot2")), column(3,plotOutput("plot3")), column(3,plotOutput("plot4"))),
                          fluidRow(column(3,plotOutput("plot1t")), column(3,plotOutput("plot2t")), column(3,plotOutput("plot3t")), column(3,plotOutput("plot4t")))),

         conditionalPanel(condition = "input.selectestimaterisk == 'done'",
                          fluidRow(column(3,h4("Risk")), column(3,h4("2.5 percentile")), column(3,h4("97.5 percentile")), column(3)),
                          fluidRow(column(3,plotOutput("plot5")), column(3,plotOutput("plot6")), column(3,plotOutput("plot7")), column(3)),
                          fluidRow(column(3,plotOutput("plot5t")), column(3,plotOutput("plot6t")), column(3,plotOutput("plot7t")), column(3)))),

tabPanel("Clusters",
         conditionalPanel(condition = "input.selectdetectclusters == 'done'",
         HTML("<br>"),
         fluidRow(column(12, verbatimTextOutput("staticdate3"))),
         fluidRow(column(3, plotOutput("plotclusters")), column(3,plotOutput("plotclusterst"))),
         fluidRow(column(12, dataTableOutput("tablaclusters"))))),

tabPanel("Report",
         HTML("<br>"),
         fluidRow(column(2,downloadButton("report", "Download report"))),

         HTML("<br>"),
         helpText("Choose the variables to include in the report. Variables that have not been calculated will not be included."),
         checkboxGroupInput("rmaps", "Maps", inline=TRUE,
                            c("Population" = "Population", "Observed" = "Observed", "Expected" = "Expected", "SIR" = "SIR",
                              "Risk" = "Risk", "2.5 percentile" = "LowerLimitCI", "97.5 percentile" = "UpperLimitCI", "Clusters" = "Clusters"),
                            selected = c("Population", "Observed", "Expected", "SIR", "Risk", "LowerLimitCI", "UpperLimitCI", "Clusters")),

         checkboxGroupInput("rtables", "Tables summary", inline=TRUE,
                            c("Population" = "Population", "Observed" = "Observed", "Expected" = "Expected", "SIR" = "SIR",
                              "Risk" = "Risk", "2.5 percentile" = "LowerLimitCI", "97.5 percentile" = "UpperLimitCI"),
                            selected = c("Population", "Observed", "Expected", "SIR", "Risk", "LowerLimitCI", "UpperLimitCI")),

         checkboxGroupInput("rtablesclusters", "Table clusters", inline=TRUE,
                            c("Clusters" = "Clusters"), selected="Clusters"),

          HTML("<br>"))
)))

#######################
# END mainPanel
#######################

))))
