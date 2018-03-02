###  Purpose

**SpatialEpiApp** is a **Shiny** web application that allows to visualize spatial and spatio-temporal disease data, estimate disease risk and detect clusters.

The application allows to fit Bayesian disease models to obtain risk estimates and their uncertainty by using **R-INLA**, and to detect clusters by using the scan statistics implemented in **SaTScan**.

The application allows user interaction and creates interactive visualizations such as maps supporting padding and zooming and tables that allow for filtering. It also enables the generation of reports containing the analyses performed.


###  Structure

The application consists of three pages: 

- 'Inputs' where the user can upload the
input files and select the type of analysis
- 'Analysis' where statistical analyses
are carried out, results can be visualized, and reports can be generated
- 'Help'
containing information about the application use and references.


**Inputs page**

The 'Inputs' page is the first page we see when we launch the application. In this page we can upload the required files and select the type of analysis to be performed. It is composed of four components: 
- Upload map
- Upload data
- Select analysis
- Contents

**Start analysis button**

When we click the 'Start analysis' button the application checks the files and options entered are correct and calculates the expected counts for each area and date. It also computes the ratio of the observed divided the expected. This ratio is called SIR irrespectively if data refers to mortality or incidence. Then the application redirects to the 'Analysis' page.

**Analysis page**


In the 'Analysis' page we can visualize the data, perform the statistical analyses, and generate
reports. On the top of the page there are four buttons:

- 'Edit Inputs' button is used when we wish to return to the 'Inputs' page to modify the analysis options or upload new data.

- 'Maps Pop O E SIR' button creates plots of the population, observed, expected and SIR variables.

- 'Estimate risk' button is used for estimating
the disease risk and their uncertainty

- 'Detect clusters' is used for the detection of disease clusters

The 'Analysis' page also contains four tabs called 'Interactive', 'Maps', 'Clusters' and 'Report' that include tables and plots with the results.

**Help page**

Both the 'Inputs' and the 'Analysis' pages include a 'Help' button that redirects to the
'Help' page. This page shows information about the use of the application, the statistiscal
methodology and the developing tools employed.










### Dependencies


**SpatialEpiApp** has been developed using **R** and **Shiny** and is dependent on the following software and **R** packages:


|  |   |
--- | ----
**Software**   | 
R  | Language and environment for statistical computing and graphics
SaTScan  | Software that analyzes spatial, temporal and space-time data using scan statistics
**R packages** |
dplyr  | A fast, consistent tool for working with data frame like objects, both in memory and out of memory
dygraphs  | Interface to 'Dygraphs' Interactive Time Series Charting Library
ggplot2   | Creates elegant data visualisations using the grammar of graphics
htmlwidgets | Provides a framework for easily creating R bindings to JavaScript libraries
knitr  | Tool for dynamic report generation in R
leaflet | Create Interactive Web Maps with the JavaScript 'Leaflet' Library
mapproj | Converts latitute/longitude into projected coordinates
maptools  | Set of tools for manipulating and reading geographic data, in particular ESRI shapefiles
RColorBrewer  | Provides color schemes for maps and other graphics
rgdal | Provides bindings to Frank Warmerdam's Geospatial Data Abstraction Library (GDAL)
rgeos | Interface to Geometry Engine - Open Source (GEOS) using the C API for topology operations on geometries
rmarkdown  | Convert R Markdown documents into a variety of formats including HTML, MS Word, PDF, and Beamer
R-INLA  | Performs full Bayesian analysis on generalised additive mixed models using Integrated Nested Laplace Approximations
shiny  | Web Application Framework for R
shinyjs| Perform common useful JavaScript operations in Shiny apps that will greatly improve the apps without having to know any JavaScript
sp | Classes and Methods for Spatial Data
SpatialEpi  | Contains methods for cluster detection, disease mapping and plotting methods
spdep   | Contains a collection of functions for spatial dependence: weighting schemes, statistics and models
xts   | Extensible time series class and methods




 

### References

- Moraga P (2017), "SpatialEpiApp: A Shiny web application for the analysis of spatial and spatio-temporal disease data." Spatial and Spatio-temporal Epidemiology, 23:47-57
DOI: https://doi.org/10.1016/j.sste.2017.08.001

- Allaire J, Cheng J, Xie Y, McPherson J, Chang W, Allen J, Wickham H, Atkins A, Hyndman
R (2016). rmarkdown: Dynamic Documents for R. R package version 1.2, URL https://CRAN.R-project.org/package=rmarkdown.

- Attali D (2016). shinyjs: Easily Improve the User Experience of Your Shiny Apps in Seconds. R package version 0.8, URL https://CRAN.R-project.org/package=shinyjs.

- Bernardinelli L, Clayton DG, Pascutto C, Montomoli C, Ghislandi M, Songini M (1995). "Bayesian analysis of space-time variation in disease risk." Statistics in Medicine, 14, 2433-2443.

- Besag J, York J, Mollie A (1991). "Bayesian image restoration with applications in spatial statistics (with discussion)." Annals of the Institute of Statistical Mathemathics, 43, 1-59.

- Bivand R, Lewin-Koh N (2016). maptools: Tools for Reading and Handling Spatial Objects. R package version 0.8-39, URL https://CRAN.R-project.org/package=maptools.

- Bivand R, Keitt T, Rowlingson B (2016). rgdal: Bindings for the Geospatial Data Abstraction Library. R
  package version 1.2-5. https://CRAN.R-project.org/package=rgdal

- Bivand R, Piras G (2015). "Comparing Implementations of Estimation Methods for Spatial Econometrics." Journal of Statistical Software, 63(18), 1-36.

- Bivand R, Rundel C (2016). rgeos: Interface to Geometry Engine - Open Source (GEOS). R package version 0.3-21, URL https://CRAN.R-project.org/package=rgeos.

- Chang W, Cheng J, Allaire J, Xie Y, McPherson J (2016). shiny: Web Application Framework for R. R package version 0.14.1, URL https://CRAN.R-project.org/package=shiny.

- Cheng J, Xie Y (2016). leaflet: Create Interactive Web Maps with the JavaScript 'Leaflet' Library. R package version 1.0.1, URL https://CRAN.R-project.org/package=leaflet.

- Elliott P, Wakefield J, Best N, David Briggs e (2000). Spatial Epidemiology: Methods and Applications. Oxford University Press.

- Kim AY, Wakefield J (2016). SpatialEpi: Methods and Data for Spatial Epidemiology. R package version 1.2.2, URL https://CRAN.R-project.org/package=SpatialEpi.

- Kulldorff M (1997). "A spatial scan statistic." Communications in Statistics - Theory and Methods, 26(1), 1481-1496.

- Kulldorff M (2006a). SaTScan(TM) v. 7.0. Software for the spatial and space-time scan statistics. URL http://www.satscan.org/.

- Kulldorff M (2006b). "Tests of spatial randomness adjusted for an inhomogeneity: A general framework." Journal of the American Statistical Association, 101(475), 1289-1305.

- Kulldorff M, Nagarwalla N (1995). "Spatial disease clusters: detection and inference." Statistics in Medicine, 14, 799-810.

- Lawson AB (2009). Bayesian Disease Mapping: Hierarchical Modeling In Spatial Epidemiology. Chapman and Hall/CRC.

- Lawson AB, Browne WJ, Vidal Rodeiro CL (2003). Disease Mapping with WinBUGS and MLWin. Wiley.

- Lawson AB, Kleinman K (2005). Spatial and Syndromic Surveillance for Public Health. Wiley.

- Lindgren F, Rue H (2015). "Bayesian Spatial Modelling with R-INLA." Journal of Statistical Software, 63(19), 1-25.

- McIlroy D, Brownrigg R, Minka TP, Bivand R (2015). mapproj: Map Projections. R package version 1.2-4, URL https://CRAN.R-project.org/package=mapproj.

- Neuwirth E (2014). RColorBrewer: ColorBrewer Palettes. R package version 1.1-2, URL https://CRAN.R-project.org/package=RColorBrewer.

- Pebesma E, Bivand R, Rowlingson B, Gómez-Rubio V, Hijmans R, Sumner M, MacQueen D, Lemon J, O'Brien J (2016).
sp: Classes and Methods for Spatial Data.
R package version 1.2-5, URL https://CRAN.R-project.org/package=sp

- R Core Team (2017). R: A Language and Environment for Statistical Computing. R Foundation for Statistical Computing, Vienna, Austria. URL https://www.R-project.org.

- Rue H, Martino S, Chopin N (2009). "Approximate Bayesian inference for latent Gaussian models by using integrated nested Laplace approximations."Journal of the Royal Statistical Society, Series B, 71(2), 319-392.

- Ryan JA, Ulrich JM (2014). xts: eXtensible Time Series. R package version 0.9-7, URL https://CRAN.R-project.org/package=xts.
  
- Vaidyanathan R, Xie Y, Allaire J, Cheng J, Russell K (2016). htmlwidgets: HTML Widgets for R. R package version 0.7, URL https://CRAN.R-project.org/package=htmlwidgets.

- Vanderkam D, Allaire J, Owen J, Gromer D, Shevtsov P, Thieurmel B (2016). dygraphs: Interface to 'Dygraphs' Interactive Time Series Charting Library. R package version 1.1.1.3, URL https://CRAN.R-project.org/package=dygraphs.

- Waller LA, Gotway CA (2004). Applied Spatial Statistics for Public Health Data. Wiley, New York.

- Wickham H (2009). ggplot2: Elegant Graphics for Data Analysis. Springer-Verlag New York. ISBN 978-0-387-98140-6. URL http://ggplot2.org.

- Wickham H, Francois R (2016). dplyr: A Grammar of Data Manipulation. R package version 0.5.0, URL https://CRAN.R-project.org/package=dplyr.

- Xie Y (2015). Dynamic Documents with R and knitr. 2nd edition. Chapman and Hall/CRC.
