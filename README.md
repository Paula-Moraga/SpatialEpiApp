# SpatialEpiApp


<img src="https://paula-moraga.github.io/img/blogimg/2018-01-04_SpatialEpiApp/animation.gif" width="100%">


The R package [SpatialEpiApp](https://CRAN.R-project.org/package=SpatialEpiApp) runs a [Shiny](https://shiny.rstudio.com/) web application that allows to visualize spatial and spatio-temporal disease data, estimate disease risk, and detect clusters. The application incorporates modules for

- Disease risk estimation using Bayesian hierarchical models with [INLA](http://www.r-inla.org),

- Detection of clusters using the scan statistics implemented in [SaTScan](http://www.satscan.org),

- Interactive visualizations such as maps supporting padding and zooming and tables that allow for filtering,

- Generation of reports containing the analyses performed.


`SpatialEpiApp` allows user interaction and creates interactive visualizations by using the R packages [Leaflet](https://rstudio.github.io/leaflet/) for rendering maps, [dygraphs](https://rstudio.github.io/dygraphs/) for plotting time series, and [DataTables](https://rstudio.github.io/DT/) for displaying data objects. It also enables the generation of reports containing the analyses performed by using [RMarkdown](http://rmarkdown.rstudio.com/).

`SpatialEpiApp` may be useful for many researchers working in health surveillance lacking the adequate statistical and programming skills to effectively use the statistical software required to conduct the statistical analyses.
With `SpatialEpiApp`, we simply need to upload the map and data and then click the buttons that create the input files required, execute the software and process the output to generate tables of values and plots with the results.

This [blog post](https://paula-moraga.github.io/blog/2018/01/04/2018-01-04_spatialepiapp/), and the package [vignette](https://cran.r-project.org/web/packages/SpatialEpiApp/vignettes/manual.pdf) can be checked for more details about its use, methods and examples. Data for running examples are in https://Paula-Moraga.github.io/software


**References**

Moraga, P. (2017), [SpatialEpiApp: A Shiny Web Application for the analysis of Spatial and Spatio-Temporal Disease Data](https://doi.org/10.1016/j.sste.2017.08.001). Spatial and Spatio-temporal Epidemiology, 23:47-57



