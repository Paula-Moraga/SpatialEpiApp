### Risk estimates

Risk estimates are obtained using **R-INLA**, http://www.r-inla.org

|Settings: |
| --- |
|Spatial model: Besag el al. (1991) |
|Spatio-temporal model: Bernardinelli et al. (1995) |


<br>

- Besag J, York J, Mollie A (1991). "Bayesian image restoration with applications in spatial
statistics (with discussion)." Annals of the Institute of Statistical Mathemathics, 43, 1-59

- Bernardinelli L, Clayton DG, Pascutto C, Montomoli C, Ghislandi M, Songini M (1995).
"Bayesian analysis of space-time variation in disease risk." Statistics in Medicine, 14, 2433-2443

- Rue H, Martino S, Chopin N (2009). "Approximate Bayesian inference for latent Gaussian
models by using integrated nested Laplace approximations."Journal of the Royal Statistical
Society, Series B, 71(2), 319-392

- Lindgren F, Rue H (2015). "Bayesian Spatial Modelling with R-INLA." Journal of Statistical
Software, 63(19), 1-25

<br>

### Detection of clusters

Detection of clusters is done using the **SaTScan** software.
Users need to download SaTScan from http://www.satscan.org.
Then, they need to install it and place the SaTScanBatch64 executable in the SpatialEpiApp/SpatialEpiApp/ss folder which is located in the R library path.

|Settings: |
| --- |
|Maximum population: 50% |
|Monte Carlo simulations: 999 |

<br>

- Kulldorff M (1997). "A spatial scan statistic." Communications in Statistics - Theory and
Methods, 26(1), 1481-1496.

- Kulldorff M (2006a). SaTScan(TM) v. 7.0. Software for the spatial and space-time scan
statistics. URL http://www.satscan.org.
