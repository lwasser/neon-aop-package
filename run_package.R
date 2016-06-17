library(devtools)
library(roxygen2)
setwd("~/Documents/GitHub/neon-aop-package/neonAOP")
document()
setwd("~/Documents/GitHub/neon-aop-package")
install("neonAOP")
library("neonAOP")

## install from github
install_github("lwasser/neon-aop-package/neonAOP")

library(rgdal)
GDALinfo("NEONdata/D17-California/SJER/2013/lidar/SJER_lidarAspect.tif")
GDALinfo("1_othersites/NEON_D03_JERC_DP2_20140521_165122_ARVI.tif")

