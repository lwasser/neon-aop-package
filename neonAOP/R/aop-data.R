#######################
## About This Code ####
## The code below contains a set of functions that allow you to easily and quickly
## open one or  more bands from a NEON HSI HDF5 file.
## You can load this file into your enviornment using source("path-to-file-here")
## and then quickly call the various functions
## Author: Leah A. Wasser
## Date Created: 1 May 2016
## Last Modified: 9 May 2016


## Use-- OPEN ONE BAND
## open_band(fileName, bandNum, epsg, dims)
## fileName (path in char format): path to the H5 file of interest
##  bandNum (numeric): the band number you wish to open 
## epsg (numeric); the epsg code for the coordinate reference system that the data are in
## dims (numeric vector): the x y and z dims of the data you are opening

## Use - create_stack 
## create_Stack(bands, epsg)
## bands - a numeric vector of band  numbers that you want to open
## epsg - the CRS of the data
#########################


#' Get Data Dimensions
#'
#' This function grabs the x,y and z dimemsions of an H5 dataset called "Reflectance"
#' It would be more robust IF you could pass it the dataset name / path too
#' @param fileName a path to the H5 file that you'd like to open
#' @keywords hdf5, dimensions
#' @export
#' @examples
#' get_data_dims("filename.h5")

get_data_dims <- function(fileName){
  # make sure everything is closed
  H5close()
  # open the file for viewing
  fid <- H5Fopen(fileName)
  # open the reflectance dataset
  did <- H5Dopen(fid, "Reflectance")
  # grab the dimensions of the object
  sid <- H5Dget_space(did)
  dims <- H5Sget_simple_extent_dims(sid)$size
  
  # close everything
  H5Sclose(sid)
  H5Dclose(did)
  H5Fclose(fid)
  return(dims)
}


#' Get subset index x,y min/max dimensions
#'
#' This function takes an input h5 file extent (the file that you wish to subset
#' and it creates a subset spatial extent object for the new
#' subset file (x/y min and max in map units).
#' @param h5Extent the spatial extent (of class extent) of the H5 file to be subsetted
#' @param dims - the x/y min and max INDEX values used to slice the H5 file
#' @param res - a vector of 2 objects representing the X and Y resolution (pixel size)
#' @keywords hdf5, dimensions
#' @export
#' @examples
#' create_extent_subset(h5Extent, dims, res=c(xres, yres))


## FUNCTION - create spatial extent object

create_extent_subset <- function(h5.extent, dims, res=c(1,1)){
  # CALCULATE the XY left corner coordinate (xmin,ymax)
  xMin <- h5.extent@xmin + (dims[1] * res[1])
  yMax <- h5.extent@ymax - (dims[3] * res[2])
  # calculate the xMAX value and the YMIN value
  xMax <-h5.extent@xmin + (dims[2] * res[1])
  yMin <-h5.extent@ymax - (dims[4] * res[2])
  
  # create extent object (left, right, top, bottom)
  rasExt <- extent(xMin, xMax, yMin, yMax)
  # return object of class extent
  return(rasExt)
  
}



#' Create h5 file extent ####
#'
#' This function uses a map tie point for an h5 file and data resolution to 
#' create and return an object of class extent. 
#' @param filename the path to the h5 file
#' @param res a vector of 2 objects - x resolution, y resolution
#' @keywords hdf5, extent
#' @export
#' @examples
#' create_extent(fileName, res=c(xres, yres))

create_extent <- function(fileName){
  # Grab upper LEFT corner coordinate from map info dataset 
  mapInfo <- h5read(fileName, "map info")
  
  # create object with each value in the map info dataset
  mapInfo<-unlist(strsplit(mapInfo, ","))
  # grab the XY left corner coordinate (xmin,ymax)
  xMin <- as.numeric(mapInfo[4])
  yMax <- as.numeric(mapInfo[5])
  # get the x and y resolution
  res <- as.numeric(c(mapInfo[2], mapInfo[3]))
  # get dims to use to cal xMax, YMin
  dims <- get_data_dims(f)
  # calculate the xMAX value and the YMIN value
  xMax <- xMin + (dims[1]*res[1])
  yMin <- yMax - (dims[2]*res[2])
  
  # create extent object (left, right, top, bottom)
  rasExt <- extent(xMin, xMax, yMin, yMax)
  # return object of class extent
  return(rasExt)
}

## FUNCTION - Clean Reflectance Data ####

#' Clean reflectance data
#'
#' This function reads in data from the "Reflecatnce" dataset, applies the data
#' ignore value, scales the data and returns a properly "projected" raster object.
#' @param filename the path to the h5 file.
#' @param reflMatrix , the matrix read in to be converted to a raster.
#' @param epsg - the epsg code for the CRS used to spatially locate the raster.
#' @keywords hdf5, extent
#' @export
#' @examples
#' clean_refl_data(fileName, reflMatrix, epsg)


clean_refl_data <- function(fileName, reflMatrix, epsg){
  # r  get attributes for the Reflectance dataset
  reflInfo <- h5readAttributes(fileName, "Reflectance")
  # grab noData value
  noData <- as.numeric(reflInfo$`data ignore value`)
  # set all values = 15,000 to NA
  reflMatrix[reflMatrix == noData] <- NA
  
  # apply the scale factor
  reflMatrix <- reflMatrix/(as.numeric(reflInfo$`Scale Factor`))
  
  # now we can create a raster and assign its spatial extent
  reflRast <- raster(reflMatrix,
                     crs=CRS(paste0("+init=epsg:", epsg)))
  
  # return a scaled and "cleaned" raster object
  return(reflRast)
}

## FUNCTION - Read Band ####
#' read band
#'
#' This function reads in data from the "Reflecatnce" dataset, applies the data
#' ignore value, scales the data and returns a properly "projected" raster object.
#' @param filename the path to the h5 file.
#' @param index a list formated object  e.g. list(1:3, 1:6, bands)
#' @keywords hdf5, extent
#' @export
#' @examples
#' read_band(fileName, index)

read_band <- function(fileName, index){
  # Extract or "slice" data for band 34 from the HDF5 file
  aBand<- h5read(fileName, "Reflectance", index=index)
  # Convert from array to matrix so we can plot and convert to a raster
  aBand <- aBand[,,1]
  # transpose the data to account for columns being read in first
  # but R wants rows first.
  aBand<-t(aBand)
  return(aBand)
}

## FUNCTION Calculate Index Extent ####


## FUNCTION - Calculate Index Extent ####
#'
#' This function calculates an index based subset to slice out data from an H5 file
#' using an input spatial extent. 
#' @param clipExtent xxxx 
#' @param h5Extent XXX
#' @keywords hdf5, extent
#' @export
#' @examples
#' calculate_index_extent(clipExten, h5Extent)
#' 
calculate_index_extent <- function(clipExtent, h5Extent){
  if(clipExtent@xmin <= h5Extent@xmin){
    xmin.index <- 1 
  } else {
    xmin.index <- round((clipExtent@xmin- h5Extent@xmin)/xscale)}
  
  # calculate y ymin.index
  if(clipExtent@ymax > h5Extent@ymax){
    ymin.index <- 1
  } else {
    ymin.index <- round((h5Extent@ymax - clipExtent@ymax) / yscale)}
  
  # calculate x xmax.index
  
  # if xmax of the clipping extent is greater than the extent of the H5 file
  # assign x max to xmax of the H5 file
  if(clipExtent@xmax >= h5Extent@xmax){
    xmax.index <- round((h5Extent@xmax-h5Extent@xmin) / xscale)
  } else {
    xmax.index <- round((clipExtent@xmax-h5Extent@xmin) / xscale)}
  
  # calculate y ymax.index
  
  if(clipExtent@ymin <= h5Extent@ymin){
    ymax.index <- round((h5Extent@ymax - h5Extent@ymin) / yscale)
  } else {
    ymax.index <- round((h5Extent@ymax - clipExtent@ymin)/yscale) }
  new.index <- c(xmin.index, xmax.index, ymin.index, ymax.index)
  return(new.index)
}


## FUNCTION - Open Band (1 band)
## dims is a data frame which contains the start and end value for
# x, y, wavelength
# dims <- list(1:5,2:6,c(2,3,4))
# trying to set a default dims value of NA


## FUNCTION - Calculate Index Extent ####
#'
#' This function calculates an index based subset to slice out data from an H5 file
#' using an input spatial extent. 
#' @param fileName the path to the h5 file that you wish to open. 
#' @param bandNum the band number in the reflectance data that you wish to open
#' @param epsg the epsg code for the CRS that the data are in.
#' @param subsetData, a boolean object. default is FALSE. If set to true, then
#' ... subset a slice out from the h5 file. otherwise take the entire xy extent.
#' @param dims, an optional object used if subsetData = TRUE that specifies the 
#' index extent to slice from the h5 file
#' @keywords hdf5, extent
#' @export
#' @examples
#' open_band(fileName, bandNum, epsg, subsetData=FALSE, dims=NULL)
#' 

open_band <- function(fileName, bandNum,  epsg, subsetData=FALSE, dims=NULL){
  # make sure any open connections are closed
  H5close()
  # if the band is a subset of the file, subset=TRUE
  # else take the specified dims which may be a subset
  # note subtracting one because R indexes all values 1:3 whereas in a zero based system
  # that would yield one more value -- double check on this but it creates the proper
  # resolution
  if(subsetData){ 
    index <- list(dims[1]:(dims[2]-1), dims[3]:(dims[4]-1), bandNum)
    aBand <- read_band(fileName, index)
    # clean data
    aBand <- clean_refl_data(fileName, aBand, epsg)
    # finally apply extent to raster, using extent function 
    h5.extent <- create_extent(fileName)
    extent(aBand) <- create_extent_subset(h5.extent, dims)
  } else {
    dims <- get_data_dims(fileName)
    index <- list(1:dims[1], 1:dims[2], bandNum)
    aBand <- read_band(fileName, index)
    # clean data
    aBand <- clean_refl_data(fileName, aBand, epsg)
    extent(aBand) <- create_extent(fileName)
  }
  
  # return matrix object
  return(aBand)
}


## FUNCTION - Open Bands, Create Stack ####
#'
#' This function calculates an index based subset to slice out data from an H5 file
#' using an input spatial extent. It returns a rasterStack object of bands. 
#' @param fileName the path to the h5 file that you wish to open. 
#' @param bandNum the band number in the reflectance data that you wish to open
#' @param epsg the epsg code for the CRS that the data are in.
#' @param subsetData, a boolean object. default is FALSE. If set to true, then
#' ... subset a slice out from the h5 file. otherwise take the entire xy extent.
#' @param dims, an optional object used if subsetData = TRUE that specifies the 
#' index extent to slice from the h5 file
#' @keywords hdf5, extent
#' @export
#' @examples
#' open_band(fileName, bandNum, epsg, subsetData=FALSE, dims=NULL)
#' 

# 
create_stack <- function(file, bands, epsg, subset=FALSE, dims=NULL){
  
  # use lapply to run the band function across all three of the bands
  rgb_rast <- lapply(bands, open_band,
                     fileName=file,
                     epsg=epsg,
                     subset=subset,
                     dims=dims)
  
  # create a raster stack from the output
  rgb_rast <- stack(rgb_rast)
  # reassign band names
  names(rgb_rast) <- bands
  return(rgb_rast)
  
} 




## FUNCTION - Extract Average Reflectance
#'
#' This function calculates an index based subset to slice out data from an H5 file
#' using an input spatial extent. It returns a rasterStack object of bands. 
#' @param aRaster REQUIRED. a raster within an H5 file that you wish to get an mean, max, min value from
#' @param aMask REQUIRED. a raster of the same extent with NA values for areas that you don't 
#' want to calculate the mean, min or max values from. 
#' @param aFun REQUIRED. the function that you wish to use on the raster, mean, max, min etc
#' @keywords hdf5, extent
#' @export
#' @examples
#' extract_av_refl(aRaster, aMask, aFun=mean)
#' 
extract_av_refl <- function(aRaster, aMask=NULL, aFun=mean){
  # mask band
  if(!is.null(aMask)){
    aRaster <- mask(aRaster, aMask) }
  # geat mean
  a.band.mean <- cellStats(aRaster, 
                           aFun, 
                           na.rm=TRUE)
  return(a.band.mean)
} 

## FUNCTION - Extract Average Reflectance
#'
#' This function calculates an index based subset to slice out data from an H5 file
#' using an input spatial extent. It returns a rasterStack object of bands. 
#' @param fileName REQUIRED. The path to the h5 file that you want to open.
#' @param bandNum REQUIRED. The band number that you wish to open, default = 1 
#' @param epsg the epsg code for the CRS that the data are in.
#' @param subsetData, a boolean object. default is FALSE. If set to true, then
#' ... subset a slice out from the h5 file. otherwise take the entire xy extent.
#' @param dims, an optional object used if subsetData = TRUE that specifies the 
#' index extent to slice from the h5 file
#' @param mask a raster containg NA values for areas that should not be included in the 
#' output summary statistic. 
#' @param fun the summary statistic that you wish to run on the data  (e.g. mean, max, min)
#' default = mean
#' @keywords hdf5, extent
#' @export
#' @examples
#' get_spectra(fileName, bandNum)

get_spectra <- function(fileName, bandNum=1, 
                        epsg=32611, subset=TRUE,
                        dims=NULL, mask=NULL, fun=mean){
  # open a band
  a.raster <- open_band(fileName, bandNum, 
                        epsg, subset,
                        dims)
  # extract a particular spectral value (min, max, mean from the raster)
  refl <- extract_av_refl(a.raster, mask, aFun = fun)
  return(refl)
}

## FUNCTION - Extract Average Reflectance
#'
#' This function calculates an index based subset to slice out data from an H5 file
#' using an input spatial extent. It returns a rasterStack object of bands. 
#' @param spectra REQUIRED. a LIST of spectra returned from Lapply.
#' @param wavelengths REQUIRED. The vector of wavelength values of the same length as 
#' the spectra object
#' @keywords hdf5, extent
#' @export
#' @examples
#' clean_spectra(fileName, bandNum)

clean_spectra <- function(spectra, wavelengths){
  # reformat the output list
  spectra<- data.frame(unlist(spectra))
  spectra$wavelength <- wavelengths
  names(spectra)[1] <- "reflectance"
  return(spectra)
}




## FUNCTION - Return Crop Extent
#'
#' This function calculates an index based subset to slice out data from an H5 file
#' using an input spatial extent. It returns a rasterStack object of bands. 
#' @param extent1 REQUIRED. an extent object.
#' @param extent2 REQUIRED. a second extent object
#' @param proj4 REQUIRED. the proj4 string for both extents
#' @keywords raster, extent
#' @export
#' @examples
#' return_crop_extent(extent1, extent2)

### THIS ISN"T WORKING
#return_crop_extent <- function(extent1, extent2, proj4){
  # turn both extents into spatial polygons
#  ext1.poly <- as(extent1, "SpatialPolygons")
#  ext2.poly <- as(extent2, "SpatialPolygons")
  
  # ensure the two extents overlap
#  if(gIntersects(h5.ext.poly, clip.extent)){
#    cropExtent <- gIntersection(ext1.poly, ext2.poly)
    #return(cropExtent)
#  } ELSE {
#    message("Spatial extents don't overlap.")
#  }
  
#  crs(h5.ext.poly) <- CRS("+proj=utm +zone=11 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0")
  
  

#}

