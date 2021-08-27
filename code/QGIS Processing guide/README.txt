This file details the steps taken to process raw data in QGIS. This folder contains this guide for processing raw climate data rasters with accompanying files and python script.
This folder does not contain the raw climate raster data as the files are too large for submission(>10GB).

Process raw data rasters to the same resolution and extent
1. Import raw climate data rasters using Layers>add layers>Add Raster Layer
2. 2. Open "Processing Toolbox" from "Processing" Menu
3. Open "Warp(reproject)" from "Processing Toolbox"
4. Select "Run as Batch Process in bottom left
5. Select all climate rasterss for input layer
6. Select "Source CRS" and "Target CRS" as "EPSG:3857"
7. Select "Resampling method to use" as "Bilineal"
8. Select "Nodata value for output bands" as "-9999"
9. Select "Output file resolution in target georeferenced units" as "0.0833"
10. Select "Calculate by expression for "Reprojected" and input "<file path to folder for saving variables> ||  @input  || '_warped.tiff'" 
11. Select "Run"

Normalisation of Temperature, UV Radiation and Sea Salt Aerosol, Time of Wetness and Frequency of Wetness data
1. Import warped climate data rasters using Layers>add layers>Add Raster Layer
2. Open "Processing Toolbox" from "Processing" Menu
3. Open "Rescale raster" from "Processing Toolbox"
4. Set new minimum raster value to 0 and new maximum raster value to 1.
5. Process all climate data rasters for Temperature, Precipitation, UV Radiation and Sea Salt Aerosol through the "Rescale Raster" tool.

Normalisation of Precipitation data
1. Import warped Precipitation climate data rasters using Layers>add layers>Add Raster Layer
2. Select "Open Existin Script" from Python button in "Processing Toolbox" window and select "precipitation normarlisation.txt".
3. Select Precipitation data rasters and process through the script.

Once rasters have been pre-processed and normalised these processed data raster should be used as the input rasters for GeoPAT2 code. 

Extracting data for ETS locations
1. Select "Plugins"> "Manage and Install Plugins" from QGIS toolbar
2. Search and install "Point Sampling Tool"
3. Import coordinates of ETSs using Layers>add layers>Add Delimited Text Layer then select "ETS_locations.csv"
4. Click point sampling tool button, make sure "ETS_locations.csv" is selected as "Layer containing sampling points:"
5. Select all climate raster data sets in the section "Layers with fields/bands to get values from". Raster layers should be selected on a month by month basis e.g select all rasters relating to January, save the output, then repeat for all rasters relating to February etc
6. In "Output point vector layer:" choose a destination to save the file. 

The output of this is the same data that is included in the "data" folder for the ProjectTemplate R code.  
The output