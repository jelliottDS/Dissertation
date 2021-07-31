import numpy as np
from osgeo import gdal, gdal_array
import glob
import subprocess
import os

os.chdir('C:/Users/jelli/Documents/GitHub/CSC8639- Dissertation/Terra Environmental Variables/Rescaled')
# open the dataset and retrieve raster data as an array
for file in os.listdir('.'):
    input_raster = file
    print(input_raster)
    dataset = gdal.Open(input_raster)
    
    band = dataset.GetRasterBand(1)
    array = band.ReadAsArray()
    nodata_val = band.GetNoDataValue()
    
    
    if nodata_val is not None:
        array = np.where(np.isnan(array), nodata_val, array)
        array = np.ma.masked_equal(array, nodata_val)

    array_ignored_nan = array[array >= array.min()]

    # create an array of zeros the same shape as the input array
    output = np.zeros_like(array).astype(np.uint8)

    # use the numpy percentile function to calculate percentile thresholds
    percentile_80 = np.percentile(array_ignored_nan, 80)
    percentile_60 = np.percentile(array_ignored_nan, 60)
    percentile_40 = np.percentile(array_ignored_nan, 40)
    percentile_20 = np.percentile(array_ignored_nan, 20)
    percentile_0 = np.percentile(array_ignored_nan, 0)


    print(percentile_0, percentile_20, percentile_40, percentile_60, percentile_80)

    output = np.where((array > percentile_0), 1, output)
    output = np.where((array > percentile_20), 2, output)
    output = np.where((array > percentile_40), 3, output)
    output = np.where((array > percentile_60), 4, output)
    output = np.where((array > percentile_80), 5, output)

    outname = os.path.splitext(input_raster)[0] + "_Classified.tif"
    gdal_array.SaveArray(output, outname, "gtiff", prototype=dataset)