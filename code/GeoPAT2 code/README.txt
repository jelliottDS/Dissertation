Step by step guide to complete analysis in GeoPat2

1. Download and install GeoPAT2 from https://github.com/Nowosad/geopat2
2. Open GeoPAT2 command line module
2. Change directory to folder containing climate data rasters (climate data raster files not included in submission due to larg size)
3. Copy and paste commands from file "processing_script.txt" in "GeoPAT2 code" folder in order. If similarity including Lima, Kinshasa and Jakarta is required
	change line ""gpat_pointsts -i allmonths_gridts -o querey_signatures.txt --xy_file="Existing_coordiantes.txt"" to "gpat_pointsts -i allmonths_gridts -o querey_signatures.txt --xy_file="Including_new_coordinates.txt"" 
4. Tiff files of global similarity to ETSs will now have been created, these can be opened and visualised in QGIS
