#reproject, resample and mask the data
import os
import rasterio
from rasterio.plot import show
from rasterio.enums import Resampling
from rasterio.warp import calculate_default_transform, reproject
from osgeo import ogr, gdal, gdalconst
#https://automating-gis-processes.github.io/CSC18/lessons/L6/reading-raster.html
#https://rasterio.readthedocs.io/en/latest/intro.html
#------------------------------------------------------------------------------
# 1 SET FOLDER PATH
#------------------------------------------------------------------------------
workingDir = "C:/Users/tamta/Documents/Studium/02_Master/17_Masterarbeit/03_Data/"
#workingDir = "D:/MA_data/"

ctDir = "CloudType_Raster/"
rdDir = "Radolan_Raster/"
msgDir = "MSGSpectral_Raster/"

ct_out = workingDir + "CloudType_Preprocessing/"
rd_out = workingDir + "Radolan_Preprocessing/"
msg_out = workingDir + "MSGSpectral_Preprocessing/"

ctList = os.listdir(workingDir + ctDir)
rdList = os.listdir(workingDir + rdDir)
msgList = os.listdir(workingDir + msgDir)

#------------------------------------------------------------------------------
# 2 REPROJECT THE DATA INTO ONE CRS
#------------------------------------------------------------------------------
#https://spatialreference.org/ref/epsg/32632/
dst_crs = "EPSG:32632" #WGS84 UTM zone 32
fileEnding = ".tif"

#3.1 CT
#-------
# for files in ctList:
#     if files.endswith(fileEnding):

for files in ctList:
    if files.endswith(fileEnding):
        filename = os.path.join(workingDir+ctDir, files)
        with rasterio.open(filename) as src:    
            transform, width, height = calculate_default_transform(src.crs, dst_crs, src.width, src.height, *src.bounds)
            kwargs = src.meta.copy()
            kwargs.update({'crs': dst_crs, 'transform': transform, 'width': width, 'height': height})

            #**kwargs allows the definition of additional "name = value" arguments for the used object
            #"w" indicates that the file is written like w"C:/Datapath"
            with rasterio.open(ct_out+"CT_"+files[3:15]+"_CRStransform.tif", "w", **kwargs) as dst:
                for i in range(1, src.count + 1):    
                    reproject(source=rasterio.band(src, i),
                              destination=rasterio.band(dst, i),
                              src_transform=src.transform,
                              src_crs=src.crs,
                              dst_transform=transform,
                              dst_crs=dst_crs,
                              resampling=Resampling.nearest)

# 3.2 Rad
#--------
#temporal resampling, only every 15min are used to match the ct product resolution
rdEnding = ("00_geotiff.tif", "15_geotiff.tif", "30_geotiff.tif", "45_geotiff.tif")
for files in rdList:
    if files.endswith(rdEnding):
        filename = os.path.join(workingDir+rdDir, files)
        with rasterio.open(filename) as src:  
            transform, width, height = calculate_default_transform(src.crs, dst_crs, src.width, src.height, *src.bounds)
            kwargs = src.meta.copy()
            kwargs.update({'crs': dst_crs, 'transform': transform, 'width': width, 'height': height})

            #**kwargs allows the definition of additional "name = value" arguments for the used object
            #"w" indicates that the file is written like w"C:/Datapath"
            with rasterio.open(rd_out+"Radolan_"+files[8:20]+"_CRStransform.tif", "w", **kwargs) as dst:
                for i in range(1, src.count + 1):    
                    reproject(source=rasterio.band(src, i),
                              destination=rasterio.band(dst, i),
                              src_transform=src.transform,
                              src_crs=src.crs,
                              dst_transform=transform,
                              dst_crs=dst_crs,
                              resampling=Resampling.nearest)
                    
#3.3 MSG
#-------
for files in msgList:
    if files.endswith(fileEnding):
        filename = os.path.join(workingDir+msgDir, files)
        with rasterio.open(filename) as src:  
            transform, width, height = calculate_default_transform(src.crs, dst_crs, src.width, src.height, *src.bounds)
            kwargs = src.meta.copy()
            kwargs.update({'crs': dst_crs, 'transform': transform, 'width': width, 'height': height})
            with rasterio.open(msg_out + files[0:22]+"_CRStransform.tif", "w", **kwargs) as dst:
                for i in range(1, src.count + 1):    
                    reproject(source=rasterio.band(src, i),
                              destination=rasterio.band(dst, i),
                              src_transform=src.transform,
                              src_crs=src.crs,
                              dst_transform=transform,
                              dst_crs=dst_crs,
                              resampling=Resampling.nearest)

del height, width, i, kwargs, src, src_crs, src_geotrans, src_proj, src_transform, transform

#------------------------------------------------------------------------------
# 3 RESAMPLE TO SAME PIXEL RESOLUTION AS CT PRODUCT
#------------------------------------------------------------------------------

#3.1 Rad
#-------
#use the previousely generated output (rd - CRStransform) therefor, update the list
rdList = os.listdir(rd_out)
#pick one ct File as resample example in which the RD should be turned
ct_file = "C:/Users/tamta/Documents/Studium/02_Master/17_Masterarbeit/03_Data/CloudType_Preprocessing/CT_201707010000_CRS_bbox.tif"

#resample radolan data from 1x1km to 3x3km like CPP-ct product
fileEnding = "CRStransform.tif"
for files in rdList:
    if files.endswith(fileEnding):
        filename = os.path.join(rd_out, files)
        
        # The file which should be resampled
        src = gdal.Open(filename, gdalconst.GA_ReadOnly)
        src_proj = src.GetProjection()
        src_geotrans = src.GetGeoTransform()

        # We want a section of source that matches this:
        # the source file should be resampled into this format
        match = gdal.Open(ct_file, gdalconst.GA_ReadOnly)
        match_proj = match.GetProjection()
        match_geotrans = match.GetGeoTransform()
        wide = match.RasterXSize
        high = match.RasterYSize

        # Output / destination
        dst_filename = rd_out+"/Radolan_"+files[8:20]+"_CRS_pixelResample.tif""
        #dst_filename = workingDir + "CRS_pixel/Radolan_"+files[8:20]+"_CRS_pixelResample.tif"
        
        dst = gdal.GetDriverByName('GTiff').Create(dst_filename, wide, high, 1, gdalconst.GDT_Float32)
        dst.SetGeoTransform(match_geotrans)
        dst.SetProjection(match_proj)
        
        #set no data to -9999
        #as SetNoData is a raster band method, the raster band must be called first
        dst_band = dst.GetRasterBand(1)
        dst_band.SetNoDataValue(-9999)
        dst_band = None

        # Do the work
        #using the bilinear matching
        gdal.ReprojectImage(src, dst, src_proj, match_proj, gdalconst.GRA_Bilinear)

        del dst #Flush
        
#3.2 MSG
#-------
msgList = os.listdir(msg_out)
#pick one ct File as resample example in which the RD should be turned
ct_file = "C:/Users/tamta/Documents/Studium/02_Master/17_Masterarbeit/03_Data/CloudType_Preprocessing/CT_201707010000_CRS_bbox.tif"

#resample radolan data from 1x1km to 3x3km like CPP-ct product
fileEnding = "CRStransform.tif"
for files in msgList:
    if files.endswith(fileEnding):
        filename = os.path.join(msg_out, files)

        # The file which should be resampled
        src = gdal.Open(filename, gdalconst.GA_ReadOnly)
        src_proj = src.GetProjection()
        src_geotrans = src.GetGeoTransform()

        # We want a section of source that matches this:
        # the source file should be resampled into this format
        match = gdal.Open(ct_file, gdalconst.GA_ReadOnly)
        match_proj = match.GetProjection()
        match_geotrans = match.GetGeoTransform()
        wide = match.RasterXSize
        high = match.RasterYSize

        # Output / destination
        dst_filename = msg_out + files[0:22] + "_CRS_pixelResample.tif"
        dst = gdal.GetDriverByName('GTiff').Create(dst_filename, wide, high, 1, gdalconst.GDT_Float32)
        dst.SetGeoTransform(match_geotrans)
        dst.SetProjection(match_proj)
        
        #set no data to -9999
        #as SetNoData is a raster band method, the raster band must be called first
        dst_band = dst.GetRasterBand(1)
        dst_band.SetNoDataValue(-9999)
        dst_band = None

        # Do the work
        #using the bilinear matching
        gdal.ReprojectImage(src, dst, src_proj, match_proj, gdalconst.GRA_Bilinear)

        del dst #Flush

del high, match, match_geotrans, match_proj, wide
#------------------------------------------------------------------------------
# 5 MASKING THE GERMAN COUNTRY BOUNDING BOX
#------------------------------------------------------------------------------
shpPath = workingDir + "Verwaltungsgebiet_Deutschland/VG250_STA.shp"
#shpPath = "C:/Users/tamta/Documents/Studium/02_Master/17_Masterarbeit/03_Data/Verwaltungsgebiet_Deutschland/VG250_STA.shp"

#get bounding box coordinates
driver = ogr.GetDriverByName("ESRI Shapefile")
vector = driver.Open(shpPath)
layer = vector.GetLayer()
feature = layer.GetFeature(0)
geom = feature.GetGeometryRef()
(minX, maxX, minY, maxY) = geom.GetEnvelope()

del driver, vector, layer, feature, geom

#mask the raster data with the bbox coordinates

#5.1 CT
#------
ctList = os.listdir(ct_out)
fileEnding = "CRStransform.tif"
    
for files in ctList:
    if files.endswith(fileEnding):
        filename = os.path.join(ct_out, files)
        dataset = gdal.Open(filename)
        outfile = ct_out + "CT_"+files[3:15]+"_CRS_bbox.tif"
        #extract regional subset to new geotiff
        gdal.Translate(outfile, dataset, projWin = [minX, maxY, maxX, minY])
        #close dataset
        dataset = None

#5.2 Rad
#-------
rdList = os.listdir(rd_out)
#rdList = os.listdir(workingDir+"CRS_pixel/")
fileEnding = "CRS_pixelResample.tif"
for files in rdList:
    if files.endswith(fileEnding):
        filename = os.path.join(rd_out, files)
        #filename = os.path.join(workingDir+"CRS_pixel/", files)
        dataset = gdal.Open(filename)
        outfile = rd_out+"/Radolan_"+files[8:20]+"_CRS_Resample_bbox.tif"
        #outfile = workingDir + "Bbox/Radolan_"+files[8:20]+"_CRS_Resample_bbox.tif"
        #extract regional subset to new geotiff
        gdal.Translate(outfile, dataset, projWin = [minX, maxY, maxX, minY])
        #close dataset
        dataset = None
        
#5.3 MSG
#-------
#update output dir
msgList = os.listdir(msg_out)

fileEnding = "CRS_pixelResample.tif"
for files in msgList:
    if files.endswith(fileEnding):
        filename = os.path.join(msg_out, files)
        dataset = gdal.Open(filename)
        outfile = msg_out + files[0:22] + "_CRS_Resample_bbox.tif"
        #extract regional subset to new geotiff
        gdal.Translate(outfile, dataset, projWin = [minX, maxY, maxX, minY])
        #close dataset
        dataset = None

del outfile
