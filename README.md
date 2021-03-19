# Cloud Type and Precipitation Analysis
  *Master's Thesis Repository*
  
  The repository presents scripts that are used to extract cloud type, precipitation and cloud properties data. In subsequent steps, those information are intersected and analysed to identify possible interdependencies between cloud type and precipitation. This is done by first separating the precipitation by predefined cloud types. Second, a new unsupervised cloud type classification is performed, again, to separate the precipitation values. Analyzing the precipitation values within the defined classes allows to draw conclusions about the cloud type - precipitation relationship.  

## 1 Data
  The date used in the study are:
- The **cloud type** (CT-product) from the CPP-product of the EUMETSAT CM-SAF
- The **spectral information** data of the EUMETSAT
- The RADOLAN **precipitation** data of the Deutscher Wetterdienst (DWD)
- The **weather situation classification** of the DWD 

## 2 Preprocessing
  The original data, except for the weather situation classification is converted into the .tif format, retransformed into the same coordinate reference system (CRS) and reprojected to the same spatial and temporal resolution.

## 3 Intersection
  A spatial and temporal intersection is performed resulting in a *cloud attribute dataset*. 

## 4 Interdependency predefined cloud type and precipitation
  To analyze the dependencies between the predefined cloud type product (CT-product) and precipitation values, the precipitation distribution within the discrete CT-product classes are analyzed. A Kruskal-Wallis test returns significant differences (alpha = 0.05) in the mean precipitation values within the individual cloud types. The post-hoc Dunn's test confirms the findings between all cloud types pairwise.

## 5 Cluster analysis
  To optimize the class delineation regarding the precipitation behavior, a cluster analysis is performed. It includes the spectral information and the precipitation distribution.
Delineable rain events can be identified as the waether situation is used. Additionally, the continuity in the cluster delineation is used to identify data for a cross-scene analysis.

## Sources
- HEISTERMANN, M., JACOBI, S., & PFAFF, T. (2013). Technical Note: An open source library for processing weather radar data (wradlib). In Hydrology and Earth System Sciences (No.2; Hydrology and Earth System Sciences, Vol. 17, Number 2, pp. 863–871). Copernicus GmbH. 
- HOYER, S., & HAMMAN, J. J. (2017). xarray: N-D labeled Arrays and Datasets in Python. Journal of Open Research Software, 5.
- KASSAMBARA, A., & MUNDT, F. (2020). Package factoextra. Extract and Visualize the Results of Multivariate Data Analyse. Version 1.0.7.
- OGLE, D. H., WHEELER, P., & DINNO, A. (2020). FSA: Fisheries Stock Analysis. https://github.com/droglenc/FSA
- RASPAUD, M., HOESE, D., LAHTINEN, P., FINKENSIEPER, S., DYBBROE, A., HOLL, G., PROUD, S., JORO, S., ZHANG, X., MERANER, A., ROBERTS, W., RASMUSSEN, L. Ø., MÉNDEZ, J. H. B., JOLEENF, ZHU, Y., DARUWALA, R., JASMIN, T., BENR, BARNIE, T., SIGURÐSSON, E., R.K.GARCIA, LEPPELT, T., COLINDUFF, EGEDE, U., LTMEYER, ITKIN, M., GOODSON, R., RADAR, S., DIVISION, N., JKOTRO, & PETERS. (2020). pytroll/satpy: Version 0.23.0 (2020/09/18). Zenodo. 


## Data sources
- EUMETSAT CM-SAF CPP product
  - Schulz, J., Albert, P., Behr, H.-D., Caprion, D., Deneke, H., Dewitte, S., Dürr, B., Fuchs, P., Gratzki, A., Hechler, P., Hollmann, R., Johnston, S., Karlsson, K.-G., Manninen, T., Müller, R., Reuter, M., Riihelä, A., Roebeling, R., Selbach, N., Tetzlaff, A., Thomas, W., Werscheck, M., Wolters, E., & Zelenka, A. (2009). Operational climate monitoring from space: the EUMETSAT Satellite Application Facility on Climate Monitoring (CM-SAF). Atmospheric Chemistry and Physics, 9(5), 1687–1709.
- Spectral EUMETSAT data
  - MÜLLER, J. (2017). MSG Level 1.5 Image Data Format Description. EUMETSAT. 
- RADOLAN Radar data product
  - WINTERRATH, T., BRENDEL, C., Hafer, M., JUNGHÄNEL, T., KLAMETH, A., LENGFELD, K., WALAWENDER, E., WEIGL, E., & BECKER, A. (2018). Radar climatology (RADKLIM) version 2017.002; gridded precipitation data for Germany. Deutscher Wetterdienst (DWD).
- Weather situation classification
  - DEUTSCHER WETTERDIENST (DWD). (2020). Objektive Wetterlagenklassifikation. Deutscher Wetterdienst KU21-Nationale Klimaüberwachung. https://www.dwd.de/DE/leistungen/wetterlagenklassifikation/online_wlkdaten.txt?view=nasPublication&nn=16102. [access: 19.03.2021]
