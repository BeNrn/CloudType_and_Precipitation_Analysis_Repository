# Cloud Type and Precipitation Analysis
  *Master's Thesis Repository*
  
  The repository presents scripts that were used to extract cloud type, precipitation and cloud properties data. In subsequent steps, those information were intersected and analysed to identify possible interdependencies between cloud type and precipitation. This is done by first separating the precipitation by predefined cloud types. Second, a new unsupervised cloud type classification is performed, again, to separate the precipitation values. Analyzing the precipitation values within the defined classes allows to draw conclusions about the cloud type - precipitation relationship.  

## 1 Data
  The date used in the study is:
- The **cloud type** (CT-product) from the CPP-product of the EUMETSAT CM-SAF
- The **spectral information** data of the EUMETSAT
- The RADOLAN **precipitation** data of the Deutscher Wetterdienst (DWD)
- The **weather situation classification** of the DWD 

## 2 Preprocessing
  The original data, except for the weather situation classification is converted into the .tif format, retransformed into the same coordinate reference system (CRS) and reprojected to the same spatial and temporal resolution.

## 3 Intersection
  Subsequently, a spatial and temporal intersection is performed resulting in a *cloud attribute dataset*. 

## 4 Mean value examination
  To analyze the dependencies between a predefined cloud type product and precipitation values the precipitation distribution within the discrete ct-product classes are analyzed. A Kruskal-Wallis test returns significant differences (alpha = 0.05) in the mean precipitation values within the individual cloud types. The post-hoc Dunn's test confirms the findings between all cloud types pairwise.

## 5 Cluster analysis
  To optimize the class delineation regarding the precipitation behavior, a cluster analysis is performed. It includes the spectral information and the precipitation distribution.
Delineable rain events can be identified as the waether situation is used. Additionally, the continuity in the cluster delineation is used to determine the possibility of cross-scene analysis.   

## Data sources
- EUMETSAT CM-SAF CPP product
  - Schulz, J., Albert, P., Behr, H.-D., Caprion, D., Deneke, H., Dewitte, S., Dürr, B., Fuchs, P., Gratzki, A., Hechler, P., Hollmann, R., Johnston, S., Karlsson, K.-G., Manninen, T., Müller, R., Reuter, M., Riihelä, A., Roebeling, R., Selbach, N., Tetzlaff, A., Thomas, W., Werscheck, M., Wolters, E., & Zelenka, A. (2009). Operational climate monitoring from space: the EUMETSAT Satellite Application Facility on Climate Monitoring (CM-SAF). Atmospheric Chemistry and Physics, 9(5), 1687–1709.
- Spectral EUMETSAT data
  - MÜLLER, J. (2017). MSG Level 1.5 Image Data Format Description. EUMETSAT. 
- RADOLAN Radar data product
  - WINTERRATH, T., BRENDEL, C., Hafer, M., JUNGHÄNEL, T., KLAMETH, A., LENGFELD, K., WALAWENDER, E., WEIGL, E., & BECKER, A. (2018). Radar climatology (RADKLIM) version 2017.002; gridded precipitation data for Germany. Deutscher Wetterdienst (DWD).
- Weather situation classification
  - DEUTSCHER WETTERDIENST (DWD). (2020). Objektive Wetterlagenklassifikation. Deutscher Wetterdienst KU21-Nationale Klimaüberwachung. https://www.dwd.de/DE/leistungen/wetterlagenklassifikation/online_wlkdaten.txt?view=nasPublication&nn=16102. [access: 19.03.2021]
