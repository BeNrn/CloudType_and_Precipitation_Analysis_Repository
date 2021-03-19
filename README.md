# Cloud Type and Precipitation Analysis
  *Master's Thesis Repository*
  The repository scripts are used to extract cloud type and precipitation data. In subsequent steps, those data is intersected and analysed to extract a regression of cloud type vs. precipitation, and later to perform a cluster analysis.

## 1 Data
  The date used is the cloud type from the CPP-product of the EUMETSAT CM-SAF and the RADOLAN data from the DWD.

## 2 Preprocessing
  The original data is converted into the .tif format, retransformed into the same coordinate reference system (CRS) and reprojected to the same spatial resolution.

## 3 Intersection

## 4 Mean value examination

## 5 Cluster analysis

## Data sources
- EUMETSAT CM-SAF CPP product
  - Schulz, J., Albert, P., Behr, H.-D., Caprion, D., Deneke, H., Dewitte, S., Dürr, B., Fuchs, P., Gratzki, A., Hechler, P., Hollmann, R., Johnston, S., Karlsson, K.-G., Manninen, T., Müller, R., Reuter, M., Riihelä, A., Roebeling, R., Selbach, N., Tetzlaff, A., Thomas, W., Werscheck, M., Wolters, E., & Zelenka, A. (2009). Operational climate monitoring from space: the EUMETSAT Satellite Application Facility on Climate Monitoring (CM-SAF). Atmospheric Chemistry and Physics, 9(5), 1687–1709.
- Spectral EUMETSAT data
  - MÜLLER, J. (2017). MSG Level 1.5 Image Data Format Description. EUMETSAT. 
- RADOLAN Radar data product
  - WINTERRATH, T., BRENDEL, C., Hafer, M., JUNGHÄNEL, T., KLAMETH, A., LENGFELD, K., WALAWENDER, E., WEIGL, E., & BECKER, A. (2018). Radar climatology (RADKLIM) version 2017.002; gridded precipitation data for Germany. Deutscher Wetterdienst (DWD).
- Weather situation classification
  - DEUTSCHER WETTERDIENST (DWD). (2020). Objektive Wetterlagenklassifikation. Deutscher Wetterdienst KU21-Nationale Klimaüberwachung. https://www.dwd.de/DE/leistungen/wetterlagenklassifikation/online_wlkdaten.txt?view=nasPublication&nn=16102. [access: 19.03.2021]
