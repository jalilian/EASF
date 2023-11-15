# Mozambique
The EASF project has been operated across three provinces in Mozambique, which are as follows:

|Province|Area (sq. km)|Number of districts|Population| Selected districts |
| ------ | ------- | ----- | ---- | --- | 
|Maputo  | 26,011  |  7 | 2,507,098 | Moamba |
|Niassa  | 129,056 | 15 | 1,865,976 | Cuamba and Mandimba |
|Zambezia| 105,008 | 16 | 5,110,787 | Gurue and Morrumbala |

The following map shows sampling provinces (grey), districts (blue) and locations (red). 

![Mozambique](/Mozambique/images/Mozambique.png)

## Sampling program

The sampling program is summarized [here](sampling_program.md).

## Environmental data

Environmental data are obtained from the following open access sources.

### Climate, soil moisture and vegetation data
The [Copernicus's Climate Data Store (CDS)](https://cds.climate.copernicus.eu), European Centre for Medium-Range Weather Forecasts (ECMWF) is used to extract surface skin temperature (skt), total precipitation (tp), volumetric soil water layer 1 (swvl1), leaf area index, high vegetation (lai_hv), and leaf area index, low vegetation (lai_lv). The CDS is publicly accessible with the DOI: [10.24381/cds.e2161bac](https://doi.org/10.24381/cds.e2161bac).

The extracted data for Mozambique from May 2022 to September can be directly imported to ```R``` with the following command:
```R
land_data <- readRDS(url("https://github.com/jalilian/EASF/raw/main/Mozambique/cds_land_data.rds"))
```

### Lancover data
The [S2 Prototype Land Cover 20m Map of Africa 2016](https://2016africalandcover20m.esrin.esa.int/) data are used for extracting land cover type of the sampling locations. This data is provided to the public by the ESA Climate Change Initiative and its Land Cover project as the source of the CCI-LC database.

The land cover type of sampling locations is as follows:
|Province|District| Collection Method|Land cover type of sampling locations | Mean elevation |
| ---- | ---------- | ----- | ----- | ----- |
Maputo  | Moamba | AL-CDC | Shrubs cover areas 2 <br> Grassland 3 <br> Cropland 1 <br> Vegetation aquatic or regularly flooded 2 <br> Built-up areas 10 | 122 |
|Maputo  | Moamba | Flit | Shrubs cover areas 1 <br> Grassland 2 <br>  Cropland 2 <br> Vegetation aquatic or regularly flooded 4 <br> Built-up areas 3 | 119 |
|Niassa  | Cuamba   | HLC | Shrubs cover areas 2 <br> Cropland 1 <br> Built-up areas 6 | 756 |
|Niassa  | Mandimba | HLC | Shrubs cover areas 1 <br> Grassland 3 <br> Cropland 7 <br> Built-up areas 1 | 754 |
|Zambezia| Gurue | HLC |  Grassland 1 <br> Cropland 10 <br> Built-up areas 1| 728 |
|Zambezia| Morrumbala | AL-CDC | Cropland 12 | 421 |
|Zambezia| Morrumbala | Prokopack | Cropland 12 | 422 |

### Elevation
Elevation data are extracted from the digital elevation model (DEM) provided by [Shuttle Radar Topography Mission (SRTM)](https://www.usgs.gov/centers/eros/science/usgs-eros-archive-digital-elevation-shuttle-radar-topography-mission-srtm-1?qt-science_center_objects=0#qt-science_center_objects) with 1 arc second (30 meter) resolution.
