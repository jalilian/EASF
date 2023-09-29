# Mozambique
The EASF project has been operated across three provinces in Mozambique, which are as follows:

|Province|Area (sq. km)|Number of districts|Population|
| ------ | ------- | ----- | ---- | 
|Maputo  | 26,011  |  7 | 2,507,098 |
|Niassa  | 129,056 | 15 | 1,865,976 |
|Zambezia| 105,008 | 16 | 5,110,787 |

## Sampling program
The sampling plan so far has been executed according to the following table:

|Province|District|Collection Method|Number of sampling locations | Sampling months (number of collections)|
| ---- | ---------- | ----- |  ----- | ---- | 
|Maputo  | Moamba | AL-CDC | 18 | 2022-09-13 (6), 2022-09-14 (15), 2022-09-15 (18),  2022-09-16 (12) <br> 2022-10-18 (12), 2022-10-19 (18), 2022-10-20 (18), 2022-10-21 (6) <br> 2022-11-09 (17), 2022-11-10 (17), 2022-11-11 (18) <br> 2023-01-11 (18), 2023-01-12 (18), 2023-01-13 (18) <br> 2023-02-02 (16), 2023-02-03 (16), 2023-02-04 (16) |
|Maputo  | Moamba | Flit | 12 | 2022: Sep (3), Oct (3), Nov (3) <br> 2023: Jan (3), Feb (3) |
|Niassa| | | | |
|Zambezia| Gurue | HLC | 12 | 2022: Sep (4), Oct (3), Nov (2) <br> 2023: Jan (2), Feb (2), Mar (2), Apr (2), May (2), Jun (2), Jul (2), Aug (2) |
|Zambezia| Morrumbala | AL-CDC | 12 | 2022: Oct (3), Nov (3) <br> 2023: Jan (3), Feb (3), Mar (3), Apr (3), May (3), Jun (3), Jul (3), Aug (3) |
|Zambezia| Morrumbala | Prokopack | 12 | 2022: Oct (1), Nov (1) <br> 2023: Jan (2), Feb (2), Mar (1), Apr (2), May (2), Jun (3), Jul (3), Aug (1) |

## Environmental data

Environmental data are obtained from the following open access sources.

### Climate, soil moisture and vegetation data
The [Copernicus's Climate Data Store (CDS)](https://cds.climate.copernicus.eu), European Centre for Medium-Range Weather Forecasts (ECMWF). DOI: 10.24381/cds.68d2bb30

The extracted data for Mozambique can be directly imported to ```R``` with the following command:
```R
land_data <- readRDS(url("https://github.com/jalilian/EASF/raw/main/Mozambique/cds_land_data.rds"))
```

### Lancover data
The [S2 Prototype Land Cover 20m Map of Africa 2016](https://2016africalandcover20m.esrin.esa.int/) data are used for extracting land cover type of the sampling locations. This data is provided to the public by the ESA Climate Change Initiative and its Land Cover project as the source of the CCI-LC database.

The land cover type of sampling locations is as follows:
|Province|District| Collection Method|Land cover type of sampling locations |
| ---- | ---------- | ----- | ----- |
Maputo  | Moamba | AL-CDC | Shrubs cover areas 2 <br> Grassland 3 <br> Cropland 1 <br> Vegetation aquatic or regularly flooded 2 <br> Built-up areas 10 |
|Maputo  | Moamba | Flit | Shrubs cover areas 1 <br> Grassland 2 <br>  Cropland 2 <br> Vegetation aquatic or regularly flooded 4 <br> Built-up areas 3|
|Niassa| | | |
|Zambezia| Gurue | HLC |  Grassland 1 <br> Cropland 10 <br> Built-up areas 1|
|Zambezia| Morrumbala | AL-CDC | Cropland 12 |
|Zambezia| Morrumbala | Prokopack | Cropland 12 |

### Elevation
The [Shuttle Radar Topography Mission (SRTM) 30 metres](https://www.usgs.gov/centers/eros/science/usgs-eros-archive-digital-elevation-shuttle-radar-topography-mission-srtm-1?qt-science_center_objects=0#qt-science_center_objects) is used for extracting elevation data.
