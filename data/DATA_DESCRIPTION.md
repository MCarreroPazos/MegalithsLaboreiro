# Data Description

This document describes all raw input datasets in `data/raw/` and derived outputs
in `data/derived/`. Together with the methods section of the paper, this provides 
the information needed to understand, reuse, and properly cite the data.

---

## Vector Data (`data/raw/shp/`)

### `sites_lab.shp` / `sites_lab.gpkg`
Point layer of megalithic and tumular monuments used in the analyses.

| Field | Type | Description |
|-------|------|-------------|
| geometry | Point | Site location |
| X | Double | Easting coordinate (UTM ETRS89 zone 29N) |
| Y | Double | Northing coordinate (UTM ETRS89 zone 29N) |

- **CRS:** EPSG:25829 (UTM zone 29N, ETRS89)
- **n:** 178 monuments (after ground-truthing with LiDAR); 138 used in the primary spatial analysis
- **Source:** Field survey campaigns (Vilas-Estévez 2015) + LiDAR-based ground-truthing (Canedo et al. 2023, 2024)
- **Notes:** 91 points were excluded from the original database of 269 (46 duplicates; 45 with dubious remote identification)

### `sites_all.shp`
All registered monuments in the study region prior to ground-truthing (n = 269).
- **CRS:** EPSG:25829
- **Source:** Compiled from existing databases and field surveys

### `mask.shp`
Polygon defining the study area boundary used as the analysis window in spatial statistics.
- **CRS:** EPSG:25829

### `intervisibility_sites.shp`
Line layer representing intervisibility connections between monuments, computed with
the Visibility Analysis plugin in QGIS 3.28 (Čučković 2016).
- **CRS:** EPSG:25829
- **Used in:** Figure 5

### `sites.gpkg`
GeoPackage version of `sites_lab` in EPSG:25829, used as the primary vector format
for reproducibility.

---

## Raster Data (`data/raw/grid/`)
All rasters share the following properties unless noted:
- **Resolution:** 1 m
- **CRS:** EPSG:25829 (UTM zone 29N, ETRS89)
- **Format:** GeoTIFF

| File | Variable | Units | Software used |
|------|----------|-------|---------------|
| `dtm.tif` | Digital Terrain Model (bare-earth DEM) | m a.s.l. | LAStools (*lasground*) + SAGA-GIS 8.5.0 (ADW interpolation) |
| `slope.tif` | Slope | degrees | ArcMap 10.4 (Surface → Slope) |
| `cum_horizon.tif` | Cumulative horizon | index (0–1) | QGIS 3.28 Visibility Analysis plugin (Čučković 2016); 1,000 evenly distributed viewpoints |
| `cum_viewshed.tif` | Cumulative viewshed (visibility index) | index (0–1) | QGIS 3.28 Visibility Analysis plugin; 1,000 viewpoints; radius 3,000 m; observer height 1.60 m |
| `dist_to_ridges.tif` | Distance to ridges, shoulders, and spurs | metres | GRASS GIS 7.8 *r.geomorphon* (outer radius 64, inner radius 5) + *r.grow.distance* |
| `dist_to_rocks.tif` | Distance to rocky outcrops | metres | Manual vectorisation in ArcMap against PNOA (Spain) and DGT orthofotos 25cm (Portugal) + GRASS *r.grow.distance* |
| `dist_to_wat_edges.tif` | Distance to watershed divides | metres | GRASS GIS *r.watershed* (threshold 25,000) + *r.grow.distance* |
| `tpi.tif` | Topographic Prominence Index | index | SAGA-GIS 8.5.0 TPI-based landform classification (Conrad 2011, 2016); small scale 500, large scale 1,000 |
| `sum_structure.tif` | Sum of geomorphological structures (ridges + rocky outcrops + watershed edges) | index | QGIS 3.28 Raster Calculator: pixel-wise sum of `dist_to_ridges.tif`, `dist_to_rocks.tif`, and `dist_to_wat_edges.tif`; used as combined geomorphological covariate in Figure 6 |

---

## Derived Data (`data/derived/`)
CSV tables produced by the analysis scripts. All files are UTF-8 encoded with comma separators.

| File | Description | Produced by |
|------|-------------|-------------|
| `Mann_Whitney_results_DEM.csv` | Mann-Whitney U test results for altitude (999 MC simulations) | `Figure 4.R` |
| `Mann_Whitney_results_Slope.csv` | Mann-Whitney U test results for slope | `Figure 4.R` |
| `Mann_Whitney_results_cum_horizon.csv` | Mann-Whitney U test results for cumulative horizon | `Figure 4.R` |
| `Mann_Whitney_results_cum_viewshed.csv` | Mann-Whitney U test results for cumulative viewshed | `Figure 4.R` |
| `Mann_Whitney_results_dist_to_ridges.csv` | Mann-Whitney U test results for distance to ridges | `Figure 4.R` |
| `Mann_Whitney_results_dist_to_rocks.csv` | Mann-Whitney U test results for distance to rocky outcrops | `Figure 4.R` |
| `Mann_Whitney_results_dist_to_water_edges.csv` | Mann-Whitney U test results for distance to watershed edges | `Figure 4.R` |
| `Final_Landscape_Comparison.csv` | Summary table: observed vs landscape median and p-value for all covariates | `Figure 4.R` |
| `Wilcoxon_results.csv` | Wilcoxon test summary from Figure 4 pipeline (8 covariates) | `Figure 4.R` |

### Column definitions for Mann-Whitney result tables
| Column | Description |
|--------|-------------|
| `Lower CI` | 2.5th percentile of the test statistic / p-value across 999 simulations |
| `Median` | 50th percentile (median) |
| `Upper CI` | 97.5th percentile |

### Column definitions for `Final_Landscape_Comparison.csv`
| Column | Description |
|--------|-------------|
| `Variable` | Internal variable name |
| `Label` |Variable name |
| `Observed_Median` | Median covariate value at site locations |
| `Landscape_Median` | Median covariate value for 5,000 random landscape points |
| `P_Value` | Wilcoxon rank-sum test p-value (sites vs landscape) |
| `Significant` | "YES" if p < 0.05, "no" otherwise |

---

## References
Canedo, D. et al. (2023). Uncovering Archaeological Sites in Airborne LiDAR Data
With Data-Centric Artificial Intelligence. *IEEE Access*, 11, 65608–65619.

Čučković, Z. (2016). Advanced viewshed analysis: a Quantum GIS plug-in for the
analysis of visual landscapes. *Journal of Open Source Software*, 1(4), 32.

Vilas-Estévez, B. (2015). Estudio de las orientaciones y emplazamientos de los
túmulos de la necrópolis de la Serra do Leboreiro. Universidad de Santiago de
Compostela.
