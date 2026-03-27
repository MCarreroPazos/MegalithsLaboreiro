# Spatial Insights of the Tumular Phenomenon in Serra do Laboreiro

[![License: MIT](https://img.shields.io/badge/Code%20License-MIT-yellow.svg)](LICENSE)
[![License: CC BY-NC 4.0](https://img.shields.io/badge/Data%20License-CC%20BY--NC%204.0-lightgrey.svg)](LICENSE-data)
[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.15480087.svg)](https://zenodo.org/records/19239670)

## About
This repository contains the code and data for the paper:
> Lima e Silva, D., Carrero-Pazos, M., Lara-Piñera, F., Fonte, J. & Vilas-Estévez, B.
> (2026). "Spatial insights of the tumular phenomenon in Serra do Laboreiro region
> (NW Iberian Peninsula)". *Journal of Archaeological Science Reports*.

## Abstract
The region of Planalto de Castro Laboreiro (Melgaço, Portugal), Baixa Limia (Ourense, Spain), and its surrounding areas is renowned for its significant concentration of tumular and megalithic sites. Despite the lack of formal excavations for most of the sites, this study employs a landscape archaeology approach supported by spatial statistics to identify possible spatial patterning behind the construction of the megalithic landscape. We tested multiple landscape features that have long been emphasized as having a significant influence on the placement of these monuments within the territory, and present how the geomorphology of the territory acts as both a constraint and social booster in the construction and deconstruction of these multiple spaces over time. 

## Keywords
Landscape archaeology, Geographic Information Systems, Spatial Statistics, Megalithic Complex, Portugal, Spain.

---

## Repository Structure
```
Laboreiro/
├── DESCRIPTION                   # Project metadata and R dependencies
├── README.md                     # This file
├── LICENSE                       # MIT License (code)
├── LICENSE-data                  # CC BY-NC 4.0 (data and text)
├── CITATION.cff                  # Machine-readable citation metadata
├── .gitignore
├── manuscript.html               # Shortcut/Copy of the compiled manuscript
├── renv.lock                     # Reproducible R environment snapshot
│
├── analysis/
│   ├── paper/
│   │   ├── quarto_project.qmd    # Main compendium document (paper + code)
│   │   ├── grateful-refs.bib     # R package citations (auto-generated)
│   │   └── styles.css            # Custom HTML styles
│   ├── scripts/
│   │   ├── 00_Setup.R                # Load packages and define shared paths
│   │   ├── Figure 1.R                # Study area map
│   │   ├── Figure 2.R                # Composite photograph panel of megalithic sites
│   │   ├── Figure 3.R                # Selected GIS covariates
│   │   ├── Figure 4.R                # Monte Carlo KDE + Mann-Whitney U tests
│   │   ├── Figure 5.R                # Intervisibility network map
│   │   └── Figure 6.R                # Spatial point process models
│   └── functions/
│       ├── rbias_sitesVSbackground.R  # Monte Carlo KDE simulation function
│       └── rbias_simulationresults.R  # Plotting function for simulation results
│
├── data/
│   ├── raw/                      # Original data
│   │   ├── shp/                  # Vector data (GeoPackage + Shapefile)
│   │   └── grid/                 # Raster covariates (GeoTIFF)
│   └── derived/                  # Outputs of analysis (CSV tables)
│
├── figures/                      # Output figures (PDF)
├── images/                       # Field photographs
└── docs/                         # Compiled HTML output (for GitHub Pages)
```

---
> **Note:** The large raster files in `data/raw/grid/` are archived separately on
> Zenodo (see DOI badge above). Download them from Zenodo and place
> them in `data/raw/grid/` before running the analysis.

> **Note:** AI Statement: Large Language Models (LLMs) have been used for coding, polishing, and scientific writing assistance. The authors are responsible for the content and accuracy of the analysis and findings reported in this manuscript.
---

## Data Description
See [`data/DATA_DESCRIPTION.md`](data/DATA_DESCRIPTION.md) for a full description of
all datasets, including variable definitions, and original data sources.

---

## Licences
- **Code** (all `.R` and `.qmd` files): [MIT License](LICENSE)
- **Data and text** (all files in `data/`, `figures/`, `images/`, and manuscript text): [CC BY-NC 4.0](LICENSE-data) — free to share and adapt with attribution, **not for commercial use**

---

## Citation
If you use any part of this repository, please cite:
> Lima e Silva, D., Carrero-Pazos, M., Lara-Piñera, F., Fonte, J. & Vilas-Estévez, B.
> (2026). "Spatial insights of the tumular phenomenon in Serra do Laboreiro region
> (NW Iberian Peninsula)". *Journal of Archaeological Science: Reports*.

A machine-readable citation is available in [`CITATION.cff`](CITATION.cff).

---

## Acknowledgements
Denise Lima e Silva is funded by FCT Foundation for Science and Technology (FCT scholarship nº. 2020.04713.BD). This work was done under the European Union’s Horizon 2020 research and innovation programme (Marie Skłodowska-Curie Grant Agreements No. 886793, PI: MCP; and No. 794048 PI: JF) and is part of the project *Paisajes Megalíticos: Explorando los factores humanos y ambientales de las sociedades neolíticas en el noroeste de la Península Ibérica (V-II milenio a.C.)* (MegaLands), PID2024-156264NA-I00 funded by MICIU /AEI /10.13039/501100011033/ FEDER, UE (IP: Miguel Carrero Pazos and Noemí Silva Sánchez). The authors want to thank the Comunidade Intermunicipal do Alto Minho (CIM Alto Minho) for providing the LiDAR data used in this work.

R package citations are managed with the
[`grateful`](https://pakillo.github.io/grateful/) package; see
`analysis/paper/grateful-refs.bib` for the full bibliography.
