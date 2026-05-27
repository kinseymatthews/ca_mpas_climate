
# ca_mpas_climate

This repository contains the data processing, analysis, and modeling code for a study examining how California commercial fishing grounds have shifted over time and how those shifts relate to climate variability, spatial management, and changing economic conditions.

## Overview

This project combines CDFW fisheries logbook and landing receipt data with environmental and economic covariates to:

- Track the spatial distribution of fishing effort over time using kernel density estimation
- Quantify changes in fishing ground location (center of gravity), extent (75% volume contour area and most northern and southern extents), and overlap with spatial regulation areas (e.g., marine protected areas, cowcod conservation areas, etc.)
- Model how SST, ex-vessel prices, fuel costs, regulations, distance to port complex, and distance to shore predict fishing effort at the block level
- Generate spatial predictions under different climate and economic scenarios

## Fisheries

Nine fisheries are analyzed, each in its own subdirectory:

| Fishery | Gear type | Folder |
|---------|-----------|--------|
| California halibut | Bottom trawl | `Halibut/` |
| Market squid | Purse seine / drum seine | `Squid/` |
| Sea cucumber | Trawl | `Cucumber/` |
| Sea cucumber | Dive | `Dive/Code/Sea Cucumber/` |
| Sea urchin | Dive | `Dive/Code/Sea Urchin/` |
| Drift gillnet | Gillnet | `Gillnet/Code/Drift/` |
| Set gillnet | Gillnet | `Gillnet/Code/Set/` |
| Spiny lobster | Trap | `Lobster/` |
| Pink shrimp | Trawl | `Shrimp/Code/Pink shrimp/` |
| Ridgeback prawn | Trawl | `Shrimp/Code/Ridgeback/` |
| Spot prawn | Trawl | `Shrimp/Code/Spot prawn/` |
| CPFV (recreational charter boat) | Hook-and-line | `CPFV/` |

Port complexes covered: Eureka, Fort Bragg, Bodega Bay, San Francisco, Monterey, Morro Bay, Santa Barbara, Los Angeles, San Diego.

## Repository structure

```
ca_mpas_climate/

│
├── <Fishery>/Code/
│   ├── 1. Data Processing.R               # Raw data cleaning and filtering
│   ├── 2. Exploratory Figures.R           # Trip/vessel count summaries
│   ├── 3. Block Data.R                    # Effort aggregated to CDFW blocks
│   ├── 4. Center of Gravity.R             # Weighted mean latitude / distance to shore
│   ├── 5. Kernel Density.R                # KDE, 75% volume contours, area time series
│   ├── 5.5 Five year Kernel Densities     # Recent 5-year combined KDE for figures
│   └── 6. MPAs.R                          # Fishing effort overlap with MPA boundaries
│
├── Variables/
│   ├── Temperature/         # Monthly SST and bottom temperature (GLORYS)
│   ├── Ex vessel price/     # Inflation-adjusted ex-vessel prices by port/fishery
│   ├── Fuel prices/         # Diesel prices by port complex
│   ├── Distance to shore/   # Block-level distance-to-shore raster
│   ├── Distance to Port Complex/  # Block-to-port-complex distances
│   └── Regulation Changes/  # MPA implementation dates and spatial closures
│
├── Modeling/Code/           # sdmTMB hurdle (delta) models + prediction scripts
├── Figures for paper/       # Synthesis figures combining all fisheries
├── Shinydashboard/          # Shiny app — California Fishing Grounds Data Explorer
└── ca_mpas_climate.Rproj
```

## Getting started

1. Open `ca_mpas_climate.Rproj` so the working directory is set to the project root.
2. Raw data are stored on a shared Google Drive and are not included in this repository (see `.gitignore`). Contact the authors for data access.
3. Scripts are numbered and intended to be run in order within each fishery folder.

## Key methods

**Kernel density estimation** — Fishing effort (tow minutes or angler hours) is aggregated to CDFW statistical blocks and used as weights in a Gaussian KDE (`spatstat::density.ppp` with `bw.diggle` bandwidth selection). The 75% volume contour is used as the operational fishing ground boundary.

**Center of gravity** — The effort-weighted mean latitude and distance to shore are computed annually for each port complex to track the broad direction of spatial shift.

**MPA overlap** — The fraction of the 75% KDE contour area that falls within MPA boundaries is calculated each year, providing a measure of how much fishing effort overlaps with protected areas before and after MPA implementation (2007–2012).

**Delta models** — Hurdle (delta) models fit with `sdmTMB` (SPDE/INLA spatial random fields) separately model the probability of fishing a block and the amount of effort conditional on fishing. Covariates include SST, bottom temperature, ex-vessel price, fuel price, distance to shore, distance to port, bathymetry, and regulatory closure fraction.

## Dependencies

```r
# Spatial / KDE
tidyverse, sf, sp, spatstat, raster, terra, rnaturalearth

# Modeling
sdmTMB, sdmTMBextra, INLA, broom, ggeffects

# Visualization
ggplot2, RColorBrewer, ggforce, scales, magick, gganimate

# App
shiny, wcfish
```

## Funding and data

This work was supported by the Ocean Protection Council through Proposition 68 funding. Fisheries data were provided by the California Department of Fish and Wildlife (CDFW) under a formal data agreement. CDFW data are provided as-is; use does not constitute CDFW's endorsement of any analytical methods or conclusions.
