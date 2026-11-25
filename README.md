# Two centuries of river engineering: changes in meta-ecosystem resilience and connectivity

Code and data for the analysis of changes in functional habitat connectivity and meta-ecosystem resilience along a 150 km section of the Danube (Austria) and its floodplains between 1817 and 2022.

**Authors:** Johannes L. Kowal, Sarah M. Gross, Gertrud Haidvogl, Thomas Hein, Severin Hohensinner, Andrea Funk
**Contact:** johannes.kowal@boku.ac.at

## Citation

- **Manuscript:** Kowal et al. (2026), *Two centuries of river engineering: changes in meta-ecosystem resilience and connectivity*, in revision.
- **Preprint:** Kowal et al. (2026), *200 years of river engineering: changes in meta-ecosystem resilience and connectivity*, bioRxiv, https://doi.org/10.64898/2026.07.03.736339
- **Data and code (all versions):** https://doi.org/10.5281/zenodo.21165310

Data and code are published on Zenodo under the Creative Commons Attribution 4.0 International license (CC BY 4.0).

## Overview

`SCRIPT.R` runs the full analysis in the following sections:

1. **PREP DATA:** habitat suitability clusters (SC) of taxa, reach widths, reaches (nodes), links and observational samples, and the habitat networks (graphs) for seven points in time (1817, 1857, 1875, 1910, 1965, 1996 and 2022).
2. **CALCULATE INDICES:** reach and catchment connectivity indices (RCI, CCI) with the riverconn package, along the network for medium and short distance migrants (MDM, SDM) and passive dispersers (PD), and along Euclidean distances for taxa dispersing more independently of the network structure (EUC).
3. **VALIDATION:** comparison of RCIs at sites where taxa were present and absent in 2022 (one sided Wilcoxon tests).
4. **CC (CLUSTERS):** connectivity clusters (CC) of reaches based on their RCIs.
5. **RESILIENCE:** multilayer networks and three structural indicators: robustness, dissimilarity between layers and modularity of connectivity clusters.
6. **PLOTS:** all figures of the manuscript and the SI Appendix, based on the saved results.

## Repository structure

| Folder or file | Content |
|---|---|
| `SCRIPT.R` | Complete analysis and figures |
| `NODES/` | Reaches (nodes) of the habitat network per year (`NODES_XXXX.shp`) |
| `LINKS/` | Links between nodes per year (`LINKS_XXXX.csv`) |
| `WEIGHTS/` | Habitat suitability of taxa, conversion of habitat types and reach widths |
| `SAMPLES/` | Presence and absence records of taxa used for validation |
| `RESULTS/` | Saved results of the analysis (indices, validation, clusters, resilience indicators) |
| `PLOTS/` | Figures (main figures also as PDF) and spatial layers used for the maps |
| `ICONS/` | Icons of the dispersal modes used in the figures |
| `PROGRESS_LOG_ROBUSTNESS.txt` | Progress log written during the robustness analysis |

## Requirements

- R 4.3 or later (developed with R 4.3.2).
- R packages loaded at the top of `SCRIPT.R` (installed automatically if missing): this.path, readxl, data.table, tidyverse, igraph, sf, terra, raster, future.apply, vegan, cluster, factoextra, riverconn, ggnewscale, ggtext, paletteer, plotly, ggspatial, ggfx, ggridges, patchwork, elevatr, grid, png, ragg, latex2exp and xdvir. In addition, rstudioapi, scales, broom and ggpubr are called directly.
- [Git LFS](https://git-lfs.com) to download the large shapefiles (`RCI_MASTER`, `VALIDATION`, `RIVERS`) when cloning the repository.
- TinyTeX with XeLaTeX for the conceptual figure (`CONCEPT`), which renders formulas with xdvir.
- An internet connection for the elevation data of the study site map (elevatr).

## Running the analysis

Open `SCRIPT.R` and run it from the top. The working directory is set to the location of the script (via rstudioapi in RStudio and VS Code, or this.path otherwise). All results are written to `RESULTS/` and all figures to `PLOTS/`.

The robustness analysis runs in parallel and can take many hours. Its progress can be followed in `PROGRESS_LOG_ROBUSTNESS.txt`. As all results are included in `RESULTS/`, the figures can be reproduced without rerunning the analysis: run the sections PACKAGES and FUNCTIONS, the data import at the beginning of PREP DATA, and then the PLOTS section.

## Data description

Abbreviations used below:

- **Habitat types (`WB_TYPE`)** following Amoros et al. (1987), as adapted by Hohensinner et al. (2011): `eua` eupotamon A, `eub` eupotamon B, `paa` parapotamon A, `pab` parapotamon B, `ple` plesiopotamon, `tri` tributary, `art` artificial waterbody, `CO` connector.
- **Habitat types H1 to H5** of the floodplain habitat index (Chovanec et al. 2005), from highly dynamic (H1) to isolated (H5).
- **Organism groups (`GROUP`):** `FISH` fish, `AMPG` amphibians, `FI` flying macroinvertebrates, `NFI` non flying macroinvertebrates, `PLANT` plants.
- **Dispersal modes (`DISP`):** `MDM` medium distance migrants, `SDM` short distance migrants, `PD` passive dispersers, `EUC` taxa dispersing more independently of the network structure.
- **Suitability clusters (`SC`):** groups of taxa with similar habitat preferences, from taxa of highly dynamic waterbodies (SC1) to taxa of isolated standing waters (SC4).

### NODES/NODES_XXXX.shp

Reaches (nodes) of the habitat network in year XXXX, as spatial line features.

| Column | Description |
|---|---|
| `FID` | Reach (node) identifier |
| `WB_TYPE` | Habitat type |
| `LENGTH` | Reach length [m] |
| `IMP` | Impounded (1) or not impounded (0) |
| `SECTION` | Part of the Old Danube (`OD`) or not (`GENERAL`) |

### LINKS/LINKS_XXXX.csv

Directed links between nodes of the habitat network in year XXXX.

| Column | Description |
|---|---|
| `IN_FID` | Source node identifier (`FID` of the reach) |
| `NEAR_FID` | Target node identifier (`FID` of the reach) |
| `TYPE` | Link type: `SL` sluice, `DAM` dam, `FP` fish pass, `LINK` confluence or upstream and downstream connection between reaches |
| `pass_u` | Upstream passability (1 for `LINK`) |
| `pass_d` | Downstream passability (1 for `LINK`) |

### WEIGHTS/SC_TAXA.csv

Habitat suitability of the floodplain habitat index types for each taxon (Chovanec et al. 2005).

| Column | Description |
|---|---|
| `ID_TAXA` | Taxon identifier |
| `H1` to `H5` | Habitat suitability for H1 to H5 (0 low to 10 high) |
| `GENUS`, `SPECIES` | Taxonomic genus and species |
| `GROUP` | Organism group |
| `DISP` | Dispersal mode |

### WEIGHTS/CONVERSION.csv

Translation between habitat types and the habitat types of the floodplain habitat index.

| Column | Description |
|---|---|
| `AMOROS` | Habitat type |
| `FHI` | Habitat type of the floodplain habitat index (H1 to H5) |
| `IMP` | Impounded (1) or not impounded (0) |

### WEIGHTS/WIDTHS.csv

Average riverbed width per habitat type and year.

| Column | Description |
|---|---|
| `WB_TYPE` | Habitat type |
| `YEAR` | Year |
| `SECTION` | Old Danube (`OD`) or not (`GENERAL`) |
| `IMP` | Impounded (1) or not impounded (0) |
| `WIDTH` | Average width [m] |

### SAMPLES/SAMPLES.shp

Presence and absence records of taxa (fish and macroinvertebrates, sampled between 2000 and 2024) used to validate the RCIs.

| Column | Description |
|---|---|
| `ID_SMPL` | Sample identifier |
| `GROUP` | Organism group |
| `DISP` | Dispersal mode |
| `GENUS`, `SPECIES` | Taxonomic genus and species |
| `PRES` | Present (1) or absent (0) at the time of sampling |
| `YEAR` | Year of sampling |

### RESULTS/RCI_MASTER/RCI_MASTER.shp

Reach connectivity indices (RCI) for every reach, year and configuration.

| Column | Description |
|---|---|
| `ID` | Index identifier |
| `RCI` | `NUM` divided by `DEN`, normalized RCI (Baldan et al. 2022) |
| `NUM` | Unscaled RCI, used in this study to compare years |
| `DEN` | Denominator for the normalization |
| `DEN_G` | Denominator for the normalization within each connected subnetwork |
| `FID` | Reach (node) identifier |
| `ID_G` | Identifier of the connected subnetwork (graph) |
| `YEAR` | Year |
| `CONF_ID` | Configuration identifier |
| `DISP` | Dispersal mode of the configuration |
| `SC` | Suitability cluster of the configuration |
| `CC` | Connectivity cluster |
| `DIST` | Weighted reach length used to calculate distances (`DSC1` to `DSC4`, one per suitability cluster) |
| `WEIGHT` | Reach (node) weight |
| `WB_TYPE` | Habitat type |

### RESULTS/CCI_MASTER.csv

Catchment connectivity indices (CCI) for every year and configuration.

| Column | Description |
|---|---|
| `ID` | Index identifier |
| `CCI` | `NUM` divided by `DEN`, normalized CCI (Baldan et al. 2022) |
| `NUM` | Unscaled CCI, used in this study to compare years |
| `DEN` | Denominator for the normalization |
| `YEAR` | Year |
| `CONF_ID` | Configuration identifier |
| `DISP` | Dispersal mode of the configuration |
| `SC` | Suitability cluster of the configuration |

### RESULTS/VALIDATION/VALIDATION.shp

Samples of 2022 joined with the RCI of the nearest reach. Contains the columns of `SAMPLES.shp`, the habitat suitability of the taxon (`H1` to `H5`, their cluster medians and deviations), its suitability cluster (`SC`) and the `RCI`, `NUM`, `DEN`, `WEIGHT` and `WB_TYPE` of the reach (`FID_RCI`).

### RESULTS/SIGNIFICANCE.csv

Validation results per configuration.

| Column | Description |
|---|---|
| `DISP`, `SC` | Configuration |
| `P` | P value of the one sided Wilcoxon test (RCI higher where taxa were present) |
| `LEVEL` | Significance level |
| `N_Present`, `N_Absent` | Number of presence and absence records |

### RESULTS/K_WSS_SC.csv and RESULTS/K_WSS_CC.csv

Within sum of squares (`WSS`) for different numbers of clusters (`K`), used to choose the number of suitability clusters (SC) and connectivity clusters (CC).

### RESULTS/CLUSTER_VARIABLES.csv

Configurations (`SC`, `DISP`) used as variables for the connectivity clusters (`LABEL`).

### RESULTS/ROBUSTNESS.rds

Path lengths in the multilayer network after removing nodes, per year (`YEAR`), replicate (`REPLICATE`) and number of removed nodes (`REMOVED`): remaining nodes (`NODES_LEFT`), sum, median, mean and standard deviation of the path lengths (`DIST_SUM`, `DIST_MEDIAN`, `DIST_MEAN`, `DIST_SD`) and maximum possible number of paths (`NR_PATHS`).

### RESULTS/ROBUSTNESS_REACH.csv

Share of connected node pairs (`REACH_SHARE`) per year (`YEAR`) and number of removed nodes (`REMOVED`), with the number of remaining nodes (`NODES_LEFT`), averaged over the replicates.

### RESULTS/DISSIMILARITY.csv

Dissimilarity (`DISSIMILARITY`) between pairs of layers (`CONFIG_1`, `CONFIG_2`) of the multilayer network per year (`YEAR`).

### RESULTS/MODULARITY.csv

Normalized modularity of the connectivity clusters (`MD`) per year (`YEAR`).

## References

- Amoros C, Roux AL, Reygrobellet JL, Bravard JP, Pautou G (1987) A method for applied ecological studies of fluvial hydrosystems. Regulated Rivers: Research and Management 1(1), 17 to 36. https://doi.org/10.1002/rrr.3450010104
- Baldan D, Cunillera-Montcusí D, Funk A, Hein T (2022) Introducing riverconn: an R package to assess river connectivity indices. Environmental Modelling and Software 156, 105470. https://doi.org/10.1016/j.envsoft.2022.105470
- Chovanec A, Waringer J, Straif M, Graf W, Reckendorfer W, Waringer-Löschenkohl A, Waidbacher H, Schultz H (2005) The Floodplain Index: a new approach for assessing the ecological status of river/floodplain systems according to the EU Water Framework Directive. Archiv für Hydrobiologie, Large Rivers 15, 169 to 185.
- Hohensinner S, Jungwirth M, Muhar S, Schmutz S (2011) Spatio-temporal habitat dynamics in a changing Danube River landscape 1812 to 2006. River Research and Applications 27(8), 939 to 955. https://doi.org/10.1002/rra.1407
