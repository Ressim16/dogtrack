# dogtrack <img src="man/figures/logo.png" align="right" height="139" alt="" />

<!-- badges: start -->
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)
[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.19449978.svg)](https://doi.org/10.5281/zenodo.19449978)
<!-- badges: end -->

**Tracking Tails — Towards Rabies Eradication in Africa**

`dogtrack` is a reproducible GPS preprocessing pipeline for free-roaming
domestic dog movement studies. It implements four sequential quality filters
with site-specific thresholds automatically derived from static GPS test files,
and is designed to handle data from multiple African field sites simultaneously.

## Installation

```r
# Install from GitHub (requires the remotes package)
remotes::install_github("Ressim16/dogtrack")
```

## Quick start

```r
library(dogtrack)

dog_data_flagged <- run_pipeline(
  data_dir      = "data/",            # folder containing dog CSVs + static_*.CSV
  metadata_path = "data/metadata.xlsx"
)

flag_summary(dog_data_flagged)
```

## GPS device

Data must be collected with the **Columbus P-10 Pro** GPS logger (or a device
producing the same CSV column format). The `LATITUDE N/S` / `LONGITUDE E/W`
columns are normalised automatically to `lat` / `lon` by `preprocess_gps()`.

## Filters

| Stage | Filter | Default threshold | Basis |
|---|---|---|---|
| 01 | HDOP | 4.9 | Empirical — static tests |
| 02 | Displacement speed | 5.556 m/s | Dürr & Ward (2014) |
| 03 | Turning angle | 150° | Device artefact — Columbus P-10 Pro |
| 03 | Distance gate | site-specific | p95 positional error — static tests |
| 04 | Ellipsoidal height | site-specific | p99.9 height + 30 m — static tests |

All filters follow a **flag-don't-delete** philosophy. Every flag column is
independent and additive; analysts combine flags at analysis time.

## Data conventions

| File type | Naming convention | Example |
|---|---|---|
| Dog GPS | `{C}{V}{S}{HH}-{DD}[session].CSV` | `UMR001-01A.CSV` |
| Static test | `static{C}{V}{NN}.CSV` | `staticUM155.CSV` |
| Metadata | One Excel sheet per site | `Uganda_Location_ID` |

`C` = country code, `V` = city code, `S` = setting (R/U), `HH` = household,
`DD` = dog index, `session` = optional A/B/C suffix.

## Citation

If you use `dogtrack` in your research, please cite:

> Zahri R., Derkx I. (2026). *dogtrack: GPS Data Preprocessing Pipeline
> for Free-Roaming Dog Movement Studies* (v0.1.0).
> https://doi.org/10.5281/zenodo.19449978

## License

MIT © 2026 Reda Zahri, Inez Derkx
