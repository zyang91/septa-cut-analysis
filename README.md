# SEPTA Service Cut Analysis

Zhanchao Yang

## Overview

This project analyzes the impact of SEPTA service cuts by computing transit travel times from census tract centroids to Philadelphia City Hall, comparing before and after scenarios.

## Recent Changes

### Memory Optimization (Latest)

The `r5r.r` script has been refactored to reduce memory usage and improve performance on computers with limited resources:

- **Function Decomposition**: The complex `build_core_and_ttm` function has been broken into three focused helper functions:
  - `setup_r5_core()` - R5 core initialization and network building
  - `compute_travel_time_matrix()` - Routing computation with reduced memory parameters
  - `cleanup_r5_core()` - Proper cleanup and garbage collection

- **Memory Optimizations**:
  - Reduced parallel threads from `n_cores - 1` to maximum of 2 threads
  - Reduced `time_window` from 60 to 30 minutes
  - Reduced `draws_per_minute` from 5 to 3
  - Added explicit garbage collection between operations

See [CHANGES.md](CHANGES.md) for detailed information about the refactoring.

## Requirements

- Java 11+
- R packages: r5r >= 1.0.0, sf, tigris, tidyverse, readr, lubridate, ggplot2, scales, tidytransit

## Usage

Run the analysis with:
```r
source("r5r.r")
```

More info will come soon!
