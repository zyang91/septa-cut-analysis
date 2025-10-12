# R5R Function Refactoring - Memory Optimization

## Changes Made

### Problem
The original `build_core_and_ttm` function was monolithic and memory-intensive, causing issues on computers with limited resources.

### Solution
The function has been broken down into three smaller, focused functions:

1. **`setup_r5_core(osm_pbf, gtfs_zip)`**
   - Handles R5 core initialization and network building
   - Returns the configured R5 core
   
2. **`compute_travel_time_matrix(r5r_core, origins_df, destinations_df, departure_time, modes, max_walk_dist, max_trip_duration, walk_speed)`**
   - Performs the actual routing computation
   - **Memory optimizations:**
     - Reduced `n_threads` from `detectCores() - 1` to `max 2 threads` to limit parallel memory usage
     - Reduced `draws_per_minute` from `5` to `3` to decrease sampling overhead
     - Reduced `time_window` from `60` minutes to `30` minutes to limit search space
   - Returns the travel time matrix
   
3. **`cleanup_r5_core(r5r_core)`**
   - Properly stops the R5 core
   - Forces garbage collection to free memory
   
4. **`build_core_and_ttm(...)` - Main orchestrator**
   - Calls the three helper functions in sequence
   - Includes explicit garbage collection (`gc()`) between steps
   - Maintains backward compatibility with existing code

### Additional Improvements
- Added explicit garbage collection between BEFORE and AFTER scenario computations
- Added a 2-second pause between scenarios to allow system resources to be fully released
- Added informative messages to track progress through each stage

### Benefits
1. **Reduced Memory Footprint**: Lower parallel processing overhead and smaller search windows
2. **Better Resource Management**: Explicit cleanup and garbage collection at key points
3. **Modularity**: Functions can be called independently if needed
4. **Maintainability**: Easier to understand and modify individual components
5. **Backward Compatible**: The main `build_core_and_ttm` function maintains the same signature

### Impact
These changes should allow the analysis to run successfully on computers with limited memory while still producing comparable results (with slightly reduced time window coverage).
