
# Generics for CoarseCell class
setGeneric("cellNumber", function(x) standardGeneric("cellNumber"))
setGeneric("cellNumber<-", function(x, value) standardGeneric("cellNumber<-"))
setGeneric("lcDeltas", function(x) standardGeneric("lcDeltas"))
setGeneric("lcDeltas<-", function(x, value) standardGeneric("lcDeltas<-"))
setGeneric("neighbourCells", function(x) standardGeneric("neighbourCells"))
setGeneric("neighbourCells<-", function(x, value) standardGeneric("neighbourCells<-"))
setGeneric("cellArea", function(x) standardGeneric("cellArea"))
setGeneric("cellArea<-", function(x, value) standardGeneric("cellArea<-"))
setGeneric("refCells", function(x) standardGeneric("refCells"))
setGeneric("refCells<-", function(x, value) standardGeneric("refCells<-"))
setGeneric("kernelDensities", function(x) standardGeneric("kernelDensities"))
setGeneric("kernelDensities<-", function(x, value) standardGeneric("kernelDensities<-"))
setGeneric("aggRefCells", function(x) standardGeneric("aggRefCells"))
setGeneric("aggRefCells<-", function(x, value) standardGeneric("aggRefCells<-"))
setGeneric("refCellsArea", function(x) standardGeneric("refCellsArea"))
setGeneric("refCellsArea<-", function(x, value) standardGeneric("refCellsArea<-"))
setGeneric("reconcileLCDeltas", function(x, match_LC_classes, LC_deltas_type) standardGeneric("reconcileLCDeltas"))
setGeneric("updateCoarseCell", function(x, LC_deltas, LC_deltas_classes, kernel_densities, ref_map_polygons) standardGeneric("updateCoarseCell"))
setGeneric("isUnallocatedLC", function(x) standardGeneric("isUnallocatedLC"))
setGeneric("lcDeltasAndCoords", function(x) standardGeneric("lcDeltasAndCoords"))
setGeneric("areasMatch", function(x) standardGeneric("areasMatch"))

# Methods for CoarseCell class
setMethod("cellNumber", "CoarseCell", function(x) x@cell_number)
setMethod("cellNumber<-", "CoarseCell", function(x, value) {
  x@cell_number <- value
  x
})
setMethod("lcDeltas", "CoarseCell", function(x) x@LC_deltas)
setMethod("lcDeltas<-", "CoarseCell", function(x, value) {
  x@LC_deltas <- value
  x
})
setMethod("neighbourCells", "CoarseCell", function(x) x@neighbour_cells)
setMethod("neighbourCells<-", "CoarseCell", function(x, value) {
  x@neighbour_cells <- value
  x
})
setMethod("cellArea", "CoarseCell", function(x) x@cell_area)
setMethod("cellArea<-", "CoarseCell", function(x, value) {
  x@cell_area <- value
  x
})
setMethod("refCells", "CoarseCell", function(x) x@ref_cells)
setMethod("refCells<-", "CoarseCell", function(x, value) {
  x@ref_cells <- value
  x
})
setMethod("kernelDensities", "CoarseCell", function(x) x@kernel_densities)
setMethod("kernelDensities<-", "CoarseCell", function(x, value) {
  x@kernel_densities <- value
  x
})
setMethod("aggRefCells", "CoarseCell", function(x) x@agg_ref_cells)
setMethod("aggRefCells<-", "CoarseCell", function(x, value) {
  x@agg_ref_cells <- value
  x
})
setMethod("refCellsArea", "CoarseCell", function(x) x@ref_cells_area)
setMethod("refCellsArea<-", "CoarseCell", function(x, value) {
  x@ref_cells_area <- value
  x
})
setMethod("lcDeltasAndCoords", "CoarseCell", function(x) {
  lc_deltas_and_coords <- cbind(x@cell_coords,
                                t(as.matrix(x@LC_deltas)))

  return(lc_deltas_and_coords)
})

# Method to update a coarse-scale cell
setMethod("updateCoarseCell", "CoarseCell", function(x,
                                                     LC_deltas,
                                                     LC_deltas_classes,
                                                     kernel_densities,
                                                     ref_map_polygons) {

  cell_LC_deltas <- unlist(LC_deltas[x@cell_number])
  cell_LC_deltas <- cell_LC_deltas[LC_deltas_classes]
  x@LC_deltas <- cell_LC_deltas

  kernel_densities_crop <- terra::crop(kernel_densities,
                                       ref_map_polygons[ref_map_polygons$coarse_ID == x@cell_number],
                                       mask = TRUE)
  x@kernel_densities <- kernel_densities_crop

  x
})


# Methods to reconcile LC deltas
setMethod("reconcileLCDeltas", "CoarseCell", function(x, 
                                                      match_LC_classes,
                                                      LC_deltas_type) {
  
  adjusted_LC_deltas <- switch(LC_deltas_type,
                               "areas" = x@LC_deltas * (x@ref_cells_area / x@cell_area),
                               "proportions" = x@LC_deltas * x@cell_area)
  
  matched_LC_deltas <- colSums(match_LC_classes * adjusted_LC_deltas)

  x@LC_deltas <- matched_LC_deltas
  x
})

# Method to check a coarse cell for unallocated land cover
setMethod("isUnallocatedLC", "CoarseCell", function(x) {

  if (is.na(x@ref_cells_area)) {
    # This accounts for cases where there are no reference map cells in a coarse resolution grid cell
    return(FALSE)
    
  } else {
    
    area_tolerance <- (x@ref_cells_area / 100) * 0.01
    
    # Check if any LC_deltas are more than or equal to 0
    if (any(x@LC_deltas > area_tolerance)) {
      return(TRUE)
    } else {
      return(FALSE)
    }
    
  }
  
})


# Method to check that areas match after downscaling and harmonisation`
setMethod("areasMatch", "CoarseCell", function(x) {

  if (!is.na(x@ref_cells_area)) {
    
    total_old_area <- x@ref_cells_area
    # New area must be within 0.01% of the original area
    area_tolerance <- (total_old_area / 100) * 0.0001
    
    new_area <- unlist(terra::global(x@ref_cells,
                                     fun = "sum",
                                     na.rm = TRUE))
    total_new_area <- sum(new_area)
    
    if(total_new_area < total_old_area - area_tolerance |
       total_new_area > total_old_area + area_tolerance) {
      
      warning("Total area of reference cells before and after downscaling does not match. New area: ",
              total_new_area,
              " Old area: ",
              total_old_area)
      
    }
  }

})
