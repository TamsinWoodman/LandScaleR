
# Generics for the MidCoords class
setGeneric("midX", function(x) standardGeneric("midX"))
setGeneric("midX<-", function(x, value) standardGeneric("midX<-"))
setGeneric("midY", function(x) standardGeneric("midY"))
setGeneric("midY<-", function(x, value) standardGeneric("midY<-"))
setGeneric("midCoords", function(x) standardGeneric("midCoords"))

# Methods for the MidCoords class
setMethod("midX", "MidCoords", function(x) x@mid_x)
setMethod("midX<-", "MidCoords", function(x, value) {
  x@mid_x <- value
  x
})
setMethod("midY", "MidCoords", function(x) x@mid_y)
setMethod("midY<-", "MidCoords", function(x, value) {
  x@mid_y <- value
  x
})
setMethod("midCoords", "MidCoords", function(x) c(x = x@mid_x, y = x@mid_y))

# Generics for the OuterCoords class
setGeneric("minX", function(x) standardGeneric("minX"))
setGeneric("minX<-", function(x, value) standardGeneric("minX<-"))
setGeneric("minY", function(x) standardGeneric("minY"))
setGeneric("minY<-", function(x, value) standardGeneric("minY<-"))
setGeneric("maxX", function(x) standardGeneric("maxX"))
setGeneric("maxX<-", function(x, value) standardGeneric("maxX<-"))
setGeneric("maxY", function(x) standardGeneric("maxY"))
setGeneric("maxY<-", function(x, value) standardGeneric("maxY<-"))

# Methods for the OuterCoords class
setMethod("minX", "OuterCoords", function(x) x@min_x)
setMethod("minX<-", "OuterCoords", function(x, value) {
  x@min_x <- value
  x
})
setMethod("minY", "OuterCoords", function(x) x@min_y)
setMethod("minY<-", "OuterCoords", function(x, value) {
  x@min_y <- value
  x
})
setMethod("maxX", "OuterCoords", function(x) x@max_x)
setMethod("maxX<-", "OuterCoords", function(x, value) {
  x@max_x <- value
  x
})
setMethod("maxY", "OuterCoords", function(x) x@max_y)
setMethod("maxY<-", "OuterCoords", function(x, value) {
  x@max_y <- value
  x
})

# Generics for CoarseCell class
setGeneric("id", function(x) standardGeneric("id"))
setGeneric("id<-", function(x, value) standardGeneric("id<-"))
setGeneric("lcDeltas", function(x) standardGeneric("lcDeltas"))
setGeneric("lcDeltas<-", function(x, value) standardGeneric("lcDeltas<-"))
setGeneric("cellArea", function(x) standardGeneric("cellArea"))
setGeneric("cellArea<-", function(x, value) standardGeneric("cellArea<-"))
setGeneric("refCells", function(x) standardGeneric("refCells"))
setGeneric("refCells<-", function(x, value) standardGeneric("refCells<-"))
setGeneric("sumRefCells", function(x) standardGeneric("sumRefCells"))
setGeneric("sumRefCells<-", function(x, value) standardGeneric("sumRefCells<-"))
setGeneric("refCellsArea", function(x) standardGeneric("refCellsArea"))
setGeneric("refCellsArea<-", function(x, value) standardGeneric("refCellsArea<-"))
setGeneric("reconcileLCDeltas", function(x, match_LC_classes) standardGeneric("reconcileLCDeltas"))

# Methods for CoarseCell class
setMethod("id", "CoarseCell", function(x) x@id)
setMethod("id<-", "CoarseCell", function(x, value) {
  x@id <- value
  x
})
setMethod("lcDeltas", "CoarseCell", function(x) x@LC_deltas)
setMethod("lcDeltas<-", "CoarseCell", function(x, value) {
  x@LC_deltas <- value
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
setMethod("sumRefCells", "CoarseCell", function(x) x@sum_ref_cells)
setMethod("sumRefCells<-", "CoarseCell", function(x, value) {
  x@sum_ref_cells <- value
  x
})
setMethod("refCellsArea", "CoarseCell", function(x) x@ref_cells_area)
setMethod("refCellsArea<-", "CoarseCell", function(x, value) {
  x@ref_cells_area <- value
  x
})

# Methods to reconcile LC deltas
setMethod("reconcileLCDeltas", "CoarseCell", function(x, match_LC_classes) {
  adjusted_LC_deltas <- x@LC_deltas * (x@ref_cells_area / x@cell_area)
  matched_LC_deltas <- colSums(match_LC_classes * adjusted_LC_deltas)

  x@LC_deltas <- matched_LC_deltas
  x
})

