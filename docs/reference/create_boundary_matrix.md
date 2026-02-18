# Create boundary matrix from planning units

Creates a sparse matrix of shared boundary lengths between adjacent
planning units. Returns a Matrix::sparseMatrix for efficient storage and
operations. This optimized version supports parallel processing via the
parallelly package. When n_cores = 1, runs sequentially with no parallel
overhead.

## Usage

``` r
create_boundary_matrix(planning_units, verbose = TRUE, n_cores = NULL)
```

## Arguments

- planning_units:

  sf object with planning unit geometries

- verbose:

  Logical, whether to print progress

- n_cores:

  Integer, number of cores to use. If NULL, uses availableCores(omit=1).
  Set to 1 for sequential processing.

## Value

Matrix::dgCMatrix sparse matrix where \[i,j\] is the shared boundary
length
