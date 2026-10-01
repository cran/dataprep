\name{melt}
\alias{melt}
\title{Fast wide-to-long data reshaping with flexible ID/measure specification}
\description{
Transforms a wide-format data frame into a long-format data frame using a
C++ backend with SIMD and optional OpenMP parallelism. Supports automatic
inference of ID columns, user-specified ID or measure variables, custom
column names, and two storage layouts (row-major or column-major).
}
\usage{
melt(data, id = NULL, measure.vars = NULL,
     variable.name = "variable", value.name = "value",
     na.rm = FALSE, cores = NULL, major = NULL,
     as.factor = NULL,
     verbose = FALSE, parallel_threshold = 5e6, id.vars = NULL)
}
\arguments{
  \item{data}{A data frame to reshape. Numeric, integer, logical,
    character, and factor columns are accepted as id columns. Factor
    columns used as \code{measure.vars} cannot be cast to numeric, so
    their cells are written as \code{NA} in the value column; pass
    \code{measure.vars} as \code{character} or \code{numeric} to
    avoid this.}
  \item{id}{Identifier columns. Character names, integer indices, a
    logical mask, or \code{NULL}. If \code{NULL} and
    \code{measure.vars} is also \code{NULL}, ID columns are inferred
    automatically: non-numeric, non-integer, non-logical columns and
    factors are treated as IDs.}
  \item{measure.vars}{Measure columns. Character names, integer
    indices, a logical mask, or \code{NULL}. If supplied, all other
    columns become IDs. Only one of \code{id} and \code{measure.vars}
    should be supplied.}
  \item{variable.name}{Name of the new column that stores the
    original column names of the measure variables. Default
    \code{"variable"}. See \code{as.factor} for its type.}
  \item{value.name}{Name of the new numeric column that stores the
    melted values. Default \code{"value"}.}
  \item{na.rm}{Logical; if \code{TRUE}, rows where the value column is
    \code{NA} are removed. Default \code{FALSE}.}
  \item{cores}{Number of OpenMP threads. \code{NULL} (default) lets the
    C++ backend pick a value based on data size. The global option
    \code{dataprep.cores} overrides the automatic choice.}
  \item{major}{Memory layout. \code{NULL} (default) is equivalent to
    \code{"col"}: column-major, identical to \code{reshape2::melt}
    (rows grouped by variable). \code{"row"} produces a row-major
    result matching the row order of \code{tidyr::pivot_longer}
    (rows grouped by id). There is no automatic switching based on
    the input shape.}
  \item{as.factor}{Whether the \code{variable} column is a factor.
    \code{NULL} (default) uses \code{TRUE} when \code{major} is
    \code{"col"} and \code{FALSE} when \code{major} is \code{"row"}.
    Explicit \code{TRUE} / \code{FALSE} overrides that default. The
    return value is always a plain \code{data.frame}; no
    \code{tibble} attributes are attached.}
  \item{verbose}{Logical; if \code{TRUE}, prints timing messages.}
  \item{parallel_threshold}{Minimum number of output elements, i.e.\
    \code{nrow(data) * length(measure.vars)}, before automatic
    parallelism is enabled. Default \code{5e6}.}
  \item{id.vars}{Alias of \code{id}, provided for reshape2 / data.table
    compatibility. Supplying both is an error.}
}
\details{
\code{melt()} is the fast, drop-in replacement for
\code{reshape2::melt} and \code{tidyr::pivot_longer}. The C++
backend (\code{melt_cpp}) has been benchmarked against \strong{all
seven major alternatives} in the R and Python ecosystems:
\code{reshape2}, \code{data.table}, \code{tidyr}, \code{pandas},
\code{polars}, \code{dask}, and \code{duckdb}. Every cell is
measured with a C++ steady-clock timer and an adaptive \code{times}
rule.

\strong{Speed-up relative to each competitor spans
\code{0.6x} to \code{1628.9x}.} The median across all tested cells
and all competitors is \code{11.3x} on Ubuntu 25.10 and
\code{5.6x} on Windows 11 Pro for Workstations; the mean is
\code{67.8x} and \code{46.6x}, respectively. The largest gaps
appear on wide tables (many value columns, few rows); the smallest
gaps appear on small tables where initialization overhead dominates.

Representative cells (means; numbers in parentheses are the
speed-up of \code{melt()} relative to that competitor) taken from
the Ubuntu 25.10 host:

\itemize{
  \item \strong{1e6 rows, 1 id, 9 value columns} ---
        \code{3.474 ms} vs \code{9.646 ms} (\code{data.table},
        \code{2.8x}), \code{80.014 ms} (\code{tidyr}, \code{23.0x}),
        \code{648.895 ms} (\code{duckdb}, \code{186.8x}).
  \item \strong{1e7 rows, 1 id, 9 value columns} ---
        \code{33.412 ms} vs \code{365.065 ms} (\code{data.table},
        \code{10.9x}), \code{1083.987 ms} (\code{tidyr}, \code{32.4x}),
        \code{6389.564 ms} (\code{duckdb}, \code{191.2x}).
  \item \strong{1e3 rows, 1 id, 10000 value columns} ---
        \code{2.295 ms} vs \code{12.415 ms} (\code{data.table},
        \code{5.4x}), \code{19.408 ms} (\code{polars}, \code{8.5x}),
        \code{3737.866 ms} (\code{dask}, \strong{\code{1628.9x}}).
  \item \strong{1e3 rows, 10 id, 10000 value columns} ---
        \code{25.009 ms} vs \code{71.605 ms} (\code{polars},
        \code{2.9x}), \code{182.019 ms} (\code{data.table},
        \code{7.3x}), \code{23777.051 ms} (\code{dask},
        \strong{\code{950.7x}}).
}

The speed-up comes from three design choices:

\enumerate{
  \item \strong{Two layout paths, selected by \code{major}.} The
        default is column-major. On the column-major path, each output
        column is written as one contiguous \code{memcpy} of the input
        column --- the fastest possible pattern. On the row-major path, tiles of eight
        rows are transposed in registers with
        \code{_mm512_shuffle_f64x2}.
  \item \strong{SIMD streaming stores.} For large outputs,
        \code{_mm512_stream_pd} and \code{_mm256_stream_si256}
        write directly to memory, bypassing the CPU cache and
        avoiding the cache pollution that would otherwise evict
        useful input data. A \code{_mm_sfence()} is issued at the
        end of each streaming region.
  \item \strong{Hugepage hint for large outputs.} Output vectors
        larger than 512 KB are allocated through
        \code{Rf_allocVector()} and then hinted with
        \code{madvise(MADV_HUGEPAGE)}, so the kernel can back
        them with 2 MB pages. This reduces first-touch page
        faults on the 1e8-row case. There is no custom
        allocator and no free pool.
}

Two fast paths for small inputs are used. Tiny inputs
(\code{n <= 2048}, \code{n_meas <= 64}, \code{n_id <= 8}) route
to \code{melt_tiny_cpp}; small inputs
(\code{total <= 131072}, \code{n_meas <= 256}) route to
\code{melt_small_cpp}. Both skip hugepage hinting, OpenMP setup,
and thread-cap detection, which keeps \code{melt()} the fastest
engine even on tables of a few thousand rows.

The output is identical to \code{reshape2::melt},
\code{data.table::melt}, \code{tidyr::pivot_longer},
\code{pandas.melt}, \code{polars.unpivot}, \code{dask}, and
\code{duckdb UNPIVOT} on every tested shape, within
\code{tol = 1e-12}. See \code{vignette("dataprep-melt-dcast")} for
the implementation notes and
\code{vignette("dataprep-performance")} for the full benchmark
tables, including \code{mean}, \code{median}, and the per-competitor
gradient at every tested scale.
}
\value{
A data frame in long format. When \code{na.rm = FALSE}, it has
\code{nrow(data) * length(measure.vars)} rows. With
\code{na.rm = TRUE}, rows whose value is \code{NA} or \code{NaN}
are dropped, so the row count is smaller and not known in advance.
Columns are the ID columns, a column named
\code{variable.name} (a factor when \code{as.factor} resolves to
\code{TRUE}, otherwise a character vector), and a numeric column
named \code{value.name}.
}
\references{
1. Example data is from \url{https://smear.avaa.csc.fi/download}.
2. Eddelbuettel, D. and Francois, R. (2011). Rcpp: Seamless R and
   C++ Integration. \emph{Journal of Statistical Software}, 40(8),
   1--18.
3. Liang, C.-S., Wu, H., Li, H.-Y., Zhang, Q., Li, Z. & He, K.-B.
   (2020). Efficient data preprocessing, episode classification, and
   source apportionment of particle number concentrations.
   \emph{Science of the Total Environment}, 741, 140923.
   \doi{10.1016/j.scitotenv.2020.140923}
}
\author{
Chun-Sheng Liang \email{chun-shengliang@qq.com}
}
\seealso{
\code{\link{dcast}} for the inverse operation.
\code{vignette("dataprep-melt-dcast")} for usage notes and
implementation.
\code{vignette("dataprep-performance")} for benchmark tables.
}
\examples{
## --- Basic usage --------------------------------------------------

df <- data.frame(id = 1:5,
                 category = factor(letters[1:5]),
                 v1 = rnorm(5), v2 = rnorm(5))

# Automatic ID inference: `id` and `category` are non-numeric
melt(df)

# Explicit ID columns
melt(df, id.vars = c("id", "category"))

# Explicit measure columns
melt(df, measure.vars = c("v1", "v2"))


## --- Custom column names and na.rm --------------------------------

df2 <- data.frame(id = 1:3, x = c(1, NA, 3), y = c(4, 5, NA))

melt(df2, id.vars = "id",
     variable.name = "var", value.name = "val",
     na.rm = TRUE)


## --- Two memory layouts ------------------------------------------

# major = NULL (default): column-major (reshape2-compatible)
# major = "col" : column-major, reshape2-compatible
# major = "row" : row-major,    tidyr-compatible
df_wide <- data.frame(id = 1:100,
                      matrix(rnorm(100 * 50), ncol = 50))

res_default <- melt(df_wide, id.vars = "id")
res_col     <- melt(df_wide, id.vars = "id", major = "col")
res_row     <- melt(df_wide, id.vars = "id", major = "row")

# The two layouts produce the same content in a different order
identical(sort(res_col$value), sort(res_row$value))

## --- as.factor controls the variable column type ------------------

# Default: factor for "col", character for "row"
is.factor(res_col$variable)                      # TRUE
is.character(res_row$variable)                   # TRUE

# Explicit override
res_row_fac <- melt(df_wide, id.vars = "id", major = "row",
                    as.factor = TRUE)
is.factor(res_row_fac$variable)                  # TRUE
}
\keyword{manip}
