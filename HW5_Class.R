## HW5 Class/Methods

setClass(
    Class = "sparse_numeric",
    slots = c(
        value = "numeric",
        pos = "integer",
        length = "integer"
    )
)

## Validity method
setValidity("sparse_numeric", function(object) {
  msgs <- character()

  if (!(is.integer(object@length) && length(object@length) == 1L && !is.na(object@length))) {
    msgs <- c(msgs, "'length' must be a single integer")
  } else if (object@length < 0L) {
    msgs <- c(msgs, "'length' must be non-negative")
  }

  if (length(object@value) != length(object@pos)) {
    msgs <- c(msgs, "'value' and 'pos' must have the same length")
  }

  if (!is.integer(object@pos)) {
    msgs <- c(msgs, "'pos' must be an integer vector")
  } else if (length(object@pos) > 0L) {
    if (any(is.na(object@pos))) msgs <- c(msgs, "'pos' contains NA")
    if (any(object@pos < 1L)) msgs <- c(msgs, "all 'pos' entries must be >= 1")
    if (any(object@pos > as.integer(object@length))) msgs <- c(msgs, "'pos' entries cannot exceed 'length'")
    if (any(duplicated(object@pos))) msgs <- c(msgs, "'pos' must not contain duplicates")
  }

  if (length(object@value) > 0L) {
    if (any(is.na(object@value))) msgs <- c(msgs, "'value' contains NA")
    if (any(object@value == 0)) msgs <- c(msgs, "'value' must not contain zeros")
  }

  if (length(msgs) == 0L) TRUE else msgs
})


## Coercion methods
setAs("numeric", "sparse_numeric", function(from) {
  n <- length(from)
  nz_idx <- which(from != 0)
  new("sparse_numeric",
      value = if (length(nz_idx) > 0) as.numeric(from[nz_idx]) else numeric(0),
      pos = if (length(nz_idx) > 0) as.integer(nz_idx) else integer(0),
      length = as.integer(n))
})

setAs("sparse_numeric", "numeric", function(from) {
  n <- as.integer(from@length)
  out <- numeric(n)
  if (length(from@pos) > 0L) out[from@pos] <- from@value
  out
})


## ------------------------------
## Generics
## ------------------------------
setGeneric("sparse_add", function(x, y, ...) standardGeneric("sparse_add"))
setGeneric("sparse_mult", function(x, y, ...) standardGeneric("sparse_mult"))
setGeneric("sparse_sub", function(x, y, ...) standardGeneric("sparse_sub"))
setGeneric("sparse_crossprod", function(x, y, ...) standardGeneric("sparse_crossprod"))
setGeneric("sparse_mean", function(x, y, ...) standardGeneric("sparse_mean"))


## Arithmetic Methods
setMethod("sparse_add", signature(x = "sparse_numeric", y = "sparse_numeric"),
          function(x, y, ...) {
            if (x@length != y@length) stop("vectors must have same length")
            if (length(x@pos) == 0L) return(y)
            if (length(y@pos) == 0L) return(x)

            allpos <- sort(unique(c(x@pos, y@pos)))
            vals <- numeric(length(allpos))
            vals[match(x@pos, allpos)] <- vals[match(x@pos, allpos)] + x@value
            vals[match(y@pos, allpos)] <- vals[match(y@pos, allpos)] + y@value

            keep <- vals != 0
            new("sparse_numeric",
                value = vals[keep],
                pos = as.integer(allpos[keep]),
                length = as.integer(x@length))
          })

setMethod("sparse_sub", signature(x = "sparse_numeric", y = "sparse_numeric"),
          function(x, y, ...) {
            if (x@length != y@length) stop("vectors must have same length")

            allpos <- sort(unique(c(x@pos, y@pos)))
            vals <- numeric(length(allpos))
            vals[match(x@pos, allpos)] <- vals[match(x@pos, allpos)] + x@value
            vals[match(y@pos, allpos)] <- vals[match(y@pos, allpos)] - y@value

            keep <- vals != 0
            new("sparse_numeric",
                value = vals[keep],
                pos = as.integer(allpos[keep]),
                length = as.integer(x@length))
          })

setMethod("sparse_mult", signature(x = "sparse_numeric", y = "sparse_numeric"),
          function(x, y, ...) {
            if (x@length != y@length) stop("vectors must have same length")
            if (length(x@pos) == 0L || length(y@pos) == 0L)
              return(new("sparse_numeric", value = numeric(0), pos = integer(0), length = x@length))
            common <- intersect(x@pos, y@pos)
            if (length(common) == 0L)
              return(new("sparse_numeric", value = numeric(0), pos = integer(0), length = x@length))
            ix <- match(common, x@pos)
            iy <- match(common, y@pos)
            prod_vals <- x@value[ix] * y@value[iy]
            keep <- prod_vals != 0
            new("sparse_numeric",
                value = prod_vals[keep],
                pos = as.integer(common[keep]),
                length = as.integer(x@length))
          })

setMethod("sparse_crossprod", signature(x = "sparse_numeric", y = "sparse_numeric"),
          function(x, y, ...) {
            if (x@length != y@length) stop("vectors must have same length")
            if (length(x@pos) == 0L || length(y@pos) == 0L) return(0)
            common <- intersect(x@pos, y@pos)
            if (length(common) == 0L) return(0)
            ix <- match(common, x@pos)
            iy <- match(common, y@pos)
            sum(x@value[ix] * y@value[iy])
          })


setMethod("sparse_mean", signature(x = "sparse_numeric", y = "missing"),
          function(x, ...) {
            if (x@length == 0L) return(NA_real_)
            sum(x@value) / as.numeric(x@length)
          })

setMethod("sparse_mean", signature(x = "sparse_numeric", y = "sparse_numeric"),
          function(x, y, ...) {
            if (x@length != y@length) stop("vectors must have same length")
            diff_vals <- sparse_sub(x, y)
            sum(diff_vals@value) / as.numeric(diff_vals@length)
          })


## Operator Linking
setMethod("+", signature(e1 = "sparse_numeric", e2 = "sparse_numeric"),
          function(e1, e2) sparse_add(e1, e2))
setMethod("-", signature(e1 = "sparse_numeric", e2 = "sparse_numeric"),
          function(e1, e2) sparse_sub(e1, e2))
setMethod("*", signature(e1 = "sparse_numeric", e2 = "sparse_numeric"),
          function(e1, e2) sparse_mult(e1, e2))


## show method
setMethod("show", "sparse_numeric", function(object) {
  cat("<sparse_numeric> of length", as.integer(object@length), "\n")
  nnz <- length(object@pos)
  cat("  Non-zero count:", nnz, "\n")

  if (nnz == 0L) {
    cat("  (All entries are zero)\n")
  } else {
    cat("  Sample non-zero entries:\n")
    n_show <- min(6L, nnz)
    df <- data.frame(Position = object@pos[seq_len(n_show)],
                     Value = object@value[seq_len(n_show)])
    print(df, row.names = FALSE)
    if (nnz > n_show)
      cat("  ... and", nnz - n_show, "more entries not shown\n")
  }

  cat("  Mean (sparse):", round(sparse_mean(object), 4), "\n")
  invisible(NULL)
})


## plot method
setMethod("plot", signature(x = "sparse_numeric", y = "sparse_numeric"),
          function(x, y, xlab = "Position", ylab = "Value",
                   main = "Sparse Vector Comparison", ...) {
            if (x@length != y@length) stop("vectors must have same length")

            max_len <- max(x@length, y@length)
            ylim_range <- range(c(x@value, y@value), finite = TRUE)
            if (length(ylim_range) == 0L) ylim_range <- c(-1, 1)

            plot(NA, xlim = c(1, max_len), ylim = ylim_range,
                 xlab = xlab, ylab = ylab, main = main, ...)

            if (length(x@pos) > 0L)
              points(x@pos, x@value, col = "steelblue", pch = 16, cex = 1.2)
            if (length(y@pos) > 0L)
              points(y@pos, y@value, col = "firebrick", pch = 17, cex = 1.2)

            overlap <- intersect(x@pos, y@pos)
            if (length(overlap) > 0L) {
              ix <- match(overlap, x@pos)
              iy <- match(overlap, y@pos)
              segments(overlap, x@value[ix], overlap, y@value[iy],
                       col = "darkgray", lty = 2)
            }

            legend("topright",
                   legend = c("x non-zero", "y non-zero", "overlap"),
                   col = c("steelblue", "firebrick", "darkgray"),
                   pch = c(16, 17, NA), lty = c(NA, NA, 2), bty = "n")
            invisible(NULL)
          })
