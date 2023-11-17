#' Impute data in a hierarchical fashion
#' 
#' @param x data frame containing both \code{col} and \code{cols}. Will be coerced.
#' @param col a character vector specifying which column contains the values to be imputed
#' @param cols a character vector specfying which columns to use when imputing
#' @param n.min Minimum number of rows to be used for calculating means
#' @return A numerical vector containing the modified version of \code{col}, with length = nrow(x)
#' 
# himpute: hierarchical imputation

# This function allows you to substitute NAs for means in a nested fashion.

# NB: now uses data.table!
himpute <- function(x, col, cols, n.min = 1) {
    
    setDT(x)
    missing <- which(is.na(x[[col]]))
    
    if (!length(missing))
        stop("Warning: no NAs in data, nothing to do!")
    
    if (!is.numeric(x[[col]])) 
        stop("col is not numeric.")
    
    if (is.integer(x[[col]])) 
        x[[col]] <- as.numeric(x[[col]])
    
    
    if (!all(cols %in% names(x))) 
        stop(sprintf("col '%s' not found in data. ", cols[!cols %in% names(x)][1]))
    
    n.cols <- length(cols)
    i.cols <- sapply(n.cols:1, function(n) 1:n)
    x$.col <- x[[col]]
    om <- mean(x$.col, na.rm=T)
    
    # set up imputation table of means per combination
    # this is a list where the first element are all combinations of cols,
    # with consecutive elements containing combinations of cols where more and
    # more single columns are left out, always dropping cols from the right
    means <- lapply(i.cols, function(i) {
        cols <- cols[i]
        x <- x[!missing, .(mean=mean(.col), n = .N), by = cols]
        l <- x$n > n.min & x$mean > 0
        x[l]
    })
    
    for (index in missing) {
        needle <- x[index, ..cols]
        siblings <- sapply(means, function(x) {
            on.cols = names(needle)[1:(ncol(x)-2)]
            x[needle, mean, on=on.cols]
        })
        valids <- sort(which(!is.na(siblings)))
        val <- ifelse(length(valids)>0, siblings[valids[[1]]], om)
        x[index, (col) := ..val]
    }
    x[[col]]
}
