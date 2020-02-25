#' Cut a continuous variable into equal-sized groups.
#'
#' @param x A numeric vector.
#' @param ngroups The number of groups desired.
#' @param labeling A function that produces the category labels (see Details).
#' @param withhold A named list of logical vectors (see Details).
#' @param varlabel A character string to be used as a label for \code{x}, or
#' \code{NULL}.
#' @param quantile.type An integer from 1 to 9, passed as the \code{type} argument to function \code{\link[stats]{quantile}}.
#' @param right Should intervals be right-closed? (passed to \code{\link[base]{cut}}).
#' @param ... Further arguments passed on to function \code{labeling}.
#' @param xcat A factor returned by \code{\link[base]{cut}}.
#' @param which,what Character vectors for labeling the categories in an
#' appropriate way (see Examples).
#' @param from,to Numeric vectors giving the ranges covered by the categories
#' of \code{x}.
#'
#' @return A \code{factor} of the same length as \code{x}. There are
#' \code{ngroups} levels plus one additional level for each element of
#' \code{withhold}.
#'
#' @details
#' The function \code{labeling} must have the signature \code{function(x, xcat,
#' which, what, from, to, ...)} and produces the character vector of factor
#' levels. See below for an example.
#' 
#' The \code{withhold} list can be used when \code{x} contains special values
#' that should not be considered in the calculation of the quantiles used to
#' create the \code{ngroups} categories. The special values are given a label
#' that corresponds to the name of the corresponding list element. See below
#' for an example.
#' 
#' @seealso
#' \code{\link[base]{cut}}
#' \code{\link[stats]{quantile}}
#'
#' @examples
#' x <- sample(100)
#' table(eqcut(x, 2))
#' table(eqcut(x, 3))
#' table(eqcut(x, 4))
#' table(eqcut(x, 5))
#' table(eqcut(x, 6))
#' table(eqcut(x, 7))
#' table(eqcut(x, 8))
#' 
#' # An example of using eqcut in a table with custom labeling function.
#' dat <- expand.grid(id=1:100, sex=c("Male", "Female"), treat=c("Treated", "Placebo"))
#' dat$age <- runif(nrow(dat), 18, 50)
#' dat$wt <- exp(rnorm(nrow(dat), log(75 + 10*(dat$sex=="Male")), 0.2))
#' dat$auc <- ifelse(dat$treat=="Placebo", NA, exp(rnorm(nrow(dat), log(1000), 0.34)))
#' dat$auc[3] <- NA  # Add a missing value
#' 
#' label(dat$sex) <- "Sex"
#' label(dat$age) <- "Age"
#' label(dat$wt)  <- "Weight"
#' label(dat$auc) <- "AUC"
#' units(dat$age) <- "y"
#' units(dat$wt)  <- "kg"
#' units(dat$auc) <- "ng.h/mL"
#'
#' w <- list(Placebo=(dat$treat=="Placebo"), Excluded=is.na(dat$auc))
#' f <- function(x, xcat, which, what, from, to, ...) {
#'    what <- sub("of ", "of<br/>", what)
#'    sprintf("%s %s<br/>&ge;%s to &lt;%s",
#'        which, what, signif_pad(from, 3, FALSE), signif_pad(to, 3, FALSE))
#' }
#' table1(~ sex + age + wt | eqcut(auc, 3, f, w), data=dat)
#'
#' @keywords utilities
#' @export
eqcut <- function(x, ngroups, labeling=eqcut.default.labeling, withhold=NULL, varlabel=if (has.label(x)) label(x) else deparse(substitute(x)), quantile.type=7, right=FALSE, ...) {
    v <- varlabel
    if (!is.null(withhold)) {
        if (!is.list(withhold) || is.null(names(withhold)) || !(all(sapply(withhold, is.logical)))) {
            stop("withhold must be a named list of logicals")
        }
        for (i in seq_len(length(withhold))) {
            x[withhold[[i]]] <- NA
        }
    }
    is.wholenumber <- function(x, tol = .Machine$double.eps^0.5) {
        abs(x - round(x)) < tol
    }
    if (!is.numeric(ngroups)) stop("ngroups must be a single integer value")
    if (length(ngroups) != 1) stop("ngroups must be a single integer value")
    if (!is.wholenumber(ngroups)) stop("ngroups must be a single integer value")
    if (ngroups < 2) stop("ngroups must be at least 2")
    if (!is.numeric(x)) stop("x must be numeric")
    if (length(x) == 0) stop("x is empty")
    if (sum(!is.na(x)) == 0) stop("x has no non-missing values")
    if (sum(!is.na(x)) < ngroups) stop(paste0("Can't form ", ngroups, " groups because there are only ", sum(!is.na(x)), " non-missing values"))
    if (length(unique(x[!is.na(x)])) < ngroups) stop(paste0("Can't form ", ngroups, " groups because there are only ", length(unique(x[!is.na(x)])), " unique non-missing values"))

    q <- quantile(x, probs=seq.int(ngroups-1)/ngroups, na.rm=TRUE, type=quantile.type)
    breaks <- c(min(x, na.rm=T), q, max(x, na.rm=T)) 
    if (max(table(breaks)) > 1) {
        tb <- table(x)
        stop(paste0("Because of duplicate values, it's not possible to form ", ngroups, " equal-sized groups. One value occurs ", max(tb), " times; consider using `withhold` to create a separate group for it."))
    }
    xcat <- cut(x, breaks=breaks, right=right, include.lowest=T)
    if (diff(range(tb <- table(xcat))) > 1) {
        warning(paste0("Because of duplicate values, it's not possible to form ", ngroups, " equal-sized groups. Largest group size: ", max(tb), "; smallest group size: ", min(tb)))
    }

    from <- c(min(x, na.rm=T), q)
    to <- c(q, max(x, na.rm=T))

    if (ngroups == 2) {
        which <- c("Below", "Above")
        what <- "median"
        if (!is.null(v)) {
            what <- paste(what, v)
        }
    } else {
        which <- paste0(1:ngroups, c("st", "nd", "rd", rep("th", ngroups - 3)))
        what <- switch(as.character(ngroups), 
               "3" = "tertile",
               "4" = "quartile",
               "5" = "quintile",
               "6" = "sextile",
               "7" = "septile",
               "8" = "octile",
               "10" = "decile",
               "16" = "hexadecile",
               "100" = "percentile",
               paste0(ngroups, "-tile"))
        if (!is.null(v)) {
            what <- paste(what, "of", v)
        }
    }
    if (!is.null(units(x))) {
        what <- paste(what, paste0("(", units(x), ")"))
    }
    lab <- labeling(x=x, xcat=xcat, which=which, what=what, from=from, to=to, right=right, ...)
    levels(xcat) <- lab
    if (!is.null(withhold)) {
        xcat <- factor(xcat, levels=c(levels(xcat), names(withhold)))
        for (i in rev(seq_len(length(withhold)))) {
            xcat[withhold[[i]]] <- names(withhold)[i]
        }
    }
    xcat
}

#' @describeIn eqcut The default labeling function.
#' @export
eqcut.default.labeling <- function(x, xcat, which, what, from, to, ...) {
    int <- levels(xcat)
    left <- substring(int, 1, 1)
    right <- substring(int, nchar(int))
    sprintf("%s %s: %s%s,%s%s",
        which, what, left, signif_pad(from, 3, FALSE), signif_pad(to, 3, FALSE), right)
}

