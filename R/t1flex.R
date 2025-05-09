#' Convert a \code{table1} object to \code{flextable}.
#'
#' @param x An object returned by \code{\link{table1}}.
#' @param tablefn Choose a function from the \code{flextable} package to use as
#' the basis for the table.
#' @param ... Further options passed to \code{tablefn}.
#' @return A \code{flextable} object.
#' @note The \code{flextable} package needs to be installed for this to work.
#' @importFrom utils getFromNamespace
#' @export
t1flex <- function(x, tablefn=c("qflextable", "flextable", "regulartable"), ...) {
  if (!requireNamespace("flextable", quietly = TRUE)) {
    stop("This function requires package 'flextable'. Please install it and try again.", call.=F)
  }
  tablefn <- match.arg(tablefn)
  tablefn <- getFromNamespace(tablefn, "flextable")
  obj <- attr(x, "obj", exact=TRUE)
  with(obj, {
    rlh <- if (is.null(rowlabelhead) || rowlabelhead=="") "\U{00A0}" else rowlabelhead
    i <- lapply(contents, function(y) {
      nrow(y)
    })
    i <- cumsum(c(1, i[-length(i)]))
    z <- lapply(contents, function(y) {
      y <- as.data.frame(y, stringsAsFactors=F)
      y2 <- data.frame(x=paste0(c("", rep("\U{00A0}\U{00A0}", nrow(y) - 1)), rownames(y)), stringsAsFactors=F)
      y <- cbind(setNames(y2, rlh), y)
      y
    })
    df <- do.call(rbind, z)

    header_df <- data.frame(
      labels = c(rlh, headings),
      keys   = LETTERS[1:ncol(df)]
    )

    if (!is.null(groupspan)) {
      zzz <- ncol(df) - sum(groupspan) - 1
      label2 <- c("", rep(labels$groups, times=groupspan), rep("", zzz))
      header_df <- cbind(data.frame(label2=label2), header_df)
    }

    colnames(df) <- header_df$keys
    rownames(df) <- NULL

    out <- tablefn(df, ...)
    out <- flextable::set_header_df(out,  header_df, key="keys")
    out <- flextable::merge_h(out, part = "header", i = 1)
    #out <- flextable::merge_v(out, part = "header", j = 1)
    #out <- flextable::theme_booktabs(out, bold_header = TRUE)
    out <- flextable::hline_top(out, border = officer::fp_border(width=1.5), part = "header")
    out <- flextable::hline_bottom(out, border = officer::fp_border(width=1.5), part = "header")
    out <- flextable::align(out, j=2:(ncolumns+1), align="center", part="body")
    out <- flextable::align(out, j=2:(ncolumns+1), align="center", part="header")
    out <- flextable::bold(out, part="header")
    out <- flextable::bold(out, i=i, j=1)

    if (!is.null(caption)) {
      out <- flextable::set_caption(out, caption=caption)
    }
    if (!is.null(footnote)) {
      out <- flextable::add_footer_lines(out, values=footnote)
    }
    out
  })
}
