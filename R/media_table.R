#' Create a media cell
#'
#' Creates a media-cell object for use with [media_table()]. A media cell
#' represents an image that can be placed alongside Markdown text or other
#' images in a rectangular layout.
#'
#' The object itself does not render anything. Rendering is handled by
#' [media_table()] according to the active output format.
#'
#' @param src Character scalar. Path or URL of the image.
#' @param width Optional character scalar giving the rendered image width.
#'   Percentage values such as `"100%"` and `"80%"` are supported in both
#'   HTML and LaTeX/PDF output. Other values are passed through to the
#'   relevant output format.
#' @param alt Character scalar. Alternative text for the image.
#'
#' @return An object of class `"media_cell"`.
#'
#' @examples
#' media("images/photo.jpg")
#'
#' media(
#'   "images/photo.jpg",
#'   width = "80%",
#'   alt = "Photograph of a farmhouse"
#' )
#'
#' @export
media <- function(src, width = NULL, alt = "") {
  structure(
    list(
      src = src,
      width = width,
      alt = alt
    ),
    class = "media_cell"
  )
}


#' Create a media and text layout for Quarto
#'
#' Creates a rectangular, headerless layout containing images and Markdown
#' text. The function is intended for visual placement rather than for
#' displaying tabular data.
#'
#' Image cells are created with [media()]. All other cells are interpreted
#' as text.
#'
#' For HTML output, text cells are passed to Quarto as Markdown using
#' embedded `data-qmd` content. For LaTeX/PDF output, Markdown is passed to
#' Quarto using `\\QuartoMarkdownBase64{}`. This allows ordinary Markdown
#' formatting, such as emphasis and inline code, to be retained across
#' output formats.
#'
#' The generated layout has no column headers. Captions, labels and
#' cross-references should be supplied by Quarto around the resulting
#' layout rather than by `media_table()` itself.
#'
#' @param x A matrix containing [media()] objects and/or character values.
#'   The matrix geometry determines the geometry of the resulting layout.
#' @param widths Optional numeric vector containing one relative width for
#'   each column. Values are normalised, so `c(50, 50)`, `c(1, 1)` and
#'   `c(0.5, 0.5)` are equivalent. If `NULL`, columns receive equal widths.
#' @param image_width Character scalar giving the default width of images
#'   within their cells. Defaults to `"100%"`. A width specified directly
#'   in [media()] overrides this value.
#' @param valign Character scalar giving vertical alignment for HTML cells.
#'   Defaults to `"middle"`. In LaTeX/PDF output, cells use vertically
#'   centred `m` columns.
#'
#' @return A `knitr_kable` object appropriate to the current output format.
#'
#' @details
#' HTML and LaTeX/PDF output are rendered with `knitr` and `kableExtra`.
#' Other output formats fall back to a simple pipe table.
#'
#' The LaTeX implementation currently uses a total table width of 15 cm.
#' Column widths are allocated proportionally within that width.
#'
#' Quarto citations are not supported inside text passed through
#' `\\QuartoMarkdownBase64{}`. Ordinary Markdown formatting is supported.
#'
#' @examples
#' \dontrun{
#' media_table(
#'   matrix(
#'     list(
#'       media("images/farmhouse.jpg"),
#'       paste(
#'         "We created five images of the",
#'         "*farmhouse* from two directions."
#'       )
#'     ),
#'     nrow = 1
#'   ),
#'   widths = c(50, 50)
#' )
#'
#' media_table(
#'   matrix(
#'     list(
#'       media("images/image1.jpg"),
#'       media("images/image2.jpg"),
#'       "Some *formatted* explanatory text.",
#'       media("images/image3.jpg")
#'     ),
#'     nrow = 2,
#'     byrow = TRUE
#'   ),
#'   widths = c(50, 50)
#' )
#' }
#'
#' @importFrom base64enc base64encode
#' @importFrom htmltools htmlEscape
#' @importFrom kableExtra column_spec kable_styling
#' @importFrom knitr is_html_output is_latex_output kable
#'
#' @export
media_table <- function(
    x,
    widths = NULL,
    image_width = "100%",
    valign = "middle"
) {

  x <- as.matrix(x)

  nr <- nrow(x)
  nc <- ncol(x)

  if (is.null(widths)) {
    widths <- rep(1 / nc, nc)
  }

  if (length(widths) != nc) {
    stop("`widths` must contain one value for each column.")
  }

  # Allow c(50, 50), c(1, 1), c(.5, .5), etc.
  widths <- widths / sum(widths)


  # ==========================================================
  # HTML
  # ==========================================================

  if (knitr::is_html_output()) {

    cells <- matrix("", nrow = nr, ncol = nc)

    for (i in seq_len(nr)) {
      for (j in seq_len(nc)) {

        z <- x[[i, j]]

        if (inherits(z, "media_cell")) {

          w <- z$width %||% image_width

          cells[i, j] <- sprintf(
            paste0(
              '<img src="%s" alt="%s" ',
              'style="width:%s;',
              'height:auto;',
              'display:block;',
              'margin-left:auto;',
              'margin-right:auto;">'
            ),
            z$src,
            z$alt,
            w
          )

        } else {

          cells[i, j] <- sprintf(
            '<span data-qmd="%s"></span>',
            htmltools::htmlEscape(
              as.character(z),
              attribute = TRUE
            )
          )
        }
      }
    }

    dimnames(cells) <- NULL

    out <- knitr::kable(
      cells,
      format = "html",
      escape = FALSE,
      align = rep("c", nc),
      col.names = NA
    )

    out <- kableExtra::kable_styling(
      out,
      full_width = TRUE,
      position = "center"
    )

    for (j in seq_len(nc)) {

      out <- kableExtra::column_spec(
        out,
        j,
        width = paste0(
          round(widths[j] * 100, 4),
          "%"
        ),
        extra_css = paste0(
          "vertical-align:",
          valign,
          ";"
        )
      )
    }

    return(out)
  }


  # ==========================================================
  # PDF / LATEX
  # ==========================================================

  if (knitr::is_latex_output()) {

    cells <- matrix("", nrow = nr, ncol = nc)

    for (i in seq_len(nr)) {
      for (j in seq_len(nc)) {

        z <- x[[i, j]]

        if (inherits(z, "media_cell")) {

          w <- z$width %||% image_width

          if (grepl("%$", w)) {

            p <- as.numeric(
              sub("%$", "", w)
            ) / 100

            latex_image_width <- paste0(
              p,
              "\\linewidth"
            )

          } else {

            latex_image_width <- w
          }

          cells[i, j] <- sprintf(
            paste0(
              "\\includegraphics[",
              "width=%s,",
              "keepaspectratio",
              "]{%s}"
            ),
            latex_image_width,
            z$src
          )

        } else {

          cells[i, j] <- sprintf(
            "\\QuartoMarkdownBase64{%s}",
            qmd_base64(
              as.character(z)
            )
          )
        }
      }
    }

    dimnames(cells) <- NULL

    # Conservative width for the current letter/KOMA layout.
    total_width_cm <- 15

    out <- knitr::kable(
      cells,
      format = "latex",
      booktabs = TRUE,
      escape = FALSE,
      align = rep("c", nc),
      col.names = NA
    )

    for (j in seq_len(nc)) {

      out <- kableExtra::column_spec(
        out,
        j,
        width = paste0(
          round(
            total_width_cm * widths[j],
            3
          ),
          "cm"
        ),
        latex_valign = "m"
      )
    }

    return(out)
  }


  # ==========================================================
  # OTHER OUTPUTS
  # ==========================================================

  cells <- matrix("", nrow = nr, ncol = nc)

  for (i in seq_len(nr)) {
    for (j in seq_len(nc)) {

      z <- x[[i, j]]

      if (inherits(z, "media_cell")) {

        cells[i, j] <- sprintf(
          "![%s](%s)",
          z$alt,
          z$src
        )

      } else {

        cells[i, j] <- as.character(z)
      }
    }
  }

  dimnames(cells) <- NULL

  knitr::kable(
    cells,
    format = "pipe",
    align = rep("c", nc),
    col.names = NA
  )
}


# Internal helpers --------------------------------------------------------

`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}


#' Encode Quarto Markdown as Base64
#'
#' Internal helper used to embed Markdown in raw LaTeX tables for Quarto.
#'
#' @param x Character scalar containing Quarto Markdown.
#'
#' @return A Base64-encoded character string.
#'
#' @keywords internal
#'
#' @importFrom base64enc base64encode
qmd_base64 <- function(x) {
  base64enc::base64encode(
    charToRaw(enc2utf8(x))
  )
}
