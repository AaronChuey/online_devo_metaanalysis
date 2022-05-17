# draw prisma diagram this code is modified from the PRISMAstatement library

#library(DiagrammeR)

# https://cran.r-project.org/web/packages/PRISMAstatement/vignettes/PRISMA.html

paren <- function(n)
  paste0("(N = ", as.character(n), ")")

pnl <- function(...)
  paste(..., sep = "\n")

#' Make PDF of the plot
#'
#' This makes a PDF file which can be included by knitr Sweave.
#' @param x output of call to \code{prisma}
#' @param filename path of output file
#' @importFrom utils capture.output
#' @examples
#' \dontrun{
#' g <- prisma(9, 8, 7, 6, 5, 4, 3, 2, 1)
#' prisma_pdf(g, "test.pdf")
#' knitr::include_graphics("test.pdf")
#' }
#' @keywords internal
prisma_pdf <- function(x, filename = "prisma.pdf") {
  if (!requireNamespace("DiagrammeRsvg", quietly = TRUE) ||
      !requireNamespace("rsvg", quietly = TRUE)) {
    stop("DiagrammeRsvg and rsvg are both required for this prisma_pdf")
  }
  utils::capture.output({
    rsvg::rsvg_pdf(svg = charToRaw(DiagrammeRsvg::export_svg(x)),
                   file = filename)
  })
  invisible()
}

prisma2 <- function(found,
                    found_other,
                    no_dupes,
                    screened,
                    screen_exclusions,
                    full_text,
                    full_text_exclusions,
                    quantitative,
                    font_size,
                    dpi) {
  DiagrammeR::grViz(
    prisma_graph2(found = found,
                  found_other = found_other,
                  no_dupes = no_dupes,
                  screened = screened,
                  screen_exclusions = screen_exclusions,
                  full_text = full_text,
                  full_text_exclusions = full_text_exclusions,
                  quantitative = quantitative,
                  font_size = font_size,
                  dpi = dpi)
    )
}

prisma_graph2 <- function (found, found_other, no_dupes, screened, screen_exclusions,
          full_text, full_text_exclusions, quantitative, font_size, dpi){

  labels <- list(
    found = pnl("Records identified through",
                "database searching",
                paren(found)),
    found_other = pnl("Additional records identified",
                      "through other sources",
                      paren(found_other)),
    no_dupes = pnl("Records after duplicates removed", paren(no_dupes)),
    screened = pnl("Records screened", paren(screened)),
    screen_exclusions = pnl("Records excluded,",  "with reasons", paren(screen_exclusions)),
    full_text = pnl("Full-text articles assessed",
                    "for eligibility",
                    paren(full_text)),
    full_text_exclusions =
      pnl("Full-text articles excluded,",
          "with reasons",
          paren(full_text_exclusions)),
    quantitative = pnl("Studies included in",
                       "meta-analysis",
                       paren(quantitative))
  )


  dot_template <- 'digraph prisma {
    node [shape="box", fontsize = %d];
    graph [dpi = %d]
    a
    b
    a -> incex;
    b -> incex;
    a [label="%s"]
    b [label="%s"]
    incex [label="%s"];

    incex -> dup;
    dup [label="%s"];
    dup -> {ex; temp}

    temp [label="%s"];
    ex [label="%s"];
    {rank=same; dup temp}
    {rank=same; ex ex2}
    ex2 [label="%s"];
    ex -> {ex2, ex3}
    ex3 [label="%s"];

  }'

  sprintf(dot_template,
          font_size,
          dpi,
          labels$found,
          labels$found_other,
          labels$no_dupes,
          labels$screened,
          labels$screen_exclusions,
          labels$full_text,
          labels$full_text_exclusions,
          labels$quantitative)
}

