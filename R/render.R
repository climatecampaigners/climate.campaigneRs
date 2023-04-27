#' Render Markdown Report
#'
#' @importFrom dplyr mutate select
#' @importFrom formattable color_bar
#' @importFrom ggmice plot_pattern
#' @importFrom kableExtra group_rows kable kable_styling
#' @importFrom sysfonts font_add_google
#' @importFrom stringr str_wrap
#' @import ggplot2
#' @import leaflet
#' @import openxlsx
#' @import showtext
#' @import tidyr
#' @import DT
#' @param out_dir A character vector of full directory name (default: \code{\link[base:tempdir]{base::tempdir()}}).
#' @param write_excel A logical that indicates if the API response should be written to an Excel file (default: \code{FALSE}).
#' @param echo A logical that indicates if code should be prevented from appearing in the final file (default: \code{FALSE}, no code).
#' @param shiny A logical that indicates if the report should rendered as shiny or not (default: FALSE).
#' @param saveFig A logical that indicates if the plots should be saved as png (default: TRUE).
#' @return No return value. Called for side effects.
#' @export
#'
#' @examples
#' \dontrun{
#' render_report()
#' }
render_report <- function(out_dir = tempdir(), write_excel = FALSE, echo = FALSE, shiny = FALSE,
                          saveFig = TRUE) {
  if (shiny) {
    rmarkdown::run(system.file("extdata", "report.Rmd", package = "climate.campaigneRs"),
                   render_args = list(params = list(shiny = shiny, out_dir = out_dir,
                                                    write_excel = write_excel,
                                                    echo = echo)))
  } else {
    rmarkdown::render(input = system.file("extdata", "report.Rmd", package = "climate.campaigneRs"), #"./inst/extdata/report.Rmd",
                      output_file = file.path(out_dir, paste0("report_", Sys.Date(), ".html")),
                      params = list(write_excel = write_excel,
                                    echo = echo,
                                    out_dir = out_dir,
                                    saveFig = saveFig),
    )

  }


}


