
#' Bar chart for categorical variable
#' @description Computes the proportions for a given categorical variable \code{x}.
#' E.g. used for socio-demographic variables such as age, gender, etc. from the
#' \code{data} API.
#'
#'
#' @param x A categorical variable.
#' @param useNA A logical that indicates if NA should be included.
#' @param textsize Text size in pts.
#' @param labelsize Annotation text size in pts.
#' @param title Text for the title.
#' @return A ggplot object.
#' @export
#'
#' @examples
#' \dontrun{
#' data <- get_data()
#' barchart(data$gender, useNA = FALSE)
#' }



barchart <- function(x, useNA = FALSE, textsize = 6, labelsize = 2, title = "") {
  Freq <- Var <- NULL
  sysfonts::font_add_google("Poppins", family = "poppins")

  if (useNA) {
    tab <- prop.table(table(x, useNA = "always"))
  } else {
    tab <- prop.table(table(x))
  }
  tab <- as.data.frame(tab)
  label <- paste0(round(tab$Freq * 100, 1), "%")
  names(tab) <- c("Var", "Freq")

  ggplot(tab, aes(x = Var, y = Freq)) +
    geom_bar(stat = "identity", fill = "#69a341") +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0, max(tab$Freq) * 1.3)) +
    scale_x_discrete(limits = rev(levels(tab$Var)), labels = function(x) stringr::str_wrap(x, width = 20)) +
    ggtitle(title) +
    ylab("") +
    xlab("") +
    theme_classic() +
    geom_text(aes(label = label), hjust = -0.1, size = labelsize, family = "poppins") +
    theme(text = element_text(size = textsize, family = "poppins"),
          plot.title = element_text(size = textsize, vjust = 3)) +
    coord_flip()
}
