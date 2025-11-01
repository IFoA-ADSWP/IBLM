#' Custom ggplot2 Theme for IBLM
#'
#' @return A ggplot2 theme object that can be added to plots.
#'
#' @import ggplot2
#'
#' @export
theme_iblm <- function() {

  theme_minimal() +
    theme(
      plot.title = element_text(color = iblm_colors[5], face = "bold", size = 14),
      plot.subtitle = element_text(color = iblm_colors[2], size = 12),
      panel.grid.major = element_line(color = iblm_colors[4], linewidth = 0.3),
      panel.grid.minor = element_line(color = iblm_colors[4], linewidth = 0.2)
    )
}


iblm_colors <- c("#113458", "#D9AB16", "#4096C0", "#DCDCD9", "#113458", "#2166AC", "#FFFFFF", "#B2182B")
