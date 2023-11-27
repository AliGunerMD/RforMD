#' @title AG Theme Customization
#' @author Ali Guner
#' @description
#' The \code{ag_theme()} function defines a custom theme for ggplot2 plots with
#' specific styling preferences. This theme enhances the visual appeal and
#' consistency of your plots by customizing various elements.
#'
#' @details
#' The theme includes the following customizations:
#' - Light background with minimal grid lines (\code{theme_light()}).
#' - Custom font family (\code{Helvetica}) and size for all text elements.
#' - Legends positioned at the top of the plot.
#' - Adjusted sizes for legend text, axis text, and strip text.
#' - Gray background for facet strips.
#' - Increased panel spacing and removed panel borders.
#' - Bold and centrally aligned plot title, and centered plot subtitle.
#'
#' @import ggplot2
#' @import magrittr
#' @return
#' The function returns a ggplot2 theme object that can be applied to ggplot objects
#' to achieve the specified custom styling.
#'
#' @examples
#' \dontrun{
#' # Example usage
#' library(ggplot2)
#' my_plot <- ggplot(data = my_data, aes(x = x_variable, y = y_variable)) +
#'   geom_point() +
#'   ag_theme()
#' print(my_plot)
#' }
#'
#' @seealso
#' \code{\link[ggplot2]{theme_light}}, \code{\link[ggplot2]{element_text}},
#' \code{\link[ggplot2]{element_rect}}, \code{\link[ggplot2]{unit}}
#'
#' @keywords plotting visualization theme customization aesthetics
#'
#' @export
#'
#'


ag_theme <- function(){

my_plot_size <- 12
my_font <- "Helvetica"

        ggplot2::theme_light(base_family = my_font) %+replace%


        ggplot2::theme(
                text = element_text(size = my_plot_size),
                legend.position = "top",
                legend.text = element_text(size = my_plot_size),
                axis.text = element_text(size = my_plot_size),
                strip.background = element_rect(fill = my_grays[2]),
                strip.text = element_text(color = "black", size = my_plot_size),
                panel.spacing = unit(1.2, "lines"),
                panel.border = element_blank(),
                plot.title = element_text(face = "bold", size = 16, hjust = .5, vjust = 2),
                plot.subtitle = element_text(hjust = .5, size = 14))

}
