#' @title to_select_colors
#' @description  To define my color pallette to standardize color use in projects
#' @author Ali Guner
#' @importFrom grDevices colorRampPalette

#' @examples
#' \dontrun{
#' # Example usage
#' to_select_colors("blue")
#' }
#' @export

to_select_colors <- function(...) {

        selected_colors <- c(
                "green" = "#006400",
                "light green" = "#90ee90",
                "teal green" = "#00827f",
                "blue" = "#6BAED6",
                "turquoise" = "#40e0d0",
                "light blue" = "#9ECAE1",
                "dark blue" = "#4292C6",
                "dodger blue" = "#1e90ff",
                "ferrari" = "#ff2800",
                "light red" = "#FC9272",
                "dark red" = "#8b0000",
                "orange" = "#ff8c00",
                "ligth orange" = "#FFC073",
                "coral red" = "#ff4040",
                "gray" = "#808080",
                "light grey" = "#d3d3d3",
                "dark gray" = "#555555",
                # "beige" = "#f5f5dc",
                # "milk" = "#fdfff5",
                # "white" = "#ffffff",
                "black" = "#000000"
                )
        cols <- c(...)

        if (is.null(cols))
                return (selected_colors)

        selected_colors[cols]
}

#' @title karadeniz color pallette
#' @description Multiple color palettes based on the "palette" argument
#' @param palette Choose your favourite palette: "Hidirnebi", "Kadirga", "Kayabasi",  "Cal", "Pokut", "Samistal", "Elevit", "Gito" , "Ovit", "Vazil"  or "Palovit"
#' @param reverse TRUE/FALSE
#' @author Ali Guner
#' @examples
#' \dontrun{
#' karadeniz("Vazil")
#' colors
#' show_col(karadeniz("Vazil")(12))
#'}
#'@export
#'
#'
karadeniz <- function(palette = "main", reverse = FALSE, ...) {

        mypalettes <- list(
                "Hidirnebi" = to_select_colors("ferrari","green", "dodger blue","orange", "gray"),             # to_select_colors(sample(names(selected_colors), 6)),
                "Kadirga" = to_select_colors("turquoise", "ferrari","dodger blue", "coral red"),
                "Kayabasi" = to_select_colors("dark blue", "green","ferrari", "gray"),
                "Cal" = to_select_colors("teal green","orange", "blue", "turquoise","ferrari"),
                "Pokut" = to_select_colors("green", "gray","ark blue","ligth orange", "black"),
                "Samistal" = to_select_colors("light grey", "coral red", "green","dark red"),
                "Elevit" = to_select_colors("ferrari","dodger blue", "teal green", "black"),
                "Gito" = to_select_colors("dark red","dark blue","light grey", "teal green"),
                "Ovit" = to_select_colors("dodger blue", "light blue","light blue", "light grey", "teal green"),
                "Palovit" = to_select_colors("light blue","dark red", "teal green","orange"),
                "Vazil" = to_select_colors("green",  "turquoise", "light blue", "ferrari")
        )
        pal <- mypalettes[[palette]]

        if (reverse) pal <- rev(pal)

        colorRampPalette(pal, bias = 2, ...)
}

#' @title scale_color_karadeniz
#' @description to COLOR function
#' @param palette hoose your favourite palette: "Hidirnebi", "Kadirga", "Kayabasi",  "Cal", "Pokut", "Samistal", "Elevit", "Gito" , "Ovit", "Vazil"  or "Palovit"
#' @param discrete TRUE/FALSE
#' @param reverse TRUE/FALSE
#' @author Ali Guner
#' @examples
#' \dontrun{
#' ggplot(data=iris,mapping=aes(x=Petal.Length, y=Petal.Width, color=Species))+
#' geom_point() + scale_color_karadeniz()
#'}
#'@export
#'
scale_color_karadeniz <- function(palette = "Hidirnebi", discrete = TRUE, reverse = FALSE, ...) {
        pal <- karadeniz(palette = palette, reverse = reverse)

        if (discrete) {
                discrete_scale("colour", paste0("karadeniz_", palette), palette = pal, ...)
        } else {
                scale_color_gradientn(colours = pal(256), ...)
        }
}

#' @title scale_fill_karadeniz
#' @description to FILL function
#' @param palette hoose your favourite palette: "Hidirnebi", "Kadirga", "Kayabasi",  "Cal", "Pokut", "Samistal", "Elevit", "Gito" , "Ovit", "Vazil"  or "Palovit"
#' @param discrete TRUE/FALSE
#' @param reverse TRUE/FALSE
#' @author Ali Guner
#' @examples
#' \dontrun{
#' ggplot(data=iris,mapping=aes(x=Petal.Length,y=Petal.Width, color=Species))+
#' geom_point() + scale_fill_karadeniz()
#'}
#'@export
scale_fill_karadeniz <- function(palette = "Hidirnebi", discrete = TRUE, reverse = FALSE, ...) {
        pal <- karadeniz(palette = palette, reverse = reverse)

        if (discrete) {
                discrete_scale("fill", paste0("karadeniz_", palette), palette = pal, ...)
        } else {
                scale_fill_gradientn(colours = pal(256), ...)
        }
}
