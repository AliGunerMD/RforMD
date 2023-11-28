#' @title to_select_colors
#' @description  To define my color pallette to standardize color use in projects
#' @author Ali Guner
#' @importFrom grDevices colorRampPalette

#' @examples
#' \dontrun{
#' # Example usage
#' to_select_colors("blue")
#' }


to_select_colors <- function(...) {

        selected_colors <- c(
                        "Lime" = "#00FF00",
                        "Forest Green" = "#228B22",
                        "Dark Green" = "#006400",
                        "Spring Green" = "#00FF7F",
                        "Sea Green" = "#2E8B57",
                        "Amazon" = "#3b7a57",
                        "Mint Green" = "#98FF98",
                        "Pistachio" = "#93C572",
                        "Hunter Green" = "#355E3B",
                        "Grass Green" = "#7CFC00",
                        "Dollar Bill" = "#85BB65",
                        "Camouflage Green" = "#78866B",
                        "Pear" = "#D1E231",
                        "Heineken Green" = "#008200",
                        "Pine Green" = "#01796F",
                        "Dark Olive Green" = "#556B2F",
                        "Light Sky Blue" = "#87CEFA",
                        "Dodger Blue" = "#1E90FF",
                        "Dark Blue" = "#00008B",
                        "Sky Blue" = "#87CEEB",
                        "Azure" = "#007FFF",
                        "Sapphire" = "#0F52BA",
                        "Deep Sky Blue" = "#00BFFF",
                        "Indigo" = "#3F00FF",
                        "Navy" = "#000080",
                        "Vivid Sky Blue" = "#00CCFF",
                        "Facebook" = "#1877F2",
                        "IBM" = "#0530AD",
                        "Twitter" = "#1DA1F2",
                        "Argentinan" = "#6CB4EE",
                        "Turquoise" = "#40E0D0",
                        "Youtube" = "#FF0000",
                        "Liverpool" = "#C8102E",
                        "Maroon" = "#800000",
                        "Tomato" = "#FF6347",
                        "Crimson" = "#DC143C",
                        "Carmine" = "#960018",
                        "Ferrari" = "#FF2800",
                        "Cadmium Red" = "#E30022",
                        "Dark Scarlet" = "#560319",
                        "Dark Orange" = "#FF8C00",
                        "Pumpkin" = "#FF7518",
                        "Burnt Orange" = "#CC5500",
                        "Orange" = "#FFA500",
                        "Persimmon" = "#EC5800",
                        "Sinopia" = "#CB410B",
                        "Peru" = "#CD853F",
                        "Chocolate" = "#D2691E",
                        "Saddle Brown" = "#8B4513",
                        "Deer" = "#BA8759",
                        "Bronze" = "#CD7F32",
                        "Sepia" = "#704214",
                        "Orchid" = "#DA70D6",
                        "Dark Orchid" = "#9932CC",
                        "Deep Fuchsia" = "#C154C1",
                        "Mardi Gras" = "#880085",
                        "Dark Magenta" = "#8B008B",
                        "Gold" = "#FFD700",
                        "Mustard" = "#FFDB58",
                        "Sunglow" = "#FFCC33",
                        "Lemon Yellow" = "#FFF44F",
                        "Metallic Sunburst" = "#9c7c38",
                        "Silver" = "#C0C0C0",
                        "Onyx" = "#353839",
                        "Charcoal" = "#36454F",
                        "Smoky Black" = "#100C08",
                        "Vampire Black" = "#080808",
                        "Dark charcoal" = "#333333",
                        "Dark Slate Grey" = "#2f4f4f",
                        "Dark Midnight Blue" = "#000036",
                        "Snow" = "#FFFAFA",
                        "Ivory" = "#FFFFF0",
                        "Milk" = "#FDFFF5",
                        "Black" = "#000000",
                        "TS red" = "#a41d34",
                        "TS blue" = "#14c0f1"
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
#'
#'
karadeniz <- function(palette = "main", reverse = FALSE, ...) {
  mypalettes <- list(
          "Hidirnebi" = to_select_colors("Dark Blue", "Pistachio", "Amazon", "Charcoal", "Smoky Black"),
          "Kadirga" = to_select_colors("Deep Sky Blue", "Argentinan", "Dodger Blue", "Deep Fuchsia", "Dark Orchid", "Dark charcoal"),
          "Kayabasi" = to_select_colors("Forest Green",  "Dark Olive Green", "Saddle Brown", "Dark Slate Grey"),
          "Cal" = to_select_colors("Orange", "Pumpkin",  "Crimson", "Chocolate", "Dark Scarlet"),
          "Pokut" = to_select_colors("Twitter",  "Facebook", "IBM", "Dark Midnight Blue"),
          "Samistal" = to_select_colors( "Vivid Sky Blue", "Sapphire", "Liverpool", "Burnt Orange", "Vampire Black"),
          "Elevit" = to_select_colors("Chocolate", "Saddle Brown", "Peru", "Cadmium Red"),
          "Gito" = to_select_colors("Dark Olive Green", "Sepia", "Persimmon", "Pumpkin"),
          "Ovit" = to_select_colors("Metallic Sunburst", "Sunglow", "Gold", "Dark Orange", "Black"),
          "Palovit" = to_select_colors("Tomato", "Liverpool", "Cadmium Red",  "Carmine","Maroon", "Smoky Black"),
          "Faroz" = to_select_colors("TS red", "TS blue"),
          "Vazil" = to_select_colors("Lime", "Camouflage Green", "Gold", "Dark charcoal")

  )
  pal <- mypalettes[[palette]]

  if (reverse) pal <- rev(pal)

  colorRampPalette(pal, bias = .5, ...)
}

#' @title scale_color_karadeniz
#' @description To define my color pallette to standardize color use in projects
#' @param palette Choose your favorite palette: "Hidirnebi", "Kadirga", "Kayabasi", "Cal", "Pokut", "Samistal", "Elevit", "Gito", "Ovit", "Palovit", "Faroz", "Vazil"
#' @param discrete TRUE/FALSE
#' @param reverse TRUE/FALSE
#' @author Ali Guner
#' @examples
#' \dontrun{
#' iris %>%
#' ggplot(aes(Petal.Length, Petal.Width, color = Species))+
#' geom_point() +
#' scale_color_karadeniz()
#'}
#'
#'iris %>%
#'ggplot(aes(Sepal.Length, Sepal.Width, color = Petal.Length)) +
#'        geom_point() +
#'        scale_color_karadeniz(palette = "Vazil", discrete = FALSE)
#'@export
#'
scale_color_karadeniz <- function(palette = "Faroz",
                                  discrete = TRUE,
                                  reverse = FALSE,
                                  ...) {
        pal <- karadeniz(palette = palette, reverse = reverse)

        if (discrete) {
                discrete_scale("colour", paste0("karadeniz_", palette), palette = pal, ...)
        } else {
                scale_color_gradientn(colours = pal(256), ...)
        }
}

#' @title scale_fill_karadeniz
#' @description To define my color pallette to standardize color use in projects
#' @param palette Choose your favorite palette: "Hidirnebi", "Kadirga", "Kayabasi", "Cal", "Pokut", "Samistal", "Elevit", "Gito", "Ovit", "Palovit", "Faroz", "Vazil"
#' @param discrete TRUE/FALSE
#' @param reverse TRUE/FALSE
#' @author Ali Guner
#' @examples
#' \dontrun{
#' #' iris %>%
#' ggplot(aes(Petal.Length, Petal.Width, fill = Species))+
#' geom_point() +
#' scale_fill_karadeniz()
#'}
#'
#'iris %>%
#'ggplot(aes(Sepal.Length, Sepal.Width, fill = Petal.Length)) +
#'        geom_point() +
#'        scale_fill_karadeniz(palette = "Vazil", discrete = FALSE)
#'@export
#'
scale_fill_karadeniz <- function(palette = "Faroz",
                                 discrete = TRUE,
                                 reverse = FALSE,
                                 ...) {
        pal <- karadeniz(palette = palette, reverse = reverse)

        if (discrete) {
                discrete_scale("fill", paste0("karadeniz_", palette), palette = pal, ...)
        } else {
                scale_fill_gradientn(colours = pal(256), ...)
        }
}
