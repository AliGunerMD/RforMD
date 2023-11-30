#' @title to_select_colors
#' @description  To define my color pallette to standardize color use in projects
#' @author Ali Guner
#' @importFrom grDevices colorRampPalette
#' @param ... Additional arguments to be passed

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
    "Myblue" = "#6BAED6",
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

  if (is.null(cols)) {
    return(selected_colors)
  }

  selected_colors[cols]
}

#' @title karadeniz color pallette
#' @description Multiple color palettes based on the "palette" argument
#' @param palette Choose your favourite palette: "Hidirnebi", "Kadirga", "Kayabasi",  "Erikbeli", "Pokut", "Samistal", "Elevit", "Gito" , "Ovit", "Vazil"  or "Palovit"
#' @param reverse TRUE/FALSE
#' @param ... Additional arguments to be passed
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
    "Hidirnebi" = to_select_colors("Pistachio", "Myblue", "Dodger Blue", "Amazon", "Charcoal", "Smoky Black"),
    "Kadirga" = to_select_colors("Deep Fuchsia", "Dark Orchid", "Argentinan", "Dodger Blue", "Dark charcoal"),
    "Kayabasi" = to_select_colors("Forest Green", "Dark Olive Green", "Saddle Brown", "Dark Slate Grey"),
    "Erikbeli" = to_select_colors("Orange", "Pumpkin", "Chocolate", "Crimson"),
    "Pokut" = to_select_colors("Twitter", "Facebook", "Dark Midnight Blue"),
    "Samistal" = to_select_colors("Liverpool", "Burnt Orange", "Dark Blue", "Sapphire", "Vampire Black"),
    "Elevit" = to_select_colors("Chocolate", "Saddle Brown", "Peru", "Cadmium Red"),
    "Gito" = to_select_colors("Dark Olive Green", "Sepia", "Persimmon", "Pumpkin"),
    "Ovit" = to_select_colors("Metallic Sunburst", "Sunglow", "Gold", "Dark Orange"),
    "Palovit" = to_select_colors("Tomato", "Liverpool", "Cadmium Red", "Carmine", "Maroon", "Dark Scarlet", "Smoky Black"),
    "Faroz" = to_select_colors("TS red", "TS blue"),
    "Vazil" = to_select_colors("Gold", "Forest Green", "Camouflage Green", "Dark charcoal")
  )
  pal <- mypalettes[[palette]]

  if (reverse) pal <- rev(pal)

  colorRampPalette(pal, bias = 1, ...)
}

#' @title scale_color_karadeniz
#' @description To define my color pallette to standardize color use in projects
#' @param palette Choose your favorite palette: "Hidirnebi", "Kadirga", "Kayabasi", "Erikbeli", "Pokut", "Samistal", "Elevit", "Gito", "Ovit", "Palovit", "Faroz", "Vazil"
#' @param discrete TRUE/FALSE
#' @param reverse TRUE/FALSE
#' @param ... Additional arguments to be passed
#' @author Ali Guner
#' @examples
#' \dontrun{
#' iris %>%
#' ggplot(aes(Petal.Length, Petal.Width, color = Species))+
#' geom_point() +
#' scale_color_karadeniz()
#'}
#'
#' iris %>%
#' ggplot(aes(Sepal.Length, Sepal.Width, color = Petal.Length)) +
#' geom_point() +
#' scale_color_karadeniz(palette = "Vazil", discrete = FALSE)
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
#' @param palette Choose your favorite palette: "Hidirnebi", "Kadirga", "Kayabasi", "Erikbeli", "Pokut", "Samistal", "Elevit", "Gito", "Ovit", "Palovit", "Faroz", "Vazil"
#' @param discrete TRUE/FALSE
#' @param reverse TRUE/FALSE
#' @param ... Additional arguments to be passed
#' @author Ali Guner
#' @examples
#' \dontrun{
#' #' iris %>%
#' ggplot(aes(Petal.Length, Petal.Width, fill = Species))+
#' geom_point() +
#' scale_fill_karadeniz()
#'}
#'
#' iris %>%
#' ggplot(aes(Sepal.Length, Sepal.Width, fill = Petal.Length)) +
#' geom_point() +
#' scale_fill_karadeniz(palette = "Vazil", discrete = FALSE)
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

#' DEFINE MY COLORS --------------------------------------
#'
#' @title  ag_colors
#' @description  To define single color for standardizing color use in projects (Source: RColorBrewer)
#' @param palette_name A character string specifying the name of the desired color palette. If NULL (default), a palette is randomly selected.
#' @return A vector representing either the selected color palette or a randomly chosen color from a palette.
#'
#' @examples
#' # Select a specific palette
#' ag_colors("greens")
#' ag_colors("greens")[4]
#'
#' # Randomly select a color from any palette
#' ag_colors()
#'
#'
#' @keywords color palette, data visualization
#' @export
#'

ag_colors <- function(palette_name = NULL) {
        # Define the palettes
        ag_blues <- c("#F7FBFF", "#DEEBF7", "#C6DBEF", "#9ECAE1", "#6BAED6", "#4292C6", "#2171B5" ,"#084594")
        ag_greens <- c("#F7FCF5", "#E5F5E0", "#C7E9C0", "#A1D99B", "#74C476", "#41AB5D" ,"#238B45", "#005A32")
        ag_reds <- c("#FFF5F0", "#FEE0D2", "#FCBBA1", "#FC9272" ,"#FB6A4A" ,"#EF3B2C", "#CB181D", "#99000D")
        ag_grays <- c("#FFFFFF", "#F0F0F0", "#D9D9D9", "#BDBDBD", "#969696", "#737373", "#525252", "#252525")
        ag_oranges <- c("#FFF5EB", "#FEE6CE", "#FDD0A2", "#FDAE6B", "#FD8D3C", "#F16913", "#D94801", "#8C2D04")
        ag_spectral <- c("#D53E4F", "#F46D43", "#FDAE61", "#FEE08B", "#E6F598", "#ABDDA4", "#66C2A5", "#3288BD")
        ag_brewer1 <- c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3", "#FF7F00", "#FFFF33", "#A65628", "#F781BF")

        # Check if the provided palette name is one of the predefined palettes
        if (!is.null(palette_name) && !(palette_name %in% c("blues", "greens", "reds", "grays", "oranges", "spectral", "brewer1"))) {
                stop("Invalid palette_name. Choose from 'blues', 'greens', 'reds', 'grays', 'oranges', 'spectral', 'brewer1'")
        }


        if (is.null(palette_name)) {

                # If palette_name is not provided, randomly select one
                palettes <- list(ag_blues, ag_greens, ag_reds, ag_grays, ag_oranges, ag_spectral, ag_brewer1)
                random_palette <- sample(palettes, 1)

                random_color <- sample(random_palette[[1]], 1)
                return(random_color)

        }

        # Return the selected palette
        switch(palette_name,
               blues = ag_blues,
               greens = ag_greens,
               reds = ag_reds,
               grays = ag_grays,
               oranges = ag_oranges,
               spectral = ag_spectral,
               brewer1 = ag_brewer1,
               stop("Invalid palette_name. Choose from 'blues', 'greens', 'reds', 'grays', 'oranges', 'spectral', 'brewer1'"))
}

