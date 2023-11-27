#' @title Generate Quarto Document Using Template
#'
#' @description
#' The \code{ag_generate_template_quarto()} function creates a new Quarto (.qmd) document
#' using a template file provided by the "MyR" package. This function simplifies the process
#' of initializing Quarto projects with a customizable template.
#'
#' @param name A character string specifying the name of the output Quarto document.
#' If not provided, the default name is "01 initial.qmd".
#' @param open A logical indicating whether to open the newly created Quarto document interactively.
#' @param ... Additional arguments passed to the \code{use_template} function.
#'
#' @return None
#'
#' @examples
#' \dontrun{
#' # Example usage
#' ag_generate_template_quarto(name = "my_quarto_project.qmd", open = TRUE)
#' }
#'
#' @seealso
#' \code{\link{use_template}}
#'
#' @importFrom usethis use_template
#'
#' @export



ag_generate_template_quarto <- function(name = "01 initial.qmd",
                                        open = interactive(),
                                        ...) {

        # Construct the path to the template file in "inst/templates" directory
        template_path <- system.file("templates", "ag_template_quarto.qmd", package = "MyR")

        # Check if the template file exists
        if (!file.exists(template_path)) {
                stop("Could not find template 'ag_template_quarto.qmd' in package 'MyR'.")
        }

        # Use the template
        new_qmd_file <- use_template("ag_template_quarto.qmd",
                     save_as = name,
                     package = "MyR", ...,
                     open = open)

        # Return the path of the created file
        return(new_qmd_file)
}
