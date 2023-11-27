#' @title Create Project Folders
#'
#' @description
#' This function automates the creation of essential folder structures for a new R project.
#' It sets up main directories such as "_Codes," "_Outputs," and "_Data," along with subdirectories,
#' subfolders, and necessary files for organizing the project efficiently.
#' The function performs the following steps:
#'
#' 1. Creates main folders: "_Codes," "_Outputs," and "_Data."
#' 2. Sets up subfolders within "_Codes," including "_Functions."
#' 3. Creates a README file within "_Codes" named "00 README.md" for project documentation.
#' 4. Sets up subfolders within "_Outputs," including "_Figures" and "_Tables."
#' 5. Sets up subfolders within "_Data," including "_Raw" and "_Processed."
#' 6. Checks for and deletes a single .qmd file in the main directory with user confirmation.
#' 7. Inserts a new .qmd template file using ag_generate_template_quarto() and moves it to the "_Codes" folder as "01 initial.qmd."
#' #'
#' This function is designed to be used right after creating a new R project via "New Project" or any other project creation method.
#'
#' @examples
#' \dontrun{
#' # Example usage
#' ag_create_project_folders()
#' }
#'
#' @seealso \code{\link{dir.create}}, \code{\link{file.remove}}, \code{\link{writeLines}}, \code{\link{ag_generate_template_quarto}}
#'
#' @keywords project folders organization initialization
#'
#' @export
#'



ag_create_project_folders <- function() {
        # Define folder names
        main_folder_names <- c("_Codes", "_Outputs", "_Data")
        output_subfolder_names <- c("_Figures", "_Tables")
        data_subfolder_names <- c("_Raw", "_Processed")

        # Check and create main folders
        for (folder in main_folder_names) {
                folder_path <- file.path(folder)
                if (!dir.exists(folder_path)) {
                        dir.create(folder_path, recursive = TRUE)
                        cat(sprintf("Created folder: %s\n", folder_path))
                }
        }

        # Check and create subfolders within "_Codes"
        codes_folder <- file.path("_Codes")
        functions_subfolder <- file.path(codes_folder, "_Functions")
        readme_file <- file.path(codes_folder, "00 README.md")

        if (!dir.exists(functions_subfolder)) {
                dir.create(functions_subfolder, recursive = TRUE)
                cat(sprintf("Created subfolder: %s\n", functions_subfolder))
        }

        # Check and create subfolders within "_Outputs"
        outputs_folder <- file.path("_Outputs")
        for (subfolder in output_subfolder_names) {
                subfolder_path <- file.path(outputs_folder, subfolder)
                if (!dir.exists(subfolder_path)) {
                        dir.create(subfolder_path, recursive = TRUE)
                        cat(sprintf("Created subfolder: %s\n", subfolder_path))
                }
        }

        # Check and create subfolders within "_Data"
        data_folder <- file.path("_Data")
        for (subfolder in data_subfolder_names) {
                subfolder_path <- file.path(data_folder, subfolder)
                if (!dir.exists(subfolder_path)) {
                        dir.create(subfolder_path, recursive = TRUE)
                        cat(sprintf("Created subfolder: %s\n", subfolder_path))
                }
        }


        # Check and create README file
        if (!file.exists(readme_file)) {
                readme_content <- "# Project Readme\n\nThis is a placeholder for project documentation."
                writeLines(readme_content, readme_file)
                cat(sprintf("Created file: %s\n", readme_file))
        }

        # Find .qmd files in the main directory
        qmd_files <- list.files(pattern = "\\.qmd$")
        if (length(qmd_files) == 1) {
                qmd_file <- qmd_files[1]
                cat(sprintf("A .qmd file (%s) found in the main directory.\n", qmd_file))
                confirm <- utils::menu(sprintf('Do you want to delete the file "%s"?', qmd_file),
                                       title = "Confirmation", choices = c("Yes", "No"))
                if (confirm == 1) {
                        file.remove(qmd_file)
                        cat(sprintf("Deleted file: %s\n", qmd_file))
                } else {
                        cat("File not deleted.\n")
                }
        } else if (length(qmd_files) > 1) {
                cat("Skipping deletion step as there are multiple .qmd files in the main directory.\n")
        } else {
                cat("No .qmd file found in the main directory.\n")
        }

        # Insert a new .qmd template into "_Codes"
        new_qmd_template <- ag_generate_template_quarto()

        # Move the newly created file to the "_Codes" folder
        if (!is.null(new_qmd_template) && file.exists(new_qmd_template)) {
                new_qmd_file <- file.path(codes_folder, "01 initial.qmd")
                file.rename(new_qmd_template, new_qmd_file)
                cat(sprintf("Moved and renamed file: %s\n", new_qmd_file))
        }
}


