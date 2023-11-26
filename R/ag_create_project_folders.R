#' @title Create Project Folders
#'
#' @description This function creates the essential folder structure for a new R project.
#' It includes the main folders, subfolders, and necessary files to organize the project.
#'
#' @details The function performs the following steps:
#' 1. Define folder names, including "_Codes," "_Outputs," and "_Data."
#' 2. Check and create the main folders: "_Codes," "_Outputs," and "_Data."
#' 3. Check and create a subfolder within "_Codes" named "_Functions."
#' 4. Check and create a README file within "_Codes" named "README.md" for project documentation.
#' 5. Find .qmd files in the main directory, and if only one is found, move and rename it to
#' "_Codes" as "00 initial.qmd" (with a warning if there are multiple .qmd files).
#' 6. Check and create subfolders within "_Outputs," including "_Figures" and "_Tables."
#' 7. Check and create subfolders within "_Data," including "_Raw" and "_Processed."
#'
#' @param None
#'
#' @import dplyr
#'
#' @return None
#'
#' @examples
#' \dontrun{
#' # Example usage
#' ag_create_project_folders()
#' }
#'
#' @seealso \code{\link{dir.create}}, \code{\link{file.rename}}, \code{\link{writeLines}}
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
                new_qmd_file <- file.path(codes_folder, "01 initial.qmd")

                file.rename(qmd_file, new_qmd_file)
                cat(sprintf("Moved and renamed file: %s\n", new_qmd_file))
        } else if (length(qmd_files) > 1) {
                cat("Skipping moving step as there are multiple .qmd files in the main directory.\n")
        } else {
                cat("No .qmd file found in the main directory.\n")
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
}


