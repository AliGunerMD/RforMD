#' @title Create Project Folders
#'
#' @description
#' This function automates the creation of essential folder structures for a new R project.
#' It sets up main directories such as "Codes," "Outputs," and "Data," along with subdirectories,
#' subfolders, and necessary files for organizing the project efficiently.
#' The function performs the following steps:
#'
#' 1. Creates main folders: "Codes," "Outputs," and "Data."
#' 2. Sets up subfolders within "Codes," including "Functions."
#' 3. Creates a README file within "Codes" named "00 README.md" for project documentation.
#' 4. Sets up subfolders within "Outputs," including "Figures" and "Tables."
#' 5. Sets up subfolders within "Data," including "Raw" and "Processed."
#' 6. Checks for and deletes a single .qmd file in the main directory with user confirmation.
#' 7. Inserts a new .qmd template file using ag_generate_template_quarto() and moves it to the "Codes" folder as "01 initial.qmd."
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
        main_folder_names <- c("Codes", "Outputs", "Data")
        output_subfolder_names <- c("Figures", "Tables")
        data_subfolder_names <- c("Raw", "Processed")

        # Check and create main folders
        for (folder in main_folder_names) {
                folder_path <- file.path(folder)
                if (!dir.exists(folder_path)) {
                        dir.create(folder_path, recursive = TRUE)
                        cat(sprintf("Created folder: %s\n", folder_path))
                }
        }

        # Check and create subfolders within "Codes"
        codes_folder <- file.path("Codes")
        functions_subfolder <- file.path(codes_folder, "Functions")
        readme_file <- file.path(codes_folder, "00 README.md")

        if (!dir.exists(functions_subfolder)) {
                dir.create(functions_subfolder, recursive = TRUE)
                cat(sprintf("Created subfolder: %s\n", functions_subfolder))
        }

        # Check and create subfolders within "Outputs"
        outputs_folder <- file.path("Outputs")
        for (subfolder in output_subfolder_names) {
                subfolder_path <- file.path(outputs_folder, subfolder)
                if (!dir.exists(subfolder_path)) {
                        dir.create(subfolder_path, recursive = TRUE)
                        cat(sprintf("Created subfolder: %s\n", subfolder_path))
                }
        }

        # Check and create subfolders within "Data"
        data_folder <- file.path("Data")
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

        # Insert a new .qmd template
        new_qmd_template <- ag_generate_template_quarto()

        # Move the newly created file to the "Codes" folder
        if (!is.null(new_qmd_template)) {
                new_qmd_file <- file.path(codes_folder, "01 initial.qmd")

                # Move the file
                file.rename(from = "01 initial.qmd", to = new_qmd_file)

                cat(sprintf("Moved to %s\n", new_qmd_file))
        }

}














#'
#'
#' @title Create an Excel File for Variables Name
#' @description This function creates an Excel file with column names of the main dataset.
#'
#' @param dataset The dataset to extract column names from. Default is 'analysisDataset'.
#' @param excel_path The path and filename for the Excel file. Default is "Data/Variables.xlsx".
#'
#' @return Saves a .xlsx file into the path.
#'
#' @author Ali Guner
#' @import openxlsx
#' @importFrom tibble add_column
#'
#' @examples
#' \dontrun{
#' #' ag_create_excel(dataset = iris)
#' ag_create_excel(dataset = penguins, excel_path = "MyExcelFile.xlsx")
#' }

#' @export



# Function to create an Excel file with column names and descriptions
# Default dataset is 'analysisDataset', default Excel path is "Data/Variables.xlsx"
ag_create_excel <- function(dataset = analysisDataset, excel_path = "Data/Variables.xlsx") {

        column_names <- colnames(dataset)

        # Create a data frame with 'original' and 'corrected' column names
        df <- data.frame(original = column_names) %>%
                dplyr::mutate(corrected_title = stringr::str_to_sentence(stringr::str_replace_all(original, "_", " ")),
                               corrected_lower = stringr::str_to_lower(stringr::str_replace_all(original, "_", " "))) %>%
                tibble::add_column(manual = " ") %>%
                tibble::add_column(units = " ")
                # mutate(combine = paste0(corrected, " (", units ," )")) %>% # should be done inside the project ag_ff_labels


        dir_path <- dirname(excel_path)

        # Check if the file already exists in the specified directory
        if (file.exists(excel_path)) {
                stop("File with the same name already exists. Please choose a different path or filename.")
        }

        # Create the directory if it doesn't exist
        if (!dir.exists(dir_path)) {
                tryCatch(
                        expr = {
                                dir.create(dir_path, recursive = TRUE)
                        },
                        error = function(e) {
                                stop("Unable to create directory. Please check if the path is valid and you have sufficient permissions.")
                        }
                )
        }

        # Create a new workbook
        wb <- openxlsx::createWorkbook()

        # Add a worksheet named "Sheet 1"
        openxlsx::addWorksheet(wb, "Sheet 1")

        # Set column widths to 18 ("auto" is an option)
        openxlsx::setColWidths(wb, sheet = 1, cols = 1:ncol(df), widths = 18)

        # Write the data frame to "Sheet 1" with autofilter
        openxlsx::writeData(wb, sheet = 1, x = df, withFilter = TRUE)

        # Create and add a style to the column headers
        headerStyle <- openxlsx::createStyle(
                fontSize = 13, fontColour = "black", halign = "center",
                fgFill = "#9ECAE1", textDecoration = "bold"
        )
        openxlsx::addStyle(wb, sheet = "Sheet 1", headerStyle, rows = 1, cols = 1:ncol(df))

        # Freeze the pane at the first row
        openxlsx::freezePane(wb, "Sheet 1", firstActiveRow = 2)

        # Save the workbook to the specified Excel path
        openxlsx::saveWorkbook(wb, excel_path)

        message("Excel file created successfully at", excel_path)
}




