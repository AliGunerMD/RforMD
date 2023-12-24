#' @title Pull Cell Values from a Dataset
#' @description
#' This function extracts cell values from a dataset based on specified conditions.
#'
#' @param dataset A data.frame object containing the dataset.
#' @param col_1_name The name of the first column for condition matching.
#' @param col_1 The value to match in the first column.
#' @param col_2_name The name of the second column for condition matching.
#' @param col_2 The value to match in the second column.
#' @param col_3_name The name of the third column for condition matching.
#' @param col_3 The value to match in the third column.
#' @param target The name of the target column from which to extract values.
#' @param format The format of the extracted values. Options are NULL (default), "bracket", "full bracket", or "paranthesis".
#'
#' @return A vector of extracted cell values from the target column.
#'
#' @author Ali Guner
#'
#' @examples
#' \dontrun{
#' penguins |> ag_pull_cell("species", "Adelie", "bill_length_mm", "39.1", target = "body_mass_g")
#' }
#'
#' @export






ag_pull_cell <- function(dataset,
                         col_1_name, col_1,
                         col_2_name, col_2,
                         col_3_name, col_3,
                         target,
                         format = NULL) {



        # Check if the input is a data.frame
        if (!is.data.frame(dataset)) {
                stop("Input must be a data.frame")
        }

        if (is.null(target)) {
                message("target should be provided.")
        }

        if(!missing(col_1_name) && !missing(col_1) && !missing(col_2_name) && !missing(col_2) && !missing(col_3_name) && !missing(col_3)){
                matched_rows <- dataset[[col_1_name]] == col_1 & dataset[[col_2_name]] == col_2 & dataset[[col_3_name]] == col_3
        } else  if(!missing(col_1_name) && !missing(col_1) && !missing(col_2_name) && !missing(col_2)){
                matched_rows <- dataset[[col_1_name]] == col_1 & dataset[[col_2_name]] == col_2
        }else  if(!missing(col_1_name) && !missing(col_1)){
                matched_rows <- dataset[[col_1_name]] == col_1
        } else {
                stop("No columns were defined.")
        }




        values <- dataset[[target]][matched_rows & !is.na(dataset[[target]])]
        if(is.null(format)){

                values <- values

        } else if (format == "bracket"){
                values <- gsub("^(\\d+\\.\\d+) \\((\\d+\\.\\d+-\\d+\\.\\d+), p(.+?)\\)$", "(\\1 [\\2], p\\3)", values)

        } else if (format == "full bracket"){
                values <- gsub("^(\\d+\\.\\d+) \\((\\d+\\.\\d+-\\d+\\.\\d+), p(.+?)\\)$", "[\\1, \\2, p\\3]", values)

        } else if (format == "paranthesis"){
                values <-    gsub("^(\\d+\\.\\d+) \\((\\d+\\.\\d+-\\d+\\.\\d+), p(.+?)\\)$", "(\\1, \\2, p\\3)", values)
        } else {
                "Incorrect format type. Should be NULL, bracket or parenthesis"
        }
        if (length(values) == 0) {
                message("No matching values found.")
                values <- NULL
        }

        return(values)

}

