

ag_pull_cell <- function(dataset,
                         col_1_name, col_1,
                         col_2_name, col_2,
                         col_3_name, col_3,
                                 col_target,
                               format = NULL) {



        # Check if the input is a data.frame
        if (!is.data.frame(dataset)) {
                stop("Input must be a data.frame")
        }

        # # Check if the column names exist in the data.frame
        # if (!(col_1 %in% colnames(dataset)) || !(col_2 %in% colnames(dataset)) || !(col_3 %in% colnames(dataset)) || !(col_target %in% colnames(dataset))) {
        #         stop("One or more column names not found in the data.frame")
        # }
        #
        #
        # if (is.null(label_name) || is.null(levels_name)) {
        #         stop("label_name and levels_name should be provided.")
        # }

        if(!missing(col_1_name) && !missing(col_1) && !missing(col_2_name) && !missing(col_2) && !missing(col_3_name) && !missing(col_3)){
                matched_rows <- dataset[[col_1_name]] == col_1 & dataset[[col_2_name]] == col_2 & dataset[[col_3_name]] == col_3
        } else  if(!missing(col_1_name) && !missing(col_1) && !missing(col_2_name) && !missing(col_2)){
                matched_rows <- dataset[[col_1_name]] == col_1 & dataset[[col_2_name]] == col_2
        }else  if(!missing(col_1_name) && !missing(col_1)){
                matched_rows <- dataset[[col_1_name]] == col_1
        } else {
                stop("No columns were defined.")
        }




        values <- dataset[[col_target]][matched_rows & !is.na(dataset[[col_target]])]
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

