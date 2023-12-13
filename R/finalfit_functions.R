
#' @title Enhanced ff_glimpse Function with FlexTable Output
#'
#' @description
#' This function provides an enhanced version of the ff_glimpse function from the `finalfit` package.
#' It generates a summary table with additional information such as levels, missing values, quartiles, mean, standard deviation, and more. The output is in the form of a `flextable` object, which allows for easy customization and formatting.
#'#'
#' @param .dataset The input .data.
#' @param strata The variable used for stratification. Default is `NULL`.
#' @param table_vars The variables to include in the summary table. Default is `NULL`, which includes all variables in the .data.
#' @param type The type of variables to include in the summary table. Valid values are `NULL` (default), "cat" (categorical), or "cont" (continuous).
#' @param missing Logical indicating whether to include a category for missing values in stratified evaluation Default is `FALSE`.
#' @param levels_cut The maximum number of levels to display for categorical variables. Default is 10.
#' @param ... Additional arguments to be passed to the `ag_flex()` function.
#'
#' @return A `flextable` object containing the summary table.
#'
#' @author Ali Guner
#'
#' @importFrom dplyr select mutate across
#' @importFrom tidyselect everything
#' @importFrom glue glue
#' @importFrom flextable flextable vline valign set_header_labels align hline
#' @importFrom stringr str_replace_all str_remove_all
#' @importFrom finalfit ff_glimpse
#' @examples
#' \dontrun{
#' library(palmerpenguins)
#' ag_ff_glimpse(penguins, type = "cont")
#' ag_ff_glimpse(penguins, strata = "sex", missing = TRUE, type = "cat")
#' }
#'
#' @export




ag_ff_glimpse <- function(.dataset, strata = NULL, table_vars = NULL, type = NULL, missing = FALSE, levels_cut = 10, ...) {
  # Check if type is valid
  valid_types <- c(NULL, "cat", "cont")
  if (!is.null(type) && !type %in% valid_types) {
    stop("Invalid type. Type should be one of: NULL, 'cat', 'cont'")
  }

  # Check if table_vars is NULL or a vector
  if (!is.null(table_vars) && !is.vector(table_vars)) {
    stop("Invalid table_vars. table_vars should be NULL or a vector")
  }

  # Check if strata is NULL or a single element vector
  if (!is.null(strata) && (!is.vector(strata) || length(strata) != 1)) {
    stop("Invalid strata. strata should be NULL or a single element vector")
  }

  # Check if missing is logical
  if (!is.logical(missing)) {
    stop("Invalid missing. missing should be a logical TRUE or FALSE")
  }

  # Check if levels_cut is numeric
  if (!is.numeric(levels_cut)) {
    stop("Invalid levels_cut. levels_cut should be a numeric value")
  }



  flex_cat <- . %>%
    ag_flex(...) %>%
    flextable::set_header_labels(
      i = 1,
      "levels_n" = "Levels (n)",
      "levels" = "Levels",
      "var_type" = "Class",
      "missing_n" = "Missing (n)",
      "missing_percent" = "Missing (%)",
      "label" = "Label",
      "levels_count" = "Levels (Count)",
      "levels_percent" = "Levels (%)",
      "n" = glue::glue("n = {nrow(.dataset)}")
    ) %>%
    flextable::vline(j = c("var_type", "missing_percent")) %>%
    flextable::valign(part = "body", valign = "top") %>%
    # flextable::hline(i = ~ before(label, names(.))) %>%
    flextable::align(j = -1, align = "center", part = "all")

  flex_cont <- . %>%
    ag_flex(...) %>%
    flextable::set_header_labels(
      i = 1,
      "quartile_25" = "Q1",
      "quartile_75" = "Q3",
      "var_type" = "Class",
      "missing_n" = "Missing (n)",
      "missing_percent" = "Missing (%)",
      "label" = "Label",
      "mean" = "Mean",
      "sd" = "SD",
      "median" = "Median",
      "min" = "Min",
      "max" = "Max",
      "n" = glue::glue("n = {nrow(.dataset)}")
    ) %>%
    flextable::vline(j = c("var_type", "missing_percent", "sd")) %>%
    flextable::align(j = -1, align = "center", part = "all")


  if (is.null(table_vars)) {
    # message("No table_vars were defined. All variables in dataset will be evaluated.")
    if (is.null(strata)) {
      table_vars <- .dataset %>%
        dplyr::select(tidyselect::everything()) %>%
        names()
    } else {
      table_vars <- .dataset %>%
        dplyr::select(tidyselect::everything(), -{{ strata }}) %>%
        names()
    }
  }


  if (is.null(strata)) {
    if (is.null(type)) {
      glimpse_table <- finalfit::ff_glimpse(.dataset, explanatory = table_vars, levels_cut = levels_cut)
      message("This is messy. It is better to define a type as Continuous or Categorical.")
    } else if (type == "cont") {
      glimpse_table <- finalfit::ff_glimpse(.dataset, explanatory = table_vars, levels_cut = levels_cut)$Continuous %>%
        flex_cont()
    } else if (type == "cat") {
      glimpse_table <- finalfit::ff_glimpse(.dataset, explanatory = table_vars, levels_cut = levels_cut)$Categorical %>%
        dplyr::mutate(dplyr::across(levels:levels_percent, ~ stringr::str_replace_all(., ", ", "\n")),
          levels = stringr::str_remove_all(levels, '\\"')
        ) %>%
        flex_cat()
    }

    return(glimpse_table)
  } else {
    splitted <- function(.dataset, strata) {
      if (missing) {
        .dataset[[strata]] <- as.character(.dataset[[strata]])
        .dataset[[strata]][is.na(.dataset[[strata]])] <- "Missing*"
      }

      split_df <- split(.dataset, .dataset[[strata]])
      split_df <- lapply(split_df, function(x) {
        x <- x[, -which(names(x) == strata)]
        x
      })
    }

    my_split <- splitted(.dataset, strata)

    table_vars <- setdiff(table_vars, strata)

    if (is.null(type)) {
      combined_df <- lapply(my_split, function(x) finalfit::ff_glimpse(x, explanatory = table_vars, levels_cut = levels_cut))
      message("This is messy. It is better to define a type as Continuous or Categorical.")
    } else {
      if (type == "cont") {
        df_list <- lapply(my_split, function(x) finalfit::ff_glimpse(x, explanatory = table_vars, levels_cut = levels_cut)$Continuous)
      } else if (type == "cat") {
        df_list <- lapply(my_split, function(x) finalfit::ff_glimpse(x, explanatory = table_vars, levels_cut = levels_cut)$Categorical)
      }
      # Combine the list elements into one dataframe
      combined_df <- do.call(rbind, df_list)

      # Create a new column "strata" with the names of the list elements
      combined_df$Strata <- rep(names(df_list), sapply(df_list, nrow))

      combined_df <- combined_df %>%
        dplyr::relocate(Strata, .before = 1) %>%
        tibble::as_tibble()

      if (type == "cat") {
        combined_df <- combined_df %>%
                dplyr::mutate(dplyr::across(levels:levels_percent, ~ stringr::str_replace_all(., ", ", "\n")),
            levels = stringr::str_remove_all(levels, '\\"')
          ) %>%
          flex_cat()
      } else {
        combined_df <- combined_df %>%
          flex_cont()
      }
    }
    return(combined_df)
  }
}










#' @title Minimized version of summary_factorlist
#' @description
#' Perform Short Summary Statistics and Hypothesis Testing

#'
#' This function generates short summary statistics and conducts hypothesis testing, including chi-square tests and Fisher's exact tests if specified.
#'
#' @param .dataset A data frame containing the variables of interest.
#' @param strata The dependent variable for hypothesis testing.
#' @param table_vars A vector of explanatory variables for analysis.
#' @param na_include Logical. Should NA values be included in the analysis? Default is NULL.
#' @param cont The type of measure for continuous variables. Default is NULL.
#' @param cont_nonpara A vector specifying non-parametric measures for continuous variables. Default is NULL.
#' @param fisher_correction Logical. Should Fisher's exact tests be corrected? Default is NULL.
#' @param ... Additional arguments to be passed to the underlying functions.
#'
#' @return A summary factor list containing short summary statistics and hypothesis testing results.
#'
#' @author Ali Guner
#'
#' @importFrom finalfit summary_factorlist
#' @import dplyr
#'
#' @keywords internal
#' @examples
#' \dontrun{
#' # Example usage of short_ff function
#' short_result <- short_ff(my_data, strata = "Response", table_vars = c("Var1", "Var2"))
#'}
#' @seealso \code{\link{summary_factorlist}}, \code{\link{ag_fisher}}
#'


short_ff <- function(.dataset, strata = NULL, table_vars = NULL,
                     na_include = NULL, cont = NULL, cont_nonpara = NULL,
                     fisher_correction = fisher_correction,
                     ...) {
  if (is.null(strata)) {
    init_ff <- summary_factorlist(.dataset,
      dependent = strata,
      explanatory = table_vars,
      na_include = na_include,
      cont = cont,
      cont_nonpara = cont_nonpara,
      total_col = TRUE,
      p = FALSE,
      ...
    ) %>%
            suppressWarnings()

    return(init_ff)
  } else {
    chisquare_ff <- summary_factorlist(.dataset,
      dependent = strata,
      explanatory = table_vars,
      na_include = na_include,
      cont = cont,
      cont_nonpara = cont_nonpara,
      total_col = TRUE,
      p = TRUE,
      ...
    ) %>%
            suppressWarnings()


    if (fisher_correction) {
      explanatory_fisher <- ag_fisher(.dataset = .dataset, table_vars = table_vars, strata = strata)

      if (is.null(explanatory_fisher)) {
        return(chisquare_ff)
      } else {
        fisher_ff <- summary_factorlist(.dataset,
          dependent = strata,
          explanatory = table_vars,
          na_include = na_include,
          cont = cont,
          cont_nonpara = cont_nonpara,
          total_col = TRUE,
          p = TRUE,
          p_cat = "fisher",
          ...
        ) %>%
                suppressWarnings()


        fisher_short <- fisher_ff %>%
          dplyr::filter(.[[1]] %in% explanatory_fisher) %>%     # do not use names (label)
          dplyr::select(1, p_fisher = p)

        combined_ff <- chisquare_ff %>%
          dplyr::left_join(fisher_short, by = "label") %>%
          dplyr::mutate(p = ifelse(is.na(p_fisher), p, p_fisher)) %>%
          dplyr::select(-p_fisher)

        return(combined_ff)
      }
    } else {
      # Return chisquare_ff if fisher_correction is FALSE
      return(chisquare_ff)
    }
  }
}




#' @title Generate Row-, Column-based or both-based Summary Statistics
#'
#' @description
#' This function generates row or column-based summary statistics based on the specified method.
#'
#' @param .dataset A data frame containing the variables of interest.
#' @param strata The dependent variable for hypothesis testing.
#' @param table_vars A vector of explanatory variables for analysis.
#' @param na_include Logical. Should NA values be included in the analysis? Default is NULL.
#' @param cont The type of measure for continuous variables. Default is NULL.
#' @param cont_nonpara A vector specifying non-parametric measures for continuous variables. Default is NULL.
#' @param fisher_correction Logical. Should Fisher's exact tests be corrected? Default is NULL.
#' @param row_col_sums The method for generating row or column-based summary statistics. Use 'row_based' or 'col_based'.
#' @param ... Additional arguments to be passed to the underlying functions.
#'
#' @return A summary factor list containing row or column-based summary statistics.
#'
#' @author Ali Guner
#' @keywords internal
#' @examples
#' \dontrun{
#' # Example usage of ff_row_col_sums function
#' row_col_result <- ff_row_col_sums(my_data, strata, table_vars, row_col_sums = "row_based")
#'}
#' @seealso \code{\link{short_ff}}


ff_row_col_sums <- function(.dataset,
                            strata = NULL,
                            table_vars = NULL,
                            na_include = NULL,
                            cont = NULL,
                            cont_nonpara = NULL,
                            fisher_correction = NULL,
                            row_col_sums,
                            ...) {
  if (row_col_sums == "row_based") {
    column <- FALSE
  } else if (row_col_sums == "col_based") {
    column <- TRUE
  }

  return(short_ff(.dataset,
    strata = strata,
    table_vars = table_vars,
    na_include = na_include,
    cont = cont,
    cont_nonpara = cont_nonpara,
    column = column,
    fisher_correction = fisher_correction,
    ...
  ))
}




#' @title Enhanced version of \code{summary_factorlist()} function of `{finalfit}`

#'
#' @description
#' Generate aggregated summary statistics and tests for categorical and continuous variables,
#' with an option for Fisher's correction in contingency tables.
#'
#' This function extends the functionality of the \code{summary_factorlist} function by introducing
#' Shapiro-Wilk test and selecting variables require Fisher's test.
#' Normality distributions of continuous variables is evaluated by Shapiro-Wilk test
#' and required test is decided based on the test.
#' For categorical variables, expected cell frequencies is calculated
#' (expected cell frequencies less than 5 in more than 20% of the cells in a
#' contingency table may require using Fisher test) and required test
#' (Chi-square or Fisher) is decided based on this.
#' \code{row_col_sums} function can be used for presenting different sum styles.
#' In the default one, the sums of the rows is 100% for the groups. For Total column, the sum of colon will be 100%.
#' This approach is good for summary tables.
#'
#'
#' @param .dataset A data frame containing the variables of interest.
#' @param strata The dependent variable (outcome) in the analysis, should be a factor.
#' @param table_vars A character vector specifying the explanatory variables (predictors) for the analysis.
#'                  These can be factors, characters, or numerics.
#' @param na_include Logical, indicating whether missing values should be included in the analysis.
#'                  Default is \code{TRUE}.
#' @param row_col_sums A character string specifying the type of summary statistics to be generated.
#'                    Options include "row_based" for row-based summaries, "col_based" for column-based summaries,
#'                    and "row_col_based" for both row and column-based summaries. Default is "row_col_based".
#' @param all_cont A character string specifying the approach for continuous variables. Options include
#'                "parametric" for parametric tests (mean), "nonparametric" for non-parametric tests (median),
#'                and "shapiro" for using the Shapiro-Wilk test for normality and choosing between parametric
#'                and non-parametric accordingly. Default is "shapiro". Manual option is also possible with \code{manual_nonparams}.
#' @param fisher_correction Logical, indicating whether Fisher's correction should be applied for expected cell
#'                         frequencies less than 5 in more than 20% of the cells in a contingency table.
#'                         Default is \code{TRUE}.
#' @param manual_nonparams If \code{all_cont} is set to "manual", this argument should contain the values (not index, the name of the variable) for
#'                         \code{cont_nonpara}. It cannot be empty in this case.
#' @param summary_factorlist_args A list containing additional arguments to be passed to \code{ag_ff_summary}.
#' All comes from the finalfit::summaryfactorlist function.
#'                                Default is an empty list.
#' @param ... Additional arguments to be passed to \code{ag_ff_summary}.
#'
#' @return A data frame containing the aggregated summary statistics and test results.
#'
#' @author Ali Guner
#'
#' @importFrom dplyr relocate
#' @examples
#' \dontrun{
#' # Example usage of ag_ff_summary function
#' library(palmerpenguins)
#'
#' table_vars_1 <- penguins %>%
#' dplyr::select(-species) %>%
#' names()
#'
#' ag_ff_summary(
#' .dataset = penguins,
#' table_vars = table_vars_1,
#' strata = "species"
#' )
#' }
#' @seealso \code{\link{ff_row_col_sums}}
#' @export



ag_ff_summary <- function(.dataset, strata = NULL, table_vars,
                          row_col_sums = "row_col_based",
                          all_cont = "shapiro",
                          manual_nonparams = NULL,
                          fisher_correction = TRUE,
                          na_include = FALSE,
                          summary_factorlist_args = list(),
                          ...) {


  # Checks
  if (!is.data.frame(.dataset)) stop("Your dataset is not a dataframe.")
  if (any(class(.dataset) %in% c("tbl_df", "tbl"))) .dataset <- data.frame(.dataset)
  if (is.null(table_vars)) stop("table_vars is empty")





  # Check data types of table_vars
  valid_vars <- sapply(table_vars, function(var) {
          if (is.factor(.dataset[[var]]) || is.character(.dataset[[var]]) || is.numeric(.dataset[[var]])) {
                  return(var)
          }
  })

  valid_vars <- as.vector(unlist(valid_vars))
  invalid_vars <- setdiff(table_vars, valid_vars)

  if (length(invalid_vars) > 0) {
          stop(paste("Invalid data type for the following variables:", paste(invalid_vars, collapse = ", "), ".
               Should be factor, character, or numeric!!"))
  }



  if (!is.null(strata)) {
          if (!is.factor(.dataset[[strata]]) && !is.character(.dataset[[strata]])) stop("The strata variable should be a factor/character. Please check the data type.")


          # Handle missing values in the strata
          missing_strata <- sum(is.na(.dataset[[strata]]))

          if (missing_strata > 0) {
                  warning(paste0("Important! There are ", missing_strata, " missing values in ", strata, ". Rows with missing values in ", strata, " will be removed from the analysis."))
                  .dataset <- .dataset[!is.na(.dataset[[strata]]), ]
          }

  }

  if(is.null(strata)){
          message("No strata was provided")
  }


  # Additional arguments from finalfit::summary_factorlist
  summary_factorlist_args <- list(
    cont_cut = 5,
    cont_range = TRUE,
    p_cont_para = "aov",
    orderbytotal = FALSE,
    digits = c(1, 1, 3, 1, 0),
    # na_include_dependent = FALSE,             already excluded. consider to relevel strata
    na_complete_cases = FALSE,
    na_to_p = FALSE,
    na_to_prop = TRUE,
    # add_dependent_label = FALSE,              "label" is good to manipulate.
    # dependent_label_prefix = "Dependent: ",
    # dependent_label_suffix = "",
    add_col_totals = FALSE,
    include_col_totals_percent = TRUE,
    col_totals_prefix = "",
    add_row_totals = FALSE,
    include_row_totals_percent = TRUE,
    include_row_missing_col = TRUE,
    row_totals_colname = "Total N",
    row_missing_colname = "Missing N"
  )



  # all_cont argument
  if (all_cont == "nonparametric") {

    if (!is.null(manual_nonparams)) message("manual_nonparams argument is useless, if all_cont is not 'manual'")
    cont <- "median"
    cont_nonpara <- NULL

  } else if (all_cont == "parametric") {

    if (!is.null(manual_nonparams)) message("manual_nonparams argument is useless, if all_cont is not 'manual'")
    cont <- "mean"
    cont_nonpara <- NULL

  } else if (all_cont == "shapiro") {

    if (!is.null(manual_nonparams)) message("manual_nonparams argument is useless, if all_cont is not 'manual'")

          numeric_vars <- .dataset %>%
                  dplyr::select(tidyselect::all_of(table_vars)) %>%
                  dplyr::select(where(is.numeric)) %>% names()

    cont <- "mean"

    if(length(numeric_vars) == 0){
            cont_nonpara <- NULL
    } else {
            cont_nonpara <- ag_shapiro(.dataset, strata = strata, table_vars = table_vars, silence = TRUE, names = FALSE)
    }

  } else if (all_cont == "manual") {

    if (is.null(manual_nonparams)) stop("When all_cont argument is manual, manual_nonparams argument can not be empty")
    cont <- "mean"
    cont_nonpara <- which(table_vars %in% manual_nonparams)

  } else {
    stop("Invalid value for 'all_cont'. Use 'parametric', 'nonparametric', 'shapiro' or 'manual'.")
  }





  if (row_col_sums == "row_based") {
    row_col_sums_df <- ff_row_col_sums(.dataset,
      strata = strata,
      table_vars = table_vars, na_include = na_include, cont = cont, cont_nonpara = cont_nonpara,
      fisher_correction = fisher_correction,
      row_col_sums = "row_based", ...
    ) %>%
      suppressMessages()

  } else if (row_col_sums == "col_based") {
    row_col_sums_df <- ff_row_col_sums(.dataset,
      strata = strata,
      table_vars = table_vars, na_include = na_include, cont = cont, cont_nonpara = cont_nonpara,
      fisher_correction = fisher_correction,
      row_col_sums = "col_based", ...
    ) %>%
      suppressMessages()

  } else if (row_col_sums == "row_col_based") {
    row_col_sums_df_comb <- cbind(
      ff_row_col_sums(.dataset,
        strata = strata,
        table_vars = table_vars, na_include = na_include, cont = cont, cont_nonpara = cont_nonpara,
        fisher_correction = fisher_correction, row_col_sums = "row_based", ...
      ) %>%
        suppressMessages() %>%
        select(-Total),
      ff_row_col_sums(.dataset,
        strata = strata,
        table_vars = table_vars, na_include = na_include, cont = cont, cont_nonpara = cont_nonpara,
        fisher_correction = fisher_correction, row_col_sums = "col_based", ...
      ) %>%
        suppressMessages() %>%
        select(Total)
    )

    row_col_sums_df <- if ("p" %in% colnames(row_col_sums_df_comb)) {
      dplyr::relocate(row_col_sums_df_comb, Total, .before = p)
    } else {
      dplyr::relocate(row_col_sums_df_comb, Total, .after = last_col())
    }
  } else {
    stop("Invalid value for row_col_sums. Use 'row_based', 'col_based', or 'row_col_based'.")
  }


  if (is.null(strata)) {
    return(row_col_sums_df %>% select(-all))
  } else {
    return(row_col_sums_df)
  }
}








#' @title Relocate columns of ag_ff_summary table
#'
#' @description
#' This function relocates columns of finalfit summary table based on the specified order,
#' considering different strata analysis options. It is primarily designed to
#' work with datasets generated by the \code{ag_ff_summary} function.
#'
#' @param .ff_table The input (ag_ff_summary table) to be modified.
#' @param order   Optional. The order to relocate the columns. Default is "GTP".
#'                Possible values are: "G" (Groups), "GP" (Groups and P-value),
#'                "GT" (Groups and Total), "GTP" (Groups, Total, and P-value),
#'                "TGP" (Total, Groups, and P-value), "T" (Total). If the dataset
#'                has only 3 columns, which means no strata was used, the order argument is not needed.
#' @author Ali Guner
#' @return Modified dataset with relocated columns.
#'
#' @importFrom tidyselect any_of
#' @importFrom dplyr select
#' @importFrom magrittr %>%
#'
#' @examples
#' \dontrun{
#' table_vars_1 <- penguins %>%
#' dplyr::select(-species) %>%
#' names()
#'
#' ag_ff_summary(penguins, table_vars = table_vars_1) %>%
#' ag_ff_relocate()
#'
#' ag_ff_summary(penguins, "species", table_vars_1) %>%
#' ag_ff_relocate("TGP")
#' }
#'
#' @seealso \code{\link{ag_ff_summary}}
#' @export



ag_ff_relocate <- function(.ff_table, order = NULL) {


        valid_orders <- c("G", "GP", "GT", "GTP", "TGP", "T")
        ncol <- ncol(.ff_table)

        original_columns <- colnames(.ff_table)
        strata_names <- original_columns[!original_columns %in% c("label", "levels", "Total", "p")]


        # Check if this is a ag_ff_summary output.
        if(!all(c("label", "levels") %in% original_columns)){
                stop("the input argument may not be a ag_ff_summary table.")
        }


        # Check ncol to understand the strata is present or not.
        if(ncol == 3){ # If the ncol is 3, definitely no strata analysis.
                relocated_data <- .ff_table

                if(is.null(order)){
                        message("Without strata, no need to relocate.")
                } else {
                        if ((order %in% valid_orders)) {
                                message("Order argument is not needed without strata. No worry, the outcome will not change.")
                        }

                        if (!(order %in% valid_orders)) {
                                message("Invalid order argument. No worry, the outcome will not change, because no need to relocate without strata.")
                        }
                }



        } else {


                if(is.null(order)){

                        order <- "GTP"
                        message("'order' is a required argument. 'GTP' will be used as default, but you can select any of: 'G', 'GP','GT', 'TGP', 'T'.")

                }


                if (!(order %in% valid_orders)) {
                        stop("Invalid order argument. Use one of: 'G', 'GP', 'GT', 'GTP', 'TGP', 'T'.")
                }


                switch(order,
                       "G"   = relocated_data <- .ff_table %>%
                               dplyr::select(label, levels, tidyselect::any_of(strata_names)),
                       "GT"  = relocated_data <- .ff_table %>%
                               dplyr::select(label, levels, tidyselect::any_of(strata_names), Total),
                       "GP"  = relocated_data <- .ff_table %>%
                               dplyr::select(label, levels, tidyselect::any_of(strata_names), p),
                       "GTP" = relocated_data <- .ff_table %>%
                               dplyr::select(label, levels, tidyselect::any_of(strata_names), Total, p),
                       "TGP" = relocated_data <- .ff_table %>%
                               dplyr::select(label, levels, Total, tidyselect::any_of(strata_names), p),
                       "T"   = relocated_data <- .ff_table %>%
                               dplyr::select(label, levels, Total)
                )


                message("Relocated columns:", paste(colnames(relocated_data), collapse = " -- "), "\n")
        }
        return(relocated_data)
}





#' @title Adjusts columns for variables in ag_ff_summary table
#' @description
#' This function adjusts column values in an ag_ff_summary table. Converts parametric values (for Total and Groups columns, whichever present) as Mean ± SD, or Median (Q1- Q3)
#' Also clean levels when levels argument is TRUE.
#' @param .data The input ag_ff_summary table.
#' @param levels Clean levels, default is FALSE.
#' @return The modified ag_ff_summary table with adjusted column values.
#'
#' @author Ali Guner
#'
#' @importFrom stringr str_replace_all str_remove_all str_to_title
#' @importFrom dplyr mutate across if_else
#' @importFrom tidyselect all_of

#' @examples
#' \dontrun{
#' table_vars_1 <- penguins %>%
#' dplyr::select(-species) %>% names()
#'
#' ag_ff_summary(penguins, "species", table_vars_1) %>%
#' ag_ff_relocate("TGP") %>%
#' ag_ff_columns(levels = TRUE)
#'
#' table_vars_2 <- iris %>%
#' dplyr::select(-Species) %>% names()
#'
#' ag_ff_summary(iris, "Species", table_vars_2) %>%
#' ag_ff_relocate("TGP") %>%
#' ag_ff_columns()
#'}
#'
#'
#' @seealso \code{\link{ag_ff_summary}}
#' @export
#'


ag_ff_columns <- function(.data, levels = FALSE) {

  original_columns <- colnames(.data)

  # Check if this is a ag_ff_summary output.
  if (!all(c("label", "levels") %in% original_columns)) {
    stop("the .data may not be a ag_ff_summary table.")
  }

  selected_column_names <- original_columns[!original_columns %in% c("label", "levels", "p")]

  columned_data <- .data %>%
          dplyr::mutate(
                  dplyr::across(tidyselect::all_of(selected_column_names), ~ stringr::str_replace_all(., " to ", " - ")),
                  dplyr::across(tidyselect::all_of(selected_column_names), ~ stringr::str_replace_all(., "\\.0", "")),
                  dplyr::across(tidyselect::all_of(selected_column_names), ~ dplyr::if_else(levels == "Mean (SD)", stringr::str_replace_all(., " \\(", " \u00B1 "), .)),
                  dplyr::across(tidyselect::all_of(selected_column_names), ~ dplyr::if_else(levels == "Mean (SD)", stringr::str_remove_all(., "\\)"), .))
    )


  if(levels){


          abbreviations <- columned_data$levels[stringr::str_detect(columned_data$levels, "^[A-Z]{2,}(\\.[A-Z]+)*$")]

          if(length(abbreviations) == 0){
                  abbreviations <- c("there is no abbreviations")
          }

          columned_data <- columned_data %>%
                  dplyr::mutate(
                          levels = dplyr::if_else(levels == "Median (IQR)", " ", levels),
                          levels = dplyr::if_else(levels == "Mean (SD)", " ", levels),
                          levels = dplyr::if_else(!stringr::str_detect(levels, abbreviations) & !stringr::str_detect(levels, "-"), stringr::str_to_title(levels), levels),
                          levels = stringr::str_replace_all(levels, "^\\w", function(match) toupper(match))
                  )

          message("Manual check may be needed for some levels.")

  }

  return(columned_data)
}


#' @title Rename labels of a ag_ff_summary table
#' @description
#' This function renames labels in ag_ff_summary table based on a vector of old and new names or an Excel file.
#' You can define old names and new names inside the R chunk. But using external excel file will be more convenient.
#' If you don't have a Variables.xlsx file, consider using \code{ag_create_excel}
#'
#'
#' @param .data The ag_ff_summary table to be modified.
#' @param use_vector Logical value indicating whether to use a vector for renaming labels. Default is \code{TRUE}.
#' @param vector_name The name of the vector containing old and new label names. Required if \code{use_vector = TRUE}.
#' @param use_excel Logical value indicating whether to use an Excel file for renaming labels. Default is \code{FALSE}.
#' @param excel_path The path to the Excel file containing old and new label names. Required if \code{use_excel = TRUE}.
#' @param excel_old_names The name of the column in the Excel file containing the old label names. Default is \code{"original"}.
#' @param excel_new_names The name of the column in the Excel file containing the new label names. Default is \code{"corrected"}.
#'
#' @return The ag_ff_summary table with renamed labels.
#'
#' @examples
#' \dontrun{
#' label_names <- c(
#'   "Sepal.Length" = "Sepal length",
#'   "Sepal.Width" = "Sepal width",
#'   "Petal.Length" = "Petal length",
#'   "Petal.Width" = "Petal width",
#'   "Species" = "Species"
#' )
#'
#' ag_ff_summary(iris, "Species", table_vars_2) %>%
#'   ag_ff_relocate("TGP") %>%
#'   ag_ff_columns(levels = TRUE) %>%
#'   ag_ff_labels(use_vector = TRUE, vector_name = label_names)
#'
#' ag_ff_summary(iris, "Species", table_vars_2) %>%
#'   ag_ff_relocate("TGP") %>%
#'   ag_ff_columns(levels = TRUE) %>%
#'   ag_ff_labels(use_excel = TRUE)
#'}
#'
#' @import dplyr
#' @importFrom readxl read_excel
#' @importFrom tibble as_tibble
#' @importFrom dplyr mutate
#' @importFrom stats setNames
#'
#' @export



# Function to rename labels in a dataset
ag_ff_labels <- function(.data,
                         use_vector = TRUE, vector_name = NULL,
                         use_excel = FALSE, excel_path = "_Data/Variables.xlsx", excel_old_names = "original", excel_new_names = "corrected") {

        # Get the original column names of the dataset
        original_columns <- colnames(.data)

        # Check if this is an ag_ff_summary output.
        if (!all(c("label", "levels") %in% original_columns)) {
                stop("the .data may not be an ag_ff_summary table.")
        }

        # Check if at least one of the renaming methods is selected
        if (!use_vector && !use_excel) {
                stop("One of use_vector or use_excel argument should be true")
        }

        # If both renaming methods are selected, prioritize the excel method
        if (use_vector && use_excel) {
                use_vector <- FALSE
        }

        # Rename labels using a vector of old_names and new_names
        if (use_vector && !use_excel) {

                # Check if vector_name is provided
                if(is.null(vector_name)){
                        stop("vector_name argument can not be NULL if use_vector is true. Provide the name of the variables vector.")
                }


                # Get the vector for renaming labels
                label_names <- data.frame(
                        label = names(vector_name),
                        name = vector_name,
                        stringsAsFactors = FALSE
                ) %>%
                        tibble::as_tibble()

                # Rename labels using mutate and recode functions
                labeled_dataset <- .data %>%
                        dplyr::mutate(label = recode(label, !!!setNames(label_names$name, label_names$label)))

                message("A vector for variable names was used to rename labels.")
        }

        # Rename labels using an Excel file
        if (use_excel) {
                # Read the Excel file and clean the column names
                Variables <- readxl::read_excel(here::here(excel_path))
                        # janitor::clean_names()

                # Check if excel_old_names and excel_new_names are present in the Excel file
                missing_names <- setdiff(c(excel_old_names, excel_new_names), colnames(Variables))
                if (length(missing_names) > 0) {
                        stop(paste("The following column names are not present in excel file:", paste(missing_names, collapse = ", ")))
                }

                # Rename labels using mutate and recode functions with columns from the Excel file
                labeled_dataset <- .data %>%
                        dplyr::mutate(label = recode(label, !!!setNames(Variables[[excel_new_names]], Variables[[excel_old_names]])))

                message("An excel file was used to rename labels.")
        }

        # Return the dataset with renamed labels
        return(labeled_dataset)
}


