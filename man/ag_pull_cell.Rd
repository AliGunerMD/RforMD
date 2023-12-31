% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ag_pull_cell.R
\name{ag_pull_cell}
\alias{ag_pull_cell}
\title{Pull Cell Values from a Dataset}
\usage{
ag_pull_cell(
  dataset,
  col_1_name,
  col_1,
  col_2_name,
  col_2,
  col_3_name,
  col_3,
  target,
  format = NULL
)
}
\arguments{
\item{dataset}{A data.frame object containing the dataset.}

\item{col_1_name}{The name of the first column for condition matching.}

\item{col_1}{The value to match in the first column.}

\item{col_2_name}{The name of the second column for condition matching.}

\item{col_2}{The value to match in the second column.}

\item{col_3_name}{The name of the third column for condition matching.}

\item{col_3}{The value to match in the third column.}

\item{target}{The name of the target column from which to extract values.}

\item{format}{The format of the extracted values. Options are NULL (default), "bracket", "full bracket", or "paranthesis".}
}
\value{
A vector of extracted cell values from the target column.
}
\description{
This function extracts cell values from a dataset based on specified conditions.
}
\examples{
\dontrun{
penguins |> ag_pull_cell("species", "Adelie", "bill_length_mm", "39.1", target = "body_mass_g")
}

}
\author{
Ali Guner
}
