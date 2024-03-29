% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/makeTables.R
\name{makeDialectRegions}
\alias{makeDialectRegions}
\title{Make the DIALECT_REGIONS table}
\usage{
makeDialectRegions(cities, updateID)
}
\arguments{
\item{cities}{The CITIES table, created with makeDemoGeo.R and makeCensusCountyDemo.R.}

\item{updateID}{Character string that will become the `updateID` column for this table. For example, "survey11Add" for Survey 11.}
}
\description{
Function to create the DIALECT_REGIONS database table.
}
