% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/makeTables.R
\name{makeUpdateMetadata}
\alias{makeUpdateMetadata}
\title{Make the UPDATE_METADATA table}
\usage{
makeUpdateMetadata(updateID, date, updater, description, con, sourceCode)
}
\arguments{
\item{updateID}{Character string that will become the `updateID` column for this table. For example, "survey11Add" for Survey 11.}

\item{date}{Date of the update, in the format YYYY-MM-DD}

\item{updater}{Name of the person doing the update (first and last), e.g. Kaija Gahm}

\item{description}{Description of the update. For a standard database update, it's just "added [surveyID]".}

\item{con}{A connection to the existing database, created with RSQLite.}

\item{sourceCode}{The name of the R script used to perform this update.}
}
\description{
This function creates the row of the UPDATE_METADATA table that corresponds to the updateID that we've been using for all the other database tables.
}
