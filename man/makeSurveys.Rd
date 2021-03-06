% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/makeTables.R
\name{makeSurveys}
\alias{makeSurveys}
\title{Make the SURVEYS table}
\usage{
makeSurveys(df, surveyID, updateID, admin, surveyName)
}
\arguments{
\item{df}{Raw survey data as a data frame.}

\item{surveyID}{Character string that will become the `surveyID` column for this table. For example, "S11" for Survey 11.}

\item{updateID}{Character string that will become the `updateID` column for this table. For example, "survey11Add" for Survey 11.}

\item{admin}{Character string: name of the person who administered the survey. For example, "Jim Wood".}

\item{surveyName}{Character string naming the survey (this is an opportunity to be more descriptive than `surveyID` allows.) For example, the `surveyName` for Survey 11 might be "Survey 11". Or, if you did a survey focused on a particular construction, you might call it "Needs Washed Survey".}
}
\description{
This function creates the SURVEYS database table from raw survey data.
}
