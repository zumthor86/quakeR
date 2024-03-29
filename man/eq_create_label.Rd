% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/eq_create_label.R
\name{eq_create_label}
\alias{eq_create_label}
\title{Create informative popup label from quakes dataframe}
\usage{
eq_create_label(df)
}
\arguments{
\item{df}{NOAA quake dataframe containing LOCATION_NAME, EQ_PRIMARY, TOTAL_DEATHS columns}
}
\value{
Character vector of HTML
}
\description{
Create informative popup label from quakes dataframe
}
\examples{
\dontrun{

 quakes_df$popup_text = eq_create_label(quakes_df))

}


}
