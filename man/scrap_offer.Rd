% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/scraper_olx.R
\name{scrap_offer}
\alias{scrap_offer}
\title{Scrapes olx flat rental offer.}
\usage{
scrap_offer(url)
}
\arguments{
\item{url}{url to olx offer}
}
\value{
tibble with data about specific offer
}
\description{
Scrapes olx flat rental offer and returns tibble containing alee
  information about offer. If some data is missing NA is imputed. Works for
  polish version of olx.
}
