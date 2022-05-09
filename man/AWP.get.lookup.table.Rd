\name{AWP.get.lookup.table}
\alias{AWP.get.lookup.table}
\title{Get a Lookup Table}
\description{
Get the lookup table from the All the World's Primates SQL database
}
\usage{
AWP.get.lookup.table(con=AWP.connect(),tab.nm="TextType")
}
\arguments{
\item{con}{connection object}
\item{tab.nm}{table name (for the parent table)}
}
\value{
a data.frame corresponding to SQL table
}
\examples{
\donttest{
AWP.get.lookup.table(con=AWP.connect(),tab.nm="TextType")
}
}
