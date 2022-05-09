\name{AWP.get.SQL.table}
\alias{AWP.get.SQL.table}
\title{Get a SQL Table}
\description{
Retrieve a table from the All the World's Primates SQL database
}
\usage{
AWP.get.SQL.table(con=AWP.connect(), tab.nm="tblGrovesMonkeys", clmns=c('all'),xpnd=FALSE)
}
\arguments{
\item{tab.nm}{table name (defaults to the main primate species list)}
\item{con}{connection object}
\item{clmns}{columns to return}
\item{xpnd}{expand the lookup column codes into full text strings}
}
\value{
a data.frame corresponding to SQL table
}
\examples{
\donttest{
online.version <- AWP.get.SQL.table(tab.nm='LMType') #a small example table
}
}
