\name{AWP.list.SQL.tables}
\alias{AWP.list.SQL.tables}
\title{List the SQL tables}
\description{
List available tables from the All the World's Primates SQL database
}
\usage{
AWP.list.SQL.tables(con=AWP.connect(), all=FALSE)
}
\arguments{
\item{con}{connection object from AWP.connect}
\item{all}{list all tables available}
}
\value{
a list (vector) of SQL table names 
}
\examples{
\donttest{
AWP.list.SQL.tables(con=AWP.connect(), all=FALSE)
}
}
