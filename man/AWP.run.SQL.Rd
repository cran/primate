\name{AWP.run.SQL}
\alias{AWP.run.SQL}
\title{Run SQL querries}
\description{
Run arbitrary SQL querries on the All the World's Primate database
}
\usage{
AWP.run.SQL(con=AWP.connect(), sql=NULL)
}
\arguments{
\item{con}{connection object}
\item{sql}{SQL string}
}
\value{
results of query
}
\examples{
\donttest{
AWP.run.SQL(con=AWP.connect(), sql=NULL)
}
}
