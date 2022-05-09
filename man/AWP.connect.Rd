\name{AWP.connect}
\alias{AWP.connect}
\title{Create a connection to the SQL DB}
\description{
Connect to the All the World's Primate Database
}
\usage{
AWP.connect(drv=AWP.driver(), prefix='jdbc:jtds:sqlserver', 
            server="s09.everleap.com", port=1433,
            db="DB_3918_atwpreview", user="DB_3918_atwpreview_user", pw="atwpreview_$_j")
}
\arguments{
\item{drv}{driver (output from AWP.driver()}
\item{prefix}{prefix to the URL (before "://")}
\item{server}{domain name for the database server}
\item{port}{port name used by the server's database}
\item{db}{database name}
\item{user}{database user name}
\item{pw}{database user password}
}
\value{
a connection object for SQL
}
\examples{
\donttest{
con <- AWP.connect(drv=AWP.driver()) 
}
}
