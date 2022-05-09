\name{AWP.read.pkg.tab}
\alias{AWP.read.pkg.tab}
\title{Read a table from the All the World's primates example microarchive within this package}
\description{
Read an All the World's Primates table from the local package cache.
}
\usage{
AWP.read.pkg.tab(tab.nm='dbo_tblGrovesMonkeys', id.clmn=NA)
}
\arguments{
\item{tab.nm}{table name}
\item{id.clmn}{id column of table}
}
\value{
data.frame corresponding to SQL table
}
\examples{

primates.tab <- AWP.read.pkg.tab(tab.nm='dbo_tblGrovesMonkeys', id.clmn='MonkeyNumberGroves')


}
