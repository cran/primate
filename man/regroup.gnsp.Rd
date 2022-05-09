\name{regroup.gnsp}
\alias{regroup.gnsp}
\title{Re-group data.frame by Genus_species}
\description{
Regroup a given data.frame by a column designated as unique genus_species combination. This function is essentially a wrapper for caroline:::groupBy()
}
\usage{
regroup.gnsp(df,clmns,agg='mean',by='gn_sp')
}
\arguments{
\item{df}{a dataframe}
\item{clmns}{columns}
\item{agg}{type of aggregation to be used}
\item{by}{the column name by which the data.frame should be re-grouped}
}
\value{
returned value
}
\examples{


primates.tab <- AWP.read.pkg.tab(tab.nm='dbo_tblGrovesMonkeys', id.clmn='MonkeyNumberGroves')
out <- regroup.gnsp(df=primates.tab,clmns=colnames(primates.tab), agg='paste')

}
