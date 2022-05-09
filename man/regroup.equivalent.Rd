\name{regroup.equivalent}
\alias{regroup.equivalent}
\title{Re-group data.frame by Genus_species in either old or new dataframe}
\description{
Regroup based on the old or the new data frame using a direction parameter.
}
\usage{
regroup.equivalent(df, gnsp.old, gnsp.new, clmns, agg='mean', direction='old2new')
}
\arguments{
\item{df}{a dataframe}
\item{gnsp.old}{old nomenclature}
\item{gnsp.new}{new nomenclature}
\item{clmns}{the columns in the data.frame to re-group}
\item{agg}{the aggregation type}
\item{direction}{the aggregation priority}
}
\value{
a regrouped data frame
}
\examples{

primates.tab <- AWP.read.pkg.tab(tab.nm='dbo_tblGrovesMonkeys', id.clmn='MonkeyNumberGroves')
primates.tab <- add.gnsp.clmn(primates.tab,gn="Genus",sp="Species",rownames=FALSE,gnsp.col='gn_sp')
primates.tab <- add.gnsp.clmn(primates.tab,gn="Genus",sp="Species",rownames=FALSE,gnsp.col='gnspg')
pri.grpd <- regroup.gnsp(df=primates.tab,clmns=colnames(primates.tab), agg='max')
out <- regroup.equivalent(pri.grpd, gnsp.old=gn_sp, gnsp.new='gnspg', 
                                    clmns='MonkeyNumberGroves', agg='paste', direction='old2new')

}
