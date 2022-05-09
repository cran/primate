\name{add.gnsp.clmn}
\alias{add.gnsp.clmn}
\title{Add a genus species column}
\description{
Adds a genus_species column to the specified dataframe
}
\usage{
add.gnsp.clmn(df,gn="Genus",sp="Species",rownames=FALSE,new.col=TRUE, gnsp.col = "gn_sp")
}
\arguments{
\item{df}{input data.frame}
\item{gn}{column name for genus}
\item{sp}{column name for species}
\item{rownames}{use the new gn_sp column to assign data.frame rownames}
\item{new.col}{TRUE if gn_sp column is to be retained, FALSE if it is to be removed}
\item{gnsp.col}{the name of the new column to add, by default is "gn_sp"}
}
\value{
modified data.frame (with genus species info concatenated and added)
}
\examples{
primates.tab <- AWP.read.pkg.tab(tab.nm='dbo_tblGrovesMonkeys', id.clmn='MonkeyNumberGroves')
primates.tab <- add.gnsp.clmn(primates.tab,gn="Genus",sp="Species",rownames=FALSE,new.col=TRUE)
}
