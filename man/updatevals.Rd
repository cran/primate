\name{updatevals}
\alias{updatevals}
\title{Update the values of an AWP data.frame}
\description{
Update the values in an old dataframe with the values in a new dataframe.
Useful for comparing a pre-existing or self-assembled dataset with AWP.
}
\usage{
updatevals(x,y=NULL,v.old,v.new,verbose=TRUE,update=FALSE,
                    na.only=TRUE,all=TRUE,missing.only=TRUE)
}
\arguments{
\item{x}{first dataframe}
\item{y}{second dataframe}
\item{v.old}{variable old}
\item{v.new}{variable new}
\item{verbose}{get all the messages}
\item{update}{update all the old with everything new}
\item{na.only}{just update the missing values in the old dataframe }
\item{all}{perform merge on all columns}
\item{missing.only}{update only those that have missing values}
}
\value{
values of one data frame are updated to reflect new data in another
}
\examples{
pri.tab <- AWP.read.pkg.tab(tab.nm='Locomotion')
#pri.AWP <- AWP.get.SQL.table(tab.nm='Locomotion') 

dim(pri.tab) #should may be fewer cols or rows locally ...
#dim(pri.AWP) # than there are available online.

apply(pri.tab, 2, function(x) sum(is.na(x))) # also more missing values
#apply(pri.AWP, 2, function(x) sum(is.na(x))) #  locally than online

# update the "Comment" column locally with the same online

vars <- c('LocomotionID','Comment')
#tmp <- merge(x=pri.tab[,c(vars)] ,y=pri.AWP[,c(vars)], by='LocomotionID')

#out <- updatevals(x=tmp,y=NULL,v.old='Comment.x',v.new='Comment.y')

}
