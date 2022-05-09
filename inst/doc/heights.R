### R code from vignette source 'heights.Rnw'
### Encoding: UTF-8

###################################################
### code chunk number 1: foo
###################################################
options(keep.source = TRUE, width = 60)
foo <- packageDescription("primate")


###################################################
### code chunk number 2: loadlib
###################################################
library(primate)


###################################################
### code chunk number 3: read
###################################################

primates.tab <- AWP.read.pkg.tab(tab.nm='dbo_tblGrovesMonkeys', id.clmn='MonkeyNumberGroves')
heights.tab <- AWP.read.pkg.tab(tab.nm='HMeasure')
locomo.tab <- AWP.read.pkg.tab(tab.nm='Locomotion')
locomot.types <- AWP.read.pkg.tab(tab.nm='dbo_LMType')

taxa.clmns <- c('Superfamily','Family','Genus','Species')


###################################################
### code chunk number 4: ti
###################################################

### HEIGHTS ###
getHMmeans <- function(code){
 ret <- groupBy(df=heights.tab[heights.tab$HMTypeCd==code,], aggregation='mean', by='PrimateID', clmns='Mean')
 names(ret) <- code; return(ret)
}

heights <- list()
levels <- c('GRND','G5','510','1020','2030','3040','40+')   
for(lev in levels)
  heights[[lev]] <- getHMmeans(lev)  #not sure why these numbers are so high... are they observations?
#names(heights) <- c(paste('h',names(heights)[1:6],sep=''),'h40up')

AWPheight <- nerge(heights)
AWPheightsp <- nerge(list(h=tab2df(AWPheight), p=primates.tab[,taxa.clmns]))
AWPheightgs <- regroup.gnsp(df=AWPheightsp,clmns=colnames(AWPheightsp)[1:7])

names(AWPheightgs) <- c(sub('X','h',x=names(AWPheightgs)[-ncol(AWPheightgs)]),"h40up")
h.levs <- names(AWPheightgs)



### LOCOMOTION ###
AWPlocLst<-list()
loc.modes <- c("BRAC","CLIM","LEAP","QUAD","SUSP")
for(loc in loc.modes){
                                 dfsub=locomo.tab[locomo.tab$LMTypeCd==loc,]
  AWPlocLst[[loc]] <- groupBy(df=dfsub,aggregation='mean',by='PrimateID',clmns='PctOfTime')
}
AWPlocDF <- nerge(AWPlocLst, all=T)
AWPlocDFsp <- nerge(list(AWPlocDF, primates.tab[,taxa.clmns]))

## add primate names and re-group by them instead (the ids are non-unique at the genus_species level)
loc.pct.modes <- paste("PctOfTime",loc.modes,sep='.')
AWPlocDFgs <- regroup.gnsp(df=AWPlocDFsp,clmns=loc.pct.modes)



###################################################
### code chunk number 5: merge
###################################################

lh <- loc.high <- nerge(list(h=AWPheightgs,l=AWPlocDFgs))
risky.modes <- loc.pct.modes[c(1,3)]
tmp <- lapply(h.levs, function(x) {ret <- subset(loc.high,get(x)>3 ,risky.modes); names(ret) <-sub('PctOfTime\\.','',x=risky.modes);ret})
names(tmp)<- h.levs


###################################################
### code chunk number 6: fig
###################################################
oldpar <- par(mfrow=c(1,length(tmp)), mar=c(2,2,3,0))
sapply(names(tmp), function(i){boxplot(tmp[[i]], ylim=c(0,100), varwidth=T, main=i)})
par(oldpar)


###################################################
### code chunk number 7: fig
###################################################
oldpar <- par(mfrow=c(1,length(tmp)), mar=c(2,2,3,0))
sapply(names(tmp), function(i){boxplot(tmp[[i]], ylim=c(0,100), varwidth=T, main=i)})
par(oldpar)


