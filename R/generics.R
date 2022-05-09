

add.gnsp.clmn <- function(df,gn="Genus",sp="Species",rownames=FALSE, new.col=TRUE, gnsp.col='gn_sp'){
  df[,gnsp.col] <- apply(df[,c(gn,sp)],1,function(x) paste(x, collapse='_'))
  if(rownames){ rownames(df) <- df[,gnsp.col]}
  if(!new.col){ df[,gnsp.col] <- NULL }
  df
}


