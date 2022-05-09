
updatevals <- function(x, y=NULL, v.old, v.new, verbose=TRUE,update=FALSE,na.only=TRUE, all=TRUE, missing.only=TRUE){
  if(!is.null(y))
    x <- nerge(list(x, y), all=all)

  for(v.i in 1:length(v.old)){
    print(v.old[v.i])
    old.vals.missing <- is.na(x[,v.old[v.i]])
    new.vals.missing <- !is.na(x[,v.new[v.i]])
    if(missing.only){
        sp.w.new.vals <- old.vals.missing & !is.na(x[,v.new[v.i]])
    }else{ #(missing.only=F)
        vals.not.equal <- x[,v.old[v.i]]!=x[,v.new[v.i]]
        sp.w.new.vals <- (old.vals.missing | vals.not.equal) & new.vals.missing
    }
    ## how to handle when different variable types?     
   print(paste('found',sum(sp.w.new.vals),'new values'))
   if(verbose)
      print(rownames(x[sp.w.new.vals,]))
   if(update)
    x[sp.w.new.vals,v.old[v.i]] <- x[sp.w.new.vals,v.new[v.i]]

  }#end for
  if(update)
    return(x) #perhaps without v.new columns?
}


