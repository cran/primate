

regroup.gnsp <- function(df,clmns,agg='mean',by='gn_sp') {
  df$PrimateID <- rownames(df)
  df <- add.gnsp.clmn(df)
  groupBy(df=df,aggregation=agg,by=by,clmns=clmns)
}


regroup.equivalent  <- function(df, gnsp.old, gnsp.new, clmns, agg='mean', direction='old2new'){
  if(direction=='old2new')
    groupBy(df=df,aggregation=agg,by=gnsp.new,clmns=clmns)
  else #(direction=='new2old')
    groupBy(df=df,aggregation=agg,by=gnsp.old,clmns=clmns)
} # another option is to just update the gnsp column values with the new names of one or the other predecessory tables and remerge based on it




