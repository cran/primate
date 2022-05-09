


AWP.read.pkg.tab <- function(tab.nm='dbo_tblGrovesMonkeys', id.clmn=NA){
 file.name <- paste(tab.nm,'csv',sep='.')
 pkg.full.path.nm <- system.file('extdata','AWP.exports', file.name , package='primate')
 tab <- read.csv(pkg.full.path.nm, stringsAsFactors=F)
 if(!is.na(id.clmn))
   rownames(tab) <- tab[,id.clmn]
 return(tab)
}



#==============================================#
#====================  SQL ====================#
#==============================================#


#https://www.microsoft.com/en-us/download/details.aspx?id=11774
#drv <- JDBC("com.microsoft.sqlserver.jdbc.SQLServerDriver",
# "/home/mint/Documents/sqljdbc_6.0/enu/jre8/sqljdbc42.jar")


AWP.driver <-  function(drv.name="net.sourceforge.jtds.jdbc.Driver", 
                        drv.file=system.file("drivers","jtds-1.2.8.jar", 
                        package='primate')){
   requireNamespace('RJDBC')
   #http://sourceforge.net/projects/jtds/files/jtds/1.2.8/jtds-1.2.8-dist.zip/download
   RJDBC::JDBC(drv.name, drv.file)
}

AWP.connect <- function(drv=AWP.driver(), prefix='jdbc:jtds:sqlserver', 
                                          server="s09.everleap.com", 
                                          port=1433, 
                                          db="DB_3918_atwpreview", 
                                          user="DB_3918_atwpreview_user", 
                                          pw="atwpreview_$_j"){
   requireNamespace('RJDBC')
   RJDBC::dbConnect(drv=drv, paste(prefix,"://",server,":",port,";",sep=''), databaseName=db, user=user, password=pw)
}

AWP.list.SQL.tables <- function(con=AWP.connect(), all=FALSE){
   requireNamespace('RJDBC')
   tab.list <- RJDBC::dbListTables(con) #1-29, 87-97
   RJDBC::dbDisconnect(con)
   keep <- rep(T,length(tab.list))
   if(!all){
     user.tabs <- c('Behavior', 'BType', 'CitationType', 'Conservation', 'Food', 'FoodType', 'Habitat', 'HabType', 'HMeasure', 'HMType', 'Hybridization', 'IUCN', 'LHType', 'LifeHistory', 'LMType', 'Locomotion', 'Physical', 'PMType', 'SMeasure', 'SMType', 'Structure', 'StructureType', 'tblCitation', 'tblCommonNames', 'tblContributorsandMonks', 'AuthorAcctGen', 'tblGrovesMonkeys', 'tblUniquePlus', 'Text', 'TextType')
     keep <- sapply(tab.list, function(t)  any(sapply(user.tabs, function(p) grepl(t, pattern=p))))
     if(length(keep) < length(user.tabs))
       warning("reported table list is shorter than available table names: some table names may have changed")
   }
   return(tab.list[keep])
}

AWP.get.SQL.table <- function(con=AWP.connect(), tab.nm='tblGrovesMonkeys', clmns=c('all'),xpnd=FALSE){
   requireNamespace('RJDBC')
   #RJDBC::dbListFields(conn=con)
   tab <- RJDBC::dbReadTable(conn=con, name=tab.nm)
   if(xpnd){
      lu.tab <- AWP.get.lookup.table(con=con, tab.nm) 
      tab <- merge(x=tab, y=lu.tab)
   }
   RJDBC::dbDisconnect(con)
   return(tab)
}

AWP.get.lookup.table <- function(con=AWP.connect(),tab.nm="TextType"){
   requireNamespace('RJDBC')
   lu.tab.nm <- paste('dbo_',tab.nm,sep='')
   tab <- AWP.get.SQL.table(con=con, lu.tab.nm)
   RJDBC::dbDisconnect(con)
   return(tab)
}

AWP.run.SQL <- function(con=AWP.connect(), sql=NULL){   
   print('not implimented yet')
   if(FALSE){
    #requireNamespace('RJDBC')
    #RJDBC::dbSendQuery(conn=con, sqls) 
    #RJDBC::dbExecute(conn=con,sql)
   }
}



# prerequisites
# sudo apt-get install r-cran-rodbc  # sql.h and sqlext.h <- 
# install.packages('RODBC','magrittr','mssqlR')
#  OR
# apt-get install unixodbc-dev  ##tdsodbc   # compile headers and connection drivers
# install.packages('odbc')
# msodbcsql17 libssl1.0.0
#apt-get install unixodbc-dev tdsodbc msodbcsql17 libssl1.0.0
