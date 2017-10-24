getAttr=function(d){
  if(is.character(d)){
    if(grepl("\\(",d))return(NULL)
    Dname=d
    d=get(d)
  }else{
    Dname=deparse(substitute(d))
  }
  if(!is.data.frame(d))return(NULL)
  colClass=sapply(d,class)
  N=function(cl)sum(sapply(d,inherits,cl))
  Nnum=N("numeric")
  Nfac=N("factor")
  Norder=N("ordered")
  Nlogical=N("logical")
  NBfac=ifelse(Nfac==0,0,sum(sapply(d,nlevels)==2))
  Nmulti=ifelse(Nfac==0,0,sum(sapply(d,nlevels)>2))
  Nchar=N("character")
  NAsIs=N("AsIs")
  NDate=N("Date")
  Ntime=N(c("POSIXt","POSIXct","POSIXlt"))
  
  out=data.frame(dataset=Dname,
             row=nrow(d),
             col=ncol(d),
             numeric=Nnum,
             factor=Nfac,
             ordered=Norder,
             logical=Nlogical,
             binary_factor=NBfac,
             multi_factor=Nmulti,
             char=Nchar,
             asis=NAsIs,
             date=NDate,
             time=Ntime)
  
  colnames(out)=gsub("_","\n",colnames(out))
  out
}

dd=data()$results[,"Item"]

dt=do.call(rbind,Filter(function(x)!is.null(x),sapply(dd,function(x)getAttr(x))))

write.csv(dt,file="R_DF_Attributes.csv",row.names=F)
