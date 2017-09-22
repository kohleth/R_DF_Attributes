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
  
  data.frame(dataset=Dname,
             N_row=nrow(d),
             N_col=ncol(d),
             N_numeric=Nnum,
             N_factor=Nfac,
             N_ordered=Norder,
             N_logical=Nlogical,
             N_binary_factor=NBfac,
             N_multi_factor=Nmulti,
             N_char=Nchar,
             N_asis=NAsIs,
             N_date=NDate,
             N_time=Ntime)
}

dd=data()$results[,"Item"]

do.call(rbind,Filter(function(x)!is.null(x),sapply(dd,function(x)getAttr(x))))