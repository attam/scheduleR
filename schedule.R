library(timeDate)
library(chron)
library(bizdays)
library(stringr)
library(lubridate)

# CMH calendar
holidays.fun <- function(x) {
  c(USNewYearsDay = USNewYearsDay(x),
    USMemorialDay = USMemorialDay(x),
    USIndependenceDay=USIndependenceDay(x),
    USLaborDay=USLaborDay(x),
    USThanksgivingDay=USThanksgivingDay(x),
    USChristmasDay = USChristmasDay(x)
  )
}
create.calendar("CMH",holidays=sort(holidays.fun(2017:2020)),weekdays=c("saturday","sunday"))
create.calendar("CMH_call_inpatient",holidays=sort(holidays.fun(2017:2020)),weekdays=c("friday","saturday","sunday"))
create.calendar("CMH_call_weekend",holidays=sort(holidays.fun(2017:2020)),weekdays=c("monday","tuesday","wednesday","thursday"))

setClass("Fellow", representation(name="character", startdate="POSIXct", blocks="list"))
setClass("block",representation(assignee="Fellow",assignment="character",startdate="Date",enddate="Date"))

create.blocks<-function(schedule,fellow){
  return(mapply(function(x,y,z) new("block",assignee=fellow,assignment=as.character(z),startdate=x,enddate=y), x=schedule[,1],y=schedule[,2],z=schedule[,3]))
}

fellowYear<-function(startdate) {
  currentYear<-as.numeric(ceiling(as.numeric((Sys.Date()-startdate)/365)))
  return(currentYear)
}

convertblocks<-function(schedule,name){
  l<-length(schedule[,1])
  assignments<-schedule[,name]
  block_a<-getdate("first day",ref(paste(substr(schedule[seq(1,l,2),1],4,7),"-",str_pad(match(substr(schedule[seq(1,l,2),1],1,3),month.abb),2,pad="0"),sep=""),"month"),cal="CMH")
  block_b<-getdate("last day",ref(paste(substr(schedule[seq(2,l,2),1],4,7),"-",str_pad(match(substr(schedule[seq(2,l,2),1],1,3),month.abb),2,pad="0"),sep=""),"month"),cal="CMH")
  blockdates<-sort(c(block_a,block_b))
  blockdates.2<-as.Date(sapply(blockdates,function(x) getblockdates(x,blockDates=T)),"1970-01-01")
  scheduleout<-cbind.data.frame(blockdates.2[1,],blockdates.2[2,],assignments)
  names(scheduleout)<-c("block_start","block_end","assignment")
  return(scheduleout)
}

create.schedule<-function(schedule){
  
}

whichblock<-function(date,schedule,name){
  temp<-getblockdates(date,blockDates=T)[1]
  schedule[match(temp,schedule[,1]),3]
}



# given a date, returns either the start/end dates of the corresponding block
# or the length (in days) of the block
getblockdates<- function(date,nDays=NULL,blockDates=NULL) {
  firstDaym<-getdate("first day",ref(date,"month"),cal="CMH")
  lastDaym<-getdate("last day",ref(date,"month"),cal="CMH")
  ndays<-numberOfDays(as.Date(firstDaym,format="%Y-%m-%d"))
  block1length<-ifelse(ndays%%2==0,ndays%/%2,ndays%/%2+1)
  block1start<-firstDaym
  block1end<-firstDaym+block1length-1
  blockn<-ifelse(date<=block1end,1,2)
  if (blockn==1){
    blocklength<-block1length
    blockdates<-c(block1start,block1end)
  } else {
    block2start<-firstDaym+block1length
    block2end<-lastDaym
    blocklength<-ndays-block1length
    blockdates<-c(block2start,block2end)
  }
  if(!is.null(nDays)) return(blocklength)
  if(!is.null(blockDates)) return(blockdates)
}


# returns vector containing block work days for a given date
blockdays<-function(date){
d<-getblockdates(date,blockDates=T)
return(bizseq(d[1],d[2],"CMH"))
}

#returns vector containing start and end date/time of call for a given inpatient block date while
callevent<-function(date){
d<-getblockdates(date,blockDates=T)
bizseq(d[1],d[2],"CMH_call_inpatient")
}

library(lubridate)
getcallweekends<-function(date){
  bdates<-getblockdates(date,blockDates=T)
  first<-bdates[1]
  last<-bdates[2]
  numfri<-ceiling(as.numeric(last + 1 - 5 + 4) / 7) - ceiling(as.numeric(first - 5 + 4) / 7)
  wkds<-list()
  for(i in 1:numfri) {
    wkds[[i]]<-bizseq(first+(i-1)*7,first+7*i-1,cal="CMH_call_weekend")
  }
  return(wkds)
}

geteligibleweekends<-function(block){
  if (block@assignment!="inpatient") {
    return(getcallweekends(block@startdate))
  } else {return (NA)}
}

numberOfDays <- function(date) {
  m <- format(date, format="%m")
  
  while (format(date, format="%m") == m) {
    date <- date + 1
  }
  
  return(as.integer(format(date - 1, format="%d")))
}

ref(paste("2018-",str_pad(1:12, 2, pad = "0"),sep=""),"month")




multi.sapply <- function(...) {
  arglist <- match.call(expand.dots = FALSE)$...
  var.names <- sapply(arglist, deparse)
  has.name <- (names(arglist) != "")
  var.names[has.name] <- names(arglist)[has.name]
  arglist <- lapply(arglist, eval.parent, n = 2)
  x <- arglist[[1]]
  arglist[[1]] <- NULL
  result <- sapply(arglist, function (FUN, x) sapply(x, FUN), x)
  colnames(result) <- var.names[-1]
  return(result)
}


