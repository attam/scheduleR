library(timeDate)
library(chron)
library(bizdays)
library(stringr)
library(lubridate)
library(tibble)
library(dplyr)
library(reshape2)

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
setClass("block",representation(assignee="character",assignment="character",startdate="Date",enddate="Date"))

create.blocks<-function(schedule){
  return(mapply(function(x,y,z,v) new("block",assignee=v,assignment=z,startdate=x,enddate=y), x=schedule[,1],y=schedule[,2],z=as.character(schedule[,3]),v=schedule[,4]))
}

fellowYear<-function(startdate) {
  currentYear<-as.numeric(ceiling(as.numeric((Sys.Date()-startdate)/365)))
  return(currentYear)
}

convertblocks<-function(schedule,name){
  schedule<-read.csv(schedule, stringsAsFactors=FALSE)
  l<-length(schedule[,1])
  assignments<-schedule[,name]
  block_a<-getdate("first day",ref(paste(substr(schedule[seq(1,l,2),1],4,7),"-",str_pad(match(substr(schedule[seq(1,l,2),1],1,3),month.abb),2,pad="0"),sep=""),"month"),cal="CMH")
  block_b<-getdate("last day",ref(paste(substr(schedule[seq(2,l,2),1],4,7),"-",str_pad(match(substr(schedule[seq(2,l,2),1],1,3),month.abb),2,pad="0"),sep=""),"month"),cal="CMH")
  blockdates<-sort(c(block_a,block_b))
  blockdates.2<-as.Date(sapply(blockdates,function(x) getblockdates(x,blockDates=T)),"1970-01-01")
  scheduleout<-cbind.data.frame(blockdates.2[1,],blockdates.2[2,],assignments)
  scheduleout$fellow<-name
  names(scheduleout)<-c("block_start","block_end","assignment","fellow")
  return(scheduleout)
}

displayschedule<-function(schedule){
  return(dcast(schedule,block_start+block_end~assignment, fun.aggregate=list))
}

getassignments<-function(schedule,month,year){
  months<-month(ymd(as.Date(schedule[,1])))
  years<-year(ymd(as.Date(schedule[,1])))
  return(schedule[which(months==month & years==year),])
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

make.schedule<-function(schedule) {
  sched<-NA
  l<-length(schedule[,1])
  for (i in 1:l) {
    sched<-rbind(sched,(cbind(blockdays(schedule[i,1]),schedule[i,3:4])))
  }
  sched<-as_tibble(sched[-c(1),])
  names(sched)<-c("date","assignment","fellow")
  sched<-dcast(sched,date~assignment, value.var="fellow", fun.aggregate = list)
  sched$call<-ifelse(weekdays(sched$date)!="Friday",sched$inpatient,NA)
  return(sched)
}

getnames<-function(call_schedule){
  list(names(call_schedule)[which(!is.na(call_schedule[2:4]))+1])
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
  temp<-bizseq(first,last,cal="CMH_call_weekend")
  wkd_starts<-temp[which(weekdays(temp)=="Friday")]
  wkds<-mapply(function(x) seq(x,x+2,1),wkd_starts)
  return(wkds)
}

rearrange<-function(callschedule){
  l<-which(!is.na(callschedule$wkdcall))
  output<-NA
  for (i in 1:length(l)) {
    output<-rbind(output,cbind(unlist(callschedule$wkdcall[l[i]]),callschedule$fellow[l[i]]))
  }
  output<-as_tibble(output[-c(1),])
  names(output)<-c("date","fellow")
  output$date<-as.Date(as.numeric(output$date),origin="1970-01-01")
  output$week<-week(output$date)
  output<-dcast(output,date~fellow)
  return(output)
}

geteligibleweekends<-function(schedule){
  l<-length(schedule[,1])
  wkdout<-list()
  for (i in 1:l){
    wkdout[i]<-ifelse(schedule[i,3]!="inpatient",list(getcallweekends(schedule[i,1])),NA)
  }
  schedule$wkdcall<-wkdout
  return(schedule)
}

wkdcall<-function(call_schedule){
  l<-length(call_schedule[,1])
  temp<-NA
  for (i in 1:l){
    temp[i]<-paste(names(call_schedule)[which(!is.na(call_schedule[i,2:4]))+1],collapse=' or ')
  }
  call_schedule$call<-temp
  return(call_schedule[,c(1,5)])
}
numberOfDays <- function(date) {
  m <- format(date, format="%m")
  
  while (format(date, format="%m") == m) {
    date <- date + 1
  }
  
  return(as.integer(format(date - 1, format="%d")))
}

readfellow <- function(holiday)
{ 
  n <- readline(prompt=paste("Enter an fellow who will be on call for",holiday,": "))
  return(as.character(n))
}

holiday_call<-function(year) {
  holidays<-as.Date(holidays.fun(year))
  fellows<-NA
  for (i in 1:6){
    fellows[i]<-readfellow(holidays[i])
  }
  temp<-as_tibble(cbind(as.Date(holidays.fun(2019)),matrix(t(fellows),length(fellows),3)))
  names(temp)<-c("date","inpatient","esrd","call")
  temp$date<-as.Date(as.numeric(temp$date),origin="1970-01-01")
  temp
  return(temp)
}

add_holidays<-function(holiday_schedule,regular_schedule){
  holiday_schedule$research<-NA
  holiday_schedule$path<-NA
  holiday_schedule$orientation<-NA
  temp<-rbind(holiday_schedule %>% select(date,esrd,inpatient,research,path,orientation,call),regular_schedule %>% arrange(date))
  temp$call[match(holiday_schedule$date,temp$date)-1]<-temp$inpatient[match(holiday_schedule$date,temp$date)]
  return(temp)
}

clean.schedule<-function(schedule){
  schedule$research[which(sapply(schedule$research,function(x) is.null(x)|identical(x,character(0))))]<-NA
  schedule$path[which(sapply(schedule$path,function(x) is.null(x)|identical(x,character(0))))]<-NA
  schedule$orientation[which(sapply(schedule$orientation,function(x) is.null(x)|identical(x,character(0))))]<-NA
  schedule$esrd[which(sapply(schedule$esrd,function(x) is.null(x)|identical(x,character(0))))]<-NA
  schedule$inpatient[which(sapply(schedule$inpatient,function(x) is.null(x)|identical(x,character(0))))]<-NA
  return(schedule)
}

make.csv<-function(schedule,fellow,type){
  temp<-NA
    temp$Startdate<-schedule$date[which(sapply(t(schedule[,which(names(schedule)==type)]),function(x) match(fellow,unlist(x)))==1)]
    temp$Starttime<-ifelse(type=="call","05:00 PM","07:00 AM")
    temp$Enddate<-temp$Startdate
    if (type=="call") temp$Enddate<-temp$Enddate+1
    temp$Endtime<-ifelse(type=="call","07:00 AM", "05:00 PM")
    temp$Subject<-switch(type,"call"="On call","inpatient"="Inpatient","esrd"="Dialysis/Transplant","research"="Research","path"="Pathology","orientation"="Orientation")
  temp$all_day<-"FALSE"
  temp$desc<-fellow
  temp<-as.data.frame(temp) %>% select(Subject,Startdate,Starttime,Enddate,Endtime,all_day,desc)
  names(temp)<-c("Subject","Start Date","Start Time","End Date","End Time","All Day Event","Description")
  write.csv(temp,file=paste(fellow,"_",type,".csv",sep=""),row.names=FALSE)
  return(temp)
}

get.events<-function(schedule,fellow){
  subject<-names(schedule)[which(!is.na(match(unlist(schedule[1,]),fellow)))]
  temp<-sapply(subject,function(x) cbind(row.names=FALSE,x,as.Date(as.numeric(schedule[1]),origin="1970-01-01"),as.character(ifelse(x=="call","05:00 PM","07:00 AM")),as.Date(as.numeric(ifelse(x=="call",schedule[1]+1,schedule[1])),origin="1970-01-01"),as.character(ifelse(x=="call","07:00 AM","05:00 PM")),fellow,stringsAsFactors=FALSE))
  temp<-as_tibble(t(temp)) %>% select(V2,V3,V4,V5,V6,V7,V8)
  names(temp)<-c("Subject","Start Date","Start Time","End Date","End Time","Description","All Day Event")
  temp$`Start Date`<-as.Date(as.numeric(temp$`Start Date`),origin="1970-01-01")
  temp$`End Date`<-as.Date(as.numeric(temp$`End Date`),origin="1970-01-01")
  return(temp)
}


# make.csv2<-function(schedule,fellow){
#   cbind(Subject=schedule$)
#   temp$Startdate<-schedule$date[which(sapply(t(schedule[,which(names(schedule)==type[i])]),function(x) match(fellow,unlist(x)))==1)]
#   temp$Starttime<-ifelse(type[i]=="call","05:00 PM","07:00 AM")
#   temp$Enddate<-temp$Startdate
#   if (type[i]=="call") temp$Enddate<-temp$Enddate+1
#   temp$Endtime<-ifelse(type[i]=="call","07:00 AM", "05:00 PM")
#   temp$Subject<-switch(type[i],"call"="On call","inpatient"="Inpatient","esrd"="Dialysis/Transplant","research"="Research","path"="Pathology","orientation"="Orientation")
#   temp2<-rbind(temp2,temp)
#   }
#   temp2$all_day<-"FALSE"
#   temp2$desc<-fellow
#   temp2<-as.data.frame(temp2) %>% select(Subject,Startdate,Starttime,Enddate,Endtime,all_day,desc)
#   names(temp2)<-c("Subject","Start Date","Start Time","End Date","End Time","All Day Event","Description")
#   write.csv(temp2,file=paste(fellow,"_.csv",sep=""),row.names=FALSE)
#   return(temp2)
# }


blockschedule_file<-"blockschedule.csv"
sched<-convertblocks(blockschedule_file,"Matta")
sched<-rbind(sched,convertblocks(blockschedule_file,"Singh"))
sched<-rbind(sched,convertblocks(blockschedule_file,"Morgans"))
x<-make.schedule(sched)
x$call<-as.character(x$call)
y<-geteligibleweekends(sched)
z<-rearrange(y)
zz<-wkdcall(z)
zzz<-full_join(x,zz) %>% arrange(date)
zzz$call<-ifelse(zzz$call!="NA",zzz$call,lead(zzz$call,n=1))
zzz<-zzz[which(!duplicated(zzz$date,fromLast = FALSE)),]
zzz$inpatient<-ifelse(zzz$inpatient=="NULL",zzz$call,zzz$inpatient)
zzz$esrd<-ifelse(zzz$esrd=="NULL",zzz$call,zzz$esrd)
holiday_sched<-holiday_call(2019)
complete_sched<-add_holidays(holiday_sched,zzz) %>% arrange(date)
complete_sched<-clean.schedule(complete_sched)
final_sched<-as.data.frame(complete_sched,row.names=FALSE)


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


