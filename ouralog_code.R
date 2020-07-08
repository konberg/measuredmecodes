#reading Oura data 

library(lubridate)
library(sentimentr)
library(tidyverse)
library(jsonlite)

rawoura<-fromJSON('./DATAIN/oura_2020-07-07T13-46-10.json')
lapply(rawoura,names)
activity<-flatten(data.frame(rawoura$activity))
activdat<-activity %>% select(summary_date,cal_active,cal_total,inactive,daily_movement,low,medium,high,steps,rest,score) %>% 
    rename(activescore=score)
minmets<-activity %>% select(summary_date,met_1min) %>% unnest(cols=c(met_1min)) %>% 
    group_by(summary_date) %>% mutate(min=seq_along(met_1min)) %>% 
    mutate(daypart=as.factor(ifelse(min <360, "AM",ifelse(min>1020,"EV", "DA")))) %>% 
    group_by(summary_date,daypart) %>% mutate(mets=round(sum(met_1min,na.rm=T),0)) %>% 
    ungroup() %>% distinct(summary_date,daypart,mets)

readiness<-flatten(data.frame(rawoura$readiness))
readidat<-readiness %>% select(summary_date,score,score_previous_day,score_previous_night,score_resting_hr,
                               score_sleep_balance,score_recovery_index) %>%rename(readiscore=score)

#rest<-flatten(data.frame(jrawoura$restful_periods))
#contains naps data; no need for now

sleep<-flatten(data.frame(rawoura$sleep))
sleepdat<-sleep %>% 
    select(summary_date,bedtime_start,bedtime_end,duration,onset_latency,light,rem,deep,breath_average,
           hr_average,temperature_delta,score)%>% 
    rename(sleepscore=score) 

ouradat<-left_join(minmets,activdat,by="summary_date")
ouradat<-left_join(ouradat,sleepdat,by="summary_date")
ouradat<-left_join(ouradat,readidat,by="summary_date")
ouradat<-ouradat %>% mutate(date=as_date(summary_date)) %>% 
    mutate(dmy=format(date,"%m/%d/%Y")) %>% select(dmy,daypart:score_recovery_index)
write.csv(ouradat, file = "./DATAOUT/PublicOuraData.csv")


