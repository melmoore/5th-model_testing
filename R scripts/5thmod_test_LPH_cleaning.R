#5th model testing--on plants in lab under heat lamps

#load libraries

library(readr)
library(ggplot2)
library(tidyr)
library(Rmisc)
library(dplyr)
library(viridis)
library(RColorBrewer)

#------------------

#TRIAL 1: ONLY MODELS

#load data

mod_raw <- read_csv("data files/5thmod_testing_2-28-19.csv", 
                    col_names = FALSE)
View(mod_raw)


#---------------------

#remove datalogger info from the head of the data frame

header<-mod_raw[1:4,]

mod_raw<-mod_raw[-(1:4),]

#-------------------

#rename columns

mod_raw<-mod_raw %>% rename(date_time=X1, rec_num=X2, se_volt=X3, tc1=X4, m3=X5, tc3=X6, sm_ul=X7, otc=X8,  
                            sm_otol=X9, tc7=X10, tc8=X11, m2=X12, m1=X13, volts=X14)

#remove columns of tcs that weren't recording data
mod_raw[,c("tc1", "tc3", "tc7", "tc8")]<-list(NULL)


#-------------------

#converting date and time to julian 

#separate date and time by space
mod_raw<-mod_raw %>% separate(date_time, c("date", "time"), sep=" ")

#create column with julian date
mod_raw$date.j<-strptime(mod_raw$date, "%m/%d")$yday+1

#separates time into hours and minutes, changes class from character to numeric. Calculates decimal time
#as minutes per hour (/60), then calculates decimal time as hour per day (/24), then adds to julian date
#to create column with julian day and decimal time of each recorded temp
mod_raw <- mod_raw %>% separate(time, c("h", "m"), ":", remove=FALSE) %>%
  mutate(h = as.numeric(h)) %>% mutate(m = as.numeric(m)) %>%
  mutate(time.dec = h+m/60) %>%
  mutate(time.dec.24 = time.dec/24) %>%
  mutate(date.time.j = date.j+time.dec.24)


#----------------------

#The data logger did not properly record date and time (says it's 1999)--plotting to determine the time
  ##the trial took place

#transforming to long data frame
mod_lng<-mod_raw %>% gather(model_id, temp, m3, sm_ul, otc, sm_otol, m2, m1)
View(mod_lng)

#convert temp to numeric for proper plotting
mod_lng$temp<-as.numeric(mod_lng$temp)

#plot temp for each mod by date time
mod_plot<-ggplot(mod_lng, aes(x=date.time.j, y=temp, group=model_id, color=model_id))
mod_plot+geom_line()



#subset to only time of trial
mod_lng_cl<-subset(mod_lng, date.time.j<214.35)


#plot temp for each model
#plot temp for each mod by date time
mod_plot2<-ggplot(mod_lng_cl, aes(x=date.time.j, y=temp, group=model_id, color=model_id))
mod_plot2+geom_line(size=2
)+scale_color_brewer(palette = "Dark2")



#m1 = transclucent
#m2 = white
#m3 = ivory



#----------------------

#TRIAL 2: MODELS AND CATERPILLARS ON SAME LEAF

#load data

wcats <- read_csv("data files/5thmod_testing_wcats_3-20-19.csv", 
                  col_names = FALSE)


irth <- read_csv("data files/5thmod-testing_wcats_IR-therm_data.csv")
View(irth)

#---------------------

#remove datalogger info from the head of the datalogger data frame

header_wcats<-wcats[1:4,]

wcats<-wcats[-(1:4),]

#---------------------------

#rename columns

wcats<-wcats %>% rename(date_time=X1, rec_num=X2, se_volt=X3, tc1=X4, m3=X5, tc3=X6, sm_ul=X7, otc=X8,  
                            sm_otol=X9, tc7=X10, tc8=X11, m2=X12, m1=X13, volts=X14)

#remove columns of tcs that weren't recording data 
wcats[,c("tc1", "tc3", "tc7", "tc8")]<-list(NULL)


#-------------------

#converting date and time to julian 

#separate date and time by space
wcats<-wcats %>% separate(date_time, c("date", "time"), sep=" ")

#create column with julian date
wcats$date.j<-strptime(wcats$date, "%m/%d")$yday+1

#separates time into hours and minutes, changes class from character to numeric. Calculates decimal time
#as minutes per hour (/60), then calculates decimal time as hour per day (/24), then adds to julian date
#to create column with julian day and decimal time of each recorded temp
wcats <- wcats %>% separate(time, c("h", "m"), ":", remove=FALSE) %>%
  mutate(h = as.numeric(h)) %>% mutate(m = as.numeric(m)) %>%
  mutate(time.dec = h+m/60) %>%
  mutate(time.dec.24 = time.dec/24) %>%
  mutate(date.time.j = date.j+time.dec.24)


#-------------

#transforming to long data frame
wcats_lng<-wcats %>% gather(model_id, temp, m3, sm_ul, otc, sm_otol, m2, m1)
View(wcats_lng)

#convert temp to numeric for proper plotting
wcats_lng$temp<-as.numeric(wcats_lng$temp)

#plot temp for each mod by date time
wcats_test_plot<-ggplot(wcats_lng, aes(x=date.time.j, y=temp, group=model_id, color=model_id))
wcats_test_plot+geom_line(size=2)



#-----------------

#IR thermometer data

#rename type column
irth<-rename(irth, type = 'Type (C or M)')

#convert type designation (C or M) to lower case
irth$type<-tolower(irth$type)

#combine Num and type columns
irth <- mutate(irth, model_id = paste(type, Num, sep="")) 

#---------------

#make a long data set of IR thermomemter data

irth_lng<- irth %>% gather(time, temp, Temp.0, Temp.5, Temp.10, Temp.15, Temp.20, Temp.25, Temp.30, Temp.35,
                           Temp.40, Temp.45, Temp.50, Temp.55, Temp.60)
View(irth_lng)


#remove the Temp. from the time column so its just minutes into trial
irth_lng$time<-gsub("Temp.", "", irth_lng$time)

#convert time to numeric
irth_lng$time<-as.numeric(irth_lng$time)

#----------------

#plot IR thermometer data

ir_plot<-ggplot(irth_lng, aes(x=time, y=temp, color=model_id))
ir_plot+geom_line(aes(linetype=type),
                  size=2)


#---------------------

#Plot IR and datalogger data from trial together 

#subset data logger data to only be from the time of the trial
wcats_lng_trial<-subset(wcats_lng, date.time.j>233.9306 & date.time.j<233.9931)

#subset out the small models
wcats_lng_trial<-subset(wcats_lng_trial, model_id=="m1" | model_id=="m2" | model_id=="m3")

#plotting to test it
dl_trial_testplot<-ggplot(wcats_lng_trial, aes(x=date.time.j, y=temp, color=model_id))
dl_trial_testplot+geom_line(size=2)


#reconfiguring time column to match IR data
wcats_lng_trial$time<-ifelse(wcats_lng_trial$time=="22:30", -1, 
                             ifelse(wcats_lng_trial$time=="22:40", 0,
                                    ifelse(wcats_lng_trial$time=="22:50", 10,
                                    ifelse(wcats_lng_trial$time=="23:00", 20,
                                    ifelse(wcats_lng_trial$time=="23:10", 30,
                                    ifelse(wcats_lng_trial$time=="23:20", 40,
                                    ifelse(wcats_lng_trial$time=="23:30", 50, 
                                    ifelse(wcats_lng_trial$time=="23:40", 60,
                                    ifelse(wcats_lng_trial$time=="23:50", 70, NA)))))))))




#try plotting both together
  ##need to figure out x axis--data logger on before IR, and there was a 5 min acclimation period for IR
trial_test_plot<-ggplot(irth_lng, aes(x=time, y=temp, color=model_id))
trial_test_plot+geom_line(aes(linetype=type),
                          size=2
)+scale_x_continuous(limits = c(-1, 75)
)+geom_line(data=wcats_lng_trial, aes(x=time, y=temp, color=model_id),
            size=1, alpha=.4)


