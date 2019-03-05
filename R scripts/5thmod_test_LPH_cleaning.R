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


