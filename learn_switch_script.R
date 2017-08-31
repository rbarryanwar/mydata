library(readxl)
subjects =list.files(pattern = ".xlsx")
subID=subjects[19]
data = read_excel(subID) #load the data file
timing = read.csv("times.csv") #load the file with the timing info in it
# head(data, n=5)
# dim(data) ## should be width of 42

#only include the relevant columns
gaze=data[,c("Timestamp", "Event", "EventKey", "Descriptor", "StimuliName", "StimuliID", "FixationDuration", "AoiIds", "AoiNames")]
# dim(gaze)
# head(gaze, n=5)

all_switches= c("ALL")

a='Learn'
i=1

# gaze[3538,4]="1 neem1_left_orange.wmv"
# gaze[4113,4]="2 neem2_right_orange.wmv"
# gaze[4701,4]="3 that3_left_yellow.wmv"
# gaze[5292,4]="4 that1_right_yellow.wmv"
# gaze[5872,4]="5 neem2_left_orange.wmv"
# gaze[6420,4]="6 that3_left_yellow.wmv"
# gaze[6992,4]="7 neem1_right_orange.wmv"
# gaze[7571,4]="8 that2_right_yellow.wmv"
# gaze[8150,4]="9 neem3_left_orange.wmv"
# gaze[8794,4]="10 neem1_right_orange.wmv"
# gaze[9367,4]="11 that2_left_yellow.wmv"
# gaze[9923,4]="12 that3_right_yellow.wmv"

# BIG LOOP --------------------------------------------------------------------


while (i<13) {

  j=sprintf("%s%i", a, i)
  start= match(j,gaze$StimuliName) #find where learning starts and save this row number
  start
  #find the video that's playing
  video =gaze[as.integer(start-1), 4]
  if (is.na(video))
  {video =gaze[as.integer(start-2), 4]
  if (is.na(video))
  {video =gaze[as.integer(start-3), 4]
  if (is.na(video))
  {video =gaze[as.integer(start-4), 4]
  if (is.na(video))
  {video =gaze[as.integer(start-5),4]
  if (is.na(video))
  {video =gaze[as.integer(start-6), 4]
  if (is.na(video))
  {video= gaze[as.integer(start-15), 4]
  if (is.na(video))
  {video= gaze[as.integer(start-7), 4]
  if (is.na(video))
  {video= gaze[as.integer(start-8), 4]
  }
  }
  }
  }
  }
  }
  }
  }
  #find the exact timing for that video and save those values as start and end times
  timing_match= match(video, timing$video)
  
  start_time=timing[timing_match,2]
  end_time=timing[timing_match,3]
  # (end_time/20)- start_point + start
  end_point = floor((timing[timing_match,3])/20) + start
  
  #tells you how many rows (each row is 20ms) from the start the head turn starts
  start_point= floor((start_time)/20) + start
  
  #use start_point as the new row index for where we want to start looking at switches
  
  data_area=gaze[start_point:end_point,9]  
  m=length(data_area)
  data_area
  
  #LEGEND: face=8, target=9, dist=3 
  x=c("switches")
  for(l in 1:(m-1)){
    if (data_area[l] != data_area[l+1])
    {e=print(l)
    b=print(data_area[l])
    c=print(l+1)
    d=print(data_area[l+1])
    x=c(x,e,b,c,d)
    }
  }
  
  
  print(x) 
  all_switches=c(all_switches,x)
  print(all_switches)
  remove('e','b','c', 'd', 'x', 'l')
  i=i+1
}


# FINISH UP ---------------------------------------------------------------

fileDir = "~/Documents/AI_data/INACC/LEARN/"
write.csv(all_switches, file=sprintf("%s%s.csv", fileDir,subID), row.names=FALSE)

rm(list = ls())
