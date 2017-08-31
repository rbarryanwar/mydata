library(readxl)
data = read_excel("TL24.02-All-Data-Subject5.xlsx") #load the data file
timing = read.csv("times.csv") #load the file with the timing info in it
# head(data, n=5)
# dim(data) ## should be width of 42

#only include the relevant columns
gaze=data[,c("Timestamp", "Event", "EventKey", "Descriptor", "StimuliName", "StimuliID", "FixationDuration", "AoiIds", "AoiNames")]
# dim(gaze)
# head(gaze, n=5)

all_switches= c("ALL")
  
a='Fam'
i=1
# 
# gaze[105,4]="1 car_right_accurate.wmv"
# gaze[728,4]="2 keys_right_accurate.wmv"
# gaze[1345,4]="3 shoe_left_accurate.wmv"
# gaze[1932,4]="4 book_right_accurate.wmv"
# gaze[2465,4]="5 Ball_left_accurate.wmv"
# gaze[3001,4]="6 cup_left_accurate.wmv"

# BIG LOOP --------------------------------------------------------------------


while (i<7) {
  #start= match("Fam1",gaze$StimuliName)
  j=sprintf("%s%i", a, i)
  start= match(j,gaze$StimuliName) #find where familiarization starts and save this row number
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


write.csv(all_switches, file="TL2402_switches.csv", append=TRUE, row.names=FALSE)

rm(list = ls())
