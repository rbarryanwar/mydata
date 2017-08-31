transitions = function(directory = "~/Documents/ScanPath_Output/TOTAL FIX DURATION"){
        setwd(directory)
        all_files = dir(directory) 
        LEFTeye1 = 0
        LEFTeye2 = 0
        RIGHTeye1 = 0
        RIGHTeye2 = 0
        NOSE1 = 0
        NOSE2 = 0
        MOUTH1 = 0
        MOUTH2 = 0
        col.names = c("recording", "condition", "LEFTeye1", "RIGHTeye1", "NOSE1", 
                      "MOUTH1", "LEFTeye2", "RIGHTeye2", "NOSE2", "MOUTH2")
        summary <- read.table(text = "", col.names = col.names)
        for (i in 118:127){
                fixFILE = all_files[i]
                data = read.table(fixFILE, sep = '\t',header = TRUE)
                Hisp = grep("HF", data$familiarizationpicture)
                Asian = grep("AF", data$familiarizationpicture)
                Black = grep("BF", data$familiarizationpicture)
                White = grep("WF", data$familiarizationpicture)
                for (j in Hisp){
                        if (as.character(data$IA_LABEL[j]=="LEFTeye2")){
                                LEFTeye2 = data$IA_DWELL_TIME[j]
                        } else if (as.character(data$IA_LABEL[j]=="RIGHTeye2")){
                                RIGHTeye2 = data$IA_DWELL_TIME[j]
                        } else if (as.character(data$IA_LABEL[j]=="MOUTH2")){
                                MOUTH2 = data$IA_DWELL_TIME[j]
                        } else if (as.character(data$IA_LABEL[j]=="NOSE2")){
                                NOSE2 = data$IA_DWELL_TIME[j]
                        } else if (as.character(data$IA_LABEL[j]=="LEFTeye1")){
                                LEFTeye1 = data$IA_DWELL_TIME[j]
                        } else if (as.character(data$IA_LABEL[j]=="RIGHTeye1")){
                                RIGHTeye1 = data$IA_DWELL_TIME[j]
                        } else if (as.character(data$IA_LABEL[j]=="MOUTH1")){
                                MOUTH1 = data$IA_DWELL_TIME[j]
                        } else if (as.character(data$IA_LABEL[j]=="NOSE1")){
                                NOSE1 = data$IA_DWELL_TIME[j]
                        } else 
                                next()
                        
                        HispanicData <- data.frame(fixFILE, "Hisp", LEFTeye1, RIGHTeye1, 
                                                   NOSE1, MOUTH1, LEFTeye2, RIGHTeye2, NOSE2, MOUTH2)
                        names(HispanicData)= col.names
                }
                LEFTeye1 = 0
                LEFTeye2 = 0
                RIGHTeye1 = 0
                RIGHTeye2 = 0
                NOSE1 = 0
                NOSE2 = 0
                MOUTH1 = 0
                MOUTH2 = 0
                
                for (k in Asian){
                        if (as.character(data$IA_LABEL[k]=="LEFTeye2")){
                                LEFTeye2 = data$IA_DWELL_TIME[k]
                        } else if (as.character(data$IA_LABEL[k]=="RIGHTeye2")){
                                RIGHTeye2 = data$IA_DWELL_TIME[k]
                        } else if (as.character(data$IA_LABEL[k]=="MOUTH2")){
                                MOUTH2 = data$IA_DWELL_TIME[k]
                        } else if (as.character(data$IA_LABEL[k]=="NOSE2")){
                                NOSE2 = data$IA_DWELL_TIME[k]
                        } else if (as.character(data$IA_LABEL[k]=="LEFTeye1")){
                                LEFTeye1 = data$IA_DWELL_TIME[k]
                        } else if (as.character(data$IA_LABEL[k]=="RIGHTeye1")){
                                RIGHTeye1 = data$IA_DWELL_TIME[k]
                        } else if (as.character(data$IA_LABEL[k]=="MOUTH1")){
                                MOUTH1 = data$IA_DWELL_TIME[k]
                        } else if (as.character(data$IA_LABEL[k]=="NOSE1")){
                                NOSE1 = data$IA_DWELL_TIME[k]
                        } else 
                                next()
                        
                        AsianData <- data.frame(fixFILE, "Asian", LEFTeye1, RIGHTeye1, 
                                                   NOSE1, MOUTH1, LEFTeye2, RIGHTeye2, NOSE2, MOUTH2)
                        names(AsianData)= col.names
                }
                LEFTeye1 = 0
                LEFTeye2 = 0
                RIGHTeye1 = 0
                RIGHTeye2 = 0
                NOSE1 = 0
                NOSE2 = 0
                MOUTH1 = 0
                MOUTH2 = 0
                
                for (l in Black){
                        if (as.character(data$IA_LABEL[l]=="LEFTeye2")){
                                LEFTeye2 = data$IA_DWELL_TIME[l]
                        } else if (as.character(data$IA_LABEL[l]=="RIGHTeye2")){
                                RIGHTeye2 = data$IA_DWELL_TIME[l]
                        } else if (as.character(data$IA_LABEL[l]=="MOUTH2")){
                                MOUTH2 = data$IA_DWELL_TIME[l]
                        } else if (as.character(data$IA_LABEL[l]=="NOSE2")){
                                NOSE2 = data$IA_DWELL_TIME[l]
                        } else if (as.character(data$IA_LABEL[l]=="LEFTeye1")){
                                LEFTeye1 = data$IA_DWELL_TIME[l]
                        } else if (as.character(data$IA_LABEL[l]=="RIGHTeye1")){
                                RIGHTeye1 = data$IA_DWELL_TIME[l]
                        } else if (as.character(data$IA_LABEL[l]=="MOUTH1")){
                                MOUTH1 = data$IA_DWELL_TIME[l]
                        } else if (as.character(data$IA_LABEL[l]=="NOSE1")){
                                NOSE1 = data$IA_DWELL_TIME[l]
                        } else 
                                next()
                        
                        BlackData <- data.frame(fixFILE, "Black", LEFTeye1, RIGHTeye1, 
                                                NOSE1, MOUTH1, LEFTeye2, RIGHTeye2, NOSE2, MOUTH2)
                        names(BlackData)= col.names
                }
                LEFTeye1 = 0
                LEFTeye2 = 0
                RIGHTeye1 = 0
                RIGHTeye2 = 0
                NOSE1 = 0
                NOSE2 = 0
                MOUTH1 = 0
                MOUTH2 = 0
                
                for (m in White){
                        if (as.character(data$IA_LABEL[m]=="LEFTeye2")){
                                LEFTeye2 = data$IA_DWELL_TIME[m]
                        } else if (as.character(data$IA_LABEL[m]=="RIGHTeye2")){
                                RIGHTeye2 = data$IA_DWELL_TIME[m]
                        } else if (as.character(data$IA_LABEL[m]=="MOUTH2")){
                                MOUTH2 = data$IA_DWELL_TIME[m]
                        } else if (as.character(data$IA_LABEL[m]=="NOSE2")){
                                NOSE2 = data$IA_DWELL_TIME[m]
                        } else if (as.character(data$IA_LABEL[m]=="LEFTeye1")){
                                LEFTeye1 = data$IA_DWELL_TIME[m]
                        } else if (as.character(data$IA_LABEL[m]=="RIGHTeye1")){
                                RIGHTeye1 = data$IA_DWELL_TIME[m]
                        } else if (as.character(data$IA_LABEL[m]=="MOUTH1")){
                                MOUTH1 = data$IA_DWELL_TIME[m]
                        } else if (as.character(data$IA_LABEL[m]=="NOSE1")){
                                NOSE1 = data$IA_DWELL_TIME[m]
                        } else 
                                next()
                        
                        WhiteData <- data.frame(fixFILE, "White", LEFTeye1, RIGHTeye1, 
                                                NOSE1, MOUTH1, LEFTeye2, RIGHTeye2, NOSE2, MOUTH2)
                        names(WhiteData)= col.names
                }
                fullSummary = rbind(HispanicData, AsianData, BlackData, WhiteData)
                summary=rbind(summary,fullSummary)
        }
}
