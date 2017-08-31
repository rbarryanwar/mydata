substrEnd = function(x,n){
        substr(x, nchar(x)-n +1, nchar(x))
}

transitions = function(directory = "~/Documents/ScanPath_Output/WITH NOSE"){
        setwd(directory)
        all_files = dir(directory) 
        col.names = c("recording", "condition", "winEyes", "winLeyeNose", "winReyeNose", 
                      "winLeyeMouth", "winReyeMouth", "winNoseMouth", "btwnLeyeReye" , 
                      "btwnLeyeLeye", "btwnReyeReye", "btwnLeyeNose", "btwnReyeNose", 
                      "btwnLeyeMouth", "btwnReyeMouth","btwnNoseMouth", "btwnNoseNose", "btwnMouthMouth")
        summary <- read.table(text = "", col.names = col.names)
        
        winEyes = 0
        winLeyeNose = 0
        winReyeNose = 0
        winLeyeMouth = 0
        winReyeMouth = 0
        winNoseMouth = 0
        btwnLeyeReye = 0
        btwnLeyeLeye = 0
        btwnReyeReye = 0
        btwnLeyeNose = 0
        btwnReyeNose = 0
        btwnLeyeMouth = 0
        btwnReyeMouth = 0
        btwnNoseMouth = 0
        btwnNoseNose = 0
        btwnMouthMouth = 0
        for (i in 1:10){
                transFILE = all_files[i]
                data = read.table(transFILE, sep = '\t',header = TRUE)
                Hisp = grep("HF", data$familiarizationpicture)
                HispTrials = as.character(data$CURRENT_FIX_INTEREST_AREA_LABEL[Hisp])
                Asian = grep("AF", data$familiarizationpicture)
                AsianTrials = as.character(data$CURRENT_FIX_INTEREST_AREA_LABEL[Asian])
                Black = grep("BF", data$familiarizationpicture)
                BlackTrials = as.character(data$CURRENT_FIX_INTEREST_AREA_LABEL[Black])
                White = grep("WF", data$familiarizationpicture)
                WhiteTrials = as.character(data$CURRENT_FIX_INTEREST_AREA_LABEL[White])
                trials = c(HispTrials, AsianTrials, BlackTrials, WhiteTrials)
                for (j in seq_along(HispTrials)){
                        if (HispTrials[j] == "." | HispTrials[j] == "RIGHT_FAMILIARIZATION" |
                            HispTrials[j] == "LEFT_FAMILIARIZATION"| HispTrials[j+1] == "." |
                            HispTrials[j+1] == "RIGHT_FAMILIARIZATION" | HispTrials[j+1] == "LEFT_FAMILIARIZATION" |
                            is.na(HispTrials[j+1])){
                                next()
                        }else if (HispTrials[j] == HispTrials[j+1]){
                                next()       
                        } else if (HispTrials[j] != HispTrials[j+1]){
                                AOI1 = as.character(HispTrials[j])
                                AOI2 = as.character(HispTrials[j+1])
                                Face1 = substrEnd(AOI1, 1)
                                Face2 = substrEnd(AOI2, 1)
                                if (Face1==Face2){
                                        #do within stuff
                                        if (((AOI1 == "LEFTeye1"| AOI1 == "LEFTeye2") & (AOI2 == "RIGHTeye1" | AOI2 == "RIGHTeye2"))|
                                            ((AOI1 == "RIGHTeye1"| AOI1 == "RIGHTeye2") & (AOI2 == "LEFTeye1" | AOI2 == "LEFTeye2"))){
                                                winEyes = winEyes + 1
                                        } else if (((AOI1 == "LEFTeye1"| AOI1 =="LEFTeye2") & (AOI2 == "NOSE1" | AOI2 == "NOSE2"))|
                                                   ((AOI1 == "NOSE1"| AOI1 =="NOSE2") & (AOI2 == "LEFTeye1" | AOI2 == "LEFTeye2"))){
                                                winLeyeNose = winLeyeNose + 1
                                        } else if (((AOI1 == "RIGHTeye1"| AOI1 =="RIGHTeye2") & (AOI2 == "NOSE1" | AOI2 == "NOSE2"))|
                                                   ((AOI1 == "NOSE1"| AOI1 =="NOSE2") & (AOI2 == "RIGHTeye1" | AOI2 == "RIGHTeye2"))) {
                                                winReyeNose = winReyeNose + 1
                                        } else if (((AOI1 == "LEFTeye1"| AOI1 =="LEFTeye2") & (AOI2 == "MOUTH1" | AOI2 == "MOUTH2"))|
                                                   ((AOI1 == "MOUTH1"| AOI1 =="MOUTH2") & (AOI2 == "LEFTeye1" | AOI2 == "LEFTeye2"))){
                                                winLeyeMouth = winLeyeMouth + 1
                                        } else if (((AOI1 == "RIGHTeye1"| AOI1 =="RIGHTeye2") & (AOI2 == "MOUTH1" | AOI2 == "MOUTH2"))|
                                                   ((AOI1 == "MOUTH1"| AOI1 =="MOUTH2") & (AOI2 == "RIGHTeye1" | AOI2 == "RIGHTeye2"))) {
                                                winReyeMouth = winReyeMouth + 1
                                        } else if (((AOI1 == "MOUTH1"| AOI1 =="MOUTH2") & (AOI2 == "NOSE1" | AOI2 == "NOSE2"))|
                                                   ((AOI1 == "NOSE1"| AOI1 =="NOSE2") & (AOI2 == "MOUTH1" | AOI2 == "MOUTH2"))) {
                                                winNoseMouth = winNoseMouth + 1
                                        } else
                                                print("ERROR")
                                }else if (Face1!=Face2){
                                        #do between stuff
                                        if ((AOI1 == "LEFTeye1"| AOI1 == "LEFTeye2") & (AOI2 == "LEFTeye1" | AOI2 == "LEFTeye2")){
                                                btwnLeyeLeye = btwnLeyeLeye + 1
                                        } else if ((AOI1 == "RIGHTeye1"| AOI1 == "RIGHTeye2") & (AOI2 == "RIGHTeye1" | AOI2 == "RIGHTeye2")){
                                                btwnReyeReye = btwnReyeReye + 1
                                        } else if (((AOI1 == "LEFTeye1"| AOI1 == "LEFTeye2") & (AOI2 == "RIGHTeye1" | AOI2 == "RIGHTeye2"))|
                                                   ((AOI1 == "RIGHTeye1"| AOI1 == "RIGHTeye2") & (AOI2 == "LEFTeye1" | AOI2 == "LEFTeye2"))){
                                                btwnLeyeReye = btwnLeyeReye + 1
                                        } else if (((AOI1 == "LEFTeye1"| AOI1 =="LEFTeye2") & (AOI2 == "NOSE1" | AOI2 == "NOSE2"))|
                                                   ((AOI1 == "NOSE1"| AOI1 =="NOSE2") & (AOI2 == "LEFTeye1" | AOI2 == "LEFTeye2"))){
                                                btwnLeyeNose = btwnLeyeNose + 1
                                        } else if (((AOI1 == "RIGHTeye1"| AOI1 =="RIGHTeye2") & (AOI2 == "NOSE1" | AOI2 == "NOSE2"))|
                                                 ((AOI1 == "NOSE1"| AOI1 =="NOSE2") & (AOI2 == "RIGHTeye1" | AOI2 == "RIGHTeye2"))) {
                                                btwnReyeNose = btwnReyeNose + 1
                                        } else if (((AOI1 == "LEFTeye1"| AOI1 =="LEFTeye2") & (AOI2 == "MOUTH1" | AOI2 == "MOUTH2"))|
                                                   ((AOI1 == "MOUTH1"| AOI1 =="MOUTH2") & (AOI2 == "LEFTeye1" | AOI2 == "LEFTeye2"))){
                                                btwnLeyeMouth = btwnLeyeMouth + 1
                                        } else if (((AOI1 == "RIGHTeye1"| AOI1 =="RIGHTeye2") & (AOI2 == "MOUTH1" | AOI2 == "MOUTH2"))|
                                                   ((AOI1 == "MOUTH1"| AOI1 =="MOUTH2") & (AOI2 == "RIGHTeye1" | AOI2 == "RIGHTeye2"))) {
                                                btwnReyeMouth = btwnReyeMouth + 1
                                        } else if (((AOI1 == "MOUTH1"| AOI1 =="MOUTH2") & (AOI2 == "NOSE1" | AOI2 == "NOSE2"))|
                                                   ((AOI1 == "NOSE1"| AOI1 =="NOSE2") & (AOI2 == "MOUTH1" | AOI2 == "MOUTH2"))) {
                                                btwnNoseMouth = btwnNoseMouth + 1
                                        } else if ((AOI1 == "NOSE1"| AOI1 =="NOSE2") & (AOI2 == "NOSE1" | AOI2 == "NOSE2")) {
                                                btwnNoseNose = btwnNoseNose + 1
                                        } else if ((AOI1 == "MOUTH1"| AOI1 =="MOUTH2") & (AOI2 == "MOUTH1" | AOI2 == "MOUTH2")) {
                                                btwnMouthMouth = btwnMouthMouth + 1
                                        } else
                                                print("ERROR")
                                }
                        }else
                                print("ERROR")
                HispanicData <- data.frame(transFILE, "Hisp", winEyes, winLeyeNose, winReyeNose,  winLeyeMouth,   winReyeMouth,
                                       winNoseMouth,   btwnLeyeReye,   btwnLeyeLeye,   btwnReyeReye,   btwnLeyeNose,
                                       btwnReyeNose,   btwnLeyeMouth, btwnReyeMouth,  btwnNoseMouth,  btwnNoseNose,   btwnMouthMouth)
                names(HispanicData)= col.names
                        }
                winEyes = 0
                winLeyeNose = 0
                winReyeNose = 0
                winLeyeMouth = 0
                winReyeMouth = 0
                winNoseMouth = 0
                btwnLeyeReye = 0
                btwnLeyeLeye = 0
                btwnReyeReye = 0
                btwnLeyeNose = 0
                btwnReyeNose = 0
                btwnLeyeMouth = 0
                btwnReyeMouth = 0
                btwnNoseMouth = 0
                btwnNoseNose = 0
                btwnMouthMouth = 0
                for (k in seq_along(AsianTrials)){
                        
                        if (AsianTrials[k] == "." | AsianTrials[k] == "RIGHT_FAMILIARIZATION" |
                            AsianTrials[k] == "LEFT_FAMILIARIZATION"| AsianTrials[k+1] == "." |
                            AsianTrials[k+1] == "RIGHT_FAMILIARIZATION" | AsianTrials[k+1] == "LEFT_FAMILIARIZATION" |
                            is.na(AsianTrials[k+1])){
                                next()
                        }else if (AsianTrials[k] == AsianTrials[k+1]){
                                next()       
                        } else if (AsianTrials[k] != AsianTrials[k+1]){
                                AOI1 = as.character(AsianTrials[k])
                                AOI2 = as.character(AsianTrials[k+1])
                                Face1 = substrEnd(AOI1, 1)
                                Face2 = substrEnd(AOI2, 1)
                                if (Face1==Face2){
                                        #do within stuff
                                        if (((AOI1 == "LEFTeye1"| AOI1 == "LEFTeye2") & (AOI2 == "RIGHTeye1" | AOI2 == "RIGHTeye2"))|
                                            ((AOI1 == "RIGHTeye1"| AOI1 == "RIGHTeye2") & (AOI2 == "LEFTeye1" | AOI2 == "LEFTeye2"))){
                                                winEyes = winEyes + 1
                                        } else if (((AOI1 == "LEFTeye1"| AOI1 =="LEFTeye2") & (AOI2 == "NOSE1" | AOI2 == "NOSE2"))|
                                                   ((AOI1 == "NOSE1"| AOI1 =="NOSE2") & (AOI2 == "LEFTeye1" | AOI2 == "LEFTeye2"))){
                                                winLeyeNose = winLeyeNose + 1
                                        } else if (((AOI1 == "RIGHTeye1"| AOI1 =="RIGHTeye2") & (AOI2 == "NOSE1" | AOI2 == "NOSE2"))|
                                                   ((AOI1 == "NOSE1"| AOI1 =="NOSE2") & (AOI2 == "RIGHTeye1" | AOI2 == "RIGHTeye2"))) {
                                                winReyeNose = winReyeNose + 1
                                        } else if (((AOI1 == "LEFTeye1"| AOI1 =="LEFTeye2") & (AOI2 == "MOUTH1" | AOI2 == "MOUTH2"))|
                                                   ((AOI1 == "MOUTH1"| AOI1 =="MOUTH2") & (AOI2 == "LEFTeye1" | AOI2 == "LEFTeye2"))){
                                                winLeyeMouth = winLeyeMouth + 1
                                        } else if (((AOI1 == "RIGHTeye1"| AOI1 =="RIGHTeye2") & (AOI2 == "MOUTH1" | AOI2 == "MOUTH2"))|
                                                   ((AOI1 == "MOUTH1"| AOI1 =="MOUTH2") & (AOI2 == "RIGHTeye1" | AOI2 == "RIGHTeye2"))) {
                                                winReyeMouth = winReyeMouth + 1
                                        } else if (((AOI1 == "MOUTH1"| AOI1 =="MOUTH2") & (AOI2 == "NOSE1" | AOI2 == "NOSE2"))|
                                                   ((AOI1 == "NOSE1"| AOI1 =="NOSE2") & (AOI2 == "MOUTH1" | AOI2 == "MOUTH2"))) {
                                                winNoseMouth = winNoseMouth + 1
                                        } else
                                                print("ERROR")
                                }else if (Face1!=Face2){
                                        #do between stuff
                                        if ((AOI1 == "LEFTeye1"| AOI1 == "LEFTeye2") & (AOI2 == "LEFTeye1" | AOI2 == "LEFTeye2")){
                                                btwnLeyeLeye = btwnLeyeLeye + 1
                                        } else if ((AOI1 == "RIGHTeye1"| AOI1 == "RIGHTeye2") & (AOI2 == "RIGHTeye1" | AOI2 == "RIGHTeye2")){
                                                btwnReyeReye = btwnReyeReye + 1
                                        } else if (((AOI1 == "LEFTeye1"| AOI1 == "LEFTeye2") & (AOI2 == "RIGHTeye1" | AOI2 == "RIGHTeye2"))|
                                                   ((AOI1 == "RIGHTeye1"| AOI1 == "RIGHTeye2") & (AOI2 == "LEFTeye1" | AOI2 == "LEFTeye2"))){
                                                btwnLeyeReye = btwnLeyeReye + 1
                                        } else if (((AOI1 == "LEFTeye1"| AOI1 =="LEFTeye2") & (AOI2 == "NOSE1" | AOI2 == "NOSE2"))|
                                                   ((AOI1 == "NOSE1"| AOI1 =="NOSE2") & (AOI2 == "LEFTeye1" | AOI2 == "LEFTeye2"))){
                                                btwnLeyeNose = btwnLeyeNose + 1
                                        } else if (((AOI1 == "RIGHTeye1"| AOI1 =="RIGHTeye2") & (AOI2 == "NOSE1" | AOI2 == "NOSE2"))|
                                                   ((AOI1 == "NOSE1"| AOI1 =="NOSE2") & (AOI2 == "RIGHTeye1" | AOI2 == "RIGHTeye2"))) {
                                                btwnReyeNose = btwnReyeNose + 1
                                        } else if (((AOI1 == "LEFTeye1"| AOI1 =="LEFTeye2") & (AOI2 == "MOUTH1" | AOI2 == "MOUTH2"))|
                                                   ((AOI1 == "MOUTH1"| AOI1 =="MOUTH2") & (AOI2 == "LEFTeye1" | AOI2 == "LEFTeye2"))){
                                                btwnLeyeMouth = btwnLeyeMouth + 1
                                        } else if (((AOI1 == "RIGHTeye1"| AOI1 =="RIGHTeye2") & (AOI2 == "MOUTH1" | AOI2 == "MOUTH2"))|
                                                   ((AOI1 == "MOUTH1"| AOI1 =="MOUTH2") & (AOI2 == "RIGHTeye1" | AOI2 == "RIGHTeye2"))) {
                                                btwnReyeMouth = btwnReyeMouth + 1
                                        } else if (((AOI1 == "MOUTH1"| AOI1 =="MOUTH2") & (AOI2 == "NOSE1" | AOI2 == "NOSE2"))|
                                                   ((AOI1 == "NOSE1"| AOI1 =="NOSE2") & (AOI2 == "MOUTH1" | AOI2 == "MOUTH2"))) {
                                                btwnNoseMouth = btwnNoseMouth + 1
                                        } else if ((AOI1 == "NOSE1"| AOI1 =="NOSE2") & (AOI2 == "NOSE1" | AOI2 == "NOSE2")) {
                                                btwnNoseNose = btwnNoseNose + 1
                                        } else if ((AOI1 == "MOUTH1"| AOI1 =="MOUTH2") & (AOI2 == "MOUTH1" | AOI2 == "MOUTH2")) {
                                                btwnMouthMouth = btwnMouthMouth + 1
                                        } else
                                                print("ERROR")
                                }
                        }else
                                print("ERROR")
                        AsianData <- data.frame(transFILE, "Asian", winEyes, winLeyeNose, winReyeNose,  winLeyeMouth,   winReyeMouth,
                                                   winNoseMouth,   btwnLeyeReye,   btwnLeyeLeye,   btwnReyeReye,   btwnLeyeNose,
                                                   btwnReyeNose,   btwnLeyeMouth, btwnReyeMouth,  btwnNoseMouth,  btwnNoseNose,   btwnMouthMouth)
                        names(AsianData)= col.names
                        
                }
                winEyes = 0
                winLeyeNose = 0
                winReyeNose = 0
                winLeyeMouth = 0
                winReyeMouth = 0
                winNoseMouth = 0
                btwnLeyeReye = 0
                btwnLeyeLeye = 0
                btwnReyeReye = 0
                btwnLeyeNose = 0
                btwnReyeNose = 0
                btwnLeyeMouth = 0
                btwnReyeMouth = 0
                btwnNoseMouth = 0
                btwnNoseNose = 0
                btwnMouthMouth = 0
                for (l in seq_along(BlackTrials)){
                        if (BlackTrials[l] == "." | BlackTrials[l] == "RIGHT_FAMILIARIZATION" |
                            BlackTrials[l] == "LEFT_FAMILIARIZATION"| BlackTrials[l+1] == "." |
                            BlackTrials[l+1] == "RIGHT_FAMILIARIZATION" | BlackTrials[l+1] == "LEFT_FAMILIARIZATION" |
                            is.na(BlackTrials[l+1])){
                                next()
                        }else if (BlackTrials[l] == BlackTrials[l+1]){
                                next()       
                        } else if (BlackTrials[l] != BlackTrials[l+1]){
                                AOI1 = as.character(BlackTrials[l])
                                AOI2 = as.character(BlackTrials[l+1])
                                Face1 = substrEnd(AOI1, 1)
                                Face2 = substrEnd(AOI2, 1)
                                if (Face1==Face2){
                                        #do within stuff
                                        if (((AOI1 == "LEFTeye1"| AOI1 == "LEFTeye2") & (AOI2 == "RIGHTeye1" | AOI2 == "RIGHTeye2"))|
                                            ((AOI1 == "RIGHTeye1"| AOI1 == "RIGHTeye2") & (AOI2 == "LEFTeye1" | AOI2 == "LEFTeye2"))){
                                                winEyes = winEyes + 1
                                        } else if (((AOI1 == "LEFTeye1"| AOI1 =="LEFTeye2") & (AOI2 == "NOSE1" | AOI2 == "NOSE2"))|
                                                   ((AOI1 == "NOSE1"| AOI1 =="NOSE2") & (AOI2 == "LEFTeye1" | AOI2 == "LEFTeye2"))){
                                                winLeyeNose = winLeyeNose + 1
                                        } else if (((AOI1 == "RIGHTeye1"| AOI1 =="RIGHTeye2") & (AOI2 == "NOSE1" | AOI2 == "NOSE2"))|
                                                   ((AOI1 == "NOSE1"| AOI1 =="NOSE2") & (AOI2 == "RIGHTeye1" | AOI2 == "RIGHTeye2"))) {
                                                winReyeNose = winReyeNose + 1
                                        } else if (((AOI1 == "LEFTeye1"| AOI1 =="LEFTeye2") & (AOI2 == "MOUTH1" | AOI2 == "MOUTH2"))|
                                                   ((AOI1 == "MOUTH1"| AOI1 =="MOUTH2") & (AOI2 == "LEFTeye1" | AOI2 == "LEFTeye2"))){
                                                winLeyeMouth = winLeyeMouth + 1
                                        } else if (((AOI1 == "RIGHTeye1"| AOI1 =="RIGHTeye2") & (AOI2 == "MOUTH1" | AOI2 == "MOUTH2"))|
                                                   ((AOI1 == "MOUTH1"| AOI1 =="MOUTH2") & (AOI2 == "RIGHTeye1" | AOI2 == "RIGHTeye2"))) {
                                                winReyeMouth = winReyeMouth + 1
                                        } else if (((AOI1 == "MOUTH1"| AOI1 =="MOUTH2") & (AOI2 == "NOSE1" | AOI2 == "NOSE2"))|
                                                   ((AOI1 == "NOSE1"| AOI1 =="NOSE2") & (AOI2 == "MOUTH1" | AOI2 == "MOUTH2"))) {
                                                winNoseMouth = winNoseMouth + 1
                                        } else
                                                print("ERROR")
                                }else if (Face1!=Face2){
                                        #do between stuff
                                        if ((AOI1 == "LEFTeye1"| AOI1 == "LEFTeye2") & (AOI2 == "LEFTeye1" | AOI2 == "LEFTeye2")){
                                                btwnLeyeLeye = btwnLeyeLeye + 1
                                        } else if ((AOI1 == "RIGHTeye1"| AOI1 == "RIGHTeye2") & (AOI2 == "RIGHTeye1" | AOI2 == "RIGHTeye2")){
                                                btwnReyeReye = btwnReyeReye + 1
                                        } else if (((AOI1 == "LEFTeye1"| AOI1 == "LEFTeye2") & (AOI2 == "RIGHTeye1" | AOI2 == "RIGHTeye2"))|
                                                   ((AOI1 == "RIGHTeye1"| AOI1 == "RIGHTeye2") & (AOI2 == "LEFTeye1" | AOI2 == "LEFTeye2"))){
                                                btwnLeyeReye = btwnLeyeReye + 1
                                        } else if (((AOI1 == "LEFTeye1"| AOI1 =="LEFTeye2") & (AOI2 == "NOSE1" | AOI2 == "NOSE2"))|
                                                   ((AOI1 == "NOSE1"| AOI1 =="NOSE2") & (AOI2 == "LEFTeye1" | AOI2 == "LEFTeye2"))){
                                                btwnLeyeNose = btwnLeyeNose + 1
                                        } else if (((AOI1 == "RIGHTeye1"| AOI1 =="RIGHTeye2") & (AOI2 == "NOSE1" | AOI2 == "NOSE2"))|
                                                   ((AOI1 == "NOSE1"| AOI1 =="NOSE2") & (AOI2 == "RIGHTeye1" | AOI2 == "RIGHTeye2"))) {
                                                btwnReyeNose = btwnReyeNose + 1
                                        } else if (((AOI1 == "LEFTeye1"| AOI1 =="LEFTeye2") & (AOI2 == "MOUTH1" | AOI2 == "MOUTH2"))|
                                                   ((AOI1 == "MOUTH1"| AOI1 =="MOUTH2") & (AOI2 == "LEFTeye1" | AOI2 == "LEFTeye2"))){
                                                btwnLeyeMouth = btwnLeyeMouth + 1
                                        } else if (((AOI1 == "RIGHTeye1"| AOI1 =="RIGHTeye2") & (AOI2 == "MOUTH1" | AOI2 == "MOUTH2"))|
                                                   ((AOI1 == "MOUTH1"| AOI1 =="MOUTH2") & (AOI2 == "RIGHTeye1" | AOI2 == "RIGHTeye2"))) {
                                                btwnReyeMouth = btwnReyeMouth + 1
                                        } else if (((AOI1 == "MOUTH1"| AOI1 =="MOUTH2") & (AOI2 == "NOSE1" | AOI2 == "NOSE2"))|
                                                   ((AOI1 == "NOSE1"| AOI1 =="NOSE2") & (AOI2 == "MOUTH1" | AOI2 == "MOUTH2"))) {
                                                btwnNoseMouth = btwnNoseMouth + 1
                                        } else if ((AOI1 == "NOSE1"| AOI1 =="NOSE2") & (AOI2 == "NOSE1" | AOI2 == "NOSE2")) {
                                                btwnNoseNose = btwnNoseNose + 1
                                        } else if ((AOI1 == "MOUTH1"| AOI1 =="MOUTH2") & (AOI2 == "MOUTH1" | AOI2 == "MOUTH2")) {
                                                btwnMouthMouth = btwnMouthMouth + 1
                                        } else
                                                print("ERROR")
                                }
                        }else
                                print("ERROR")
                        BlackData <- data.frame(transFILE, "Black", winEyes, winLeyeNose, winReyeNose,  winLeyeMouth,   winReyeMouth,
                                                winNoseMouth,   btwnLeyeReye,   btwnLeyeLeye,   btwnReyeReye,   btwnLeyeNose,
                                                btwnReyeNose,   btwnLeyeMouth, btwnReyeMouth,  btwnNoseMouth,  btwnNoseNose,   btwnMouthMouth)
                        names(BlackData)= col.names
                        
                }
                winEyes = 0
                winLeyeNose = 0
                winReyeNose = 0
                winLeyeMouth = 0
                winReyeMouth = 0
                winNoseMouth = 0
                btwnLeyeReye = 0
                btwnLeyeLeye = 0
                btwnReyeReye = 0
                btwnLeyeNose = 0
                btwnReyeNose = 0
                btwnLeyeMouth = 0
                btwnReyeMouth = 0
                btwnNoseMouth = 0
                btwnNoseNose = 0
                btwnMouthMouth = 0
                for (m in seq_along(WhiteTrials)){
                        if (WhiteTrials[m] == "." | WhiteTrials[m] == "RIGHT_FAMILIARIZATION" |
                            WhiteTrials[m] == "LEFT_FAMILIARIZATION"| WhiteTrials[m+1] == "." |
                            WhiteTrials[m+1] == "RIGHT_FAMILIARIZATION" | WhiteTrials[m+1] == "LEFT_FAMILIARIZATION" | 
                            is.na(WhiteTrials[m+1])){
                                next()
                        }else if (WhiteTrials[m] == WhiteTrials[m+1]){
                                next()       
                        } else if (WhiteTrials[m] != WhiteTrials[m+1]){
                                AOI1 = as.character(WhiteTrials[m])
                                AOI2 = as.character(WhiteTrials[m+1])
                                Face1 = substrEnd(AOI1, 1)
                                Face2 = substrEnd(AOI2, 1)
                                if (Face1==Face2){
                                        #do within stuff
                                        if (((AOI1 == "LEFTeye1"| AOI1 == "LEFTeye2") & (AOI2 == "RIGHTeye1" | AOI2 == "RIGHTeye2"))|
                                            ((AOI1 == "RIGHTeye1"| AOI1 == "RIGHTeye2") & (AOI2 == "LEFTeye1" | AOI2 == "LEFTeye2"))){
                                                winEyes = winEyes + 1
                                        } else if (((AOI1 == "LEFTeye1"| AOI1 =="LEFTeye2") & (AOI2 == "NOSE1" | AOI2 == "NOSE2"))|
                                                   ((AOI1 == "NOSE1"| AOI1 =="NOSE2") & (AOI2 == "LEFTeye1" | AOI2 == "LEFTeye2"))){
                                                winLeyeNose = winLeyeNose + 1
                                        } else if (((AOI1 == "RIGHTeye1"| AOI1 =="RIGHTeye2") & (AOI2 == "NOSE1" | AOI2 == "NOSE2"))|
                                                   ((AOI1 == "NOSE1"| AOI1 =="NOSE2") & (AOI2 == "RIGHTeye1" | AOI2 == "RIGHTeye2"))) {
                                                winReyeNose = winReyeNose + 1
                                        } else if (((AOI1 == "LEFTeye1"| AOI1 =="LEFTeye2") & (AOI2 == "MOUTH1" | AOI2 == "MOUTH2"))|
                                                   ((AOI1 == "MOUTH1"| AOI1 =="MOUTH2") & (AOI2 == "LEFTeye1" | AOI2 == "LEFTeye2"))){
                                                winLeyeMouth = winLeyeMouth + 1
                                        } else if (((AOI1 == "RIGHTeye1"| AOI1 =="RIGHTeye2") & (AOI2 == "MOUTH1" | AOI2 == "MOUTH2"))|
                                                   ((AOI1 == "MOUTH1"| AOI1 =="MOUTH2") & (AOI2 == "RIGHTeye1" | AOI2 == "RIGHTeye2"))) {
                                                winReyeMouth = winReyeMouth + 1
                                        } else if (((AOI1 == "MOUTH1"| AOI1 =="MOUTH2") & (AOI2 == "NOSE1" | AOI2 == "NOSE2"))|
                                                   ((AOI1 == "NOSE1"| AOI1 =="NOSE2") & (AOI2 == "MOUTH1" | AOI2 == "MOUTH2"))) {
                                                winNoseMouth = winNoseMouth + 1
                                        } else
                                                print("ERROR")
                                }else if (Face1!=Face2){
                                        #do between stuff
                                        if ((AOI1 == "LEFTeye1"| AOI1 == "LEFTeye2") & (AOI2 == "LEFTeye1" | AOI2 == "LEFTeye2")){
                                                btwnLeyeLeye = btwnLeyeLeye + 1
                                        } else if ((AOI1 == "RIGHTeye1"| AOI1 == "RIGHTeye2") & (AOI2 == "RIGHTeye1" | AOI2 == "RIGHTeye2")){
                                                btwnReyeReye = btwnReyeReye + 1
                                        } else if (((AOI1 == "LEFTeye1"| AOI1 == "LEFTeye2") & (AOI2 == "RIGHTeye1" | AOI2 == "RIGHTeye2"))|
                                                   ((AOI1 == "RIGHTeye1"| AOI1 == "RIGHTeye2") & (AOI2 == "LEFTeye1" | AOI2 == "LEFTeye2"))){
                                                btwnLeyeReye = btwnLeyeReye + 1
                                        } else if (((AOI1 == "LEFTeye1"| AOI1 =="LEFTeye2") & (AOI2 == "NOSE1" | AOI2 == "NOSE2"))|
                                                   ((AOI1 == "NOSE1"| AOI1 =="NOSE2") & (AOI2 == "LEFTeye1" | AOI2 == "LEFTeye2"))){
                                                btwnLeyeNose = btwnLeyeNose + 1
                                        } else if (((AOI1 == "RIGHTeye1"| AOI1 =="RIGHTeye2") & (AOI2 == "NOSE1" | AOI2 == "NOSE2"))|
                                                   ((AOI1 == "NOSE1"| AOI1 =="NOSE2") & (AOI2 == "RIGHTeye1" | AOI2 == "RIGHTeye2"))) {
                                                btwnReyeNose = btwnReyeNose + 1
                                        } else if (((AOI1 == "LEFTeye1"| AOI1 =="LEFTeye2") & (AOI2 == "MOUTH1" | AOI2 == "MOUTH2"))|
                                                   ((AOI1 == "MOUTH1"| AOI1 =="MOUTH2") & (AOI2 == "LEFTeye1" | AOI2 == "LEFTeye2"))){
                                                btwnLeyeMouth = btwnLeyeMouth + 1
                                        } else if (((AOI1 == "RIGHTeye1"| AOI1 =="RIGHTeye2") & (AOI2 == "MOUTH1" | AOI2 == "MOUTH2"))|
                                                   ((AOI1 == "MOUTH1"| AOI1 =="MOUTH2") & (AOI2 == "RIGHTeye1" | AOI2 == "RIGHTeye2"))) {
                                                btwnReyeMouth = btwnReyeMouth + 1
                                        } else if (((AOI1 == "MOUTH1"| AOI1 =="MOUTH2") & (AOI2 == "NOSE1" | AOI2 == "NOSE2"))|
                                                   ((AOI1 == "NOSE1"| AOI1 =="NOSE2") & (AOI2 == "MOUTH1" | AOI2 == "MOUTH2"))) {
                                                btwnNoseMouth = btwnNoseMouth + 1
                                        } else if ((AOI1 == "NOSE1"| AOI1 =="NOSE2") & (AOI2 == "NOSE1" | AOI2 == "NOSE2")) {
                                                btwnNoseNose = btwnNoseNose + 1
                                        } else if ((AOI1 == "MOUTH1"| AOI1 =="MOUTH2") & (AOI2 == "MOUTH1" | AOI2 == "MOUTH2")) {
                                                btwnMouthMouth = btwnMouthMouth + 1
                                        } else
                                                print("ERROR")
                                }
                        }else
                                print("ERROR")
                        WhiteData <- data.frame(transFILE, "White", winEyes, winLeyeNose, winReyeNose,  winLeyeMouth,   winReyeMouth,
                                                winNoseMouth,   btwnLeyeReye,   btwnLeyeLeye,   btwnReyeReye,   btwnLeyeNose,
                                                btwnReyeNose,   btwnLeyeMouth, btwnReyeMouth,  btwnNoseMouth,  btwnNoseNose,   btwnMouthMouth)
                        names(WhiteData)= col.names
                        
                }
                fullSummary = rbind(HispanicData, AsianData, BlackData, WhiteData)
                summary=rbind(summary,fullSummary)
                winEyes = 0
                winLeyeNose = 0
                winReyeNose = 0
                winLeyeMouth = 0
                winReyeMouth = 0
                winNoseMouth = 0
                btwnLeyeReye = 0
                btwnLeyeLeye = 0
                btwnReyeReye = 0
                btwnLeyeNose = 0
                btwnReyeNose = 0
                btwnLeyeMouth = 0
                btwnReyeMouth = 0
                btwnNoseMouth = 0
                btwnNoseNose = 0
                btwnMouthMouth = 0
                HispanicData={}
                AsianData={}
                BlackData={}
                WhiteData={}
        }
        }





