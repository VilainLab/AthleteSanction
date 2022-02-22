###AthletePerCountry
    r <- read.csv("Z:/Suro/AtheleteSanction/AthleteSanctions_02162020.csv")
    dinf <- r$DateofInfraction
    r$YearofInfraction <- rep("-",nrow(r))
    r$NearestOlympicYear <- rep("-",nrow(r))
    st <- strsplit(dinf, split = "/")
    for(ii in 1:nrow(r)){
        r$YearofInfraction[ii] <- as.numeric(st[[ii]][3])
        yeardiff <- as.numeric(r$YearofInfraction[ii])%%4
		if(yeardiff == 0){
    	    r$NearestOlympicYear[ii] = as.numeric(r$YearofInfraction[ii])
        }else{
		    r$NearestOlympicYear[ii] = as.numeric(r$YearofInfraction[ii]) + (4 - yeardiff)
		}
		
    	'if(yeardiff == 0){
    	    r$NearestOlympicYear[ii] = as.numeric(r$YearofInfraction[ii])
        }else if(yeardiff <= 2){
    	    r$NearestOlympicYear[ii] = as.numeric(r$YearofInfraction[ii]) - (4 - yeardiff)
		}else{
		    r$NearestOlympicYear[ii] = as.numeric(r$YearofInfraction[ii]) + (4 - yeardiff)
		}'
		if(r$NearestOlympicYear[ii] == 2020){
		    r$NearestOlympicYear[ii] = 2016
		}else{r$NearestOlympicYear[ii] = r$NearestOlympicYear[ii]}
    }
	write.csv(r, "Z:/Suro/AtheleteSanction/AthleteSanctions_02162020_mod.csv", row.names = FALSE)
###Country Analysis
    library(hash)
    r1 <- read.csv("Z:/Suro/AtheleteSanction/AthleteSanctions_02162020_mod.csv")
	r2 <- read.csv("Z:/Suro/AtheleteSanction/CountryList.csv", check.names = FALSE)
	ha <- hash()
	.set(ha, keys = names(r2)[3:(length(names(r2)) -1)], 
	    values = 3:(length(names(r2)) -1))
	r1$TotalParticipant <- rep("-",nrow(r1))
	for(uu in 1:nrow(r1)){
	    cnt <- r1$Nationality[uu]; yr <- r1$NearestOlympicYear[uu]
		pascnt <- paste("^", cnt, "$", sep = "")
		pascnt1 <- paste("^", r2$NationAbbr, "$", sep = "")
		g1 <- grep(pascnt, pascnt1, fixed = TRUE)
		val <- hash::values(ha, keys = yr)
		if(length(g1) > 0 & length(val) > 0){
		    r1$TotalParticipant[uu] <- r2[g1, val]
		}else{
		    print(paste("Participant not found for country", cnt))
		}
	}
	write.csv(r1, "Z:/Suro/AtheleteSanction/AthleteSanctions_Participants.csv", row.names = FALSE)
	
	
###Classification By Income
athleteData <- r1
Country <- read.csv("Z:/Suro/AtheleteSanction/Country.csv")
ha <- hash()
.set(ha, keys = Country$Country_Code, values = Country$Classification)
atheletecount <- athleteData$Nationality
atheleteclass <- c()
for(i in 1:length(atheletecount)){
    print(paste("Country:", atheletecount[i]))
    atc <- hash::values(ha, keys = atheletecount[i])
	print(paste("Country:", atheletecount[i], "Athelete Class:", atc))
    atheleteclass <-c(atheleteclass, as.character(atc))
}
###Corruption
datcorrup <- read.csv("Z:/Suro/AtheleteSanction/Country - corruption.csv")
atheletecount <- athleteData$Nationality
cIndx <- c()
for(i in 1:length(atheletecount)){
    
    #
    datt <- datcorrup[which(datcorrup$Country_Code == atheletecount[i]),]
	if(nrow(datt)<1){
	    print(i)
		print(paste("Country:", atheletecount[i], "i:", i))
		cIndx <-c(cIndx,"-")
	}else{
        cIndx <-c(cIndx, as.character(datt$CPI.Score.2019))
	}
}
###Global South
datglob <- read.csv("Z:/Suro/AtheleteSanction/Country - global south.csv")
atheletecount <- athleteData$Nationality
gs <- c()
for(i in 1:length(atheletecount)){
    print(paste("Country:", atheletecount[i]))
	datt <- datglob[which(datglob$Country_Code == atheletecount[i]),]
	if(nrow(datt)== 0){
	    gs <-c(gs, "-")
	}else{
        gs <-c(gs, as.character(datt$Classification))
	}
    
}
###Human Index
dathdi <- read.csv("Z:/Suro/AtheleteSanction/Human Development Index.csv")
atheletecount <- athleteData$Nationality
hdi <- c()
for(i in 1:length(atheletecount)){
    #print(paste("Country:", atheletecount[i]))
    datt <- dathdi[which(dathdi$Country_Code == atheletecount[i]),]
	if(nrow(datt)== 0){
	   hdi <- c(hdi, -1)
	}else{
       hdi <-c(hdi, as.character(datt$HumanIndex))
	}
}
datFinal <- data.frame(athleteData,
    EconomicClass= atheleteclass,
    CorruptionIndex = cIndx, 
    GlobalClassification = gs, 
    HumanIndex =hdi)
write.csv(datFinal,"Z:/Suro/AtheleteSanction/ModifiedAtlete_02172020.csv", row.names = FALSE)
    'dinf <- r$DateofInfraction
    r$YearofInfraction <- rep("-",nrow(r))
    r$NearestOlympicYear <- rep("-",nrow(r))
    st <- strsplit(dinf, split = "/")
    for(ii in 1:nrow(r)){
        r$YearofInfraction[ii] <- as.numeric(st[[ii]][3])
        yeardiff <- as.numeric(r$YearofInfraction[ii])%%4
    	if(yeardiff == 0){
    	    r$NearestOlympicYear[ii] = as.numeric(r$YearofInfraction[ii])
        }else{
    	    r$NearestOlympicYear[ii] = as.numeric(r$YearofInfraction[ii]) + (4 - yeardiff)
		}
		if(r$NearestOlympicYear[ii] == 2020){
		    r$NearestOlympicYear[ii] = 2016
		}else{r$NearestOlympicYear[ii] = r$NearestOlympicYear[ii]}
    }
	write.csv(r, "Z:/Suro/AtheleteSanction/AthleteSanctions_OlympicYear.csv", row.names = FALSE)'
###Map of the world 
outdir = "Z:/Suro/AtheleteSanction/AtheleteSanction01032021/"
datFinal <- read.csv("Z:/Suro/AtheleteSanction/AtheleteSanction05262021/ModifiedAtlete_05292021.csv")

#GlobalSouthVsGlobalNorth
outdir = "Z:/Suro/AtheleteSanction/AtheleteSanction05262021/"
datFinal <- read.csv("Z:/Suro/AtheleteSanction/AtheleteSanction05262021/AthleteInfo_12282021.csv")
r <-read.csv("Z:/Suro/AtheleteSanction/AtheleteSanction05262021/Athlete_Countries_10-27-2021.csv")
datGlobS <- datFinal[which(datFinal$GlobalClassification == "GS"),]
nrow(datGlobS)
write.csv(datGlobS, file.path(outdir,"GlobalSouth.csv"), row.names = FALSE)
countryTotalGS = 0
uniqCount <- unique(datGlobS$Nationality); 
pasCount <- paste0("^", r$NationAbbr, "$")
country <- c(); year <- c(); Participants <- c()
for(ii in 1:length(uniqCount)){
    datS <- datGlobS[which(datGlobS$Nationality == uniqCount[ii]),]
	print(uniqCount[ii])
	print(ii)
	uniqyr <- unique(datS$NearestOlympicYear)
	#print(countryTotalGS)
	datt <- r[which(r$NationAbbr == uniqCount[ii]),]
	countryTotalGS = countryTotalGS + unique(datt$Country_Total)
	print(countryTotalGS)
	'if(length(uniqyr) > 1){
	    for(jj in 1:length(uniqyr)){
		    datSY <- datS[which(datS$NearestOlympicYear == uniqyr[jj]),]
			country <- c(country, uniqCount[ii]); 
			#year <- c(year, uniqyr[jj]); 
			if(length(unique(datSY$TotalParticipant))>1){
			    print(uniqCount[ii])
				print(ii)
		        print(length(unique(datSY$TotalParticipant)))
		    }
		    datt <- r[which(r$NationAbbr == uniqCount[ii]),]
            Participants <- c(Participants, unique(datt$Country_Total))
		    countryTotalGS = countryTotalGS + unique(datt$Country_Total)
			print(countryTotalGS)
        }
	}else{
	    datSY <- datS[which(datS$NearestOlympicYear == uniqyr),]
		country <- c(country, uniqCount[ii]); 
		#year <- c(year, uniqyr); 
		if(length(unique(datSY$TotalParticipant))>1){
			    print(uniqCount[ii])
				print(ii)
		        print(length(unique(datSY$TotalParticipant)))
		}
		countryTotalGS = countryTotalGS + unique(datSY$Country_Total) 
		print(countryTotalGS)
	}'
}
#datCountry <- data.frame(country, year, Participants)
percGS <- (nrow(datGlobS)/countryTotalGS)*100
'for (ii in 1:nrow(datGlobS)){
     
    countryTotalGS = countryTotalGS + datGlobS$TotalParticipant[ii]
}'
datGlobN <- datFinal[which(datFinal$GlobalClassification == "GN"),]
nrow(datGlobN)
write.csv(datGlobN, file.path(outdir,"GlobalNorth_02192020.csv"), row.names = FALSE)
countryTotalGN = 0;
uniqCount <- unique(datGlobN$Nationality)
 country <- c()
 year <- c()
for(ii in 1:length(uniqCount)){
    print(uniqCount[ii])
    datN <- datGlobN[which(datGlobN$Nationality == uniqCount[ii]),]
	datt <- r[which(r$NationAbbr == uniqCount[ii]),]
	countryTotalGN = countryTotalGN + unique(datt$Country_Total)
	print(countryTotalGN)
	#uniqyr <- unique(datN$NearestOlympicYear)
	'if(length(uniqyr) > 1){
	    
	    for(jj in 1:length(uniqyr)){
		    datNY <- datN[which(datN$NearestOlympicYear == uniqyr[jj]),]
			if(length(unique(datNY$TotalParticipant)) > 1){
			    print(ii)
			    print(uniqyr[jj]); print(uniqCount[ii])
			}else{print("OK")}
			    country <- c(country, uniqCount[ii]); 
		        year <- c(year, uniqyr[jj])
		       datt <- r[which(r$NationAbbr == uniqCount[ii]),]
                    # if(datt$Country_Total == " "){print(uniqCount[ii])}
                    #Participants <- c(Participants, (datt$Country_Total))
		    countryTotalGN = countryTotalGN + (datt$Country_Total)
        }
	}else{
	    datNY <- datN[which(datN$NearestOlympicYear == uniqyr),]
		datt <- r[which(r$NationAbbr == uniqCount[ii]),]
               # if(datt$Country_Total == " "){print(uniqCount[ii])}
                    #Participants <- c(Participants, (datt$Country_Total))
		    countryTotalGN = countryTotalGN + (datt$Country_Total)
		country <- c(country, uniqCount[ii]); 
		#year <- c(year, uniqyr)
	}'
}
datCountry <- data.frame(country,Participants)
#percGS <- (nrow(datGlobS)/countryTotalGS)*100
percGS <- (nrow(datGlobS)/countryTotalGS)*100
percGN <- (nrow(datGlobN)/countryTotalGN)*100
dat <- data.frame(GlobalClassifications = c("GN", "GS"),
    NumberofAthletes = c(nrow(datGlobN), nrow(datGlobS)),
	TotalAthletes = c(countryTotalGN, countryTotalGS),
    SanctionPercent = c(percGN, percGS))
write.csv(dat, file.path(outdir,"GlobalClassifications_02192020.csv"), row.names = FALSE)
library(ggplot2)
p6 <- ggplot(data = dat, aes(x = GlobalClassifications, y= SanctionPercent, fill = GlobalClassifications)) + geom_bar(stat = "identity")+ labs(x = "GlobalClassifications", y = "% of Sanctioned Athletes") + theme(legend.title = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.title.x = element_text(face = "bold", size = 50),axis.title.y = element_text(face = "bold", size = 50), axis.text.x = element_text(face="bold", size = 30, angle = 45, vjust = 0.5), axis.text.y = element_text(face="bold", size = 30), legend.text=element_text(face = "bold",size=25))
p6
####Global South
datGlobS_lessThan24 <- datFinal[which(datFinal$GlobalClassification == "GS" 
    & datFinal$Sanction_months <= 24),]
countryTotalGS_24 = 0
uniqCount_less24 <- unique(datGlobS_lessThan24$Nationality); 
for(ii in 1:length(uniqCount_less24)){
    #datS_24 <- datGlobS_lessThan24[which(datGlobS_lessThan24$Nationality == uniqCount_less24[ii]),]
	#uniqyr_24 <- unique(datS_24$NearestOlympicYear)
	datS_24Y <- r[which(r$NationAbbr == uniqCount_less24[ii]),]
	countryTotalGS_24 = countryTotalGS_24 + unique(datS_24Y$Country_Total)
	print(uniqCount_less24[ii])
	print(countryTotalGS_24)
	'if(length(uniqyr_24) > 1){
	    for(jj in 1:length(uniqyr_24)){
		    datS_24Y <- datS_24[which(datS_24$NearestOlympicYear == uniqyr_24[jj]),]
		    countryTotalGS_24 = countryTotalGS_24 + unique(datS_24Y$Country_Total)
                     
                    
                   
		    countryTotalGN = countryTotalGN + )
        }
	}else{
	    datS_24Y <- datS_24[which(datS_24$NearestOlympicYear == uniqyr_24),]
		countryTotalGS_24 = countryTotalGS_24 + unique(datS_24Y$TotalParticipant)
	}'
}
percGS_24 <- (nrow(datGlobS_lessThan24)/countryTotalGS_24)*100
datGlobS_lessThan25_48 <- datFinal[which(datFinal$GlobalClassification == "GS" 
    & (datFinal$Sanction_months >= 25 & datFinal$Sanction_months <= 48)),]
countryTotalGS_25_48 = 0
uniqCount_less25_48 <- unique(datGlobS_lessThan25_48$Nationality); 
for(ii in 1:length(uniqCount_less25_48)){
    datS_25_48Y <- r[which(r$NationAbbr == uniqCount_less25_48[ii]),]
	countryTotalGS_25_48 = countryTotalGS_25_48 + unique(datS_25_48Y$Country_Total)
    'datS_25_48 <- datGlobS_lessThan25_48[which(datGlobS_lessThan25_48$Nationality == uniqCount_less25_48[ii]),]
	uniqyr_25_48 <- unique(datS_25_48$NearestOlympicYear)
	if(length(uniqyr_25_48) > 1){
	    for(jj in 1:length(uniqyr_25_48)){
		    datS_25_48Y <- datS_25_48[which(datS_25_48$NearestOlympicYear == uniqyr_25_48[jj]),]
		    countryTotalGS_25_48 = countryTotalGS_25_48 + unique(datS_25_48Y$TotalParticipant)
        }
	}else{
	    datS_25_48Y <- datS_25_48[which(datS_25_48$NearestOlympicYear == uniqyr_25_48),]
		countryTotalGS_25_48 = countryTotalGS_25_48 + unique(datS_25_48Y$TotalParticipant)
	}'
}
percGS_25_48 <- (nrow(datGlobS_lessThan25_48)/countryTotalGS_25_48)*100
datGlobS_lessThan49_96 <- datFinal[which(datFinal$GlobalClassification == "GS" 
    & (datFinal$Sanction_months >= 49 & datFinal$Sanction_months <= 96)),]
countryTotalGS_49_96 = 0
uniqCount_less49_96 <- unique(datGlobS_lessThan49_96$Nationality); 
for(ii in 1:length(uniqCount_less49_96)){
    datS_49_96Y <- r[which(r$NationAbbr == uniqCount_less49_96[ii]),]
	countryTotalGS_49_96 = countryTotalGS_49_96 + unique(datS_49_96Y$Country_Total)
    
    'datS_49_96 <- datGlobS_lessThan49_96[which(datGlobS_lessThan49_96$Nationality == uniqCount_less49_96[ii]),]
	uniqyr_49_96 <- unique(datS_49_96$NearestOlympicYear)
	if(length(uniqyr_49_96) > 1){
	    for(jj in 1:length(uniqyr_49_96)){
		    datS_49_96Y <- datS_49_96[which(datS_49_96$NearestOlympicYear == uniqyr_49_96[jj]),]
		    countryTotalGS_49_96 = countryTotalGS_49_96 + unique(datS_49_96Y$TotalParticipant)
        }
	}else{
	    datS_49_96Y <- datS_49_96[which(datS_49_96$NearestOlympicYear == uniqyr_49_96),]
		countryTotalGS_49_96 = countryTotalGS_49_96 + unique(datS_49_96Y$TotalParticipant)
	}'
}
percGS_49_96 <- (nrow(datGlobS_lessThan49_96)/countryTotalGS_49_96)*100
datGlobS_GreaterThan97 <- datFinal[which(datFinal$GlobalClassification == "GS" 
    & (datFinal$Sanction_months >= 97)),]
countryTotalGS_97 = 0
uniqCount_97 <- unique(datGlobS_GreaterThan97$Nationality); 
for(ii in 1:length(uniqCount_97)){
    datS_97Y <- r[which(r$NationAbbr == uniqCount_97[ii]),]
	countryTotalGS_97 = countryTotalGS_97 + unique(datS_97Y$Country_Total)
    'datS_97 <- datGlobS_GreaterThan97[which(datGlobS_GreaterThan97$Nationality == uniqCount_97[ii]),]
	uniqyr_97 <- unique(datS_97$NearestOlympicYear)
	if(length(uniqyr_97) > 1){
	    for(jj in 1:length(uniqyr_97)){
		    datS_97Y <- datS_97[which(datS_97$NearestOlympicYear == uniqyr_97[jj]),]
		    countryTotalGS_97 = countryTotalGS_97 + unique(datS_97Y$TotalParticipant)
        }
	}else{
	    datS_97Y <- datS_97[which(datS_97$NearestOlympicYear == uniqyr_97),]
		countryTotalGS_97 = countryTotalGS_97 + unique(datS_97Y$TotalParticipant)
	}'
}
percGS_97 <- (nrow(datGlobS_GreaterThan97)/countryTotalGS_97)*100

####Global North
datGlobN_lessThan24 <- datFinal[which(datFinal$GlobalClassification == "GN" 
    & datFinal$Sanction_months <= 24),]
countryTotalGN_24 = 0
uniqCount_less24 <- unique(datGlobN_lessThan24$Nationality); 
for(ii in 1:length(uniqCount_less24)){
    datN_24Y <- r[which(r$NationAbbr == uniqCount_less24[ii]),]
	countryTotalGN_24 = countryTotalGN_24 + unique(datN_24Y$Country_Total)
	
	'datN_24 <- datGlobN_lessThan24[which(datGlobN_lessThan24$Nationality == uniqCount_less24[ii]),]
	uniqyr_24 <- unique(datN_24$NearestOlympicYear)
	if(length(uniqyr_24) > 1){
	    for(jj in 1:length(uniqyr_24)){
		    datN_24Y <- datN_24[which(datN_24$NearestOlympicYear == uniqyr_24[jj]),]
		    countryTotalGN_24 = countryTotalGN_24 + unique(datN_24Y$TotalParticipant)
        }
	}else{
	    datN_24Y <- datN_24[which(datN_24$NearestOlympicYear == uniqyr_24),]
		countryTotalGN_24 = countryTotalGN_24 + unique(datN_24Y$TotalParticipant)
	}'
}
percGN_24 <- (nrow(datGlobN_lessThan24)/countryTotalGN_24)*100
datGlobN_lessThan25_48 <- datFinal[which(datFinal$GlobalClassification == "GN" 
    & (datFinal$Sanction_months >= 25 & datFinal$Sanction_months <= 48)),]
countryTotalGN_25_48 = 0
uniqCount_less25_48 <- unique(datGlobN_lessThan25_48$Nationality); 
for(ii in 1:length(uniqCount_less25_48)){
    datN_25_48Y <- r[which(r$NationAbbr == uniqCount_less25_48[ii]),]
	countryTotalGN_25_48 = countryTotalGN_25_48 + unique(datN_25_48Y$Country_Total)
    'datN_25_48 <- datGlobN_lessThan25_48[which(datGlobN_lessThan25_48$Nationality == uniqCount_less25_48[ii]),]
	uniqyr_25_48 <- unique(datN_25_48$NearestOlympicYear)
	if(length(uniqyr_25_48) > 1){
	    for(jj in 1:length(uniqyr_25_48)){
		    datN_25_48Y <- datN_25_48[which(datN_25_48$NearestOlympicYear == uniqyr_25_48[jj]),]
		    countryTotalGN_25_48 = countryTotalGN_25_48 + unique(datN_25_48Y$TotalParticipant)
        }
	}else{
	    datN_25_48Y <- datN_25_48[which(datN_25_48$NearestOlympicYear == uniqyr_25_48),]
		countryTotalGN_25_48 = countryTotalGN_25_48 + unique(datN_25_48Y$TotalParticipant)
	}'
}
percGN_25_48 <- (nrow(datGlobN_lessThan25_48)/countryTotalGN_25_48)*100
datGlobN_lessThan49_96 <- datFinal[which(datFinal$GlobalClassification == "GN" 
    & (datFinal$Sanction_months >= 49 & datFinal$Sanction_months <= 96)),]
countryTotalGN_49_96 = 0
uniqCount_less49_96 <- unique(datGlobN_lessThan49_96$Nationality); 
for(ii in 1:length(uniqCount_less49_96)){
    datN_49_96Y <- r[which(r$NationAbbr == uniqCount_less49_96[ii]),]
	countryTotalGN_49_96 = countryTotalGN_49_96 + unique(datN_49_96Y$Country_Total)
    'datN_49_96 <- datGlobN_lessThan49_96[which(datGlobN_lessThan49_96$Nationality == uniqCount_less49_96[ii]),]
	uniqyr_97 <- unique(datN_49_96$NearestOlympicYear)
	if(length(uniqyr_97) > 1){
	    for(jj in 1:length(uniqyr_97)){
		    datN_49_96Y <- datN_49_96[which(datN_49_96$NearestOlympicYear == uniqyr_97[jj]),]
		    countryTotalGN_49_96 = countryTotalGN_49_96 + unique(datN_49_96Y$TotalParticipant)
        }
	}else{
	    datN_49_96Y <- datN_49_96[which(datN_49_96$NearestOlympicYear == uniqyr_97),]
		countryTotalGN_49_96 = countryTotalGN_49_96 + unique(datN_49_96Y$TotalParticipant)
	}'
}
percGN_49_96 <- (nrow(datGlobN_lessThan49_96)/countryTotalGN_49_96)*100
datGlobN_GreaterThan97 <- datFinal[which(datFinal$GlobalClassification == "GN" 
    & (datFinal$Sanction_months >= 97)),]
countryTotalGN_97 = 0
uniqCount_97 <- unique(datGlobN_GreaterThan97$Nationality); 
for(ii in 1:length(uniqCount_97)){
    datN_97Y <- r[which(r$NationAbbr == uniqCount_97[ii]),]
	countryTotalGN_97 = countryTotalGN_97 + unique(datN_97Y$Country_Total)
    'datN_97 <- datGlobN_GreaterThan97[which(datGlobN_GreaterThan97$Nationality == uniqCount_97[ii]),]
	uniqyr_97 <- unique(datN_97$NearestOlympicYear)
	if(length(uniqyr_97) > 1){
	    for(jj in 1:length(uniqyr_97)){
		    datN_97Y <- datN_97[which(datN_97$NearestOlympicYear == uniqyr_97[jj]),]
		    countryTotalGN_97 = countryTotalGN_97 + unique(datN_97Y$TotalParticipant)
        }
	}else{
	    datN_97Y <- datN_97[which(datN_97$NearestOlympicYear == uniqyr_97),]
		countryTotalGN_97 = countryTotalGN_97 + unique(datN_97Y$TotalParticipant)
	}'
}
percGN_97 <- (nrow(datGlobN_GreaterThan97)/countryTotalGN_97)*100
datgn_gs <- data.frame(SanctionPeriod = c(
            "<=24mnths",
			"<=24mnths",
	        "25_48mnths", 
			"25_48mnths",
	        "49_96mnths",
			"49_96mnths",
			">=97mnths",
			">=97mnths"),
        NumberofAthletes = c(
	        nrow(datGlobN_lessThan24),
            nrow(datGlobS_lessThan24),			
	        nrow(datGlobN_lessThan25_48),
			nrow(datGlobS_lessThan25_48),
		    nrow(datGlobN_lessThan49_96),
			nrow(datGlobS_lessThan49_96),
		    nrow(datGlobN_GreaterThan97),
			nrow(datGlobS_GreaterThan97)),
	    TotalAthletes = c(countryTotalGN_24, 
		    countryTotalGS_24,
		    countryTotalGN_25_48,
			countryTotalGS_25_48,
			countryTotalGN_49_96,
			countryTotalGS_49_96,
			countryTotalGN_97,
			countryTotalGS_97),
        SanctionPercent = c(percGN_24,
		    percGS_24,
     		percGN_25_48,
			percGS_25_48,
			percGN_49_96,
			percGS_49_96,
			percGN_97,
			percGS_97),
		GlobalClassification = c("GN",
		    "GS",
     		"GN",
		    "GS",
			"GN",
		    "GS",
			"GN",
		    "GS"))
write.csv(datgn_gs, file.path(outdir,"GlobalClassifications_GN_GS.csv"), row.names = FALSE)
library(ggplot2)
library(ggplot2)

#r5 <- read.csv("Z:/Suro/AtheleteSanction/GlobalClassificationvsSanctionMonths.csv")
p7 <- ggplot(data = datgn_gs, aes(x = factor(SanctionPeriod, levels=unique(SanctionPeriod)), y= SanctionPercent, fill = factor(GlobalClassification, level = c("GN","GS")))) + geom_bar(stat = "identity", position="dodge")+ labs(x = "SanctionPeriod", y = "% of Sanctioned Athletes", fill = "Global Classifications") + theme(legend.title = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.title.x = element_text(face = "bold", size = 50),axis.title.y = element_text(face = "bold", size = 50), axis.text.x = element_text(face="bold", size = 30, angle = 45, vjust = 0.5), axis.text.y = element_text(face="bold", size = 30), legend.text=element_text(face = "bold",size=25))  
p7

####Sex Division GS_vs_GN
#GlobalSouthVsGlobalNorth
outdir = "Z:/Suro/AtheleteSanction/AtheleteSanction05262021/"
datFinal <- read.csv("Z:/Suro/AtheleteSanction/AtheleteSanction05262021/AthleteInfo_12282021.csv")
r <-read.csv("Z:/Suro/AtheleteSanction/AtheleteSanction05262021/Athlete_Countries_10-27-2021.csv")
#datGlobS <- datFinal[which(datFinal$GlobalClassification == "GS"),]
'nrow(datGlobS)
write.csv(datGlobS, file.path(outdir,"GlobalSouth.csv"), row.names = FALSE)
countryMaleGS = 0
countryFemaleGS = 0
countryTotalGS = 0
totalpercountry <- c()
uniqCount <- unique(datGlobS$Nationality); 
country <- c(); year <- c(); MaleParticipants <- c(); FemaleParticipants <- c()
for(ii in 1:length(uniqCount)){
    datS <- datGlobS[which(datGlobS$Nationality == uniqCount[ii]),]
	#uniqyr <- unique(datS$NearestOlympicYear)
    countryMaleGS = countryMaleGS + unique(datSY$Male)
	countryFemaleGS = countryFemaleGS + unique(datSY$Female)
	countryTotalGS = countryTotalGS + unique(datSY$TotalParticipant)
	
}'
#datCountry <- data.frame(country, year, MaleParticipants, FemaleParticipants, countryTotalGS, totalpercountry)
datGlobS <- datFinal[which(datFinal$GlobalClassification == "GS"),]
nrow(datGlobS)
write.csv(datGlobS, file.path(outdir,"GlobalSouth_02192020.csv"), row.names = FALSE)
countryMaleGS = 0; countryFemaleGS = 0
uniqCount <- unique(datGlobS$Nationality); 
countryTotalGS = 0; totalpercountry <- c()
MaleParticipants <- c();  FemaleParticipants <- c()
country <-c(); year <- c()
for(ii in 1:length(uniqCount)){
    datS <- datGlobS[which(datGlobS$Nationality == uniqCount[ii]),]
	uniqyr <- unique(datS$NearestOlympicYear)
	if(length(uniqyr) > 1){
	    
	    for(jj in 1:length(uniqyr)){
		    country <- c(country, uniqCount[ii]); 
			year <- c(year, uniqyr[jj]); 
		    datSY <- datS[which(datS$NearestOlympicYear == uniqyr[jj]),]
			if(length(unique(datSY$Male)) > 1){
			    print(ii)
			    print(uniqyr[jj]); print(uniqCount[ii])
			}else{print("OK")}
			    print(ii)
				print(uniqCount[ii])
				print(unique(datSY$Female))
			    MaleParticipants <- c(MaleParticipants, unique(datSY$Male))
		        FemaleParticipants <- c(FemaleParticipants, unique(datSY$Female)) 
		        countryMaleGS = countryMaleGS + unique(datSY$Male)
				countryFemaleGS = countryFemaleGS + unique(datSY$Female)
				sumM_F <- sum(unique(datSY$Male), unique(datSY$Female))
			    totalpercountry <- c(totalpercountry,sumM_F)
				countryTotalGS = countryTotalGS + unique(datSY$TotalParticipant)
        }
	}else{
	    MaleParticipants <- c(MaleParticipants, unique(datSY$Male))
		FemaleParticipants <- c(FemaleParticipants, unique(datSY$Female))
	    country <- c(country, uniqCount[ii]); 
		print(ii)
		print(uniqCount[ii])
		print(unique(datSY$Female))
		year <- c(year, uniqyr); 
	    datSY <- datS[which(datS$NearestOlympicYear == uniqyr),]
		countryMaleGS = countryMaleGS + unique(datSY$Male)
		countryFemaleGS = countryFemaleGS + unique(datSY$Female)
		sumM_F <- sum(unique(datSY$Male), unique(datSY$Female))
		totalpercountry <- c(totalpercountry,sumM_F)
		countryTotalGS = countryTotalGS + unique(datSY$TotalParticipant)
	}
}

#percGS <- (nrow(datGlobS)/countryMaleGS)*100
'for (ii in 1:nrow(datGlobS)){
     
    countryMaleGS = countryMaleGS + datGlobS$Male[ii]
}'
datGlobN <- datFinal[which(datFinal$GlobalClassification == "GN"),]
nrow(datGlobN)
write.csv(datGlobN, file.path(outdir,"GlobalNorth_02192020.csv"), row.names = FALSE)
countryMaleGN = 0; countryFemaleGN = 0
uniqCount <- unique(datGlobN$Nationality); 
countryTotalGN = 0; totalpercountry <- c()
MaleParticipants <- c();  FemaleParticipants <- c()
country <-c(); year <- c()
for(ii in 1:length(uniqCount)){
    datN <- datGlobN[which(datGlobN$Nationality == uniqCount[ii]),]
	uniqyr <- unique(datN$NearestOlympicYear)
	if(length(uniqyr) > 1){
	    
	    for(jj in 1:length(uniqyr)){
		    country <- c(country, uniqCount[ii]); 
			year <- c(year, uniqyr[jj]); 
		    datNY <- datN[which(datN$NearestOlympicYear == uniqyr[jj]),]
			if(length(unique(datNY$Male)) > 1){
			    print(ii)
			    print(uniqyr[jj]); print(uniqCount[ii])
			}else{print("OK")}
			    MaleParticipants <- c(MaleParticipants, unique(datNY$Male))
		        FemaleParticipants <- c(FemaleParticipants, unique(datNY$Female)) 
		        countryMaleGN = countryMaleGN + unique(datNY$Male)
				countryFemaleGN = countryFemaleGN + unique(datNY$Female)
				sumM_F <- sum(unique(datNY$Male), unique(datNY$Female))
			    totalpercountry <- c(totalpercountry,sumM_F)
				countryTotalGN = countryTotalGN + unique(datNY$TotalParticipant)
        }
	}else{
	    MaleParticipants <- c(MaleParticipants, unique(datNY$Male))
		FemaleParticipants <- c(FemaleParticipants, unique(datNY$Female))
	    country <- c(country, uniqCount[ii]); 
		year <- c(year, uniqyr); 
	    datNY <- datN[which(datN$NearestOlympicYear == uniqyr),]
		countryMaleGN = countryMaleGN + unique(datNY$Male)
		countryFemaleGN = countryFemaleGN + unique(datNY$Female)
		sumM_F <- sum(unique(datNY$Male), unique(datNY$Female))
		totalpercountry <- c(totalpercountry,sumM_F)
		countryTotalGN = countryTotalGN + unique(datNY$TotalParticipant)
	}
}
datCountry <- data.frame(country, year, MaleParticipants, FemaleParticipants, countryTotalGN, totalpercountry)
#percGS <- (nrow(datGlobS)/countryMaleGS)*100
percGS_Male <- (countryMaleGS/countryTotalGS)*100
percGN_Male <- (countryMaleGN/countryTotalGN)*100
percGS_Female <- (countryFemaleGS/countryTotalGS)*100
percGN_Female <- (countryFemaleGN/countryTotalGN)*100
dat <- data.frame(GlobalClassifications = c("GN", "GS"),
    NumberofAthletes = c(countryTotalGN, countryTotalGS),
	TotalMaleAthletes = c(countryMaleGN, countryMaleGS),
	TotalFemaleAthletes = c(countryFemaleGN, countryFemaleGS),
    SanctionPercentMale = c(percGN_Male, percGS_Male),
	SanctionPercentFemale = c(percGN_Female, percGS_Female))
write.csv(dat, file.path(outdir,"GlobalClassificationsMale_Female_02192020.csv"), row.names = FALSE)
datMaleFemale <- data.frame(GlobalClassifications = c("GN", "GN", "GS", "GS"),
    Sex = c("Male", "Female", "Male", "Female"),
    TotalAthletes = c(countryMaleGN, countryFemaleGN, countryMaleGS, countryFemaleGS),
	SanctionPercent = c(percGN_Male, percGN_Female, percGS_Male, percGS_Female))
write.csv(dat, file.path(outdir,"GlobalClassificationsMale_FemalePlotReady_05282020.csv"), row.names = FALSE)
library(ggplot2)
p6 <- ggplot(data = datMaleFemale, aes(x = GlobalClassifications, y= SanctionPercent, fill = factor(Sex, level= c("Male", "Female")))) + geom_bar(stat = "identity")+ labs(x = "GlobalClassifications", y = "% of Sanctioned Athletes", fill = "Sex") + theme(legend.title = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.title.x = element_text(face = "bold", size = 50),axis.title.y = element_text(face = "bold", size = 50), axis.text.x = element_text(face="bold", size = 30, angle = 45, vjust = 0.5), axis.text.y = element_text(face="bold", size = 30), legend.text=element_text(face = "bold",size=25)) + scale_fill_manual(values=c("green","hotpink"))
p6
####Global South
datGlobS_lessThan24 <- datFinal[which(datFinal$GlobalClassification == "GS" 
    & datFinal$Sanction_months <= 24),]
countryMaleGS_24 = 0;countryFemaleGS_24 = 0;countryTotalGS_24 = 0
MaleParticipantsGS_24 <- c();FemaleParticipantsGS_24 <- c()
uniqCount_less24 <- unique(datGlobS_lessThan24$Nationality); 
year <- c();totalpercountry_24 <- c()
for(ii in 1:length(uniqCount_less24)){
    datS_24 <- datGlobS_lessThan24[which(datGlobS_lessThan24$Nationality == uniqCount_less24[ii]),]
	uniqyr_24 <- unique(datS_24$NearestOlympicYear)
	
	if(length(uniqyr_24) > 1){
	    for(jj in 1:length(uniqyr_24)){
		    datS_24Y <- datS_24[which(datS_24$NearestOlympicYear == uniqyr_24[jj]),]
			MaleParticipantsGS_24 <- c(MaleParticipantsGS_24, unique(datS_24Y$Male))
		    FemaleParticipantsGS_24 <- c(FemaleParticipantsGS_24, unique(datS_24Y$Female)) 
			year <- c(year, uniqyr); 
			countryMaleGS_24 = countryMaleGS_24 + unique(datS_24Y$Male)
			countryFemaleGS_24 = countryFemaleGS_24 + unique(datS_24Y$Female)
			sumM_F_24 <- sum(unique(datNY$Male), unique(datNY$Female))
			totalpercountry_24 <- c(totalpercountry_24,sumM_F_24)
			countryTotalGS_24 = countryTotalGS_24 + unique(datS_24Y$TotalParticipant)
        }
	}else{
	    datS_24Y <- datS_24[which(datS_24$NearestOlympicYear == uniqyr_24),]
		MaleParticipantsGS_24 <- c(MaleParticipantsGS_24, unique(datS_24Y$Male))
		FemaleParticipantsGS_24 <- c(FemaleParticipantsGS_24, unique(datS_24Y$Female)) 
		year <- c(year, uniqyr); 
		countryMaleGS_24 = countryMaleGS_24 + unique(datS_24Y$Male)
		countryFemaleGS_24 = countryFemaleGS_24 + unique(datS_24Y$Female)
		sumM_F_24 <- sum(unique(datNY$Male), unique(datNY$Female))
		totalpercountry_24 <- c(totalpercountry_24,sumM_F_24)
		countryTotalGS_24 = countryTotalGS_24 + unique(datS_24Y$TotalParticipant)
	}
}
percGS_24_Male <- ((countryMaleGS_24)/countryTotalGS_24)*100
percGS_24_Female <- ((countryFemaleGS_24)/countryTotalGS_24)*100
datGlobS_lessThan25_48 <- datFinal[which(datFinal$GlobalClassification == "GS" 
    & (datFinal$Sanction_months >= 25 & datFinal$Sanction_months <= 48)),]
countryMaleGS_25_48 = 0;countryFemaleGS_25_48 = 0;countryTotalGS_25_48 = 0
MaleParticipantsGS_25_48 <- c();FemaleParticipantsGS_25_48 <- c()
uniqCount_less24 <- unique(datGlobS_lessThan24$Nationality); 
year <- c();totalpercountry_25_48 <- c()
for(ii in 1:length(uniqCount_less24)){
    datS_25_48 <- datGlobS_lessThan24[which(datGlobS_lessThan24$Nationality == uniqCount_less24[ii]),]
	uniqyr_25_48 <- unique(datS_25_48$NearestOlympicYear)
	
	if(length(uniqyr_25_48) > 1){
	    for(jj in 1:length(uniqyr_25_48)){
		    datS_25_48Y <- datS_25_48[which(datS_25_48$NearestOlympicYear == uniqyr_25_48[jj]),]
			MaleParticipantsGS_25_48 <- c(MaleParticipantsGS_25_48, unique(datS_25_48Y$Male))
		    FemaleParticipantsGS_25_48 <- c(FemaleParticipantsGS_25_48, unique(datS_25_48Y$Female)) 
			year <- c(year, uniqyr); 
			countryMaleGS_25_48 = countryMaleGS_25_48 + unique(datS_25_48Y$Male)
			countryFemaleGS_25_48 = countryFemaleGS_25_48 + unique(datS_25_48Y$Female)
			sumM_F_25_48 <- sum(unique(datNY$Male), unique(datNY$Female))
			totalpercountry_25_48 <- c(totalpercountry_25_48,sumM_F_25_48)
			countryTotalGS_25_48 = countryTotalGS_25_48 + unique(datS_25_48Y$TotalParticipant)
        }
	}else{
	    datS_25_48Y <- datS_25_48[which(datS_25_48$NearestOlympicYear == uniqyr_25_48),]
		MaleParticipantsGS_25_48 <- c(MaleParticipantsGS_25_48, unique(datS_25_48Y$Male))
		FemaleParticipantsGS_25_48 <- c(FemaleParticipantsGS_25_48, unique(datS_25_48Y$Female)) 
		year <- c(year, uniqyr); 
		countryMaleGS_25_48 = countryMaleGS_25_48 + unique(datS_25_48Y$Male)
		countryFemaleGS_25_48 = countryFemaleGS_25_48 + unique(datS_25_48Y$Female)
		sumM_F_25_48 <- sum(unique(datNY$Male), unique(datNY$Female))
		totalpercountry_25_48 <- c(totalpercountry_25_48,sumM_F_25_48)
		countryTotalGS_25_48 = countryTotalGS_25_48 + unique(datS_25_48Y$TotalParticipant)
	}
}
percGS_25_48_Male <- ((countryMaleGS_25_48)/countryTotalGS_25_48)*100
percGS_25_48_Female <- ((countryFemaleGS_25_48)/countryTotalGS_25_48)*100
datGlobS_lessThan49_96 <- datFinal[which(datFinal$GlobalClassification == "GS" 
    & (datFinal$Sanction_months >= 49 & datFinal$Sanction_months <= 96)),]
countryMaleGS_49_96 = 0;countryFemaleGS_49_96 = 0;countryTotalGS_49_96 = 0
MaleParticipantsGS_49_96 <- c();FemaleParticipantsGS_49_96 <- c()
uniqCount_less24 <- unique(datGlobS_lessThan24$Nationality); 
year <- c();totalpercountry_49_96 <- c()
for(ii in 1:length(uniqCount_less24)){
    datS_49_96 <- datGlobS_lessThan24[which(datGlobS_lessThan24$Nationality == uniqCount_less24[ii]),]
	uniqyr_49_96 <- unique(datS_49_96$NearestOlympicYear)
	
	if(length(uniqyr_49_96) > 1){
	    for(jj in 1:length(uniqyr_49_96)){
		    datS_49_96Y <- datS_49_96[which(datS_49_96$NearestOlympicYear == uniqyr_49_96[jj]),]
			MaleParticipantsGS_49_96 <- c(MaleParticipantsGS_49_96, unique(datS_49_96Y$Male))
		    FemaleParticipantsGS_49_96 <- c(FemaleParticipantsGS_49_96, unique(datS_49_96Y$Female)) 
			year <- c(year, uniqyr); 
			countryMaleGS_49_96 = countryMaleGS_49_96 + unique(datS_49_96Y$Male)
			countryFemaleGS_49_96 = countryFemaleGS_49_96 + unique(datS_49_96Y$Female)
			sumM_F_49_96 <- sum(unique(datNY$Male), unique(datNY$Female))
			totalpercountry_49_96 <- c(totalpercountry_49_96,sumM_F_49_96)
			countryTotalGS_49_96 = countryTotalGS_49_96 + unique(datS_49_96Y$TotalParticipant)
        }
	}else{
	    datS_49_96Y <- datS_49_96[which(datS_49_96$NearestOlympicYear == uniqyr_49_96),]
		MaleParticipantsGS_49_96 <- c(MaleParticipantsGS_49_96, unique(datS_49_96Y$Male))
		FemaleParticipantsGS_49_96 <- c(FemaleParticipantsGS_49_96, unique(datS_49_96Y$Female)) 
		year <- c(year, uniqyr); 
		countryMaleGS_49_96 = countryMaleGS_49_96 + unique(datS_49_96Y$Male)
		countryFemaleGS_49_96 = countryFemaleGS_49_96 + unique(datS_49_96Y$Female)
		sumM_F_49_96 <- sum(unique(datNY$Male), unique(datNY$Female))
		totalpercountry_49_96 <- c(totalpercountry_49_96,sumM_F_49_96)
		countryTotalGS_49_96 = countryTotalGS_49_96 + unique(datS_49_96Y$TotalParticipant)
	}
}
percGS_49_96_Male <- ((countryMaleGS_49_96)/countryTotalGS_49_96)*100
percGS_49_96_Female <- ((countryFemaleGS_49_96)/countryTotalGS_49_96)*100
datGlobS_GreaterThan97 <- datFinal[which(datFinal$GlobalClassification == "GS" 
    & (datFinal$Sanction_months >= 97)),]
countryMaleGS_GreaterThan97 = 0;countryFemaleGS_GreaterThan97 = 0;countryTotalGS_GreaterThan97 = 0
MaleParticipantsGS_GreaterThan97 <- c();FemaleParticipantsGS_GreaterThan97 <- c()
uniqCount_less24 <- unique(datGlobS_lessThan24$Nationality); 
year <- c();totalpercountry_GreaterThan97 <- c()
for(ii in 1:length(uniqCount_less24)){
    datS_GreaterThan97 <- datGlobS_lessThan24[which(datGlobS_lessThan24$Nationality == uniqCount_less24[ii]),]
	uniqyr_GreaterThan97 <- unique(datS_GreaterThan97$NearestOlympicYear)
	
	if(length(uniqyr_GreaterThan97) > 1){
	    for(jj in 1:length(uniqyr_GreaterThan97)){
		    datS_GreaterThan97Y <- datS_GreaterThan97[which(datS_GreaterThan97$NearestOlympicYear == uniqyr_GreaterThan97[jj]),]
			MaleParticipantsGS_GreaterThan97 <- c(MaleParticipantsGS_GreaterThan97, unique(datS_GreaterThan97Y$Male))
		    FemaleParticipantsGS_GreaterThan97 <- c(FemaleParticipantsGS_GreaterThan97, unique(datS_GreaterThan97Y$Female)) 
			year <- c(year, uniqyr); 
			countryMaleGS_GreaterThan97 = countryMaleGS_GreaterThan97 + unique(datS_GreaterThan97Y$Male)
			countryFemaleGS_GreaterThan97 = countryFemaleGS_GreaterThan97 + unique(datS_GreaterThan97Y$Female)
			sumM_F_GreaterThan97 <- sum(unique(datNY$Male), unique(datNY$Female))
			totalpercountry_GreaterThan97 <- c(totalpercountry_GreaterThan97,sumM_F_GreaterThan97)
			countryTotalGS_GreaterThan97 = countryTotalGS_GreaterThan97 + unique(datS_GreaterThan97Y$TotalParticipant)
        }
	}else{
	    datS_GreaterThan97Y <- datS_GreaterThan97[which(datS_GreaterThan97$NearestOlympicYear == uniqyr_GreaterThan97),]
		MaleParticipantsGS_GreaterThan97 <- c(MaleParticipantsGS_GreaterThan97, unique(datS_GreaterThan97Y$Male))
		FemaleParticipantsGS_GreaterThan97 <- c(FemaleParticipantsGS_GreaterThan97, unique(datS_GreaterThan97Y$Female)) 
		year <- c(year, uniqyr); 
		countryMaleGS_GreaterThan97 = countryMaleGS_GreaterThan97 + unique(datS_GreaterThan97Y$Male)
		countryFemaleGS_GreaterThan97 = countryFemaleGS_GreaterThan97 + unique(datS_GreaterThan97Y$Female)
		sumM_F_GreaterThan97 <- sum(unique(datNY$Male), unique(datNY$Female))
		totalpercountry_GreaterThan97 <- c(totalpercountry_GreaterThan97,sumM_F_GreaterThan97)
		countryTotalGS_GreaterThan97 = countryTotalGS_GreaterThan97 + unique(datS_GreaterThan97Y$TotalParticipant)
	}
}
percGS_GreaterThan97_Male <- ((countryMaleGS_GreaterThan97)/countryTotalGS_GreaterThan97)*100
percGS_GreaterThan97_Female <- ((countryFemaleGS_GreaterThan97)/countryTotalGS_GreaterThan97)*100

####Global North
datGlobN_lessThan24 <- datFinal[which(datFinal$GlobalClassification == "GN" 
    & datFinal$Sanction_months <= 24),]
countryMaleGN_24 = 0;countryFemaleGN_24 = 0;countryTotalGN_24 = 0
MaleParticipantsGN_24 <- c();FemaleParticipantsGN_24 <- c()
uniqCount_less24 <- unique(datGlobN_lessThan24$Nationality); 
year <- c();totalpercountry_24 <- c()
for(ii in 1:length(uniqCount_less24)){
    datN_24 <- datGlobN_lessThan24[which(datGlobN_lessThan24$Nationality == uniqCount_less24[ii]),]
	uniqyr_24 <- unique(datN_24$NearestOlympicYear)
	
	if(length(uniqyr_24) > 1){
	    for(jj in 1:length(uniqyr_24)){
		    datN_24Y <- datN_24[which(datN_24$NearestOlympicYear == uniqyr_24[jj]),]
			MaleParticipantsGN_24 <- c(MaleParticipantsGN_24, unique(datN_24Y$Male))
		    FemaleParticipantsGN_24 <- c(FemaleParticipantsGN_24, unique(datN_24Y$Female)) 
			year <- c(year, uniqyr); 
			countryMaleGN_24 = countryMaleGN_24 + unique(datN_24Y$Male)
			countryFemaleGN_24 = countryFemaleGN_24 + unique(datN_24Y$Female)
			sumM_F_24 <- sum(unique(datNY$Male), unique(datNY$Female))
			totalpercountry_24 <- c(totalpercountry_24,sumM_F_24)
			countryTotalGN_24 = countryTotalGN_24 + unique(datN_24Y$TotalParticipant)
        }
	}else{
	    datN_24Y <- datN_24[which(datN_24$NearestOlympicYear == uniqyr_24),]
		MaleParticipantsGN_24 <- c(MaleParticipantsGN_24, unique(datN_24Y$Male))
		FemaleParticipantsGN_24 <- c(FemaleParticipantsGN_24, unique(datN_24Y$Female)) 
		year <- c(year, uniqyr); 
		countryMaleGN_24 = countryMaleGN_24 + unique(datN_24Y$Male)
		countryFemaleGN_24 = countryFemaleGN_24 + unique(datN_24Y$Female)
		sumM_F_24 <- sum(unique(datNY$Male), unique(datNY$Female))
		totalpercountry_24 <- c(totalpercountry_24,sumM_F_24)
		countryTotalGN_24 = countryTotalGN_24 + unique(datN_24Y$TotalParticipant)
	}
}
percGN_24_Male <- ((countryMaleGN_24)/countryTotalGN_24)*100
percGN_24_Female <- ((countryFemaleGN_24)/countryTotalGN_24)*100
datGlobN_lessThan25_48 <- datFinal[which(datFinal$GlobalClassification == "GN" 
    & (datFinal$Sanction_months >= 25 & datFinal$Sanction_months <= 48)),]
countryMaleGN_25_48 = 0;countryFemaleGN_25_48 = 0;countryTotalGN_25_48 = 0
MaleParticipantsGN_25_48 <- c();FemaleParticipantsGN_25_48 <- c()
uniqCount_less24 <- unique(datGlobN_lessThan24$Nationality); 
year <- c();totalpercountry_25_48 <- c()
for(ii in 1:length(uniqCount_less24)){
    datN_25_48 <- datGlobN_lessThan24[which(datGlobN_lessThan24$Nationality == uniqCount_less24[ii]),]
	uniqyr_25_48 <- unique(datN_25_48$NearestOlympicYear)
	
	if(length(uniqyr_25_48) > 1){
	    for(jj in 1:length(uniqyr_25_48)){
		    datN_25_48Y <- datN_25_48[which(datN_25_48$NearestOlympicYear == uniqyr_25_48[jj]),]
			MaleParticipantsGN_25_48 <- c(MaleParticipantsGN_25_48, unique(datN_25_48Y$Male))
		    FemaleParticipantsGN_25_48 <- c(FemaleParticipantsGN_25_48, unique(datN_25_48Y$Female)) 
			year <- c(year, uniqyr); 
			countryMaleGN_25_48 = countryMaleGN_25_48 + unique(datN_25_48Y$Male)
			countryFemaleGN_25_48 = countryFemaleGN_25_48 + unique(datN_25_48Y$Female)
			sumM_F_25_48 <- sum(unique(datNY$Male), unique(datNY$Female))
			totalpercountry_25_48 <- c(totalpercountry_25_48,sumM_F_25_48)
			countryTotalGN_25_48 = countryTotalGN_25_48 + unique(datN_25_48Y$TotalParticipant)
        }
	}else{
	    datN_25_48Y <- datN_25_48[which(datN_25_48$NearestOlympicYear == uniqyr_25_48),]
		MaleParticipantsGN_25_48 <- c(MaleParticipantsGN_25_48, unique(datN_25_48Y$Male))
		FemaleParticipantsGN_25_48 <- c(FemaleParticipantsGN_25_48, unique(datN_25_48Y$Female)) 
		year <- c(year, uniqyr); 
		countryMaleGN_25_48 = countryMaleGN_25_48 + unique(datN_25_48Y$Male)
		countryFemaleGN_25_48 = countryFemaleGN_25_48 + unique(datN_25_48Y$Female)
		sumM_F_25_48 <- sum(unique(datNY$Male), unique(datNY$Female))
		totalpercountry_25_48 <- c(totalpercountry_25_48,sumM_F_25_48)
		countryTotalGN_25_48 = countryTotalGN_25_48 + unique(datN_25_48Y$TotalParticipant)
	}
}
percGN_25_48_Male <- ((countryMaleGN_25_48)/countryTotalGN_25_48)*100
percGN_25_48_Female <- ((countryFemaleGN_25_48)/countryTotalGN_25_48)*100
datGlobN_lessThan49_96 <- datFinal[which(datFinal$GlobalClassification == "GN" 
    & (datFinal$Sanction_months >= 49 & datFinal$Sanction_months <= 96)),]
countryMaleGN_49_96 = 0;countryFemaleGN_49_96 = 0;countryTotalGN_49_96 = 0
MaleParticipantsGN_49_96 <- c();FemaleParticipantsGN_49_96 <- c()
uniqCount_less24 <- unique(datGlobN_lessThan24$Nationality); 
year <- c();totalpercountry_49_96 <- c()
for(ii in 1:length(uniqCount_less24)){
    datN_49_96 <- datGlobN_lessThan24[which(datGlobN_lessThan24$Nationality == uniqCount_less24[ii]),]
	uniqyr_49_96 <- unique(datN_49_96$NearestOlympicYear)
	
	if(length(uniqyr_49_96) > 1){
	    for(jj in 1:length(uniqyr_49_96)){
		    datN_49_96Y <- datN_49_96[which(datN_49_96$NearestOlympicYear == uniqyr_49_96[jj]),]
			MaleParticipantsGN_49_96 <- c(MaleParticipantsGN_49_96, unique(datN_49_96Y$Male))
		    FemaleParticipantsGN_49_96 <- c(FemaleParticipantsGN_49_96, unique(datN_49_96Y$Female)) 
			year <- c(year, uniqyr); 
			countryMaleGN_49_96 = countryMaleGN_49_96 + unique(datN_49_96Y$Male)
			countryFemaleGN_49_96 = countryFemaleGN_49_96 + unique(datN_49_96Y$Female)
			sumM_F_49_96 <- sum(unique(datNY$Male), unique(datNY$Female))
			totalpercountry_49_96 <- c(totalpercountry_49_96,sumM_F_49_96)
			countryTotalGN_49_96 = countryTotalGN_49_96 + unique(datN_49_96Y$TotalParticipant)
        }
	}else{
	    datN_49_96Y <- datN_49_96[which(datN_49_96$NearestOlympicYear == uniqyr_49_96),]
		MaleParticipantsGN_49_96 <- c(MaleParticipantsGN_49_96, unique(datN_49_96Y$Male))
		FemaleParticipantsGN_49_96 <- c(FemaleParticipantsGN_49_96, unique(datN_49_96Y$Female)) 
		year <- c(year, uniqyr); 
		countryMaleGN_49_96 = countryMaleGN_49_96 + unique(datN_49_96Y$Male)
		countryFemaleGN_49_96 = countryFemaleGN_49_96 + unique(datN_49_96Y$Female)
		sumM_F_49_96 <- sum(unique(datNY$Male), unique(datNY$Female))
		totalpercountry_49_96 <- c(totalpercountry_49_96,sumM_F_49_96)
		countryTotalGN_49_96 = countryTotalGN_49_96 + unique(datN_49_96Y$TotalParticipant)
	}
}
percGN_49_96_Male <- ((countryMaleGN_49_96)/countryTotalGN_49_96)*100
percGN_49_96_Female <- ((countryFemaleGN_49_96)/countryTotalGN_49_96)*100
datGlobN_GreaterThan97 <- datFinal[which(datFinal$GlobalClassification == "GN" 
    & (datFinal$Sanction_months >= 97)),]
countryMaleGN_GreaterThan97 = 0;countryFemaleGN_GreaterThan97 = 0;countryTotalGN_GreaterThan97 = 0
MaleParticipantsGN_GreaterThan97 <- c();FemaleParticipantsGN_GreaterThan97 <- c()
uniqCount_less24 <- unique(datGlobN_lessThan24$Nationality); 
year <- c();totalpercountry_GreaterThan97 <- c()
for(ii in 1:length(uniqCount_less24)){
    datN_GreaterThan97 <- datGlobN_lessThan24[which(datGlobN_lessThan24$Nationality == uniqCount_less24[ii]),]
	uniqyr_GreaterThan97 <- unique(datN_GreaterThan97$NearestOlympicYear)
	
	if(length(uniqyr_GreaterThan97) > 1){
	    for(jj in 1:length(uniqyr_GreaterThan97)){
		    datN_GreaterThan97Y <- datN_GreaterThan97[which(datN_GreaterThan97$NearestOlympicYear == uniqyr_GreaterThan97[jj]),]
			MaleParticipantsGN_GreaterThan97 <- c(MaleParticipantsGN_GreaterThan97, unique(datN_GreaterThan97Y$Male))
		    FemaleParticipantsGN_GreaterThan97 <- c(FemaleParticipantsGN_GreaterThan97, unique(datN_GreaterThan97Y$Female)) 
			year <- c(year, uniqyr); 
			countryMaleGN_GreaterThan97 = countryMaleGN_GreaterThan97 + unique(datN_GreaterThan97Y$Male)
			countryFemaleGN_GreaterThan97 = countryFemaleGN_GreaterThan97 + unique(datN_GreaterThan97Y$Female)
			sumM_F_GreaterThan97 <- sum(unique(datNY$Male), unique(datNY$Female))
			totalpercountry_GreaterThan97 <- c(totalpercountry_GreaterThan97,sumM_F_GreaterThan97)
			countryTotalGN_GreaterThan97 = countryTotalGN_GreaterThan97 + unique(datN_GreaterThan97Y$TotalParticipant)
        }
	}else{
	    datN_GreaterThan97Y <- datN_GreaterThan97[which(datN_GreaterThan97$NearestOlympicYear == uniqyr_GreaterThan97),]
		MaleParticipantsGN_GreaterThan97 <- c(MaleParticipantsGN_GreaterThan97, unique(datN_GreaterThan97Y$Male))
		FemaleParticipantsGN_GreaterThan97 <- c(FemaleParticipantsGN_GreaterThan97, unique(datN_GreaterThan97Y$Female)) 
		year <- c(year, uniqyr); 
		countryMaleGN_GreaterThan97 = countryMaleGN_GreaterThan97 + unique(datN_GreaterThan97Y$Male)
		countryFemaleGN_GreaterThan97 = countryFemaleGN_GreaterThan97 + unique(datN_GreaterThan97Y$Female)
		sumM_F_GreaterThan97 <- sum(unique(datNY$Male), unique(datNY$Female))
		totalpercountry_GreaterThan97 <- c(totalpercountry_GreaterThan97,sumM_F_GreaterThan97)
		countryTotalGN_GreaterThan97 = countryTotalGN_GreaterThan97 + unique(datN_GreaterThan97Y$TotalParticipant)
	}
}
percGN_GreaterThan97_Male <- ((countryMaleGN_GreaterThan97)/countryTotalGN_GreaterThan97)*100
percGN_GreaterThan97_Female <- ((countryFemaleGN_GreaterThan97)/countryTotalGN_GreaterThan97)*100


datgn_gs <- data.frame(SanctionPeriod = c(
            "<=24mnths",
			"<=24mnths",
			"<=24mnths",
			"<=24mnths",
	        "25_48mnths", 
			"25_48mnths",
			"25_48mnths", 
			"25_48mnths",
	        "49_96mnths",
			"49_96mnths",
			"49_96mnths",
			"49_96mnths",
			"greaterthan97mnths",
			"greaterthan97mnths",
			"greaterthan97mnths",
			"greaterthan97mnths"),
        NumberofAthletes = c(length(MaleParticipantsGN_24),
            length(FemaleParticipantsGN_24),
            length(MaleParticipantsGS_24),
            length(FemaleParticipantsGS_24),			
	        length(MaleParticipantsGN_25_48),
            length(FemaleParticipantsGN_25_48),
            length(MaleParticipantsGS_25_48),
            length(FemaleParticipantsGS_25_48),
		    length(MaleParticipantsGN_49_96),
            length(FemaleParticipantsGN_49_96),
            length(MaleParticipantsGS_49_96),
            length(FemaleParticipantsGS_49_96),
			length(MaleParticipantsGN_GreaterThan97),
            length(FemaleParticipantsGN_GreaterThan97),
            length(MaleParticipantsGS_GreaterThan97),
            length(FemaleParticipantsGS_GreaterThan97)),
	    TotalAthletes = c(countryTotalGN_24, 
		    countryTotalGN_24,
		    countryTotalGS_24,
			countryTotalGS_24,
			countryTotalGN_25_48, 
		    countryTotalGN_25_48,
		    countryTotalGS_25_48,
			countryTotalGS_25_48,
			countryTotalGN_49_96, 
		    countryTotalGN_49_96,
		    countryTotalGS_49_96,
			countryTotalGS_49_96,
			countryTotalGN_GreaterThan97, 
		    countryTotalGN_GreaterThan97,
		    countryTotalGS_GreaterThan97,
			countryTotalGS_GreaterThan97),
		Sex =c("Male", "Female", "Male", "Female", "Male", "Female", "Male", "Female", "Male", "Female", "Male", "Female", "Male", "Female", "Male", "Female"), 
        SanctionPercent = c(percGN_24_Male,
		    percGN_24_Female,
     		percGS_24_Male,
		    percGS_24_Female,
			percGN_25_48_Male,
		    percGN_25_48_Female,
     		percGS_25_48_Male,
		    percGS_25_48_Female,
			percGN_49_96_Male,
		    percGN_49_96_Female,
     		percGS_49_96_Male,
		    percGS_49_96_Female,
			percGN_GreaterThan97_Male,
		    percGN_GreaterThan97_Female,
     		percGS_GreaterThan97_Male,
		    percGS_GreaterThan97_Female),
		GlobalClassification = c("GN",
		    "GN",
			"GS",
		    "GS",
     		"GN",
		    "GN",
			"GS",
		    "GS",
			"GN",
		    "GN",
			"GS",
		    "GS",
			"GN",
		    "GN",
			"GS",
		    "GS"))
write.csv(datgn_gs, file.path(outdir,"GlobalClassifications_Male_Female_GN_GN.csv"), row.names = FALSE)
 p8 <- ggplot(data = datgn_gs, aes(x = factor(GlobalClassification, level =c("GN", "GS")), y= SanctionPercent, fill = factor(Sex, level = c("Male","Female")))) + geom_bar(stat = "identity", position="stack")+ labs(x = "SanctionPeriod", y = "% of Sanctioned Athletes", fill = "Global Classifications") + theme(legend.title = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.title.x = element_text(face = "bold", size = 50),axis.title.y = element_text(face = "bold", size = 50), axis.text.x = element_text(face="bold", size = 30, angle = 45, vjust = 0.5), axis.text.y = element_text(face="bold", size = 30), legend.text=element_text(face = "bold",size=25),  strip.text = element_text(size = 25))  +  facet_wrap( ~ factor(SanctionPeriod, level = c("<=24mnths","25_48mnths", "49_96mnths", "greaterthan97mnths")))  + scale_fill_manual(values=c("green","pink"))
p8

#HumanIndex
outdir = "Z:/Suro/AtheleteSanction/AtheleteSanction05262021/"
datFinal <- read.csv("Z:/Suro/AtheleteSanction/AtheleteSanction05262021/AthleteInfo_12282021.csv")
r <-read.csv("Z:/Suro/AtheleteSanction/AtheleteSanction05262021/Athlete_Countries_10-27-2021.csv")
HumanIndxBetween0.35to0.549 <- datFinal[which(datFinal$HumanIndex >= 0.35 & datFinal$HumanIndex <= 0.549),]
nrow(HumanIndxBetween0.35to0.549)
countryTotal_0.35to0.549 = 0
uniqCount <- unique(HumanIndxBetween0.35to0.549$Nationality); 
country <- c(); year <- c(); Participants <- c()
for(ii in 1:length(uniqCount)){
    dat_HIBetween0.35to0.549 <- HumanIndxBetween0.35to0.549[which(HumanIndxBetween0.35to0.549$Nationality == uniqCount[ii]),]
	uniqyr <- unique(dat_HIBetween0.35to0.549$NearestOlympicYear)
	dat_HIBetween0.35to0.549Y <- r[which(r$NationAbbr == uniqCount[ii]),]
	countryTotal_0.35to0.549 = countryTotal_0.35to0.549 + unique(dat_HIBetween0.35to0.549Y$Country_Total)
	print(uniqCount[ii])
	print(countryTotal_0.35to0.549)
	
	'if(length(uniqyr) > 1){
	    for(jj in 1:length(uniqyr)){
		    dat_HIBetween0.35to0.549Y <- dat_HIBetween0.35to0.549[which(dat_HIBetween0.35to0.549$NearestOlympicYear == uniqyr[jj]),]
			country <- c(country, uniqCount[ii]); 
			year <- c(year, uniqyr[jj]); 
			Participants <- c(Participants, unique(dat_HIBetween0.35to0.549Y$TotalParticipant))
		    countryTotal_0.35to0.549 = countryTotal_0.35to0.549 + unique(dat_HIBetween0.35to0.549Y$TotalParticipant)
        }
	}else{
	    dat_HIBetween0.35to0.549Y <- dat_HIBetween0.35to0.549[which(dat_HIBetween0.35to0.549$NearestOlympicYear == uniqyr),]
		country <- c(country, uniqCount[ii]); 
		year <- c(year, uniqyr); 
		Participants <- c(Participants, unique(dat_HIBetween0.35to0.549Y$TotalParticipant))
		countryTotal_0.35to0.549 = countryTotal_0.35to0.549 + unique(dat_HIBetween0.35to0.549Y$TotalParticipant)
	}'
}
percHI_0.35to0.549 = (nrow(HumanIndxBetween0.35to0.549)/countryTotal_0.35to0.549)*100
percHI_0.35to0.549_TotSanctionedPercent = (nrow(HumanIndxBetween0.35to0.549)/nrow(datFinal))*100
HumanIndxBetween0.55to0.699 <- datFinal[which(datFinal$HumanIndex >= 0.55 & datFinal$HumanIndex <= 0.699),]
nrow(HumanIndxBetween0.55to0.699)
countryTotal_0.55to0.699 = 0
uniqCount <- unique(HumanIndxBetween0.55to0.699$Nationality); 
country <- c(); year <- c(); Participants <- c()
for(ii in 1:length(uniqCount)){
    dat_HIBetween0.55to0.699 <- HumanIndxBetween0.55to0.699[which(HumanIndxBetween0.55to0.699$Nationality == uniqCount[ii]),]
	uniqyr <- unique(dat_HIBetween0.55to0.699$NearestOlympicYear)
	dat_HIBetween0.55to0.699Y <- r[which(r$NationAbbr == uniqCount[ii]),]
	countryTotal_0.55to0.699 = countryTotal_0.55to0.699 + unique(dat_HIBetween0.55to0.699Y$Country_Total)
	print(uniqCount[ii])
	print(countryTotal_0.55to0.699)
	'if(length(uniqyr) > 1){
	    for(jj in 1:length(uniqyr)){
		    dat_HIBetween0.55to0.699Y <- dat_HIBetween0.55to0.699[which(dat_HIBetween0.55to0.699$NearestOlympicYear == uniqyr[jj]),]
			country <- c(country, uniqCount[ii]); 
			year <- c(year, uniqyr[jj]); 
			Participants <- c(Participants, unique(dat_HIBetween0.55to0.699Y$TotalParticipant))
		    countryTotal_0.55to0.699 = countryTotal_0.55to0.699 + unique(dat_HIBetween0.55to0.699Y$TotalParticipant)
        }
	}else{
	    dat_HIBetween0.55to0.699Y <- dat_HIBetween0.55to0.699[which(dat_HIBetween0.55to0.699$NearestOlympicYear == uniqyr),]
		country <- c(country, uniqCount[ii]); 
		year <- c(year, uniqyr); 
		Participants <- c(Participants, unique(dat_HIBetween0.55to0.699Y$TotalParticipant))
		countryTotal_0.55to0.699 = countryTotal_0.55to0.699 + unique(dat_HIBetween0.55to0.699Y$TotalParticipant)
	}'
}
percHI_0.55to0.699_TotSanctionedPercent = (nrow(HumanIndxBetween0.55to0.699)/nrow(datFinal))*100
percHI_0.55to0.699 = (nrow(HumanIndxBetween0.55to0.699)/countryTotal_0.55to0.699)*100
HumanIndxBetween0.7to0.799 <- datFinal[which(datFinal$HumanIndex >= 0.7 & datFinal$HumanIndex <= 0.799),]
nrow(HumanIndxBetween0.7to0.799)
countryTotal_0.7to0.799 = 0
uniqCount <- unique(HumanIndxBetween0.7to0.799$Nationality); 
country <- c(); year <- c(); Participants <- c()
for(ii in 1:length(uniqCount)){
    dat_HIBetween0.7to0.799 <- HumanIndxBetween0.7to0.799[which(HumanIndxBetween0.7to0.799$Nationality == uniqCount[ii]),]
	uniqyr <- unique(dat_HIBetween0.7to0.799$NearestOlympicYear)
	dat_HIBetween0.7to0.799Y <- r[which(r$NationAbbr == uniqCount[ii]),]
	countryTotal_0.7to0.799 = countryTotal_0.7to0.799 + unique(dat_HIBetween0.7to0.799Y$Country_Total)
	print(uniqCount[ii])
	print(countryTotal_0.7to0.799)
	'if(length(uniqyr) > 1){
	    for(jj in 1:length(uniqyr)){
		    dat_HIBetween0.7to0.799Y <- dat_HIBetween0.7to0.799[which(dat_HIBetween0.7to0.799$NearestOlympicYear == uniqyr[jj]),]
			country <- c(country, uniqCount[ii]); 
			year <- c(year, uniqyr[jj]); 
			Participants <- c(Participants, unique(dat_HIBetween0.7to0.799Y$TotalParticipant))
		    countryTotal_0.7to0.799 = countryTotal_0.7to0.799 + unique(dat_HIBetween0.7to0.799Y$TotalParticipant)
        }
	}else{
	    dat_HIBetween0.7to0.799Y <- dat_HIBetween0.7to0.799[which(dat_HIBetween0.7to0.799$NearestOlympicYear == uniqyr),]
		country <- c(country, uniqCount[ii]); 
		year <- c(year, uniqyr); 
		Participants <- c(Participants, unique(dat_HIBetween0.7to0.799Y$TotalParticipant))
		countryTotal_0.7to0.799 = countryTotal_0.7to0.799 + unique(dat_HIBetween0.7to0.799Y$TotalParticipant)
	}'
}
percHI_0.7to0.799_TotSanctionedPercent = (nrow(HumanIndxBetween0.7to0.799)/nrow(datFinal))*100
percHI_0.7to0.799 = (nrow(HumanIndxBetween0.7to0.799)/countryTotal_0.7to0.799)*100
HumanIndxBetween0.8to1 <- datFinal[which(datFinal$HumanIndex >= 0.8 & datFinal$HumanIndex <= 1),]
nrow(HumanIndxBetween0.8to1)
countryTotal_0.8to1 = 0
uniqCount <- unique(HumanIndxBetween0.8to1$Nationality); 
country <- c(); year <- c(); Participants <- c()
for(ii in 1:length(uniqCount)){
    dat_HIBetween0.8to1 <- HumanIndxBetween0.8to1[which(HumanIndxBetween0.8to1$Nationality == uniqCount[ii]),]
	uniqyr <- unique(dat_HIBetween0.8to1$NearestOlympicYear)
	dat_HIBetween0.8to1Y <- r[which(r$NationAbbr == uniqCount[ii]),]
	countryTotal_0.8to1 = countryTotal_0.8to1 + unique(dat_HIBetween0.8to1Y$Country_Total)
	print(uniqCount[ii])
	print(countryTotal_0.8to1)
	'if(length(uniqyr) > 1){
	    for(jj in 1:length(uniqyr)){
		    dat_HIBetween0.8to1Y <- dat_HIBetween0.8to1[which(dat_HIBetween0.8to1$NearestOlympicYear == uniqyr[jj]),]
			country <- c(country, uniqCount[ii]); 
			year <- c(year, uniqyr[jj]); 
			Participants <- c(Participants, unique(dat_HIBetween0.8to1Y$TotalParticipant))
		    countryTotal_0.8to1 = countryTotal_0.8to1 + unique(dat_HIBetween0.8to1Y$TotalParticipant)
        }
	}else{
	    dat_HIBetween0.8to1Y <- dat_HIBetween0.8to1[which(dat_HIBetween0.8to1$NearestOlympicYear == uniqyr),]
		country <- c(country, uniqCount[ii]); 
		year <- c(year, uniqyr); 
		Participants <- c(Participants, unique(dat_HIBetween0.8to1Y$TotalParticipant))
		countryTotal_0.8to1 = countryTotal_0.8to1 + unique(dat_HIBetween0.8to1Y$TotalParticipant)
	}'
}
percHI_0.8to1_TotSanctionedPercent = (nrow(HumanIndxBetween0.8to1)/nrow(datFinal))*100
percHI_0.8to1 = (nrow(HumanIndxBetween0.8to1)/countryTotal_0.8to1)*100
datHI<- data.frame(HumanIndex = c(
            "HI_0.35to0.549",
			"HI_0.55to0.699", 
			"HI_0.7to0.799",
			"HI_0.8to1"),
        NumberofAthletes = c(
	        nrow(HumanIndxBetween0.35to0.549),
            nrow(HumanIndxBetween0.55to0.699),			
	        nrow(HumanIndxBetween0.7to0.799),
			nrow(HumanIndxBetween0.8to1)),
	    TotalAthletesFrom_HIPercentage = c(countryTotal_0.35to0.549,
		    countryTotal_0.55to0.699,
			countryTotal_0.7to0.799,
			countryTotal_0.8to1),
		SanctionPercent_HI = c(percHI_0.35to0.549,
		    percHI_0.55to0.699,
			percHI_0.7to0.799,
			percHI_0.8to1),
		SanctionPercent_TotalSanctioned = c(percHI_0.35to0.549_TotSanctionedPercent,
		percHI_0.55to0.699_TotSanctionedPercent,
		percHI_0.7to0.799_TotSanctionedPercent,
		percHI_0.8to1_TotSanctionedPercent))
library(ggplot2)
p10 <- ggplot(data = datHI, aes(x = factor(HumanIndex, levels=unique(HumanIndex)), y= SanctionPercent_HI, fill = factor(HumanIndex, levels=unique(HumanIndex)))) + geom_bar(stat = "identity")+ labs(x = "SanctionPeriod", y = "% of Sanctioned Athletes") + theme(legend.title = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.title.x = element_text(face = "bold", size = 50),axis.title.y = element_text(face = "bold", size = 50), axis.text.x = element_text(face="bold", size = 30, angle = 45, hjust = 1), axis.text.y = element_text(face="bold", size = 30), legend.text=element_text(face = "bold",size=25)) + scale_fill_manual(values=c("green","red", "blue", "yellow")) 
p10

#HumanIndex
###0.35_0.549
outdir = "Z:/Suro/AtheleteSanction/AtheleteSanction05262021/"
datFinal <- read.csv("Z:/Suro/AtheleteSanction/AtheleteSanction05262021/AthleteInfo_12282021.csv")
r <-read.csv("Z:/Suro/AtheleteSanction/AtheleteSanction05262021/Athlete_Countries_10-27-2021.csv")
HumanIndxBetween0.35to0.549_LessThan24mnths <- datFinal[which((datFinal$HumanIndex >= 0.35 
    & datFinal$HumanIndex <= 0.549)
	& datFinal$Sanction_months <= 24),]
nrow(HumanIndxBetween0.35to0.549_LessThan24mnths)
countryTotal_0.35to0.549_LessThan24mnths = 0
uniqCount <- unique(HumanIndxBetween0.35to0.549_LessThan24mnths$Nationality); 
country <- c(); year <- c(); Participants <- c()
for(ii in 1:length(uniqCount)){
    dat_HIBetween0.35to0.549_LessThan24mnths <- HumanIndxBetween0.35to0.549_LessThan24mnths[which(HumanIndxBetween0.35to0.549_LessThan24mnths$Nationality == uniqCount[ii]),]
	uniqyr <- unique(dat_HIBetween0.35to0.549_LessThan24mnths$NearestOlympicYear)
	dat_HIBetween0.35to0.549_LessThan24mnthsY <- r[which(r$NationAbbr == uniqCount[ii]),]
	countryTotal_0.35to0.549_LessThan24mnths = countryTotal_0.35to0.549_LessThan24mnths + unique(dat_HIBetween0.35to0.549_LessThan24mnthsY$Country_Total)
	'if(length(uniqyr) > 1){
	    for(jj in 1:length(uniqyr)){
		    dat_HIBetween0.35to0.549_LessThan24mnthsY <- dat_HIBetween0.35to0.549_LessThan24mnths[which(dat_HIBetween0.35to0.549_LessThan24mnths$NearestOlympicYear == uniqyr[jj]),]
			country <- c(country, uniqCount[ii]); 
			year <- c(year, uniqyr[jj]); 
			Participants <- c(Participants, unique(dat_HIBetween0.35to0.549_LessThan24mnthsY$TotalParticipant))
		    countryTotal_0.35to0.549_LessThan24mnths = countryTotal_0.35to0.549_LessThan24mnths + unique(dat_HIBetween0.35to0.549_LessThan24mnthsY$TotalParticipant)
        }
	}else{
	    dat_HIBetween0.35to0.549_LessThan24mnthsY <- dat_HIBetween0.35to0.549_LessThan24mnths[which(dat_HIBetween0.35to0.549_LessThan24mnths$NearestOlympicYear == uniqyr),]
		country <- c(country, uniqCount[ii]); 
		year <- c(year, uniqyr); 
		Participants <- c(Participants, unique(dat_HIBetween0.35to0.549_LessThan24mnthsY$TotalParticipant))
		countryTotal_0.35to0.549_LessThan24mnths = countryTotal_0.35to0.549_LessThan24mnths + unique(dat_HIBetween0.35to0.549_LessThan24mnthsY$TotalParticipant)
	}'
}
percHI_0.35to0.549_LessThan24mnths = (nrow(HumanIndxBetween0.35to0.549_LessThan24mnths)/countryTotal_0.35to0.549_LessThan24mnths)*100

HumanIndxBetween0.35to0.549_25_48mnths <- datFinal[which((datFinal$HumanIndex >= 0.35 
    & datFinal$HumanIndex <= 0.549)
	& (datFinal$Sanction_months >= 25 
	& datFinal$Sanction_months <= 48)),]
nrow(HumanIndxBetween0.35to0.549_25_48mnths)
countryTotal_0.35to0.549_25_48mnths = 0
uniqCount <- unique(HumanIndxBetween0.35to0.549_25_48mnths$Nationality); 
country <- c(); year <- c(); Participants <- c()
for(ii in 1:length(uniqCount)){
    dat_HIBetween0.35to0.549_25_48mnths <- HumanIndxBetween0.35to0.549_25_48mnths[which(HumanIndxBetween0.35to0.549_25_48mnths$Nationality == uniqCount[ii]),]
	uniqyr <- unique(dat_HIBetween0.35to0.549_25_48mnths$NearestOlympicYear)
	dat_HIBetween0.35to0.549_25_48mnthsY <- r[which(r$NationAbbr == uniqCount[ii]),]
	countryTotal_0.35to0.549_25_48mnths = countryTotal_0.35to0.549_25_48mnths + unique(dat_HIBetween0.35to0.549_25_48mnthsY$Country_Total)
	'if(length(uniqyr) > 1){
	    for(jj in 1:length(uniqyr)){
		    dat_HIBetween0.35to0.549_25_48mnthsY <- dat_HIBetween0.35to0.549_25_48mnths[which(dat_HIBetween0.35to0.549_25_48mnths$NearestOlympicYear == uniqyr[jj]),]
			country <- c(country, uniqCount[ii]); 
			year <- c(year, uniqyr[jj]); 
			Participants <- c(Participants, unique(dat_HIBetween0.35to0.549_25_48mnthsY$TotalParticipant))
		    countryTotal_0.35to0.549_25_48mnths = countryTotal_0.35to0.549_25_48mnths + unique(dat_HIBetween0.35to0.549_25_48mnthsY$TotalParticipant)
        }
	}else{
	    dat_HIBetween0.35to0.549_25_48mnthsY <- dat_HIBetween0.35to0.549_25_48mnths[which(dat_HIBetween0.35to0.549_25_48mnths$NearestOlympicYear == uniqyr),]
		country <- c(country, uniqCount[ii]); 
		year <- c(year, uniqyr); 
		Participants <- c(Participants, unique(dat_HIBetween0.35to0.549_25_48mnthsY$TotalParticipant))
		countryTotal_0.35to0.549_25_48mnths = countryTotal_0.35to0.549_25_48mnths + unique(dat_HIBetween0.35to0.549_25_48mnthsY$TotalParticipant)
	}'
}
percHI_0.35to0.549_25_48mnths = (nrow(HumanIndxBetween0.35to0.549_25_48mnths)/countryTotal_0.35to0.549_25_48mnths)*100


HumanIndxBetween0.35to0.549_49_96mnths <- datFinal[which((datFinal$HumanIndex >= 0.35 
    & datFinal$HumanIndex <= 0.549)
	& (datFinal$Sanction_months >= 49 
	& datFinal$Sanction_months <= 96)),]
nrow(HumanIndxBetween0.35to0.549_49_96mnths)
countryTotal_0.35to0.549_49_96mnths = 0
uniqCount <- unique(HumanIndxBetween0.35to0.549_49_96mnths$Nationality); 
country <- c(); year <- c(); Participants <- c()
for(ii in 1:length(uniqCount)){
    dat_HIBetween0.35to0.549_49_96mnths <- HumanIndxBetween0.35to0.549_49_96mnths[which(HumanIndxBetween0.35to0.549_49_96mnths$Nationality == uniqCount[ii]),]
	uniqyr <- unique(dat_HIBetween0.35to0.549_49_96mnths$NearestOlympicYear)
	dat_HIBetween0.35to0.549_49_96mnthsY <- r[which(r$NationAbbr == uniqCount[ii]),]
	countryTotal_0.35to0.549_49_96mnths = countryTotal_0.35to0.549_49_96mnths + unique(dat_HIBetween0.35to0.549_49_96mnthsY$Country_Total)
	'if(length(uniqyr) > 1){
	    for(jj in 1:length(uniqyr)){
		    dat_HIBetween0.35to0.549_49_96mnthsY <- dat_HIBetween0.35to0.549_49_96mnths[which(dat_HIBetween0.35to0.549_49_96mnths$NearestOlympicYear == uniqyr[jj]),]
			country <- c(country, uniqCount[ii]); 
			year <- c(year, uniqyr[jj]); 
			Participants <- c(Participants, unique(dat_HIBetween0.35to0.549_49_96mnthsY$TotalParticipant))
		    countryTotal_0.35to0.549_49_96mnths = countryTotal_0.35to0.549_49_96mnths + unique(dat_HIBetween0.35to0.549_49_96mnthsY$TotalParticipant)
        }
	}else{
	    dat_HIBetween0.35to0.549_49_96mnthsY <- dat_HIBetween0.35to0.549_49_96mnths[which(dat_HIBetween0.35to0.549_49_96mnths$NearestOlympicYear == uniqyr),]
		country <- c(country, uniqCount[ii]); 
		year <- c(year, uniqyr); 
		Participants <- c(Participants, unique(dat_HIBetween0.35to0.549_49_96mnthsY$TotalParticipant))
		countryTotal_0.35to0.549_49_96mnths = countryTotal_0.35to0.549_49_96mnths + unique(dat_HIBetween0.35to0.549_49_96mnthsY$TotalParticipant)
	}'
}
percHI_0.35to0.549_49_96mnths = (nrow(HumanIndxBetween0.35to0.549_49_96mnths)/countryTotal_0.35to0.549_49_96mnths)*100

HumanIndxBetween0.35to0.549_GreaterThan97 <- datFinal[which((datFinal$HumanIndex >= 0.35 
    & datFinal$HumanIndex <= 0.549)
	& (datFinal$Sanction_months >= 97)),]
nrow(HumanIndxBetween0.35to0.549_GreaterThan97)
countryTotal_0.35to0.549_GreaterThan97 = 0
uniqCount <- unique(HumanIndxBetween0.35to0.549_GreaterThan97$Nationality); 
country <- c(); year <- c(); Participants <- c()
for(ii in 1:length(uniqCount)){
    dat_HIBetween0.35to0.549_GreaterThan97 <- HumanIndxBetween0.35to0.549_GreaterThan97[which(HumanIndxBetween0.35to0.549_GreaterThan97$Nationality == uniqCount[ii]),]
	uniqyr <- unique(dat_HIBetween0.35to0.549_GreaterThan97$NearestOlympicYear)
	dat_HIBetween0.35to0.549_GreaterThan97Y <- r[which(r$NationAbbr == uniqCount[ii]),]
	countryTotal_0.35to0.549_GreaterThan97 = countryTotal_0.35to0.549_GreaterThan97 + unique(dat_HIBetween0.35to0.549_GreaterThan97Y$Country_Total)
	'if(length(uniqyr) > 1){
	    for(jj in 1:length(uniqyr)){
		    dat_HIBetween0.35to0.549_GreaterThan97Y <- dat_HIBetween0.35to0.549_GreaterThan97[which(dat_HIBetween0.35to0.549_GreaterThan97$NearestOlympicYear == uniqyr[jj]),]
			country <- c(country, uniqCount[ii]); 
			year <- c(year, uniqyr[jj]); 
			Participants <- c(Participants, unique(dat_HIBetween0.35to0.549_GreaterThan97Y$TotalParticipant))
		    countryTotal_0.35to0.549_GreaterThan97 = countryTotal_0.35to0.549_GreaterThan97 + unique(dat_HIBetween0.35to0.549_GreaterThan97Y$TotalParticipant)
        }
	}else{
	    dat_HIBetween0.35to0.549_GreaterThan97Y <- dat_HIBetween0.35to0.549_GreaterThan97[which(dat_HIBetween0.35to0.549_GreaterThan97$NearestOlympicYear == uniqyr),]
		country <- c(country, uniqCount[ii]); 
		year <- c(year, uniqyr); 
		Participants <- c(Participants, unique(dat_HIBetween0.35to0.549_GreaterThan97Y$TotalParticipant))
		countryTotal_0.35to0.549_GreaterThan97 = countryTotal_0.35to0.549_GreaterThan97 + unique(dat_HIBetween0.35to0.549_GreaterThan97Y$TotalParticipant)
	}'
}
percHI_0.35to0.549_GreaterThan97 = (nrow(HumanIndxBetween0.35to0.549_GreaterThan97)/countryTotal_0.35to0.549_GreaterThan97)*100

#HumanIndxBetween0.55to0.699 <- datFinal[which(datFinal$HumanIndex >= 0.55 & datFinal$HumanIndex <= 0.699),]
#nrow(HumanIndxBetween0.55to0.699)
#0.55to0.699
HumanIndxBetween0.55to0.699_LessThan24mnths <- datFinal[which((datFinal$HumanIndex >= 0.55 & datFinal$HumanIndex <= 0.699)
	& datFinal$Sanction_months <= 24),]
nrow(HumanIndxBetween0.55to0.699_LessThan24mnths)
countryTotal_0.55to0.699_LessThan24mnths = 0
uniqCount <- unique(HumanIndxBetween0.55to0.699_LessThan24mnths$Nationality); 
country <- c(); year <- c(); Participants <- c()
for(ii in 1:length(uniqCount)){
    dat_HIBetween0.55to0.699_LessThan24mnths <- HumanIndxBetween0.55to0.699_LessThan24mnths[which(HumanIndxBetween0.55to0.699_LessThan24mnths$Nationality == uniqCount[ii]),]
	uniqyr <- unique(dat_HIBetween0.55to0.699_LessThan24mnths$NearestOlympicYear)
	dat_HIBetween0.55to0.699_LessThan24mnthsY <- r[which(r$NationAbbr == uniqCount[ii]),]
	countryTotal_0.55to0.699_LessThan24mnths = countryTotal_0.55to0.699_LessThan24mnths + unique(dat_HIBetween0.55to0.699_LessThan24mnthsY$Country_Total)
	
	'if(length(uniqyr) > 1){
	    for(jj in 1:length(uniqyr)){
		    dat_HIBetween0.55to0.699_LessThan24mnthsY <- dat_HIBetween0.55to0.699_LessThan24mnths[which(dat_HIBetween0.55to0.699_LessThan24mnths$NearestOlympicYear == uniqyr[jj]),]
			country <- c(country, uniqCount[ii]); 
			year <- c(year, uniqyr[jj]); 
			Participants <- c(Participants, unique(dat_HIBetween0.55to0.699_LessThan24mnthsY$TotalParticipant))
		    countryTotal_0.55to0.699_LessThan24mnths = countryTotal_0.55to0.699_LessThan24mnths + unique(dat_HIBetween0.55to0.699_LessThan24mnthsY$TotalParticipant)
        }
	}else{
	    dat_HIBetween0.55to0.699_LessThan24mnthsY <- dat_HIBetween0.55to0.699_LessThan24mnths[which(dat_HIBetween0.55to0.699_LessThan24mnths$NearestOlympicYear == uniqyr),]
		country <- c(country, uniqCount[ii]); 
		year <- c(year, uniqyr); 
		Participants <- c(Participants, unique(dat_HIBetween0.55to0.699_LessThan24mnthsY$TotalParticipant))
		countryTotal_0.55to0.699_LessThan24mnths = countryTotal_0.55to0.699_LessThan24mnths + unique(dat_HIBetween0.55to0.699_LessThan24mnthsY$TotalParticipant)
	}'
}
percHI_0.55to0.699_LessThan24mnths = (nrow(HumanIndxBetween0.55to0.699_LessThan24mnths)/countryTotal_0.55to0.699_LessThan24mnths)*100

HumanIndxBetween0.55to0.699_25_48mnths <- datFinal[which((datFinal$HumanIndex >= 0.55 & datFinal$HumanIndex <= 0.699)
	& (datFinal$Sanction_months >= 25 
	& datFinal$Sanction_months <= 48)),]
nrow(HumanIndxBetween0.55to0.699_25_48mnths)
countryTotal_0.55to0.699_25_48mnths = 0
uniqCount <- unique(HumanIndxBetween0.55to0.699_25_48mnths$Nationality); 
country <- c(); year <- c(); Participants <- c()
for(ii in 1:length(uniqCount)){
    dat_HIBetween0.55to0.699_25_48mnths <- HumanIndxBetween0.55to0.699_25_48mnths[which(HumanIndxBetween0.55to0.699_25_48mnths$Nationality == uniqCount[ii]),]
	uniqyr <- unique(dat_HIBetween0.55to0.699_25_48mnths$NearestOlympicYear)
	dat_HIBetween0.55to0.699_25_48mnthsY <- r[which(r$NationAbbr == uniqCount[ii]),]
	countryTotal_0.55to0.699_25_48mnths = countryTotal_0.55to0.699_25_48mnths + unique(dat_HIBetween0.55to0.699_25_48mnthsY$Country_Total)
	'if(length(uniqyr) > 1){
	    for(jj in 1:length(uniqyr)){
		    dat_HIBetween0.55to0.699_25_48mnthsY <- dat_HIBetween0.55to0.699_25_48mnths[which(dat_HIBetween0.55to0.699_25_48mnths$NearestOlympicYear == uniqyr[jj]),]
			country <- c(country, uniqCount[ii]); 
			year <- c(year, uniqyr[jj]); 
			Participants <- c(Participants, unique(dat_HIBetween0.55to0.699_25_48mnthsY$TotalParticipant))
		    countryTotal_0.55to0.699_25_48mnths = countryTotal_0.55to0.699_25_48mnths + unique(dat_HIBetween0.55to0.699_25_48mnthsY$TotalParticipant)
        }
	}else{
	    dat_HIBetween0.55to0.699_25_48mnthsY <- dat_HIBetween0.55to0.699_25_48mnths[which(dat_HIBetween0.55to0.699_25_48mnths$NearestOlympicYear == uniqyr),]
		country <- c(country, uniqCount[ii]); 
		year <- c(year, uniqyr); 
		Participants <- c(Participants, unique(dat_HIBetween0.55to0.699_25_48mnthsY$TotalParticipant))
		countryTotal_0.55to0.699_25_48mnths = countryTotal_0.55to0.699_25_48mnths + unique(dat_HIBetween0.55to0.699_25_48mnthsY$TotalParticipant)
	}'
	
}
percHI_0.55to0.699_25_48mnths = (nrow(HumanIndxBetween0.55to0.699_25_48mnths)/countryTotal_0.55to0.699_25_48mnths)*100


HumanIndxBetween0.55to0.699_49_96mnths <- datFinal[which((datFinal$HumanIndex >= 0.55 & datFinal$HumanIndex <= 0.699)
	& (datFinal$Sanction_months >= 49 
	& datFinal$Sanction_months <= 96)),]
nrow(HumanIndxBetween0.55to0.699_49_96mnths)
countryTotal_0.55to0.699_49_96mnths = 0
uniqCount <- unique(HumanIndxBetween0.55to0.699_49_96mnths$Nationality); 
country <- c(); year <- c(); Participants <- c()
for(ii in 1:length(uniqCount)){
    dat_HIBetween0.55to0.699_49_96mnths <- HumanIndxBetween0.55to0.699_49_96mnths[which(HumanIndxBetween0.55to0.699_49_96mnths$Nationality == uniqCount[ii]),]
	uniqyr <- unique(dat_HIBetween0.55to0.699_49_96mnths$NearestOlympicYear)
	dat_HIBetween0.55to0.699_49_96mnthsY <- r[which(r$NationAbbr == uniqCount[ii]),]
	countryTotal_0.55to0.699_49_96mnths = countryTotal_0.55to0.699_49_96mnths + unique(dat_HIBetween0.55to0.699_49_96mnthsY$Country_Total)
	'if(length(uniqyr) > 1){
	    for(jj in 1:length(uniqyr)){
		    dat_HIBetween0.55to0.699_49_96mnthsY <- dat_HIBetween0.55to0.699_49_96mnths[which(dat_HIBetween0.55to0.699_49_96mnths$NearestOlympicYear == uniqyr[jj]),]
			country <- c(country, uniqCount[ii]); 
			year <- c(year, uniqyr[jj]); 
			Participants <- c(Participants, unique(dat_HIBetween0.55to0.699_49_96mnthsY$TotalParticipant))
		    countryTotal_0.55to0.699_49_96mnths = countryTotal_0.55to0.699_49_96mnths + unique(dat_HIBetween0.55to0.699_49_96mnthsY$TotalParticipant)
        }
	}else{
	    dat_HIBetween0.55to0.699_49_96mnthsY <- dat_HIBetween0.55to0.699_49_96mnths[which(dat_HIBetween0.55to0.699_49_96mnths$NearestOlympicYear == uniqyr),]
		country <- c(country, uniqCount[ii]); 
		year <- c(year, uniqyr); 
		Participants <- c(Participants, unique(dat_HIBetween0.55to0.699_49_96mnthsY$TotalParticipant))
		countryTotal_0.55to0.699_49_96mnths = countryTotal_0.55to0.699_49_96mnths + unique(dat_HIBetween0.55to0.699_49_96mnthsY$TotalParticipant)
	}'
}
percHI_0.55to0.699_49_96mnths = (nrow(HumanIndxBetween0.55to0.699_49_96mnths)/countryTotal_0.55to0.699_49_96mnths)*100

HumanIndxBetween0.55to0.699_GreaterThan97 <- datFinal[which((datFinal$HumanIndex >= 0.55 & datFinal$HumanIndex <= 0.699)
	& (datFinal$Sanction_months >= 97)),]
nrow(HumanIndxBetween0.55to0.699_GreaterThan97)
countryTotal_0.55to0.699_GreaterThan97 = 0
uniqCount <- unique(HumanIndxBetween0.55to0.699_GreaterThan97$Nationality); 
country <- c(); year <- c(); Participants <- c()
for(ii in 1:length(uniqCount)){
    dat_HIBetween0.55to0.699_GreaterThan97 <- HumanIndxBetween0.55to0.699_GreaterThan97[which(HumanIndxBetween0.55to0.699_GreaterThan97$Nationality == uniqCount[ii]),]
	uniqyr <- unique(dat_HIBetween0.55to0.699_GreaterThan97$NearestOlympicYear)
	dat_HIBetween0.55to0.699_GreaterThan97Y <- r[which(r$NationAbbr == uniqCount[ii]),]
	countryTotal_0.55to0.699_GreaterThan97 = countryTotal_0.55to0.699_GreaterThan97 + unique(dat_HIBetween0.55to0.699_GreaterThan97Y$Country_Total)
	'if(length(uniqyr) > 1){
	    for(jj in 1:length(uniqyr)){
		    dat_HIBetween0.55to0.699_GreaterThan97Y <- dat_HIBetween0.55to0.699_GreaterThan97[which(dat_HIBetween0.55to0.699_GreaterThan97$NearestOlympicYear == uniqyr[jj]),]
			country <- c(country, uniqCount[ii]); 
			year <- c(year, uniqyr[jj]); 
			Participants <- c(Participants, unique(dat_HIBetween0.55to0.699_GreaterThan97Y$TotalParticipant))
		    countryTotal_0.55to0.699_GreaterThan97 = countryTotal_0.55to0.699_GreaterThan97 + unique(dat_HIBetween0.55to0.699_GreaterThan97Y$TotalParticipant)
        }
	}else{
	    dat_HIBetween0.55to0.699_GreaterThan97Y <- dat_HIBetween0.55to0.699_GreaterThan97[which(dat_HIBetween0.55to0.699_GreaterThan97$NearestOlympicYear == uniqyr),]
		country <- c(country, uniqCount[ii]); 
		year <- c(year, uniqyr); 
		Participants <- c(Participants, unique(dat_HIBetween0.55to0.699_GreaterThan97Y$TotalParticipant))
		countryTotal_0.55to0.699_GreaterThan97 = countryTotal_0.55to0.699_GreaterThan97 + unique(dat_HIBetween0.55to0.699_GreaterThan97Y$TotalParticipant)
	}'
}
percHI_0.55to0.699_GreaterThan97 = (nrow(HumanIndxBetween0.55to0.699_GreaterThan97)/countryTotal_0.55to0.699_GreaterThan97)*100

HumanIndxBetween0.7to0.799 <- datFinal[which(datFinal$HumanIndex >= 0.7 & datFinal$HumanIndex <= 0.799),]
nrow(HumanIndxBetween0.7to0.799)
#0.7to0.799
HumanIndxBetween0.7to0.799_LessThan24mnths <- datFinal[which((datFinal$HumanIndex >= 0.7 & datFinal$HumanIndex <= 0.799)
	& datFinal$Sanction_months <= 24),]
nrow(HumanIndxBetween0.7to0.799_LessThan24mnths)
countryTotal_0.7to0.799_LessThan24mnths = 0
uniqCount <- unique(HumanIndxBetween0.7to0.799_LessThan24mnths$Nationality); 
country <- c(); year <- c(); Participants <- c()
for(ii in 1:length(uniqCount)){
    dat_HIBetween0.7to0.799_LessThan24mnths <- HumanIndxBetween0.7to0.799_LessThan24mnths[which(HumanIndxBetween0.7to0.799_LessThan24mnths$Nationality == uniqCount[ii]),]
	uniqyr <- unique(dat_HIBetween0.7to0.799_LessThan24mnths$NearestOlympicYear)
	dat_HIBetween0.7to0.799_LessThan24mnthsY <- r[which(r$NationAbbr == uniqCount[ii]),]
	countryTotal_0.7to0.799_LessThan24mnths = countryTotal_0.7to0.799_LessThan24mnths + unique(dat_HIBetween0.7to0.799_LessThan24mnthsY$Country_Total)
	'if(length(uniqyr) > 1){
	    for(jj in 1:length(uniqyr)){
		    dat_HIBetween0.7to0.799_LessThan24mnthsY <- dat_HIBetween0.7to0.799_LessThan24mnths[which(dat_HIBetween0.7to0.799_LessThan24mnths$NearestOlympicYear == uniqyr[jj]),]
			country <- c(country, uniqCount[ii]); 
			year <- c(year, uniqyr[jj]); 
			Participants <- c(Participants, unique(dat_HIBetween0.7to0.799_LessThan24mnthsY$TotalParticipant))
		    countryTotal_0.7to0.799_LessThan24mnths = countryTotal_0.7to0.799_LessThan24mnths + unique(dat_HIBetween0.7to0.799_LessThan24mnthsY$TotalParticipant)
        }
	}else{
	    dat_HIBetween0.7to0.799_LessThan24mnthsY <- dat_HIBetween0.7to0.799_LessThan24mnths[which(dat_HIBetween0.7to0.799_LessThan24mnths$NearestOlympicYear == uniqyr),]
		country <- c(country, uniqCount[ii]); 
		year <- c(year, uniqyr); 
		Participants <- c(Participants, unique(dat_HIBetween0.7to0.799_LessThan24mnthsY$TotalParticipant))
		countryTotal_0.7to0.799_LessThan24mnths = countryTotal_0.7to0.799_LessThan24mnths + unique(dat_HIBetween0.7to0.799_LessThan24mnthsY$TotalParticipant)
	}'
}
percHI_0.7to0.799_LessThan24mnths = (nrow(HumanIndxBetween0.7to0.799_LessThan24mnths)/countryTotal_0.7to0.799_LessThan24mnths)*100

HumanIndxBetween0.7to0.799_25_48mnths <- datFinal[which((datFinal$HumanIndex >= 0.7 & datFinal$HumanIndex <= 0.799)
	& (datFinal$Sanction_months >= 25 
	& datFinal$Sanction_months <= 48)),]
nrow(HumanIndxBetween0.7to0.799_25_48mnths)
countryTotal_0.7to0.799_25_48mnths = 0
uniqCount <- unique(HumanIndxBetween0.7to0.799_25_48mnths$Nationality); 
country <- c(); year <- c(); Participants <- c()
for(ii in 1:length(uniqCount)){
    dat_HIBetween0.7to0.799_25_48mnths <- HumanIndxBetween0.7to0.799_25_48mnths[which(HumanIndxBetween0.7to0.799_25_48mnths$Nationality == uniqCount[ii]),]
	uniqyr <- unique(dat_HIBetween0.7to0.799_25_48mnths$NearestOlympicYear)
	dat_HIBetween0.7to0.799_25_48mnthsY <- r[which(r$NationAbbr == uniqCount[ii]),]
	countryTotal_0.7to0.799_25_48mnths = countryTotal_0.7to0.799_25_48mnths + unique(dat_HIBetween0.7to0.799_25_48mnthsY$Country_Total)
	
	'if(length(uniqyr) > 1){
	    for(jj in 1:length(uniqyr)){
		    dat_HIBetween0.7to0.799_25_48mnthsY <- dat_HIBetween0.7to0.799_25_48mnths[which(dat_HIBetween0.7to0.799_25_48mnths$NearestOlympicYear == uniqyr[jj]),]
			country <- c(country, uniqCount[ii]); 
			year <- c(year, uniqyr[jj]); 
			Participants <- c(Participants, unique(dat_HIBetween0.7to0.799_25_48mnthsY$TotalParticipant))
		    countryTotal_0.7to0.799_25_48mnths = countryTotal_0.7to0.799_25_48mnths + unique(dat_HIBetween0.7to0.799_25_48mnthsY$TotalParticipant)
        }
	}else{
	    dat_HIBetween0.7to0.799_25_48mnthsY <- dat_HIBetween0.7to0.799_25_48mnths[which(dat_HIBetween0.7to0.799_25_48mnths$NearestOlympicYear == uniqyr),]
		country <- c(country, uniqCount[ii]); 
		year <- c(year, uniqyr); 
		Participants <- c(Participants, unique(dat_HIBetween0.7to0.799_25_48mnthsY$TotalParticipant))
		countryTotal_0.7to0.799_25_48mnths = countryTotal_0.7to0.799_25_48mnths + unique(dat_HIBetween0.7to0.799_25_48mnthsY$TotalParticipant)
	}'
}
percHI_0.7to0.799_25_48mnths = (nrow(HumanIndxBetween0.7to0.799_25_48mnths)/countryTotal_0.7to0.799_25_48mnths)*100


HumanIndxBetween0.7to0.799_49_96mnths <- datFinal[which((datFinal$HumanIndex >= 0.7 & datFinal$HumanIndex <= 0.799)
	& (datFinal$Sanction_months >= 49 
	& datFinal$Sanction_months <= 96)),]
nrow(HumanIndxBetween0.7to0.799_49_96mnths)
countryTotal_0.7to0.799_49_96mnths = 0
uniqCount <- unique(HumanIndxBetween0.7to0.799_49_96mnths$Nationality); 
for(ii in 1:length(uniqCount)){
dat_HIBetween0.7to0.799_49_96mnthsY <- r[which(r$NationAbbr == uniqCount[ii]),]
countryTotal_0.7to0.799_49_96mnths = countryTotal_0.7to0.799_49_96mnths + unique(dat_HIBetween0.7to0.799_49_96mnthsY$Country_Total)
'country <- c(); year <- c(); Participants <- c()
for(ii in 1:length(uniqCount)){
    dat_HIBetween0.7to0.799_49_96mnths <- HumanIndxBetween0.7to0.799_49_96mnths[which(HumanIndxBetween0.7to0.799_49_96mnths$Nationality == uniqCount[ii]),]
	uniqyr <- unique(dat_HIBetween0.7to0.799_49_96mnths$NearestOlympicYear)
	if(length(uniqyr) > 1){
	    for(jj in 1:length(uniqyr)){
		    dat_HIBetween0.7to0.799_49_96mnthsY <- dat_HIBetween0.7to0.799_49_96mnths[which(dat_HIBetween0.7to0.799_49_96mnths$NearestOlympicYear == uniqyr[jj]),]
			country <- c(country, uniqCount[ii]); 
			year <- c(year, uniqyr[jj]); 
			Participants <- c(Participants, unique(dat_HIBetween0.7to0.799_49_96mnthsY$TotalParticipant))
		    countryTotal_0.7to0.799_49_96mnths = countryTotal_0.7to0.799_49_96mnths + unique(dat_HIBetween0.7to0.799_49_96mnthsY$TotalParticipant)
        }
	}else{
	    dat_HIBetween0.7to0.799_49_96mnthsY <- dat_HIBetween0.7to0.799_49_96mnths[which(dat_HIBetween0.7to0.799_49_96mnths$NearestOlympicYear == uniqyr),]
		country <- c(country, uniqCount[ii]); 
		year <- c(year, uniqyr); 
		Participants <- c(Participants, unique(dat_HIBetween0.7to0.799_49_96mnthsY$TotalParticipant))
		countryTotal_0.7to0.799_49_96mnths = countryTotal_0.7to0.799_49_96mnths + unique(dat_HIBetween0.7to0.799_49_96mnthsY$TotalParticipant)
	}'
}
percHI_0.7to0.799_49_96mnths = (nrow(HumanIndxBetween0.7to0.799_49_96mnths)/countryTotal_0.7to0.799_49_96mnths)*100

HumanIndxBetween0.7to0.799_GreaterThan97 <- datFinal[which((datFinal$HumanIndex >= 0.7 & datFinal$HumanIndex <= 0.799)
	& (datFinal$Sanction_months >= 97)),]
nrow(HumanIndxBetween0.7to0.799_GreaterThan97)
countryTotal_0.7to0.799_GreaterThan97 = 0
uniqCount <- unique(HumanIndxBetween0.7to0.799_GreaterThan97$Nationality); 
for(ii in 1:length(uniqCount)){
dat_HIBetween0.7to0.799_GreaterThan97Y <- r[which(r$NationAbbr == uniqCount[ii]),]
countryTotal_0.7to0.799_GreaterThan97 = countryTotal_0.7to0.799_GreaterThan97 + unique(dat_HIBetween0.7to0.799_GreaterThan97Y$Country_Total)
'country <- c(); year <- c(); Participants <- c()
for(ii in 1:length(uniqCount)){
    dat_HIBetween0.7to0.799_GreaterThan97 <- HumanIndxBetween0.7to0.799_GreaterThan97[which(HumanIndxBetween0.7to0.799_GreaterThan97$Nationality == uniqCount[ii]),]
	uniqyr <- unique(dat_HIBetween0.7to0.799_GreaterThan97$NearestOlympicYear)
	if(length(uniqyr) > 1){
	    for(jj in 1:length(uniqyr)){
		    dat_HIBetween0.7to0.799_GreaterThan97Y <- dat_HIBetween0.7to0.799_GreaterThan97[which(dat_HIBetween0.7to0.799_GreaterThan97$NearestOlympicYear == uniqyr[jj]),]
			country <- c(country, uniqCount[ii]); 
			year <- c(year, uniqyr[jj]); 
			Participants <- c(Participants, unique(dat_HIBetween0.7to0.799_GreaterThan97Y$TotalParticipant))
		    countryTotal_0.7to0.799_GreaterThan97 = countryTotal_0.7to0.799_GreaterThan97 + unique(dat_HIBetween0.7to0.799_GreaterThan97Y$TotalParticipant)
        }
	}else{
	    dat_HIBetween0.7to0.799_GreaterThan97Y <- dat_HIBetween0.7to0.799_GreaterThan97[which(dat_HIBetween0.7to0.799_GreaterThan97$NearestOlympicYear == uniqyr),]
		country <- c(country, uniqCount[ii]); 
		year <- c(year, uniqyr); 
		Participants <- c(Participants, unique(dat_HIBetween0.7to0.799_GreaterThan97Y$TotalParticipant))
		countryTotal_0.7to0.799_GreaterThan97 = countryTotal_0.7to0.799_GreaterThan97 + unique(dat_HIBetween0.7to0.799_GreaterThan97Y$TotalParticipant)
	}'
}
percHI_0.7to0.799_GreaterThan97 = (nrow(HumanIndxBetween0.7to0.799_GreaterThan97)/countryTotal_0.7to0.799_GreaterThan97)*100

'HumanIndxBetween0.8to1 <- datFinal[which(datFinal$HumanIndex >= 0.8 & datFinal$HumanIndex <= 1),]
nrow(HumanIndxBetween0.8to1)
'
HumanIndxBetween0.8to1_LessThan24mnths <- datFinal[which((datFinal$HumanIndex >= 0.8 & datFinal$HumanIndex <= 1)
	& datFinal$Sanction_months <= 24),]
nrow(HumanIndxBetween0.8to1_LessThan24mnths)
countryTotal_0.8to1_LessThan24mnths = 0
uniqCount <- unique(HumanIndxBetween0.8to1_LessThan24mnths$Nationality); 
country <- c(); year <- c(); Participants <- c()
for(ii in 1:length(uniqCount)){
    dat_HIBetween0.8to1_LessThan24mnths <- HumanIndxBetween0.8to1_LessThan24mnths[which(HumanIndxBetween0.8to1_LessThan24mnths$Nationality == uniqCount[ii]),]
	uniqyr <- unique(dat_HIBetween0.8to1_LessThan24mnths$NearestOlympicYear)
	dat_HIBetween0.8to1_LessThan24mnthsY <- r[which(r$NationAbbr == uniqCount[ii]),]
    countryTotal_0.8to1_LessThan24mnths = countryTotal_0.8to1_LessThan24mnths + unique(dat_HIBetween0.8to1_LessThan24mnthsY$Country_Total)
	'if(length(uniqyr) > 1){
	    for(jj in 1:length(uniqyr)){
		    dat_HIBetween0.8to1_LessThan24mnthsY <- dat_HIBetween0.8to1_LessThan24mnths[which(dat_HIBetween0.8to1_LessThan24mnths$NearestOlympicYear == uniqyr[jj]),]
			country <- c(country, uniqCount[ii]); 
			year <- c(year, uniqyr[jj]); 
			Participants <- c(Participants, unique(dat_HIBetween0.8to1_LessThan24mnthsY$TotalParticipant))
		    countryTotal_0.8to1_LessThan24mnths = countryTotal_0.8to1_LessThan24mnths + unique(dat_HIBetween0.8to1_LessThan24mnthsY$TotalParticipant)
        }
	}else{
	    dat_HIBetween0.8to1_LessThan24mnthsY <- dat_HIBetween0.8to1_LessThan24mnths[which(dat_HIBetween0.8to1_LessThan24mnths$NearestOlympicYear == uniqyr),]
		country <- c(country, uniqCount[ii]); 
		year <- c(year, uniqyr); 
		Participants <- c(Participants, unique(dat_HIBetween0.8to1_LessThan24mnthsY$TotalParticipant))
		countryTotal_0.8to1_LessThan24mnths = countryTotal_0.8to1_LessThan24mnths + unique(dat_HIBetween0.8to1_LessThan24mnthsY$TotalParticipant)
	}'
}
percHI_0.8to1_LessThan24mnths = (nrow(HumanIndxBetween0.8to1_LessThan24mnths)/countryTotal_0.8to1_LessThan24mnths)*100

HumanIndxBetween0.8to1_25_48mnths <- datFinal[which((datFinal$HumanIndex >= 0.8 & datFinal$HumanIndex <= 1)
	& (datFinal$Sanction_months >= 25 
	& datFinal$Sanction_months <= 48)),]
nrow(HumanIndxBetween0.8to1_25_48mnths)
countryTotal_0.8to1_25_48mnths = 0
uniqCount <- unique(HumanIndxBetween0.8to1_25_48mnths$Nationality); 
country <- c(); year <- c(); Participants <- c()
for(ii in 1:length(uniqCount)){
    dat_HIBetween0.8to1_25_48mnths <- HumanIndxBetween0.8to1_25_48mnths[which(HumanIndxBetween0.8to1_25_48mnths$Nationality == uniqCount[ii]),]
	uniqyr <- unique(dat_HIBetween0.8to1_25_48mnths$NearestOlympicYear)
	dat_HIBetween0.8to1_25_48mnthsY <- r[which(r$NationAbbr == uniqCount[ii]),]
    countryTotal_0.8to1_25_48mnths = countryTotal_0.8to1_25_48mnths + unique(dat_HIBetween0.8to1_25_48mnthsY$Country_Total)
	'if(length(uniqyr) > 1){
	    for(jj in 1:length(uniqyr)){
		    dat_HIBetween0.8to1_25_48mnthsY <- dat_HIBetween0.8to1_25_48mnths[which(dat_HIBetween0.8to1_25_48mnths$NearestOlympicYear == uniqyr[jj]),]
			country <- c(country, uniqCount[ii]); 
			year <- c(year, uniqyr[jj]); 
			Participants <- c(Participants, unique(dat_HIBetween0.8to1_25_48mnthsY$Male))
		    countryTotal_0.8to1_25_48mnths = countryTotal_0.8to1_25_48mnths + unique(dat_HIBetween0.8to1_25_48mnthsY$Male)
        }
	}else{
	    dat_HIBetween0.8to1_25_48mnthsY <- dat_HIBetween0.8to1_25_48mnths[which(dat_HIBetween0.8to1_25_48mnths$NearestOlympicYear == uniqyr),]
		country <- c(country, uniqCount[ii]); 
		year <- c(year, uniqyr); 
		Participants <- c(Participants, unique(dat_HIBetween0.8to1_25_48mnthsY$Male))
		countryTotal_0.8to1_25_48mnths = countryTotal_0.8to1_25_48mnths + unique(dat_HIBetween0.8to1_25_48mnthsY$Male)
	}'
}
percHI_0.8to1_25_48mnths = (nrow(HumanIndxBetween0.8to1_25_48mnths)/countryTotal_0.8to1_25_48mnths)*100


HumanIndxBetween0.8to1_49_96mnths <- datFinal[which((datFinal$HumanIndex >= 0.8 & datFinal$HumanIndex <= 1)
	& (datFinal$Sanction_months >= 49 
	& datFinal$Sanction_months <= 96)),]
nrow(HumanIndxBetween0.8to1_49_96mnths)
countryTotal_0.8to1_49_96mnths = 0
uniqCount <- unique(HumanIndxBetween0.8to1_49_96mnths$Nationality); 
country <- c(); year <- c(); Participants <- c()
for(ii in 1:length(uniqCount)){
    dat_HIBetween0.8to1_49_96mnths <- HumanIndxBetween0.8to1_49_96mnths[which(HumanIndxBetween0.8to1_49_96mnths$Nationality == uniqCount[ii]),]
	uniqyr <- unique(dat_HIBetween0.8to1_49_96mnths$NearestOlympicYear)
	dat_HIBetween0.8to1_49_96mnthsY <- r[which(r$NationAbbr == uniqCount[ii]),]
    countryTotal_0.8to1_49_96mnths = countryTotal_0.8to1_49_96mnths + unique(dat_HIBetween0.8to1_49_96mnthsY$Country_Total)
	'if(length(uniqyr) > 1){
	    for(jj in 1:length(uniqyr)){
		    dat_HIBetween0.8to1_49_96mnthsY <- dat_HIBetween0.8to1_49_96mnths[which(dat_HIBetween0.8to1_49_96mnths$NearestOlympicYear == uniqyr[jj]),]
			country <- c(country, uniqCount[ii]); 
			year <- c(year, uniqyr[jj]); 
			Participants <- c(Participants, unique(dat_HIBetween0.8to1_49_96mnthsY$Male))
		    countryTotal_0.8to1_49_96mnths = countryTotal_0.8to1_49_96mnths + unique(dat_HIBetween0.8to1_49_96mnthsY$Male)
        }
	}else{
	    dat_HIBetween0.8to1_49_96mnthsY <- dat_HIBetween0.8to1_49_96mnths[which(dat_HIBetween0.8to1_49_96mnths$NearestOlympicYear == uniqyr),]
		country <- c(country, uniqCount[ii]); 
		year <- c(year, uniqyr); 
		Participants <- c(Participants, unique(dat_HIBetween0.8to1_49_96mnthsY$Male))
		countryTotal_0.8to1_49_96mnths = countryTotal_0.8to1_49_96mnths + unique(dat_HIBetween0.8to1_49_96mnthsY$Male)
	}'
}
percHI_0.8to1_49_96mnths = (nrow(HumanIndxBetween0.8to1_49_96mnths)/countryTotal_0.8to1_49_96mnths)*100

HumanIndxBetween0.8to1_GreaterThan97 <- datFinal[which((datFinal$HumanIndex >= 0.8 & datFinal$HumanIndex <= 1)
	& (datFinal$Sanction_months >= 97)),]
nrow(HumanIndxBetween0.8to1_GreaterThan97)
countryTotal_0.8to1_GreaterThan97 = 0
uniqCount <- unique(HumanIndxBetween0.8to1_GreaterThan97$Nationality); 
country <- c(); year <- c(); Participants <- c()
for(ii in 1:length(uniqCount)){
    dat_HIBetween0.8to1_GreaterThan97 <- HumanIndxBetween0.8to1_GreaterThan97[which(HumanIndxBetween0.8to1_GreaterThan97$Nationality == uniqCount[ii]),]
	uniqyr <- unique(dat_HIBetween0.8to1_GreaterThan97$NearestOlympicYear)
	dat_HIBetween0.8to1_GreaterThan97Y <- r[which(r$NationAbbr == uniqCount[ii]),]
    countryTotal_0.8to1_GreaterThan97 = countryTotal_0.8to1_GreaterThan97 + unique(dat_HIBetween0.8to1_GreaterThan97Y$Country_Total)
	'if(length(uniqyr) > 1){
	    for(jj in 1:length(uniqyr)){
		    dat_HIBetween0.8to1_GreaterThan97Y <- dat_HIBetween0.8to1_GreaterThan97[which(dat_HIBetween0.8to1_GreaterThan97$NearestOlympicYear == uniqyr[jj]),]
			country <- c(country, uniqCount[ii]); 
			year <- c(year, uniqyr[jj]); 
			Participants <- c(Participants, unique(dat_HIBetween0.8to1_GreaterThan97Y$Male))
		    countryTotal_0.8to1_GreaterThan97 = countryTotal_0.8to1_GreaterThan97 + unique(dat_HIBetween0.8to1_GreaterThan97Y$Male)
        }
	}else{
	    dat_HIBetween0.8to1_GreaterThan97Y <- dat_HIBetween0.8to1_GreaterThan97[which(dat_HIBetween0.8to1_GreaterThan97$NearestOlympicYear == uniqyr),]
		country <- c(country, uniqCount[ii]); 
		year <- c(year, uniqyr); 
		Participants <- c(Participants, unique(dat_HIBetween0.8to1_GreaterThan97Y$Male))
		countryTotal_0.8to1_GreaterThan97 = countryTotal_0.8to1_GreaterThan97 + unique(dat_HIBetween0.8to1_GreaterThan97Y$Male)
	}'
}
percHI_0.8to1_GreaterThan97 = (nrow(HumanIndxBetween0.8to1_GreaterThan97)/countryTotal_0.8to1_GreaterThan97)*100
datHI_SP<- data.frame(HumanIndex = c(
            "HI_0.35to0.549",
			"HI_0.35to0.549",
			"HI_0.35to0.549",
			"HI_0.35to0.549",
			"HI_0.55to0.699",
			"HI_0.55to0.699",
			"HI_0.55to0.699",
			"HI_0.55to0.699",
			"HI_0.70to0.799",
			"HI_0.70to0.799",
			"HI_0.70to0.799",
			"HI_0.70to0.799",
			"HI_0.80to1",
			"HI_0.80to1",
			"HI_0.80to1",
			"HI_0.80to1"),
        NumberofAthletes = c(
	        nrow(HumanIndxBetween0.35to0.549_LessThan24mnths),
            nrow(HumanIndxBetween0.35to0.549_25_48mnths),			
	        nrow(HumanIndxBetween0.35to0.549_49_96mnths),
			nrow(HumanIndxBetween0.35to0.549_GreaterThan97),
			nrow(HumanIndxBetween0.55to0.699_LessThan24mnths),
            nrow(HumanIndxBetween0.55to0.699_25_48mnths),			
	        nrow(HumanIndxBetween0.55to0.699_49_96mnths),
			nrow(HumanIndxBetween0.55to0.699_GreaterThan97),
			nrow(HumanIndxBetween0.7to0.799_LessThan24mnths),
            nrow(HumanIndxBetween0.7to0.799_25_48mnths),			
	        nrow(HumanIndxBetween0.7to0.799_49_96mnths),
			nrow(HumanIndxBetween0.7to0.799_GreaterThan97),
			nrow(HumanIndxBetween0.8to1_LessThan24mnths),
            nrow(HumanIndxBetween0.8to1_25_48mnths),			
	        nrow(HumanIndxBetween0.8to1_49_96mnths),
			nrow(HumanIndxBetween0.8to1_GreaterThan97)),
	    TotalAthletes = c(countryTotal_0.35to0.549_LessThan24mnths,
		    countryTotal_0.35to0.549_25_48mnths,
			countryTotal_0.35to0.549_49_96mnths,
			countryTotal_0.35to0.549_GreaterThan97,
			countryTotal_0.55to0.699_LessThan24mnths,
		    countryTotal_0.55to0.699_25_48mnths,
			countryTotal_0.55to0.699_49_96mnths,
			countryTotal_0.55to0.699_GreaterThan97,
			countryTotal_0.7to0.799_LessThan24mnths,
		    countryTotal_0.7to0.799_25_48mnths,
			countryTotal_0.7to0.799_49_96mnths,
			countryTotal_0.7to0.799_GreaterThan97,
			countryTotal_0.8to1_LessThan24mnths,
			countryTotal_0.8to1_25_48mnths,
			countryTotal_0.8to1_49_96mnths,
			countryTotal_0.8to1_GreaterThan97),
        SanctionPercent = c(percHI_0.35to0.549_LessThan24mnths,
		    percHI_0.35to0.549_25_48mnths,
			percHI_0.35to0.549_49_96mnths,
			percHI_0.35to0.549_GreaterThan97,
			percHI_0.55to0.699_LessThan24mnths,
		    percHI_0.55to0.699_25_48mnths,
			percHI_0.55to0.699_49_96mnths,
			percHI_0.55to0.699_GreaterThan97,
			percHI_0.7to0.799_LessThan24mnths,
		    percHI_0.7to0.799_25_48mnths,
			percHI_0.7to0.799_49_96mnths,
			percHI_0.7to0.799_GreaterThan97,
			percHI_0.8to1_LessThan24mnths,
		    percHI_0.8to1_25_48mnths,
			percHI_0.8to1_49_96mnths,
			percHI_0.8to1_GreaterThan97),
		SanctionPeriod = c("<=24mnths",
		    "25_48mnths",
			"49_96mnths",
			">=97",
			"<=24mnths",
		    "25_48mnths",
			"49_96mnths",
			">=97",
			"<=24mnths",
		    "25_48mnths",
			"49_96mnths",
			">=97",
			"<=24mnths",
		    "25_48mnths",
			"49_96mnths",
			">=97"))
write.csv(datHI_SP, file.path(outdir,"HumanIndex_PerSanctionPeriod.csv"), row.names = FALSE)

library(ggplot2)
p11 <- ggplot(data = datHI_SP, aes(x = factor(SanctionPeriod, level = unique(SanctionPeriod)), y= SanctionPercent, fill = factor(HumanIndex, levels=unique(HumanIndex)))) + geom_bar(stat = "identity", position="dodge")+ labs(x = "SanctionPeriod", y = "% of Sanctioned Athletes", fill = "Human Index") + theme(legend.title = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.title.x = element_text(face = "bold", size = 50),axis.title.y = element_text(face = "bold", size = 50), axis.text.x = element_text(face="bold", size = 30, angle = 45, hjust = 1), axis.text.y = element_text(face="bold", size = 30), legend.text=element_text(face = "bold",size=25)) + scale_fill_manual(values=c("green","red", "blue", "yellow")) 
p11


###HumanIndex-Sex

#HumanIndex
HumanIndxBetween0.35to0.549 <- datFinal[which(datFinal$HumanIndex >= 0.35 & datFinal$HumanIndex <= 0.549),]
nrow(HumanIndxBetween0.35to0.549)
countryTotal_0.35to0.549 = 0
countryMaleGS = 0
countryFemaleGS = 0
countryTotalGS = 0
totalpercountry <- c()
uniqCount <- unique(HumanIndxBetween0.35to0.549$Nationality); 
country <- c(); year <- c(); Participants <- c()
country <- c(); year <- c(); MaleParticipants0.35to0.549 <- c(); FemaleParticipants0.35to0.549 <- c()
for(ii in 1:length(uniqCount)){
    dat_HIBetween0.35to0.549 <- HumanIndxBetween0.35to0.549[which(HumanIndxBetween0.35to0.549$Nationality == uniqCount[ii]),]
	uniqyr <- unique(dat_HIBetween0.35to0.549$NearestOlympicYear)
	if(length(uniqyr) > 1){
	    for(jj in 1:length(uniqyr)){
		    dat_HIBetween0.35to0.549Y <- dat_HIBetween0.35to0.549[which(dat_HIBetween0.35to0.549$NearestOlympicYear == uniqyr[jj]),]
			country <- c(country, uniqCount[ii]); 
			year <- c(year, uniqyr[jj]); 
			
			Participants <- c(Participants, unique(dat_HIBetween0.35to0.549Y$TotalParticipant))
		    countryTotal_0.35to0.549 = countryTotal_0.35to0.549 + unique(dat_HIBetween0.35to0.549Y$TotalParticipant)
        }
	}else{
	    dat_HIBetween0.35to0.549Y <- dat_HIBetween0.35to0.549[which(dat_HIBetween0.35to0.549$NearestOlympicYear == uniqyr),]
		country <- c(country, uniqCount[ii]); 
		year <- c(year, uniqyr); 
		Participants <- c(Participants, unique(dat_HIBetween0.35to0.549Y$TotalParticipant))
		countryTotal_0.35to0.549 = countryTotal_0.35to0.549 + unique(dat_HIBetween0.35to0.549Y$TotalParticipant)
	}
}
percHI_0.35to0.549_HI = (nrow(HumanIndxBetween0.35to0.549)/countryTotal_0.35to0.549)*100
percHI_0.35to0.549_TotSanctionedPercent = (nrow(HumanIndxBetween0.35to0.549)/nrow(datFinal))*100
HumanIndxBetween0.55to0.699 <- datFinal[which(datFinal$HumanIndex >= 0.55 & datFinal$HumanIndex <= 0.699),]
nrow(HumanIndxBetween0.55to0.699)
countryTotal_0.55to0.699 = 0
uniqCount <- unique(HumanIndxBetween0.55to0.699$Nationality); 
country <- c(); year <- c(); Participants <- c()
for(ii in 1:length(uniqCount)){
    dat_HIBetween0.55to0.699 <- HumanIndxBetween0.55to0.699[which(HumanIndxBetween0.55to0.699$Nationality == uniqCount[ii]),]
	uniqyr <- unique(dat_HIBetween0.55to0.699$NearestOlympicYear)
	if(length(uniqyr) > 1){
	    for(jj in 1:length(uniqyr)){
		    dat_HIBetween0.55to0.699Y <- dat_HIBetween0.55to0.699[which(dat_HIBetween0.55to0.699$NearestOlympicYear == uniqyr[jj]),]
			country <- c(country, uniqCount[ii]); 
			year <- c(year, uniqyr[jj]); 
			Participants <- c(Participants, unique(dat_HIBetween0.55to0.699Y$TotalParticipant))
		    countryTotal_0.55to0.699 = countryTotal_0.55to0.699 + unique(dat_HIBetween0.55to0.699Y$TotalParticipant)
        }
	}else{
	    dat_HIBetween0.55to0.699Y <- dat_HIBetween0.55to0.699[which(dat_HIBetween0.55to0.699$NearestOlympicYear == uniqyr),]
		country <- c(country, uniqCount[ii]); 
		year <- c(year, uniqyr); 
		Participants <- c(Participants, unique(dat_HIBetween0.55to0.699Y$TotalParticipant))
		countryTotal_0.55to0.699 = countryTotal_0.55to0.699 + unique(dat_HIBetween0.55to0.699Y$TotalParticipant)
	}
}
percHI_0.55to0.699_TotSanctionedPercent = (nrow(HumanIndxBetween0.55to0.699)/nrow(datFinal))*100
percHI_0.55to0.699 = (nrow(HumanIndxBetween0.55to0.699)/countryTotal_0.55to0.699)*100
HumanIndxBetween0.7to0.799 <- datFinal[which(datFinal$HumanIndex >= 0.7 & datFinal$HumanIndex <= 0.799),]
nrow(HumanIndxBetween0.7to0.799)
countryTotal_0.7to0.799 = 0
uniqCount <- unique(HumanIndxBetween0.7to0.799$Nationality); 
country <- c(); year <- c(); Participants <- c()
for(ii in 1:length(uniqCount)){
    dat_HIBetween0.7to0.799 <- HumanIndxBetween0.7to0.799[which(HumanIndxBetween0.7to0.799$Nationality == uniqCount[ii]),]
	uniqyr <- unique(dat_HIBetween0.7to0.799$NearestOlympicYear)
	if(length(uniqyr) > 1){
	    for(jj in 1:length(uniqyr)){
		    dat_HIBetween0.7to0.799Y <- dat_HIBetween0.7to0.799[which(dat_HIBetween0.7to0.799$NearestOlympicYear == uniqyr[jj]),]
			country <- c(country, uniqCount[ii]); 
			year <- c(year, uniqyr[jj]); 
			Participants <- c(Participants, unique(dat_HIBetween0.7to0.799Y$TotalParticipant))
		    countryTotal_0.7to0.799 = countryTotal_0.7to0.799 + unique(dat_HIBetween0.7to0.799Y$TotalParticipant)
        }
	}else{
	    dat_HIBetween0.7to0.799Y <- dat_HIBetween0.7to0.799[which(dat_HIBetween0.7to0.799$NearestOlympicYear == uniqyr),]
		country <- c(country, uniqCount[ii]); 
		year <- c(year, uniqyr); 
		Participants <- c(Participants, unique(dat_HIBetween0.7to0.799Y$TotalParticipant))
		countryTotal_0.7to0.799 = countryTotal_0.7to0.799 + unique(dat_HIBetween0.7to0.799Y$TotalParticipant)
	}
}
percHI_0.7to0.799_TotSanctionedPercent = (nrow(HumanIndxBetween0.7to0.799)/nrow(datFinal))*100
percHI_0.7to0.799 = (nrow(HumanIndxBetween0.7to0.799)/countryTotal_0.7to0.799)*100
HumanIndxBetween0.8to1 <- datFinal[which(datFinal$HumanIndex >= 0.8 & datFinal$HumanIndex <= 1),]
nrow(HumanIndxBetween0.8to1)
countryTotal_0.8to1 = 0
uniqCount <- unique(HumanIndxBetween0.8to1$Nationality); 
country <- c(); year <- c(); Participants <- c()
for(ii in 1:length(uniqCount)){
    dat_HIBetween0.8to1 <- HumanIndxBetween0.8to1[which(HumanIndxBetween0.8to1$Nationality == uniqCount[ii]),]
	uniqyr <- unique(dat_HIBetween0.8to1$NearestOlympicYear)
	if(length(uniqyr) > 1){
	    for(jj in 1:length(uniqyr)){
		    dat_HIBetween0.8to1Y <- dat_HIBetween0.8to1[which(dat_HIBetween0.8to1$NearestOlympicYear == uniqyr[jj]),]
			country <- c(country, uniqCount[ii]); 
			year <- c(year, uniqyr[jj]); 
			Participants <- c(Participants, unique(dat_HIBetween0.8to1Y$TotalParticipant))
		    countryTotal_0.8to1 = countryTotal_0.8to1 + unique(dat_HIBetween0.8to1Y$TotalParticipant)
        }
	}else{
	    dat_HIBetween0.8to1Y <- dat_HIBetween0.8to1[which(dat_HIBetween0.8to1$NearestOlympicYear == uniqyr),]
		country <- c(country, uniqCount[ii]); 
		year <- c(year, uniqyr); 
		Participants <- c(Participants, unique(dat_HIBetween0.8to1Y$TotalParticipant))
		countryTotal_0.8to1 = countryTotal_0.8to1 + unique(dat_HIBetween0.8to1Y$TotalParticipant)
	}
}
percHI_0.8to1_TotSanctionedPercent = (nrow(HumanIndxBetween0.8to1)/nrow(datFinal))*100
percHI_0.8to1 = (nrow(HumanIndxBetween0.8to1)/countryTotal_0.8to1)*100
datHI<- data.frame(HumanIndex = c(
            "HI_0.35to0.549",
			"HI_0.55to0.699", 
			"HI_0.7to0.799",
			"HI_0.8to1"),
        NumberofAthletes = c(
	        nrow(HumanIndxBetween0.35to0.549),
            nrow(HumanIndxBetween0.55to0.699),			
	        nrow(HumanIndxBetween0.7to0.799),
			nrow(HumanIndxBetween0.8to1)),
	    TotalAthletesFrom_HIPercentage = c(countryTotal_0.35to0.549,
		    countryTotal_0.55to0.699,
			countryTotal_0.7to0.799,
			countryTotal_0.8to1),
		SanctionPercent_HI = c(percHI_0.35to0.549,
		    percHI_0.55to0.699,
			percHI_0.7to0.799,
			percHI_0.8to1),
		SanctionPercent_TotalSanctioned = c(percHI_0.35to0.549_TotSanctionedPercent,
		percHI_0.55to0.699_TotSanctionedPercent,
		percHI_0.7to0.799_TotSanctionedPercent,
		percHI_0.8to1_TotSanctionedPercent))
p10 <- ggplot(data = datHI, aes(x = factor(HumanIndex, levels=unique(HumanIndex)), y= SanctionPercent_HI, fill = factor(HumanIndex, levels=unique(HumanIndex)))) + geom_bar(stat = "identity")+ labs(x = "SanctionPeriod", y = "Percentage of Athletes") + theme(legend.title = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.title.x = element_text(face = "bold", size = 50),axis.title.y = element_text(face = "bold", size = 50), axis.text.x = element_text(face="bold", size = 30, angle = 45, hjust = 1), axis.text.y = element_text(face="bold", size = 30), legend.text=element_text(face = "bold",size=25)) + scale_fill_manual(values=c("green","red", "blue", "yellow")) 
p10

#HumanIndex
###0.35_0.549
HumanIndxBetween0.35to0.549_LessThan24mnths <- datFinal[which((datFinal$HumanIndex >= 0.35 
    & datFinal$HumanIndex <= 0.549)
	& datFinal$Sanction_months <= 24),]
nrow(HumanIndxBetween0.35to0.549_LessThan24mnths)
countryTotal_0.35to0.549_LessThan24mnths = 0
uniqCount <- unique(HumanIndxBetween0.35to0.549_LessThan24mnths$Nationality); 
country <- c(); year <- c(); Participants <- c()
for(ii in 1:length(uniqCount)){
    dat_HIBetween0.35to0.549_LessThan24mnths <- HumanIndxBetween0.35to0.549_LessThan24mnths[which(HumanIndxBetween0.35to0.549_LessThan24mnths$Nationality == uniqCount[ii]),]
	uniqyr <- unique(dat_HIBetween0.35to0.549_LessThan24mnths$NearestOlympicYear)
	if(length(uniqyr) > 1){
	    for(jj in 1:length(uniqyr)){
		    dat_HIBetween0.35to0.549_LessThan24mnthsY <- dat_HIBetween0.35to0.549_LessThan24mnths[which(dat_HIBetween0.35to0.549_LessThan24mnths$NearestOlympicYear == uniqyr[jj]),]
			country <- c(country, uniqCount[ii]); 
			year <- c(year, uniqyr[jj]); 
			Participants <- c(Participants, unique(dat_HIBetween0.35to0.549_LessThan24mnthsY$TotalParticipant))
		    countryTotal_0.35to0.549_LessThan24mnths = countryTotal_0.35to0.549_LessThan24mnths + unique(dat_HIBetween0.35to0.549_LessThan24mnthsY$TotalParticipant)
        }
	}else{
	    dat_HIBetween0.35to0.549_LessThan24mnthsY <- dat_HIBetween0.35to0.549_LessThan24mnths[which(dat_HIBetween0.35to0.549_LessThan24mnths$NearestOlympicYear == uniqyr),]
		country <- c(country, uniqCount[ii]); 
		year <- c(year, uniqyr); 
		Participants <- c(Participants, unique(dat_HIBetween0.35to0.549_LessThan24mnthsY$TotalParticipant))
		countryTotal_0.35to0.549_LessThan24mnths = countryTotal_0.35to0.549_LessThan24mnths + unique(dat_HIBetween0.35to0.549_LessThan24mnthsY$TotalParticipant)
	}
}
percHI_0.35to0.549_LessThan24mnths = (nrow(HumanIndxBetween0.35to0.549_LessThan24mnths)/countryTotal_0.35to0.549_LessThan24mnths)*100

HumanIndxBetween0.35to0.549_25_48mnths <- datFinal[which((datFinal$HumanIndex >= 0.35 
    & datFinal$HumanIndex <= 0.549)
	& (datFinal$Sanction_months >= 25 
	& datFinal$Sanction_months <= 48)),]
nrow(HumanIndxBetween0.35to0.549_25_48mnths)
countryTotal_0.35to0.549_25_48mnths = 0
uniqCount <- unique(HumanIndxBetween0.35to0.549_25_48mnths$Nationality); 
country <- c(); year <- c(); Participants <- c()
for(ii in 1:length(uniqCount)){
    dat_HIBetween0.35to0.549_25_48mnths <- HumanIndxBetween0.35to0.549_25_48mnths[which(HumanIndxBetween0.35to0.549_25_48mnths$Nationality == uniqCount[ii]),]
	uniqyr <- unique(dat_HIBetween0.35to0.549_25_48mnths$NearestOlympicYear)
	if(length(uniqyr) > 1){
	    for(jj in 1:length(uniqyr)){
		    dat_HIBetween0.35to0.549_25_48mnthsY <- dat_HIBetween0.35to0.549_25_48mnths[which(dat_HIBetween0.35to0.549_25_48mnths$NearestOlympicYear == uniqyr[jj]),]
			country <- c(country, uniqCount[ii]); 
			year <- c(year, uniqyr[jj]); 
			Participants <- c(Participants, unique(dat_HIBetween0.35to0.549_25_48mnthsY$TotalParticipant))
		    countryTotal_0.35to0.549_25_48mnths = countryTotal_0.35to0.549_25_48mnths + unique(dat_HIBetween0.35to0.549_25_48mnthsY$TotalParticipant)
        }
	}else{
	    dat_HIBetween0.35to0.549_25_48mnthsY <- dat_HIBetween0.35to0.549_25_48mnths[which(dat_HIBetween0.35to0.549_25_48mnths$NearestOlympicYear == uniqyr),]
		country <- c(country, uniqCount[ii]); 
		year <- c(year, uniqyr); 
		Participants <- c(Participants, unique(dat_HIBetween0.35to0.549_25_48mnthsY$TotalParticipant))
		countryTotal_0.35to0.549_25_48mnths = countryTotal_0.35to0.549_25_48mnths + unique(dat_HIBetween0.35to0.549_25_48mnthsY$TotalParticipant)
	}
}
percHI_0.35to0.549_25_48mnths = (nrow(HumanIndxBetween0.35to0.549_25_48mnths)/countryTotal_0.35to0.549_25_48mnths)*100


HumanIndxBetween0.35to0.549_49_96mnths <- datFinal[which((datFinal$HumanIndex >= 0.35 
    & datFinal$HumanIndex <= 0.549)
	& (datFinal$Sanction_months >= 49 
	& datFinal$Sanction_months <= 96)),]
nrow(HumanIndxBetween0.35to0.549_49_96mnths)
countryTotal_0.35to0.549_49_96mnths = 0
uniqCount <- unique(HumanIndxBetween0.35to0.549_49_96mnths$Nationality); 
country <- c(); year <- c(); Participants <- c()
for(ii in 1:length(uniqCount)){
    dat_HIBetween0.35to0.549_49_96mnths <- HumanIndxBetween0.35to0.549_49_96mnths[which(HumanIndxBetween0.35to0.549_49_96mnths$Nationality == uniqCount[ii]),]
	uniqyr <- unique(dat_HIBetween0.35to0.549_49_96mnths$NearestOlympicYear)
	if(length(uniqyr) > 1){
	    for(jj in 1:length(uniqyr)){
		    dat_HIBetween0.35to0.549_49_96mnthsY <- dat_HIBetween0.35to0.549_49_96mnths[which(dat_HIBetween0.35to0.549_49_96mnths$NearestOlympicYear == uniqyr[jj]),]
			country <- c(country, uniqCount[ii]); 
			year <- c(year, uniqyr[jj]); 
			Participants <- c(Participants, unique(dat_HIBetween0.35to0.549_49_96mnthsY$TotalParticipant))
		    countryTotal_0.35to0.549_49_96mnths = countryTotal_0.35to0.549_49_96mnths + unique(dat_HIBetween0.35to0.549_49_96mnthsY$TotalParticipant)
        }
	}else{
	    dat_HIBetween0.35to0.549_49_96mnthsY <- dat_HIBetween0.35to0.549_49_96mnths[which(dat_HIBetween0.35to0.549_49_96mnths$NearestOlympicYear == uniqyr),]
		country <- c(country, uniqCount[ii]); 
		year <- c(year, uniqyr); 
		Participants <- c(Participants, unique(dat_HIBetween0.35to0.549_49_96mnthsY$TotalParticipant))
		countryTotal_0.35to0.549_49_96mnths = countryTotal_0.35to0.549_49_96mnths + unique(dat_HIBetween0.35to0.549_49_96mnthsY$TotalParticipant)
	}
}
percHI_0.35to0.549_49_96mnths = (nrow(HumanIndxBetween0.35to0.549_49_96mnths)/countryTotal_0.35to0.549_49_96mnths)*100

HumanIndxBetween0.35to0.549_GreaterThan97 <- datFinal[which((datFinal$HumanIndex >= 0.35 
    & datFinal$HumanIndex <= 0.549)
	& (datFinal$Sanction_months >= 97)),]
nrow(HumanIndxBetween0.35to0.549_GreaterThan97)
countryTotal_0.35to0.549_GreaterThan97 = 0
uniqCount <- unique(HumanIndxBetween0.35to0.549_GreaterThan97$Nationality); 
country <- c(); year <- c(); Participants <- c()
for(ii in 1:length(uniqCount)){
    dat_HIBetween0.35to0.549_GreaterThan97 <- HumanIndxBetween0.35to0.549_GreaterThan97[which(HumanIndxBetween0.35to0.549_GreaterThan97$Nationality == uniqCount[ii]),]
	uniqyr <- unique(dat_HIBetween0.35to0.549_GreaterThan97$NearestOlympicYear)
	if(length(uniqyr) > 1){
	    for(jj in 1:length(uniqyr)){
		    dat_HIBetween0.35to0.549_GreaterThan97Y <- dat_HIBetween0.35to0.549_GreaterThan97[which(dat_HIBetween0.35to0.549_GreaterThan97$NearestOlympicYear == uniqyr[jj]),]
			country <- c(country, uniqCount[ii]); 
			year <- c(year, uniqyr[jj]); 
			Participants <- c(Participants, unique(dat_HIBetween0.35to0.549_GreaterThan97Y$TotalParticipant))
		    countryTotal_0.35to0.549_GreaterThan97 = countryTotal_0.35to0.549_GreaterThan97 + unique(dat_HIBetween0.35to0.549_GreaterThan97Y$TotalParticipant)
        }
	}else{
	    dat_HIBetween0.35to0.549_GreaterThan97Y <- dat_HIBetween0.35to0.549_GreaterThan97[which(dat_HIBetween0.35to0.549_GreaterThan97$NearestOlympicYear == uniqyr),]
		country <- c(country, uniqCount[ii]); 
		year <- c(year, uniqyr); 
		Participants <- c(Participants, unique(dat_HIBetween0.35to0.549_GreaterThan97Y$TotalParticipant))
		countryTotal_0.35to0.549_GreaterThan97 = countryTotal_0.35to0.549_GreaterThan97 + unique(dat_HIBetween0.35to0.549_GreaterThan97Y$TotalParticipant)
	}
}
percHI_0.35to0.549_GreaterThan97 = (nrow(HumanIndxBetween0.35to0.549_GreaterThan97)/countryTotal_0.35to0.549_GreaterThan97)*100

#HumanIndxBetween0.55to0.699 <- datFinal[which(datFinal$HumanIndex >= 0.55 & datFinal$HumanIndex <= 0.699),]
#nrow(HumanIndxBetween0.55to0.699)
#0.55to0.699
HumanIndxBetween0.55to0.699_LessThan24mnths <- datFinal[which((datFinal$HumanIndex >= 0.55 & datFinal$HumanIndex <= 0.699)
	& datFinal$Sanction_months <= 24),]
nrow(HumanIndxBetween0.55to0.699_LessThan24mnths)
countryTotal_0.55to0.699_LessThan24mnths = 0
uniqCount <- unique(HumanIndxBetween0.55to0.699_LessThan24mnths$Nationality); 
country <- c(); year <- c(); Participants <- c()
for(ii in 1:length(uniqCount)){
    dat_HIBetween0.55to0.699_LessThan24mnths <- HumanIndxBetween0.55to0.699_LessThan24mnths[which(HumanIndxBetween0.55to0.699_LessThan24mnths$Nationality == uniqCount[ii]),]
	uniqyr <- unique(dat_HIBetween0.55to0.699_LessThan24mnths$NearestOlympicYear)
	if(length(uniqyr) > 1){
	    for(jj in 1:length(uniqyr)){
		    dat_HIBetween0.55to0.699_LessThan24mnthsY <- dat_HIBetween0.55to0.699_LessThan24mnths[which(dat_HIBetween0.55to0.699_LessThan24mnths$NearestOlympicYear == uniqyr[jj]),]
			country <- c(country, uniqCount[ii]); 
			year <- c(year, uniqyr[jj]); 
			Participants <- c(Participants, unique(dat_HIBetween0.55to0.699_LessThan24mnthsY$TotalParticipant))
		    countryTotal_0.55to0.699_LessThan24mnths = countryTotal_0.55to0.699_LessThan24mnths + unique(dat_HIBetween0.55to0.699_LessThan24mnthsY$TotalParticipant)
        }
	}else{
	    dat_HIBetween0.55to0.699_LessThan24mnthsY <- dat_HIBetween0.55to0.699_LessThan24mnths[which(dat_HIBetween0.55to0.699_LessThan24mnths$NearestOlympicYear == uniqyr),]
		country <- c(country, uniqCount[ii]); 
		year <- c(year, uniqyr); 
		Participants <- c(Participants, unique(dat_HIBetween0.55to0.699_LessThan24mnthsY$TotalParticipant))
		countryTotal_0.55to0.699_LessThan24mnths = countryTotal_0.55to0.699_LessThan24mnths + unique(dat_HIBetween0.55to0.699_LessThan24mnthsY$TotalParticipant)
	}
}
percHI_0.55to0.699_LessThan24mnths = (nrow(HumanIndxBetween0.55to0.699_LessThan24mnths)/countryTotal_0.55to0.699_LessThan24mnths)*100

HumanIndxBetween0.55to0.699_25_48mnths <- datFinal[which((datFinal$HumanIndex >= 0.55 & datFinal$HumanIndex <= 0.699)
	& (datFinal$Sanction_months >= 25 
	& datFinal$Sanction_months <= 48)),]
nrow(HumanIndxBetween0.55to0.699_25_48mnths)
countryTotal_0.55to0.699_25_48mnths = 0
uniqCount <- unique(HumanIndxBetween0.55to0.699_25_48mnths$Nationality); 
country <- c(); year <- c(); Participants <- c()
for(ii in 1:length(uniqCount)){
    dat_HIBetween0.55to0.699_25_48mnths <- HumanIndxBetween0.55to0.699_25_48mnths[which(HumanIndxBetween0.55to0.699_25_48mnths$Nationality == uniqCount[ii]),]
	uniqyr <- unique(dat_HIBetween0.55to0.699_25_48mnths$NearestOlympicYear)
	if(length(uniqyr) > 1){
	    for(jj in 1:length(uniqyr)){
		    dat_HIBetween0.55to0.699_25_48mnthsY <- dat_HIBetween0.55to0.699_25_48mnths[which(dat_HIBetween0.55to0.699_25_48mnths$NearestOlympicYear == uniqyr[jj]),]
			country <- c(country, uniqCount[ii]); 
			year <- c(year, uniqyr[jj]); 
			Participants <- c(Participants, unique(dat_HIBetween0.55to0.699_25_48mnthsY$TotalParticipant))
		    countryTotal_0.55to0.699_25_48mnths = countryTotal_0.55to0.699_25_48mnths + unique(dat_HIBetween0.55to0.699_25_48mnthsY$TotalParticipant)
        }
	}else{
	    dat_HIBetween0.55to0.699_25_48mnthsY <- dat_HIBetween0.55to0.699_25_48mnths[which(dat_HIBetween0.55to0.699_25_48mnths$NearestOlympicYear == uniqyr),]
		country <- c(country, uniqCount[ii]); 
		year <- c(year, uniqyr); 
		Participants <- c(Participants, unique(dat_HIBetween0.55to0.699_25_48mnthsY$TotalParticipant))
		countryTotal_0.55to0.699_25_48mnths = countryTotal_0.55to0.699_25_48mnths + unique(dat_HIBetween0.55to0.699_25_48mnthsY$TotalParticipant)
	}
}
percHI_0.55to0.699_25_48mnths = (nrow(HumanIndxBetween0.55to0.699_25_48mnths)/countryTotal_0.55to0.699_25_48mnths)*100


HumanIndxBetween0.55to0.699_49_96mnths <- datFinal[which((datFinal$HumanIndex >= 0.55 & datFinal$HumanIndex <= 0.699)
	& (datFinal$Sanction_months >= 49 
	& datFinal$Sanction_months <= 96)),]
nrow(HumanIndxBetween0.55to0.699_49_96mnths)
countryTotal_0.55to0.699_49_96mnths = 0
uniqCount <- unique(HumanIndxBetween0.55to0.699_49_96mnths$Nationality); 
country <- c(); year <- c(); Participants <- c()
for(ii in 1:length(uniqCount)){
    dat_HIBetween0.55to0.699_49_96mnths <- HumanIndxBetween0.55to0.699_49_96mnths[which(HumanIndxBetween0.55to0.699_49_96mnths$Nationality == uniqCount[ii]),]
	uniqyr <- unique(dat_HIBetween0.55to0.699_49_96mnths$NearestOlympicYear)
	if(length(uniqyr) > 1){
	    for(jj in 1:length(uniqyr)){
		    dat_HIBetween0.55to0.699_49_96mnthsY <- dat_HIBetween0.55to0.699_49_96mnths[which(dat_HIBetween0.55to0.699_49_96mnths$NearestOlympicYear == uniqyr[jj]),]
			country <- c(country, uniqCount[ii]); 
			year <- c(year, uniqyr[jj]); 
			Participants <- c(Participants, unique(dat_HIBetween0.55to0.699_49_96mnthsY$TotalParticipant))
		    countryTotal_0.55to0.699_49_96mnths = countryTotal_0.55to0.699_49_96mnths + unique(dat_HIBetween0.55to0.699_49_96mnthsY$TotalParticipant)
        }
	}else{
	    dat_HIBetween0.55to0.699_49_96mnthsY <- dat_HIBetween0.55to0.699_49_96mnths[which(dat_HIBetween0.55to0.699_49_96mnths$NearestOlympicYear == uniqyr),]
		country <- c(country, uniqCount[ii]); 
		year <- c(year, uniqyr); 
		Participants <- c(Participants, unique(dat_HIBetween0.55to0.699_49_96mnthsY$TotalParticipant))
		countryTotal_0.55to0.699_49_96mnths = countryTotal_0.55to0.699_49_96mnths + unique(dat_HIBetween0.55to0.699_49_96mnthsY$TotalParticipant)
	}
}
percHI_0.55to0.699_49_96mnths = (nrow(HumanIndxBetween0.55to0.699_49_96mnths)/countryTotal_0.55to0.699_49_96mnths)*100

HumanIndxBetween0.55to0.699_GreaterThan97 <- datFinal[which((datFinal$HumanIndex >= 0.55 & datFinal$HumanIndex <= 0.699)
	& (datFinal$Sanction_months >= 97)),]
nrow(HumanIndxBetween0.55to0.699_GreaterThan97)
countryTotal_0.55to0.699_GreaterThan97 = 0
uniqCount <- unique(HumanIndxBetween0.55to0.699_GreaterThan97$Nationality); 
country <- c(); year <- c(); Participants <- c()
for(ii in 1:length(uniqCount)){
    dat_HIBetween0.55to0.699_GreaterThan97 <- HumanIndxBetween0.55to0.699_GreaterThan97[which(HumanIndxBetween0.55to0.699_GreaterThan97$Nationality == uniqCount[ii]),]
	uniqyr <- unique(dat_HIBetween0.55to0.699_GreaterThan97$NearestOlympicYear)
	if(length(uniqyr) > 1){
	    for(jj in 1:length(uniqyr)){
		    dat_HIBetween0.55to0.699_GreaterThan97Y <- dat_HIBetween0.55to0.699_GreaterThan97[which(dat_HIBetween0.55to0.699_GreaterThan97$NearestOlympicYear == uniqyr[jj]),]
			country <- c(country, uniqCount[ii]); 
			year <- c(year, uniqyr[jj]); 
			Participants <- c(Participants, unique(dat_HIBetween0.55to0.699_GreaterThan97Y$TotalParticipant))
		    countryTotal_0.55to0.699_GreaterThan97 = countryTotal_0.55to0.699_GreaterThan97 + unique(dat_HIBetween0.55to0.699_GreaterThan97Y$TotalParticipant)
        }
	}else{
	    dat_HIBetween0.55to0.699_GreaterThan97Y <- dat_HIBetween0.55to0.699_GreaterThan97[which(dat_HIBetween0.55to0.699_GreaterThan97$NearestOlympicYear == uniqyr),]
		country <- c(country, uniqCount[ii]); 
		year <- c(year, uniqyr); 
		Participants <- c(Participants, unique(dat_HIBetween0.55to0.699_GreaterThan97Y$TotalParticipant))
		countryTotal_0.55to0.699_GreaterThan97 = countryTotal_0.55to0.699_GreaterThan97 + unique(dat_HIBetween0.55to0.699_GreaterThan97Y$TotalParticipant)
	}
}
percHI_0.55to0.699_GreaterThan97 = (nrow(HumanIndxBetween0.55to0.699_GreaterThan97)/countryTotal_0.55to0.699_GreaterThan97)*100

HumanIndxBetween0.7to0.799 <- datFinal[which(datFinal$HumanIndex >= 0.7 & datFinal$HumanIndex <= 0.799),]
nrow(HumanIndxBetween0.7to0.799)
#0.7to0.799
HumanIndxBetween0.7to0.799_LessThan24mnths <- datFinal[which((datFinal$HumanIndex >= 0.7 & datFinal$HumanIndex <= 0.799)
	& datFinal$Sanction_months <= 24),]
nrow(HumanIndxBetween0.7to0.799_LessThan24mnths)
countryTotal_0.7to0.799_LessThan24mnths = 0
uniqCount <- unique(HumanIndxBetween0.7to0.799_LessThan24mnths$Nationality); 
country <- c(); year <- c(); Participants <- c()
for(ii in 1:length(uniqCount)){
    dat_HIBetween0.7to0.799_LessThan24mnths <- HumanIndxBetween0.7to0.799_LessThan24mnths[which(HumanIndxBetween0.7to0.799_LessThan24mnths$Nationality == uniqCount[ii]),]
	uniqyr <- unique(dat_HIBetween0.7to0.799_LessThan24mnths$NearestOlympicYear)
	if(length(uniqyr) > 1){
	    for(jj in 1:length(uniqyr)){
		    dat_HIBetween0.7to0.799_LessThan24mnthsY <- dat_HIBetween0.7to0.799_LessThan24mnths[which(dat_HIBetween0.7to0.799_LessThan24mnths$NearestOlympicYear == uniqyr[jj]),]
			country <- c(country, uniqCount[ii]); 
			year <- c(year, uniqyr[jj]); 
			Participants <- c(Participants, unique(dat_HIBetween0.7to0.799_LessThan24mnthsY$TotalParticipant))
		    countryTotal_0.7to0.799_LessThan24mnths = countryTotal_0.7to0.799_LessThan24mnths + unique(dat_HIBetween0.7to0.799_LessThan24mnthsY$TotalParticipant)
        }
	}else{
	    dat_HIBetween0.7to0.799_LessThan24mnthsY <- dat_HIBetween0.7to0.799_LessThan24mnths[which(dat_HIBetween0.7to0.799_LessThan24mnths$NearestOlympicYear == uniqyr),]
		country <- c(country, uniqCount[ii]); 
		year <- c(year, uniqyr); 
		Participants <- c(Participants, unique(dat_HIBetween0.7to0.799_LessThan24mnthsY$TotalParticipant))
		countryTotal_0.7to0.799_LessThan24mnths = countryTotal_0.7to0.799_LessThan24mnths + unique(dat_HIBetween0.7to0.799_LessThan24mnthsY$TotalParticipant)
	}
}
percHI_0.7to0.799_LessThan24mnths = (nrow(HumanIndxBetween0.7to0.799_LessThan24mnths)/countryTotal_0.7to0.799_LessThan24mnths)*100

HumanIndxBetween0.7to0.799_25_48mnths <- datFinal[which((datFinal$HumanIndex >= 0.7 & datFinal$HumanIndex <= 0.799)
	& (datFinal$Sanction_months >= 25 
	& datFinal$Sanction_months <= 48)),]
nrow(HumanIndxBetween0.7to0.799_25_48mnths)
countryTotal_0.7to0.799_25_48mnths = 0
uniqCount <- unique(HumanIndxBetween0.7to0.799_25_48mnths$Nationality); 
country <- c(); year <- c(); Participants <- c()
for(ii in 1:length(uniqCount)){
    dat_HIBetween0.7to0.799_25_48mnths <- HumanIndxBetween0.7to0.799_25_48mnths[which(HumanIndxBetween0.7to0.799_25_48mnths$Nationality == uniqCount[ii]),]
	uniqyr <- unique(dat_HIBetween0.7to0.799_25_48mnths$NearestOlympicYear)
	if(length(uniqyr) > 1){
	    for(jj in 1:length(uniqyr)){
		    dat_HIBetween0.7to0.799_25_48mnthsY <- dat_HIBetween0.7to0.799_25_48mnths[which(dat_HIBetween0.7to0.799_25_48mnths$NearestOlympicYear == uniqyr[jj]),]
			country <- c(country, uniqCount[ii]); 
			year <- c(year, uniqyr[jj]); 
			Participants <- c(Participants, unique(dat_HIBetween0.7to0.799_25_48mnthsY$TotalParticipant))
		    countryTotal_0.7to0.799_25_48mnths = countryTotal_0.7to0.799_25_48mnths + unique(dat_HIBetween0.7to0.799_25_48mnthsY$TotalParticipant)
        }
	}else{
	    dat_HIBetween0.7to0.799_25_48mnthsY <- dat_HIBetween0.7to0.799_25_48mnths[which(dat_HIBetween0.7to0.799_25_48mnths$NearestOlympicYear == uniqyr),]
		country <- c(country, uniqCount[ii]); 
		year <- c(year, uniqyr); 
		Participants <- c(Participants, unique(dat_HIBetween0.7to0.799_25_48mnthsY$TotalParticipant))
		countryTotal_0.7to0.799_25_48mnths = countryTotal_0.7to0.799_25_48mnths + unique(dat_HIBetween0.7to0.799_25_48mnthsY$TotalParticipant)
	}
}
percHI_0.7to0.799_25_48mnths = (nrow(HumanIndxBetween0.7to0.799_25_48mnths)/countryTotal_0.7to0.799_25_48mnths)*100


HumanIndxBetween0.7to0.799_49_96mnths <- datFinal[which((datFinal$HumanIndex >= 0.7 & datFinal$HumanIndex <= 0.799)
	& (datFinal$Sanction_months >= 49 
	& datFinal$Sanction_months <= 96)),]
nrow(HumanIndxBetween0.7to0.799_49_96mnths)
countryTotal_0.7to0.799_49_96mnths = 0
uniqCount <- unique(HumanIndxBetween0.7to0.799_49_96mnths$Nationality); 
country <- c(); year <- c(); Participants <- c()
for(ii in 1:length(uniqCount)){
    dat_HIBetween0.7to0.799_49_96mnths <- HumanIndxBetween0.7to0.799_49_96mnths[which(HumanIndxBetween0.7to0.799_49_96mnths$Nationality == uniqCount[ii]),]
	uniqyr <- unique(dat_HIBetween0.7to0.799_49_96mnths$NearestOlympicYear)
	if(length(uniqyr) > 1){
	    for(jj in 1:length(uniqyr)){
		    dat_HIBetween0.7to0.799_49_96mnthsY <- dat_HIBetween0.7to0.799_49_96mnths[which(dat_HIBetween0.7to0.799_49_96mnths$NearestOlympicYear == uniqyr[jj]),]
			country <- c(country, uniqCount[ii]); 
			year <- c(year, uniqyr[jj]); 
			Participants <- c(Participants, unique(dat_HIBetween0.7to0.799_49_96mnthsY$TotalParticipant))
		    countryTotal_0.7to0.799_49_96mnths = countryTotal_0.7to0.799_49_96mnths + unique(dat_HIBetween0.7to0.799_49_96mnthsY$TotalParticipant)
        }
	}else{
	    dat_HIBetween0.7to0.799_49_96mnthsY <- dat_HIBetween0.7to0.799_49_96mnths[which(dat_HIBetween0.7to0.799_49_96mnths$NearestOlympicYear == uniqyr),]
		country <- c(country, uniqCount[ii]); 
		year <- c(year, uniqyr); 
		Participants <- c(Participants, unique(dat_HIBetween0.7to0.799_49_96mnthsY$TotalParticipant))
		countryTotal_0.7to0.799_49_96mnths = countryTotal_0.7to0.799_49_96mnths + unique(dat_HIBetween0.7to0.799_49_96mnthsY$TotalParticipant)
	}
}
percHI_0.7to0.799_49_96mnths = (nrow(HumanIndxBetween0.7to0.799_49_96mnths)/countryTotal_0.7to0.799_49_96mnths)*100

HumanIndxBetween0.7to0.799_GreaterThan97 <- datFinal[which((datFinal$HumanIndex >= 0.7 & datFinal$HumanIndex <= 0.799)
	& (datFinal$Sanction_months >= 97)),]
nrow(HumanIndxBetween0.7to0.799_GreaterThan97)
countryTotal_0.7to0.799_GreaterThan97 = 0
uniqCount <- unique(HumanIndxBetween0.7to0.799_GreaterThan97$Nationality); 
country <- c(); year <- c(); Participants <- c()
for(ii in 1:length(uniqCount)){
    dat_HIBetween0.7to0.799_GreaterThan97 <- HumanIndxBetween0.7to0.799_GreaterThan97[which(HumanIndxBetween0.7to0.799_GreaterThan97$Nationality == uniqCount[ii]),]
	uniqyr <- unique(dat_HIBetween0.7to0.799_GreaterThan97$NearestOlympicYear)
	if(length(uniqyr) > 1){
	    for(jj in 1:length(uniqyr)){
		    dat_HIBetween0.7to0.799_GreaterThan97Y <- dat_HIBetween0.7to0.799_GreaterThan97[which(dat_HIBetween0.7to0.799_GreaterThan97$NearestOlympicYear == uniqyr[jj]),]
			country <- c(country, uniqCount[ii]); 
			year <- c(year, uniqyr[jj]); 
			Participants <- c(Participants, unique(dat_HIBetween0.7to0.799_GreaterThan97Y$TotalParticipant))
		    countryTotal_0.7to0.799_GreaterThan97 = countryTotal_0.7to0.799_GreaterThan97 + unique(dat_HIBetween0.7to0.799_GreaterThan97Y$TotalParticipant)
        }
	}else{
	    dat_HIBetween0.7to0.799_GreaterThan97Y <- dat_HIBetween0.7to0.799_GreaterThan97[which(dat_HIBetween0.7to0.799_GreaterThan97$NearestOlympicYear == uniqyr),]
		country <- c(country, uniqCount[ii]); 
		year <- c(year, uniqyr); 
		Participants <- c(Participants, unique(dat_HIBetween0.7to0.799_GreaterThan97Y$TotalParticipant))
		countryTotal_0.7to0.799_GreaterThan97 = countryTotal_0.7to0.799_GreaterThan97 + unique(dat_HIBetween0.7to0.799_GreaterThan97Y$TotalParticipant)
	}
}
percHI_0.7to0.799_GreaterThan97 = (nrow(HumanIndxBetween0.7to0.799_GreaterThan97)/countryTotal_0.7to0.799_GreaterThan97)*100

'HumanIndxBetween0.8to1 <- datFinal[which(datFinal$HumanIndex >= 0.8 & datFinal$HumanIndex <= 1),]
nrow(HumanIndxBetween0.8to1)
'
HumanIndxBetween0.8to1_LessThan24mnths <- datFinal[which((datFinal$HumanIndex >= 0.8 & datFinal$HumanIndex <= 1)
	& datFinal$Sanction_months <= 24),]
nrow(HumanIndxBetween0.8to1_LessThan24mnths)
countryTotal_0.8to1_LessThan24mnths = 0
uniqCount <- unique(HumanIndxBetween0.8to1_LessThan24mnths$Nationality); 
country <- c(); year <- c(); Participants <- c()
for(ii in 1:length(uniqCount)){
    dat_HIBetween0.8to1_LessThan24mnths <- HumanIndxBetween0.8to1_LessThan24mnths[which(HumanIndxBetween0.8to1_LessThan24mnths$Nationality == uniqCount[ii]),]
	uniqyr <- unique(dat_HIBetween0.8to1_LessThan24mnths$NearestOlympicYear)
	if(length(uniqyr) > 1){
	    for(jj in 1:length(uniqyr)){
		    dat_HIBetween0.8to1_LessThan24mnthsY <- dat_HIBetween0.8to1_LessThan24mnths[which(dat_HIBetween0.8to1_LessThan24mnths$NearestOlympicYear == uniqyr[jj]),]
			country <- c(country, uniqCount[ii]); 
			year <- c(year, uniqyr[jj]); 
			Participants <- c(Participants, unique(dat_HIBetween0.8to1_LessThan24mnthsY$TotalParticipant))
		    countryTotal_0.8to1_LessThan24mnths = countryTotal_0.8to1_LessThan24mnths + unique(dat_HIBetween0.8to1_LessThan24mnthsY$TotalParticipant)
        }
	}else{
	    dat_HIBetween0.8to1_LessThan24mnthsY <- dat_HIBetween0.8to1_LessThan24mnths[which(dat_HIBetween0.8to1_LessThan24mnths$NearestOlympicYear == uniqyr),]
		country <- c(country, uniqCount[ii]); 
		year <- c(year, uniqyr); 
		Participants <- c(Participants, unique(dat_HIBetween0.8to1_LessThan24mnthsY$TotalParticipant))
		countryTotal_0.8to1_LessThan24mnths = countryTotal_0.8to1_LessThan24mnths + unique(dat_HIBetween0.8to1_LessThan24mnthsY$TotalParticipant)
	}
}
percHI_0.8to1_LessThan24mnths = (nrow(HumanIndxBetween0.8to1_LessThan24mnths)/countryTotal_0.8to1_LessThan24mnths)*100

HumanIndxBetween0.8to1_25_48mnths <- datFinal[which((datFinal$HumanIndex >= 0.8 & datFinal$HumanIndex <= 1)
	& (datFinal$Sanction_months >= 25 
	& datFinal$Sanction_months <= 48)),]
nrow(HumanIndxBetween0.8to1_25_48mnths)
countryTotal_0.8to1_25_48mnths = 0
uniqCount <- unique(HumanIndxBetween0.8to1_25_48mnths$Nationality); 
country <- c(); year <- c(); Participants <- c()
for(ii in 1:length(uniqCount)){
    dat_HIBetween0.8to1_25_48mnths <- HumanIndxBetween0.8to1_25_48mnths[which(HumanIndxBetween0.8to1_25_48mnths$Nationality == uniqCount[ii]),]
	uniqyr <- unique(dat_HIBetween0.8to1_25_48mnths$NearestOlympicYear)
	if(length(uniqyr) > 1){
	    for(jj in 1:length(uniqyr)){
		    dat_HIBetween0.8to1_25_48mnthsY <- dat_HIBetween0.8to1_25_48mnths[which(dat_HIBetween0.8to1_25_48mnths$NearestOlympicYear == uniqyr[jj]),]
			country <- c(country, uniqCount[ii]); 
			year <- c(year, uniqyr[jj]); 
			Participants <- c(Participants, unique(dat_HIBetween0.8to1_25_48mnthsY$Male))
		    countryTotal_0.8to1_25_48mnths = countryTotal_0.8to1_25_48mnths + unique(dat_HIBetween0.8to1_25_48mnthsY$Male)
        }
	}else{
	    dat_HIBetween0.8to1_25_48mnthsY <- dat_HIBetween0.8to1_25_48mnths[which(dat_HIBetween0.8to1_25_48mnths$NearestOlympicYear == uniqyr),]
		country <- c(country, uniqCount[ii]); 
		year <- c(year, uniqyr); 
		Participants <- c(Participants, unique(dat_HIBetween0.8to1_25_48mnthsY$Male))
		countryTotal_0.8to1_25_48mnths = countryTotal_0.8to1_25_48mnths + unique(dat_HIBetween0.8to1_25_48mnthsY$Male)
	}
}
percHI_0.8to1_25_48mnths = (nrow(HumanIndxBetween0.8to1_25_48mnths)/countryTotal_0.8to1_25_48mnths)*100


HumanIndxBetween0.8to1_49_96mnths <- datFinal[which((datFinal$HumanIndex >= 0.8 & datFinal$HumanIndex <= 1)
	& (datFinal$Sanction_months >= 49 
	& datFinal$Sanction_months <= 96)),]
nrow(HumanIndxBetween0.8to1_49_96mnths)
countryTotal_0.8to1_49_96mnths = 0
uniqCount <- unique(HumanIndxBetween0.8to1_49_96mnths$Nationality); 
country <- c(); year <- c(); Participants <- c()
for(ii in 1:length(uniqCount)){
    dat_HIBetween0.8to1_49_96mnths <- HumanIndxBetween0.8to1_49_96mnths[which(HumanIndxBetween0.8to1_49_96mnths$Nationality == uniqCount[ii]),]
	uniqyr <- unique(dat_HIBetween0.8to1_49_96mnths$NearestOlympicYear)
	if(length(uniqyr) > 1){
	    for(jj in 1:length(uniqyr)){
		    dat_HIBetween0.8to1_49_96mnthsY <- dat_HIBetween0.8to1_49_96mnths[which(dat_HIBetween0.8to1_49_96mnths$NearestOlympicYear == uniqyr[jj]),]
			country <- c(country, uniqCount[ii]); 
			year <- c(year, uniqyr[jj]); 
			Participants <- c(Participants, unique(dat_HIBetween0.8to1_49_96mnthsY$Male))
		    countryTotal_0.8to1_49_96mnths = countryTotal_0.8to1_49_96mnths + unique(dat_HIBetween0.8to1_49_96mnthsY$Male)
        }
	}else{
	    dat_HIBetween0.8to1_49_96mnthsY <- dat_HIBetween0.8to1_49_96mnths[which(dat_HIBetween0.8to1_49_96mnths$NearestOlympicYear == uniqyr),]
		country <- c(country, uniqCount[ii]); 
		year <- c(year, uniqyr); 
		Participants <- c(Participants, unique(dat_HIBetween0.8to1_49_96mnthsY$Male))
		countryTotal_0.8to1_49_96mnths = countryTotal_0.8to1_49_96mnths + unique(dat_HIBetween0.8to1_49_96mnthsY$Male)
	}
}
percHI_0.8to1_49_96mnths = (nrow(HumanIndxBetween0.8to1_49_96mnths)/countryTotal_0.8to1_49_96mnths)*100

HumanIndxBetween0.8to1_GreaterThan97 <- datFinal[which((datFinal$HumanIndex >= 0.8 & datFinal$HumanIndex <= 1)
	& (datFinal$Sanction_months >= 97)),]
nrow(HumanIndxBetween0.8to1_GreaterThan97)
countryTotal_0.8to1_GreaterThan97 = 0
uniqCount <- unique(HumanIndxBetween0.8to1_GreaterThan97$Nationality); 
country <- c(); year <- c(); Participants <- c()
for(ii in 1:length(uniqCount)){
    dat_HIBetween0.8to1_GreaterThan97 <- HumanIndxBetween0.8to1_GreaterThan97[which(HumanIndxBetween0.8to1_GreaterThan97$Nationality == uniqCount[ii]),]
	uniqyr <- unique(dat_HIBetween0.8to1_GreaterThan97$NearestOlympicYear)
	if(length(uniqyr) > 1){
	    for(jj in 1:length(uniqyr)){
		    dat_HIBetween0.8to1_GreaterThan97Y <- dat_HIBetween0.8to1_GreaterThan97[which(dat_HIBetween0.8to1_GreaterThan97$NearestOlympicYear == uniqyr[jj]),]
			country <- c(country, uniqCount[ii]); 
			year <- c(year, uniqyr[jj]); 
			Participants <- c(Participants, unique(dat_HIBetween0.8to1_GreaterThan97Y$Male))
		    countryTotal_0.8to1_GreaterThan97 = countryTotal_0.8to1_GreaterThan97 + unique(dat_HIBetween0.8to1_GreaterThan97Y$Male)
        }
	}else{
	    dat_HIBetween0.8to1_GreaterThan97Y <- dat_HIBetween0.8to1_GreaterThan97[which(dat_HIBetween0.8to1_GreaterThan97$NearestOlympicYear == uniqyr),]
		country <- c(country, uniqCount[ii]); 
		year <- c(year, uniqyr); 
		Participants <- c(Participants, unique(dat_HIBetween0.8to1_GreaterThan97Y$Male))
		countryTotal_0.8to1_GreaterThan97 = countryTotal_0.8to1_GreaterThan97 + unique(dat_HIBetween0.8to1_GreaterThan97Y$Male)
	}
}
percHI_0.8to1_GreaterThan97 = (nrow(HumanIndxBetween0.8to1_GreaterThan97)/countryTotal_0.8to1_GreaterThan97)*100
datHI_SP<- data.frame(HumanIndex = c(
            "HumanIndxBetween0.35to0.549",
			"HumanIndxBetween0.35to0.549",
			"HumanIndxBetween0.35to0.549",
			"HumanIndxBetween0.35to0.549",
			"HumanIndxBetween0.55to0.699",
			"HumanIndxBetween0.55to0.699",
			"HumanIndxBetween0.55to0.699",
			"HumanIndxBetween0.55to0.699",
			"HumanIndxBetween0.70to0.799",
			"HumanIndxBetween0.70to0.799",
			"HumanIndxBetween0.70to0.799",
			"HumanIndxBetween0.70to0.799",
			"HumanIndxBetween0.80to1",
			"HumanIndxBetween0.80to1",
			"HumanIndxBetween0.80to1",
			"HumanIndxBetween0.80to1"),
        NumberofAthletes = c(
	        nrow(HumanIndxBetween0.35to0.549_LessThan24mnths),
            nrow(HumanIndxBetween0.35to0.549_25_48mnths),			
	        nrow(HumanIndxBetween0.35to0.549_49_96mnths),
			nrow(HumanIndxBetween0.35to0.549_GreaterThan97),
			nrow(HumanIndxBetween0.55to0.699_LessThan24mnths),
            nrow(HumanIndxBetween0.55to0.699_25_48mnths),			
	        nrow(HumanIndxBetween0.55to0.699_49_96mnths),
			nrow(HumanIndxBetween0.55to0.699_GreaterThan97),
			nrow(HumanIndxBetween0.7to0.799_LessThan24mnths),
            nrow(HumanIndxBetween0.7to0.799_25_48mnths),			
	        nrow(HumanIndxBetween0.7to0.799_49_96mnths),
			nrow(HumanIndxBetween0.7to0.799_GreaterThan97),
			nrow(HumanIndxBetween0.8to1_LessThan24mnths),
            nrow(HumanIndxBetween0.8to1_25_48mnths),			
	        nrow(HumanIndxBetween0.8to1_49_96mnths),
			nrow(HumanIndxBetween0.8to1_GreaterThan97)),
	    TotalAthletes = c(countryTotal_0.35to0.549_LessThan24mnths,
		    countryTotal_0.35to0.549_25_48mnths,
			countryTotal_0.35to0.549_49_96mnths,
			countryTotal_0.35to0.549_GreaterThan97,
			countryTotal_0.55to0.699_LessThan24mnths,
		    countryTotal_0.55to0.699_25_48mnths,
			countryTotal_0.55to0.699_49_96mnths,
			countryTotal_0.55to0.699_GreaterThan97,
			countryTotal_0.7to0.799_LessThan24mnths,
		    countryTotal_0.7to0.799_25_48mnths,
			countryTotal_0.7to0.799_49_96mnths,
			countryTotal_0.7to0.799_GreaterThan97,
			countryTotal_0.8to1_LessThan24mnths,
			countryTotal_0.8to1_25_48mnths,
			countryTotal_0.8to1_49_96mnths,
			countryTotal_0.8to1_GreaterThan97),
        SanctionPercent = c(percHI_0.35to0.549_LessThan24mnths,
		    percHI_0.35to0.549_25_48mnths,
			percHI_0.35to0.549_49_96mnths,
			percHI_0.35to0.549_GreaterThan97,
			percHI_0.55to0.699_LessThan24mnths,
		    percHI_0.55to0.699_25_48mnths,
			percHI_0.55to0.699_49_96mnths,
			percHI_0.55to0.699_GreaterThan97,
			percHI_0.7to0.799_LessThan24mnths,
		    percHI_0.7to0.799_25_48mnths,
			percHI_0.7to0.799_49_96mnths,
			percHI_0.7to0.799_GreaterThan97,
			percHI_0.8to1_LessThan24mnths,
		    percHI_0.8to1_25_48mnths,
			percHI_0.8to1_49_96mnths,
			percHI_0.8to1_GreaterThan97),
		SanctionPeriod = c("LessThan24mnths",
		    "25_48mnths",
			"49_96mnths",
			"GreaterThan97",
			"LessThan24mnths",
		    "25_48mnths",
			"49_96mnths",
			"GreaterThan97",
			"LessThan24mnths",
		    "25_48mnths",
			"49_96mnths",
			"GreaterThan97",
			"LessThan24mnths",
		    "25_48mnths",
			"49_96mnths",
			"GreaterThan97"))
write.csv(datHI_SP, file.path(outdir,"HumanIndex_PerSanctionPeriod.csv"), row.names = FALSE)

library(ggplot2)
p11 <- ggplot(data = datHI_SP, aes(x = factor(HumanIndex, levels=unique(HumanIndex)), y= SanctionPercent, fill = factor(SanctionPeriod, level = unique(SanctionPeriod)))) + geom_bar(stat = "identity", position="dodge")+ labs(x = "SanctionPeriod", y = "Percentage of Athletes", fill = "Global Classifications") + theme(legend.title = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.title.x = element_text(face = "bold", size = 50),axis.title.y = element_text(face = "bold", size = 50), axis.text.x = element_text(face="bold", size = 30, angle = 45, vjust = 0.5), axis.text.y = element_text(face="bold", size = 30), legend.text=element_text(face = "bold",size=25)) + scale_fill_manual(values=c("green","red", "blue", "yellow")) 
p11
 ###Human Index Sex
 
 
###HumanIndex-Sex
outdir = "Z:/Suro/AtheleteSanction/AtheleteSanction05262021/"
datFinal <- read.csv("Z:/Suro/AtheleteSanction/AtheleteSanction05262021/ModifiedAtlete_05292021.csv")
#HumanIndex
HumanIndxBetween0.35to0.549 <- datFinal[which(datFinal$HumanIndex >= 0.35 & datFinal$HumanIndex <= 0.549),]
nrow(HumanIndxBetween0.35to0.549)
#countryTotal_0.35to0.549 = 0
countryMale_0.35to0.549 = 0
countryFemale_0.35to0.549 = 0
countryTotal_0.35to0.549 = 0
totalpercountry <- c()
uniqCount <- unique(HumanIndxBetween0.35to0.549$Nationality); 
#country <- c(); year <- c(); Participants <- c()
country <- c(); year <- c(); MaleParticipants0.35to0.549 <- c(); FemaleParticipants0.35to0.549 <- c()
Participants <- c()
for(ii in 1:length(uniqCount)){
    dat_HIBetween0.35to0.549 <- HumanIndxBetween0.35to0.549[which(HumanIndxBetween0.35to0.549$Nationality == uniqCount[ii]),]
    uniqyr <- unique(dat_HIBetween0.35to0.549$NearestOlympicYear)
    if(length(uniqyr) > 1){
        for(jj in 1:length(uniqyr)){
            dat_HIBetween0.35to0.549Y <- dat_HIBetween0.35to0.549[which(dat_HIBetween0.35to0.549$NearestOlympicYear == uniqyr[jj]),]
            country <- c(country, uniqCount[ii]); 
            year <- c(year, uniqyr[jj]); 
            MaleParticipants0.35to0.549 <- c(MaleParticipants0.35to0.549, unique(dat_HIBetween0.35to0.549Y$Male))
            FemaleParticipants0.35to0.549 <- c(FemaleParticipants0.35to0.549, unique(dat_HIBetween0.35to0.549Y$Female))
            #Participants <- c(Participants, unique(dat_HIBetween0.35to0.549Y$TotalParticipant))
            countryMale_0.35to0.549 = countryMale_0.35to0.549+  unique(dat_HIBetween0.35to0.549Y$Male)
            countryFemale_0.35to0.549 = countryFemale_0.35to0.549 + unique(dat_HIBetween0.35to0.549Y$Female)
            countryTotal_0.35to0.549 = countryTotal_0.35to0.549 + unique(dat_HIBetween0.35to0.549Y$TotalParticipant)
        }
    }else{
        dat_HIBetween0.35to0.549Y <- dat_HIBetween0.35to0.549[which(dat_HIBetween0.35to0.549$NearestOlympicYear == uniqyr),]
        country <- c(country, uniqCount[ii]); 
        year <- c(year, uniqyr); 
        MaleParticipants0.35to0.549 <- c(MaleParticipants0.35to0.549, unique(dat_HIBetween0.35to0.549Y$Male))
        FemaleParticipants0.35to0.549 <- c(FemaleParticipants0.35to0.549, unique(dat_HIBetween0.35to0.549Y$Female))
        #Participants <- c(Participants, unique(dat_HIBetween0.35to0.549Y$TotalParticipant))
        countryMale_0.35to0.549 = countryMale_0.35to0.549+  unique(dat_HIBetween0.35to0.549Y$Male)
        countryFemale_0.35to0.549 = countryFemale_0.35to0.549 + unique(dat_HIBetween0.35to0.549Y$Female)
        countryTotal_0.35to0.549 = countryTotal_0.35to0.549 + unique(dat_HIBetween0.35to0.549Y$TotalParticipant)
    }
}
percHI_0.35to0.549_HI_Male = (countryMale_0.35to0.549/countryTotal_0.35to0.549)*100
percHI_0.35to0.549_HI_Female = (countryFemale_0.35to0.549/countryTotal_0.35to0.549)*100
#percHI_0.35to0.549_TotSanctionedPercent = (nrow(HumanIndxBetween0.35to0.549)/nrow(datFinal))*100
HumanIndxBetween0.55to0.699 <- datFinal[which(datFinal$HumanIndex >= 0.55 & datFinal$HumanIndex <= 0.699),]
nrow(HumanIndxBetween0.55to0.699)
#countryTotal_0.35to0.549 = 0
countryMale_0.55to0.699 = 0
countryFemale_0.55to0.699 = 0
countryTotal_0.55to0.699 = 0
totalpercountry <- c()
uniqCount <- unique(HumanIndxBetween0.55to0.699$Nationality); 
#country <- c(); year <- c(); Participants <- c()
country <- c(); year <- c(); MaleParticipants0.55to0.699 <- c(); FemaleParticipants0.55to0.699 <- c()
for(ii in 1:length(uniqCount)){
    dat_HIBetween0.55to0.699 <- HumanIndxBetween0.55to0.699[which(HumanIndxBetween0.55to0.699$Nationality == uniqCount[ii]),]
    uniqyr <- unique(dat_HIBetween0.55to0.699$NearestOlympicYear)
    if(length(uniqyr) > 1){
        for(jj in 1:length(uniqyr)){
            dat_HIBetween0.55to0.699Y <- dat_HIBetween0.55to0.699[which(dat_HIBetween0.55to0.699$NearestOlympicYear == uniqyr[jj]),]
            country <- c(country, uniqCount[ii]); 
            year <- c(year, uniqyr[jj]); 
            MaleParticipants0.55to0.699 <- c(MaleParticipants0.55to0.699, unique(dat_HIBetween0.55to0.699Y$Male))
            FemaleParticipants0.55to0.699 <- c(FemaleParticipants0.55to0.699, unique(dat_HIBetween0.55to0.699Y$Female))
            #Participants <- c(Participants, unique(dat_HIBetween0.55to0.699Y$TotalParticipant))
            countryMale_0.55to0.699 = countryMale_0.55to0.699+  unique(dat_HIBetween0.55to0.699Y$Male)
            countryFemale_0.55to0.699 = countryFemale_0.55to0.699 + unique(dat_HIBetween0.55to0.699Y$Female)
            countryTotal_0.55to0.699 = countryTotal_0.55to0.699 + unique(dat_HIBetween0.55to0.699Y$TotalParticipant)
        }
    }else{
        dat_HIBetween0.55to0.699Y <- dat_HIBetween0.55to0.699[which(dat_HIBetween0.55to0.699$NearestOlympicYear == uniqyr),]
        country <- c(country, uniqCount[ii]); 
        year <- c(year, uniqyr); 
        MaleParticipants0.55to0.699 <- c(MaleParticipants0.55to0.699, unique(dat_HIBetween0.55to0.699Y$Male))
        FemaleParticipants0.55to0.699 <- c(FemaleParticipants0.55to0.699, unique(dat_HIBetween0.55to0.699Y$Female))
        #Participants <- c(Participants, unique(dat_HIBetween0.55to0.699Y$TotalParticipant))
        countryMale_0.55to0.699 = countryMale_0.55to0.699+  unique(dat_HIBetween0.55to0.699Y$Male)
        countryFemale_0.55to0.699 = countryFemale_0.55to0.699 + unique(dat_HIBetween0.55to0.699Y$Female)
        countryTotal_0.55to0.699 = countryTotal_0.55to0.699 + unique(dat_HIBetween0.55to0.699Y$TotalParticipant)
    }
}
percHI_0.55to0.699_HI_Male = (countryMale_0.55to0.699/countryTotal_0.55to0.699)*100
percHI_0.55to0.699_HI_Female = (countryFemale_0.55to0.699/countryTotal_0.55to0.699)*100
HumanIndxBetween0.7to0.799 <- datFinal[which(datFinal$HumanIndex >= 0.7 & datFinal$HumanIndex <= 0.799),]
nrow(HumanIndxBetween0.7to0.799)
#countryTotal_0.7to0.799 = 0
countryMale_0.7to0.799 = 0
countryFemale_0.7to0.799 = 0
countryTotal_0.7to0.799 = 0
totalpercountry <- c()
uniqCount <- unique(HumanIndxBetween0.7to0.799$Nationality); 
#country <- c(); year <- c(); Participants <- c()
country <- c(); year <- c(); MaleParticipants0.7to0.799 <- c(); FemaleParticipants0.7to0.799 <- c()
for(ii in 1:length(uniqCount)){
    dat_HIBetween0.7to0.799 <- HumanIndxBetween0.7to0.799[which(HumanIndxBetween0.7to0.799$Nationality == uniqCount[ii]),]
    uniqyr <- unique(dat_HIBetween0.7to0.799$NearestOlympicYear)
    if(length(uniqyr) > 1){
        for(jj in 1:length(uniqyr)){
            dat_HIBetween0.7to0.799Y <- dat_HIBetween0.7to0.799[which(dat_HIBetween0.7to0.799$NearestOlympicYear == uniqyr[jj]),]
            country <- c(country, uniqCount[ii]); 
            year <- c(year, uniqyr[jj]); 
            MaleParticipants0.7to0.799 <- c(MaleParticipants0.7to0.799, unique(dat_HIBetween0.7to0.799Y$Male))
            FemaleParticipants0.7to0.799 <- c(FemaleParticipants0.7to0.799, unique(dat_HIBetween0.7to0.799Y$Female))
            #Participants <- c(Participants, unique(dat_HIBetween0.7to0.799Y$TotalParticipant))
            countryMale_0.7to0.799 = countryMale_0.7to0.799+  unique(dat_HIBetween0.7to0.799Y$Male)
            countryFemale_0.7to0.799 = countryFemale_0.7to0.799 + unique(dat_HIBetween0.7to0.799Y$Female)
            countryTotal_0.7to0.799 = countryTotal_0.7to0.799 + unique(dat_HIBetween0.7to0.799Y$TotalParticipant)
        }
    }else{
        dat_HIBetween0.7to0.799Y <- dat_HIBetween0.7to0.799[which(dat_HIBetween0.7to0.799$NearestOlympicYear == uniqyr),]
        country <- c(country, uniqCount[ii]); 
        year <- c(year, uniqyr); 
        MaleParticipants0.7to0.799 <- c(MaleParticipants0.7to0.799, unique(dat_HIBetween0.7to0.799Y$Male))
        FemaleParticipants0.7to0.799 <- c(FemaleParticipants0.7to0.799, unique(dat_HIBetween0.7to0.799Y$Female))
        #Participants <- c(Participants, unique(dat_HIBetween0.7to0.799Y$TotalParticipant))
        countryMale_0.7to0.799 = countryMale_0.7to0.799+  unique(dat_HIBetween0.7to0.799Y$Male)
        countryFemale_0.7to0.799 = countryFemale_0.7to0.799 + unique(dat_HIBetween0.7to0.799Y$Female)
        countryTotal_0.7to0.799 = countryTotal_0.7to0.799 + unique(dat_HIBetween0.7to0.799Y$TotalParticipant)
    }
}
percHI_0.7to0.799_HI_Male = (countryMale_0.7to0.799/countryTotal_0.7to0.799)*100
percHI_0.7to0.799_HI_Female = (countryFemale_0.7to0.799/countryTotal_0.7to0.799)*100
HumanIndxBetween0.8to1 <- datFinal[which(datFinal$HumanIndex >= 0.8 & datFinal$HumanIndex <= 1),]
nrow(HumanIndxBetween0.8to1)
#countryTotal_0.8to1 = 0
countryMale_0.8to1 = 0
countryFemale_0.8to1 = 0
countryTotal_0.8to1 = 0
totalpercountry <- c()
uniqCount <- unique(HumanIndxBetween0.8to1$Nationality); 
#country <- c(); year <- c(); Participants <- c()
country <- c(); year <- c(); MaleParticipants0.8to1 <- c(); FemaleParticipants0.8to1 <- c()
for(ii in 1:length(uniqCount)){
    dat_HIBetween0.8to1 <- HumanIndxBetween0.8to1[which(HumanIndxBetween0.8to1$Nationality == uniqCount[ii]),]
    uniqyr <- unique(dat_HIBetween0.8to1$NearestOlympicYear)
    if(length(uniqyr) > 1){
        for(jj in 1:length(uniqyr)){
            dat_HIBetween0.8to1Y <- dat_HIBetween0.8to1[which(dat_HIBetween0.8to1$NearestOlympicYear == uniqyr[jj]),]
            country <- c(country, uniqCount[ii]); 
            year <- c(year, uniqyr[jj]); 
            MaleParticipants0.8to1 <- c(MaleParticipants0.8to1, unique(dat_HIBetween0.8to1Y$Male))
            FemaleParticipants0.8to1 <- c(FemaleParticipants0.8to1, unique(dat_HIBetween0.8to1Y$Female))
            #Participants <- c(Participants, unique(dat_HIBetween0.8to1Y$TotalParticipant))
            countryMale_0.8to1 = countryMale_0.8to1+  unique(dat_HIBetween0.8to1Y$Male)
            countryFemale_0.8to1 = countryFemale_0.8to1 + unique(dat_HIBetween0.8to1Y$Female)
            countryTotal_0.8to1 = countryTotal_0.8to1 + unique(dat_HIBetween0.8to1Y$TotalParticipant)
        }
    }else{
        dat_HIBetween0.8to1Y <- dat_HIBetween0.8to1[which(dat_HIBetween0.8to1$NearestOlympicYear == uniqyr),]
        country <- c(country, uniqCount[ii]); 
        year <- c(year, uniqyr); 
        MaleParticipants0.8to1 <- c(MaleParticipants0.8to1, unique(dat_HIBetween0.8to1Y$Male))
        FemaleParticipants0.8to1 <- c(FemaleParticipants0.8to1, unique(dat_HIBetween0.8to1Y$Female))
        #Participants <- c(Participants, unique(dat_HIBetween0.8to1Y$TotalParticipant))
        countryMale_0.8to1 = countryMale_0.8to1+  unique(dat_HIBetween0.8to1Y$Male)
        countryFemale_0.8to1 = countryFemale_0.8to1 + unique(dat_HIBetween0.8to1Y$Female)
        countryTotal_0.8to1 = countryTotal_0.8to1 + unique(dat_HIBetween0.8to1Y$TotalParticipant)
    }
}
percHI_0.8to1_HI_Male = (countryMale_0.8to1/countryTotal_0.8to1)*100
percHI_0.8to1_HI_Female = (countryFemale_0.8to1/countryTotal_0.8to1)*100
datHI_Male_Female<- data.frame(HumanIndex = c(
    "HI_0.35to0.549",
    "HI_0.55to0.699", 
    "HI_0.7to0.799",
    "HI_0.8to1"),
    NumberofAthletes = c(countryTotal_0.35to0.549, countryTotal_0.55to0.699, countryTotal_0.7to0.799, countryTotal_0.8to1),
    TotalMaleAthletes = c(countryMale_0.35to0.549,
                          countryMale_0.55to0.699,
                          countryMale_0.7to0.799,
                          countryMale_0.8to1),
    TotalFemaleAthletes = c(countryFemale_0.35to0.549,
                            countryFemale_0.55to0.699,
                            countryFemale_0.7to0.799,
                            countryFemale_0.8to1),
    SanctionPercent_HI_Male = c(percHI_0.35to0.549_HI_Male,
                                percHI_0.55to0.699_HI_Male,
                                percHI_0.7to0.799_HI_Male,
                                percHI_0.8to1_HI_Male),
    SanctionPercent_HI_Male = c(percHI_0.35to0.549_HI_Female,
                                percHI_0.55to0.699_HI_Female,
                                percHI_0.7to0.799_HI_Female,
                                percHI_0.8to1_HI_Female))
write.csv(datHI_Male_Female, file.path(outdir,"humanIndexMale_Female_02192020.csv"), row.names = FALSE)
datHI_M_F<- data.frame(HumanIndex = c(
    "HI_0.35to0.549",
    "HI_0.35to0.549",
    "HI_0.55to0.699", 
    "HI_0.55to0.699", 
    "HI_0.7to0.799",
    "HI_0.7to0.799",
    "HI_0.8to1",
    "HI_0.8to1"),
    Sex =c("Male", "Female","Male", "Female","Male", "Female","Male", "Female"),
    TotalMaleAthletes = c(countryMale_0.35to0.549, countryFemale_0.35to0.549,
                          countryMale_0.55to0.699, countryFemale_0.55to0.699,
                          countryMale_0.7to0.799, countryFemale_0.7to0.799,
                          countryMale_0.8to1, countryFemale_0.8to1),
    SanctionPercent = c(percHI_0.35to0.549_HI_Male,percHI_0.35to0.549_HI_Female,
                        percHI_0.55to0.699_HI_Male,percHI_0.55to0.699_HI_Female,
                        percHI_0.7to0.799_HI_Male,percHI_0.7to0.799_HI_Female,
                        percHI_0.8to1_HI_Male, percHI_0.8to1_HI_Female)
)
write.csv(datHI_M_F, file.path(outdir,"humanIndexMale_Female_02192020_plotReady.csv"), row.names = FALSE)			
p10 <- ggplot(data = datHI_M_F, aes(x = HumanIndex, y= SanctionPercent, fill = factor(Sex, level = c("Male", "Female")))) + geom_bar(stat = "identity")+ labs(x = "HumanIndex", y = "% of Sanctioned Athletes") + theme(legend.title = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.title.x = element_text(face = "bold", size = 50),axis.title.y = element_text(face = "bold", size = 50), axis.text.x = element_text(face="bold", size = 30, angle = 45, hjust = 1), axis.text.y = element_text(face="bold", size = 30), legend.text=element_text(face = "bold",size=25)) +scale_fill_manual(values=c("green","hotpink")) 
p10


###0.35_0.549
HumanIndxBetween0.35to0.549_LessThan24mnths <- datFinal[which((datFinal$HumanIndex >= 0.35 
    & datFinal$HumanIndex <= 0.549)
	& datFinal$Sanction_months <= 24),]
nrow(HumanIndxBetween0.35to0.549_LessThan24mnths)
#countryTotal_0.35to0.549_LessThan24mnths = 0
countryMale_0.35to0.549_LessThan24mnths = 0
countryFemale_0.35to0.549_LessThan24mnths = 0
countryTotal_0.35to0.549_LessThan24mnths = 0
totalpercountry <- c()
uniqCount <- unique(HumanIndxBetween0.35to0.549_LessThan24mnths$Nationality); 
#country <- c(); year <- c(); Participants <- c()
country <- c(); year <- c(); MaleParticipants0.35to0.549_LessThan24mnths <- c(); FemaleParticipants0.35to0.549_LessThan24mnths <- c()
Participants <- c()
for(ii in 1:length(uniqCount)){
    dat_HIBetween0.35to0.549_LessThan24mnths <- HumanIndxBetween0.35to0.549_LessThan24mnths[which(HumanIndxBetween0.35to0.549_LessThan24mnths$Nationality == uniqCount[ii]),]
    uniqyr <- unique(dat_HIBetween0.35to0.549_LessThan24mnths$NearestOlympicYear)
    if(length(uniqyr) > 1){
        for(jj in 1:length(uniqyr)){
            dat_HIBetween0.35to0.549_LessThan24mnthsY <- dat_HIBetween0.35to0.549_LessThan24mnths[which(dat_HIBetween0.35to0.549_LessThan24mnths$NearestOlympicYear == uniqyr[jj]),]
            country <- c(country, uniqCount[ii]); 
            year <- c(year, uniqyr[jj]); 
            MaleParticipants0.35to0.549_LessThan24mnths <- c(MaleParticipants0.35to0.549_LessThan24mnths, unique(dat_HIBetween0.35to0.549_LessThan24mnthsY$Male))
            FemaleParticipants0.35to0.549_LessThan24mnths <- c(FemaleParticipants0.35to0.549_LessThan24mnths, unique(dat_HIBetween0.35to0.549_LessThan24mnthsY$Female))
            #Participants <- c(Participants, unique(dat_HIBetween0.35to0.549_LessThan24mnthsY$TotalParticipant))
            countryMale_0.35to0.549_LessThan24mnths = countryMale_0.35to0.549_LessThan24mnths+  unique(dat_HIBetween0.35to0.549_LessThan24mnthsY$Male)
            countryFemale_0.35to0.549_LessThan24mnths = countryFemale_0.35to0.549_LessThan24mnths + unique(dat_HIBetween0.35to0.549_LessThan24mnthsY$Female)
            countryTotal_0.35to0.549_LessThan24mnths = countryTotal_0.35to0.549_LessThan24mnths + unique(dat_HIBetween0.35to0.549_LessThan24mnthsY$TotalParticipant)
        }
    }else{
        dat_HIBetween0.35to0.549_LessThan24mnthsY <- dat_HIBetween0.35to0.549_LessThan24mnths[which(dat_HIBetween0.35to0.549_LessThan24mnths$NearestOlympicYear == uniqyr),]
        country <- c(country, uniqCount[ii]); 
        year <- c(year, uniqyr); 
        MaleParticipants0.35to0.549_LessThan24mnths <- c(MaleParticipants0.35to0.549_LessThan24mnths, unique(dat_HIBetween0.35to0.549_LessThan24mnthsY$Male))
        FemaleParticipants0.35to0.549_LessThan24mnths <- c(FemaleParticipants0.35to0.549_LessThan24mnths, unique(dat_HIBetween0.35to0.549_LessThan24mnthsY$Female))
        #Participants <- c(Participants, unique(dat_HIBetween0.35to0.549_LessThan24mnthsY$TotalParticipant))
        countryMale_0.35to0.549_LessThan24mnths = countryMale_0.35to0.549_LessThan24mnths+  unique(dat_HIBetween0.35to0.549_LessThan24mnthsY$Male)
        countryFemale_0.35to0.549_LessThan24mnths = countryFemale_0.35to0.549_LessThan24mnths + unique(dat_HIBetween0.35to0.549_LessThan24mnthsY$Female)
        countryTotal_0.35to0.549_LessThan24mnths = countryTotal_0.35to0.549_LessThan24mnths + unique(dat_HIBetween0.35to0.549_LessThan24mnthsY$TotalParticipant)
    }
}
percHI_0.35to0.549_LessThan24mnths_HI_Male = (countryMale_0.35to0.549_LessThan24mnths/countryTotal_0.35to0.549_LessThan24mnths)*100
percHI_0.35to0.549_LessThan24mnths_HI_Female = (countryFemale_0.35to0.549_LessThan24mnths/countryTotal_0.35to0.549_LessThan24mnths)*100

HumanIndxBetween0.35to0.549_25_48mnths <- datFinal[which((datFinal$HumanIndex >= 0.35 
    & datFinal$HumanIndex <= 0.549)
	& (datFinal$Sanction_months >= 25 
	& datFinal$Sanction_months <= 48)),]
nrow(HumanIndxBetween0.35to0.549_25_48mnths)
#countryTotal_0.35to0.549_25_48mnths = 0
countryMale_0.35to0.549_25_48mnths = 0
countryFemale_0.35to0.549_25_48mnths = 0
countryTotal_0.35to0.549_25_48mnths = 0
totalpercountry <- c()
uniqCount <- unique(HumanIndxBetween0.35to0.549_25_48mnths$Nationality); 
#country <- c(); year <- c(); Participants <- c()
country <- c(); year <- c(); MaleParticipants0.35to0.549_25_48mnths <- c(); FemaleParticipants0.35to0.549_25_48mnths <- c()
Participants <- c()
for(ii in 1:length(uniqCount)){
    dat_HIBetween0.35to0.549_25_48mnths <- HumanIndxBetween0.35to0.549_25_48mnths[which(HumanIndxBetween0.35to0.549_25_48mnths$Nationality == uniqCount[ii]),]
    uniqyr <- unique(dat_HIBetween0.35to0.549_25_48mnths$NearestOlympicYear)
    if(length(uniqyr) > 1){
        for(jj in 1:length(uniqyr)){
            dat_HIBetween0.35to0.549_25_48mnthsY <- dat_HIBetween0.35to0.549_25_48mnths[which(dat_HIBetween0.35to0.549_25_48mnths$NearestOlympicYear == uniqyr[jj]),]
            country <- c(country, uniqCount[ii]); 
            year <- c(year, uniqyr[jj]); 
            MaleParticipants0.35to0.549_25_48mnths <- c(MaleParticipants0.35to0.549_25_48mnths, unique(dat_HIBetween0.35to0.549_25_48mnthsY$Male))
            FemaleParticipants0.35to0.549_25_48mnths <- c(FemaleParticipants0.35to0.549_25_48mnths, unique(dat_HIBetween0.35to0.549_25_48mnthsY$Female))
            #Participants <- c(Participants, unique(dat_HIBetween0.35to0.549_25_48mnthsY$TotalParticipant))
            countryMale_0.35to0.549_25_48mnths = countryMale_0.35to0.549_25_48mnths+  unique(dat_HIBetween0.35to0.549_25_48mnthsY$Male)
            countryFemale_0.35to0.549_25_48mnths = countryFemale_0.35to0.549_25_48mnths + unique(dat_HIBetween0.35to0.549_25_48mnthsY$Female)
            countryTotal_0.35to0.549_25_48mnths = countryTotal_0.35to0.549_25_48mnths + unique(dat_HIBetween0.35to0.549_25_48mnthsY$TotalParticipant)
        }
    }else{
        dat_HIBetween0.35to0.549_25_48mnthsY <- dat_HIBetween0.35to0.549_25_48mnths[which(dat_HIBetween0.35to0.549_25_48mnths$NearestOlympicYear == uniqyr),]
        country <- c(country, uniqCount[ii]); 
        year <- c(year, uniqyr); 
        MaleParticipants0.35to0.549_25_48mnths <- c(MaleParticipants0.35to0.549_25_48mnths, unique(dat_HIBetween0.35to0.549_25_48mnthsY$Male))
        FemaleParticipants0.35to0.549_25_48mnths <- c(FemaleParticipants0.35to0.549_25_48mnths, unique(dat_HIBetween0.35to0.549_25_48mnthsY$Female))
        #Participants <- c(Participants, unique(dat_HIBetween0.35to0.549_25_48mnthsY$TotalParticipant))
        countryMale_0.35to0.549_25_48mnths = countryMale_0.35to0.549_25_48mnths+  unique(dat_HIBetween0.35to0.549_25_48mnthsY$Male)
        countryFemale_0.35to0.549_25_48mnths = countryFemale_0.35to0.549_25_48mnths + unique(dat_HIBetween0.35to0.549_25_48mnthsY$Female)
        countryTotal_0.35to0.549_25_48mnths = countryTotal_0.35to0.549_25_48mnths + unique(dat_HIBetween0.35to0.549_25_48mnthsY$TotalParticipant)
    }
}
percHI_0.35to0.549_25_48mnths_HI_Male = (countryMale_0.35to0.549_25_48mnths/countryTotal_0.35to0.549_25_48mnths)*100
percHI_0.35to0.549_25_48mnths_HI_Female = (countryFemale_0.35to0.549_25_48mnths/countryTotal_0.35to0.549_25_48mnths)*100


HumanIndxBetween0.35to0.549_49_96mnths <- datFinal[which((datFinal$HumanIndex >= 0.35 
    & datFinal$HumanIndex <= 0.549)
	& (datFinal$Sanction_months >= 49 
	& datFinal$Sanction_months <= 96)),]
nrow(HumanIndxBetween0.35to0.549_49_96mnths)
#countryTotal_0.35to0.549_49_96mnths = 0
countryMale_0.35to0.549_49_96mnths = 0
countryFemale_0.35to0.549_49_96mnths = 0
countryTotal_0.35to0.549_49_96mnths = 0
totalpercountry <- c()
uniqCount <- unique(HumanIndxBetween0.35to0.549_49_96mnths$Nationality); 
#country <- c(); year <- c(); Participants <- c()
country <- c(); year <- c(); MaleParticipants0.35to0.549_49_96mnths <- c(); FemaleParticipants0.35to0.549_49_96mnths <- c()
Participants <- c()
for(ii in 1:length(uniqCount)){
    dat_HIBetween0.35to0.549_49_96mnths <- HumanIndxBetween0.35to0.549_49_96mnths[which(HumanIndxBetween0.35to0.549_49_96mnths$Nationality == uniqCount[ii]),]
    uniqyr <- unique(dat_HIBetween0.35to0.549_49_96mnths$NearestOlympicYear)
    if(length(uniqyr) > 1){
        for(jj in 1:length(uniqyr)){
            dat_HIBetween0.35to0.549_49_96mnthsY <- dat_HIBetween0.35to0.549_49_96mnths[which(dat_HIBetween0.35to0.549_49_96mnths$NearestOlympicYear == uniqyr[jj]),]
            country <- c(country, uniqCount[ii]); 
            year <- c(year, uniqyr[jj]); 
            MaleParticipants0.35to0.549_49_96mnths <- c(MaleParticipants0.35to0.549_49_96mnths, unique(dat_HIBetween0.35to0.549_49_96mnthsY$Male))
            FemaleParticipants0.35to0.549_49_96mnths <- c(FemaleParticipants0.35to0.549_49_96mnths, unique(dat_HIBetween0.35to0.549_49_96mnthsY$Female))
            #Participants <- c(Participants, unique(dat_HIBetween0.35to0.549_49_96mnthsY$TotalParticipant))
            countryMale_0.35to0.549_49_96mnths = countryMale_0.35to0.549_49_96mnths+  unique(dat_HIBetween0.35to0.549_49_96mnthsY$Male)
            countryFemale_0.35to0.549_49_96mnths = countryFemale_0.35to0.549_49_96mnths + unique(dat_HIBetween0.35to0.549_49_96mnthsY$Female)
            countryTotal_0.35to0.549_49_96mnths = countryTotal_0.35to0.549_49_96mnths + unique(dat_HIBetween0.35to0.549_49_96mnthsY$TotalParticipant)
        }
    }else{
        dat_HIBetween0.35to0.549_49_96mnthsY <- dat_HIBetween0.35to0.549_49_96mnths[which(dat_HIBetween0.35to0.549_49_96mnths$NearestOlympicYear == uniqyr),]
        country <- c(country, uniqCount[ii]); 
        year <- c(year, uniqyr); 
        MaleParticipants0.35to0.549_49_96mnths <- c(MaleParticipants0.35to0.549_49_96mnths, unique(dat_HIBetween0.35to0.549_49_96mnthsY$Male))
        FemaleParticipants0.35to0.549_49_96mnths <- c(FemaleParticipants0.35to0.549_49_96mnths, unique(dat_HIBetween0.35to0.549_49_96mnthsY$Female))
        #Participants <- c(Participants, unique(dat_HIBetween0.35to0.549_49_96mnthsY$TotalParticipant))
        countryMale_0.35to0.549_49_96mnths = countryMale_0.35to0.549_49_96mnths+  unique(dat_HIBetween0.35to0.549_49_96mnthsY$Male)
        countryFemale_0.35to0.549_49_96mnths = countryFemale_0.35to0.549_49_96mnths + unique(dat_HIBetween0.35to0.549_49_96mnthsY$Female)
        countryTotal_0.35to0.549_49_96mnths = countryTotal_0.35to0.549_49_96mnths + unique(dat_HIBetween0.35to0.549_49_96mnthsY$TotalParticipant)
    }
}
percHI_0.35to0.549_49_96mnths_HI_Male = (countryMale_0.35to0.549_49_96mnths/countryTotal_0.35to0.549_49_96mnths)*100
percHI_0.35to0.549_49_96mnths_HI_Female = (countryFemale_0.35to0.549_49_96mnths/countryTotal_0.35to0.549_49_96mnths)*100

HumanIndxBetween0.35to0.549_GreaterThan97mnths <- datFinal[which((datFinal$HumanIndex >= 0.35 
    & datFinal$HumanIndex <= 0.549)
	& (datFinal$Sanction_months >= 97)),]
nrow(HumanIndxBetween0.35to0.549_GreaterThan97mnths)
#countryTotal_0.35to0.549_GreaterThan97mnths = 0
countryMale_0.35to0.549_GreaterThan97mnths = 0
countryFemale_0.35to0.549_GreaterThan97mnths = 0
countryTotal_0.35to0.549_GreaterThan97mnths = 0
totalpercountry <- c()
uniqCount <- unique(HumanIndxBetween0.35to0.549_GreaterThan97mnths$Nationality); 
#country <- c(); year <- c(); Participants <- c()
country <- c(); year <- c(); MaleParticipants0.35to0.549_GreaterThan97mnths <- c(); FemaleParticipants0.35to0.549_GreaterThan97mnths <- c()
Participants <- c()
for(ii in 1:length(uniqCount)){
    dat_HIBetween0.35to0.549_GreaterThan97mnths <- HumanIndxBetween0.35to0.549_GreaterThan97mnths[which(HumanIndxBetween0.35to0.549_GreaterThan97mnths$Nationality == uniqCount[ii]),]
    uniqyr <- unique(dat_HIBetween0.35to0.549_GreaterThan97mnths$NearestOlympicYear)
    if(length(uniqyr) > 1){
        for(jj in 1:length(uniqyr)){
            dat_HIBetween0.35to0.549_GreaterThan97mnthsY <- dat_HIBetween0.35to0.549_GreaterThan97mnths[which(dat_HIBetween0.35to0.549_GreaterThan97mnths$NearestOlympicYear == uniqyr[jj]),]
            country <- c(country, uniqCount[ii]); 
            year <- c(year, uniqyr[jj]); 
            MaleParticipants0.35to0.549_GreaterThan97mnths <- c(MaleParticipants0.35to0.549_GreaterThan97mnths, unique(dat_HIBetween0.35to0.549_GreaterThan97mnthsY$Male))
            FemaleParticipants0.35to0.549_GreaterThan97mnths <- c(FemaleParticipants0.35to0.549_GreaterThan97mnths, unique(dat_HIBetween0.35to0.549_GreaterThan97mnthsY$Female))
            #Participants <- c(Participants, unique(dat_HIBetween0.35to0.549_GreaterThan97mnthsY$TotalParticipant))
            countryMale_0.35to0.549_GreaterThan97mnths = countryMale_0.35to0.549_GreaterThan97mnths+  unique(dat_HIBetween0.35to0.549_GreaterThan97mnthsY$Male)
            countryFemale_0.35to0.549_GreaterThan97mnths = countryFemale_0.35to0.549_GreaterThan97mnths + unique(dat_HIBetween0.35to0.549_GreaterThan97mnthsY$Female)
            countryTotal_0.35to0.549_GreaterThan97mnths = countryTotal_0.35to0.549_GreaterThan97mnths + unique(dat_HIBetween0.35to0.549_GreaterThan97mnthsY$TotalParticipant)
        }
    }else{
        dat_HIBetween0.35to0.549_GreaterThan97mnthsY <- dat_HIBetween0.35to0.549_GreaterThan97mnths[which(dat_HIBetween0.35to0.549_GreaterThan97mnths$NearestOlympicYear == uniqyr),]
        country <- c(country, uniqCount[ii]); 
        year <- c(year, uniqyr); 
        MaleParticipants0.35to0.549_GreaterThan97mnths <- c(MaleParticipants0.35to0.549_GreaterThan97mnths, unique(dat_HIBetween0.35to0.549_GreaterThan97mnthsY$Male))
        FemaleParticipants0.35to0.549_GreaterThan97mnths <- c(FemaleParticipants0.35to0.549_GreaterThan97mnths, unique(dat_HIBetween0.35to0.549_GreaterThan97mnthsY$Female))
        #Participants <- c(Participants, unique(dat_HIBetween0.35to0.549_GreaterThan97mnthsY$TotalParticipant))
        countryMale_0.35to0.549_GreaterThan97mnths = countryMale_0.35to0.549_GreaterThan97mnths+  unique(dat_HIBetween0.35to0.549_GreaterThan97mnthsY$Male)
        countryFemale_0.35to0.549_GreaterThan97mnths = countryFemale_0.35to0.549_GreaterThan97mnths + unique(dat_HIBetween0.35to0.549_GreaterThan97mnthsY$Female)
        countryTotal_0.35to0.549_GreaterThan97mnths = countryTotal_0.35to0.549_GreaterThan97mnths + unique(dat_HIBetween0.35to0.549_GreaterThan97mnthsY$TotalParticipant)
    }
}
percHI_0.35to0.549_GreaterThan97mnths_HI_Male = (countryMale_0.35to0.549_GreaterThan97mnths/countryTotal_0.35to0.549_GreaterThan97mnths)*100
percHI_0.35to0.549_GreaterThan97mnths_HI_Female = (countryFemale_0.35to0.549_GreaterThan97mnths/countryTotal_0.35to0.549_GreaterThan97mnths)*100

#HumanIndxBetween0.55to0.699 <- datFinal[which(datFinal$HumanIndex >= 0.55 & datFinal$HumanIndex <= 0.699),]
#nrow(HumanIndxBetween0.55to0.699)
#0.55to0.699
HumanIndxBetween0.55to0.699_LessThan24mnths <- datFinal[which((datFinal$HumanIndex >= 0.55 
    & datFinal$HumanIndex <= 0.699)
	& datFinal$Sanction_months <= 24),]
nrow(HumanIndxBetween0.55to0.699_LessThan24mnths)
#countryTotal_0.55to0.699_LessThan24mnths = 0
countryMale_0.55to0.699_LessThan24mnths = 0
countryFemale_0.55to0.699_LessThan24mnths = 0
countryTotal_0.55to0.699_LessThan24mnths = 0
totalpercountry <- c()
uniqCount <- unique(HumanIndxBetween0.55to0.699_LessThan24mnths$Nationality); 
#country <- c(); year <- c(); Participants <- c()
country <- c(); year <- c(); MaleParticipants0.55to0.699_LessThan24mnths <- c(); FemaleParticipants0.55to0.699_LessThan24mnths <- c()
Participants <- c()
for(ii in 1:length(uniqCount)){
    dat_HIBetween0.55to0.699_LessThan24mnths <- HumanIndxBetween0.55to0.699_LessThan24mnths[which(HumanIndxBetween0.55to0.699_LessThan24mnths$Nationality == uniqCount[ii]),]
    uniqyr <- unique(dat_HIBetween0.55to0.699_LessThan24mnths$NearestOlympicYear)
    if(length(uniqyr) > 1){
        for(jj in 1:length(uniqyr)){
            dat_HIBetween0.55to0.699_LessThan24mnthsY <- dat_HIBetween0.55to0.699_LessThan24mnths[which(dat_HIBetween0.55to0.699_LessThan24mnths$NearestOlympicYear == uniqyr[jj]),]
            country <- c(country, uniqCount[ii]); 
            year <- c(year, uniqyr[jj]); 
            MaleParticipants0.55to0.699_LessThan24mnths <- c(MaleParticipants0.55to0.699_LessThan24mnths, unique(dat_HIBetween0.55to0.699_LessThan24mnthsY$Male))
            FemaleParticipants0.55to0.699_LessThan24mnths <- c(FemaleParticipants0.55to0.699_LessThan24mnths, unique(dat_HIBetween0.55to0.699_LessThan24mnthsY$Female))
            #Participants <- c(Participants, unique(dat_HIBetween0.55to0.699_LessThan24mnthsY$TotalParticipant))
            countryMale_0.55to0.699_LessThan24mnths = countryMale_0.55to0.699_LessThan24mnths+  unique(dat_HIBetween0.55to0.699_LessThan24mnthsY$Male)
            countryFemale_0.55to0.699_LessThan24mnths = countryFemale_0.55to0.699_LessThan24mnths + unique(dat_HIBetween0.55to0.699_LessThan24mnthsY$Female)
            countryTotal_0.55to0.699_LessThan24mnths = countryTotal_0.55to0.699_LessThan24mnths + unique(dat_HIBetween0.55to0.699_LessThan24mnthsY$TotalParticipant)
        }
    }else{
        dat_HIBetween0.55to0.699_LessThan24mnthsY <- dat_HIBetween0.55to0.699_LessThan24mnths[which(dat_HIBetween0.55to0.699_LessThan24mnths$NearestOlympicYear == uniqyr),]
        country <- c(country, uniqCount[ii]); 
        year <- c(year, uniqyr); 
        MaleParticipants0.55to0.699_LessThan24mnths <- c(MaleParticipants0.55to0.699_LessThan24mnths, unique(dat_HIBetween0.55to0.699_LessThan24mnthsY$Male))
        FemaleParticipants0.55to0.699_LessThan24mnths <- c(FemaleParticipants0.55to0.699_LessThan24mnths, unique(dat_HIBetween0.55to0.699_LessThan24mnthsY$Female))
        #Participants <- c(Participants, unique(dat_HIBetween0.55to0.699_LessThan24mnthsY$TotalParticipant))
        countryMale_0.55to0.699_LessThan24mnths = countryMale_0.55to0.699_LessThan24mnths+  unique(dat_HIBetween0.55to0.699_LessThan24mnthsY$Male)
        countryFemale_0.55to0.699_LessThan24mnths = countryFemale_0.55to0.699_LessThan24mnths + unique(dat_HIBetween0.55to0.699_LessThan24mnthsY$Female)
        countryTotal_0.55to0.699_LessThan24mnths = countryTotal_0.55to0.699_LessThan24mnths + unique(dat_HIBetween0.55to0.699_LessThan24mnthsY$TotalParticipant)
    }
}
percHI_0.55to0.699_LessThan24mnths_HI_Male = (countryMale_0.55to0.699_LessThan24mnths/countryTotal_0.55to0.699_LessThan24mnths)*100
percHI_0.55to0.699_LessThan24mnths_HI_Female = (countryFemale_0.55to0.699_LessThan24mnths/countryTotal_0.55to0.699_LessThan24mnths)*100

HumanIndxBetween0.55to0.699_25_48mnths <- datFinal[which((datFinal$HumanIndex >= 0.55 
    & datFinal$HumanIndex <= 0.699)
	& (datFinal$Sanction_months >= 25 
	& datFinal$Sanction_months <= 48)),]
nrow(HumanIndxBetween0.55to0.699_25_48mnths)
#countryTotal_0.55to0.699_25_48mnths = 0
countryMale_0.55to0.699_25_48mnths = 0
countryFemale_0.55to0.699_25_48mnths = 0
countryTotal_0.55to0.699_25_48mnths = 0
totalpercountry <- c()
uniqCount <- unique(HumanIndxBetween0.55to0.699_25_48mnths$Nationality); 
#country <- c(); year <- c(); Participants <- c()
country <- c(); year <- c(); MaleParticipants0.55to0.699_25_48mnths <- c(); FemaleParticipants0.55to0.699_25_48mnths <- c()
Participants <- c()
for(ii in 1:length(uniqCount)){
    dat_HIBetween0.55to0.699_25_48mnths <- HumanIndxBetween0.55to0.699_25_48mnths[which(HumanIndxBetween0.55to0.699_25_48mnths$Nationality == uniqCount[ii]),]
    uniqyr <- unique(dat_HIBetween0.55to0.699_25_48mnths$NearestOlympicYear)
    if(length(uniqyr) > 1){
        for(jj in 1:length(uniqyr)){
            dat_HIBetween0.55to0.699_25_48mnthsY <- dat_HIBetween0.55to0.699_25_48mnths[which(dat_HIBetween0.55to0.699_25_48mnths$NearestOlympicYear == uniqyr[jj]),]
            country <- c(country, uniqCount[ii]); 
            year <- c(year, uniqyr[jj]); 
            MaleParticipants0.55to0.699_25_48mnths <- c(MaleParticipants0.55to0.699_25_48mnths, unique(dat_HIBetween0.55to0.699_25_48mnthsY$Male))
            FemaleParticipants0.55to0.699_25_48mnths <- c(FemaleParticipants0.55to0.699_25_48mnths, unique(dat_HIBetween0.55to0.699_25_48mnthsY$Female))
            #Participants <- c(Participants, unique(dat_HIBetween0.55to0.699_25_48mnthsY$TotalParticipant))
            countryMale_0.55to0.699_25_48mnths = countryMale_0.55to0.699_25_48mnths+  unique(dat_HIBetween0.55to0.699_25_48mnthsY$Male)
            countryFemale_0.55to0.699_25_48mnths = countryFemale_0.55to0.699_25_48mnths + unique(dat_HIBetween0.55to0.699_25_48mnthsY$Female)
            countryTotal_0.55to0.699_25_48mnths = countryTotal_0.55to0.699_25_48mnths + unique(dat_HIBetween0.55to0.699_25_48mnthsY$TotalParticipant)
        }
    }else{
        dat_HIBetween0.55to0.699_25_48mnthsY <- dat_HIBetween0.55to0.699_25_48mnths[which(dat_HIBetween0.55to0.699_25_48mnths$NearestOlympicYear == uniqyr),]
        country <- c(country, uniqCount[ii]); 
        year <- c(year, uniqyr); 
        MaleParticipants0.55to0.699_25_48mnths <- c(MaleParticipants0.55to0.699_25_48mnths, unique(dat_HIBetween0.55to0.699_25_48mnthsY$Male))
        FemaleParticipants0.55to0.699_25_48mnths <- c(FemaleParticipants0.55to0.699_25_48mnths, unique(dat_HIBetween0.55to0.699_25_48mnthsY$Female))
        #Participants <- c(Participants, unique(dat_HIBetween0.55to0.699_25_48mnthsY$TotalParticipant))
        countryMale_0.55to0.699_25_48mnths = countryMale_0.55to0.699_25_48mnths+  unique(dat_HIBetween0.55to0.699_25_48mnthsY$Male)
        countryFemale_0.55to0.699_25_48mnths = countryFemale_0.55to0.699_25_48mnths + unique(dat_HIBetween0.55to0.699_25_48mnthsY$Female)
        countryTotal_0.55to0.699_25_48mnths = countryTotal_0.55to0.699_25_48mnths + unique(dat_HIBetween0.55to0.699_25_48mnthsY$TotalParticipant)
    }
}
percHI_0.55to0.699_25_48mnths_HI_Male = (countryMale_0.55to0.699_25_48mnths/countryTotal_0.55to0.699_25_48mnths)*100
percHI_0.55to0.699_25_48mnths_HI_Female = (countryFemale_0.55to0.699_25_48mnths/countryTotal_0.55to0.699_25_48mnths)*100


HumanIndxBetween0.55to0.699_49_96mnths <- datFinal[which((datFinal$HumanIndex >= 0.55 
    & datFinal$HumanIndex <= 0.699)
	& (datFinal$Sanction_months >= 49 
	& datFinal$Sanction_months <= 96)),]
nrow(HumanIndxBetween0.55to0.699_49_96mnths)
#countryTotal_0.55to0.699_49_96mnths = 0
countryMale_0.55to0.699_49_96mnths = 0
countryFemale_0.55to0.699_49_96mnths = 0
countryTotal_0.55to0.699_49_96mnths = 0
totalpercountry <- c()
uniqCount <- unique(HumanIndxBetween0.55to0.699_49_96mnths$Nationality); 
#country <- c(); year <- c(); Participants <- c()
country <- c(); year <- c(); MaleParticipants0.55to0.699_49_96mnths <- c(); FemaleParticipants0.55to0.699_49_96mnths <- c()
Participants <- c()
for(ii in 1:length(uniqCount)){
    dat_HIBetween0.55to0.699_49_96mnths <- HumanIndxBetween0.55to0.699_49_96mnths[which(HumanIndxBetween0.55to0.699_49_96mnths$Nationality == uniqCount[ii]),]
    uniqyr <- unique(dat_HIBetween0.55to0.699_49_96mnths$NearestOlympicYear)
    if(length(uniqyr) > 1){
        for(jj in 1:length(uniqyr)){
            dat_HIBetween0.55to0.699_49_96mnthsY <- dat_HIBetween0.55to0.699_49_96mnths[which(dat_HIBetween0.55to0.699_49_96mnths$NearestOlympicYear == uniqyr[jj]),]
            country <- c(country, uniqCount[ii]); 
            year <- c(year, uniqyr[jj]); 
            MaleParticipants0.55to0.699_49_96mnths <- c(MaleParticipants0.55to0.699_49_96mnths, unique(dat_HIBetween0.55to0.699_49_96mnthsY$Male))
            FemaleParticipants0.55to0.699_49_96mnths <- c(FemaleParticipants0.55to0.699_49_96mnths, unique(dat_HIBetween0.55to0.699_49_96mnthsY$Female))
            #Participants <- c(Participants, unique(dat_HIBetween0.55to0.699_49_96mnthsY$TotalParticipant))
            countryMale_0.55to0.699_49_96mnths = countryMale_0.55to0.699_49_96mnths+  unique(dat_HIBetween0.55to0.699_49_96mnthsY$Male)
            countryFemale_0.55to0.699_49_96mnths = countryFemale_0.55to0.699_49_96mnths + unique(dat_HIBetween0.55to0.699_49_96mnthsY$Female)
            countryTotal_0.55to0.699_49_96mnths = countryTotal_0.55to0.699_49_96mnths + unique(dat_HIBetween0.55to0.699_49_96mnthsY$TotalParticipant)
        }
    }else{
        dat_HIBetween0.55to0.699_49_96mnthsY <- dat_HIBetween0.55to0.699_49_96mnths[which(dat_HIBetween0.55to0.699_49_96mnths$NearestOlympicYear == uniqyr),]
        country <- c(country, uniqCount[ii]); 
        year <- c(year, uniqyr); 
        MaleParticipants0.55to0.699_49_96mnths <- c(MaleParticipants0.55to0.699_49_96mnths, unique(dat_HIBetween0.55to0.699_49_96mnthsY$Male))
        FemaleParticipants0.55to0.699_49_96mnths <- c(FemaleParticipants0.55to0.699_49_96mnths, unique(dat_HIBetween0.55to0.699_49_96mnthsY$Female))
        #Participants <- c(Participants, unique(dat_HIBetween0.55to0.699_49_96mnthsY$TotalParticipant))
        countryMale_0.55to0.699_49_96mnths = countryMale_0.55to0.699_49_96mnths+  unique(dat_HIBetween0.55to0.699_49_96mnthsY$Male)
        countryFemale_0.55to0.699_49_96mnths = countryFemale_0.55to0.699_49_96mnths + unique(dat_HIBetween0.55to0.699_49_96mnthsY$Female)
        countryTotal_0.55to0.699_49_96mnths = countryTotal_0.55to0.699_49_96mnths + unique(dat_HIBetween0.55to0.699_49_96mnthsY$TotalParticipant)
    }
}
percHI_0.55to0.699_49_96mnths_HI_Male = (countryMale_0.55to0.699_49_96mnths/countryTotal_0.55to0.699_49_96mnths)*100
percHI_0.55to0.699_49_96mnths_HI_Female = (countryFemale_0.55to0.699_49_96mnths/countryTotal_0.55to0.699_49_96mnths)*100

HumanIndxBetween0.55to0.699_GreaterThan97mnths <- datFinal[which((datFinal$HumanIndex >= 0.55 
    & datFinal$HumanIndex <= 0.699)
	& (datFinal$Sanction_months >= 97)),]
nrow(HumanIndxBetween0.55to0.699_GreaterThan97mnths)
#countryTotal_0.55to0.699_GreaterThan97mnths = 0
countryMale_0.55to0.699_GreaterThan97mnths = 0
countryFemale_0.55to0.699_GreaterThan97mnths = 0
countryTotal_0.55to0.699_GreaterThan97mnths = 0
totalpercountry <- c()
uniqCount <- unique(HumanIndxBetween0.55to0.699_GreaterThan97mnths$Nationality); 
#country <- c(); year <- c(); Participants <- c()
country <- c(); year <- c(); MaleParticipants0.55to0.699_GreaterThan97mnths <- c(); FemaleParticipants0.55to0.699_GreaterThan97mnths <- c()
Participants <- c()
for(ii in 1:length(uniqCount)){
    dat_HIBetween0.55to0.699_GreaterThan97mnths <- HumanIndxBetween0.55to0.699_GreaterThan97mnths[which(HumanIndxBetween0.55to0.699_GreaterThan97mnths$Nationality == uniqCount[ii]),]
    uniqyr <- unique(dat_HIBetween0.55to0.699_GreaterThan97mnths$NearestOlympicYear)
    if(length(uniqyr) > 1){
        for(jj in 1:length(uniqyr)){
            dat_HIBetween0.55to0.699_GreaterThan97mnthsY <- dat_HIBetween0.55to0.699_GreaterThan97mnths[which(dat_HIBetween0.55to0.699_GreaterThan97mnths$NearestOlympicYear == uniqyr[jj]),]
            country <- c(country, uniqCount[ii]); 
            year <- c(year, uniqyr[jj]); 
            MaleParticipants0.55to0.699_GreaterThan97mnths <- c(MaleParticipants0.55to0.699_GreaterThan97mnths, unique(dat_HIBetween0.55to0.699_GreaterThan97mnthsY$Male))
            FemaleParticipants0.55to0.699_GreaterThan97mnths <- c(FemaleParticipants0.55to0.699_GreaterThan97mnths, unique(dat_HIBetween0.55to0.699_GreaterThan97mnthsY$Female))
            #Participants <- c(Participants, unique(dat_HIBetween0.55to0.699_GreaterThan97mnthsY$TotalParticipant))
            countryMale_0.55to0.699_GreaterThan97mnths = countryMale_0.55to0.699_GreaterThan97mnths+  unique(dat_HIBetween0.55to0.699_GreaterThan97mnthsY$Male)
            countryFemale_0.55to0.699_GreaterThan97mnths = countryFemale_0.55to0.699_GreaterThan97mnths + unique(dat_HIBetween0.55to0.699_GreaterThan97mnthsY$Female)
            countryTotal_0.55to0.699_GreaterThan97mnths = countryTotal_0.55to0.699_GreaterThan97mnths + unique(dat_HIBetween0.55to0.699_GreaterThan97mnthsY$TotalParticipant)
        }
    }else{
        dat_HIBetween0.55to0.699_GreaterThan97mnthsY <- dat_HIBetween0.55to0.699_GreaterThan97mnths[which(dat_HIBetween0.55to0.699_GreaterThan97mnths$NearestOlympicYear == uniqyr),]
        country <- c(country, uniqCount[ii]); 
        year <- c(year, uniqyr); 
        MaleParticipants0.55to0.699_GreaterThan97mnths <- c(MaleParticipants0.55to0.699_GreaterThan97mnths, unique(dat_HIBetween0.55to0.699_GreaterThan97mnthsY$Male))
        FemaleParticipants0.55to0.699_GreaterThan97mnths <- c(FemaleParticipants0.55to0.699_GreaterThan97mnths, unique(dat_HIBetween0.55to0.699_GreaterThan97mnthsY$Female))
        #Participants <- c(Participants, unique(dat_HIBetween0.55to0.699_GreaterThan97mnthsY$TotalParticipant))
        countryMale_0.55to0.699_GreaterThan97mnths = countryMale_0.55to0.699_GreaterThan97mnths+  unique(dat_HIBetween0.55to0.699_GreaterThan97mnthsY$Male)
        countryFemale_0.55to0.699_GreaterThan97mnths = countryFemale_0.55to0.699_GreaterThan97mnths + unique(dat_HIBetween0.55to0.699_GreaterThan97mnthsY$Female)
        countryTotal_0.55to0.699_GreaterThan97mnths = countryTotal_0.55to0.699_GreaterThan97mnths + unique(dat_HIBetween0.55to0.699_GreaterThan97mnthsY$TotalParticipant)
    }
}
percHI_0.55to0.699_GreaterThan97mnths_HI_Male = (countryMale_0.55to0.699_GreaterThan97mnths/countryTotal_0.55to0.699_GreaterThan97mnths)*100
percHI_0.55to0.699_GreaterThan97mnths_HI_Female = (countryFemale_0.55to0.699_GreaterThan97mnths/countryTotal_0.55to0.699_GreaterThan97mnths)*100


#0.70to0.799
HumanIndxBetween0.70to0.799_LessThan24mnths <- datFinal[which((datFinal$HumanIndex >= 0.70 
    & datFinal$HumanIndex <= 0.799)
	& datFinal$Sanction_months <= 24),]
nrow(HumanIndxBetween0.70to0.799_LessThan24mnths)
#countryTotal_0.70to0.799_LessThan24mnths = 0
countryMale_0.70to0.799_LessThan24mnths = 0
countryFemale_0.70to0.799_LessThan24mnths = 0
countryTotal_0.70to0.799_LessThan24mnths = 0
totalpercountry <- c()
uniqCount <- unique(HumanIndxBetween0.70to0.799_LessThan24mnths$Nationality); 
#country <- c(); year <- c(); Participants <- c()
country <- c(); year <- c(); MaleParticipants0.70to0.799_LessThan24mnths <- c(); FemaleParticipants0.70to0.799_LessThan24mnths <- c()
Participants <- c()
for(ii in 1:length(uniqCount)){
    dat_HIBetween0.70to0.799_LessThan24mnths <- HumanIndxBetween0.70to0.799_LessThan24mnths[which(HumanIndxBetween0.70to0.799_LessThan24mnths$Nationality == uniqCount[ii]),]
    uniqyr <- unique(dat_HIBetween0.70to0.799_LessThan24mnths$NearestOlympicYear)
    if(length(uniqyr) > 1){
        for(jj in 1:length(uniqyr)){
            dat_HIBetween0.70to0.799_LessThan24mnthsY <- dat_HIBetween0.70to0.799_LessThan24mnths[which(dat_HIBetween0.70to0.799_LessThan24mnths$NearestOlympicYear == uniqyr[jj]),]
            country <- c(country, uniqCount[ii]); 
            year <- c(year, uniqyr[jj]); 
            MaleParticipants0.70to0.799_LessThan24mnths <- c(MaleParticipants0.70to0.799_LessThan24mnths, unique(dat_HIBetween0.70to0.799_LessThan24mnthsY$Male))
            FemaleParticipants0.70to0.799_LessThan24mnths <- c(FemaleParticipants0.70to0.799_LessThan24mnths, unique(dat_HIBetween0.70to0.799_LessThan24mnthsY$Female))
            #Participants <- c(Participants, unique(dat_HIBetween0.70to0.799_LessThan24mnthsY$TotalParticipant))
            countryMale_0.70to0.799_LessThan24mnths = countryMale_0.70to0.799_LessThan24mnths+  unique(dat_HIBetween0.70to0.799_LessThan24mnthsY$Male)
            countryFemale_0.70to0.799_LessThan24mnths = countryFemale_0.70to0.799_LessThan24mnths + unique(dat_HIBetween0.70to0.799_LessThan24mnthsY$Female)
            countryTotal_0.70to0.799_LessThan24mnths = countryTotal_0.70to0.799_LessThan24mnths + unique(dat_HIBetween0.70to0.799_LessThan24mnthsY$TotalParticipant)
        }
    }else{
        dat_HIBetween0.70to0.799_LessThan24mnthsY <- dat_HIBetween0.70to0.799_LessThan24mnths[which(dat_HIBetween0.70to0.799_LessThan24mnths$NearestOlympicYear == uniqyr),]
        country <- c(country, uniqCount[ii]); 
        year <- c(year, uniqyr); 
        MaleParticipants0.70to0.799_LessThan24mnths <- c(MaleParticipants0.70to0.799_LessThan24mnths, unique(dat_HIBetween0.70to0.799_LessThan24mnthsY$Male))
        FemaleParticipants0.70to0.799_LessThan24mnths <- c(FemaleParticipants0.70to0.799_LessThan24mnths, unique(dat_HIBetween0.70to0.799_LessThan24mnthsY$Female))
        #Participants <- c(Participants, unique(dat_HIBetween0.70to0.799_LessThan24mnthsY$TotalParticipant))
        countryMale_0.70to0.799_LessThan24mnths = countryMale_0.70to0.799_LessThan24mnths+  unique(dat_HIBetween0.70to0.799_LessThan24mnthsY$Male)
        countryFemale_0.70to0.799_LessThan24mnths = countryFemale_0.70to0.799_LessThan24mnths + unique(dat_HIBetween0.70to0.799_LessThan24mnthsY$Female)
        countryTotal_0.70to0.799_LessThan24mnths = countryTotal_0.70to0.799_LessThan24mnths + unique(dat_HIBetween0.70to0.799_LessThan24mnthsY$TotalParticipant)
    }
}
percHI_0.70to0.799_LessThan24mnths_HI_Male = (countryMale_0.70to0.799_LessThan24mnths/countryTotal_0.70to0.799_LessThan24mnths)*100
percHI_0.70to0.799_LessThan24mnths_HI_Female = (countryFemale_0.70to0.799_LessThan24mnths/countryTotal_0.70to0.799_LessThan24mnths)*100

HumanIndxBetween0.70to0.799_25_48mnths <- datFinal[which((datFinal$HumanIndex >= 0.70 
    & datFinal$HumanIndex <= 0.799)
	& (datFinal$Sanction_months >= 25 
	& datFinal$Sanction_months <= 48)),]
nrow(HumanIndxBetween0.70to0.799_25_48mnths)
#countryTotal_0.70to0.799_25_48mnths = 0
countryMale_0.70to0.799_25_48mnths = 0
countryFemale_0.70to0.799_25_48mnths = 0
countryTotal_0.70to0.799_25_48mnths = 0
totalpercountry <- c()
uniqCount <- unique(HumanIndxBetween0.70to0.799_25_48mnths$Nationality); 
#country <- c(); year <- c(); Participants <- c()
country <- c(); year <- c(); MaleParticipants0.70to0.799_25_48mnths <- c(); FemaleParticipants0.70to0.799_25_48mnths <- c()
Participants <- c()
for(ii in 1:length(uniqCount)){
    dat_HIBetween0.70to0.799_25_48mnths <- HumanIndxBetween0.70to0.799_25_48mnths[which(HumanIndxBetween0.70to0.799_25_48mnths$Nationality == uniqCount[ii]),]
    uniqyr <- unique(dat_HIBetween0.70to0.799_25_48mnths$NearestOlympicYear)
    if(length(uniqyr) > 1){
        for(jj in 1:length(uniqyr)){
            dat_HIBetween0.70to0.799_25_48mnthsY <- dat_HIBetween0.70to0.799_25_48mnths[which(dat_HIBetween0.70to0.799_25_48mnths$NearestOlympicYear == uniqyr[jj]),]
            country <- c(country, uniqCount[ii]); 
            year <- c(year, uniqyr[jj]); 
            MaleParticipants0.70to0.799_25_48mnths <- c(MaleParticipants0.70to0.799_25_48mnths, unique(dat_HIBetween0.70to0.799_25_48mnthsY$Male))
            FemaleParticipants0.70to0.799_25_48mnths <- c(FemaleParticipants0.70to0.799_25_48mnths, unique(dat_HIBetween0.70to0.799_25_48mnthsY$Female))
            #Participants <- c(Participants, unique(dat_HIBetween0.70to0.799_25_48mnthsY$TotalParticipant))
            countryMale_0.70to0.799_25_48mnths = countryMale_0.70to0.799_25_48mnths+  unique(dat_HIBetween0.70to0.799_25_48mnthsY$Male)
            countryFemale_0.70to0.799_25_48mnths = countryFemale_0.70to0.799_25_48mnths + unique(dat_HIBetween0.70to0.799_25_48mnthsY$Female)
            countryTotal_0.70to0.799_25_48mnths = countryTotal_0.70to0.799_25_48mnths + unique(dat_HIBetween0.70to0.799_25_48mnthsY$TotalParticipant)
        }
    }else{
        dat_HIBetween0.70to0.799_25_48mnthsY <- dat_HIBetween0.70to0.799_25_48mnths[which(dat_HIBetween0.70to0.799_25_48mnths$NearestOlympicYear == uniqyr),]
        country <- c(country, uniqCount[ii]); 
        year <- c(year, uniqyr); 
        MaleParticipants0.70to0.799_25_48mnths <- c(MaleParticipants0.70to0.799_25_48mnths, unique(dat_HIBetween0.70to0.799_25_48mnthsY$Male))
        FemaleParticipants0.70to0.799_25_48mnths <- c(FemaleParticipants0.70to0.799_25_48mnths, unique(dat_HIBetween0.70to0.799_25_48mnthsY$Female))
        #Participants <- c(Participants, unique(dat_HIBetween0.70to0.799_25_48mnthsY$TotalParticipant))
        countryMale_0.70to0.799_25_48mnths = countryMale_0.70to0.799_25_48mnths+  unique(dat_HIBetween0.70to0.799_25_48mnthsY$Male)
        countryFemale_0.70to0.799_25_48mnths = countryFemale_0.70to0.799_25_48mnths + unique(dat_HIBetween0.70to0.799_25_48mnthsY$Female)
        countryTotal_0.70to0.799_25_48mnths = countryTotal_0.70to0.799_25_48mnths + unique(dat_HIBetween0.70to0.799_25_48mnthsY$TotalParticipant)
    }
}
percHI_0.70to0.799_25_48mnths_HI_Male = (countryMale_0.70to0.799_25_48mnths/countryTotal_0.70to0.799_25_48mnths)*100
percHI_0.70to0.799_25_48mnths_HI_Female = (countryFemale_0.70to0.799_25_48mnths/countryTotal_0.70to0.799_25_48mnths)*100


HumanIndxBetween0.70to0.799_49_96mnths <- datFinal[which((datFinal$HumanIndex >= 0.70 
    & datFinal$HumanIndex <= 0.799)
	& (datFinal$Sanction_months >= 49 
	& datFinal$Sanction_months <= 96)),]
nrow(HumanIndxBetween0.70to0.799_49_96mnths)
#countryTotal_0.70to0.799_49_96mnths = 0
countryMale_0.70to0.799_49_96mnths = 0
countryFemale_0.70to0.799_49_96mnths = 0
countryTotal_0.70to0.799_49_96mnths = 0
totalpercountry <- c()
uniqCount <- unique(HumanIndxBetween0.70to0.799_49_96mnths$Nationality); 
#country <- c(); year <- c(); Participants <- c()
country <- c(); year <- c(); MaleParticipants0.70to0.799_49_96mnths <- c(); FemaleParticipants0.70to0.799_49_96mnths <- c()
Participants <- c()
for(ii in 1:length(uniqCount)){
    dat_HIBetween0.70to0.799_49_96mnths <- HumanIndxBetween0.70to0.799_49_96mnths[which(HumanIndxBetween0.70to0.799_49_96mnths$Nationality == uniqCount[ii]),]
    uniqyr <- unique(dat_HIBetween0.70to0.799_49_96mnths$NearestOlympicYear)
    if(length(uniqyr) > 1){
        for(jj in 1:length(uniqyr)){
            dat_HIBetween0.70to0.799_49_96mnthsY <- dat_HIBetween0.70to0.799_49_96mnths[which(dat_HIBetween0.70to0.799_49_96mnths$NearestOlympicYear == uniqyr[jj]),]
            country <- c(country, uniqCount[ii]); 
            year <- c(year, uniqyr[jj]); 
            MaleParticipants0.70to0.799_49_96mnths <- c(MaleParticipants0.70to0.799_49_96mnths, unique(dat_HIBetween0.70to0.799_49_96mnthsY$Male))
            FemaleParticipants0.70to0.799_49_96mnths <- c(FemaleParticipants0.70to0.799_49_96mnths, unique(dat_HIBetween0.70to0.799_49_96mnthsY$Female))
            #Participants <- c(Participants, unique(dat_HIBetween0.70to0.799_49_96mnthsY$TotalParticipant))
            countryMale_0.70to0.799_49_96mnths = countryMale_0.70to0.799_49_96mnths+  unique(dat_HIBetween0.70to0.799_49_96mnthsY$Male)
            countryFemale_0.70to0.799_49_96mnths = countryFemale_0.70to0.799_49_96mnths + unique(dat_HIBetween0.70to0.799_49_96mnthsY$Female)
            countryTotal_0.70to0.799_49_96mnths = countryTotal_0.70to0.799_49_96mnths + unique(dat_HIBetween0.70to0.799_49_96mnthsY$TotalParticipant)
        }
    }else{
        dat_HIBetween0.70to0.799_49_96mnthsY <- dat_HIBetween0.70to0.799_49_96mnths[which(dat_HIBetween0.70to0.799_49_96mnths$NearestOlympicYear == uniqyr),]
        country <- c(country, uniqCount[ii]); 
        year <- c(year, uniqyr); 
        MaleParticipants0.70to0.799_49_96mnths <- c(MaleParticipants0.70to0.799_49_96mnths, unique(dat_HIBetween0.70to0.799_49_96mnthsY$Male))
        FemaleParticipants0.70to0.799_49_96mnths <- c(FemaleParticipants0.70to0.799_49_96mnths, unique(dat_HIBetween0.70to0.799_49_96mnthsY$Female))
        #Participants <- c(Participants, unique(dat_HIBetween0.70to0.799_49_96mnthsY$TotalParticipant))
        countryMale_0.70to0.799_49_96mnths = countryMale_0.70to0.799_49_96mnths+  unique(dat_HIBetween0.70to0.799_49_96mnthsY$Male)
        countryFemale_0.70to0.799_49_96mnths = countryFemale_0.70to0.799_49_96mnths + unique(dat_HIBetween0.70to0.799_49_96mnthsY$Female)
        countryTotal_0.70to0.799_49_96mnths = countryTotal_0.70to0.799_49_96mnths + unique(dat_HIBetween0.70to0.799_49_96mnthsY$TotalParticipant)
    }
}
percHI_0.70to0.799_49_96mnths_HI_Male = (countryMale_0.70to0.799_49_96mnths/countryTotal_0.70to0.799_49_96mnths)*100
percHI_0.70to0.799_49_96mnths_HI_Female = (countryFemale_0.70to0.799_49_96mnths/countryTotal_0.70to0.799_49_96mnths)*100

HumanIndxBetween0.70to0.799_GreaterThan97mnths <- datFinal[which((datFinal$HumanIndex >= 0.70 
    & datFinal$HumanIndex <= 0.799)
	& (datFinal$Sanction_months >= 97)),]
nrow(HumanIndxBetween0.70to0.799_GreaterThan97mnths)
#countryTotal_0.70to0.799_GreaterThan97mnths = 0
countryMale_0.70to0.799_GreaterThan97mnths = 0
countryFemale_0.70to0.799_GreaterThan97mnths = 0
countryTotal_0.70to0.799_GreaterThan97mnths = 0
totalpercountry <- c()
uniqCount <- unique(HumanIndxBetween0.70to0.799_GreaterThan97mnths$Nationality); 
#country <- c(); year <- c(); Participants <- c()
country <- c(); year <- c(); MaleParticipants0.70to0.799_GreaterThan97mnths <- c(); FemaleParticipants0.70to0.799_GreaterThan97mnths <- c()
Participants <- c()
for(ii in 1:length(uniqCount)){
    dat_HIBetween0.70to0.799_GreaterThan97mnths <- HumanIndxBetween0.70to0.799_GreaterThan97mnths[which(HumanIndxBetween0.70to0.799_GreaterThan97mnths$Nationality == uniqCount[ii]),]
    uniqyr <- unique(dat_HIBetween0.70to0.799_GreaterThan97mnths$NearestOlympicYear)
    if(length(uniqyr) > 1){
        for(jj in 1:length(uniqyr)){
            dat_HIBetween0.70to0.799_GreaterThan97mnthsY <- dat_HIBetween0.70to0.799_GreaterThan97mnths[which(dat_HIBetween0.70to0.799_GreaterThan97mnths$NearestOlympicYear == uniqyr[jj]),]
            country <- c(country, uniqCount[ii]); 
            year <- c(year, uniqyr[jj]); 
            MaleParticipants0.70to0.799_GreaterThan97mnths <- c(MaleParticipants0.70to0.799_GreaterThan97mnths, unique(dat_HIBetween0.70to0.799_GreaterThan97mnthsY$Male))
            FemaleParticipants0.70to0.799_GreaterThan97mnths <- c(FemaleParticipants0.70to0.799_GreaterThan97mnths, unique(dat_HIBetween0.70to0.799_GreaterThan97mnthsY$Female))
            #Participants <- c(Participants, unique(dat_HIBetween0.70to0.799_GreaterThan97mnthsY$TotalParticipant))
            countryMale_0.70to0.799_GreaterThan97mnths = countryMale_0.70to0.799_GreaterThan97mnths+  unique(dat_HIBetween0.70to0.799_GreaterThan97mnthsY$Male)
            countryFemale_0.70to0.799_GreaterThan97mnths = countryFemale_0.70to0.799_GreaterThan97mnths + unique(dat_HIBetween0.70to0.799_GreaterThan97mnthsY$Female)
            countryTotal_0.70to0.799_GreaterThan97mnths = countryTotal_0.70to0.799_GreaterThan97mnths + unique(dat_HIBetween0.70to0.799_GreaterThan97mnthsY$TotalParticipant)
        }
    }else{
        dat_HIBetween0.70to0.799_GreaterThan97mnthsY <- dat_HIBetween0.70to0.799_GreaterThan97mnths[which(dat_HIBetween0.70to0.799_GreaterThan97mnths$NearestOlympicYear == uniqyr),]
        country <- c(country, uniqCount[ii]); 
        year <- c(year, uniqyr); 
        MaleParticipants0.70to0.799_GreaterThan97mnths <- c(MaleParticipants0.70to0.799_GreaterThan97mnths, unique(dat_HIBetween0.70to0.799_GreaterThan97mnthsY$Male))
        FemaleParticipants0.70to0.799_GreaterThan97mnths <- c(FemaleParticipants0.70to0.799_GreaterThan97mnths, unique(dat_HIBetween0.70to0.799_GreaterThan97mnthsY$Female))
        #Participants <- c(Participants, unique(dat_HIBetween0.70to0.799_GreaterThan97mnthsY$TotalParticipant))
        countryMale_0.70to0.799_GreaterThan97mnths = countryMale_0.70to0.799_GreaterThan97mnths+  unique(dat_HIBetween0.70to0.799_GreaterThan97mnthsY$Male)
        countryFemale_0.70to0.799_GreaterThan97mnths = countryFemale_0.70to0.799_GreaterThan97mnths + unique(dat_HIBetween0.70to0.799_GreaterThan97mnthsY$Female)
        countryTotal_0.70to0.799_GreaterThan97mnths = countryTotal_0.70to0.799_GreaterThan97mnths + unique(dat_HIBetween0.70to0.799_GreaterThan97mnthsY$TotalParticipant)
    }
}
percHI_0.70to0.799_GreaterThan97mnths_HI_Male = (countryMale_0.70to0.799_GreaterThan97mnths/countryTotal_0.70to0.799_GreaterThan97mnths)*100
percHI_0.70to0.799_GreaterThan97mnths_HI_Female = (countryFemale_0.70to0.799_GreaterThan97mnths/countryTotal_0.70to0.799_GreaterThan97mnths)*100

#0.8to1
HumanIndxBetween0.80to1_LessThan24mnths <- datFinal[which((datFinal$HumanIndex >= 0.80 
    & datFinal$HumanIndex <= 1)
	& datFinal$Sanction_months <= 24),]
nrow(HumanIndxBetween0.80to1_LessThan24mnths)
#countryTotal_0.80to1_LessThan24mnths = 0
countryMale_0.80to1_LessThan24mnths = 0
countryFemale_0.80to1_LessThan24mnths = 0
countryTotal_0.80to1_LessThan24mnths = 0
totalpercountry <- c()
uniqCount <- unique(HumanIndxBetween0.80to1_LessThan24mnths$Nationality); 
#country <- c(); year <- c(); Participants <- c()
country <- c(); year <- c(); MaleParticipants0.80to1_LessThan24mnths <- c(); FemaleParticipants0.80to1_LessThan24mnths <- c()
Participants <- c()
for(ii in 1:length(uniqCount)){
    dat_HIBetween0.80to1_LessThan24mnths <- HumanIndxBetween0.80to1_LessThan24mnths[which(HumanIndxBetween0.80to1_LessThan24mnths$Nationality == uniqCount[ii]),]
    uniqyr <- unique(dat_HIBetween0.80to1_LessThan24mnths$NearestOlympicYear)
    if(length(uniqyr) > 1){
        for(jj in 1:length(uniqyr)){
            dat_HIBetween0.80to1_LessThan24mnthsY <- dat_HIBetween0.80to1_LessThan24mnths[which(dat_HIBetween0.80to1_LessThan24mnths$NearestOlympicYear == uniqyr[jj]),]
            country <- c(country, uniqCount[ii]); 
            year <- c(year, uniqyr[jj]); 
            MaleParticipants0.80to1_LessThan24mnths <- c(MaleParticipants0.80to1_LessThan24mnths, unique(dat_HIBetween0.80to1_LessThan24mnthsY$Male))
            FemaleParticipants0.80to1_LessThan24mnths <- c(FemaleParticipants0.80to1_LessThan24mnths, unique(dat_HIBetween0.80to1_LessThan24mnthsY$Female))
            #Participants <- c(Participants, unique(dat_HIBetween0.80to1_LessThan24mnthsY$TotalParticipant))
            countryMale_0.80to1_LessThan24mnths = countryMale_0.80to1_LessThan24mnths+  unique(dat_HIBetween0.80to1_LessThan24mnthsY$Male)
            countryFemale_0.80to1_LessThan24mnths = countryFemale_0.80to1_LessThan24mnths + unique(dat_HIBetween0.80to1_LessThan24mnthsY$Female)
            countryTotal_0.80to1_LessThan24mnths = countryTotal_0.80to1_LessThan24mnths + unique(dat_HIBetween0.80to1_LessThan24mnthsY$TotalParticipant)
        }
    }else{
        dat_HIBetween0.80to1_LessThan24mnthsY <- dat_HIBetween0.80to1_LessThan24mnths[which(dat_HIBetween0.80to1_LessThan24mnths$NearestOlympicYear == uniqyr),]
        country <- c(country, uniqCount[ii]); 
        year <- c(year, uniqyr); 
        MaleParticipants0.80to1_LessThan24mnths <- c(MaleParticipants0.80to1_LessThan24mnths, unique(dat_HIBetween0.80to1_LessThan24mnthsY$Male))
        FemaleParticipants0.80to1_LessThan24mnths <- c(FemaleParticipants0.80to1_LessThan24mnths, unique(dat_HIBetween0.80to1_LessThan24mnthsY$Female))
        #Participants <- c(Participants, unique(dat_HIBetween0.80to1_LessThan24mnthsY$TotalParticipant))
        countryMale_0.80to1_LessThan24mnths = countryMale_0.80to1_LessThan24mnths+  unique(dat_HIBetween0.80to1_LessThan24mnthsY$Male)
        countryFemale_0.80to1_LessThan24mnths = countryFemale_0.80to1_LessThan24mnths + unique(dat_HIBetween0.80to1_LessThan24mnthsY$Female)
        countryTotal_0.80to1_LessThan24mnths = countryTotal_0.80to1_LessThan24mnths + unique(dat_HIBetween0.80to1_LessThan24mnthsY$TotalParticipant)
    }
}
percHI_0.80to1_LessThan24mnths_HI_Male = (countryMale_0.80to1_LessThan24mnths/countryTotal_0.80to1_LessThan24mnths)*100
percHI_0.80to1_LessThan24mnths_HI_Female = (countryFemale_0.80to1_LessThan24mnths/countryTotal_0.80to1_LessThan24mnths)*100

HumanIndxBetween0.80to1_25_48mnths <- datFinal[which((datFinal$HumanIndex >= 0.80 
    & datFinal$HumanIndex <= 1)
	& (datFinal$Sanction_months >= 25 
	& datFinal$Sanction_months <= 48)),]
nrow(HumanIndxBetween0.80to1_25_48mnths)
#countryTotal_0.80to1_25_48mnths = 0
countryMale_0.80to1_25_48mnths = 0
countryFemale_0.80to1_25_48mnths = 0
countryTotal_0.80to1_25_48mnths = 0
totalpercountry <- c()
uniqCount <- unique(HumanIndxBetween0.80to1_25_48mnths$Nationality); 
#country <- c(); year <- c(); Participants <- c()
country <- c(); year <- c(); MaleParticipants0.80to1_25_48mnths <- c(); FemaleParticipants0.80to1_25_48mnths <- c()
Participants <- c()
for(ii in 1:length(uniqCount)){
    dat_HIBetween0.80to1_25_48mnths <- HumanIndxBetween0.80to1_25_48mnths[which(HumanIndxBetween0.80to1_25_48mnths$Nationality == uniqCount[ii]),]
    uniqyr <- unique(dat_HIBetween0.80to1_25_48mnths$NearestOlympicYear)
    if(length(uniqyr) > 1){
        for(jj in 1:length(uniqyr)){
            dat_HIBetween0.80to1_25_48mnthsY <- dat_HIBetween0.80to1_25_48mnths[which(dat_HIBetween0.80to1_25_48mnths$NearestOlympicYear == uniqyr[jj]),]
            country <- c(country, uniqCount[ii]); 
            year <- c(year, uniqyr[jj]); 
            MaleParticipants0.80to1_25_48mnths <- c(MaleParticipants0.80to1_25_48mnths, unique(dat_HIBetween0.80to1_25_48mnthsY$Male))
            FemaleParticipants0.80to1_25_48mnths <- c(FemaleParticipants0.80to1_25_48mnths, unique(dat_HIBetween0.80to1_25_48mnthsY$Female))
            #Participants <- c(Participants, unique(dat_HIBetween0.80to1_25_48mnthsY$TotalParticipant))
            countryMale_0.80to1_25_48mnths = countryMale_0.80to1_25_48mnths+  unique(dat_HIBetween0.80to1_25_48mnthsY$Male)
            countryFemale_0.80to1_25_48mnths = countryFemale_0.80to1_25_48mnths + unique(dat_HIBetween0.80to1_25_48mnthsY$Female)
            countryTotal_0.80to1_25_48mnths = countryTotal_0.80to1_25_48mnths + unique(dat_HIBetween0.80to1_25_48mnthsY$TotalParticipant)
        }
    }else{
        dat_HIBetween0.80to1_25_48mnthsY <- dat_HIBetween0.80to1_25_48mnths[which(dat_HIBetween0.80to1_25_48mnths$NearestOlympicYear == uniqyr),]
        country <- c(country, uniqCount[ii]); 
        year <- c(year, uniqyr); 
        MaleParticipants0.80to1_25_48mnths <- c(MaleParticipants0.80to1_25_48mnths, unique(dat_HIBetween0.80to1_25_48mnthsY$Male))
        FemaleParticipants0.80to1_25_48mnths <- c(FemaleParticipants0.80to1_25_48mnths, unique(dat_HIBetween0.80to1_25_48mnthsY$Female))
        #Participants <- c(Participants, unique(dat_HIBetween0.80to1_25_48mnthsY$TotalParticipant))
        countryMale_0.80to1_25_48mnths = countryMale_0.80to1_25_48mnths+  unique(dat_HIBetween0.80to1_25_48mnthsY$Male)
        countryFemale_0.80to1_25_48mnths = countryFemale_0.80to1_25_48mnths + unique(dat_HIBetween0.80to1_25_48mnthsY$Female)
        countryTotal_0.80to1_25_48mnths = countryTotal_0.80to1_25_48mnths + unique(dat_HIBetween0.80to1_25_48mnthsY$TotalParticipant)
    }
}
percHI_0.80to1_25_48mnths_HI_Male = (countryMale_0.80to1_25_48mnths/countryTotal_0.80to1_25_48mnths)*100
percHI_0.80to1_25_48mnths_HI_Female = (countryFemale_0.80to1_25_48mnths/countryTotal_0.80to1_25_48mnths)*100


HumanIndxBetween0.80to1_49_96mnths <- datFinal[which((datFinal$HumanIndex >= 0.80 
    & datFinal$HumanIndex <= 1)
	& (datFinal$Sanction_months >= 49 
	& datFinal$Sanction_months <= 96)),]
nrow(HumanIndxBetween0.80to1_49_96mnths)
#countryTotal_0.80to1_49_96mnths = 0
countryMale_0.80to1_49_96mnths = 0
countryFemale_0.80to1_49_96mnths = 0
countryTotal_0.80to1_49_96mnths = 0
totalpercountry <- c()
uniqCount <- unique(HumanIndxBetween0.80to1_49_96mnths$Nationality); 
#country <- c(); year <- c(); Participants <- c()
country <- c(); year <- c(); MaleParticipants0.80to1_49_96mnths <- c(); FemaleParticipants0.80to1_49_96mnths <- c()
Participants <- c()
for(ii in 1:length(uniqCount)){
    dat_HIBetween0.80to1_49_96mnths <- HumanIndxBetween0.80to1_49_96mnths[which(HumanIndxBetween0.80to1_49_96mnths$Nationality == uniqCount[ii]),]
    uniqyr <- unique(dat_HIBetween0.80to1_49_96mnths$NearestOlympicYear)
    if(length(uniqyr) > 1){
        for(jj in 1:length(uniqyr)){
            dat_HIBetween0.80to1_49_96mnthsY <- dat_HIBetween0.80to1_49_96mnths[which(dat_HIBetween0.80to1_49_96mnths$NearestOlympicYear == uniqyr[jj]),]
            country <- c(country, uniqCount[ii]); 
            year <- c(year, uniqyr[jj]); 
            MaleParticipants0.80to1_49_96mnths <- c(MaleParticipants0.80to1_49_96mnths, unique(dat_HIBetween0.80to1_49_96mnthsY$Male))
            FemaleParticipants0.80to1_49_96mnths <- c(FemaleParticipants0.80to1_49_96mnths, unique(dat_HIBetween0.80to1_49_96mnthsY$Female))
            #Participants <- c(Participants, unique(dat_HIBetween0.80to1_49_96mnthsY$TotalParticipant))
            countryMale_0.80to1_49_96mnths = countryMale_0.80to1_49_96mnths+  unique(dat_HIBetween0.80to1_49_96mnthsY$Male)
            countryFemale_0.80to1_49_96mnths = countryFemale_0.80to1_49_96mnths + unique(dat_HIBetween0.80to1_49_96mnthsY$Female)
            countryTotal_0.80to1_49_96mnths = countryTotal_0.80to1_49_96mnths + unique(dat_HIBetween0.80to1_49_96mnthsY$TotalParticipant)
        }
    }else{
        dat_HIBetween0.80to1_49_96mnthsY <- dat_HIBetween0.80to1_49_96mnths[which(dat_HIBetween0.80to1_49_96mnths$NearestOlympicYear == uniqyr),]
        country <- c(country, uniqCount[ii]); 
        year <- c(year, uniqyr); 
        MaleParticipants0.80to1_49_96mnths <- c(MaleParticipants0.80to1_49_96mnths, unique(dat_HIBetween0.80to1_49_96mnthsY$Male))
        FemaleParticipants0.80to1_49_96mnths <- c(FemaleParticipants0.80to1_49_96mnths, unique(dat_HIBetween0.80to1_49_96mnthsY$Female))
        #Participants <- c(Participants, unique(dat_HIBetween0.80to1_49_96mnthsY$TotalParticipant))
        countryMale_0.80to1_49_96mnths = countryMale_0.80to1_49_96mnths+  unique(dat_HIBetween0.80to1_49_96mnthsY$Male)
        countryFemale_0.80to1_49_96mnths = countryFemale_0.80to1_49_96mnths + unique(dat_HIBetween0.80to1_49_96mnthsY$Female)
        countryTotal_0.80to1_49_96mnths = countryTotal_0.80to1_49_96mnths + unique(dat_HIBetween0.80to1_49_96mnthsY$TotalParticipant)
    }
}
percHI_0.80to1_49_96mnths_HI_Male = (countryMale_0.80to1_49_96mnths/countryTotal_0.80to1_49_96mnths)*100
percHI_0.80to1_49_96mnths_HI_Female = (countryFemale_0.80to1_49_96mnths/countryTotal_0.80to1_49_96mnths)*100

HumanIndxBetween0.80to1_GreaterThan97mnths <- datFinal[which((datFinal$HumanIndex >= 0.80 
    & datFinal$HumanIndex <= 1)
	& (datFinal$Sanction_months >= 97)),]
nrow(HumanIndxBetween0.80to1_GreaterThan97mnths)
#countryTotal_0.80to1_GreaterThan97mnths = 0
countryMale_0.80to1_GreaterThan97mnths = 0
countryFemale_0.80to1_GreaterThan97mnths = 0
countryTotal_0.80to1_GreaterThan97mnths = 0
totalpercountry <- c()
uniqCount <- unique(HumanIndxBetween0.80to1_GreaterThan97mnths$Nationality); 
#country <- c(); year <- c(); Participants <- c()
country <- c(); year <- c(); MaleParticipants0.80to1_GreaterThan97mnths <- c(); FemaleParticipants0.80to1_GreaterThan97mnths <- c()
Participants <- c()
for(ii in 1:length(uniqCount)){
    dat_HIBetween0.80to1_GreaterThan97mnths <- HumanIndxBetween0.80to1_GreaterThan97mnths[which(HumanIndxBetween0.80to1_GreaterThan97mnths$Nationality == uniqCount[ii]),]
    uniqyr <- unique(dat_HIBetween0.80to1_GreaterThan97mnths$NearestOlympicYear)
    if(length(uniqyr) > 1){
        for(jj in 1:length(uniqyr)){
            dat_HIBetween0.80to1_GreaterThan97mnthsY <- dat_HIBetween0.80to1_GreaterThan97mnths[which(dat_HIBetween0.80to1_GreaterThan97mnths$NearestOlympicYear == uniqyr[jj]),]
            country <- c(country, uniqCount[ii]); 
            year <- c(year, uniqyr[jj]); 
            MaleParticipants0.80to1_GreaterThan97mnths <- c(MaleParticipants0.80to1_GreaterThan97mnths, unique(dat_HIBetween0.80to1_GreaterThan97mnthsY$Male))
            FemaleParticipants0.80to1_GreaterThan97mnths <- c(FemaleParticipants0.80to1_GreaterThan97mnths, unique(dat_HIBetween0.80to1_GreaterThan97mnthsY$Female))
            #Participants <- c(Participants, unique(dat_HIBetween0.80to1_GreaterThan97mnthsY$TotalParticipant))
            countryMale_0.80to1_GreaterThan97mnths = countryMale_0.80to1_GreaterThan97mnths+  unique(dat_HIBetween0.80to1_GreaterThan97mnthsY$Male)
            countryFemale_0.80to1_GreaterThan97mnths = countryFemale_0.80to1_GreaterThan97mnths + unique(dat_HIBetween0.80to1_GreaterThan97mnthsY$Female)
            countryTotal_0.80to1_GreaterThan97mnths = countryTotal_0.80to1_GreaterThan97mnths + unique(dat_HIBetween0.80to1_GreaterThan97mnthsY$TotalParticipant)
        }
    }else{
        dat_HIBetween0.80to1_GreaterThan97mnthsY <- dat_HIBetween0.80to1_GreaterThan97mnths[which(dat_HIBetween0.80to1_GreaterThan97mnths$NearestOlympicYear == uniqyr),]
        country <- c(country, uniqCount[ii]); 
        year <- c(year, uniqyr); 
        MaleParticipants0.80to1_GreaterThan97mnths <- c(MaleParticipants0.80to1_GreaterThan97mnths, unique(dat_HIBetween0.80to1_GreaterThan97mnthsY$Male))
        FemaleParticipants0.80to1_GreaterThan97mnths <- c(FemaleParticipants0.80to1_GreaterThan97mnths, unique(dat_HIBetween0.80to1_GreaterThan97mnthsY$Female))
        #Participants <- c(Participants, unique(dat_HIBetween0.80to1_GreaterThan97mnthsY$TotalParticipant))
        countryMale_0.80to1_GreaterThan97mnths = countryMale_0.80to1_GreaterThan97mnths+  unique(dat_HIBetween0.80to1_GreaterThan97mnthsY$Male)
        countryFemale_0.80to1_GreaterThan97mnths = countryFemale_0.80to1_GreaterThan97mnths + unique(dat_HIBetween0.80to1_GreaterThan97mnthsY$Female)
        countryTotal_0.80to1_GreaterThan97mnths = countryTotal_0.80to1_GreaterThan97mnths + unique(dat_HIBetween0.80to1_GreaterThan97mnthsY$TotalParticipant)
    }
}
percHI_0.80to1_GreaterThan97mnths_HI_Male = (countryMale_0.80to1_GreaterThan97mnths/countryTotal_0.80to1_GreaterThan97mnths)*100
percHI_0.80to1_GreaterThan97mnths_HI_Female = (countryFemale_0.80to1_GreaterThan97mnths/countryTotal_0.80to1_GreaterThan97mnths)*100
datHI_SP<- data.frame(HumanIndex = c(
            "HI_0.35to0.549",
			"HI_0.35to0.549",
			"HI_0.35to0.549",
			"HI_0.35to0.549",
			"HI_0.35to0.549",
			"HI_0.35to0.549",
			"HI_0.35to0.549",
			"HI_0.35to0.549",
			"HI_0.55to0.699",
			"HI_0.55to0.699",
			"HI_0.55to0.699",
			"HI_0.55to0.699",
			"HI_0.55to0.699",
			"HI_0.55to0.699",
			"HI_0.55to0.699",
			"HI_0.55to0.699",
			"HI_0.70to0.799",
			"HI_0.70to0.799",
			"HI_0.70to0.799",
			"HI_0.70to0.799",
			"HI_0.70to0.799",
			"HI_0.70to0.799",
			"HI_0.70to0.799",
			"HI_0.70to0.799",
			"HI_0.8to1",
			"HI_0.8to1",
			"HI_0.8to1",
			"HI_0.8to1",
			"HI_0.8to1",
			"HI_0.8to1",
			"HI_0.8to1",
			"HI_0.8to1"),
		Sex =c("Male", "Female","Male", "Female","Male", "Female","Male", "Female",
		       "Male", "Female","Male", "Female","Male", "Female","Male", "Female",
			   "Male", "Female","Male", "Female","Male", "Female","Male", "Female",
			   "Male", "Female","Male", "Female","Male", "Female","Male", "Female"),
    TotalMaleAthletes = c(countryMale_0.35to0.549_LessThan24mnths, countryFemale_0.35to0.549_LessThan24mnths,
	                      countryMale_0.35to0.549_25_48mnths, countryFemale_0.35to0.549_25_48mnths,
						  countryMale_0.35to0.549_49_96mnths, countryFemale_0.35to0.549_49_96mnths,
						  countryMale_0.35to0.549_GreaterThan97mnths, countryFemale_0.35to0.549_GreaterThan97mnths,
						  countryMale_0.55to0.699_LessThan24mnths, countryFemale_0.55to0.699_LessThan24mnths,
	                      countryMale_0.55to0.699_25_48mnths, countryFemale_0.55to0.699_25_48mnths,
						  countryMale_0.55to0.699_49_96mnths, countryFemale_0.55to0.699_49_96mnths,
						  countryMale_0.55to0.699_GreaterThan97mnths, countryFemale_0.55to0.699_GreaterThan97mnths,
                          countryMale_0.70to0.799_LessThan24mnths, countryFemale_0.70to0.799_LessThan24mnths,
	                      countryMale_0.70to0.799_25_48mnths, countryFemale_0.70to0.799_25_48mnths,
						  countryMale_0.70to0.799_49_96mnths, countryFemale_0.70to0.799_49_96mnths,
						  countryMale_0.70to0.799_GreaterThan97mnths, countryFemale_0.70to0.799_GreaterThan97mnths,
                          countryMale_0.80to1_LessThan24mnths, countryFemale_0.80to1_LessThan24mnths,
	                      countryMale_0.80to1_25_48mnths, countryFemale_0.80to1_25_48mnths,
						  countryMale_0.80to1_49_96mnths, countryFemale_0.80to1_49_96mnths,
						  countryMale_0.80to1_GreaterThan97mnths, countryFemale_0.80to1_GreaterThan97mnths),
    SanctionPercent = c(percHI_0.35to0.549_LessThan24mnths_HI_Male,percHI_0.35to0.549_LessThan24mnths_HI_Female,
	                    percHI_0.35to0.549_25_48mnths_HI_Male,percHI_0.35to0.549_25_48mnths_HI_Female,
						percHI_0.35to0.549_49_96mnths_HI_Male,percHI_0.35to0.549_49_96mnths_HI_Female,
						percHI_0.35to0.549_GreaterThan97mnths_HI_Male,percHI_0.35to0.549_GreaterThan97mnths_HI_Female,
						percHI_0.55to0.699_LessThan24mnths_HI_Male,percHI_0.55to0.699_LessThan24mnths_HI_Female,
	                    percHI_0.55to0.699_25_48mnths_HI_Male,percHI_0.55to0.699_25_48mnths_HI_Female,
						percHI_0.55to0.699_49_96mnths_HI_Male,percHI_0.55to0.699_49_96mnths_HI_Female,
						percHI_0.55to0.699_GreaterThan97mnths_HI_Male,percHI_0.55to0.699_GreaterThan97mnths_HI_Female,
                        percHI_0.70to0.799_LessThan24mnths_HI_Male,percHI_0.70to0.799_LessThan24mnths_HI_Female,
	                    percHI_0.70to0.799_25_48mnths_HI_Male,percHI_0.70to0.799_25_48mnths_HI_Female,
						percHI_0.70to0.799_49_96mnths_HI_Male,percHI_0.70to0.799_49_96mnths_HI_Female,
						percHI_0.70to0.799_GreaterThan97mnths_HI_Male,percHI_0.70to0.799_GreaterThan97mnths_HI_Female,
						percHI_0.80to1_LessThan24mnths_HI_Male,percHI_0.80to1_LessThan24mnths_HI_Female,
	                    percHI_0.80to1_25_48mnths_HI_Male,percHI_0.80to1_25_48mnths_HI_Female,
						percHI_0.80to1_49_96mnths_HI_Male,percHI_0.80to1_49_96mnths_HI_Female,
						percHI_0.80to1_GreaterThan97mnths_HI_Male,percHI_0.80to1_GreaterThan97mnths_HI_Female),
	SanctionPeriod = c("<=24mnths", "<=24mnths", "25_48mnths", "25_48mnths", "49_96mnths", "49_96mnths", ">=97mnths", ">=97mnths",
	                   "<=24mnths", "<=24mnths", "25_48mnths", "25_48mnths", "49_96mnths", "49_96mnths", ">=97mnths", ">=97mnths",
					   "<=24mnths", "<=24mnths", "25_48mnths", "25_48mnths", "49_96mnths", "49_96mnths", ">=97mnths", ">=97mnths", 
					   "<=24mnths", "<=24mnths", "25_48mnths", "25_48mnths", "49_96mnths", "49_96mnths", ">=97mnths", ">=97mnths")
)
write.csv(datHI_M_F, file.path(outdir,"humanIndexMale_Female_02192020_plotReady.csv"), row.names = FALSE)
       

'library(ggplot2)
p11 <- ggplot(data = datHI_SP, aes(x = factor(HumanIndex, levels=unique(HumanIndex)), y= SanctionPercent, fill = factor(SanctionPeriod, level = unique(SanctionPeriod)))) + geom_bar(stat = "identity", position="dodge")+ labs(x = "SanctionPeriod", y = "Percentage of Athletes", fill = "Global Classifications") + theme(legend.title = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.title.x = element_text(face = "bold", siZ:e = 50),axis.title.y = element_text(face = "bold", siZ:e = 50), axis.text.x = element_text(face="bold", siZ:e = 30, angle = 45, vjust = 0.5), axis.text.y = element_text(face="bold", siZ:e = 30), legend.text=element_text(face = "bold",siZ:e=25)) + scale_fill_manual(values=c("green","red", "blue", "yellow")) 
p11'

 p8 <- ggplot(data = datHI_SP, aes(x = factor(SanctionPeriod, levels=unique(SanctionPeriod)), y= SanctionPercent, fill = factor(Sex, level = c("Male", "Female")))) + geom_bar(stat = "identity", position="stack")+ labs(x = "SanctionPeriod", y = "% of Sanctioned Athletes", fill = "Human Index") + theme(legend.title = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.title.x = element_text(face = "bold", size = 50),axis.title.y = element_text(face = "bold", size = 50), axis.text.x = element_text(face="bold", size = 30, angle = 45, hjust = 1), axis.text.y = element_text(face="bold", size = 30), legend.text=element_text(face = "bold",size=25),  strip.text = element_text(size = 25))  +  facet_wrap( ~ factor(HumanIndex , level = c("HI_0.35to0.549","HI_0.55to0.699", "HI_0.70to0.799", "HI_0.8to1"))) + scale_fill_manual(values=(c("green", "hotpink")))
p8
#EconomicClass
outdir = "Z:/Suro/AtheleteSanction/AtheleteSanction05262021/"
datFinal <- read.csv("Z:/Suro/AtheleteSanction/AtheleteSanction05262021/AthleteInfo_12282021.csv")
r <-read.csv("Z:/Suro/AtheleteSanction/AtheleteSanction05262021/Athlete_Countries_10-27-2021.csv")
EconomicClass_LIC <- datFinal[which(datFinal$EconomicClass  == "LIC"),]
nrow(EconomicClass_LIC)
countryTotal_LIC = 0
uniqCount <- unique(EconomicClass_LIC$Nationality); 
country <- c(); year <- c(); Participants <- c()
for(ii in 1:length(uniqCount)){
    dat_EC_LIC <- EconomicClass_LIC[which(EconomicClass_LIC$Nationality == uniqCount[ii]),]
	uniqyr <- unique(dat_EC_LIC$NearestOlympicYear)
	dat_EC_LICY <- r[which(r$NationAbbr == uniqCount[ii]),]
	countryTotal_LIC = countryTotal_LIC + unique(dat_EC_LICY$Country_Total)
	print(uniqCount[ii])
	print(countryTotal_LIC)
	'if(length(uniqyr) > 1){
	    for(jj in 1:length(uniqyr)){
		    dat_EC_LICY <- dat_EC_LIC[which(dat_EC_LIC$NearestOlympicYear == uniqyr[jj]),]
			country <- c(country, uniqCount[ii]); 
			year <- c(year, uniqyr[jj]); 
			Participants <- c(Participants, unique(dat_EC_LICY$TotalParticipant ))
		    countryTotal_LIC = countryTotal_LIC + unique(dat_EC_LICY$TotalParticipant )
        }
	}else{
	    dat_EC_LICY <- dat_EC_LIC[which(dat_EC_LIC$NearestOlympicYear == uniqyr),]
		country <- c(country, uniqCount[ii]); 
		year <- c(year, uniqyr); 
		Participants <- c(Participants, unique(dat_EC_LICY$TotalParticipant ))
		countryTotal_LIC = countryTotal_LIC + unique(dat_EC_LICY$TotalParticipant )
	}'
}
percEC_LIC = (nrow(EconomicClass_LIC)/countryTotal_LIC)*100
EconomicClass_MC <- datFinal[which(datFinal$EconomicClass == "UMC" | datFinal$EconomicClass == "LMC"),]
nrow(EconomicClass_MC)
countryTotal_MC = 0
uniqCount <- unique(EconomicClass_MC$Nationality); 
country <- c(); year <- c(); Participants <- c()
for(ii in 1:length(uniqCount)){
    dat_EC_MC <- EconomicClass_MC[which(EconomicClass_MC$Nationality == uniqCount[ii]),]
	uniqyr <- unique(dat_EC_MC$NearestOlympicYear)
	dat_EC_MCY <- r[which(r$NationAbbr == uniqCount[ii]),]
	countryTotal_MC = countryTotal_MC + unique(dat_EC_MCY$Country_Total)
	print(uniqCount[ii])
	print(countryTotal_MC)
	'if(length(uniqyr) > 1){
	    for(jj in 1:length(uniqyr)){
		    dat_EC_UMCY <- dat_EC_UMC[which(dat_EC_UMC$NearestOlympicYear == uniqyr[jj]),]
			country <- c(country, uniqCount[ii]); 
			year <- c(year, uniqyr[jj]); 
			Participants <- c(Participants, unique(dat_EC_UMCY$TotalParticipant ))
		    countryTotal_UMC = countryTotal_UMC + unique(dat_EC_UMCY$TotalParticipant )
        }
	}else{
	    dat_EC_UMCY <- dat_EC_UMC[which(dat_EC_UMC$NearestOlympicYear == uniqyr),]
		country <- c(country, uniqCount[ii]); 
		year <- c(year, uniqyr); 
		Participants <- c(Participants, unique(dat_EC_UMCY$TotalParticipant ))
		countryTotal_UMC = countryTotal_UMC + unique(dat_EC_UMCY$TotalParticipant )
	}'
}
percEC_MC = (nrow(EconomicClass_MC)/countryTotal_MC)*100
EconomicClass_HIC <- datFinal[which(datFinal$EconomicClass  == "HIC"),]
nrow(EconomicClass_HIC)
countryTotal_HIC = 0
uniqCount <- unique(EconomicClass_HIC$Nationality); 
country <- c(); year <- c(); Participants <- c()
for(ii in 1:length(uniqCount)){
    dat_EC_HIC <- EconomicClass_HIC[which(EconomicClass_HIC$Nationality == uniqCount[ii]),]
	uniqyr <- unique(dat_EC_HIC$NearestOlympicYear)
	dat_EC_HICY <- r[which(r$NationAbbr == uniqCount[ii]),]
	countryTotal_HIC = countryTotal_HIC + unique(dat_EC_HICY$Country_Total)
	print(uniqCount[ii])
	print(countryTotal_HIC)
	'if(length(uniqyr) > 1){
	    for(jj in 1:length(uniqyr)){
		    dat_EC_HICY <- dat_EC_HIC[which(dat_EC_HIC$NearestOlympicYear == uniqyr[jj]),]
			country <- c(country, uniqCount[ii]); 
			year <- c(year, uniqyr[jj]); 
			Participants <- c(Participants, unique(dat_EC_HICY$TotalParticipant ))
		    countryTotal_HIC = countryTotal_HIC + unique(dat_EC_HICY$TotalParticipant )
        }
	}else{
	    dat_EC_HICY <- dat_EC_HIC[which(dat_EC_HIC$NearestOlympicYear == uniqyr),]
		country <- c(country, uniqCount[ii]); 
		year <- c(year, uniqyr); 
		Participants <- c(Participants, unique(dat_EC_HICY$TotalParticipant ))
		countryTotal_HIC = countryTotal_HIC + unique(dat_EC_HICY$TotalParticipant )
	}'
}
percEC_HIC = (nrow(EconomicClass_HIC)/countryTotal_HIC)*100
'EconomicClass_LMC <- datFinal[which(datFinal$EconomicClass  == "LMC"),]
nrow(EconomicClass_LMC)
countryTotal_LMC = 0
uniqCount <- unique(EconomicClass_LMC$Nationality); 
country <- c(); year <- c(); Participants <- c()
for(ii in 1:length(uniqCount)){
    dat_EC_LMC <- EconomicClass_LMC[which(EconomicClass_LMC$Nationality == uniqCount[ii]),]
	uniqyr <- unique(dat_EC_LMC$NearestOlympicYear)
	if(length(uniqyr) > 1){
	    for(jj in 1:length(uniqyr)){
		    dat_EC_LMCY <- dat_EC_LMC[which(dat_EC_LMC$NearestOlympicYear == uniqyr[jj]),]
			country <- c(country, uniqCount[ii]); 
			year <- c(year, uniqyr[jj]); 
			Participants <- c(Participants, unique(dat_EC_LMCY$TotalParticipant ))
		    countryTotal_LMC = countryTotal_LMC + unique(dat_EC_LMCY$TotalParticipant )
        }
	}else{
	    dat_EC_LMCY <- dat_EC_LMC[which(dat_EC_LMC$NearestOlympicYear == uniqyr),]
		country <- c(country, uniqCount[ii]); 
		year <- c(year, uniqyr); 
		Participants <- c(Participants, unique(dat_EC_LMCY$TotalParticipant ))
		countryTotal_LMC = countryTotal_LMC + unique(dat_EC_LMCY$TotalParticipant )
	}
}'
#percEC_LMC = (nrow(EconomicClass_LMC)/countryTotal_LMC)*100
datEC<- data.frame(EconomicClass  = c(
            "EC_LIC",
			"EC_MC", 
			"EC_HIC"),
        NumberofAthletes = c(
	        nrow(EconomicClass_LIC),
            nrow(EconomicClass_MC),			
	        nrow(EconomicClass_HIC)),
	    TotalAthletes = c(countryTotal_LIC,
		    countryTotal_MC,
			countryTotal_HIC),
        SanctionPercent = c(percEC_LIC,
		    percEC_MC,
			percEC_HIC))
p13 <- ggplot(data = datEC, aes(x = factor(EconomicClass , levels=unique(EconomicClass)), y= SanctionPercent, fill = factor(EconomicClass , levels=unique(EconomicClass )))) + geom_bar(stat = "identity")+ labs(x = "EconomicClass", y = "% of Sanctioned Athletes") + theme(legend.title = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.title.x = element_text(face = "bold", size = 50),axis.title.y = element_text(face = "bold", size = 50), axis.text.x = element_text(face="bold", size = 30, angle = 45, hjust = 1), axis.text.y = element_text(face="bold", size = 30), legend.text=element_text(face = "bold",size=25)) + scale_fill_manual(values=c("purple","orange", "brown")) 
p13

#EconomicClass 
outdir = "Z:/Suro/AtheleteSanction/AtheleteSanction05262021/"
datFinal <- read.csv("Z:/Suro/AtheleteSanction/AtheleteSanction05262021/AthleteInfo_12282021.csv")
r <-read.csv("Z:/Suro/AtheleteSanction/AtheleteSanction05262021/Athlete_Countries_10-27-2021.csv")
EconomicClass_LIC_LessThan24mnths <- datFinal[which((datFinal$EconomicClass  == "LIC")
	& datFinal$Sanction_months <= 24),]
nrow(EconomicClass_LIC_LessThan24mnths)
countryTotal_LIC_LessThan24mnths = 0
uniqCount <- unique(EconomicClass_LIC_LessThan24mnths$Nationality); 
country <- c(); year <- c(); Participants <- c()
for(ii in 1:length(uniqCount)){
    dat_EC_LIC_LessThan24mnths <- EconomicClass_LIC_LessThan24mnths[which(EconomicClass_LIC_LessThan24mnths$Nationality == uniqCount[ii]),]
	uniqyr <- unique(dat_EC_LIC_LessThan24mnths$NearestOlympicYear)
	dat_EC_LIC_LessThan24mnthsY <- r[which(r$NationAbbr == uniqCount[ii]),]
	countryTotal_LIC_LessThan24mnths = countryTotal_LIC_LessThan24mnths + unique(dat_EC_LIC_LessThan24mnthsY$Country_Total)
	print(uniqCount[ii])
	print(countryTotal_LIC_LessThan24mnths)
	'if(length(uniqyr) > 1){
	    for(jj in 1:length(uniqyr)){
		    dat_EC_LIC_LessThan24mnthsY <- dat_EC_LIC_LessThan24mnths[which(dat_EC_LIC_LessThan24mnths$NearestOlympicYear == uniqyr[jj]),]
			country <- c(country, uniqCount[ii]); 
			year <- c(year, uniqyr[jj]); 
			Participants <- c(Participants, unique(dat_EC_LIC_LessThan24mnthsY$TotalParticipant ))
		    countryTotal_LIC_LessThan24mnths = countryTotal_LIC_LessThan24mnths + unique(dat_EC_LIC_LessThan24mnthsY$TotalParticipant )
        }
	}else{
	    dat_EC_LIC_LessThan24mnthsY <- dat_EC_LIC_LessThan24mnths[which(dat_EC_LIC_LessThan24mnths$NearestOlympicYear == uniqyr),]
		country <- c(country, uniqCount[ii]); 
		year <- c(year, uniqyr); 
		Participants <- c(Participants, unique(dat_EC_LIC_LessThan24mnthsY$TotalParticipant ))
		countryTotal_LIC_LessThan24mnths = countryTotal_LIC_LessThan24mnths + unique(dat_EC_LIC_LessThan24mnthsY$TotalParticipant )
	}'
}
percEC_LIC_LessThan24mnths = (nrow(EconomicClass_LIC_LessThan24mnths)/countryTotal_LIC_LessThan24mnths)*100

EconomicClass_LIC_25_48mnths <- datFinal[which((datFinal$EconomicClass == "LIC")
	& (datFinal$Sanction_months >= 25 
	& datFinal$Sanction_months <= 48)),]
nrow(EconomicClass_LIC_25_48mnths)
countryTotal_LIC_25_48mnths = 0
uniqCount <- unique(EconomicClass_LIC_25_48mnths$Nationality); 
country <- c(); year <- c(); Participants <- c()
for(ii in 1:length(uniqCount)){
    dat_EC_LIC_25_48mnths <- EconomicClass_LIC_25_48mnths[which(EconomicClass_LIC_25_48mnths$Nationality == uniqCount[ii]),]
	uniqyr <- unique(dat_EC_LIC_25_48mnths$NearestOlympicYear)
	dat_EC_LIC_25_48mnthsY <- r[which(r$NationAbbr == uniqCount[ii]),]
	countryTotal_LIC_25_48mnths = countryTotal_LIC_25_48mnths + unique(dat_EC_LIC_25_48mnthsY$Country_Total)
	print(uniqCount[ii])
	print(countryTotal_LIC_25_48mnths)
	'if(length(uniqyr) > 1){
	    for(jj in 1:length(uniqyr)){
		    dat_EC_LIC_25_48mnthsY <- dat_EC_LIC_25_48mnths[which(dat_EC_LIC_25_48mnths$NearestOlympicYear == uniqyr[jj]),]
			country <- c(country, uniqCount[ii]); 
			year <- c(year, uniqyr[jj]); 
			Participants <- c(Participants, unique(dat_EC_LIC_25_48mnthsY$TotalParticipant ))
		    countryTotal_LIC_25_48mnths = countryTotal_LIC_25_48mnths + unique(dat_EC_LIC_25_48mnthsY$TotalParticipant )
        }
	}else{
	    dat_EC_LIC_25_48mnthsY <- dat_EC_LIC_25_48mnths[which(dat_EC_LIC_25_48mnths$NearestOlympicYear == uniqyr),]
		country <- c(country, uniqCount[ii]); 
		year <- c(year, uniqyr); 
		Participants <- c(Participants, unique(dat_EC_LIC_25_48mnthsY$TotalParticipant ))
		countryTotal_LIC_25_48mnths = countryTotal_LIC_25_48mnths + unique(dat_EC_LIC_25_48mnthsY$TotalParticipant )
	}'
}
percEC_LIC_25_48mnths = (nrow(EconomicClass_LIC_25_48mnths)/countryTotal_LIC_25_48mnths)*100


EconomicClass_LIC_49_96mnths <- datFinal[which((datFinal$EconomicClass == "LIC")
	& (datFinal$Sanction_months >= 49 
	& datFinal$Sanction_months <= 96)),]
nrow(EconomicClass_LIC_49_96mnths)
countryTotal_LIC_49_96mnths = 0
uniqCount <- unique(EconomicClass_LIC_49_96mnths$Nationality); 
country <- c(); year <- c(); Participants <- c()
for(ii in 1:length(uniqCount)){
    dat_EC_LIC_49_96mnths <- EconomicClass_LIC_49_96mnths[which(EconomicClass_LIC_49_96mnths$Nationality == uniqCount[ii]),]
	uniqyr <- unique(dat_EC_LIC_49_96mnths$NearestOlympicYear)
	dat_EC_LIC_49_96mnthsY <- r[which(r$NationAbbr == uniqCount[ii]),]
	countryTotal_LIC_49_96mnths = countryTotal_LIC_49_96mnths + unique(dat_EC_LIC_49_96mnthsY$Country_Total)
	print(uniqCount[ii])
	print(countryTotal_LIC_49_96mnths)
	'if(length(uniqyr) > 1){
	    for(jj in 1:length(uniqyr)){
		    dat_EC_LIC_49_96mnthsY <- dat_EC_LIC_49_96mnths[which(dat_EC_LIC_49_96mnths$NearestOlympicYear == uniqyr[jj]),]
			country <- c(country, uniqCount[ii]); 
			year <- c(year, uniqyr[jj]); 
			Participants <- c(Participants, unique(dat_EC_LIC_49_96mnthsY$TotalParticipant ))
		    countryTotal_LIC_49_96mnths = countryTotal_LIC_49_96mnths + unique(dat_EC_LIC_49_96mnthsY$TotalParticipant )
        }
	}else{
	    dat_EC_LIC_49_96mnthsY <- dat_EC_LIC_49_96mnths[which(dat_EC_LIC_49_96mnths$NearestOlympicYear == uniqyr),]
		country <- c(country, uniqCount[ii]); 
		year <- c(year, uniqyr); 
		Participants <- c(Participants, unique(dat_EC_LIC_49_96mnthsY$TotalParticipant ))
		countryTotal_LIC_49_96mnths = countryTotal_LIC_49_96mnths + unique(dat_EC_LIC_49_96mnthsY$TotalParticipant )
	}'
}
percEC_LIC_49_96mnths = (nrow(EconomicClass_LIC_49_96mnths)/countryTotal_LIC_49_96mnths)*100

EconomicClass_LIC_GreaterThan97mnths <- datFinal[which((datFinal$EconomicClass == "LIC")
	& (datFinal$Sanction_months >= 97)),]
nrow(EconomicClass_LIC_GreaterThan97mnths)
countryTotal_LIC_GreaterThan97mnths = 0
uniqCount <- unique(EconomicClass_LIC_GreaterThan97mnths$Nationality);

country <- c(); year <- c(); Participants <- c()
for(ii in 1:length(uniqCount)){
    dat_EC_LIC_GreaterThan97mnthsY <- r[which(r$NationAbbr == uniqCount[ii]),]
    countryTotal_LIC_GreaterThan97mnths = countryTotal_LIC_GreaterThan97mnths + unique(dat_EC_LIC_GreaterThan97mnthsY$Country_Total)
    print(uniqCount[ii])
    print(countryTotal_LIC_GreaterThan97mnths)
    'dat_EC_LIC_GreaterThan97mnths <- EconomicClass_LIC_GreaterThan97mnths[which(EconomicClass_LIC_GreaterThan97mnths$Nationality == uniqCount[ii]),]
	uniqyr <- unique(dat_EC_LIC_GreaterThan97mnths$NearestOlympicYear)
	if(length(uniqyr) > 1){
	    for(jj in 1:length(uniqyr)){
		    dat_EC_LIC_GreaterThan97mnthsY <- dat_EC_LIC_GreaterThan97mnths[which(dat_EC_LIC_GreaterThan97mnths$NearestOlympicYear == uniqyr[jj]),]
			country <- c(country, uniqCount[ii]); 
			year <- c(year, uniqyr[jj]); 
			Participants <- c(Participants, unique(dat_EC_LIC_GreaterThan97mnthsY$TotalParticipant ))
		    countryTotal_LIC_GreaterThan97mnths = countryTotal_LIC_GreaterThan97mnths + unique(dat_EC_LIC_GreaterThan97mnthsY$TotalParticipant )
        }
	}else{
	    dat_EC_LIC_GreaterThan97mnthsY <- dat_EC_LIC_GreaterThan97mnths[which(dat_EC_LIC_GreaterThan97mnths$NearestOlympicYear == uniqyr),]
		country <- c(country, uniqCount[ii]); 
		year <- c(year, uniqyr); 
		Participants <- c(Participants, unique(dat_EC_LIC_GreaterThan97mnthsY$TotalParticipant ))
		countryTotal_LIC_GreaterThan97mnths = countryTotal_LIC_GreaterThan97mnths + unique(dat_EC_LIC_GreaterThan97mnthsY$TotalParticipant )
	}'
}
percEC_LIC_GreaterThan97mnths = (nrow(EconomicClass_LIC_GreaterThan97mnths)/countryTotal_LIC_GreaterThan97mnths)*100

#EconomicClass_UMC <- datFinal[which(datFinal$EconomicClass == "UMC" | datFinal$EconomicClass == "LMC"),]
#nrow(EconomicClass_UMC)
#UMC
EconomicClass_MC_LessThan24mnths <- datFinal[which((datFinal$EconomicClass == "UMC" | datFinal$EconomicClass == "LMC")
	& datFinal$Sanction_months <= 24),]
nrow(EconomicClass_MC_LessThan24mnths)
countryTotal_MC_LessThan24mnths = 0
uniqCount <- unique(EconomicClass_MC_LessThan24mnths$Nationality); 
country <- c(); year <- c(); Participants <- c()
for(ii in 1:length(uniqCount)){
    dat_EC_MC_LessThan24mnths <- EconomicClass_MC_LessThan24mnths[which(EconomicClass_MC_LessThan24mnths$Nationality == uniqCount[ii]),]
	uniqyr <- unique(dat_EC_MC_LessThan24mnths$NearestOlympicYear)
	dat_EC_MC_LessThan24mnthsY <- r[which(r$NationAbbr == uniqCount[ii]),]
    countryTotal_MC_LessThan24mnths = countryTotal_MC_LessThan24mnths + unique(dat_EC_MC_LessThan24mnthsY$Country_Total)
    print(uniqCount[ii])
    print(countryTotal_MC_LessThan24mnths)
	'if(length(uniqyr) > 1){
	    for(jj in 1:length(uniqyr)){
		    dat_EC_MC_LessThan24mnthsY <- dat_EC_MC_LessThan24mnths[which(dat_EC_MC_LessThan24mnths$NearestOlympicYear == uniqyr[jj]),]
			country <- c(country, uniqCount[ii]); 
			year <- c(year, uniqyr[jj]); 
			Participants <- c(Participants, unique(dat_EC_MC_LessThan24mnthsY$TotalParticipant ))
		    countryTotal_MC_LessThan24mnths = countryTotal_MC_LessThan24mnths + unique(dat_EC_MC_LessThan24mnthsY$TotalParticipant )
        }
	}else{
	    dat_EC_MC_LessThan24mnthsY <- dat_EC_MC_LessThan24mnths[which(dat_EC_MC_LessThan24mnths$NearestOlympicYear == uniqyr),]
		country <- c(country, uniqCount[ii]); 
		year <- c(year, uniqyr); 
		Participants <- c(Participants, unique(dat_EC_MC_LessThan24mnthsY$TotalParticipant ))
		countryTotal_MC_LessThan24mnths = countryTotal_MC_LessThan24mnths + unique(dat_EC_MC_LessThan24mnthsY$TotalParticipant )
	}'
}
percEC_MC_LessThan24mnths = (nrow(EconomicClass_MC_LessThan24mnths)/countryTotal_MC_LessThan24mnths)*100

EconomicClass_MC_25_48mnths <- datFinal[which((datFinal$EconomicClass == "UMC" | datFinal$EconomicClass == "LMC")
	& (datFinal$Sanction_months >= 25 
	& datFinal$Sanction_months <= 48)),]
nrow(EconomicClass_MC_25_48mnths)
countryTotal_MC_25_48mnths = 0
uniqCount <- unique(EconomicClass_MC_25_48mnths$Nationality); 
country <- c(); year <- c(); Participants <- c()
for(ii in 1:length(uniqCount)){
    dat_EC_MC_25_48mnths <- EconomicClass_MC_25_48mnths[which(EconomicClass_MC_25_48mnths$Nationality == uniqCount[ii]),]
	uniqyr <- unique(dat_EC_MC_25_48mnths$NearestOlympicYear)
	dat_EC_MC_25_48mnthsY <- r[which(r$NationAbbr == uniqCount[ii]),]
    countryTotal_MC_25_48mnths = countryTotal_MC_25_48mnths + unique(dat_EC_MC_25_48mnthsY$Country_Total)
    print(uniqCount[ii])
    print(countryTotal_MC_25_48mnths)
	'if(length(uniqyr) > 1){
	    for(jj in 1:length(uniqyr)){
		    dat_EC_MC_25_48mnthsY <- dat_EC_MC_25_48mnths[which(dat_EC_MC_25_48mnths$NearestOlympicYear == uniqyr[jj]),]
			country <- c(country, uniqCount[ii]); 
			year <- c(year, uniqyr[jj]); 
			Participants <- c(Participants, unique(dat_EC_MC_25_48mnthsY$TotalParticipant ))
		    countryTotal_MC_25_48mnths = countryTotal_MC_25_48mnths + unique(dat_EC_MC_25_48mnthsY$TotalParticipant )
        }
	}else{
	    dat_EC_MC_25_48mnthsY <- dat_EC_MC_25_48mnths[which(dat_EC_MC_25_48mnths$NearestOlympicYear == uniqyr),]
		country <- c(country, uniqCount[ii]); 
		year <- c(year, uniqyr); 
		Participants <- c(Participants, unique(dat_EC_MC_25_48mnthsY$TotalParticipant ))
		countryTotal_MC_25_48mnths = countryTotal_MC_25_48mnths + unique(dat_EC_MC_25_48mnthsY$TotalParticipant )
	}'
}
percEC_MC_25_48mnths = (nrow(EconomicClass_MC_25_48mnths)/countryTotal_MC_25_48mnths)*100


EconomicClass_MC_49_96mnths <- datFinal[which((datFinal$EconomicClass == "UMC" | datFinal$EconomicClass == "LMC")
	& (datFinal$Sanction_months >= 49 
	& datFinal$Sanction_months <= 96)),]
nrow(EconomicClass_MC_49_96mnths)
countryTotal_MC_49_96mnths = 0
uniqCount <- unique(EconomicClass_MC_49_96mnths$Nationality); 
country <- c(); year <- c(); Participants <- c()
for(ii in 1:length(uniqCount)){
    dat_EC_MC_49_96mnths <- EconomicClass_MC_49_96mnths[which(EconomicClass_MC_49_96mnths$Nationality == uniqCount[ii]),]
	uniqyr <- unique(dat_EC_MC_49_96mnths$NearestOlympicYear)
	dat_EC_MC_49_96mnthsY <- r[which(r$NationAbbr == uniqCount[ii]),]
    countryTotal_MC_49_96mnths = countryTotal_MC_49_96mnths + unique(dat_EC_MC_49_96mnthsY$Country_Total)
    print(uniqCount[ii])
    print(countryTotal_MC_49_96mnths)
	'if(length(uniqyr) > 1){
	    for(jj in 1:length(uniqyr)){
		    dat_EC_MC_49_96mnthsY <- dat_EC_MC_49_96mnths[which(dat_EC_MC_49_96mnths$NearestOlympicYear == uniqyr[jj]),]
			country <- c(country, uniqCount[ii]); 
			year <- c(year, uniqyr[jj]); 
			Participants <- c(Participants, unique(dat_EC_MC_49_96mnthsY$TotalParticipant ))
		    countryTotal_MC_49_96mnths = countryTotal_MC_49_96mnths + unique(dat_EC_MC_49_96mnthsY$TotalParticipant )
        }
	}else{
	    dat_EC_MC_49_96mnthsY <- dat_EC_MC_49_96mnths[which(dat_EC_MC_49_96mnths$NearestOlympicYear == uniqyr),]
		country <- c(country, uniqCount[ii]); 
		year <- c(year, uniqyr); 
		Participants <- c(Participants, unique(dat_EC_MC_49_96mnthsY$TotalParticipant ))
		countryTotal_MC_49_96mnths = countryTotal_MC_49_96mnths + unique(dat_EC_MC_49_96mnthsY$TotalParticipant )
	}'
}
percEC_MC_49_96mnths = (nrow(EconomicClass_MC_49_96mnths)/countryTotal_MC_49_96mnths)*100

EconomicClass_MC_GreaterThan97mnths <- datFinal[which((datFinal$EconomicClass == "UMC" | datFinal$EconomicClass == "LMC")
	& (datFinal$Sanction_months >= 97)),]
nrow(EconomicClass_MC_GreaterThan97mnths)
countryTotal_MC_GreaterThan97mnths = 0
uniqCount <- unique(EconomicClass_MC_GreaterThan97mnths$Nationality); 
country <- c(); year <- c(); Participants <- c()
for(ii in 1:length(uniqCount)){
    dat_EC_MC_GreaterThan97mnths <- EconomicClass_MC_GreaterThan97mnths[which(EconomicClass_MC_GreaterThan97mnths$Nationality == uniqCount[ii]),]
	uniqyr <- unique(dat_EC_MC_GreaterThan97mnths$NearestOlympicYear)
	dat_EC_MC_GreaterThan97mnthsY <- r[which(r$NationAbbr == uniqCount[ii]),]
    countryTotal_MC_GreaterThan97mnths = countryTotal_MC_GreaterThan97mnths + unique(dat_EC_MC_GreaterThan97mnthsY$Country_Total)
    print(uniqCount[ii])
    print(countryTotal_MC_GreaterThan97mnths)
	'if(length(uniqyr) > 1){
	    for(jj in 1:length(uniqyr)){
		    dat_EC_MC_GreaterThan97mnthsY <- dat_EC_MC_GreaterThan97mnths[which(dat_EC_MC_GreaterThan97mnths$NearestOlympicYear == uniqyr[jj]),]
			country <- c(country, uniqCount[ii]); 
			year <- c(year, uniqyr[jj]); 
			Participants <- c(Participants, unique(dat_EC_MC_GreaterThan97mnthsY$TotalParticipant ))
		    countryTotal_MC_GreaterThan97mnths = countryTotal_MC_GreaterThan97mnths + unique(dat_EC_MC_GreaterThan97mnthsY$TotalParticipant )
        }
	}else{
	    dat_EC_MC_GreaterThan97mnthsY <- dat_EC_MC_GreaterThan97mnths[which(dat_EC_MC_GreaterThan97mnths$NearestOlympicYear == uniqyr),]
		country <- c(country, uniqCount[ii]); 
		year <- c(year, uniqyr); 
		Participants <- c(Participants, unique(dat_EC_MC_GreaterThan97mnthsY$TotalParticipant ))
		countryTotal_MC_GreaterThan97mnths = countryTotal_MC_GreaterThan97mnths + unique(dat_EC_MC_GreaterThan97mnthsY$TotalParticipant )
	}'
}
percEC_MC_GreaterThan97mnths = (nrow(EconomicClass_MC_GreaterThan97mnths)/countryTotal_MC_GreaterThan97mnths)*100

EconomicClass_HIC <- datFinal[which(datFinal$EconomicClass  == "HIC"),]
nrow(EconomicClass_HIC)
#HIC
EconomicClass_HIC_LessThan24mnths <- datFinal[which((datFinal$EconomicClass  == "HIC")
	& datFinal$Sanction_months <= 24),]
nrow(EconomicClass_HIC_LessThan24mnths)
countryTotal_HIC_LessThan24mnths = 0
uniqCount <- unique(EconomicClass_HIC_LessThan24mnths$Nationality); 
country <- c(); year <- c(); Participants <- c()
for(ii in 1:length(uniqCount)){
    dat_EC_HIC_LessThan24mnths <- EconomicClass_HIC_LessThan24mnths[which(EconomicClass_HIC_LessThan24mnths$Nationality == uniqCount[ii]),]
	uniqyr <- unique(dat_EC_HIC_LessThan24mnths$NearestOlympicYear)
	dat_EC_HIC_LessThan24mnthsY <- r[which(r$NationAbbr == uniqCount[ii]),]
    countryTotal_HIC_LessThan24mnths = countryTotal_HIC_LessThan24mnths + unique(dat_EC_HIC_LessThan24mnthsY$Country_Total)
    print(uniqCount[ii])
    print(countryTotal_HIC_LessThan24mnths)
	'if(length(uniqyr) > 1){
	if(length(uniqyr) > 1){
	    for(jj in 1:length(uniqyr)){
		    dat_EC_HIC_LessThan24mnthsY <- dat_EC_HIC_LessThan24mnths[which(dat_EC_HIC_LessThan24mnths$NearestOlympicYear == uniqyr[jj]),]
			country <- c(country, uniqCount[ii]); 
			year <- c(year, uniqyr[jj]); 
			Participants <- c(Participants, unique(dat_EC_HIC_LessThan24mnthsY$TotalParticipant ))
		    countryTotal_HIC_LessThan24mnths = countryTotal_HIC_LessThan24mnths + unique(dat_EC_HIC_LessThan24mnthsY$TotalParticipant )
        }
	}else{
	    dat_EC_HIC_LessThan24mnthsY <- dat_EC_HIC_LessThan24mnths[which(dat_EC_HIC_LessThan24mnths$NearestOlympicYear == uniqyr),]
		country <- c(country, uniqCount[ii]); 
		year <- c(year, uniqyr); 
		Participants <- c(Participants, unique(dat_EC_HIC_LessThan24mnthsY$TotalParticipant ))
		countryTotal_HIC_LessThan24mnths = countryTotal_HIC_LessThan24mnths + unique(dat_EC_HIC_LessThan24mnthsY$TotalParticipant )
	}'
}
percEC_HIC_LessThan24mnths = (nrow(EconomicClass_HIC_LessThan24mnths)/countryTotal_HIC_LessThan24mnths)*100

EconomicClass_HIC_25_48mnths<- datFinal[which((datFinal$EconomicClass  == "HIC")
	& (datFinal$Sanction_months >= 25 
	& datFinal$Sanction_months <= 48)),]
nrow(EconomicClass_HIC_25_48mnths)
countryTotal_HIC_25_48mnths= 0
uniqCount <- unique(EconomicClass_HIC_25_48mnths$Nationality); 
country <- c(); year <- c(); Participants <- c()
for(ii in 1:length(uniqCount)){
    dat_EC_HIC_25_48mnths<- EconomicClass_HIC_25_48mnths[which(EconomicClass_HIC_25_48mnths$Nationality == uniqCount[ii]),]
	uniqyr <- unique(dat_EC_HIC_25_48mnths$NearestOlympicYear)
	dat_EC_HIC_25_48mnthsY <- r[which(r$NationAbbr == uniqCount[ii]),]
    countryTotal_HIC_25_48mnths = countryTotal_HIC_25_48mnths + unique(dat_EC_HIC_25_48mnthsY$Country_Total)
    print(uniqCount[ii])
    print(countryTotal_HIC_25_48mnths)
	'if(length(uniqyr) > 1){
	    for(jj in 1:length(uniqyr)){
		    dat_EC_HIC_25_48mnthsY <- dat_EC_HIC_25_48mnths[which(dat_EC_HIC_25_48mnths$NearestOlympicYear == uniqyr[jj]),]
			country <- c(country, uniqCount[ii]); 
			year <- c(year, uniqyr[jj]); 
			Participants <- c(Participants, unique(dat_EC_HIC_25_48mnthsY$TotalParticipant ))
		    countryTotal_HIC_25_48mnths= countryTotal_HIC_25_48mnths+ unique(dat_EC_HIC_25_48mnthsY$TotalParticipant )
        }
	}else{
	    dat_EC_HIC_25_48mnthsY <- dat_EC_HIC_25_48mnths[which(dat_EC_HIC_25_48mnths$NearestOlympicYear == uniqyr),]
		country <- c(country, uniqCount[ii]); 
		year <- c(year, uniqyr); 
		Participants <- c(Participants, unique(dat_EC_HIC_25_48mnthsY$TotalParticipant ))
		countryTotal_HIC_25_48mnths= countryTotal_HIC_25_48mnths+ unique(dat_EC_HIC_25_48mnthsY$TotalParticipant )
	}'
}
percEC_HIC_25_48mnths= (nrow(EconomicClass_HIC_25_48mnths)/countryTotal_HIC_25_48mnths)*100

                  
EconomicClass_HIC_49_96mnths <- datFinal[which((datFinal$EconomicClass  == "HIC")
	& (datFinal$Sanction_months >= 49 
	& datFinal$Sanction_months <= 96)),]
nrow(EconomicClass_HIC_49_96mnths)
countryTotal_HIC_49_96mnths = 0
uniqCount <- unique(EconomicClass_HIC_49_96mnths$Nationality); 
country <- c(); year <- c(); Participants <- c()
for(ii in 1:length(uniqCount)){
    dat_EC_HIC_49_96mnths <- EconomicClass_HIC_49_96mnths[which(EconomicClass_HIC_49_96mnths$Nationality == uniqCount[ii]),]
	uniqyr <- unique(dat_EC_HIC_49_96mnths$NearestOlympicYear)
	dat_EC_HIC_49_96mnthsY <- r[which(r$NationAbbr == uniqCount[ii]),]
    countryTotal_HIC_49_96mnths = countryTotal_HIC_49_96mnths + unique(dat_EC_HIC_49_96mnthsY$Country_Total)
    print(uniqCount[ii])
    print(countryTotal_HIC_49_96mnths)
	'if(length(uniqyr) > 1){
	    for(jj in 1:length(uniqyr)){
		    dat_EC_HIC_49_96mnthsY <- dat_EC_HIC_49_96mnths[which(dat_EC_HIC_49_96mnths$NearestOlympicYear == uniqyr[jj]),]
			country <- c(country, uniqCount[ii]); 
			year <- c(year, uniqyr[jj]); 
			Participants <- c(Participants, unique(dat_EC_HIC_49_96mnthsY$TotalParticipant ))
		    countryTotal_HIC_49_96mnths = countryTotal_HIC_49_96mnths + unique(dat_EC_HIC_49_96mnthsY$TotalParticipant )
        }
	}else{
	    dat_EC_HIC_49_96mnthsY <- dat_EC_HIC_49_96mnths[which(dat_EC_HIC_49_96mnths$NearestOlympicYear == uniqyr),]
		country <- c(country, uniqCount[ii]); 
		year <- c(year, uniqyr); 
		Participants <- c(Participants, unique(dat_EC_HIC_49_96mnthsY$TotalParticipant ))
		countryTotal_HIC_49_96mnths = countryTotal_HIC_49_96mnths + unique(dat_EC_HIC_49_96mnthsY$TotalParticipant )
	}'
}
percEC_HIC_49_96mnths = (nrow(EconomicClass_HIC_49_96mnths)/countryTotal_HIC_49_96mnths)*100

EconomicClass_HIC_GreaterThan97mnths <- datFinal[which((datFinal$EconomicClass  == "HIC")
	& (datFinal$Sanction_months >= 97)),]
nrow(EconomicClass_HIC_GreaterThan97mnths)
countryTotal_HIC_GreaterThan97mnths = 0
uniqCount <- unique(EconomicClass_HIC_GreaterThan97mnths$Nationality); 
country <- c(); year <- c(); Participants <- c()
for(ii in 1:length(uniqCount)){
    dat_EC_HIC_GreaterThan97mnths <- EconomicClass_HIC_GreaterThan97mnths[which(EconomicClass_HIC_GreaterThan97mnths$Nationality == uniqCount[ii]),]
	uniqyr <- unique(dat_EC_HIC_GreaterThan97mnths$NearestOlympicYear)
	dat_EC_HIC_GreaterThan97mnthsY <- r[which(r$NationAbbr == uniqCount[ii]),]
    countryTotal_HIC_GreaterThan97mnths = countryTotal_HIC_GreaterThan97mnths + unique(dat_EC_HIC_GreaterThan97mnthsY$Country_Total)
    print(uniqCount[ii])
    print(countryTotal_HIC_GreaterThan97mnths)
	'if(length(uniqyr) > 1){
	    for(jj in 1:length(uniqyr)){
		    dat_EC_HIC_GreaterThan97mnthsY <- dat_EC_HIC_GreaterThan97mnths[which(dat_EC_HIC_GreaterThan97mnths$NearestOlympicYear == uniqyr[jj]),]
			country <- c(country, uniqCount[ii]); 
			year <- c(year, uniqyr[jj]); 
			Participants <- c(Participants, unique(dat_EC_HIC_GreaterThan97mnthsY$TotalParticipant ))
		    countryTotal_HIC_GreaterThan97mnths = countryTotal_HIC_GreaterThan97mnths + unique(dat_EC_HIC_GreaterThan97mnthsY$TotalParticipant )
        }
	}else{
	    dat_EC_HIC_GreaterThan97mnthsY <- dat_EC_HIC_GreaterThan97mnths[which(dat_EC_HIC_GreaterThan97mnths$NearestOlympicYear == uniqyr),]
		country <- c(country, uniqCount[ii]); 
		year <- c(year, uniqyr); 
		Participants <- c(Participants, unique(dat_EC_HIC_GreaterThan97mnthsY$TotalParticipant ))
		countryTotal_HIC_GreaterThan97mnths = countryTotal_HIC_GreaterThan97mnths + unique(dat_EC_HIC_GreaterThan97mnthsY$TotalParticipant )
	}'
}
percEC_HIC_GreaterThan97mnths = (nrow(EconomicClass_HIC_GreaterThan97mnths)/countryTotal_HIC_GreaterThan97mnths)*100

'EconomicClass_LMC <- datFinal[which(datFinal$EconomicClass  == "LMC"),]
nrow(EconomicClass_LMC)
'
'EconomicClass_LMC_GreaterThan97mnths <- datFinal[which((datFinal$EconomicClass  == "LMC")
	& datFinal$Sanction_months <= 24),]
nrow(EconomicClass_LMC_GreaterThan97mnths)
countryTotal_LMC_GreaterThan97mnths = 0
uniqCount <- unique(EconomicClass_LMC_GreaterThan97mnths$Nationality); 
country <- c(); year <- c(); Participants <- c()
for(ii in 1:length(uniqCount)){
    dat_EC_LMC_GreaterThan97mnths <- EconomicClass_LMC_GreaterThan97mnths[which(EconomicClass_LMC_GreaterThan97mnths$Nationality == uniqCount[ii]),]
	uniqyr <- unique(dat_EC_LMC_GreaterThan97mnths$NearestOlympicYear)
	if(length(uniqyr) > 1){
	    for(jj in 1:length(uniqyr)){
		    dat_EC_LMC_GreaterThan97mnthsY <- dat_EC_LMC_GreaterThan97mnths[which(dat_EC_LMC_GreaterThan97mnths$NearestOlympicYear == uniqyr[jj]),]
			country <- c(country, uniqCount[ii]); 
			year <- c(year, uniqyr[jj]); 
			Participants <- c(Participants, unique(dat_HI_LMC_GreaterThan97mnthsY$TotalParticipant ))
		    countryTotal_LMC_GreaterThan97mnths = countryTotal_LMC_GreaterThan97mnths + unique(dat_HI_LMC_GreaterThan97mnthsY$TotalParticipant )
        }
	}else{
	    dat_HI_LMC_GreaterThan97mnthsY <- dat_HI_LMC_GreaterThan97mnths[which(dat_HI_LMC_GreaterThan97mnths$NearestOlympicYear == uniqyr),]
		country <- c(country, uniqCount[ii]); 
		year <- c(year, uniqyr); 
		Participants <- c(Participants, unique(dat_HI_LMC_GreaterThan97mnthsY$TotalParticipant ))
		countryTotal_LMC_GreaterThan97mnths = countryTotal_LMC_GreaterThan97mnths + unique(dat_HI_LMC_GreaterThan97mnthsY$TotalParticipant )
	}
}
percEC_LMC_GreaterThan97mnths = (nrow(EconomicClass_LMC_GreaterThan97mnths)/countryTotal_LMC_GreaterThan97mnths)*100

EconomicClass_LMC_GreaterThan97 <- datFinal[which((datFinal$EconomicClass  == "LMC")
	& (datFinal$Sanction_months >= 25 
	& datFinal$Sanction_months <= 48)),]
nrow(EconomicClass_LMC_GreaterThan97)
countryTotal_LMC_GreaterThan97 = 0
uniqCount <- unique(EconomicClass_LMC_GreaterThan97$Nationality); 
country <- c(); year <- c(); Participants <- c()
for(ii in 1:length(uniqCount)){
    dat_HI_LMC_GreaterThan97 <- EconomicClass_LMC_GreaterThan97[which(EconomicClass_LMC_GreaterThan97$Nationality == uniqCount[ii]),]
	uniqyr <- unique(dat_HI_LMC_GreaterThan97$NearestOlympicYear)
	if(length(uniqyr) > 1){
	    for(jj in 1:length(uniqyr)){
		    dat_HI_LMC_GreaterThan97Y <- dat_HI_LMC_GreaterThan97[which(dat_HI_LMC_GreaterThan97$NearestOlympicYear == uniqyr[jj]),]
			country <- c(country, uniqCount[ii]); 
			year <- c(year, uniqyr[jj]); 
			Participants <- c(Participants, unique(dat_HI_LMC_GreaterThan97Y$TotalParticipant ))
		    countryTotal_LMC_GreaterThan97 = countryTotal_LMC_GreaterThan97 + unique(dat_HI_LMC_GreaterThan97Y$TotalParticipant )
        }
	}else{
	    dat_HI_LMC_GreaterThan97Y <- dat_HI_LMC_GreaterThan97[which(dat_HI_LMC_GreaterThan97$NearestOlympicYear == uniqyr),]
		country <- c(country, uniqCount[ii]); 
		year <- c(year, uniqyr); 
		Participants <- c(Participants, unique(dat_HI_LMC_GreaterThan97Y$TotalParticipant ))
		countryTotal_LMC_GreaterThan97 = countryTotal_LMC_GreaterThan97 + unique(dat_HI_LMC_GreaterThan97Y$TotalParticipant )
	}
}
percEC_LMC_GreaterThan97 = (nrow(EconomicClass_LMC_GreaterThan97)/countryTotal_LMC_GreaterThan97)*100


EconomicClass_LMC_GreaterThan97 <- datFinal[which((datFinal$EconomicClass  == "LMC")
	& (datFinal$Sanction_months >= 49 
	& datFinal$Sanction_months <= 96)),]
nrow(EconomicClass_LMC_GreaterThan97)
countryTotal_LMC_GreaterThan97 = 0
uniqCount <- unique(EconomicClass_LMC_GreaterThan97$Nationality); 
country <- c(); year <- c(); Participants <- c()
for(ii in 1:length(uniqCount)){
    dat_HI_LMC_GreaterThan97 <- EconomicClass_LMC_GreaterThan97[which(EconomicClass_LMC_GreaterThan97$Nationality == uniqCount[ii]),]
	uniqyr <- unique(dat_HI_LMC_GreaterThan97$NearestOlympicYear)
	if(length(uniqyr) > 1){
	    for(jj in 1:length(uniqyr)){
		    dat_HI_LMC_GreaterThan97Y <- dat_HI_LMC_GreaterThan97[which(dat_HI_LMC_GreaterThan97$NearestOlympicYear == uniqyr[jj]),]
			country <- c(country, uniqCount[ii]); 
			year <- c(year, uniqyr[jj]); 
			Participants <- c(Participants, unique(dat_HI_LMC_GreaterThan97Y$TotalParticipant ))
		    countryTotal_LMC_GreaterThan97 = countryTotal_LMC_GreaterThan97 + unique(dat_HI_LMC_GreaterThan97Y$TotalParticipant )
        }
	}else{
	    dat_HI_LMC_GreaterThan97Y <- dat_HI_LMC_GreaterThan97[which(dat_HI_LMC_GreaterThan97$NearestOlympicYear == uniqyr),]
		country <- c(country, uniqCount[ii]); 
		year <- c(year, uniqyr); 
		Participants <- c(Participants, unique(dat_HI_LMC_GreaterThan97Y$TotalParticipant ))
		countryTotal_LMC_GreaterThan97 = countryTotal_LMC_GreaterThan97 + unique(dat_HI_LMC_GreaterThan97Y$TotalParticipant )
	}
}
percEC_LMC_GreaterThan97 = (nrow(EconomicClass_LMC_GreaterThan97)/countryTotal_LMC_GreaterThan97)*100

EconomicClass_LMC_GreaterThan97mnths <- datFinal[which((datFinal$EconomicClass  == "LMC")
	& (datFinal$Sanction_months >= 97)),]
nrow(EconomicClass_LMC_97)
countryTotal_LMC_97 = 0
uniqCount <- unique(EconomicClass_LMC_97$Nationality); 
country <- c(); year <- c(); Participants <- c()
for(ii in 1:length(uniqCount)){
    dat_HI_LMC_97 <- EconomicClass_LMC_97[which(EconomicClass_LMC_97$Nationality == uniqCount[ii]),]
	uniqyr <- unique(dat_HI_LMC_97$NearestOlympicYear)
	if(length(uniqyr) > 1){
	    for(jj in 1:length(uniqyr)){
		    dat_HI_LMC_97Y <- dat_HI_LMC_97[which(dat_HI_LMC_97$NearestOlympicYear == uniqyr[jj]),]
			country <- c(country, uniqCount[ii]); 
			year <- c(year, uniqyr[jj]); 
			Participants <- c(Participants, unique(dat_HI_LMC_97Y$TotalParticipant ))
		    countryTotal_LMC_97 = countryTotal_LMC_97 + unique(dat_HI_LMC_97Y$TotalParticipant )
        }
	}else{
	    dat_HI_LMC_97Y <- dat_HI_LMC_97[which(dat_HI_LMC_97$NearestOlympicYear == uniqyr),]
		country <- c(country, uniqCount[ii]); 
		year <- c(year, uniqyr); 
		Participants <- c(Participants, unique(dat_HI_LMC_97Y$TotalParticipant ))
		countryTotal_LMC_97 = countryTotal_LMC_97 + unique(dat_HI_LMC_97Y$TotalParticipant )
	}
}
percEC_LMC_97 = (nrow(EconomicClass_LMC_97)/countryTotal_LMC_97)*100'
datEC_SP<- data.frame(EconomicClass  = c(
            "EC_LIC",
			"EC_LIC",
			"EC_LIC",
			"EC_LIC",
			"EC_MC",
			"EC_MC",
			"EC_MC",
			"EC_MC",
			"EC_HIC",
			"EC_HIC",
			"EC_HIC",
			"EC_HIC"),
        NumberofAthletes = c(
	        nrow(EconomicClass_LIC_LessThan24mnths),
            nrow(EconomicClass_LIC_25_48mnths),			
	        nrow(EconomicClass_LIC_49_96mnths),
			nrow(EconomicClass_LIC_GreaterThan97mnths),
			nrow(EconomicClass_MC_LessThan24mnths),
            nrow(EconomicClass_MC_25_48mnths),			
	        nrow(EconomicClass_MC_49_96mnths),
			nrow(EconomicClass_MC_GreaterThan97mnths),
			nrow(EconomicClass_HIC_LessThan24mnths),
            nrow(EconomicClass_HIC_25_48mnths),			
	        nrow(EconomicClass_HIC_49_96mnths),
			nrow(EconomicClass_HIC_GreaterThan97mnths)),
	    TotalAthletes = c(countryTotal_LIC_LessThan24mnths,
		    countryTotal_LIC_25_48mnths,
			countryTotal_LIC_49_96mnths,
			countryTotal_LIC_GreaterThan97mnths,
			countryTotal_MC_LessThan24mnths,
		    countryTotal_MC_25_48mnths,
			countryTotal_MC_49_96mnths,
			countryTotal_MC_GreaterThan97mnths,
			countryTotal_HIC_LessThan24mnths,
		    countryTotal_HIC_25_48mnths,
			countryTotal_HIC_49_96mnths,
			countryTotal_HIC_GreaterThan97mnths),
        SanctionPercent = c(percEC_LIC_LessThan24mnths,
		    percEC_LIC_25_48mnths,
			percEC_LIC_49_96mnths,
			percEC_LIC_GreaterThan97mnths,
			percEC_MC_LessThan24mnths,
		    percEC_MC_25_48mnths,
			percEC_MC_49_96mnths,
			percEC_MC_GreaterThan97mnths,
			percEC_HIC_LessThan24mnths,
		    percEC_HIC_25_48mnths,
			percEC_HIC_49_96mnths,
			percEC_HIC_GreaterThan97mnths),
		SanctionPeriod = c("<=24mnths",
		    "25_48mnths",
			"49_96mnths",
			">=97mnths",
			"<=24mnths",
		    "25_48mnths",
			"49_96mnths",
			">=97mnths",
			"<=24mnths",
		    "25_48mnths",
			"49_96mnths",
			">=97mnths"))
write.csv(datEC_SP, file.path(outdir,"EconomicClass_PerSanctionPeriod.csv"), row.names = FALSE)

library(ggplot2)
p11 <- ggplot(data = datEC_SP, aes(x = factor(SanctionPeriod, level = unique(SanctionPeriod)) , y= SanctionPercent, fill = factor(EconomicClass , levels=unique(EconomicClass )))) + geom_bar(stat = "identity", position="dodge")+ labs(x = "SanctionPeriod", y = "% of Sanctioned Athletes", fill = "Economic Class") + theme(legend.title = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.title.x = element_text(face = "bold", size = 50),axis.title.y = element_text(face = "bold", size = 50), axis.text.x = element_text(face="bold", size = 30, angle = 45, hjust = 1), axis.text.y = element_text(face="bold", size = 30), legend.text=element_text(face = "bold",size=25)) + scale_fill_manual(values=c("purple","orange", "brown")) 
p11

#EconomicClass-Sex
outdir = "Z:/Suro/AtheleteSanction/AtheleteSanction05262021/"
datFinal <- read.csv("Z:/Suro/AtheleteSanction/AtheleteSanction05262021/ModifiedAtlete_05292021.csv")
EconomicClass_LIC <- datFinal[which(datFinal$EconomicClass  == "LIC"),]
nrow(EconomicClass_LIC)
countryTotal_LIC = 0
countryMaleLIC = 0
countryFemaleLIC = 0
uniqCount <- unique(EconomicClass_LIC$Nationality); 
country <- c(); year <- c(); Participants <- c(); MaleParticipants_LIC <- c(); FemaleParticipants_LIC <- c()
for(ii in 1:length(uniqCount)){
    dat_EC_LIC <- EconomicClass_LIC[which(EconomicClass_LIC$Nationality == uniqCount[ii]),]
	uniqyr <- unique(dat_EC_LIC$NearestOlympicYear)
	if(length(uniqyr) > 1){
	    for(jj in 1:length(uniqyr)){
		    
		    dat_EC_LICY <- dat_EC_LIC[which(dat_EC_LIC$NearestOlympicYear == uniqyr[jj]),]
			MaleParticipants_LIC <- c(MaleParticipants_LIC, unique(dat_EC_LICY$Male))
		    FemaleParticipants_LIC <- c(FemaleParticipants_LIC, unique(dat_EC_LICY$Female))
			countryMaleLIC = countryMaleLIC + unique(dat_EC_LICY$Male)
			countryFemaleLIC = countryFemaleLIC + unique(dat_EC_LICY$Female)
			country <- c(country, uniqCount[ii]); 
			year <- c(year, uniqyr[jj]); 
			Participants <- c(Participants, unique(dat_EC_LICY$TotalParticipant ))
		    countryTotal_LIC = countryTotal_LIC + unique(dat_EC_LICY$TotalParticipant )
        }
	}else{
	    
	    dat_EC_LICY <- dat_EC_LIC[which(dat_EC_LIC$NearestOlympicYear == uniqyr),]
		MaleParticipants_LIC <- c(MaleParticipants_LIC, unique(dat_EC_LICY$Male))
		FemaleParticipants_LIC <- c(FemaleParticipants_LIC, unique(dat_EC_LICY$Female))
		countryMaleLIC = countryMaleLIC + unique(dat_EC_LICY$Male)
		countryFemaleLIC = countryFemaleLIC + unique(dat_EC_LICY$Female)
		country <- c(country, uniqCount[ii]); 
		year <- c(year, uniqyr); 
		Participants <- c(Participants, unique(dat_EC_LICY$TotalParticipant ))
		countryTotal_LIC = countryTotal_LIC + unique(dat_EC_LICY$TotalParticipant )
	}
}
percM_LIC <- (countryMaleLIC/countryTotal_LIC)*100
percF_LIC <- (countryFemaleLIC/countryTotal_LIC)*100
EconomicClass_MC <- datFinal[which(datFinal$EconomicClass == "UMC" | datFinal$EconomicClass == "LMC"),]
nrow(EconomicClass_MC)
countryTotal_MC = 0
countryMaleMC = 0
countryFemaleMC = 0
uniqCount <- unique(EconomicClass_MC$Nationality); 
country <- c(); year <- c(); Participants <- c(); MaleParticipants_MC <- c(); FemaleParticipants_MC <- c()
for(ii in 1:length(uniqCount)){
    print(ii)
    dat_EC_MC <- EconomicClass_MC[which(EconomicClass_MC$Nationality == uniqCount[ii]),]
	uniqyr <- unique(dat_EC_MC$NearestOlympicYear)
	unique(dat_EC_MC$Male)
	if(length(uniqyr) > 1){
	    for(jj in 1:length(uniqyr)){
		   
		    dat_EC_MCY <- dat_EC_MC[which(dat_EC_MC$NearestOlympicYear == uniqyr[jj]),]
			MaleParticipants_MC <- c(MaleParticipants_MC, unique(dat_EC_MCY$Male))
		    FemaleParticipants_MC <- c(FemaleParticipants_MC, unique(dat_EC_MCY$Female))
			countryMaleMC = countryMaleMC + unique(dat_EC_MCY$Male)
			countryFemaleMC = countryFemaleMC + unique(dat_EC_MCY$Female)
			country <- c(country, uniqCount[ii]); 
			year <- c(year, uniqyr[jj]); 
			Participants <- c(Participants, unique(dat_EC_MCY$TotalParticipant ))
		    countryTotal_MC = countryTotal_MC + unique(dat_EC_MCY$TotalParticipant )
        }
	}else{
	    
	    dat_EC_MCY <- dat_EC_MC[which(dat_EC_MC$NearestOlympicYear == uniqyr),]
		MaleParticipants_MC <- c(MaleParticipants_MC, unique(dat_EC_MCY$Male))
		FemaleParticipants_MC <- c(FemaleParticipants_MC, unique(dat_EC_MCY$Female))
		countryMaleMC = countryMaleMC + unique(dat_EC_MCY$Male)
		countryFemaleMC = countryFemaleMC + unique(dat_EC_MCY$Female)
		country <- c(country, uniqCount[ii]); 
		year <- c(year, uniqyr); 
		Participants <- c(Participants, unique(dat_EC_MCY$TotalParticipant ))
		countryTotal_MC = countryTotal_MC + unique(dat_EC_MCY$TotalParticipant )
	}
}
percM_MC <- (countryMaleMC/countryTotal_MC)*100
percF_MC <- (countryFemaleMC/countryTotal_MC)*100
EconomicClass_HIC <- datFinal[which(datFinal$EconomicClass  == "HIC"),]
nrow(EconomicClass_HIC)
countryTotal_HIC = 0
countryMaleHIC = 0
countryFemaleHIC = 0
uniqCount <- unique(EconomicClass_HIC$Nationality); 
country <- c(); year <- c(); Participants <- c(); MaleParticipants_HIC <- c(); FemaleParticipants_HIC <- c()
for(ii in 1:length(uniqCount)){
    dat_EC_HIC <- EconomicClass_HIC[which(EconomicClass_HIC$Nationality == uniqCount[ii]),]
	uniqyr <- unique(dat_EC_HIC$NearestOlympicYear)
	if(length(uniqyr) > 1){
	    for(jj in 1:length(uniqyr)){
		    
			
		    dat_EC_HICY <- dat_EC_HIC[which(dat_EC_HIC$NearestOlympicYear == uniqyr[jj]),]
			MaleParticipants_HIC <- c(MaleParticipants_HIC, unique(dat_EC_HICY$Male))
		    FemaleParticipants_HIC <- c(FemaleParticipants_HIC, unique(dat_EC_HICY$Female))
			countryMaleHIC = countryMaleHIC + unique(dat_EC_HICY$Male)
			countryFemaleHIC = countryFemaleHIC + unique(dat_EC_HICY$Female)
			country <- c(country, uniqCount[ii]); 
			year <- c(year, uniqyr[jj]); 
			Participants <- c(Participants, unique(dat_EC_HICY$TotalParticipant ))
		    countryTotal_HIC = countryTotal_HIC + unique(dat_EC_HICY$TotalParticipant )
        }
	}else{
	    
	    dat_EC_HICY <- dat_EC_HIC[which(dat_EC_HIC$NearestOlympicYear == uniqyr),]
		MaleParticipants_HIC <- c(MaleParticipants_HIC, unique(dat_EC_HICY$Male))
		FemaleParticipants_HIC <- c(FemaleParticipants_HIC, unique(dat_EC_HICY$Female))
		countryMaleHIC = countryMaleHIC + unique(dat_EC_HICY$Male)
		countryFemaleHIC = countryFemaleHIC + unique(dat_EC_HICY$Female)
		country <- c(country, uniqCount[ii]); 
		year <- c(year, uniqyr); 
		Participants <- c(Participants, unique(dat_EC_HICY$TotalParticipant ))
		countryTotal_HIC = countryTotal_HIC + unique(dat_EC_HICY$TotalParticipant )
	}
}
percM_HIC <- (countryMaleHIC/countryTotal_HIC)*100
percF_HIC <- (countryFemaleHIC/countryTotal_HIC)*100
'EconomicClass_LMC <- datFinal[which(datFinal$EconomicClass  == "LMC"),]
nrow(EconomicClass_LMC)
countryTotal_LMC = 0
uniqCount <- unique(EconomicClass_LMC$Nationality); 
country <- c(); year <- c(); Participants <- c()
for(ii in 1:length(uniqCount)){
    dat_EC_LMC <- EconomicClass_LMC[which(EconomicClass_LMC$Nationality == uniqCount[ii]),]
	uniqyr <- unique(dat_EC_LMC$NearestOlympicYear)
	if(length(uniqyr) > 1){
	    for(jj in 1:length(uniqyr)){
		    dat_EC_LMCY <- dat_EC_LMC[which(dat_EC_LMC$NearestOlympicYear == uniqyr[jj]),]
			country <- c(country, uniqCount[ii]); 
			year <- c(year, uniqyr[jj]); 
			Participants <- c(Participants, unique(dat_EC_LMCY$TotalParticipant ))
		    countryTotal_LMC = countryTotal_LMC + unique(dat_EC_LMCY$TotalParticipant )
        }
	}else{
	    dat_EC_LMCY <- dat_EC_LMC[which(dat_EC_LMC$NearestOlympicYear == uniqyr),]
		country <- c(country, uniqCount[ii]); 
		year <- c(year, uniqyr); 
		Participants <- c(Participants, unique(dat_EC_LMCY$TotalParticipant ))
		countryTotal_LMC = countryTotal_LMC + unique(dat_EC_LMCY$TotalParticipant )
	}
}'
#percEC_LMC = (nrow(EconomicClass_LMC)/countryTotal_LMC)*100
datEC_Sex_SP<- data.frame(EconomicClass  = c(
            "EC_LIC",
			"EC_LIC",
			"EC_MC", 
			"EC_MC", 
			"EC_HIC",
			"EC_HIC"),
        TotalAthletes = c(countryTotal_LIC,
		    countryTotal_LIC,
		    countryTotal_UMC,
			countryTotal_UMC,
			countryTotal_HIC,
			countryTotal_HIC),
		Sex = c("Male", "Female", "Male", "Female","Male", "Female"),
		TotalAthletes_M_F= c(countryMaleLIC, countryFemaleLIC,countryMaleMC, countryFemaleMC,countryMaleHIC, countryFemaleHIC),
		SanctionPercent = c(percM_LIC,percF_LIC,percM_MC,percF_MC,percM_HIC,percF_HIC))
write.csv(datEC_Sex_SP, "Z:/Suro/AtheleteSanction/AtheleteSanction05262021/EC_SP.csv")
p13 <- ggplot(data = datEC_Sex_SP, aes(x = factor(EconomicClass , levels=unique(EconomicClass)), y= SanctionPercent, fill = factor(Sex , levels=c("Male", "Female")))) + geom_bar(stat = "identity")+ labs(x = "EconomicClass", y = "% of Sanctioned Athletes") + theme(legend.title = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.title.x = element_text(face = "bold", size = 50),axis.title.y = element_text(face = "bold", size = 50), axis.text.x = element_text(face="bold", size = 30, angle = 45, hjust = 1), axis.text.y = element_text(face="bold", size = 30), legend.text=element_text(face = "bold",size=25))  + scale_fill_manual(values=c("green", "hotpink"))
p13

#EconomicClass 
outdir = "Z:/Suro/AtheleteSanction/AtheleteSanction05262021/"
datFinal <- read.csv("Z:/Suro/AtheleteSanction/AtheleteSanction05262021/ModifiedAtlete_05292021.csv")
EconomicClass_LIC_LessThan24mnths <- datFinal[which((datFinal$EconomicClass  == "LIC")
	& datFinal$Sanction_months <= 24),]
#nrow(EconomicClass_LIC_LessThan24mnths)
nrow(EconomicClass_LIC_LessThan24mnths)
countryTotal_LIC_LessThan24mnths = 0
countryMaleLIC_LessThan24mnths = 0
countryFemaleLIC_LessThan24mnths = 0
uniqCount <- unique(EconomicClass_LIC_LessThan24mnths$Nationality); 
country <- c(); year <- c(); Participants <- c(); MaleParticipants_LIC_LessThan24mnths <- c(); FemaleParticipants_LIC_LessThan24mnths <- c()
for(ii in 1:length(uniqCount)){
    dat_EC_LIC_LessThan24mnths <- EconomicClass_LIC_LessThan24mnths[which(EconomicClass_LIC_LessThan24mnths$Nationality == uniqCount[ii]),]
	uniqyr <- unique(dat_EC_LIC_LessThan24mnths$NearestOlympicYear)
	if(length(uniqyr) > 1){
	    for(jj in 1:length(uniqyr)){
		    
		    dat_EC_LIC_LessThan24mnthsY <- dat_EC_LIC_LessThan24mnths[which(dat_EC_LIC_LessThan24mnths$NearestOlympicYear == uniqyr[jj]),]
			MaleParticipants_LIC_LessThan24mnths <- c(MaleParticipants_LIC_LessThan24mnths, unique(dat_EC_LIC_LessThan24mnthsY$Male))
		    FemaleParticipants_LIC_LessThan24mnths <- c(FemaleParticipants_LIC_LessThan24mnths, unique(dat_EC_LIC_LessThan24mnthsY$Female))
			countryMaleLIC_LessThan24mnths = countryMaleLIC_LessThan24mnths + unique(dat_EC_LIC_LessThan24mnthsY$Male)
			countryFemaleLIC_LessThan24mnths = countryFemaleLIC_LessThan24mnths + unique(dat_EC_LIC_LessThan24mnthsY$Female)
			country <- c(country, uniqCount[ii]); 
			year <- c(year, uniqyr[jj]); 
			Participants <- c(Participants, unique(dat_EC_LIC_LessThan24mnthsY$TotalParticipant ))
		    countryTotal_LIC_LessThan24mnths = countryTotal_LIC_LessThan24mnths + unique(dat_EC_LIC_LessThan24mnthsY$TotalParticipant )
        }
	}else{
	    
	    dat_EC_LIC_LessThan24mnthsY <- dat_EC_LIC_LessThan24mnths[which(dat_EC_LIC_LessThan24mnths$NearestOlympicYear == uniqyr),]
		MaleParticipants_LIC_LessThan24mnths <- c(MaleParticipants_LIC_LessThan24mnths, unique(dat_EC_LIC_LessThan24mnthsY$Male))
		FemaleParticipants_LIC_LessThan24mnths <- c(FemaleParticipants_LIC_LessThan24mnths, unique(dat_EC_LIC_LessThan24mnthsY$Female))
		countryMaleLIC_LessThan24mnths = countryMaleLIC_LessThan24mnths + unique(dat_EC_LIC_LessThan24mnthsY$Male)
		countryFemaleLIC_LessThan24mnths = countryFemaleLIC_LessThan24mnths + unique(dat_EC_LIC_LessThan24mnthsY$Female)
		country <- c(country, uniqCount[ii]); 
		year <- c(year, uniqyr); 
		Participants <- c(Participants, unique(dat_EC_LIC_LessThan24mnthsY$TotalParticipant ))
		countryTotal_LIC_LessThan24mnths = countryTotal_LIC_LessThan24mnths + unique(dat_EC_LIC_LessThan24mnthsY$TotalParticipant )
	}
}
percM_LIC_LessThan24mnths <- (countryMaleLIC_LessThan24mnths/countryTotal_LIC_LessThan24mnths)*100
percF_LIC_LessThan24mnths <- (countryFemaleLIC_LessThan24mnths/countryTotal_LIC_LessThan24mnths)*100

EconomicClass_LIC_25_48mnths <- datFinal[which((datFinal$EconomicClass == "LIC")
	& (datFinal$Sanction_months >= 25 
	& datFinal$Sanction_months <= 48)),]

nrow(EconomicClass_LIC_25_48mnths)
countryTotal_LIC_25_48mnths = 0
countryMaleLIC_25_48mnths = 0
countryFemaleLIC_25_48mnths = 0
uniqCount <- unique(EconomicClass_LIC_25_48mnths$Nationality); 
country <- c(); year <- c(); Participants <- c(); MaleParticipants_LIC_25_48mnths <- c(); FemaleParticipants_LIC_25_48mnths <- c()
for(ii in 1:length(uniqCount)){
    dat_EC_LIC_25_48mnths <- EconomicClass_LIC_25_48mnths[which(EconomicClass_LIC_25_48mnths$Nationality == uniqCount[ii]),]
	uniqyr <- unique(dat_EC_LIC_25_48mnths$NearestOlympicYear)
	if(length(uniqyr) > 1){
	    for(jj in 1:length(uniqyr)){
		    
		    dat_EC_LIC_25_48mnthsY <- dat_EC_LIC_25_48mnths[which(dat_EC_LIC_25_48mnths$NearestOlympicYear == uniqyr[jj]),]
			MaleParticipants_LIC_25_48mnths <- c(MaleParticipants_LIC_25_48mnths, unique(dat_EC_LIC_25_48mnthsY$Male))
		    FemaleParticipants_LIC_25_48mnths <- c(FemaleParticipants_LIC_25_48mnths, unique(dat_EC_LIC_25_48mnthsY$Female))
			countryMaleLIC_25_48mnths = countryMaleLIC_25_48mnths + unique(dat_EC_LIC_25_48mnthsY$Male)
			countryFemaleLIC_25_48mnths = countryFemaleLIC_25_48mnths + unique(dat_EC_LIC_25_48mnthsY$Female)
			country <- c(country, uniqCount[ii]); 
			year <- c(year, uniqyr[jj]); 
			Participants <- c(Participants, unique(dat_EC_LIC_25_48mnthsY$TotalParticipant ))
		    countryTotal_LIC_25_48mnths = countryTotal_LIC_25_48mnths + unique(dat_EC_LIC_25_48mnthsY$TotalParticipant )
        }
	}else{
	    
	    dat_EC_LIC_25_48mnthsY <- dat_EC_LIC_25_48mnths[which(dat_EC_LIC_25_48mnths$NearestOlympicYear == uniqyr),]
		MaleParticipants_LIC_25_48mnths <- c(MaleParticipants_LIC_25_48mnths, unique(dat_EC_LIC_25_48mnthsY$Male))
		FemaleParticipants_LIC_25_48mnths <- c(FemaleParticipants_LIC_25_48mnths, unique(dat_EC_LIC_25_48mnthsY$Female))
		countryMaleLIC_25_48mnths = countryMaleLIC_25_48mnths + unique(dat_EC_LIC_25_48mnthsY$Male)
		countryFemaleLIC_25_48mnths = countryFemaleLIC_25_48mnths + unique(dat_EC_LIC_25_48mnthsY$Female)
		country <- c(country, uniqCount[ii]); 
		year <- c(year, uniqyr); 
		Participants <- c(Participants, unique(dat_EC_LIC_25_48mnthsY$TotalParticipant ))
		countryTotal_LIC_25_48mnths = countryTotal_LIC_25_48mnths + unique(dat_EC_LIC_25_48mnthsY$TotalParticipant )
	}
}
percM_LIC_25_48mnths <- (countryMaleLIC_25_48mnths/countryTotal_LIC_25_48mnths)*100
percF_LIC_25_48mnths <- (countryFemaleLIC_25_48mnths/countryTotal_LIC_25_48mnths)*100


EconomicClass_LIC_49_96mnths <- datFinal[which((datFinal$EconomicClass == "LIC")
	& (datFinal$Sanction_months >= 49 
	& datFinal$Sanction_months <= 96)),]
nrow(EconomicClass_LIC_49_96mnths)
countryTotal_LIC_49_96mnths = 0
countryMaleLIC_49_96mnths = 0
countryFemaleLIC_49_96mnths = 0
uniqCount <- unique(EconomicClass_LIC_49_96mnths$Nationality); 
country <- c(); year <- c(); Participants <- c(); MaleParticipants_LIC_49_96mnths <- c(); FemaleParticipants_LIC_49_96mnths <- c()
for(ii in 1:length(uniqCount)){
    dat_EC_LIC_49_96mnths <- EconomicClass_LIC_49_96mnths[which(EconomicClass_LIC_49_96mnths$Nationality == uniqCount[ii]),]
	uniqyr <- unique(dat_EC_LIC_49_96mnths$NearestOlympicYear)
	if(length(uniqyr) > 1){
	    for(jj in 1:length(uniqyr)){
		    
		    dat_EC_LIC_49_96mnthsY <- dat_EC_LIC_49_96mnths[which(dat_EC_LIC_49_96mnths$NearestOlympicYear == uniqyr[jj]),]
			MaleParticipants_LIC_49_96mnths <- c(MaleParticipants_LIC_49_96mnths, unique(dat_EC_LIC_49_96mnthsY$Male))
		    FemaleParticipants_LIC_49_96mnths <- c(FemaleParticipants_LIC_49_96mnths, unique(dat_EC_LIC_49_96mnthsY$Female))
			countryMaleLIC_49_96mnths = countryMaleLIC_49_96mnths + unique(dat_EC_LIC_49_96mnthsY$Male)
			countryFemaleLIC_49_96mnths = countryFemaleLIC_49_96mnths + unique(dat_EC_LIC_49_96mnthsY$Female)
			country <- c(country, uniqCount[ii]); 
			year <- c(year, uniqyr[jj]); 
			Participants <- c(Participants, unique(dat_EC_LIC_49_96mnthsY$TotalParticipant ))
		    countryTotal_LIC_49_96mnths = countryTotal_LIC_49_96mnths + unique(dat_EC_LIC_49_96mnthsY$TotalParticipant )
        }
	}else{
	    
	    dat_EC_LIC_49_96mnthsY <- dat_EC_LIC_49_96mnths[which(dat_EC_LIC_49_96mnths$NearestOlympicYear == uniqyr),]
		MaleParticipants_LIC_49_96mnths <- c(MaleParticipants_LIC_49_96mnths, unique(dat_EC_LIC_49_96mnthsY$Male))
		FemaleParticipants_LIC_49_96mnths <- c(FemaleParticipants_LIC_49_96mnths, unique(dat_EC_LIC_49_96mnthsY$Female))
		countryMaleLIC_49_96mnths = countryMaleLIC_49_96mnths + unique(dat_EC_LIC_49_96mnthsY$Male)
		countryFemaleLIC_49_96mnths = countryFemaleLIC_49_96mnths + unique(dat_EC_LIC_49_96mnthsY$Female)
		country <- c(country, uniqCount[ii]); 
		year <- c(year, uniqyr); 
		Participants <- c(Participants, unique(dat_EC_LIC_49_96mnthsY$TotalParticipant ))
		countryTotal_LIC_49_96mnths = countryTotal_LIC_49_96mnths + unique(dat_EC_LIC_49_96mnthsY$TotalParticipant )
	}
}
percM_LIC_49_96mnths <- (countryMaleLIC_49_96mnths/countryTotal_LIC_49_96mnths)*100
percF_LIC_49_96mnths <- (countryFemaleLIC_49_96mnths/countryTotal_LIC_49_96mnths)*100

EconomicClass_LIC_GreaterThan97mnths <- datFinal[which((datFinal$EconomicClass == "LIC")
	& (datFinal$Sanction_months >= 97)),]

nrow(EconomicClass_LIC_GreaterThan97mnths)
countryTotal_LIC_GreaterThan97mnths = 0
countryMaleLIC_GreaterThan97mnths = 0
countryFemaleLIC_GreaterThan97mnths = 0
uniqCount <- unique(EconomicClass_LIC_GreaterThan97mnths$Nationality); 
country <- c(); year <- c(); Participants <- c(); MaleParticipants_LIC_GreaterThan97mnths <- c(); FemaleParticipants_LIC_GreaterThan97mnths <- c()
for(ii in 1:length(uniqCount)){
    dat_EC_LIC_GreaterThan97mnths <- EconomicClass_LIC_GreaterThan97mnths[which(EconomicClass_LIC_GreaterThan97mnths$Nationality == uniqCount[ii]),]
	uniqyr <- unique(dat_EC_LIC_GreaterThan97mnths$NearestOlympicYear)
	if(length(uniqyr) > 1){
	    for(jj in 1:length(uniqyr)){
		    
		    dat_EC_LIC_GreaterThan97mnthsY <- dat_EC_LIC_GreaterThan97mnths[which(dat_EC_LIC_GreaterThan97mnths$NearestOlympicYear == uniqyr[jj]),]
			MaleParticipants_LIC_GreaterThan97mnths <- c(MaleParticipants_LIC_GreaterThan97mnths, unique(dat_EC_LIC_GreaterThan97mnthsY$Male))
		    FemaleParticipants_LIC_GreaterThan97mnths <- c(FemaleParticipants_LIC_GreaterThan97mnths, unique(dat_EC_LIC_GreaterThan97mnthsY$Female))
			countryMaleLIC_GreaterThan97mnths = countryMaleLIC_GreaterThan97mnths + unique(dat_EC_LIC_GreaterThan97mnthsY$Male)
			countryFemaleLIC_GreaterThan97mnths = countryFemaleLIC_GreaterThan97mnths + unique(dat_EC_LIC_GreaterThan97mnthsY$Female)
			country <- c(country, uniqCount[ii]); 
			year <- c(year, uniqyr[jj]); 
			Participants <- c(Participants, unique(dat_EC_LIC_GreaterThan97mnthsY$TotalParticipant ))
		    countryTotal_LIC_GreaterThan97mnths = countryTotal_LIC_GreaterThan97mnths + unique(dat_EC_LIC_GreaterThan97mnthsY$TotalParticipant )
        }
	}else{
	    
	    dat_EC_LIC_GreaterThan97mnthsY <- dat_EC_LIC_GreaterThan97mnths[which(dat_EC_LIC_GreaterThan97mnths$NearestOlympicYear == uniqyr),]
		MaleParticipants_LIC_GreaterThan97mnths <- c(MaleParticipants_LIC_GreaterThan97mnths, unique(dat_EC_LIC_GreaterThan97mnthsY$Male))
		FemaleParticipants_LIC_GreaterThan97mnths <- c(FemaleParticipants_LIC_GreaterThan97mnths, unique(dat_EC_LIC_GreaterThan97mnthsY$Female))
		countryMaleLIC_GreaterThan97mnths = countryMaleLIC_GreaterThan97mnths + unique(dat_EC_LIC_GreaterThan97mnthsY$Male)
		countryFemaleLIC_GreaterThan97mnths = countryFemaleLIC_GreaterThan97mnths + unique(dat_EC_LIC_GreaterThan97mnthsY$Female)
		country <- c(country, uniqCount[ii]); 
		year <- c(year, uniqyr); 
		Participants <- c(Participants, unique(dat_EC_LIC_GreaterThan97mnthsY$TotalParticipant ))
		countryTotal_LIC_GreaterThan97mnths = countryTotal_LIC_GreaterThan97mnths + unique(dat_EC_LIC_GreaterThan97mnthsY$TotalParticipant )
	}
}
percM_LIC_GreaterThan97mnths <- (countryMaleLIC_GreaterThan97mnths/countryTotal_LIC_GreaterThan97mnths)*100
percF_LIC_GreaterThan97mnths <- (countryFemaleLIC_GreaterThan97mnths/countryTotal_LIC_GreaterThan97mnths)*100

#EconomicClass_UMC <- datFinal[which(datFinal$EconomicClass == "UMC" | datFinal$EconomicClass == "LMC"),]
#nrow(EconomicClass_UMC)
#UMC
EconomicClass_MC_LessThan24mnths <- datFinal[which((datFinal$EconomicClass == "UMC" | datFinal$EconomicClass == "LMC")
	& datFinal$Sanction_months <= 24),]
nrow(EconomicClass_MC_LessThan24mnths)
countryTotal_MC_LessThan24mnths = 0
countryMaleMC_LessThan24mnths = 0
countryFemaleMC_LessThan24mnths = 0
uniqCount <- unique(EconomicClass_MC_LessThan24mnths$Nationality); 
country <- c(); year <- c(); Participants <- c(); MaleParticipants_MC_LessThan24mnths <- c(); FemaleParticipants_MC_LessThan24mnths <- c()
for(ii in 1:length(uniqCount)){
    dat_EC_MC_LessThan24mnths <- EconomicClass_MC_LessThan24mnths[which(EconomicClass_MC_LessThan24mnths$Nationality == uniqCount[ii]),]
	uniqyr <- unique(dat_EC_MC_LessThan24mnths$NearestOlympicYear)
	if(length(uniqyr) > 1){
	    for(jj in 1:length(uniqyr)){
		    
		    dat_EC_MC_LessThan24mnthsY <- dat_EC_MC_LessThan24mnths[which(dat_EC_MC_LessThan24mnths$NearestOlympicYear == uniqyr[jj]),]
			MaleParticipants_MC_LessThan24mnths <- c(MaleParticipants_MC_LessThan24mnths, unique(dat_EC_MC_LessThan24mnthsY$Male))
		    FemaleParticipants_MC_LessThan24mnths <- c(FemaleParticipants_MC_LessThan24mnths, unique(dat_EC_MC_LessThan24mnthsY$Female))
			countryMaleMC_LessThan24mnths = countryMaleMC_LessThan24mnths + unique(dat_EC_MC_LessThan24mnthsY$Male)
			countryFemaleMC_LessThan24mnths = countryFemaleMC_LessThan24mnths + unique(dat_EC_MC_LessThan24mnthsY$Female)
			country <- c(country, uniqCount[ii]); 
			year <- c(year, uniqyr[jj]); 
			Participants <- c(Participants, unique(dat_EC_MC_LessThan24mnthsY$TotalParticipant ))
		    countryTotal_MC_LessThan24mnths = countryTotal_MC_LessThan24mnths + unique(dat_EC_MC_LessThan24mnthsY$TotalParticipant )
        }
	}else{
	    
	    dat_EC_MC_LessThan24mnthsY <- dat_EC_MC_LessThan24mnths[which(dat_EC_MC_LessThan24mnths$NearestOlympicYear == uniqyr),]
		MaleParticipants_MC_LessThan24mnths <- c(MaleParticipants_MC_LessThan24mnths, unique(dat_EC_MC_LessThan24mnthsY$Male))
		FemaleParticipants_MC_LessThan24mnths <- c(FemaleParticipants_MC_LessThan24mnths, unique(dat_EC_MC_LessThan24mnthsY$Female))
		countryMaleMC_LessThan24mnths = countryMaleMC_LessThan24mnths + unique(dat_EC_MC_LessThan24mnthsY$Male)
		countryFemaleMC_LessThan24mnths = countryFemaleMC_LessThan24mnths + unique(dat_EC_MC_LessThan24mnthsY$Female)
		country <- c(country, uniqCount[ii]); 
		year <- c(year, uniqyr); 
		Participants <- c(Participants, unique(dat_EC_MC_LessThan24mnthsY$TotalParticipant ))
		countryTotal_MC_LessThan24mnths = countryTotal_MC_LessThan24mnths + unique(dat_EC_MC_LessThan24mnthsY$TotalParticipant )
	}
}
percM_MC_LessThan24mnths <- (countryMaleMC_LessThan24mnths/countryTotal_MC_LessThan24mnths)*100
percF_MC_LessThan24mnths <- (countryFemaleMC_LessThan24mnths/countryTotal_MC_LessThan24mnths)*100


EconomicClass_MC_25_48mnths <- datFinal[which((datFinal$EconomicClass == "UMC" | datFinal$EconomicClass == "LMC")
	& (datFinal$Sanction_months >= 25 
	& datFinal$Sanction_months <= 48)),]
nrow(EconomicClass_MC_25_48mnths)
countryTotal_MC_25_48mnths = 0
countryMaleMC_25_48mnths = 0
countryFemaleMC_25_48mnths = 0
uniqCount <- unique(EconomicClass_MC_25_48mnths$Nationality); 
country <- c(); year <- c(); Participants <- c(); MaleParticipants_MC_25_48mnths <- c(); FemaleParticipants_MC_25_48mnths <- c()
for(ii in 1:length(uniqCount)){
    dat_EC_MC_25_48mnths <- EconomicClass_MC_25_48mnths[which(EconomicClass_MC_25_48mnths$Nationality == uniqCount[ii]),]
	uniqyr <- unique(dat_EC_MC_25_48mnths$NearestOlympicYear)
	if(length(uniqyr) > 1){
	    for(jj in 1:length(uniqyr)){
		    
		    dat_EC_MC_25_48mnthsY <- dat_EC_MC_25_48mnths[which(dat_EC_MC_25_48mnths$NearestOlympicYear == uniqyr[jj]),]
			MaleParticipants_MC_25_48mnths <- c(MaleParticipants_MC_25_48mnths, unique(dat_EC_MC_25_48mnthsY$Male))
		    FemaleParticipants_MC_25_48mnths <- c(FemaleParticipants_MC_25_48mnths, unique(dat_EC_MC_25_48mnthsY$Female))
			countryMaleMC_25_48mnths = countryMaleMC_25_48mnths + unique(dat_EC_MC_25_48mnthsY$Male)
			countryFemaleMC_25_48mnths = countryFemaleMC_25_48mnths + unique(dat_EC_MC_25_48mnthsY$Female)
			country <- c(country, uniqCount[ii]); 
			year <- c(year, uniqyr[jj]); 
			Participants <- c(Participants, unique(dat_EC_MC_25_48mnthsY$TotalParticipant ))
		    countryTotal_MC_25_48mnths = countryTotal_MC_25_48mnths + unique(dat_EC_MC_25_48mnthsY$TotalParticipant )
        }
	}else{
	    
	    dat_EC_MC_25_48mnthsY <- dat_EC_MC_25_48mnths[which(dat_EC_MC_25_48mnths$NearestOlympicYear == uniqyr),]
		MaleParticipants_MC_25_48mnths <- c(MaleParticipants_MC_25_48mnths, unique(dat_EC_MC_25_48mnthsY$Male))
		FemaleParticipants_MC_25_48mnths <- c(FemaleParticipants_MC_25_48mnths, unique(dat_EC_MC_25_48mnthsY$Female))
		countryMaleMC_25_48mnths = countryMaleMC_25_48mnths + unique(dat_EC_MC_25_48mnthsY$Male)
		countryFemaleMC_25_48mnths = countryFemaleMC_25_48mnths + unique(dat_EC_MC_25_48mnthsY$Female)
		country <- c(country, uniqCount[ii]); 
		year <- c(year, uniqyr); 
		Participants <- c(Participants, unique(dat_EC_MC_25_48mnthsY$TotalParticipant ))
		countryTotal_MC_25_48mnths = countryTotal_MC_25_48mnths + unique(dat_EC_MC_25_48mnthsY$TotalParticipant )
	}
}
percM_MC_25_48mnths <- (countryMaleMC_25_48mnths/countryTotal_MC_25_48mnths)*100
percF_MC_25_48mnths <- (countryFemaleMC_25_48mnths/countryTotal_MC_25_48mnths)*100


EconomicClass_MC_49_96mnths <- datFinal[which((datFinal$EconomicClass == "UMC" | datFinal$EconomicClass == "LMC")
	& (datFinal$Sanction_months >= 49 
	& datFinal$Sanction_months <= 96)),]
nrow(EconomicClass_MC_49_96mnths)
countryTotal_MC_49_96mnths = 0
countryMaleMC_49_96mnths = 0
countryFemaleMC_49_96mnths = 0
uniqCount <- unique(EconomicClass_MC_49_96mnths$Nationality); 
country <- c(); year <- c(); Participants <- c(); MaleParticipants_MC_49_96mnths <- c(); FemaleParticipants_MC_49_96mnths <- c()
for(ii in 1:length(uniqCount)){
    dat_EC_MC_49_96mnths <- EconomicClass_MC_49_96mnths[which(EconomicClass_MC_49_96mnths$Nationality == uniqCount[ii]),]
	uniqyr <- unique(dat_EC_MC_49_96mnths$NearestOlympicYear)
	if(length(uniqyr) > 1){
	    for(jj in 1:length(uniqyr)){
		    
		    dat_EC_MC_49_96mnthsY <- dat_EC_MC_49_96mnths[which(dat_EC_MC_49_96mnths$NearestOlympicYear == uniqyr[jj]),]
			MaleParticipants_MC_49_96mnths <- c(MaleParticipants_MC_49_96mnths, unique(dat_EC_MC_49_96mnthsY$Male))
		    FemaleParticipants_MC_49_96mnths <- c(FemaleParticipants_MC_49_96mnths, unique(dat_EC_MC_49_96mnthsY$Female))
			countryMaleMC_49_96mnths = countryMaleMC_49_96mnths + unique(dat_EC_MC_49_96mnthsY$Male)
			countryFemaleMC_49_96mnths = countryFemaleMC_49_96mnths + unique(dat_EC_MC_49_96mnthsY$Female)
			country <- c(country, uniqCount[ii]); 
			year <- c(year, uniqyr[jj]); 
			Participants <- c(Participants, unique(dat_EC_MC_49_96mnthsY$TotalParticipant ))
		    countryTotal_MC_49_96mnths = countryTotal_MC_49_96mnths + unique(dat_EC_MC_49_96mnthsY$TotalParticipant )
        }
	}else{
	    
	    dat_EC_MC_49_96mnthsY <- dat_EC_MC_49_96mnths[which(dat_EC_MC_49_96mnths$NearestOlympicYear == uniqyr),]
		MaleParticipants_MC_49_96mnths <- c(MaleParticipants_MC_49_96mnths, unique(dat_EC_MC_49_96mnthsY$Male))
		FemaleParticipants_MC_49_96mnths <- c(FemaleParticipants_MC_49_96mnths, unique(dat_EC_MC_49_96mnthsY$Female))
		countryMaleMC_49_96mnths = countryMaleMC_49_96mnths + unique(dat_EC_MC_49_96mnthsY$Male)
		countryFemaleMC_49_96mnths = countryFemaleMC_49_96mnths + unique(dat_EC_MC_49_96mnthsY$Female)
		country <- c(country, uniqCount[ii]); 
		year <- c(year, uniqyr); 
		Participants <- c(Participants, unique(dat_EC_MC_49_96mnthsY$TotalParticipant ))
		countryTotal_MC_49_96mnths = countryTotal_MC_49_96mnths + unique(dat_EC_MC_49_96mnthsY$TotalParticipant )
	}
}
percM_MC_49_96mnths <- (countryMaleMC_49_96mnths/countryTotal_MC_49_96mnths)*100
percF_MC_49_96mnths <- (countryFemaleMC_49_96mnths/countryTotal_MC_49_96mnths)*100

EconomicClass_MC_GreaterThan97mnths <- datFinal[which((datFinal$EconomicClass == "UMC" | datFinal$EconomicClass == "LMC")
	& (datFinal$Sanction_months >= 97)),]
nrow(EconomicClass_MC_GreaterThan97mnths)
countryTotal_MC_GreaterThan97mnths = 0
countryMaleMC_GreaterThan97mnths = 0
countryFemaleMC_GreaterThan97mnths = 0
uniqCount <- unique(EconomicClass_MC_GreaterThan97mnths$Nationality); 
country <- c(); year <- c(); Participants <- c(); MaleParticipants_MC_GreaterThan97mnths <- c(); FemaleParticipants_MC_GreaterThan97mnths <- c()
for(ii in 1:length(uniqCount)){
    dat_EC_MC_GreaterThan97mnths <- EconomicClass_MC_GreaterThan97mnths[which(EconomicClass_MC_GreaterThan97mnths$Nationality == uniqCount[ii]),]
	uniqyr <- unique(dat_EC_MC_GreaterThan97mnths$NearestOlympicYear)
	if(length(uniqyr) > 1){
	    for(jj in 1:length(uniqyr)){
		    
		    dat_EC_MC_GreaterThan97mnthsY <- dat_EC_MC_GreaterThan97mnths[which(dat_EC_MC_GreaterThan97mnths$NearestOlympicYear == uniqyr[jj]),]
			MaleParticipants_MC_GreaterThan97mnths <- c(MaleParticipants_MC_GreaterThan97mnths, unique(dat_EC_MC_GreaterThan97mnthsY$Male))
		    FemaleParticipants_MC_GreaterThan97mnths <- c(FemaleParticipants_MC_GreaterThan97mnths, unique(dat_EC_MC_GreaterThan97mnthsY$Female))
			countryMaleMC_GreaterThan97mnths = countryMaleMC_GreaterThan97mnths + unique(dat_EC_MC_GreaterThan97mnthsY$Male)
			countryFemaleMC_GreaterThan97mnths = countryFemaleMC_GreaterThan97mnths + unique(dat_EC_MC_GreaterThan97mnthsY$Female)
			country <- c(country, uniqCount[ii]); 
			year <- c(year, uniqyr[jj]); 
			Participants <- c(Participants, unique(dat_EC_MC_GreaterThan97mnthsY$TotalParticipant ))
		    countryTotal_MC_GreaterThan97mnths = countryTotal_MC_GreaterThan97mnths + unique(dat_EC_MC_GreaterThan97mnthsY$TotalParticipant )
        }
	}else{
	    
	    dat_EC_MC_GreaterThan97mnthsY <- dat_EC_MC_GreaterThan97mnths[which(dat_EC_MC_GreaterThan97mnths$NearestOlympicYear == uniqyr),]
		MaleParticipants_MC_GreaterThan97mnths <- c(MaleParticipants_MC_GreaterThan97mnths, unique(dat_EC_MC_GreaterThan97mnthsY$Male))
		FemaleParticipants_MC_GreaterThan97mnths <- c(FemaleParticipants_MC_GreaterThan97mnths, unique(dat_EC_MC_GreaterThan97mnthsY$Female))
		countryMaleMC_GreaterThan97mnths = countryMaleMC_GreaterThan97mnths + unique(dat_EC_MC_GreaterThan97mnthsY$Male)
		countryFemaleMC_GreaterThan97mnths = countryFemaleMC_GreaterThan97mnths + unique(dat_EC_MC_GreaterThan97mnthsY$Female)
		country <- c(country, uniqCount[ii]); 
		year <- c(year, uniqyr); 
		Participants <- c(Participants, unique(dat_EC_MC_GreaterThan97mnthsY$TotalParticipant ))
		countryTotal_MC_GreaterThan97mnths = countryTotal_MC_GreaterThan97mnths + unique(dat_EC_MC_GreaterThan97mnthsY$TotalParticipant )
	}
}
percM_MC_GreaterThan97mnths <- (countryMaleMC_GreaterThan97mnths/countryTotal_MC_GreaterThan97mnths)*100
percF_MC_GreaterThan97mnths <- (countryFemaleMC_GreaterThan97mnths/countryTotal_MC_GreaterThan97mnths)*100

EconomicClass_HIC <- datFinal[which(datFinal$EconomicClass  == "HIC"),]
nrow(EconomicClass_HIC)
#HIC
EconomicClass_HIC_LessThan24mnths <- datFinal[which((datFinal$EconomicClass  == "HIC")
	& datFinal$Sanction_months <= 24),]
nrow(EconomicClass_HIC_LessThan24mnths)
countryTotal_HIC_LessThan24mnths = 0
countryMaleHIC_LessThan24mnths = 0
countryFemaleHIC_LessThan24mnths = 0
uniqCount <- unique(EconomicClass_HIC_LessThan24mnths$Nationality); 
country <- c(); year <- c(); Participants <- c(); MaleParticipants_HIC_LessThan24mnths <- c(); FemaleParticipants_HIC_LessThan24mnths <- c()
for(ii in 1:length(uniqCount)){
    dat_EC_HIC_LessThan24mnths <- EconomicClass_HIC_LessThan24mnths[which(EconomicClass_HIC_LessThan24mnths$Nationality == uniqCount[ii]),]
	uniqyr <- unique(dat_EC_HIC_LessThan24mnths$NearestOlympicYear)
	if(length(uniqyr) > 1){
	    for(jj in 1:length(uniqyr)){
		    
		    dat_EC_HIC_LessThan24mnthsY <- dat_EC_HIC_LessThan24mnths[which(dat_EC_HIC_LessThan24mnths$NearestOlympicYear == uniqyr[jj]),]
			MaleParticipants_HIC_LessThan24mnths <- c(MaleParticipants_HIC_LessThan24mnths, unique(dat_EC_HIC_LessThan24mnthsY$Male))
		    FemaleParticipants_HIC_LessThan24mnths <- c(FemaleParticipants_HIC_LessThan24mnths, unique(dat_EC_HIC_LessThan24mnthsY$Female))
			countryMaleHIC_LessThan24mnths = countryMaleHIC_LessThan24mnths + unique(dat_EC_HIC_LessThan24mnthsY$Male)
			countryFemaleHIC_LessThan24mnths = countryFemaleHIC_LessThan24mnths + unique(dat_EC_HIC_LessThan24mnthsY$Female)
			country <- c(country, uniqCount[ii]); 
			year <- c(year, uniqyr[jj]); 
			Participants <- c(Participants, unique(dat_EC_HIC_LessThan24mnthsY$TotalParticipant ))
		    countryTotal_HIC_LessThan24mnths = countryTotal_HIC_LessThan24mnths + unique(dat_EC_HIC_LessThan24mnthsY$TotalParticipant )
        }
	}else{
	    
	    dat_EC_HIC_LessThan24mnthsY <- dat_EC_HIC_LessThan24mnths[which(dat_EC_HIC_LessThan24mnths$NearestOlympicYear == uniqyr),]
		MaleParticipants_HIC_LessThan24mnths <- c(MaleParticipants_HIC_LessThan24mnths, unique(dat_EC_HIC_LessThan24mnthsY$Male))
		FemaleParticipants_HIC_LessThan24mnths <- c(FemaleParticipants_HIC_LessThan24mnths, unique(dat_EC_HIC_LessThan24mnthsY$Female))
		countryMaleHIC_LessThan24mnths = countryMaleHIC_LessThan24mnths + unique(dat_EC_HIC_LessThan24mnthsY$Male)
		countryFemaleHIC_LessThan24mnths = countryFemaleHIC_LessThan24mnths + unique(dat_EC_HIC_LessThan24mnthsY$Female)
		country <- c(country, uniqCount[ii]); 
		year <- c(year, uniqyr); 
		Participants <- c(Participants, unique(dat_EC_HIC_LessThan24mnthsY$TotalParticipant ))
		countryTotal_HIC_LessThan24mnths = countryTotal_HIC_LessThan24mnths + unique(dat_EC_HIC_LessThan24mnthsY$TotalParticipant )
	}
}
percM_HIC_LessThan24mnths <- (countryMaleHIC_LessThan24mnths/countryTotal_HIC_LessThan24mnths)*100
percF_HIC_LessThan24mnths <- (countryFemaleHIC_LessThan24mnths/countryTotal_HIC_LessThan24mnths)*100

EconomicClass_HIC_25_48mnths<- datFinal[which((datFinal$EconomicClass  == "HIC")
	& (datFinal$Sanction_months >= 25 
	& datFinal$Sanction_months <= 48)),]
nrow(EconomicClass_HIC_25_48mnths)
countryTotal_HIC_25_48mnths = 0
countryMaleHIC_25_48mnths = 0
countryFemaleHIC_25_48mnths = 0
uniqCount <- unique(EconomicClass_HIC_25_48mnths$Nationality); 
country <- c(); year <- c(); Participants <- c(); MaleParticipants_HIC_25_48mnths <- c(); FemaleParticipants_HIC_25_48mnths <- c()
for(ii in 1:length(uniqCount)){
    dat_EC_HIC_25_48mnths <- EconomicClass_HIC_25_48mnths[which(EconomicClass_HIC_25_48mnths$Nationality == uniqCount[ii]),]
	uniqyr <- unique(dat_EC_HIC_25_48mnths$NearestOlympicYear)
	if(length(uniqyr) > 1){
	    for(jj in 1:length(uniqyr)){
		    
		    dat_EC_HIC_25_48mnthsY <- dat_EC_HIC_25_48mnths[which(dat_EC_HIC_25_48mnths$NearestOlympicYear == uniqyr[jj]),]
			MaleParticipants_HIC_25_48mnths <- c(MaleParticipants_HIC_25_48mnths, unique(dat_EC_HIC_25_48mnthsY$Male))
		    FemaleParticipants_HIC_25_48mnths <- c(FemaleParticipants_HIC_25_48mnths, unique(dat_EC_HIC_25_48mnthsY$Female))
			countryMaleHIC_25_48mnths = countryMaleHIC_25_48mnths + unique(dat_EC_HIC_25_48mnthsY$Male)
			countryFemaleHIC_25_48mnths = countryFemaleHIC_25_48mnths + unique(dat_EC_HIC_25_48mnthsY$Female)
			country <- c(country, uniqCount[ii]); 
			year <- c(year, uniqyr[jj]); 
			Participants <- c(Participants, unique(dat_EC_HIC_25_48mnthsY$TotalParticipant ))
		    countryTotal_HIC_25_48mnths = countryTotal_HIC_25_48mnths + unique(dat_EC_HIC_25_48mnthsY$TotalParticipant )
        }
	}else{
	    
	    dat_EC_HIC_25_48mnthsY <- dat_EC_HIC_25_48mnths[which(dat_EC_HIC_25_48mnths$NearestOlympicYear == uniqyr),]
		MaleParticipants_HIC_25_48mnths <- c(MaleParticipants_HIC_25_48mnths, unique(dat_EC_HIC_25_48mnthsY$Male))
		FemaleParticipants_HIC_25_48mnths <- c(FemaleParticipants_HIC_25_48mnths, unique(dat_EC_HIC_25_48mnthsY$Female))
		countryMaleHIC_25_48mnths = countryMaleHIC_25_48mnths + unique(dat_EC_HIC_25_48mnthsY$Male)
		countryFemaleHIC_25_48mnths = countryFemaleHIC_25_48mnths + unique(dat_EC_HIC_25_48mnthsY$Female)
		country <- c(country, uniqCount[ii]); 
		year <- c(year, uniqyr); 
		Participants <- c(Participants, unique(dat_EC_HIC_25_48mnthsY$TotalParticipant ))
		countryTotal_HIC_25_48mnths = countryTotal_HIC_25_48mnths + unique(dat_EC_HIC_25_48mnthsY$TotalParticipant )
	}
}
percM_HIC_25_48mnths <- (countryMaleHIC_25_48mnths/countryTotal_HIC_25_48mnths)*100
percF_HIC_25_48mnths <- (countryFemaleHIC_25_48mnths/countryTotal_HIC_25_48mnths)*100

                  
EconomicClass_HIC_49_96mnths <- datFinal[which((datFinal$EconomicClass  == "HIC")
	& (datFinal$Sanction_months >= 49 
	& datFinal$Sanction_months <= 96)),]
nrow(EconomicClass_HIC_49_96mnths)
countryTotal_HIC_49_96mnths = 0
countryMaleHIC_49_96mnths = 0
countryFemaleHIC_49_96mnths = 0
uniqCount <- unique(EconomicClass_HIC_49_96mnths$Nationality); 
country <- c(); year <- c(); Participants <- c(); MaleParticipants_HIC_49_96mnths <- c(); FemaleParticipants_HIC_49_96mnths <- c()
for(ii in 1:length(uniqCount)){
    dat_EC_HIC_49_96mnths <- EconomicClass_HIC_49_96mnths[which(EconomicClass_HIC_49_96mnths$Nationality == uniqCount[ii]),]
	uniqyr <- unique(dat_EC_HIC_49_96mnths$NearestOlympicYear)
	if(length(uniqyr) > 1){
	    for(jj in 1:length(uniqyr)){
		    
		    dat_EC_HIC_49_96mnthsY <- dat_EC_HIC_49_96mnths[which(dat_EC_HIC_49_96mnths$NearestOlympicYear == uniqyr[jj]),]
			MaleParticipants_HIC_49_96mnths <- c(MaleParticipants_HIC_49_96mnths, unique(dat_EC_HIC_49_96mnthsY$Male))
		    FemaleParticipants_HIC_49_96mnths <- c(FemaleParticipants_HIC_49_96mnths, unique(dat_EC_HIC_49_96mnthsY$Female))
			countryMaleHIC_49_96mnths = countryMaleHIC_49_96mnths + unique(dat_EC_HIC_49_96mnthsY$Male)
			countryFemaleHIC_49_96mnths = countryFemaleHIC_49_96mnths + unique(dat_EC_HIC_49_96mnthsY$Female)
			country <- c(country, uniqCount[ii]); 
			year <- c(year, uniqyr[jj]); 
			Participants <- c(Participants, unique(dat_EC_HIC_49_96mnthsY$TotalParticipant ))
		    countryTotal_HIC_49_96mnths = countryTotal_HIC_49_96mnths + unique(dat_EC_HIC_49_96mnthsY$TotalParticipant )
        }
	}else{
	    
	    dat_EC_HIC_49_96mnthsY <- dat_EC_HIC_49_96mnths[which(dat_EC_HIC_49_96mnths$NearestOlympicYear == uniqyr),]
		MaleParticipants_HIC_49_96mnths <- c(MaleParticipants_HIC_49_96mnths, unique(dat_EC_HIC_49_96mnthsY$Male))
		FemaleParticipants_HIC_49_96mnths <- c(FemaleParticipants_HIC_49_96mnths, unique(dat_EC_HIC_49_96mnthsY$Female))
		countryMaleHIC_49_96mnths = countryMaleHIC_49_96mnths + unique(dat_EC_HIC_49_96mnthsY$Male)
		countryFemaleHIC_49_96mnths = countryFemaleHIC_49_96mnths + unique(dat_EC_HIC_49_96mnthsY$Female)
		country <- c(country, uniqCount[ii]); 
		year <- c(year, uniqyr); 
		Participants <- c(Participants, unique(dat_EC_HIC_49_96mnthsY$TotalParticipant ))
		countryTotal_HIC_49_96mnths = countryTotal_HIC_49_96mnths + unique(dat_EC_HIC_49_96mnthsY$TotalParticipant )
	}
}
percM_HIC_49_96mnths <- (countryMaleHIC_49_96mnths/countryTotal_HIC_49_96mnths)*100
percF_HIC_49_96mnths <- (countryFemaleHIC_49_96mnths/countryTotal_HIC_49_96mnths)*100

EconomicClass_HIC_GreaterThan97mnths <- datFinal[which((datFinal$EconomicClass  == "HIC")
	& (datFinal$Sanction_months >= 97)),]
nrow(EconomicClass_HIC_GreaterThan97mnths)
countryTotal_HIC_GreaterThan97mnths = 0
countryMaleHIC_GreaterThan97mnths = 0
countryFemaleHIC_GreaterThan97mnths = 0
uniqCount <- unique(EconomicClass_HIC_GreaterThan97mnths$Nationality); 
country <- c(); year <- c(); Participants <- c(); MaleParticipants_HIC_GreaterThan97mnths <- c(); FemaleParticipants_HIC_GreaterThan97mnths <- c()
for(ii in 1:length(uniqCount)){
    dat_EC_HIC_GreaterThan97mnths <- EconomicClass_HIC_GreaterThan97mnths[which(EconomicClass_HIC_GreaterThan97mnths$Nationality == uniqCount[ii]),]
	uniqyr <- unique(dat_EC_HIC_GreaterThan97mnths$NearestOlympicYear)
	if(length(uniqyr) > 1){
	    for(jj in 1:length(uniqyr)){
		    
		    dat_EC_HIC_GreaterThan97mnthsY <- dat_EC_HIC_GreaterThan97mnths[which(dat_EC_HIC_GreaterThan97mnths$NearestOlympicYear == uniqyr[jj]),]
			MaleParticipants_HIC_GreaterThan97mnths <- c(MaleParticipants_HIC_GreaterThan97mnths, unique(dat_EC_HIC_GreaterThan97mnthsY$Male))
		    FemaleParticipants_HIC_GreaterThan97mnths <- c(FemaleParticipants_HIC_GreaterThan97mnths, unique(dat_EC_HIC_GreaterThan97mnthsY$Female))
			countryMaleHIC_GreaterThan97mnths = countryMaleHIC_GreaterThan97mnths + unique(dat_EC_HIC_GreaterThan97mnthsY$Male)
			countryFemaleHIC_GreaterThan97mnths = countryFemaleHIC_GreaterThan97mnths + unique(dat_EC_HIC_GreaterThan97mnthsY$Female)
			country <- c(country, uniqCount[ii]); 
			year <- c(year, uniqyr[jj]); 
			Participants <- c(Participants, unique(dat_EC_HIC_GreaterThan97mnthsY$TotalParticipant ))
		    countryTotal_HIC_GreaterThan97mnths = countryTotal_HIC_GreaterThan97mnths + unique(dat_EC_HIC_GreaterThan97mnthsY$TotalParticipant )
        }
	}else{
	    
	    dat_EC_HIC_GreaterThan97mnthsY <- dat_EC_HIC_GreaterThan97mnths[which(dat_EC_HIC_GreaterThan97mnths$NearestOlympicYear == uniqyr),]
		MaleParticipants_HIC_GreaterThan97mnths <- c(MaleParticipants_HIC_GreaterThan97mnths, unique(dat_EC_HIC_GreaterThan97mnthsY$Male))
		FemaleParticipants_HIC_GreaterThan97mnths <- c(FemaleParticipants_HIC_GreaterThan97mnths, unique(dat_EC_HIC_GreaterThan97mnthsY$Female))
		countryMaleHIC_GreaterThan97mnths = countryMaleHIC_GreaterThan97mnths + unique(dat_EC_HIC_GreaterThan97mnthsY$Male)
		countryFemaleHIC_GreaterThan97mnths = countryFemaleHIC_GreaterThan97mnths + unique(dat_EC_HIC_GreaterThan97mnthsY$Female)
		country <- c(country, uniqCount[ii]); 
		year <- c(year, uniqyr); 
		Participants <- c(Participants, unique(dat_EC_HIC_GreaterThan97mnthsY$TotalParticipant ))
		countryTotal_HIC_GreaterThan97mnths = countryTotal_HIC_GreaterThan97mnths + unique(dat_EC_HIC_GreaterThan97mnthsY$TotalParticipant )
	}
}
percM_HIC_GreaterThan97mnths <- (countryMaleHIC_GreaterThan97mnths/countryTotal_HIC_GreaterThan97mnths)*100
percF_HIC_GreaterThan97mnths <- (countryFemaleHIC_GreaterThan97mnths/countryTotal_HIC_GreaterThan97mnths)*100


'EconomicClass_LMC <- datFinal[which(datFinal$EconomicClass  == "LMC"),]
nrow(EconomicClass_LMC)
'
'EconomicClass_LMC_GreaterThan97mnths <- datFinal[which((datFinal$EconomicClass  == "LMC")
	& datFinal$Sanction_months <= 24),]
nrow(EconomicClass_LMC_GreaterThan97mnths)
countryTotal_LMC_GreaterThan97mnths = 0
uniqCount <- unique(EconomicClass_LMC_GreaterThan97mnths$Nationality); 
country <- c(); year <- c(); Participants <- c()
for(ii in 1:length(uniqCount)){
    dat_HI_LMC_GreaterThan97mnths <- EconomicClass_LMC_GreaterThan97mnths[which(EconomicClass_LMC_GreaterThan97mnths$Nationality == uniqCount[ii]),]
	uniqyr <- unique(dat_HI_LMC_GreaterThan97mnths$NearestOlympicYear)
	if(length(uniqyr) > 1){
	    for(jj in 1:length(uniqyr)){
		    dat_HI_LMC_GreaterThan97mnthsY <- dat_HI_LMC_GreaterThan97mnths[which(dat_HI_LMC_GreaterThan97mnths$NearestOlympicYear == uniqyr[jj]),]
			country <- c(country, uniqCount[ii]); 
			year <- c(year, uniqyr[jj]); 
			Participants <- c(Participants, unique(dat_HI_LMC_GreaterThan97mnthsY$TotalParticipant ))
		    countryTotal_LMC_GreaterThan97mnths = countryTotal_LMC_GreaterThan97mnths + unique(dat_HI_LMC_GreaterThan97mnthsY$TotalParticipant )
        }
	}else{
	    dat_HI_LMC_GreaterThan97mnthsY <- dat_HI_LMC_GreaterThan97mnths[which(dat_HI_LMC_GreaterThan97mnths$NearestOlympicYear == uniqyr),]
		country <- c(country, uniqCount[ii]); 
		year <- c(year, uniqyr); 
		Participants <- c(Participants, unique(dat_HI_LMC_GreaterThan97mnthsY$TotalParticipant ))
		countryTotal_LMC_GreaterThan97mnths = countryTotal_LMC_GreaterThan97mnths + unique(dat_HI_LMC_GreaterThan97mnthsY$TotalParticipant )
	}
}
percHI_LMC_GreaterThan97mnths = (nrow(EconomicClass_LMC_GreaterThan97mnths)/countryTotal_LMC_GreaterThan97mnths)*100

EconomicClass_LMC_GreaterThan97 <- datFinal[which((datFinal$EconomicClass  == "LMC")
	& (datFinal$Sanction_months >= 25 
	& datFinal$Sanction_months <= 48)),]
nrow(EconomicClass_LMC_GreaterThan97)
countryTotal_LMC_GreaterThan97 = 0
uniqCount <- unique(EconomicClass_LMC_GreaterThan97$Nationality); 
country <- c(); year <- c(); Participants <- c()
for(ii in 1:length(uniqCount)){
    dat_HI_LMC_GreaterThan97 <- EconomicClass_LMC_GreaterThan97[which(EconomicClass_LMC_GreaterThan97$Nationality == uniqCount[ii]),]
	uniqyr <- unique(dat_HI_LMC_GreaterThan97$NearestOlympicYear)
	if(length(uniqyr) > 1){
	    for(jj in 1:length(uniqyr)){
		    dat_HI_LMC_GreaterThan97Y <- dat_HI_LMC_GreaterThan97[which(dat_HI_LMC_GreaterThan97$NearestOlympicYear == uniqyr[jj]),]
			country <- c(country, uniqCount[ii]); 
			year <- c(year, uniqyr[jj]); 
			Participants <- c(Participants, unique(dat_HI_LMC_GreaterThan97Y$TotalParticipant ))
		    countryTotal_LMC_GreaterThan97 = countryTotal_LMC_GreaterThan97 + unique(dat_HI_LMC_GreaterThan97Y$TotalParticipant )
        }
	}else{
	    dat_HI_LMC_GreaterThan97Y <- dat_HI_LMC_GreaterThan97[which(dat_HI_LMC_GreaterThan97$NearestOlympicYear == uniqyr),]
		country <- c(country, uniqCount[ii]); 
		year <- c(year, uniqyr); 
		Participants <- c(Participants, unique(dat_HI_LMC_GreaterThan97Y$TotalParticipant ))
		countryTotal_LMC_GreaterThan97 = countryTotal_LMC_GreaterThan97 + unique(dat_HI_LMC_GreaterThan97Y$TotalParticipant )
	}
}
percHI_LMC_GreaterThan97 = (nrow(EconomicClass_LMC_GreaterThan97)/countryTotal_LMC_GreaterThan97)*100


EconomicClass_LMC_GreaterThan97 <- datFinal[which((datFinal$EconomicClass  == "LMC")
	& (datFinal$Sanction_months >= 49 
	& datFinal$Sanction_months <= 96)),]
nrow(EconomicClass_LMC_GreaterThan97)
countryTotal_LMC_GreaterThan97 = 0
uniqCount <- unique(EconomicClass_LMC_GreaterThan97$Nationality); 
country <- c(); year <- c(); Participants <- c()
for(ii in 1:length(uniqCount)){
    dat_HI_LMC_GreaterThan97 <- EconomicClass_LMC_GreaterThan97[which(EconomicClass_LMC_GreaterThan97$Nationality == uniqCount[ii]),]
	uniqyr <- unique(dat_HI_LMC_GreaterThan97$NearestOlympicYear)
	if(length(uniqyr) > 1){
	    for(jj in 1:length(uniqyr)){
		    dat_HI_LMC_GreaterThan97Y <- dat_HI_LMC_GreaterThan97[which(dat_HI_LMC_GreaterThan97$NearestOlympicYear == uniqyr[jj]),]
			country <- c(country, uniqCount[ii]); 
			year <- c(year, uniqyr[jj]); 
			Participants <- c(Participants, unique(dat_HI_LMC_GreaterThan97Y$TotalParticipant ))
		    countryTotal_LMC_GreaterThan97 = countryTotal_LMC_GreaterThan97 + unique(dat_HI_LMC_GreaterThan97Y$TotalParticipant )
        }
	}else{
	    dat_HI_LMC_GreaterThan97Y <- dat_HI_LMC_GreaterThan97[which(dat_HI_LMC_GreaterThan97$NearestOlympicYear == uniqyr),]
		country <- c(country, uniqCount[ii]); 
		year <- c(year, uniqyr); 
		Participants <- c(Participants, unique(dat_HI_LMC_GreaterThan97Y$TotalParticipant ))
		countryTotal_LMC_GreaterThan97 = countryTotal_LMC_GreaterThan97 + unique(dat_HI_LMC_GreaterThan97Y$TotalParticipant )
	}
}
percHI_LMC_GreaterThan97 = (nrow(EconomicClass_LMC_GreaterThan97)/countryTotal_LMC_GreaterThan97)*100

EconomicClass_LMC_GreaterThan97mnths <- datFinal[which((datFinal$EconomicClass  == "LMC")
	& (datFinal$Sanction_months >= 97)),]
nrow(EconomicClass_LMC_97)
countryTotal_LMC_97 = 0
uniqCount <- unique(EconomicClass_LMC_97$Nationality); 
country <- c(); year <- c(); Participants <- c()
for(ii in 1:length(uniqCount)){
    dat_HI_LMC_97 <- EconomicClass_LMC_97[which(EconomicClass_LMC_97$Nationality == uniqCount[ii]),]
	uniqyr <- unique(dat_HI_LMC_97$NearestOlympicYear)
	if(length(uniqyr) > 1){
	    for(jj in 1:length(uniqyr)){
		    dat_HI_LMC_97Y <- dat_HI_LMC_97[which(dat_HI_LMC_97$NearestOlympicYear == uniqyr[jj]),]
			country <- c(country, uniqCount[ii]); 
			year <- c(year, uniqyr[jj]); 
			Participants <- c(Participants, unique(dat_HI_LMC_97Y$TotalParticipant ))
		    countryTotal_LMC_97 = countryTotal_LMC_97 + unique(dat_HI_LMC_97Y$TotalParticipant )
        }
	}else{
	    dat_HI_LMC_97Y <- dat_HI_LMC_97[which(dat_HI_LMC_97$NearestOlympicYear == uniqyr),]
		country <- c(country, uniqCount[ii]); 
		year <- c(year, uniqyr); 
		Participants <- c(Participants, unique(dat_HI_LMC_97Y$TotalParticipant ))
		countryTotal_LMC_97 = countryTotal_LMC_97 + unique(dat_HI_LMC_97Y$TotalParticipant )
	}
}
percHI_LMC_97 = (nrow(EconomicClass_LMC_97)/countryTotal_LMC_97)*100'
datEC_SP_Sex<- data.frame(EconomicClass  = c(
            "EC_LIC",
			"EC_LIC",
			"EC_LIC",
			"EC_LIC",
			"EC_LIC",
			"EC_LIC",
			"EC_LIC",
			"EC_LIC",
			"EC_MC",
			"EC_MC",
			"EC_MC",
			"EC_MC",
			"EC_MC",
			"EC_MC",
			"EC_MC",
			"EC_MC",
			"EC_HIC",
			"EC_HIC",
			"EC_HIC",
			"EC_HIC",
			"EC_HIC",
			"EC_HIC",
			"EC_HIC",
			"EC_HIC"),
        NumberofAthletes = c(
	        nrow(EconomicClass_LIC_LessThan24mnths),
			nrow(EconomicClass_LIC_LessThan24mnths),
            nrow(EconomicClass_LIC_25_48mnths),	
            nrow(EconomicClass_LIC_25_48mnths),			
	        nrow(EconomicClass_LIC_49_96mnths),
			nrow(EconomicClass_LIC_49_96mnths),
			nrow(EconomicClass_LIC_GreaterThan97mnths),
			nrow(EconomicClass_LIC_GreaterThan97mnths),
			nrow(EconomicClass_MC_LessThan24mnths),
			nrow(EconomicClass_MC_LessThan24mnths),
            nrow(EconomicClass_MC_25_48mnths),	
            nrow(EconomicClass_MC_25_48mnths),			
	        nrow(EconomicClass_MC_49_96mnths),
			nrow(EconomicClass_MC_49_96mnths),
			nrow(EconomicClass_MC_GreaterThan97mnths),
			nrow(EconomicClass_MC_GreaterThan97mnths),
			nrow(EconomicClass_HIC_LessThan24mnths),
			nrow(EconomicClass_HIC_LessThan24mnths),
            nrow(EconomicClass_HIC_25_48mnths),	
            nrow(EconomicClass_HIC_25_48mnths),			
	        nrow(EconomicClass_HIC_49_96mnths),
			nrow(EconomicClass_HIC_49_96mnths),
			nrow(EconomicClass_HIC_GreaterThan97mnths),
			nrow(EconomicClass_HIC_GreaterThan97mnths)),
	    TotalAthletes = c(countryMaleLIC_LessThan24mnths,countryFemaleLIC_LessThan24mnths,
		    countryMaleLIC_25_48mnths,countryFemaleLIC_25_48mnths,
			countryMaleLIC_49_96mnths,countryFemaleLIC_49_96mnths,
			countryMaleLIC_GreaterThan97mnths,countryFemaleLIC_GreaterThan97mnths,
			countryMaleMC_LessThan24mnths,countryFemaleMC_LessThan24mnths,
		    countryMaleMC_25_48mnths,countryFemaleMC_25_48mnths,
			countryMaleMC_49_96mnths,countryFemaleMC_49_96mnths,
			countryMaleMC_GreaterThan97mnths,countryFemaleMC_GreaterThan97mnths,
			countryMaleHIC_LessThan24mnths,countryFemaleHIC_LessThan24mnths,
		    countryMaleHIC_25_48mnths,countryFemaleHIC_25_48mnths,
			countryMaleHIC_49_96mnths,countryFemaleHIC_49_96mnths,
			countryMaleHIC_GreaterThan97mnths,countryFemaleHIC_GreaterThan97mnths),
        SanctionPercent = c(percM_LIC_LessThan24mnths,percF_LIC_LessThan24mnths,
		   percM_LIC_25_48mnths, percF_LIC_25_48mnths,
			percM_LIC_49_96mnths,percF_LIC_49_96mnths,
			percM_LIC_GreaterThan97mnths,percF_LIC_GreaterThan97mnths,
			percM_MC_LessThan24mnths,percF_MC_LessThan24mnths,
		   percM_MC_25_48mnths, percF_MC_25_48mnths,
			percM_MC_49_96mnths,percF_MC_49_96mnths,
			percM_MC_GreaterThan97mnths,percF_MC_GreaterThan97mnths,
			percM_HIC_LessThan24mnths,percF_HIC_LessThan24mnths,
		   percM_HIC_25_48mnths, percF_HIC_25_48mnths,
			percM_HIC_49_96mnths,percF_HIC_49_96mnths,
			percM_HIC_GreaterThan97mnths,percF_HIC_GreaterThan97mnths),
		Sex = c("Male", "Female", "Male", "Female", "Male", "Female", "Male", "Female", "Male", "Female", "Male", "Female", "Male", "Female", "Male", "Female", "Male", "Female","Male", "Female","Male", "Female","Male", "Female"),
		SanctionPeriod = c("<=24mnths",
		    "<=24mnths",
		    "25_48mnths",
			"25_48mnths",
			"49_96mnths",
			"49_96mnths",
			">=97mnths",
			">=97mnths",
			"<=24mnths",
		    "<=24mnths",
		    "25_48mnths",
			"25_48mnths",
			"49_96mnths",
			"49_96mnths",
			">=97mnths",
			">=97mnths",
			"<=24mnths",
		    "<=24mnths",
		    "25_48mnths",
			"25_48mnths",
			"49_96mnths",
			"49_96mnths",
			">=97mnths",
			">=97mnths"))
write.csv(datEC_SP_Sex, file.path(outdir,"EconomicClass_PerSanctionPeriodSex.csv"), row.names = FALSE)

library(ggplot2)
p8 <- ggplot(data = datEC_SP_Sex, aes(x = factor(SanctionPeriod, levels=unique(SanctionPeriod)), y= SanctionPercent, fill = factor(Sex, level = unique(Sex)))) + geom_bar(stat = "identity", position="stack")+ labs(x = "SanctionPeriod", y = "% of Sanctioned Athletes", fill = "Human Index") + theme(legend.title = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.title.x = element_text(face = "bold", size = 50),axis.title.y = element_text(face = "bold", size = 50), axis.text.x = element_text(face="bold", size = 30, angle = 45, hjust = 1), axis.text.y = element_text(face="bold", size = 30), legend.text=element_text(face = "bold",size=25),  strip.text = element_text(size = 25))  +  facet_wrap( ~ factor(EconomicClass , level = c("EC_LIC","EC_MC", "EC_HIC")))
p8


##Country athlete distribution

  
library(tidyverse)
world <- map_data("world")
athl <- read.csv("Z:/Suro/AtheleteSanction/AtheleteSanction05262021/AthleteLocations.csv")
a <- unique(athl$Sanctioned)
ggplot() +
    geom_map(
        data = world, map = world,
        aes(long, lat, map_id = region),
        color = "white", fill = "lightgray", size = 0.1
    ) +
    geom_point(
        data = athl,
        aes(Longitude, Latitude, size= Sanctioned),
        alpha = 0.7
    )+
	 labs(x = "Longitude", y = "Latitude", size = "Sanctioned Athletes") + 
	 scale_size(
                           breaks = c(0, 20, 40, 60, 80),
                           labels = c(0, 20, 40, 60, 80),
                           range = c(5, 15)
    )+ theme(
    legend.title = element_text(size = 20,
                                face = "bold"),
    axis.title.x = element_text(face = "bold", size = 20),
    axis.title.y = element_text(face = "bold", size = 20),
    axis.text.x = element_text(face = "bold",size = 25),
    axis.text.y = element_text(face = "bold", size = 30),
    legend.text = element_text(face = "bold", size = 20)
  )

ggplot() +
    geom_map(
        data = world, map = world,
        aes(long, lat, map_id = region),
        color = "white", fill = "lightgray", size = 0.1
    ) +
geom_sf(aes(Longitude, Latitude, fill = Sanctioned)) +
    scale_fill_viridis_c(option = "plasma", trans = "sqrt")


library("rworldmap")
library(tidyverse)
#world <- map_data("world")
athl <- read.csv("Z:/Suro/AtheleteSanction/AtheleteSanction05262021/AthleteLocations.csv")
mapDevice('x11')
#join to a coarse resolution map
sathl <- joinCountryData2Map(athl, joinCode="NAME", nameJoinColumn="CountryFullName")

mapCountryData(sathl, nameColumnToPlot="Sanctioned", catMethod="fixedWidth")



library(ggplot2)
r5 <- read.csv("Z:/Suro/AtheleteSanction/GlobalClassificationvsSanctionMonths.csv")
p6 <- ggplot(data = r5, aes(x = SanctionPeriod, y= NumberofAthletes, fill = factor(GlobalClassifications, level = c("GN","GN")))) + geom_bar(stat = "identity", position="dodge")+ labs(x = "SanctionPeriod", y = "Number of Athletes", fill = "Global Classifications") + theme(legend.title = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.title.x = element_text(face = "bold", siZ:e = 50),axis.title.y = element_text(face = "bold", siZ:e = 50), axis.text.x = element_text(face="bold", siZ:e = 30, angle = 45, vjust = 0.5), axis.text.y = element_text(face="bold", siZ:e = 30), legend.text=element_text(face = "bold",siZ:e=25)) + scale_fill_manual(values=c("green","red")) + scale_x_continuous(breaks = c((unique(r5$SanctionPeriod))),labels = c("lessThan24", "Between25to48", "Between49to96", "greaterThan97"))


library(ggplot2)
r5 <- read.csv("Z:/Suro/AtheleteSanction/GlobalClassificationvsSanctionMonths.csv")
p6 <- ggplot(data = r5, aes(x = GlobalClassifications, y= PercentageAthletes, fill = factor(SanctionPeriod, level = c("greaterThan97","Between25to48", "Between49to96", "lessThan24" )))) + geom_bar(stat = "identity")+ labs(x = "GlobalClassifications", y = "PercentageAthletes", fill = "SanctionPeriod") + theme(legend.title = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.title.x = element_text(face = "bold", siZ:e = 50),axis.title.y = element_text(face = "bold", siZ:e = 50), axis.text.x = element_text(face="bold", siZ:e = 30, angle = 45, vjust = 0.5), axis.text.y = element_text(face="bold", siZ:e = 30), legend.text=element_text(face = "bold",siZ:e=25)) 


###AthleteSanctionYearsBySex
library(ggplot2)
r <- read.csv("Z:/Suro/AtheleteSanction/AtheleteSanction05262021/Athlete_SanctionedTimes_01022022.csv")
p8 <- ggplot(data = r, aes(x = factor(SanctionedTime, levels=unique(SanctionedTime)), y= Athletes, fill = factor(Sex, level = c("Male", "Female")))) + geom_bar(stat = "identity", position="stack")+ labs(x = "SanctionDuration", y = "Number of Athletes", fill = "Sex") + theme(legend.title = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.title.x = element_text(face = "bold", size = 50),axis.title.y = element_text(face = "bold", size = 60), axis.text.x = element_text(face="bold", size = 60, angle = 45, vjust = 0.5), axis.text.y = element_text(face="bold", size = 60), legend.text=element_text(face = "bold",size=50),  strip.text = element_text(size = 40))  + scale_fill_manual(values=(c("green", "hotpink")))

###AthleteSanctionYearsBySex
library(ggplot2)
r1 <- read.csv("Z:/Suro/AtheleteSanction/AtheleteSanction05262021/SanctionedPerYear.csv")
p8 <- ggplot(data = r1, aes(x = factor(Year, levels=unique(Year)), y= SanctionedAthletes)) + geom_bar(stat = "identity")+ labs(x = "Year of Sanction", y = "Number of Athletes") + theme(legend.title = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.title.x = element_text(face = "bold", size = 50),axis.title.y = element_text(face = "bold", size = 50), axis.text.x = element_text(face="bold", size = 30, angle = 45, vjust = 0.5), axis.text.y = element_text(face="bold", size = 30), legend.text=element_text(face = "bold",size=25),  strip.text = element_text(size = 25))  