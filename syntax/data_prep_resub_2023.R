  # Syntax for Walthéry & Chung paper
  # 
  # Author: Pierre Walthery
  # February 2021
  # Last updated: 4 July 2022
  ###############################################################################
  
  rm(list=ls())
setwd("/home/piet/Dropbox/work/CTUR/")
  pkgs<-c("dplyr","foreign","haven","ggplot2",
		      "weights","htmlTable","reshape2","geepack","gee","texreg","xtable",
		      "modelsummary","kableExtra","gt")

  
  lapply(pkgs, library, character.only = TRUE)
  
 
	
  cbPalette <- c("#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7","#FFFFFF","#56B4E9")
  lab.mod<-c("work-life balance satisfaction", "life satisfaction", "Satisfaction with social life", "enjoyment")
  ynames<-list("nsatbal.m", "nsatis.st.m", "nsatsoc.m", "nenjoy.d.m",
		           "nsatbal.d", "nsatis.st.d", "nsatsoc.d", "nenjoy.d.d")
  
  
  xnames<-list(  "car.rou.rat", "car.enr.rat", "car.aln.rat", "car.spo.rat", "car.pri.rat" ,"car.sec.rat")
  xv<-c("pri","aln", "rou","enr")

  xvars.m<-"car.tmp+dvage.m+hhftpt+nhiqual3.m+wee.m"
  xvars.d<-"car.tmp+dvage.d+hhftpt+nhiqual3.d+wee.d"
  
  
  
  ### We first open the base individual dataset
  
  ind.tus15<-read_dta("data/UKTUS/2015/UKDS/UKDA-8128-stata11_se/stata11_se/uktus15_individual.dta")%>%
		  select(CCarTyp1,CCarTyp2,CCarTyp3,CCarTyp4,CCarTyp5,CCarTyp6,CCarTyp7,CCarTyp8,CCarTyp9,CCarTy10,dilodefr,DM014,DMSex,
				  dsoc,DVAge,IncCat,DVHsize,FtPtWk,HiQual,HhOut,ind_wt,NumAdult,NumChild,NumMPart,NumCPart,dhhtype,dnrkid04,Anxious,
				  GenHlth,Happy,SatBal,SatInc, SatHlth,
				  NumCPart,NumMPart,NumCivP,NumSSex,DM016,DM1619,
				  SatPart,SatisOv,Satis,SatJob,SatLeis,SatSoc,SEHrWkUs,dmarsta,
				  serial,pnum,Relate1,Relate2,Relate3,Relate4,Relate5,Relate6,Relate7,Relate8,Relate9,Relate10,
				  Rushed,Sector,SIC2007,WkArrang,WkArran2,WkArran3,WkArran4,WkArran5,WkArran6,WkArran7,WkArran8,
				  WkArran9, WorkSta,Worth,XSOC2000)

  #### Population selection
  # Respondents ages 16+
  # Households with fully productive interviews
  # Married couples with children
  # Only children under 11
  names(ind.tus15)<-tolower(names(ind.tus15))
  #### Age of the youngest child (household -level)
  ind.tus15<-droplevels(merge(ind.tus15,read_dta("data/UKTUS/PW/agekidvars_16.dta"),by="serial", all.x=T,all.y=F))				
  
  ## Whether parent
  ind.tus15$npar<-ifelse(ind.tus15$relate1==8 | ind.tus15$relate2==8 | ind.tus15$relate3==8 | 
                           ind.tus15$relate4==8 | ind.tus15$relate5==8 | ind.tus15$relate6==8 | ind.tus15$relate7==8 |
                           ind.tus15$relate8==8 | ind.tus15$relate9==8 | ind.tus15$relate10==8,"Parent","Not")
  
  
  ind.tus15<-droplevels(ind.tus15%>%
                          filter(as_factor(hhout)=="Productive : Household interview completed, all eligible household members completed individual interviews and diary",
                                 as_factor(dhhtype)=="Married/cohab couple - with children <= 15",
                                 dvage>=16,
                                 as_factor(npar)=="Parent",
                                 as_factor(agekidx)=="0-4" | as_factor(agekidx)=="5-10"
                          ))
  
  
  
  
  #### Household income
  ind.tus15<-ind.tus15<-merge(ind.tus15,
		  read_dta("data/UKTUS/2015/UKDS/UKDA-8128-stata11_se/stata11_se/uktus15_household.dta")%>%
				  select(Income,IncCat,serial),by="serial", all.x=T,all.y=F)				
  

### Need this as a compatibility layer with previously recoded work-related data
ind.tus15$pid<-((ind.tus15$serial*100)+ind.tus15$pnum)


				 
  
  #### Temporary fix for incorrect dhhtype 
  ind.tus15<-merge(ind.tus15%>%select(-dhhtype),
		     read_dta("data/UKTUS/2015/UKDS/UKDA-8128-stata11_se/stata11_se/hhtype.dta")%>%select(dhhtype3,serial),by="serial", all.x=T,all.y=F)				
  ind.tus15$dhhtype<-ind.tus15$dhhtype3
  
  #### Under 16
  ind.tus15$ischi015<-ifelse(ind.tus15$dvage<16,1,0)

  ind.tus15<-merge(ind.tus15,
		  ind.tus15%>%select(serial,ischi015)%>%group_by(serial)%>%
		    mutate(nrchi015=sum(ischi015),rn=row_number())%>%ungroup()%>%filter(rn==1)%>%select(-ischi015),
		  by="serial",all.x=T,all.y=F)
  
   ind.tus15$ischi1618<-ifelse(ind.tus15$dvage>=16 & ind.tus15$dvage<19 & (as_factor(ind.tus15$worksta)=="Full-time student" | as_factor(ind.tus15$worksta)=="On a government training scheme"),"16-18,FT Student","Not")
  

#### 16-18, not in FTE, not 
		   ind.tus15$ischi1618<-ifelse(ind.tus15$dvage>=16 & ind.tus15$dvage<19 & (as_factor(ind.tus15$worksta)=="Full-time student" | as_factor(ind.tus15$worksta)=="On a government training scheme") & as_factor(ind.tus15$dmarsta)!="Married/cohabitating",1,0)
		   		   ind.tus15<-merge(ind.tus15,
				   ind.tus15%>%select(serial,ischi1618)%>%
				               group_by(serial)%>%
				               mutate(nrchi1618=sum(ischi1618))%>%
				               ungroup()%>%select(-ischi1618),
				   by="serial")
		   
		   ind.tus15$test<-ind.tus15$numcpart+ind.tus15$nummpart
  
		   
  ind.tus15$nrcohab<-ind.tus15$numcpart+ind.tus15$nummpart  +ind.tus15$numcivp+ind.tus15$numssex

  ind.tus15$tmp<-ind.tus15$numadult+ind.tus15$dm1619
 
  ind.tus15$dhhtype3<-NA
  ind.tus15$dhhtype3<-ifelse(ind.tus15$numadult==1 & ind.tus15$dvhsize==1 ,"Single person household",ind.tus15$dhhtype3)
  ind.tus15$dhhtype3<-ifelse(ind.tus15$numadult==2 & ind.tus15$dvhsize>2 & ind.tus15$nrcohab==2 & ind.tus15$nrchi015>0,
		                     "Married/cohab couple - with children <= 15",ind.tus15$dhhtype3)
  ind.tus15$dhhtype3<-ifelse((ind.tus15$numadult==2 & ind.tus15$dvhsize==2 & ind.tus15$nrcohab==2 & ind.tus15$nrchi015==0) |  
				            ((ind.tus15$numadult+ind.tus15$nrchi1618==ind.tus15$dvhsize) & ind.tus15$nrcohab==2 & ind.tus15$nrchi015==0),"Married/cohab couple -  no children <= 15",ind.tus15$dhhtype3)
  ind.tus15$dhhtype3<-ifelse(ind.tus15$numadult==1 & ind.tus15$dvhsize>1 & ind.tus15$nrcohab==0 & ind.tus15$numchild>0,"Single parent  - with children <= 15",ind.tus15$dhhtype3)
  ind.tus15$dhhtype3<-ifelse(ind.tus15$numadult>1 & ind.tus15$dvhsize>1  & ind.tus15$nrcohab==0 & ind.tus15$nrchi015==0 & 
				            (ind.tus15$numadult+ind.tus15$nrchi1618==ind.tus15$dvhsize),"Single parent -   no children <= 15",ind.tus15$dhhtype3)
  ind.tus15$dhhtype3<-ifelse(ind.tus15$nrcohab>0 & ind.tus15$dvhsize>=3 & is.na(ind.tus15$dhhtype3)==T,"Married/cohab couples in other hhlds",ind.tus15$dhhtype3)
  ind.tus15$dhhtype3<-ifelse(ind.tus15$nrcohab==0 & ind.tus15$dvhsize>1 & is.na(ind.tus15$dhhtype3)==T,"Other hhlds eg brothers/sisters; unrelated; etc",ind.tus15$dhhtype3)

  ind.tus15$dhhtype4<-NA
  ind.tus15$dhhtype4<-ifelse(ind.tus15$numadult==1 & ind.tus15$dvhsize==1 ,"Single person household",ind.tus15$dhhtype4)
  ind.tus15$dhhtype4<-ifelse(ind.tus15$numadult==2 & ind.tus15$dvhsize>2 & ind.tus15$nrcohab==2 & ind.tus15$dm016>0,
		  "Married/cohab couple - with children <= 16",ind.tus15$dhhtype4)
  ind.tus15$dhhtype4<-ifelse((ind.tus15$numadult==2 & ind.tus15$dvhsize==2 & ind.tus15$nrcohab==2 & ind.tus15$dm016==0) |  
				  ((ind.tus15$numadult+ind.tus15$dm1619==ind.tus15$dvhsize) & ind.tus15$nrcohab==2 & ind.tus15$dm016==0),"Married/cohab couple -  no children <=16",ind.tus15$dhhtype4)
  ind.tus15$dhhtype4<-ifelse(ind.tus15$numadult==1 & ind.tus15$dvhsize>1 & ind.tus15$nrcohab==0 & ind.tus15$numchild>0,"Single parent  - with children <= 16",ind.tus15$dhhtype4)
  ind.tus15$dhhtype4<-ifelse(ind.tus15$dvhsize>1  & ind.tus15$nrcohab==0 & ind.tus15$dm016==0 & 
				  (ind.tus15$dm1619==(ind.tus15$dvhsize-1)),"Single parent - Children 16-19",ind.tus15$dhhtype4)
  ind.tus15$dhhtype4<-ifelse(ind.tus15$nrcohab>0 & ind.tus15$dvhsize>=3 & is.na(ind.tus15$dhhtype4)==T,"Married/cohab couples in other hhlds",ind.tus15$dhhtype4)
  ind.tus15$dhhtype4<-ifelse(ind.tus15$nrcohab==0 & ind.tus15$dvhsize>1 & is.na(ind.tus15$dhhtype4)==T,"Other hhlds, not married/cohab. etc",ind.tus15$dhhtype4)
  
  
  
  
  

  
  ### Employed are either self employed, in paid employment or on maternity leave
  ind.tus15$inpdw<-as.factor(ifelse(as_factor(ind.tus15$dilodefr)=="In employment", "In paid work","Not in paid work"))
  
  
  
  ind.tus15$inpdw2<-as.factor(ifelse(as_factor(ind.tus15$worksta)=="In paid employment (full or part-time)" |  
                                       as_factor(ind.tus15$worksta)=="On maternity leave" |  
                                       as_factor(ind.tus15$worksta)=="Self employed","In paid work","Not in paid work"))
  
  

#### SOC 2010 industry, ONS four category skills levels indicator

		ind.tus15$nsk4<-ifelse((ind.tus15$xsoc2000>=1100 & ind.tus15$xsoc2000<1200) |  (ind.tus15$xsoc2000>=2100 & ind.tus15$xsoc2000<22500),"SL4","NA")
		ind.tus15$nsk4<-ifelse( (ind.tus15$xsoc2000>=1200 & ind.tus15$xsoc2000<1300) |(ind.tus15$xsoc2000>=3100 & ind.tus15$xsoc2000<3600) |(ind.tus15$xsoc2000>=5100 & ind.tus15$xsoc2000<5500),"SL3",ind.tus15$nsk4)
		ind.tus15$nsk4<-ifelse((ind.tus15$xsoc2000>=4100 & ind.tus15$xsoc2000<4300) | (ind.tus15$xsoc2000>=6100 & ind.tus15$xsoc2000<8300),"SL2",ind.tus15$nsk4)
		ind.tus15$nsk4<-ifelse(ind.tus15$xsoc2000>=9100 & ind.tus15$xsoc2000<9300,"SL1",ind.tus15$nsk4)
  
  
		
  
  #### flexible WTA
  ind.tus15<-ind.tus15%>%mutate(wflex=
                                  case_when((as_factor(wkarrang)=="Yes"  | as_factor(wkarran2)=="Yes" | 
                                               as_factor(wkarran3)=="Yes" | as_factor(wkarran4)=="Yes" | 
                                               as_factor(wkarran5)=="Yes" | as_factor(wkarran6)=="Yes" | 
                                               as_factor(wkarran7)=="Yes") & as_factor(inpdw)=="In paid work"~ 'Yes',
				                                    (as_factor(wkarran8)=="Yes" | as_factor(wkarran9)=="Yes" | 
				                                     as_factor(wkarran9)=="Item not applicable" | as_factor(wkarran9)=="No answer/refused") &
				                                      as_factor(inpdw)=="In paid work"~ 'No',
				                                    as_factor(inpdw)=="Not in paid work"~"Not in paid work"))
  
							   
  
  							   #### Combined skills levels and flexible WTA
  							   ind.tus15<-ind.tus15%>% mutate(slfl= as.factor(paste(nsk4, wflex,sep=" ")))
  							   ind.tus15$slfl<-as.character(ind.tus15$slfl)
  							   ind.tus15$slfl<-ifelse(as_factor(ind.tus15$inpdw)=="Not in paid work","Not in paid work",ind.tus15$slfl)
  							   ind.tus15$slfl<-ifelse(is.na(ind.tus15$nsk4)==T,NA,ind.tus15$slfl)
  							   ind.tus15$slfl<-as.factor(ind.tus15$slfl)
  							   							   ind.tus15$slfl<-factor(ind.tus15$slfl,levels(ind.tus15$slfl)[c(8,1:7,9)])
  											
  														   
  														   
  														   #### Full-time/part-time work
  ind.tus15<- ind.tus15%>%mutate(wftpt = case_when(
  				(as_factor(ftptwk)=="...full time," | (sehrwkus>30 & sehrwkus<=200)) & inpdw=="In paid work" ~"Full-time" ,
  				(as_factor(ftptwk)=="or part time?" | (sehrwkus>=1 & sehrwkus<=30)) & inpdw=="In paid work"~"Part-time"  ,
  				inpdw=="Not in paid work" ~"Not in paid work"))
  
  
  
  
  #### Combined Working-time and flexible working-time arrangements
  ind.tus15<-ind.tus15%>%filter( !is.na(wftpt) & !is.na(wflex))%>%mutate(flexpt = as.factor(paste(wftpt, wflex,sep=" ")))
  
  ind.tus15$flexpt<-factor(ind.tus15$flexpt,levels(ind.tus15$flexpt)[c(3,1,2,4,5)])
  
  
  #### Combined sector and flexible WTA
  ind.tus15$nsect<-as.factor(
                   ifelse(
                   as_factor(ind.tus15$sector)=="Item not applicable" | as_factor(ind.tus15$sector)=="Don't know" | 
                     as_factor(ind.tus15$sector)=="No answer/refused",NA,ind.tus15$sector))
  levels(ind.tus15$nsect)<-c("Private","Public")
  
  ind.tus15<-ind.tus15%>%mutate(sectfl= as.factor(paste(nsect, wflex,sep=" ")))
  ind.tus15$sectfl<-as.character(ind.tus15$sectfl)
  ind.tus15$sectfl<-ifelse(is.na(ind.tus15$nsect) ,NA,ind.tus15$sectfl)
  ind.tus15$sectfl<-ifelse(ind.tus15$inpdw=="Not in paid work" ,"Not in paid work",ind.tus15$sectfl)
  
  
  
  
  ### Education five categories
  
  ind.tus15 <- mutate(ind.tus15, nhiqual5 = case_when(
  				(as_factor(hiqual)=='Degree level qualification incl. foundation degrees, graduat' | as_factor(hiqual)=='Diploma in higher education')~"Degree" , 
  				as_factor(hiqual)=='HNC / HND' | as_factor(hiqual)=='ONC / OND' | as_factor(hiqual)== 'BTEC/BEC/TEC/EdExcel/LQL' | 
  						as_factor(hiqual)== 'DSCOTVEC/SCOTEC/SCOTBEC (Scotland)' | as_factor(hiqual)== 'Teaching qualification (excluding PGCE)' | 
  						as_factor(hiqual)== 'Nursing or other medical qualification not yet mentioned' |
  						as_factor(hiqual)=='Other higher education qualification below degree level'~"Higher" ,
  				as_factor(hiqual)==' A level/GCE in Applied Subjects or equivalent' | as_factor(hiqual)=='New Diploma' | as_factor(hiqual)=='Welsh Baccalaureate' |
  						as_factor(hiqual)=='International Baccalaureate' | as_factor(hiqual)=='NVQ / SVQ' | as_factor(hiqual)=='GNVQ / GSVQ' |
  						as_factor(hiqual)=='AS level or equivalent' | as_factor(hiqual)=='Certificate of sixth years studies (CSYS) (Scotland)' |
  						as_factor(hiqual)=='Leaving certificate (Republic of Ireland)' | as_factor(hiqual)== 'Access to HE'~"A Level" ,
  				as_factor(hiqual)== "O level or equivalent" | as_factor(hiqual) == 'Standard Grade or Ordinary Grade / Lower (Scotland)' |
  						as_factor(hiqual)=='GCSE/Vocational GCSE'~"GCSE and equ."     ,
  				as_factor(hiqual)=='Advanced Higher/Higher/Intermediate/Access qualifications (S' | as_factor(hiqual)=="Junior certificate (Republic of Ireland)" |
  						as_factor(hiqual)=='cse' | as_factor(hiqual)=="RSA/OCR" | as_factor(hiqual)=='YT certificate / YTP' | as_factor(hiqual)=="City and Guilds" |
  						as_factor(hiqual)=='Key Skills (Eng., W and NI)/Core Skills (Scotland)' | as_factor(hiqual)=="Entry Level Qualifications" |
  						as_factor(hiqual)=='Award, Certificate or Diploma, at entry level and level 1 to'~"Below GCSE" ,
  				as_factor(hiqual)=="Any other professional/vocational/foreign qualifications" | as_factor(hiqual)== "None of the above"~"Other"
  		))  
  
  
  
  ### Education four categories
  ind.tus15 <- mutate(ind.tus15, nhiqual4 = case_when(
  				(nhiqual5=='Degree')~"Degree" , 
  				(nhiqual5=='Higher' | nhiqual5=='A Level')~"Above GCSE" ,
  				(nhiqual5=='GCSE and equ.')~"GCSE and equ." ,
  				(nhiqual5=='Below GCSE')~"Below GCSE" ,
  				(nhiqual5=='Other')~"Other"   		))  
  

  ### Education three categories
  ind.tus15 <- mutate(ind.tus15, nhiqual3 = case_when(
				  (nhiqual5=='Degree')~"Degree" , 
  				(nhiqual5=='Higher')~"Higher Ed" ,
  				(nhiqual5=='GCSE and equ.' | nhiqual5=='A Level' | nhiqual5=='Below GCSE' | nhiqual5=='Other')~"Secondary or below" 
  		))  
  
  
  
  ### Public vs private sector
  ind.tus15 <- ind.tus15 %>% mutate(nsect= case_when(
    as_factor(ind.tus15$sector)=='Public sector'~"Public"  ,
    as_factor(ind.tus15$sector)=='Private sector'~"Private" 
  		))  
  
  
  
  ind.tus15$nsect<-as.factor(ifelse(ind.tus15$inpdw=="In paid work" & !is.na(ind.tus15$nsect),as.character(ind.tus15$nsect),"Not in paid work"))
  
  ind.tus15$nsect<-factor(ind.tus15$nsect,levels(ind.tus15$nsect)[c(2,3,1)])
  
  
  ### External childcare
  ### Beware, the variable is defined at the *Child* level
  ### It needs to be constructed as a household-level one
  
  ind.tus15$ccare.tmp<-ifelse(as_factor(ind.tus15$ccartyp1)=="Yes" | as_factor(ind.tus15$ccartyp2)=="Yes"| as_factor(ind.tus15$ccartyp3)=="Yes" | as_factor(ind.tus15$ccartyp4)=="Yes" |
  				          as_factor(ind.tus15$ccartyp5)=="Yes" | as_factor(ind.tus15$ccartyp6)=="Yes" | as_factor(ind.tus15$ccartyp7)=="Yes" | as_factor(ind.tus15$ccartyp8)=="Yes" | 
  						  as_factor(ind.tus15$ccartyp9)=="Yes" | as_factor(ind.tus15$ccarty10)=="Yes",
  		         1,0)
  
  ind.tus15<-ind.tus15%>%group_by(serial)%>%mutate(tmp=sum(ccare.tmp),ccare.d=ifelse(tmp>0,"Yes","No" ))
  		         
  ind.tus15$ccare.d<-ifelse(is.na(ind.tus15$ccare.d)==T,"No",ind.tus15$ccare.d)
  		 
  
  for(i in c("satisov","satjob","satbal","satinc", "satpart","satleis","satsoc","sathlth")){
  	ind.tus15<-ind.tus15 %>% mutate(tmp=ifelse(eval(parse(text=i))>0,eval(parse(text=i)),NA))

  	                                	ind.tus15$tmp<-ifelse(ind.tus15$tmp==8,7,ind.tus15$tmp)
  	names(ind.tus15)[which(names(ind.tus15)=="tmp")]<-paste0("n",i)
  	
#  	ind.tus15<-ind.tus15 %>% mutate(tmp=as.numeric(recode_factor(eval(parse(text=i)),"Completely dissatisfied"=-3,
#  							"Mostly dissatisfied"=-2,
#  							"Somewhat dissatisfied"=-1,
#  							"Neither satisfied or dissatisfied"=0,
#  							"Somewhat satisfied"=1,
#  							"Mostly satisfied"=2,
#  							"Completely satisfied"=3)))
#  	names(ind.tus15)[which(names(ind.tus15)=="tmp")]<-paste0("nm",i)
  }
  
  ind.tus15$nrusha<-ifelse(
                    as_factor(ind.tus15$rushed)=="Always" & !(as_factor(ind.tus15$rushed)=="Item not applicable"| as_factor(ind.tus15$rushed)=="Don't know" | as_factor(ind.tus15$rushed)=="No answer/refused"),"Rushed", "Not Rushed")
  
  ind.tus15<-ind.tus15 %>% mutate(nrusha=
                          case_when(as_factor(rushed)=="Always"~"Always",
                                    as_factor(rushed)=="Sometimes"~"Not always",
                                    as_factor(rushed)=="Never"~"Not always",
  						                      .default = NA_character_))
  

  
  ind.tus15$nsatis<-ifelse(ind.tus15$satis>=0,ind.tus15$satis,NA)
  ind.tus15$nanxious<-ifelse(ind.tus15$anxious>=0,ind.tus15$anxious,NA)
  ind.tus15$nhappy<-ifelse(ind.tus15$happy>=0,ind.tus15$happy,NA)
  ind.tus15$nworth<-ifelse(ind.tus15$worth>=0,ind.tus15$worth,NA)
  
  
  
  ### First homogeneised life satisfaction instrument standardised
  ind.tus15$nsatis.st<-(ind.tus15$nsatis-mean(ind.tus15$nsatis,na.rm=T))/sd(ind.tus15$nsatis,na.rm=T)
  ind.tus15$nsatisov.st<-(ind.tus15$nsatisov-mean(ind.tus15$nsatisov,na.rm=T))/sd(ind.tus15$nsatisov,na.rm=T)
  
  ind.tus15$satis.st<-ind.tus15$nsatis.st
    ind.tus15$satis.st<-ifelse(is.na(ind.tus15$nsatis.st) & !is.na(ind.tus15$nsatisov.st),ind.tus15$nsatisov.st,ind.tus15$nsatis.st)
  
	### Other wellbeing instruments standardised
	ind.tus15$nsatbal.st<-(ind.tus15$nsatbal-mean(ind.tus15$nsatbal,na.rm=T))/sd(ind.tus15$nsatbal,na.rm=T)
	ind.tus15$nsatpart.st<-(ind.tus15$nsatpart-mean(ind.tus15$nsatpart,na.rm=T))/sd(ind.tus15$nsatpart,na.rm=T)
	ind.tus15$nhappy.st<-(ind.tus15$nhappy-mean(ind.tus15$nhappy,na.rm=T))/sd(ind.tus15$nhappy,na.rm=T)
	
	
  
  
  
  
  ind.tus15$nkid<-ifelse(ind.tus15$relate1==4 | ind.tus15$relate2==4 | ind.tus15$relate3==4 | 
  					   ind.tus15$relate4==4 | ind.tus15$relate5==4 | ind.tus15$relate6==4 | ind.tus15$relate7==4 |
  					   ind.tus15$relate4==4 | ind.tus15$relate9==4 | ind.tus15$relate10==4,"Child","Not")
  	   
  	   ind.tus15$agekidx[as_factor(ind.tus15$agekidx)=="Item not applicable"] <-NA
  	   

	   
	   
	   
  
  #### Diary  Long
  #ep.chap15<-data.frame(read.dta("data/UKTUS/PW/epis_chap0015.dta",convert.underscore = T) %>% filter(year==2015 ) %>% select(-'what.oth1'))
### Previously recoded dataset  
ep.chap15<-read_dta("data/UKTUS/PW/epis_chap0015.dta") %>% filter(year==2015 ) %>% select(-'what_oth1')
  
  
  ep.chap15$pnum<-ep.chap15$pid-(ep.chap15$serial*100)

  #Adding extra diary variables
  ep.chap15<-merge(ep.chap15,
  		            read_dta("data/UKTUS/2015/UKDS/UKDA-8128-stata11_se/stata11_se/uktus15_diary_ep_long.dta")%>% 
  						   select(serial,pnum,daynum,epnum,What_Oth1,WithChild,WithMother,WithFather,WithSpouse),
  				by.x=c("serial","pnum","daynum","epn"),by.y=c("serial","pnum","daynum","epnum"))
  
  names(ep.chap15)<-tolower(names(ep.chap15))
  ## Valid enjoyment values
  ep.chap15$nenjoy<-ifelse(ep.chap15$enjoy>=1,ep.chap15$enjoy,NA)
  
  # Whether a rushed day
  ep.chap15$rush_d<-ifelse(ep.chap15$rushd_d=="Yes" | ep.chap15$rushd_d=="No",ep.chap15$rushd_d,NA)

  ### Whether respondent usually feels rushed
  ep.chap15$rush_p<-ifelse(ep.chap15$rush_p=="Yes" | ep.chap15$rush_p=="No",ep.chap15$rush_p,NA)

  ### Weekend vs weekday
  ep.chap15$wee<-ifelse(as_factor(ep.chap15$dow)=="Saturday" | as_factor(ep.chap15$dow)=="Sunday","Weekend","Weekday")
  
  

  #Flagging caring episodes and enjoyment type in the diary:
# Primary
# Secondary
# any (excl copresence)
# any (incl copresence)
# alone
# joint
# routine
# enriched
# (for kids: time with mothers)
# (for kids: time with fathers)
# (for kids: time with both)

  ep.chap15<-ep.chap15%>%mutate(slp.pri.d=ifelse(whatdoing==0 | whatdoing==110 | whatdoing==5310,eptime,0),
                                any.pri.d=ifelse(whatdoing!=0 & whatdoing!=110 & whatdoing!=5310 & !is.na(whatdoing),eptime,0),
                                car.pri.d=ifelse(whatdoing>=3800 & whatdoing<3900 & as_factor(withchild)=="Reported" & !(what_oth1>=3800 & what_oth1<3900),eptime,0), 
  							  car.sec.d=ifelse(what_oth1>=3800 & what_oth1<3900 & as_factor(withchild)=="Reported" & !(whatdoing>=3800 & whatdoing<3900),eptime,0),
                                car.copr.d=ifelse(as_factor(withchild)=="Reported" & car.pri.d==0 & car.sec.d==0,eptime,0),
  							  car.ann.d=ifelse(car.pri.d>0 | car.sec.d>0  ,eptime,0),
  							  car.anw.d=ifelse(car.pri.d>0 | car.sec.d>0 | car.copr.d >0 ,eptime,0),
  		                      car.aln.d=ifelse(((whatdoing>=3800 & whatdoing<3900) | (what_oth1>=3800 & what_oth1<3900) ) & as_factor(withspouse)=="Not reported" & as_factor(withchild)=="Reported",eptime,0),
  							  car.spo.d=ifelse(((whatdoing>=3800 & whatdoing<3900) | (what_oth1>=3800 & what_oth1<3900) ) & as_factor(withspouse)=="Reported" & as_factor(withchild)=="Reported",eptime,0),
  							  car.rou.d=ifelse(  (((whatdoing>=3800 & whatdoing<3820) | (whatdoing == 3840 | whatdoing == 3890)) | 
  												  ((what_oth1>=3800 & what_oth1<3820) | (what_oth1 == 3840 | what_oth1 == 3890))) &  as_factor(withchild)=="Reported",eptime,0),
  							  car.enr.d=ifelse  ( ( (whatdoing==3820 | whatdoing==3830) | (what_oth1==3820 | what_oth1==3830)) & as_factor(withchild)=="Reported",eptime,0),
  							  mum.d=ifelse(as_factor(withmother)=="Reported" & as_factor(withfather)=="Not reported"  & age<15,eptime,0),
  							  dad.d=ifelse(as_factor(withfather)=="Reported" & as_factor(withmother)=="Not reported" & age<15,eptime,0),
  							  both.d=ifelse(as_factor(withfather)=="Reported" & as_factor(withmother)=="Reported" & age<15,eptime,0),
  							  
  							  ###### Enjoyment from here
  							  enj.pri.d=ifelse(nenjoy>=1 & whatdoing>=3800 & whatdoing<3900 & as_factor(withchild)=="Reported" & !(what_oth1>=3800 & what_oth1<3900),nenjoy*car.pri.d,NA), 
  							  enj.sec.d=ifelse(nenjoy>=1 & what_oth1>=3800 & what_oth1<3900 & as_factor(withchild)=="Reported" & !(whatdoing>=3800 & whatdoing<3900),nenjoy*car.sec.d,NA),
  							  enj.copr.d=ifelse(nenjoy>=1 & as_factor(withchild)=="Reported" & car.pri.d==0 & car.sec.d==0,nenjoy*car.copr.d,NA),
  							  enj.ann.d=ifelse(nenjoy>=1 & car.pri.d>0 | car.sec.d>0  ,nenjoy,NA),
  							  enj.anw.d=ifelse(nenjoy>=1 & car.pri.d>0 | car.sec.d>0 | car.copr.d >0 ,nenjoy,NA),
  							  enj.aln.d=ifelse(nenjoy>=1 & ((whatdoing>=3800 & whatdoing<3900) | (what_oth1>=3800 & what_oth1<3900) ) & as_factor(withspouse)=="Not reported" & as_factor(withchild)=="Reported",nenjoy,NA),
  							  enj.spo.d=ifelse(nenjoy>=1 & ((whatdoing>=3800 & whatdoing<3900) | (what_oth1>=3800 & what_oth1<3900) ) & as_factor(withspouse)=="Reported" & as_factor(withchild)=="Reported",nenjoy,NA),
  							  enj.rou.d=ifelse(nenjoy>=1 & (      ((whatdoing>=3800 & whatdoing<3820) | (whatdoing == 3840 | whatdoing == 3890)) | 
  												  ((what_oth1>=3800 & what_oth1<3820) | (what_oth1 == 3840 | what_oth1 == 3890))) &  as_factor(withchild)=="Reported",nenjoy,NA),
  							  enj.enr.d=ifelse(nenjoy>=1 & ( (whatdoing==3820 | whatdoing==3830) | (what_oth1==3820 | what_oth1==3830)) & as_factor(withchild)=="Reported",nenjoy,NA),
  							  enj.mum.d=ifelse(mum.d>0 & nenjoy>=1 & as_factor(withmother)=="Reported" & as_factor(withfather)=="Not reported" & age<15,nenjoy,NA),
  							  enj.dad.d=ifelse(dad.d>0 & nenjoy>=1 & as_factor(withfather)=="Reported" & as_factor(withmother)=="Not reported" & age<15,nenjoy,NA),
  							  enj.both.d=ifelse(both.d>0 & nenjoy>=1 & as_factor(withfather)=="Reported" & as_factor(withmother)=="Reported" & age<15,nenjoy,NA),
  							  nenjoy.t=ifelse(nenjoy>=1,nenjoy*eptime,NA),
  							  )
  					  
  
  
 
# Daily amount of time for each caring type
# Daily mean enjoyment values by caring type

  ### Person level totals of time spent caring (adults)
  ep.chap15<-ep.chap15%>%group_by(serial,pnum,daynum)%>%mutate(
                              any.pri.t=1440-sum(slp.pri.d),
                              car.pri.t=sum(car.pri.d),
  		                    car.sec.t=sum(car.sec.d),
  		                    car.copr.t=sum(car.copr.d),
  		                    car.ann.t=sum(car.ann.d),
                              car.anw.t=sum(car.anw.d),
  		                    car.aln.t=sum(car.aln.d),
   							car.spo.t=sum(car.spo.d),
  							car.rou.t=sum(car.rou.d),
  							car.enr.t=sum(car.enr.d),
  							dad.t=sum(dad.d),
  							mum.t=sum(mum.d),
  							both.t=sum(both.d),

							##### Rate
  							car.pri.r=sum(car.pri.d)/any.pri.t,
  							car.sec.r=sum(car.sec.d)/any.pri.t,
  							car.copr.r=sum(car.copr.d)/any.pri.t,
  							car.ann.r=sum(car.ann.d)/any.pri.t,
  							car.anw.r=sum(car.anw.d)/any.pri.t,
  							car.aln.r=sum(car.aln.d)/any.pri.t,
  							car.spo.r=sum(car.spo.d)/any.pri.t,
  							car.rou.r=sum(car.rou.d)/any.pri.t,
  							car.enr.r=sum(car.enr.d)/any.pri.t,
  							dad.r=sum(dad.d)/any.pri.t,
  							mum.r=sum(mum.d)/any.pri.t,
  							both.r=sum(both.d)/any.pri.t,
  							##### Enjoyment
  							enj.pri.m=sum(enj.pri.d,na.rm=T)/car.pri.t,
  							enj.sec.m=sum(enj.sec.d,na.rm=T)/car.sec.t,
  							enj.copr.m=sum(enj.copr.d,na.rm=T)/car.copr.t,
  							enj.ann.m=sum(enj.ann.d*car.ann.d,na.rm=T)/car.ann.t,
  							enj.anw.m=sum(enj.anw.d*car.anw.d,na.rm=T)/car.anw.t,
  							enj.aln.m=sum(enj.aln.d*car.aln.d,na.rm=T)/car.aln.t,
  							enj.spo.m=sum(enj.spo.d*car.spo.d,na.rm=T)/car.spo.t,
  							enj.rou.m=sum(enj.rou.d*car.rou.d,na.rm=T)/car.rou.t,
  							enj.enr.m=sum(enj.enr.d*car.enr.d,na.rm=T)/car.enr.t,
  							enj.dad.m=ifelse(dad.t>0,sum(enj.dad.d*dad.d,na.rm=T)/dad.t,NA),
  							enj.mum.m=ifelse(mum.t>0,sum(enj.mum.d*mum.d,na.rm=T)/mum.t,NA),
  							enj.both.m=ifelse(both.t>0,sum(enj.both.d*both.d,na.rm=T)/both.t,NA),
  							nenjoy.d=sum(nenjoy.t)/1440,    ### Mean daily enjoyment (overall)
  							)%>% ungroup()
  
  
  
  
 
 ###  Saving temporary episodes and individuals datasets.
#write.csv(ep.chap15,"/home/piet/Dropbox/work/CTUR/papers/shared_care/data/geo_ep_tmp.csv")
#write.csv(ind.tus15,"/home/piet/Dropbox/work/CTUR/papers/shared_care/data/geo_ind_tmp.csv")

  
  #### Household-level relative time spent caring, by caring type
  #### For each household, we need to separately compute the total daily amount of time spent caring by caring type
  #### Then father and mother only,  then compute the ratio of both
  
  #### We merge household day-level datasets with caring totals from each parents
  #### We also need to compute enjoyment whilst caring, and proporion of cara
  ### We can relax some of the requirements form the GEO report
  ### We can have families with missing children data, but with children in the household
  
  #### Getting the data
  t.m<-ep.chap15%>%filter(as_factor(sex)=="Female" & epn==1 & age>16) %>%             #& (dow=="Saturday" | dow=="Sunday")) 
  		select(serial,pnum,daynum,wee,any.pri.t:both.t,nenjoy.d,enj.pri.m,enj.aln.m,enj.rou.m,enj.enr.m,rush_d,sex,dia_wt_a)
  
  t.m<-merge(t.m,ind.tus15%>%
               select(serial,pnum,agekidx,ccare.d,dvage,flexpt,Income,inpdw,nhiqual3,
                       nhiqual4,nsect,nanxious,nhappy,nsatbal,nsathlth,nsatpart,nsatsoc,nworth,nhappy.st,nsatbal.st, 
  				                  nsatis.st ,nsatpart.st, nsk4, npar,sectfl,slfl,wflex,wftpt),
  		   by=c("serial","pnum"),all.x=F,all.y=T)%>%distinct()	
  
  ## dad (any day)
  t.d<-ep.chap15%>%filter(as_factor(sex)=="Male" & epn==1 & age>16)%>% 
    select(serial,daynum,pnum,wee,any.pri.t:both.r,nenjoy.d,enj.pri.m,enj.aln.m,enj.rou.m,enj.enr.m,rush_d,sex,dia_wt_a)
  
  t.d<-merge(t.d,ind.tus15%>%select(serial,pnum,agekidx,ccare.d,dvage,flexpt,Income,inpdw,nhiqual3,
                                                   nhiqual4,nsect,nanxious,nhappy,nsatbal,nsathlth,nsatpart,nsatsoc,nworth,nhappy.st,nsatbal.st, 
                                                   nsatis.st ,nsatpart.st, nsk4, npar,sectfl,slfl,wflex,wftpt),
		  by=c("serial","pnum"),all.x=F,all.y=T)%>%distinct()	
  
     
  names(t.m)[4:ncol(t.m)]<-paste0(names(t.m)[4:ncol(t.m)],'.m')
  names(t.d)[4:ncol(t.d)]<-paste0(names(t.d)[4:ncol(t.d)],'.d')
  
  #### Household matched dataset, no observation of care removed
  hh.t<-droplevels(merge(t.d,t.m,,
				         by=c("serial","daynum"),
						 all.x=T,all.y=F))
		 
  
  hh.t<-hh.t%>%mutate(car.pri.rat=car.pri.t.d/(car.pri.t.m+car.pri.t.d),
                      car.sec.rat=car.sec.t.d/(car.pri.t.m+car.pri.t.d),
                      car.ann.rat=car.ann.t.d/(car.pri.t.m+car.pri.t.d),
                      car.anw.rat=car.anw.t.d/(car.anw.t.m+car.anw.t.d),
                      car.aln.rat=car.aln.t.d/(car.aln.t.m+car.aln.t.d),
                      car.spo.rat=car.spo.t.d/(car.pri.t.m+car.pri.t.d),
                      #car.rou.rat=car.rou.t.d/car.rou.t.m
  		            car.rou.rat=car.rou.t.d/(car.rou.t.m+car.rou.t.d),
  					car.enr.rat=car.enr.t.d/(car.enr.t.m+car.enr.t.d)
  					)
hh.t$car.pri.rat<-ifelse(hh.t$car.pri.rat>1,1,hh.t$car.pri.rat)
hh.t$car.rou.rat<-ifelse(hh.t$car.rou.rat>1,1,hh.t$car.rou.rat)
hh.t$car.enr.rat<-ifelse(hh.t$car.enr.rat>1,1,hh.t$car.enr.rat)
hh.t$car.aln.rat<-ifelse(hh.t$car.aln.rat>1,1,hh.t$car.aln.rat)

					
  			hh.t$car.pri.d.d<-ifelse(hh.t$car.pri.rat==0,"Not","Cares")
  			hh.t$car.pri.d.m<-ifelse(hh.t$car.pri.t.m==0,"Not","Cares")
  			
  			#### For regression: categorical versions of dad/mum caring rates
			
			### Dichotomic version
  
  hh.t<- hh.t %>% mutate(
  		car.pri.cat = case_when(car.pri.rat == 0 ~ " Not caring",car.pri.rat> 0 & car.pri.rat< .4 ~ '<40%',car.pri.rat>=.4 & car.pri.rat<1~'>=40%', car.pri.rat>=1  ~ "M don't care"),
  		car.sec.cat = case_when(car.sec.rat == 0 ~ " Not caring",car.sec.rat> 0 & car.sec.rat< .4 ~ '<40%',car.sec.rat>=.4 & car.sec.rat<1~'>=40%', car.sec.rat>=1   ~ "M don't care"),
  		car.ann.cat = case_when(car.ann.rat == 0 ~ " Not caring",car.ann.rat> 0 & car.ann.rat< .4 ~ '<40%',car.ann.rat>=.4 & car.ann.rat<1~'>=40%', car.ann.rat>=1   ~ "M don't care"),
  		car.anw.cat = case_when(car.anw.rat == 0 ~ " Not caring",car.anw.rat> 0 & car.anw.rat< .4 ~ '<40%',car.anw.rat>=.4 & car.anw.rat<1~'>=40%', car.anw.rat>=1   ~ "M don't care"),
  		car.aln.cat = case_when(car.aln.rat == 0 ~ " Not caring",car.aln.rat> 0 & car.aln.rat< .4 ~ '<40%',car.aln.rat>=.4 & car.aln.rat<1~'>=40%', car.aln.rat>=1   ~ "M don't care"),
  		car.spo.cat = case_when(car.spo.rat == 0 ~ " Not caring",car.spo.rat> 0 & car.spo.rat< .4 ~ '<40%',car.spo.rat>=.4 & car.spo.rat<1~'>=40%', car.spo.rat>=1   ~ "M don't care"),
  		car.rou.cat = case_when(car.rou.rat == 0 ~ " Not caring",car.rou.rat> 0 & car.rou.rat< .4 ~ '<40%',car.rou.rat>=.4 & car.rou.rat<1~'>=40%', car.rou.rat>=1  ~ "M don't care"),
  		car.enr.cat = case_when(car.enr.rat == 0 ~ " Not caring",car.enr.rat> 0 & car.enr.rat< .4 ~ '<40%',car.enr.rat>=.4 & car.enr.rat<1~'>=40%', car.enr.rat>=1   ~ "M don't care"),
  		car.pri.cat3 = case_when(car.pri.rat == 0 ~ " Not caring",car.pri.rat> 0 & car.pri.rat< .4 ~ '<40%',car.pri.rat>=.4~'>=40%'),
  		car.sec.cat3 = case_when(car.sec.rat == 0 ~ " Not caring",car.sec.rat> 0 & car.sec.rat< .4 ~ '<40%',car.sec.rat>=.4~'>=40%'),
  		car.ann.cat3 = case_when(car.ann.rat == 0 ~ " Not caring",car.ann.rat> 0 & car.ann.rat< .4 ~ '<40%',car.ann.rat>=.4~'>=40%'),
  		car.anw.cat3 = case_when(car.anw.rat == 0 ~ " Not caring",car.anw.rat> 0 & car.anw.rat< .4 ~ '<40%',car.anw.rat>=.4~'>=40%'),
  		car.aln.cat3 = case_when(car.aln.rat == 0 ~ " Not caring",car.aln.rat> 0 & car.aln.rat< .4 ~ '<40%',car.aln.rat>=.4~'>=40%'),
  		car.spo.cat3 = case_when(car.spo.rat == 0 ~ " Not caring",car.spo.rat> 0 & car.spo.rat< .4 ~ '<40%',car.spo.rat>=.4~'>=40%'),
  		car.rou.cat3 = case_when(car.rou.rat == 0 ~ " Not caring",car.rou.rat> 0 & car.rou.rat< .4 ~ '<40%',car.rou.rat>=.4~'>=40%'),
  		car.enr.cat3 = case_when(car.enr.rat == 0 ~ " Not caring",car.enr.rat> 0 & car.enr.rat< .4 ~ '<40%',car.enr.rat>=.4~'>=40%'),
		### Tercile and quartiles of childcare share
		car.cpr.q3=ntile(car.pri.rat,3),		car.cpr.q4=ntile(car.pri.rat,4),
		car.crr.q3=ntile(car.rou.rat,3),		car.crr.q4=ntile(car.rou.rat,4),
		car.cer.q3=ntile(car.enr.rat,3),		car.cer.q4=ntile(car.enr.rat,4),
		car.car.q3=ntile(car.aln.rat,3),		car.car.q4=ntile(car.aln.rat,4),
		### Tercile and quartiles of childcare time
		car.pri.q3.d=ntile(car.pri.t.d,3),	car.pri.q3.m=ntile(car.pri.t.m,3), car.pri.q4.d=ntile(car.pri.t.d,4),	car.pri.q4.m=ntile(car.pri.t.m,4),
		car.rou.q3.d=ntile(car.rou.t.d,3),	car.rou.q3.m=ntile(car.rou.t.m,3), car.rou.q4.d=ntile(car.rou.t.d,4),	car.rou.q4.m=ntile(car.rou.t.m,4),
		car.enr.q3.d=ntile(car.enr.t.d,3),	car.enr.q3.m=ntile(car.enr.t.m,3), car.enr.q4.d=ntile(car.enr.t.d,4),	car.enr.q4.m=ntile(car.enr.t.m,4),
		car.aln.q3.d=ntile(car.aln.t.d,3),	car.aln.q3.m=ntile(car.aln.t.m,3), car.aln.q4.d=ntile(car.aln.t.d,4),	car.aln.q4.m=ntile(car.aln.t.m,4),		
)

  
						
						
						
  						#### Household-level parental working-time configurations
  hh.t<-hh.t%>% mutate(hhftpt= as.factor(paste(wftpt.d, wftpt.m,sep=" ")))
  hh.t$hhftpt<-as.character(hh.t$hhftpt)
  hh.t$hhftpt<-ifelse(hh.t$hhftpt=="Full-time Full-time" | hh.t$hhftpt=="Full-time Not in paid work" | hh.t$hhftpt=="Full-time Part-time",hh.t$hhftpt,"Other")
  hh.t$hhftpt<-as.factor(hh.t$hhftpt)
  levels(hh.t$hhftpt)<-c("D-FT+M-FT","D-FT+M-NIPW","D-FT+M-PT","Other")
  
  ##### FINAL POPULATION SELECTION (Households with two parents in paid work and no missing information on primary and secondary childcare 
  hh.t2<-hh.t
#  hh.t<-droplevels(hh.t%>%filter(inpdw.m=="In paid work" & inpdw.d=="In paid work" & !is.na(car.ann.cat3) & !is.na(nhiqual3.d) ))
 hh.t<-droplevels(hh.t%>%filter(!is.na(car.ann.cat3) ))
  
  



