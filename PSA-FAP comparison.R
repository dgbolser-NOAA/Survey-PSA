#########################################################################################################################################
#### compare PSA ranks with fleet allocation plan
####
#########################################################################################################################################

### script not operational; code snippets taken from another script. Revisit
#### include FAP information and examine surveys that were excluded and look at logistics ##################################################################
#### update with new FAP ###################################################################################################################################
FAP<-read.csv("FY24 NMFS draft FAP v3.csv")
FINSS<- read.csv("FY23 FINSS - VPASS Name Match.csv")
FINSS24<- read.csv("FY24 FINSS - VPASS Name Match.csv")

#tidy up the FAP df
FAP<-FAP[1:63,1:7]
colnames(FAP)<- c('final_rank', 'region', 'vpass_name', 'center_rank', 'DAS_requested', 'ship', 'DAS_allocated')

#tidy and match names with FINSS names
FINSS<-FINSS[,3:4]
colnames(FINSS)<- c('survey', 'vpass_name')

FINSS24<- FINSS24[,4:5]
colnames(FINSS24)<- c('survey', 'vpass_name')

FINSS<-full_join(FINSS,FINSS24, by = c('survey','vpass_name'))

#fix the obvious ones
FINSS$vpass_name[FINSS$survey == "Bottom Trawl Survey_Fall"]<-" NEFSC Bottom Trawl Survey Autumn - October/November component"

#### match the names for merging with survey scores; still many that are unmatched. Might have to have several VPASS name

#merge fap data with survey grouped scores; problem here ##############################################################################################################
fap_surveys<- full_join(survey_grouped_scores, FAP, by = 'survey')

#identify which surveys still need to be manually entered
vpass_na<- fap_surveys[is.na(fap_surveys$vpass_name),]
survey_na<- fap_surveys[is.na(fap_surveys$survey),]

#Just look at it by FAP for now. 
FAP<- left_join(FAP,FINSS,by = "vpass_name")

#fix some missing names
FAP[2,8]<- "Bottom Trawl Survey_Fall"
FAP[15,8]<- "Bottom Trawl Survey_Fall"
FAP[57,8]<- "Reef Fish Visual Census Survey - U.S. Caribbean"
FAP[38,8]<-"Gulf of Alaska/Shelikof Walleye Pollock Acoustic Trawl Survey_Winter"

#fix some names that are there
FAP[7,8]<- "Coastal Pelagic Species (CPS) Survey (aka Sardine Survey)"
FAP[3,8]<-"Hake Acoustic_Summer"
FAP[14,8]<-"Gulf of Alaska/Shelikof Walleye Pollock Acoustic Trawl Survey_Winter" #might be wrong
FAP[32,8]<-"Northeast Ecosystem Monitoring (EcoMon)_Summer" #did they change season names?
FAP[34,8]<-"SEAMAP-GOM Plankton (ADCNR)" #wrong placeholder
FAP[37,8]<-"Eastern Bering Sea Groundfish Bottom Trawl" #probably wrong placeholder
FAP[48,8]<-"Northeast Ecosystem Monitoring (EcoMon)_Fall" #did they change season names?

FAP[29,8]<-"Bigeye tuna - Western and Central Pacific" #joining with species below
FAP[52,8]<-"Bigeye tuna - Western and Central Pacific" #joining with species below
FAP[46,8]<-"Bluefin tuna - Western Atlantic" #joining with species below

#remaining clear fish surveys are bluefin and bigeye tuna; can deal with those after merging
fap_comp<-left_join(FAP, survey_grouped_scores, by = "survey")

#get the tuna scores
tuna<-scores[scores$stock_name == "Bigeye tuna - Western and Central Pacific"| scores$stock_name == "Bluefin tuna - Western Atlantic",]

tuna<-tuna[,c(1,17,19,20)]

colnames(tuna)<-c("survey","p_score","s_score","score")
tuna$n<-1
tuna$spp_number<-"1"

#janky workaround
fap_comp<-full_join(fap_comp,tuna, by = c("survey","p_score","s_score","score","n","spp_number"))
fap_comp[c(29,52),9:13]<-fap_comp[65,9:13]
fap_comp[46,9:13]<-fap_comp[64,9:13]                    

fap_comp<-fap_comp[1:63,]

#rank the surveys by their scores
fap_comp$PSA_rank<-rank(fap_comp$score, na.last = "keep")

fap_comp<-fap_comp%>%
  group_by(region)%>%
  mutate(PSA_center_rank = rank(score, na.last = "keep"))%>%
  ungroup()

#fix some things before comparing ranks
fap_comp$center_rank<-as.numeric(fap_comp$center_rank)

fap_comp$final_rank[fap_comp$final_rank == 999]<-NA

fap_comp$delta_rank<- fap_comp$final_rank - fap_comp$PSA_rank
fap_comp$delta_center_rank<- fap_comp$center_rank - fap_comp$PSA_center_rank

#write.csv(fap_comp,"FAP_PSA_rank_comparison.csv", row.names = F)
