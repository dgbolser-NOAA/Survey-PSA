####################################################################################################################################################
#### manipulate gap analysis table and CVA data
####
####################################################################################################################################################

####clear environment
rm(list=ls())

#load packages
library(FishLife)
library(tidyverse)
library(rfishbase)

#set wds
data<- "C:/Users/Derek.Bolser/Documents/Survey-PSA/Data"
setwd(data)

#read in data
gap<- read.csv("gap_analysis_assessment_table.csv")
cva<-read.csv("cva_data.csv")
ssm<- read.csv("Assessment_Summary_Data_stock_smart_2024.csv")
status<-read.csv("SIS_stock_status.csv")

#### trim columns
ssm<-ssm[,c(2:6,8:10,19:23,25:30,39:41,52,53)]

#### populate dfs with species from complexes #################################################################################################
ssm_c<- ssm[ssm$Scientific.Name == "",]
ssm_c <- ssm_c %>% arrange(Stock.Name)

#Aleutian Islands Blackspotted and Rougheye Rockfish Complex (2 spp)
ssm_c[33,]<-ssm_c[1,]

ssm_c[1,6]<-"Sebastes aleutianus"
ssm_c[1,7]<-"Rougheye rockfish"

ssm_c[33,6]<-"Sebastes melanostictus"
ssm_c[33,7]<-"Blackspotted rockfish"

#American Samoa Bottomfish Multi-species Complex (11 spp)
ssm_c[34:43,]<-ssm_c[2,]

ssm_c[2,6]<-"Aphareus rutilans"
ssm_c[2,7]<-"Rusty jobfish"

ssm_c[34,6]<-"Aprion virescens"
ssm_c[34,7]<-"Green jobfish"

ssm_c[35,6]<-"Caranx lugubris"
ssm_c[35,7]<-"Black trevally"

ssm_c[36,6]<-"Etelis carbunculus"
ssm_c[36,7]<-"Ruby snapper"

ssm_c[37,6]<-"Etelis coruscans"
ssm_c[37,7]<-"Flame snapper"

ssm_c[38,6]<-"Lethrinus rubrioperculatus"
ssm_c[38,7]<-"Redgill emperor"

ssm_c[39,6]<-"Lutjanus kasmira"
ssm_c[39,7]<-"Bluestripe snapper"

ssm_c[40,6]<-"Pristipomoides flavipinnis"
ssm_c[40,7]<-"Yelloweye snapper"

ssm_c[41,6]<-"Pristipomoides zonatus"
ssm_c[41,7]<-"Oblique-banded snapper"

ssm_c[42,6]<-"Pristipomoides filamentosus"
ssm_c[42,7]<-"Pink snapper"

ssm_c[43,6]<-"Variola louti"
ssm_c[43,7]<-"Lyretail grouper"

#Atlantic small coastal sharks complex (4 spp) 
ssm_c[44:46,]<-ssm_c[3,]

ssm_c[3,6]<-"Carcharhinus acronotus"
ssm_c[3,7]<-"Blacknose shark"

ssm_c[44,6]<-"Rhizoprionodon terraenovae"
ssm_c[44,7]<-"Atlantic sharpnose shark"

ssm_c[45,6]<-"Sphyrna tiburo"
ssm_c[45,7]<-"Bonnethead shark"

ssm_c[46,6]<-"Carcharhinus isodon"
ssm_c[46,7]<-"Finetooth shark"

#Bering Sea / Aleutian Islands Blackspotted and Rougheye Rockfish Complex
ssm_c[47,]<-ssm_c[4,]

ssm_c[4,6]<-"Sebastes aleutianus"
ssm_c[4,7]<-"Rougheye rockfish"

ssm_c[47,6]<-"Sebastes melanostictus"
ssm_c[47,7]<-"Blackspotted rockfish"

#Bering Sea / Aleutian Islands Other Flatfish Complex
ssm_c[48:61,]<-ssm_c[5,]

ssm_c[5,6]<-"Liopsetta glacialis"
ssm_c[5,7]<-"Arctic flounder"

ssm_c[48,6]<-"Isopsetta isolepis"
ssm_c[48,7]<-"Butter sole"

ssm_c[49,6]<-"Pleuronectes decurrens"
ssm_c[49,7]<-"Curlfin sole"

ssm_c[50,6]<-"Embassichthys bathybius"
ssm_c[50,7]<-"Deepsea sole"

ssm_c[51,6]<-"Microstomus pacificus"
ssm_c[51,7]<-"Dover sole"

ssm_c[52,6]<-"Parophrys vetulus"
ssm_c[52,7]<-"English sole"

ssm_c[53,6]<-"Limanda proboscidea"
ssm_c[53,7]<-"Longhead dab"

ssm_c[54,6]<-"Citharichthys sordidus"
ssm_c[54,7]<-"Pacific sanddab"

ssm_c[55,6]<-"Eopsetta jordani" #still in the complex?
ssm_c[55,7]<-"Petrale sole"

ssm_c[56,6]<-"Glyptocephalus zachirus"
ssm_c[56,7]<-"Rex sole"

ssm_c[57,6]<-"Clidoderma asperrimum"
ssm_c[57,7]<-"Roughscale sole"

ssm_c[58,6]<-"Psettichthys melanostictus"
ssm_c[58,7]<-"Sand sole"

ssm_c[59,6]<-"Lyopsetta exilis"
ssm_c[59,7]<-"Slender sole"

ssm_c[60,6]<-"Platichthys stellatus"
ssm_c[60,7]<-"Starry flounder"

ssm_c[61,6]<-"Limanda sakhalinensis"
ssm_c[61,7]<-"Sakhalin sole"

#Bering Sea / Aleutian Islands Other Rockfish Complex; primarily shortspine thornyhead (95%); the remaining 5% dominated by dusky rockfish
ssm_c[62,]<-ssm_c[6,]

ssm_c[6,6]<-"Sebastolobus alascanus"
ssm_c[6,7]<-"Shortspine thornyhead"

ssm_c[62,6]<-"Sebastes variabilis"
ssm_c[62,7]<-"Dusky rockfish"

#Bering Sea / Aleutian Islands Other Skates Complex; appears to be an updated name for the assessment of the skate complex.
ssm_c[63:77,]<-ssm_c[7,]

ssm_c[7,6]<-"Bathyraja parmifera"
ssm_c[7,7]<-"Alaska skate"

ssm_c[63,6]<-"Bathyraja aleutica"
ssm_c[63,7]<-"Aleutian skate"

ssm_c[64,6]<-"Beringraja binoculata"
ssm_c[64,7]<-"Big skate"

ssm_c[65,6]<-"Bathyraja interrupta"
ssm_c[65,7]<-"Bering skate"

ssm_c[66,6]<-"Bathyraja maculata"
ssm_c[66,7]<-"Whiteblotched skate"

ssm_c[67,6]<-"Bathyraja taranetzi"
ssm_c[67,7]<-"Mud skate"

ssm_c[68,6]<-"Beringraja rhina" #reclassified to Caliraja but are databases updated?
ssm_c[68,7]<-"Longnose skate"

ssm_c[69,6]<-"Bathyraja lindbergi"
ssm_c[69,7]<-"Commander skate"

ssm_c[70,6]<-"Bathyraja mariposa"
ssm_c[70,7]<-"Butterfly skate"

ssm_c[71,6]<-"Bathyraja minispinosa"
ssm_c[71,7]<-"whitebrow skate"

ssm_c[72,6]<-"Bathyraja trachura"
ssm_c[72,7]<-"Roughtail skate"

ssm_c[73,6]<-"Bathyraja violacea"
ssm_c[73,7]<-"Okhotsk skate"

ssm_c[74,6]<-"Amblyraja badia"
ssm_c[74,7]<-"Roughshoulder skate"

ssm_c[75,6]<-"Bathyraja abyssicola"
ssm_c[75,7]<-"Deepsea skate"

ssm_c[76,6]<-"Bathyraja panthera"
ssm_c[76,7]<-"Leopard skate"

ssm_c[77,6]<-"Bathyraja spinosissima"
ssm_c[77,7]<-"Pacific white skate"

#Bering Sea / Aleutian Islands Sculpin Complex; 26 spp
ssm_c[78:102,]<-ssm_c[8,]

ssm_c[8,6]<-"Hemitripterus bolini"
ssm_c[8,7]<-"Bigmouth sculpin"

ssm_c[78,6]<-"Myoxocephalus polyacanthocephalus"
ssm_c[78,7]<-"Great sculpin"

ssm_c[79,6]<-"Myoxocephalus jaok"
ssm_c[79,7]<-"Plain sculpin"

ssm_c[80,6]<-"Myoxocephalus scorpius"
ssm_c[80,7]<-"Warty sculpin"

ssm_c[81,6]<-"Hemilepidotus jordani"
ssm_c[81,7]<-"Yellow irish lord"

ssm_c[82,6]<-"Enophrys diceraus"
ssm_c[82,7]<-"Antlered sculpin"

ssm_c[83,6]<-"Gymnocanthus tricuspis"
ssm_c[83,7]<-"Arctic staghorn sculpin"

ssm_c[84,6]<-"Gymnocanthus galeatus"
ssm_c[84,7]<-"Armorhead sculpin"

ssm_c[85,6]<-"Malacocottus kincaidi"
ssm_c[85,7]<-"Blackfin sculpin"

ssm_c[86,6]<-"Hemilepidotus papilio"
ssm_c[86,7]<-"Butterfly sculpin"

ssm_c[87,6]<-"Blepsias bilobus"
ssm_c[87,7]<-"Crested sculpin"

ssm_c[88,6]<-"Malacocottus zonurus"
ssm_c[88,7]<-"Darkfin sculpin"

ssm_c[89,6]<-"Nautichthys pribilovius"
ssm_c[89,7]<-"Eyeshade sculpin"

ssm_c[90,6]<-"Triglops metopias"
ssm_c[90,7]<-"Highbrow sculpin"

ssm_c[91,6]<-"Artediellus pacificus"
ssm_c[91,7]<-"Hookhorn sculpin"

ssm_c[92,6]<-"Gymnocanthus detrisus"
ssm_c[92,7]<-"Purplegray sculpin"

ssm_c[93,6]<-"Hemilepidotus hemilepidotus"
ssm_c[93,7]<-"Red irish lord"

ssm_c[94,6]<-"Triglops pingelii"
ssm_c[94,7]<-"Ribbed sclupin"

ssm_c[95,6]<-"Trachidermus fasciatus"
ssm_c[95,7]<-"Roughspine sclupin" #actual common name is roughskin; no results for roughspine

ssm_c[96,6]<-"Nautichthys oculofasciatus"
ssm_c[96,7]<-"Sailfin sclupin"

ssm_c[97,6]<-"Triglops forficatus"
ssm_c[97,7]<-"Scissortail sclupin"

ssm_c[98,6]<-"Icelus spatula"
ssm_c[98,7]<-"Spatulate sclupin"

ssm_c[99,6]<-"Triglops scepticus"
ssm_c[99,7]<-"Spectacled sclupin"

ssm_c[100,6]<-"Dasycottus setiger"
ssm_c[100,7]<-"Spinyhead sclupin"

ssm_c[101,6]<-"Icelus spiniger"
ssm_c[101,7]<-"Thorny sclupin"

ssm_c[102,6]<-"Gymnocanthus pistilliger"
ssm_c[102,7]<-"Threaded sclupin"

#Bering Sea / Aleutian Islands Shark Complex
ssm_c[103:104,]<-ssm_c[9,]

ssm_c[9,6]<-"Squalus suckleyi"
ssm_c[9,7]<-"Pacific spiny dogfish"

ssm_c[103,6]<-"Somniosus pacificus"
ssm_c[103,7]<-"Pacific sleeper shark"

ssm_c[104,6]<-"Lamna ditropis"
ssm_c[104,7]<-"Salmon shark"

#10; the BS/AI skates complex appears deprecated (last assessed in 2007), now spp are in 'other skates' (assessed in 2023)
#11; exclude squid

#Black grouper - Southern Atlantic Coast / Gulf of Mexico
ssm_c[12,6]<-"Mycteroperca bonaci"
ssm_c[12,7]<-"Black grouper"

#California Blue and Deacon rockfish Complex
ssm_c[105,]<-ssm_c[13,]

ssm_c[13,6]<-"Sebastes mystinus"
ssm_c[13,7]<-"Blue rockfish"

ssm_c[105,6]<-"Sebastes diaconus"
ssm_c[105,7]<-"Deacon rockfish"

#caribbean parrotfishes complex; no stock assessment online, only data workshop report. including complexes 1 and 2
ssm_c[106:114,]<-ssm_c[14,]

ssm_c[14,6]<-"Scarus coeruleus"
ssm_c[14,7]<-"Blue parrotfish"

ssm_c[106,6]<-"Scarus coelestinus"
ssm_c[106,7]<-"Midnight parrotfish"

ssm_c[107,6]<-"Scarus guacamaia"
ssm_c[107,7]<-"Rainbow parrotfish"

ssm_c[108,6]<-"Scarus taeniopterus"
ssm_c[108,7]<-"Princess parrotfish"

ssm_c[109,6]<-"Scarus vetula"
ssm_c[109,7]<-"Queen parrotfish"

ssm_c[110,6]<-"Sparisoma aurofrenatum"
ssm_c[110,7]<-"Redband parrotfish"

ssm_c[111,6]<-"Sparisoma rubripinne"
ssm_c[111,7]<-"Redfin parrotfish"

ssm_c[112,6]<-"Sparisoma chrysopterum"
ssm_c[112,7]<-"Redtail parrotfish"

ssm_c[113,6]<-"Sparisoma viride"
ssm_c[113,7]<-"Stoplight parrotfish"

ssm_c[114,6]<-"Scarus iseri"
ssm_c[114,7]<-"Striped parrotfish"

#Eastern Bering Sea Blackspotted and Rougheye Rockfish Complex
ssm_c[115,]<-ssm_c[15,]

ssm_c[15,6]<-"Sebastes aleutianus"
ssm_c[15,7]<-"Rougheye rockfish"

ssm_c[115,6]<-"Sebastes melanostictus"
ssm_c[115,7]<-"Blackspotted rockfish"

#Guam Bottomfish Multi-species Complex (13 spp)
ssm_c[116:127,]<-ssm_c[16,]

ssm_c[16,6]<-"Aphareus rutilans"
ssm_c[16,7]<-"Rusty jobfish"

ssm_c[116,6]<-"Caranx lugubris"
ssm_c[116,7]<-"Black trevally"

ssm_c[117,6]<-"Caranx ignobilis"
ssm_c[117,7]<-"Giant trevally"

ssm_c[118,6]<-"Etelis carbunculus"
ssm_c[118,7]<-"Ruby snapper"

ssm_c[119,6]<-"Etelis coruscans"
ssm_c[119,7]<-"Flame snapper"

ssm_c[120,6]<-"Lethrinus rubrioperculatus"
ssm_c[120,7]<-"Redgill emperor"

ssm_c[121,6]<-"Lutjanus kasmira"
ssm_c[121,7]<-"Bluestripe snapper"

ssm_c[122,6]<-"Pristipomoides flavipinnis"
ssm_c[122,7]<-"Yelloweye snapper"

ssm_c[123,6]<-"Pristipomoides sieboldii"
ssm_c[123,7]<-"Von Siebold's snapper"

ssm_c[124,6]<-"Pristipomoides zonatus"
ssm_c[124,7]<-"Oblique-banded snapper"

ssm_c[125,6]<-"Pristipomoides auricilla"
ssm_c[125,7]<-"Goldflag snapper"

ssm_c[126,6]<-"Pristipomoides filamentosus"
ssm_c[126,7]<-"Pink snapper"

ssm_c[127,6]<-"Variola louti"
ssm_c[127,7]<-"Lyretail grouper"

#Gulf smoothhound
ssm_c[128:129,]<-ssm_c[17,]

ssm_c[17,6]<-"Mustelus canis"
ssm_c[17,7]<-"Smooth dogfish"

ssm_c[128,6]<-"Mustelus norrisi"
ssm_c[128,7]<-"Florida smoothhound"

ssm_c[129,6]<-"Mustelus sinusmexicanus"
ssm_c[129,7]<-"Gulf smoothhound"

#	Gulf of Alaska Blackspotted and Rougheye Rockfish Complex
ssm_c[130,]<-ssm_c[18,]

ssm_c[18,6]<-"Sebastes aleutianus"
ssm_c[18,7]<-"Rougheye rockfish"

ssm_c[130,6]<-"Sebastes melanostictus"
ssm_c[130,7]<-"Blackspotted rockfish"

#GOA other rockfish; 27 spp
ssm_c[131:156,]<-ssm_c[19,]

ssm_c[19,6]<-"Sebastes aurora"
ssm_c[19,7]<-"Aurora rockfish"

ssm_c[131,6]<-"Sebastes melanostomus"
ssm_c[131,7]<-"Blackgill rockfish"

ssm_c[132,6]<-"Sebastes paucispinis"
ssm_c[132,7]<-"Bocaccio"

ssm_c[133,6]<-"Sebastes goodei"
ssm_c[133,7]<-"Chilipepper"

ssm_c[134,6]<-"Sebastes crameri"
ssm_c[134,7]<-"Darkblotched rockfish"

ssm_c[135,6]<-"Sebastes elongatus"
ssm_c[135,7]<-"Greenstriped rockfish"

ssm_c[136,6]<-"Sebastes variegatus"
ssm_c[136,7]<-"Harlequin rockfish"

ssm_c[137,6]<-"Sebastes polyspinis"
ssm_c[137,7]<-"Northern rockfish"

ssm_c[138,6]<-"Sebastes wilsoni"
ssm_c[138,7]<-"Pygmy rockfish"

ssm_c[139,6]<-"Sebastes babcocki"
ssm_c[139,7]<-"Redbanded rockfish"

ssm_c[140,6]<-"Sebastes proriger"
ssm_c[140,7]<-"Redstripe rockfish"

ssm_c[141,6]<-"Sebastes zacentrus"
ssm_c[141,7]<-"Sharpchin rockfish"

ssm_c[142,6]<-"Sebastes jordani"
ssm_c[142,7]<-"Shortbelly rockfish"

ssm_c[143,6]<-"Sebastes brevispinis"
ssm_c[143,7]<-"Silvergray rockfish"

ssm_c[144,6]<-"Sebastes diploproa"
ssm_c[144,7]<-"Splitnose rockfish"

ssm_c[145,6]<-"Sebastes saxicola"
ssm_c[145,7]<-"Stripetail rockfish"

ssm_c[146,6]<-"Sebastes miniatus"
ssm_c[146,7]<-"Vermilion rockfish"

ssm_c[147,6]<-"Sebastes entomelas"
ssm_c[147,7]<-"Widow rockfish"

ssm_c[148,6]<-"Sebastes reedi"
ssm_c[148,7]<-"Yellowmouth rockfish"

ssm_c[149,6]<-"Sebastes flavidus"
ssm_c[149,7]<-"Yellowtail rockfish"

ssm_c[150,6]<-"Sebastes pinniger"
ssm_c[150,7]<-"Canary rockfish"

ssm_c[151,6]<-"Sebastes nebulosus"
ssm_c[151,7]<-"China rockfish"

ssm_c[152,6]<-"Sebastes caurinus"
ssm_c[152,7]<-"Copper rockfish"

ssm_c[153,6]<-"Sebastes maliger"
ssm_c[153,7]<-"Quillback rockfish"

ssm_c[154,6]<-"Sebastes helvomaculatus"
ssm_c[154,7]<-"Rosethorn rockfish"

ssm_c[155,6]<-"Sebastes nigrocinctus"
ssm_c[155,7]<-"Tiger rockfish"

ssm_c[156,6]<-"Sebastes ruberrimus"
ssm_c[156,7]<-"Yelloweye rockfish"

#20; Gulf of Alaska Other Shallow Water Flatfish Complex; no stock assessment

#Gulf of alaska sclupin complex; most abundant are listed. 
ssm_c[157:159,]<-ssm_c[21,]

ssm_c[21,6]<-"Hemilepidotus jordani"
ssm_c[21,7]<-"Yellow irish lord"

ssm_c[157,6]<-"Myoxocephalus polyacanthocephalus"
ssm_c[157,7]<-"Great sculpin"

ssm_c[158,6]<-"Hemitripterus bolini"
ssm_c[158,7]<-"Bigmouth sculpin"

ssm_c[159,6]<-"Myoxocephalus jaok"
ssm_c[159,7]<-"Plain sculpin"

#Gulf of Alaska Shallow Water Flatfish Complex
ssm_c[160:166,]<-ssm_c[22,]

ssm_c[22,6]<-"Lepidopsetta polyxystra"
ssm_c[22,7]<-"Northern rock sole"

ssm_c[160,6]<-"Lepidopsetta bilineata"
ssm_c[160,7]<-"Southern rock sole"

ssm_c[161,6]<-"Limanda aspera"
ssm_c[161,7]<-"Yellowfin sole"

ssm_c[162,6]<-"Isopsetta isolepis"
ssm_c[162,7]<-"Butter sole"

ssm_c[163,6]<-"Platichthys stellatus"
ssm_c[163,7]<-"Starry flounder"

ssm_c[164,6]<-"Parophrys vetulus"
ssm_c[164,7]<-"English sole"

ssm_c[165,6]<-"Psettichthys melanostictus"
ssm_c[165,7]<-"Sand sole"

ssm_c[166,6]<-"Pleuronectes quadrituberculatus"
ssm_c[166,7]<-"Alaska plaice"

#Gulf of Alaska shark complex
ssm_c[167:168,]<-ssm_c[23,]

ssm_c[23,6]<-"Squalus suckleyi"
ssm_c[23,7]<-"Pacific spiny dogfish" 

ssm_c[167,6]<-"Somniosus pacificus"
ssm_c[167,7]<-"Pacific sleeper shark"

ssm_c[168,6]<-"Lamna ditropis"
ssm_c[168,7]<-"Salmon shark"

#Gulf of Alaska Skate complex
ssm_c[169:173,]<-ssm_c[24,]

ssm_c[24,6]<-"Bathyraja parmifera"
ssm_c[24,7]<-"Alaska skate"

ssm_c[169,6]<-"Bathyraja aleutica"
ssm_c[169,7]<-"Aleutian skate"

ssm_c[170,6]<-"Beringraja binoculata"
ssm_c[170,7]<-"Big skate"

ssm_c[171,6]<-"Bathyraja interrupta"
ssm_c[171,7]<-"Bering skate"

ssm_c[172,6]<-"Raja rhina"
ssm_c[172,7]<-"Longnose skate"

ssm_c[173,6]<-"Amblyraja badia"
ssm_c[173,7]<-"Roughshoulder skate"

#25; ignore GOA squid for now

#Main Hawaiian Islands Deep 7 Bottomfish Multi-species Complex
ssm_c[174:179,]<-ssm_c[26,]

ssm_c[26,6]<-"Etelis carbunculus"
ssm_c[26,7]<-"Squirrelfish snapper"

ssm_c[174,6]<-"Pristipomoides zonatus"
ssm_c[174,7]<-"Brigham's snapper"

ssm_c[175,6]<-"Hyporthodus quernus"
ssm_c[175,7]<-"Hawaiian grouper"

ssm_c[176,6]<- "Pristipomoides sieboldii"
ssm_c[176,7]<- "Von Siebold's snapper"

ssm_c[177,6]<- "Aphareus rutilans"
ssm_c[177,7]<- "Silverjaw snapper"

ssm_c[178,6]<-"Etelis coruscans"
ssm_c[178,7]<-"Longtail snapper"

ssm_c[179,6]<-"Pristipomoides filamentosus"
ssm_c[179,7]<-"Pink snapper"

#Northern California Gopher / Black-and-Yellow Rockfish Complex
ssm_c[180,]<-ssm_c[27,]

ssm_c[27,6]<-"Sebastes carnatus"
ssm_c[27,7]<-"Gopher rockfish"

ssm_c[180,6]<-"Sebastes chrysomelas"
ssm_c[180,7]<-"Black-and-yellow rockfish"

#Northern Mariana Islands Bottomfish Multi-species Complex (13 spp)
ssm_c[181:192,]<-ssm_c[28,]

ssm_c[28,6]<-"Aphareus rutilans"
ssm_c[28,7]<-"Rusty jobfish"

ssm_c[181,6]<-"Caranx lugubris"
ssm_c[181,7]<-"Black trevally"

ssm_c[182,6]<-"Caranx ignobilis"
ssm_c[182,7]<-"Giant trevally"

ssm_c[183,6]<-"Etelis carbunculus"
ssm_c[183,7]<-"Ruby snapper"

ssm_c[184,6]<-"Etelis coruscans"
ssm_c[184,7]<-"Flame snapper"

ssm_c[185,6]<-"Lethrinus rubrioperculatus"
ssm_c[185,7]<-"Redgill emperor"

ssm_c[186,6]<-"Lutjanus kasmira"
ssm_c[186,7]<-"Bluestripe snapper"

ssm_c[187,6]<-"Pristipomoides flavipinnis"
ssm_c[187,7]<-"Yelloweye snapper"

ssm_c[188,6]<-"Pristipomoides sieboldii"
ssm_c[188,7]<-"Von Siebold's snapper"

ssm_c[189,6]<-"Pristipomoides zonatus"
ssm_c[189,7]<-"Oblique-banded snapper"

ssm_c[190,6]<-"Pristipomoides auricilla"
ssm_c[190,7]<-"Goldflag snapper"

ssm_c[191,6]<-"Pristipomoides filamentosus"
ssm_c[191,7]<-"Pink snapper"

ssm_c[192,6]<-"Variola louti"
ssm_c[192,7]<-"Lyretail grouper"

#Oregon Blue and Deacon Rockfish Complex
ssm_c[193,]<-ssm_c[29,]

ssm_c[29,6]<-"Sebastes mystinus"
ssm_c[29,7]<-"Blue rockfish"

ssm_c[193,6]<-"Sebastes diaconus"
ssm_c[193,7]<-"Deacon rockfish"

#Pacific Coast Blackspotted and Rougheye Rockfish Complex
ssm_c[194,]<-ssm_c[30,]

ssm_c[30,6]<-"Sebastes melanostictus"
ssm_c[30,7]<-"Blackspotted rockfish"

ssm_c[194,6]<-"Sebastes aleutianus"
ssm_c[194,7]<-"Rougheye rockfish"

#Vermilion and Sunset rockfish Complex - Northern California
ssm_c[195,]<-ssm_c[31,]

ssm_c[31,6]<-"Sebastes miniatus"
ssm_c[31,7]<-"Vermilion rockfish"

ssm_c[195,6]<-"Sebastes crocotulus"
ssm_c[195,7]<-"Sunset rockfish"

#Vermilion and Sunset Rockfish Complex - Southern California
ssm_c[196,]<-ssm_c[32,]

ssm_c[32,6]<-"Sebastes miniatus"
ssm_c[32,7]<-"Vermilion rockfish"

ssm_c[196,6]<-"Sebastes crocotulus"
ssm_c[196,7]<-"Sunset rockfish"

#### include species from complexes in sis_spp
sis_c<- ssm_c[,6:7]
sis_c<-sis_c[!sis_c$Scientific.Name == "",]
colnames(sis_c)<-c("scientific_name","common_name")

sis_spp<-rbind(sis_spp,sis_c)
sis_spp<-unique(sis_spp)

#### add complex spp to ssm
ssm<-rbind(ssm,ssm_c)

##fix stuff ############################################################################################################################################
ssm$Scientific.Name<- gsub(" "," _ ", ssm$Scientific.Name)

#first, seperate genus and species to make this code work
ssm<-separate(ssm, col = Scientific.Name, sep = "_", into = c("Genus", "Species"))

#Arrowtooth flounder reclassified into a new genus; spp name is stomias. Other flounders too?
ssm$Genus[ssm$Genus == 'Reinhardtius '] <- 'Atheresthes ' #hippoglossoides is still the OG genus
ssm$Genus[ssm$Species == ' hippoglossoides']<-"Reinhardtius " #this one still has the Reinharditius genus

#fix sole genera
ssm$Genus[ssm$Genus == 'Pleuronectes '] <- 'Pleuronichthys '
ssm$Genus[ssm$Species == ' quadrituberculatus']<-'Pleuronectes ' #Alaska plaice still has the og genus

#fix grouper genera
ssm$Genus[ssm$Species == ' niveatus']<-"Hyporthodus "
ssm$Genus[ssm$Species == ' nigritus']<-"Hyporthodus "
ssm$Genus[ssm$Species == ' flavolimbatus']<-"Hyporthodus "

#fix big skate
ssm$Genus[ssm$Genus == 'Beringraja '] <- 'Raja '

####make the scientific name column again ##################################################################################################################
ssm$scientific_name<- paste(ssm$Genus,"_",ssm$Species)
ssm$scientific_name<-gsub(" ","",ssm$scientific_name)
ssm$scientific_name<-gsub("_"," _ ",ssm$scientific_name)

#remove genus and species
ssm$Genus<- NULL
ssm$Species<- NULL

####incorporate stock status information; consider B/Bmsy for higher resolution later? ########################################################
#remove asterisk
status$Entity.Name<-gsub("[*]","",status$Entity.Name)

status<-status[,4:6]

colnames(status)<- c('Stock.Name', 'overfishing', 'overfished')

#combine with species name
status<-left_join(ssm, status, by = 'Stock.Name')

#assign average values or 'unknown' to missing values
status$overfishing[is.na(status$overfishing)]<- "Unknown"
status$overfished[is.na(status$overfished)]<- "Unknown"

##### fix edited spp in status ########################################################################################################################
#first, seperate genus and species to make this code work
status<-separate(status, col = scientific_name, sep = "_", into = c("Genus", "Species"))

#Arrowtooth flounder reclassified into a new genus; spp name is stomias. Other flounders too?
status$Genus[status$Genus == 'Reinhardtius '] <- 'Atheresthes ' #hippoglossoides is still the OG genus
status$Genus[status$Species == ' hippoglossoides']<-"Reinhardtius " #this one still has the Reinharditius genus

#fix sole genera
status$Genus[status$Genus == 'Pleuronectes '] <- 'Pleuronichthys '
status$Genus[status$Species == ' quadrituberculatus']<-'Pleuronectes ' #Alaska plaice still has the og genus

#fix grouper genera
status$Genus[status$Species == ' niveatus']<-"Hyporthodus "
status$Genus[status$Species == ' nigritus']<-"Hyporthodus "
status$Genus[status$Species == ' flavolimbatus']<-"Hyporthodus "

#fix big skate
status$Genus[status$Genus == 'Beringraja '] <- 'Raja '

####make the scientific name column again ##################################################################################################################
status$scientific_name<- paste(status$Genus,"_",status$Species)
status$scientific_name<-gsub(" ","",status$scientific_name)
status$scientific_name<-gsub("_"," _ ",status$scientific_name)

#fix the _NA species for complexes
status$scientific_name<-gsub(" _ NA",NA,status$scientific_name)

#remove NA species 
status<-status[!is.na(status$scientific_name),]

#determine number of species; 290
all_spp<- ssm %>%
  summarize(
    n = n_distinct(scientific_name)
  )

#remove inverts
target_genera <- c('Spisula ', 'Sicyonia ', 'Ranina ', 'Pleoticus ', 'Placopecten ', 
                   'Patinopecten ', 'Paralithodes ', 'Panulirus ', 'Loligo ', 
                   'Litopenaeus ', 'Lithodes ', 'Illex ', 'Farfantepenaeus ', 
                   'Enteroctopus ', 'Chionoecetes ', 'Chaceon ', 'Arctica ')

inverts <- status[status$Genus %in% target_genera, ]

inverts<-inverts%>% #to work with rfishbase
  unite("scientific_name",Genus,Species,sep = " ",remove = F)

status <- status[!status$Genus %in% target_genera, ]

#trim whitespace
status$Genus<-trimws(status$Genus)
status$Species<-trimws(status$Species)

#### adjust gap analysis table, then merge with status #################################
#trim columns on gap analysis
gap<-gap[,c(1,4,6,7,9,10,12,13,15,16,18,19)]

#manipulate to merge with other dfs; revisit if doing the fishbase lookup and merging
colnames(gap)<- c('Stock.Name','Target.Frequency',"Catch.Target","Catch.Gap","Abundance.Target","Abundance.Gap","LH.Target","LH.Gap",
                  "Comp.Target","Comp.Gap","Eco.Target","Eco.Gap")

#get an average gap
gap$Mean.Gap <- rowMeans(gap[, c(4, 6, 8, 10)], na.rm = TRUE)

#join with status
status<-left_join(status,gap, by = "Stock.Name")

####now work with CVA data ##############################################################################################################################
#manipulate CVA data
cva<-cva[,c(1,3,4,10)]

#fix colun names
colnames(cva)<-c("Common.Name","Attribute.Type","Attribute.Name","Mean")

#remove variables that won't be used because of duplication
cva_filtered<- cva%>%
  filter(!Attribute.Name %in% c("Population Growth Rate", "Stock Size/Status"))

#get a mean sensitivty and exposure metric across all variables
#ignoring the component score and vulnerability rank for now. 
cva_aggregated<-cva_filtered%>%
  group_by(Common.Name,Attribute.Type)%>%
  summarize(Aggregated.cva.metric = mean(Mean,na.rm = T))%>%
  ungroup()

#pivot wider to make columns for exposure and sensitivity
cva_aw<-cva_aggregated%>%
  pivot_wider(names_from = Attribute.Type, values_from = Aggregated.cva.metric)

#join with status by common name
status<-left_join(status,cva_aw, by = "Common.Name")

#### FishLife ##################################################################################################
#### fix some names and make a list to apply the function

#fix alaska pollock; USED
status$Species[status$Genus == 'Gadus'] #name appears fine, problem with FishLife...
status$Species[status$Species == 'chalcogrammus']<- "predictive" #change to predictive since the species doesn't work. 

#Deacon rockfish not coming up; duplicate Blue rockfish later, predictive for now
status$Species[status$Species == 'diaconus']<- "predictive"

#Sunset rockfish not coming up; dublicate vermilion later, predictive for now
status$Species[status$Species == 'crocotulus']<- "predictive"

#make a list with a df for each organism ###############################################################################################################
status_list <- setNames(split(status, seq(nrow(status))), paste(status$Genus,"_",status$Species))

###fishlife function ###################################################################################################################################
FLfn<-function(x){
  y = Plot_taxa(Search_species(Genus=x["Genus"],Species=x["Species"])$match_taxonomy, mfrow=c(3,2))
  z = y[[1]]$Mean_pred
  return(z)
}

FL_data<- lapply(status_list,FLfn)

#### make one df
#make dfs for each spp
FL_data<-lapply(FL_data,rbind)
FL_data<-lapply(FL_data,as.data.frame)
FL_data<-map2(FL_data,names(FL_data), cbind)

#make one df
FL_df<-do.call(rbind,FL_data)

#fix Alaska pollock
FL_df$`.y[[i]]`[FL_df$`.y[[i]]` == 'Gadus _ predictive']<- "Gadus _ chalcogrammus" #change to predictive since the species doesn't work. 

#Duplicate Blue rockfish for deacon rockfish
FL_df<-FL_df[!FL_df$`.y[[i]]` == 'Sebastes _ predictive',]

FL_df[551,]<-FL_df[FL_df$`.y[[i]]` == 'Sebastes _ mystinus',]
FL_df[551,21]<- 'Sebastes _ diaconus'

#Duplicate vermilion rockfish for sunset rockfish
FL_df[552,]<-FL_df[FL_df$`.y[[i]]` == 'Sebastes _ miniatus',]
FL_df[552,21]<- 'Sebastes _ crocotulus'

#remove duplicates
FL_df<-unique(FL_df)

#exponentiate to get familiar values for LH params
FL_df[,1:7]<-apply(FL_df[,1:7],2,exp)

#exponentiate other variables
FL_df$Fmsy_over_M<- exp(FL_df$ln_Fmsy_over_M)
FL_df$Fmsy<- exp(FL_df$ln_Fmsy)
FL_df$MASPS<- exp(FL_df$ln_MASPS)
FL_df$margsd<- exp(FL_df$ln_margsd)

#name columns
colnames(FL_df)<-c("Loo","K","Winfinity","tmax","tm","M","Lm","Temperature","ln_var","rho","ln_MASPS","ln_margsd","h","logitbound_h","ln_Fmsy_over_M",
                   "ln_Fmsy","ln_r","r","ln_G", "G","scientific_name","Fmsy_over_M","Fmsy","MASPS","margsd")

#### make one DF ####################################################################################################################################
alldata<- left_join(status,FL_df, by = c("scientific_name"))

#write csv
#write.csv(alldata,"raw_input_PSA_data_updated.csv",row.names = F)
