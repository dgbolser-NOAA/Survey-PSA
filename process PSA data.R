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
pawd<- "C:/Users/Derek.Bolser/Documents/Productivity analysis/SurveyPSA"
setwd(pawd)

#read in data
gap<- read.csv("gap_analysis_assessment_table.csv")
cva<-read.csv("cva_data.csv")
sis_spp<- read.csv("sis_sci_names.csv")
ssm<- read.csv("Assessment_Summary_Data_stock_smart.csv")
status<-read.csv("SIS_stock_status.csv")

#### populate dfs with species from complexes #################################################################################################
ssm_c<- ssm[ssm$Scientific.Name == "",]

#atlantic coastal sharks 
ssm_c[32:34,]<-ssm_c[1,]

ssm_c[1,2]<-"Carcharhinus acronotus"
ssm_c[1,3]<-"Blacknose shark"

ssm_c[32,2]<-"Rhizoprionodon terraenovae"
ssm_c[32,3]<-"Atlantic sharpnose shark"

ssm_c[33,2]<-"Sphyrna tiburo"
ssm_c[33,3]<-"Bonnethead shark"

ssm_c[34,2]<-"Carcharhinus isodon"
ssm_c[34,3]<-"Finetooth shark"

#gulf smoothhound
ssm_c[35:36,]<-ssm_c[2,]

ssm_c[2,2]<-"Mustelus canis"
ssm_c[2,3]<-"Smooth dogfish"

ssm_c[35,2]<-"Mustelus norrisi"
ssm_c[35,3]<-"Florida smoothhound"

ssm_c[36,2]<-"Mustelus sinusmexicanus"
ssm_c[36,3]<-"Gulf smoothhound"

#caribbean parrotfishes complex; no stock assessment online, including complexes 1 and 2
ssm_c[37:45,]<-ssm_c[3,]

ssm_c[3,2]<-"Scarus coeruleus"
ssm_c[3,3]<-"Blue parrotfish"

ssm_c[37,2]<-"Scarus coelestinus"
ssm_c[37,3]<-"Midnight parrotfish"

ssm_c[38,2]<-"Scarus guacamaia"
ssm_c[38,3]<-"Rainbow parrotfish"

ssm_c[39,2]<-"Scarus taeniopterus"
ssm_c[39,3]<-"Princess parrotfish"

ssm_c[40,2]<-"Scarus vetula"
ssm_c[40,3]<-"Queen parrotfish"

ssm_c[41,2]<-"Sparisoma aurofrenatum"
ssm_c[41,3]<-"Redband parrotfish"

ssm_c[42,2]<-"Sparisoma rubripinne"
ssm_c[42,3]<-"Redfin parrotfish"

ssm_c[43,2]<-"Sparisoma chrysopterum"
ssm_c[43,3]<-"Redtail parrotfish"

ssm_c[44,2]<-"Sparisoma viride"
ssm_c[44,3]<-"Stoplight parrotfish"

ssm_c[45,2]<-"Scarus iseri"
ssm_c[45,3]<-"Striped parrotfish"

#Aleutian Islands Blackspotted and Rougheye Rockfish Complex
ssm_c[46,]<-ssm_c[4,]

ssm_c[4,2]<-"Sebastes aleutianus"
ssm_c[4,3]<-"Rougheye rockfish"

ssm_c[46,2]<-"Sebastes melanostictus"
ssm_c[46,3]<-"Blackspotted rockfish"

#Bering Sea / Aleutian Islands Blackspotted and Rougheye Rockfish Complex
ssm_c[47,]<-ssm_c[5,]

ssm_c[5,2]<-"Sebastes aleutianus"
ssm_c[5,3]<-"Rougheye rockfish"

ssm_c[47,2]<-"Sebastes melanostictus"
ssm_c[47,3]<-"Blackspotted rockfish"

#Bering Sea / Aleutian Islands Other Flatfish Complex
ssm_c[48:61,]<-ssm_c[6,]

ssm_c[6,2]<-"Liopsetta glacialis"
ssm_c[6,3]<-"Arctic flounder"

ssm_c[48,2]<-"Isopsetta isolepis"
ssm_c[48,3]<-"Butter sole"

ssm_c[49,2]<-"Pleuronectes decurrens"
ssm_c[49,3]<-"Curlfin sole"

ssm_c[50,2]<-"Embassichthys bathybius"
ssm_c[50,3]<-"Deepsea sole"

ssm_c[51,2]<-"Microstomus pacificus"
ssm_c[51,3]<-"Dover sole"

ssm_c[52,2]<-"Parophrys vetulus"
ssm_c[52,3]<-"English sole"

ssm_c[53,2]<-"Limanda proboscidea"
ssm_c[53,3]<-"Longhead dab"

ssm_c[54,2]<-"Citharichthys sordidus"
ssm_c[54,3]<-"Pacific sanddab"

ssm_c[55,2]<-"Eopsetta jordani" #still in the complex?
ssm_c[55,3]<-"Petrale sole"

ssm_c[56,2]<-"Glyptocephalus zachirus"
ssm_c[56,3]<-"Rex sole"

ssm_c[57,2]<-"Clidoderma asperrimum"
ssm_c[57,3]<-"Roughscale sole"

ssm_c[58,2]<-"Psettichthys melanostictus"
ssm_c[58,3]<-"Sand sole"

ssm_c[59,2]<-"Lyopsetta exilis"
ssm_c[59,3]<-"Slender sole"

ssm_c[60,2]<-"Platichthys stellatus"
ssm_c[60,3]<-"Starry flounder"

ssm_c[61,2]<-"Limanda sakhalinensis"
ssm_c[61,3]<-"Sakhalin sole"

#Bering Sea / Aleutian Islands Other Rockfish Complex; primarily shortspine thornyhead (95%); the remaining 5% dominated by dusky rockfish
ssm_c[62,]<-ssm_c[7,]

ssm_c[7,2]<-"Sebastolobus alascanus"
ssm_c[7,3]<-"Shortspine thornyhead"

ssm_c[62,2]<-"Sebastes variabilis"
ssm_c[62,3]<-"Dusky rockfish"

#Bering Sea / Aleutian Islands Other Skates Complex !!!! this is really the skates complex; not other skates!!!!
ssm_c[63:67,]<-ssm_c[11,]

ssm_c[11,2]<-"Bathyraja parmifera"
ssm_c[11,3]<-"Alaska skate"

ssm_c[63,2]<-"Bathyraja aleutica"
ssm_c[63,3]<-"Aleutian skate"

ssm_c[64,2]<-"Beringraja binoculata"
ssm_c[64,3]<-"Big skate"

ssm_c[65,2]<-"Bathyraja interrupta"
ssm_c[65,3]<-"Bering skate"

ssm_c[66,2]<-"Bathyraja maculata"
ssm_c[66,3]<-"Whiteblotched skate"

ssm_c[67,2]<-"Bathyraja taranetzi"
ssm_c[67,3]<-"Mud skate"

#Bering Sea / Aleutian Islands Sculpin Complex; 26 spp
ssm_c[68:92,]<-ssm_c[9,]

ssm_c[9,2]<-"Hemitripterus bolini"
ssm_c[9,3]<-"Bigmouth sculpin"

ssm_c[68,2]<-"Myoxocephalus polyacanthocephalus"
ssm_c[68,3]<-"Great sculpin"

ssm_c[69,2]<-"Myoxocephalus jaok"
ssm_c[69,3]<-"Plain sculpin"

ssm_c[70,2]<-"Myoxocephalus scorpius"
ssm_c[70,3]<-"Warty sculpin"

ssm_c[71,2]<-"Hemilepidotus jordani"
ssm_c[71,3]<-"Yellow irish lord"

ssm_c[72,2]<-"Enophrys diceraus"
ssm_c[72,3]<-"Antlered sculpin"

ssm_c[73,2]<-"Gymnocanthus tricuspis"
ssm_c[73,3]<-"Arctic staghorn sculpin"

ssm_c[74,2]<-"Gymnocanthus galeatus"
ssm_c[74,3]<-"Armorhead sculpin"

ssm_c[75,2]<-"Malacocottus kincaidi"
ssm_c[75,3]<-"Blackfin sculpin"

ssm_c[76,2]<-"Hemilepidotus papilio"
ssm_c[76,3]<-"Butterfly sculpin"

ssm_c[77,2]<-"Blepsias bilobus"
ssm_c[77,3]<-"Crested sculpin"

ssm_c[78,2]<-"Malacocottus zonurus"
ssm_c[78,3]<-"Darkfin sculpin"

ssm_c[79,2]<-"Nautichthys pribilovius"
ssm_c[79,3]<-"Eyeshade sculpin"

ssm_c[80,2]<-"Triglops metopias"
ssm_c[80,3]<-"Highbrow sculpin"

ssm_c[81,2]<-"Artediellus pacificus"
ssm_c[81,3]<-"Hookhorn sculpin"

ssm_c[82,2]<-"Gymnocanthus detrisus"
ssm_c[82,3]<-"Purplegray sculpin"

ssm_c[83,2]<-"Hemilepidotus hemilepidotus"
ssm_c[83,3]<-"Red irish lord"

ssm_c[84,2]<-"Triglops pingelii"
ssm_c[84,3]<-"Ribbed sclupin"

ssm_c[85,2]<-"Trachidermus fasciatus"
ssm_c[85,3]<-"Roughspine sclupin" #actual common name is roughskin; no results for roughspine

ssm_c[86,2]<-"Nautichthys oculofasciatus"
ssm_c[86,3]<-"Sailfin sclupin"

ssm_c[87,2]<-"Triglops forficatus"
ssm_c[87,3]<-"Scissortail sclupin"

ssm_c[88,2]<-"Icelus spatula"
ssm_c[88,3]<-"Spatulate sclupin"

ssm_c[89,2]<-"Triglops scepticus"
ssm_c[89,3]<-"Spectacled sclupin"

ssm_c[90,2]<-"Dasycottus setiger"
ssm_c[90,3]<-"Spinyhead sclupin"

ssm_c[91,2]<-"Icelus spiniger"
ssm_c[91,3]<-"Thorny sclupin"

ssm_c[92,2]<-"Gymnocanthus pistilliger"
ssm_c[92,3]<-"Threaded sclupin"

#Bering Sea / Aleutian Islands Shark Complex
ssm_c[92:93,]<-ssm_c[10,]

ssm_c[10,2]<-"Squalus suckleyi"
ssm_c[10,3]<-"Pacific spiny dogfish"

ssm_c[92,2]<-"Somniosus pacificus"
ssm_c[92,3]<-"Pacific sleeper shark"

ssm_c[92,2]<-"Lamna ditropis"
ssm_c[92,3]<-"Salmon shark"

#no stock assessment for 'other skates'
#exclude squid

#Eastern Bering Sea Blackspotted and Rougheye Rockfish Complex
ssm_c[93,]<-ssm_c[13,]

ssm_c[13,2]<-"Sebastes aleutianus"
ssm_c[13,3]<-"Rougheye rockfish"

ssm_c[93,2]<-"Sebastes melanostictus"
ssm_c[93,3]<-"Blackspotted rockfish"

#	Gulf of Alaska Blackspotted and Rougheye Rockfish Complex
ssm_c[94,]<-ssm_c[14,]

ssm_c[14,2]<-"Sebastes aleutianus"
ssm_c[14,3]<-"Rougheye rockfish"

ssm_c[94,2]<-"Sebastes melanostictus"
ssm_c[94,3]<-"Blackspotted rockfish"

#GOA other rockfish; 27 spp
ssm_c[95:120,]<-ssm_c[15,]

ssm_c[15,2]<-"Sebastes aurora"
ssm_c[15,3]<-"Aurora rockfish"

ssm_c[95,2]<-"Sebastes melanostomus"
ssm_c[95,3]<-"Blackgill rockfish"

ssm_c[96,2]<-"Sebastes paucispinis"
ssm_c[96,3]<-"Bocaccio"

ssm_c[97,2]<-"Sebastes goodei"
ssm_c[97,3]<-"Chilipepper"

ssm_c[98,2]<-"Sebastes crameri"
ssm_c[98,3]<-"Darkblotched rockfish"

ssm_c[99,2]<-"Sebastes elongatus"
ssm_c[99,3]<-"Greenstriped rockfish"

ssm_c[100,2]<-"Sebastes variegatus"
ssm_c[100,3]<-"Harlequin rockfish"

ssm_c[101,2]<-"Sebastes polyspinis"
ssm_c[101,3]<-"Northern rockfish"

ssm_c[102,2]<-"Sebastes wilsoni"
ssm_c[102,3]<-"Pygmy rockfish"

ssm_c[103,2]<-"Sebastes babcocki"
ssm_c[103,3]<-"Redbanded rockfish"

ssm_c[104,2]<-"Sebastes proriger"
ssm_c[104,3]<-"Redstripe rockfish"

ssm_c[105,2]<-"Sebastes zacentrus"
ssm_c[105,3]<-"Sharpchin rockfish"

ssm_c[106,2]<-"Sebastes jordani"
ssm_c[106,3]<-"Shortbelly rockfish"

ssm_c[107,2]<-"Sebastes brevispinis"
ssm_c[107,3]<-"Silvergray rockfish"

ssm_c[108,2]<-"Sebastes diploproa"
ssm_c[108,3]<-"Splitnose rockfish"

ssm_c[109,2]<-"Sebastes saxicola"
ssm_c[109,3]<-"Stripetail rockfish"

ssm_c[110,2]<-"Sebastes miniatus"
ssm_c[110,3]<-"Vermilion rockfish"

ssm_c[111,2]<-"Sebastes entomelas"
ssm_c[111,3]<-"Widow rockfish"

ssm_c[112,2]<-"Sebastes reedi"
ssm_c[112,3]<-"Yellowmouth rockfish"

ssm_c[113,2]<-"Sebastes flavidus"
ssm_c[113,3]<-"Yellowtail rockfish"

ssm_c[114,2]<-"Sebastes pinniger"
ssm_c[114,3]<-"Canary rockfish"

ssm_c[115,2]<-"Sebastes nebulosus"
ssm_c[115,3]<-"China rockfish"

ssm_c[116,2]<-"Sebastes caurinus"
ssm_c[116,3]<-"Copper rockfish"

ssm_c[117,2]<-"Sebastes maliger"
ssm_c[117,3]<-"Quillback rockfish"

ssm_c[118,2]<-"Sebastes helvomaculatus"
ssm_c[118,3]<-"Rosethorn rockfish"

ssm_c[119,2]<-"Sebastes nigrocinctus"
ssm_c[119,3]<-"Tiger rockfish"

ssm_c[120,2]<-"Sebastes ruberrimus"
ssm_c[120,3]<-"Yelloweye rockfish"

#Gulf of Alaska Other Shallow Water Flatfish Complex; no stock assessment
#Gulf of alaska sclupin complex; there might be overlap with the other sculpin complex
ssm_c[121:123,]<-ssm_c[17,]

ssm_c[17,2]<-"Hemilepidotus jordani"
ssm_c[17,3]<-"Yellow irish lord"

ssm_c[121,2]<-"Myoxocephalus polyacanthocephalus"
ssm_c[121,3]<-"Great sculpin"

ssm_c[122,2]<-"Hemitripterus bolini"
ssm_c[122,3]<-"Bigmouth sculpin"

ssm_c[123,2]<-"Myoxocephalus jaok"
ssm_c[123,3]<-"Plain sculpin"

#Gulf of Alaska Shallow Water Flatfish Complex
ssm_c[123:129,]<-ssm_c[18,]

ssm_c[18,2]<-"Lepidopsetta polyxystra"
ssm_c[18,3]<-"Northern rock sole"

ssm_c[123,2]<-"Lepidopsetta bilineata"
ssm_c[123,3]<-"Southern rock sole"

ssm_c[124,2]<-"Limanda aspera"
ssm_c[124,3]<-"Yellowfin sole"

ssm_c[125,2]<-"Isopsetta isolepis"
ssm_c[125,3]<-"Butter sole"

ssm_c[126,2]<-"Platichthys stellatus"
ssm_c[126,3]<-"Starry flounder"

ssm_c[127,2]<-"Parophrys vetulus"
ssm_c[127,3]<-"English sole"

ssm_c[128,2]<-"Psettichthys melanostictus"
ssm_c[128,3]<-"Sand sole"

ssm_c[129,2]<-"Pleuronectes quadrituberculatus"
ssm_c[129,3]<-"Alaska plaice"

#Gulf of Alaska shark complex; same spp as other complex
ssm_c[130:131,]<-ssm_c[19,]

ssm_c[19,2]<-"Squalus suckleyi"
ssm_c[19,3]<-"Spiny dogfish" #pacific spiny dogfish?

ssm_c[130,2]<-"Somniosus pacificus"
ssm_c[130,3]<-"Pacific sleeper shark"

ssm_c[131,2]<-"Lamna ditropis"
ssm_c[131,3]<-"Salmon shark"

#Gulf of Alaska Skate complex; overlap here too
ssm_c[132:136,]<-ssm_c[20,]

ssm_c[20,2]<-"Bathyraja parmifera"
ssm_c[20,3]<-"Alaska skate"

ssm_c[132,2]<-"Bathyraja aleutica"
ssm_c[132,3]<-"Aleutian skate"

ssm_c[133,2]<-"Beringraja binoculata"
ssm_c[133,3]<-"Big skate"

ssm_c[134,2]<-"Bathyraja interrupta"
ssm_c[134,3]<-"Bering skate"

ssm_c[135,2]<-"Raja rhina"
ssm_c[135,3]<-"Longnose skate"

ssm_c[136,2]<-"Amblyraja badia"
ssm_c[136,3]<-"Roughshoulder skate"

#ignore GOA squid for now

#California Blue and Deacon rockfish Complex
ssm_c[137,]<-ssm_c[22,]

ssm_c[22,2]<-"Sebastes mystinus"
ssm_c[22,3]<-"Blue rockfish"

ssm_c[137,2]<-"Sebastes diaconus"
ssm_c[137,3]<-"Deacon rockfish"

#Northern California Gopher / Black-and-Yellow Rockfish Complex
ssm_c[138,]<-ssm_c[23,]

ssm_c[23,2]<-"Sebastes carnatus"
ssm_c[23,3]<-"Gopher rockfish"

ssm_c[138,2]<-"Sebastes chrysomelas"
ssm_c[138,3]<-"Black-and-yellow rockfish"

#Oregon Blue and Deacon Rockfish Complex
ssm_c[139,]<-ssm_c[24,]

ssm_c[24,2]<-"Sebastes mystinus"
ssm_c[24,3]<-"Blue rockfish"

ssm_c[139,2]<-"Sebastes diaconus"
ssm_c[139,3]<-"Deacon rockfish"

#Pacific Coast Blackspotted and Rougheye Rockfish Complex
ssm_c[140,]<-ssm_c[25,]

ssm_c[25,2]<-"Sebastes melanostictus"
ssm_c[25,3]<-"Blackspotted rockfish"

ssm_c[140,2]<-"Sebastes aleutianus"
ssm_c[140,3]<-"Rougheye rockfish"

#Vermilion and Sunset Rockfish Complex - Southern California
ssm_c[141,]<-ssm_c[26,]

ssm_c[26,2]<-"Sebastes miniatus"
ssm_c[26,3]<-"Vermilion rockfish"

ssm_c[141,2]<-"Sebastes crocotulus"
ssm_c[141,3]<-"Sunset rockfish"

#Vermilion and Sunset rockfish Complex - Northern California
ssm_c[142,]<-ssm_c[27,]

ssm_c[27,2]<-"Sebastes miniatus"
ssm_c[27,3]<-"Vermilion rockfish"

ssm_c[142,2]<-"Sebastes crocotulus"
ssm_c[142,3]<-"Sunset rockfish"

#American Samoa Bottomfish Multi-species Complex (11 spp)
ssm_c[143:152,]<-ssm_c[28,]

ssm_c[28,2]<-"Aphareus rutilans"
ssm_c[28,3]<-"Rusty jobfish"

ssm_c[143,2]<-"Aprion virescens"
ssm_c[143,3]<-"Green jobfish"

ssm_c[144,2]<-"Caranx lugubris"
ssm_c[144,3]<-"Black trevally"

ssm_c[145,2]<-"Etelis carbunculus"
ssm_c[145,3]<-"Ruby snapper"

ssm_c[146,2]<-"Etelis coruscans"
ssm_c[146,3]<-"Flame snapper"

ssm_c[147,2]<-"Lethrinus rubrioperculatus"
ssm_c[147,3]<-"Redgill emperor"

ssm_c[148,2]<-"Lutjanus kasmira"
ssm_c[148,3]<-"Bluestripe snapper"

ssm_c[149,2]<-"Pristipomoides flavipinnis"
ssm_c[149,3]<-"Yelloweye snapper"

ssm_c[150,2]<-"Pristipomoides zonatus"
ssm_c[150,3]<-"Oblique-banded snapper"

ssm_c[151,2]<-"Pristipomoides filamentosus"
ssm_c[151,3]<-"Pink snapper"

ssm_c[152,2]<-"Variola louti"
ssm_c[152,3]<-"Lyretail grouper"

#Main Hawaiian Islands Deep 7 Bottomfish Multi-species Complex
ssm_c[152:157,]<-ssm_c[29,]

ssm_c[29,2]<-"Etelis carbunculus"
ssm_c[29,3]<-"Squirrelfish snapper"

ssm_c[152,2]<-"Pristipomoides zonatus"
ssm_c[152,3]<-"Brigham's snapper"

ssm_c[153,2]<-"Hyporthodus quernus"
ssm_c[153,3]<-"Hawaiian grouper"

ssm_c[154,2]<- "Pristipomoides sieboldii"
ssm_c[154,3]<- "Von Siebold's snapper"

ssm_c[155,2]<- "Aphareus rutilans"
ssm_c[155,3]<- "Silverjaw snapper"

ssm_c[156,2]<-"Etelis coruscans"
ssm_c[156,3]<-"Longtail snapper"

ssm_c[157,2]<-"Pristipomoides filamentosus"
ssm_c[157,3]<-"Pink snapper"

#Guam Bottomfish Multi-species Complex (13 spp)
ssm_c[158:169,]<-ssm_c[30,]

ssm_c[30,2]<-"Aphareus rutilans"
ssm_c[30,3]<-"Rusty jobfish"

ssm_c[158,2]<-"Caranx lugubris"
ssm_c[158,3]<-"Black trevally"

ssm_c[159,2]<-"Caranx ignobilis"
ssm_c[159,3]<-"Giant trevally"

ssm_c[160,2]<-"Etelis carbunculus"
ssm_c[160,3]<-"Ruby snapper"

ssm_c[161,2]<-"Etelis coruscans"
ssm_c[161,3]<-"Flame snapper"

ssm_c[162,2]<-"Lethrinus rubrioperculatus"
ssm_c[162,3]<-"Redgill emperor"

ssm_c[163,2]<-"Lutjanus kasmira"
ssm_c[163,3]<-"Bluestripe snapper"

ssm_c[164,2]<-"Pristipomoides flavipinnis"
ssm_c[164,3]<-"Yelloweye snapper"

ssm_c[165,2]<-"Pristipomoides sieboldii"
ssm_c[165,3]<-"Von Siebold's snapper"

ssm_c[166,2]<-"Pristipomoides zonatus"
ssm_c[166,3]<-"Oblique-banded snapper"

ssm_c[167,2]<-"Pristipomoides auricilla"
ssm_c[167,3]<-"Goldflag snapper"

ssm_c[168,2]<-"Pristipomoides filamentosus"
ssm_c[168,3]<-"Pink snapper"

ssm_c[169,2]<-"Variola louti"
ssm_c[169,3]<-"Lyretail grouper"

#Northern Mariana Islands Bottomfish Multi-species Complex (13 spp)
ssm_c[170:181,]<-ssm_c[31,]

ssm_c[31,2]<-"Aphareus rutilans"
ssm_c[31,3]<-"Rusty jobfish"

ssm_c[170,2]<-"Caranx lugubris"
ssm_c[170,3]<-"Black trevally"

ssm_c[171,2]<-"Caranx ignobilis"
ssm_c[171,3]<-"Giant trevally"

ssm_c[172,2]<-"Etelis carbunculus"
ssm_c[172,3]<-"Ruby snapper"

ssm_c[173,2]<-"Etelis coruscans"
ssm_c[173,3]<-"Flame snapper"

ssm_c[174,2]<-"Lethrinus rubrioperculatus"
ssm_c[174,3]<-"Redgill emperor"

ssm_c[175,2]<-"Lutjanus kasmira"
ssm_c[175,3]<-"Bluestripe snapper"

ssm_c[176,2]<-"Pristipomoides flavipinnis"
ssm_c[176,3]<-"Yelloweye snapper"

ssm_c[177,2]<-"Pristipomoides sieboldii"
ssm_c[177,3]<-"Von Siebold's snapper"

ssm_c[178,2]<-"Pristipomoides zonatus"
ssm_c[178,3]<-"Oblique-banded snapper"

ssm_c[179,2]<-"Pristipomoides auricilla"
ssm_c[179,3]<-"Goldflag snapper"

ssm_c[180,2]<-"Pristipomoides filamentosus"
ssm_c[180,3]<-"Pink snapper"

ssm_c[181,2]<-"Variola louti"
ssm_c[181,3]<-"Lyretail grouper"

#### include new species from complexes in sis_spp
sis_c<- ssm_c[,2:3]
sis_c<-sis_c[!sis_c$Scientific.Name == "",]
colnames(sis_c)<-c("scientific_name","common_name")

sis_spp<-rbind(sis_spp,sis_c)

#### add complex spp to ssm
ssm<-rbind(ssm,ssm_c)

####incorporate model category (species lvl) ################################################################
##fix stuff
ssm$Scientific.Name<- gsub(" "," _ ", ssm$Scientific.Name)

#ssm<-ssm[,c(1,2,5)]
#ssm<-ssm[!ssm$Scientific.Name == "",]

#fix columns
colnames(ssm)<-c('stock_name', 'scientific_name', 'common_name','stock_area','assessment_level')

#fix names in ssm first; not sure why they're wrong...
#first, seperate genus and species to make this code work
ssm<-separate(ssm, col = scientific_name, sep = "_", into = c("Genus", "Species"))

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

status<-status[,4:7]

colnames(status)<- c('stock_name', 'overfishing', 'overfished', 'approaching_overfished')

#combine with species name
status<-left_join(ssm, status, by = 'stock_name')

#remove species complexes
#status<- status %>% 
#  filter(!grepl('Complex', stock_name))

#status<-status[,1:5]

#assign average values or 'unknown' to missing values
status$overfishing[is.na(status$overfishing)]<- "Unknown"
status$overfished[is.na(status$overfished)]<- "Unknown"
#status$assessment_level[is.na(status$assessment_level)]<- 2.5

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

#remove genus and species
status$Genus<- NULL
status$Species<- NULL
status$approaching_overfished<-NULL

#fix the _NA species for complexes
status$scientific_name<-gsub(" _ NA",NA,status$scientific_name)

#### examine species from sis #############################################################################################
#throw out duplicates
sis_spp<-sis_spp%>%
  group_by(scientific_name)%>%
  mutate(common_name = paste0(common_name, collapse = " ")) #any sort of seperator is gonna get funky

sis_spp<-unique(sis_spp)

sis_spp<-separate(sis_spp, col = scientific_name, sep = " ", into = c("Genus", "Species"))

#deal with inverts
inverts<-sis_spp[sis_spp$Genus == 'Spisula'|sis_spp$Genus == 'Sicyonia'|sis_spp$Genus == 'Ranina'|sis_spp$Genus == 'Pleoticus'|
                   sis_spp$Genus == 'Placopecten'|sis_spp$Genus == 'Patinopecten'|sis_spp$Genus == 'Paralithodes'|sis_spp$Genus == 'Panulirus'|
                   sis_spp$Genus == 'Loligo'|sis_spp$Genus == 'Litopenaeus'|sis_spp$Genus == 'Lithodes'|sis_spp$Genus == 'Illex'|
                   sis_spp$Genus == 'Farfantepenaeus'|sis_spp$Genus == 'Enteroctopus'|sis_spp$Genus == 'Chionoecetes'|sis_spp$Genus == 'Chaceon'|
                   sis_spp$Genus == 'Arctica',]

inverts<-inverts%>% #to work with rfishbase
  unite("sci_name",Genus,Species,sep = " ",remove = F)

#exclude inverts from sis_spp so fishlife will work
sis_spp<-sis_spp[!sis_spp$Genus%in%inverts$Genus,]

#Arrowtooth flounder reclassified into a new genus; spp name is stomias. Other flounders too?
sis_spp$Genus[sis_spp$Genus == 'Reinhardtius'] <- 'Atheresthes' #hippoglossoides is still the OG genus
sis_spp$Genus[sis_spp$Species == 'hippoglossoides']<-"Reinhardtius" #this one still has the Reinharditius genus

#fix sole genera
sis_spp$Genus[sis_spp$Genus == 'Pleuronectes'] <- 'Pleuronichthys'
sis_spp$Genus[sis_spp$Species == 'quadrituberculatus']<-'Pleuronectes' #Alaska plaice still has the og genus

#fix grouper genera
sis_spp$Genus[sis_spp$Species == 'niveatus']<-"Hyporthodus"
sis_spp$Genus[sis_spp$Species == 'nigritus']<-"Hyporthodus"
sis_spp$Genus[sis_spp$Species == 'flavolimbatus']<-"Hyporthodus"

#fix big skate
sis_spp$Species[sis_spp$Genus == 'Raja'] #only one member of this genus; change spp to predictive? try changing to Raja first
sis_spp$Genus[sis_spp$Genus == 'Beringraja'] <- 'Raja'

#fix alaska pollock
sis_spp$Species[sis_spp$Genus == 'Gadus'] #name appears fine, problem with FishLife...
sis_spp$Species[sis_spp$Species == 'chalcogrammus']<- "predictive" #change to predictive since the species doesn't work. 

#Deacon rockfish not coming up; duplicate Blue rockfish later, predictive for now
sis_spp$Species[sis_spp$Species == 'diaconus']<- "predictive"

#Sunset rockfish not coming up; dublicate vermilion later, predictive for now
sis_spp$Species[sis_spp$Species == 'crocotulus']<- "predictive"

#make a list with a df for each organism
sis_spp_list <- setNames(split(sis_spp, seq(nrow(sis_spp))), paste(sis_spp$Genus,"_",sis_spp$Species))

#### adjust gap analysis table, then merge with sis_spp #################################
#trim columns on gap analysis
gap<-gap[,c(1:3,7,10,13,16,19)]

#make a common name column
gap$common_name<-gap$Stock
gap$common_name <- iconv(gap$common_name, from = "", to = "UTF-8", sub = "byte") #to fix wacky questionmarks
gap$common_name <- gsub(" <96> ", "--", gap$common_name)
gap$Stock <- iconv(gap$Stock, from = "", to = "UTF-8", sub = "byte") #to fix wacky questionmarks
gap$Stock <- gsub(" <96> ", " - ", gap$Stock)

gap$common_name <- gsub(" -.*", "", gap$common_name)
gap$common_name <- gsub("- .*", "", gap$common_name)
gap$common_name <- gsub("--.*", "", gap$common_name)

#remove complexes and unwanted spp; not necessary for now because things will get thrown out when merging
#gap<-as.data.frame(gap[!grepl("Plan", gap$common_name),])
#gap<-as.data.frame(gap[!grepl("Complex", gap$common_name),])
#gap<-as.data.frame(gap[!grepl("Unit", gap[,1]),])
#gap<-as.data.frame(gap[!grepl("coral", gap[,1]),])
#gap<-as.data.frame(gap[!grepl("Habitat", gap[,1]),])
#gap<-as.data.frame(gap[!grepl("/", gap[,1]),])
#gap<-as.data.frame(gap[!grepl("1", gap[,1]),])
#gap<-as.data.frame(gap[!grepl("2", gap[,1]),])
#gap<-as.data.frame(gap[!grepl("3", gap[,1]),])
#gap<-as.data.frame(gap[!grepl("Multi", gap[,1]),])
#gap<-as.data.frame(gap[!grepl("Fishery", gap[,1]),])
#gap<-as.data.frame(gap[!grepl("Deepwater", gap[,1]),])
#gap<-as.data.frame(gap[!grepl("Shallow", gap[,1]),])
#gap<-as.data.frame(gap[!grepl("Puerto Rico", gap[,1]),])
#gap<-as.data.frame(gap[!grepl("crab", gap[,1]),])
#gap<-as.data.frame(gap[!grepl("shrimp", gap[,1]),])
#gap<-as.data.frame(gap[!grepl("scallop", gap[,1]),])
#gap<-as.data.frame(gap[!grepl("lobster", gap[,1]),])
#gap<-as.data.frame(gap[!grepl("Krill", gap[,1]),])
#gap<-as.data.frame(gap[!grepl("Sargassum", gap[,1]),])
#gap<-as.data.frame(gap[!grepl("fans", gap[,1]),])
#gap<-as.data.frame(gap[!grepl("Refugium", gap[,1]),])
#gap<-as.data.frame(gap[!grepl("squid", gap[,1]),])
#gap<-as.data.frame(gap[!grepl("Gulf", gap[,1]),])
#gap<-as.data.frame(gap[!grepl("Islands", gap[,1]),])
#gap<-as.data.frame(gap[!grepl("Bogoslof", gap[,1]),])
#gap<-as.data.frame(gap[!grepl("Eastern Bering Sea", gap[,1]),])
#gap<-as.data.frame(gap[!grepl("quahog", gap[,1]),])

#trim
gap[,9]<-trimws(gap[,9])

#make model category numeric
#gap<-gap[!is.na(gap$Model_category),]
gap$Model_category<-as.numeric(gap$Model_category)

#calculate an average for species with multiple assessments. we lose the specific stock names but can revisit. 
#gap_summary <- gap %>%
#  group_by(common_name) %>%
#  summarise(across(-c(Stock), ~ mean(.x, na.rm = TRUE)))

#pull data from FishBase; this should not be necessary if merging by stock name. 
#snf<-function(x){
#  y<- common_to_sci(x['common_name'])
#  return(as.data.frame(y))
#}

#sci_name_list<-apply(gap,1,snf)

#make one DF and seperate scientific names to match sis_spp
#sci_name_df<-do.call(rbind,sci_name_list) 

#merge with gap df
#colnames(gap_summary)<-c('ComName','Last_assessment','avg_model_category',"Catch_gap","Abundance_gap","LH_gap","Comp_gap","Eco_gap")
#gap_spp<-left_join(sci_name_df,gap_summary,by = 'ComName')
#gap_spp<- gap_spp[!is.na(gap_spp$avg_model_category),]

#remove codes and language
#gap_spp<- gap_spp[,-c(3,4)]

#manipulate to merge with other dfs; revisit if doing the fishbase lookup and merging
colnames(gap)<- c('stock_name','last_assessment','model_category',"catch_gap","abundance_gap","LH_gap","comp_gap","eco_gap","common_name")

#gap_spp$scientific_name<- gsub(" "," _ ", gap_spp$scientific_name)

# all_scores<- left_join(cname_scores, gap_spp, by = 'scientific_name')
# 
# #extract NAs
# na_category<-all_scores[is.na(all_scores$avg_model_category),]
# na_category<-na_category[,c(1,16)]
# 
# write.csv(na_category, 'missing_model_category_spp.csv',row.names = F)
# 
# #remove spp with NA categories for now
# all_scores<- all_scores[!is.na(all_scores$avg_model_category),]
# 
# 
# ####other extra code
# 
# ####merge to get common name
# sis_og$scientific_name<- gsub(" "," _ ", sis_og$scientific_name)
# 
# colnames(scores)<-c("Loo","K","tmax", "tm","M","h", "r", "MASPS", "margsd", "rho",
#                     "Fmsy_over_M", "scientific_name", "avg_p_score", "avg_s_score", "score")
# 
# cname_scores<- merge(scores, sis_og, by = 'scientific_name')
# cname_scores<-unique(cname_scores)

####now work with CVA data ##############################################################################################################################
#manipulate CVA data
cva<-cva[,c(1:4,10:13)]

#fix colun names
colnames(cva)<-c("common_name","Functional_group","Attribute_type","Attribute_name","Mean","Component_score","Vulnerability_rank","Region")

#remove variables that won't be used because of duplication
cva_filtered<- cva%>%
  filter(!Attribute_name %in% c("Adult Mobility", "Dispersal of Early Life Stages", "Early Life History Survival and Settlement Requirements",
                                "Population Growth Rate", "Spawning Cycle", "Stock Size/Status"))

#pivot wider
cva_wide<-cva_filtered%>%
  pivot_wider(names_from = Attribute_name, values_from = Mean)

#take the highest value of remaining unrepresented sensitivity to stressors (- prey spec, habitat spec, repro complex) and exposure variables
#ignoring the component score and vulnerability rank for now. 
cva_aggregated<-cva_wide%>%
  group_by(common_name,Attribute_type,Region)%>%
  summarize(`Complexity in Reproductive Strategy` = `Complexity in Reproductive Strategy`,
            `Habitat Specificity`= `Habitat Specificity`,`Prey Specificity` = `Prey Specificity`, Functional_group = Functional_group,
            Aggregated_cva_metric = max(c_across(-c(Functional_group,Component_score,Vulnerability_rank, `Complexity in Reproductive Strategy`,`Habitat Specificity`,`Prey Specificity`)),na.rm = T))%>%
  ungroup()

#pivot wider to define the new exposure metric
cva_aw<-cva_aggregated%>%
  pivot_wider(names_from = Attribute_type, values_from = Aggregated_cva_metric)

#consolidate rows
cva_aw<- cva_aw%>%
  group_by(common_name, Region)%>%
  summarize(Functional_group = Functional_group, `Complexity in Reproductive Strategy` = max(`Complexity in Reproductive Strategy`,na.rm = T),
            `Habitat Specificity`= max(`Habitat Specificity`,na.rm = T),`Prey Specificity` = max(`Prey Specificity`,na.rm = T),
            Exposure = max(Exposure,na.rm = T), Sensitivity = max(Sensitivity,na.rm = T))%>%
  ungroup()

cva_aw<-unique(cva_aw)

#change -inf to NA
cva_aw <- cva_aw %>%
  mutate(across(everything(), ~ ifelse(. == -Inf, NA, .)))

#### now fix the names ###############################################################################################################################
#try the scientific name pull from fishbase
#snl_cva<-apply(cva_aw,1,snf)

#make one DF and seperate scientific names to match sis_spp
#snl_cva_df<-do.call(rbind,snl_cva)

#merge with gap df
#colnames(cva_aw)<-c('ComName','Region','Functional_group',"Repro_strat","Hab_spec","Prey_spec","Exposure","Sensitivity")
#cva_spp<-left_join(snl_cva_df,cva_aw,by = 'ComName')
#cva_spp<- cva_spp[!is.na(cva_spp$Region),]

#remove codes and language
#cva_spp<- cva_spp[,-c(3,4)]

#fix scientific name column; lots of species that don't seem accurate but merging will eliminate the bad ones
#colnames(cva_spp)<-c('scientific_name','ComName','Region','Functional_group',"Repro_strat","Hab_spec","Prey_spec","Exposure","Sensitivity")
#cva_spp$scientific_name<- gsub(" "," _ ", cva_spp$scientific_name)

#merge with gap; this eliminates a fair amount but merging with scores will take care of it. 
#gap_cva<- left_join(gap_spp,cva_spp,by = c('ComName','scientific_name'))
#gap_cva<- gap_cva[!is.na(gap_cva$Last_assessment),]

#setwd(pawd)
#write.csv(gap_cva, "processed_gap_and_cva_data.csv", row.names = F)

#### FishLife ##################################################################################################
FLfn<-function(x){
  y = Plot_taxa(Search_species(Genus=x["Genus"],Species=x["Species"])$match_taxonomy, mfrow=c(3,2))
  z = y[[1]]$Mean_pred
  return(z)
}

FL_data<- lapply(sis_spp_list,FLfn)

#### make one df
#make dfs for each spp
FL_data<-lapply(FL_data,rbind)
FL_data<-lapply(FL_data,as.data.frame)
FL_data<-map2(FL_data,names(FL_data), cbind)

#make one df
FL_df<-do.call(rbind,FL_data)

#fix Alaska pollock
FL_df$`.y[[i]]`[FL_df$`.y[[i]]` == 'Gadus _ predictive']<- "Gadus _ chalcogrammus" #change to predictive since the species doesn't work. 

#remove duplicates
FL_df<-unique(FL_df)

#Duplicate Blue rockfish for deacon rockfish
FL_df<-FL_df[!FL_df$`.y[[i]]` == 'Sebastes _ predictive',]

FL_df[261,]<-FL_df[FL_df$`.y[[i]]` == 'Sebastes _ mystinus',]
FL_df[261,21]<- 'Sebastes _ diaconus'

#Duplicate vermilion rockfish for sunset rockfish
FL_df[262,]<-FL_df[FL_df$`.y[[i]]` == 'Sebastes _ miniatus',]
FL_df[262,21]<- 'Sebastes _ crocotulus'

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
#### status has the most entries, followed by ssm, gap, then cva_aw
#### ssm doesn't have any unique information, not necessary anymore
alldata<- left_join(status,gap, by = c('stock_name','common_name'))
alldata<- left_join(alldata,cva_aw, by = 'common_name') #many to many, ok for now...
alldata<- left_join(alldata,FL_df, by = 'scientific_name')

#write csv
write.csv(alldata,"raw_input_PSA_data.csv",row.names = F)
