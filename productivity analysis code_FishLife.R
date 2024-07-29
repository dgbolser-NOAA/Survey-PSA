###################################################################################################################
#### PSA for survey periodicity and prioritization
####
###################################################################################################################

####clear environment
rm(list=ls())

#install packages
#devtools::install_github("james-thorson/FishLife")
#remotes::install_github( 'ropensci/rfishbase@fb-21.06', force=TRUE )
#devtools::install_github("james-thorson/VAST@main")
#devtools::install_github("cfree14/freeR")
#remotes::install_github("cfree14/freeR")

#load packages
library(FishLife)
library(tidyverse)
library(ggrepel)
library(rfishbase)
library(freeR)
library(ramlegacy)

#set wds
pawd<- "C:/Users/Derek.Bolser/Documents/Productivity analysis"

#read in and edit species list
setwd(pawd)
sis_spp<- read.csv("sis_sci_names.csv")
ssm<- read.csv("Assessment_Summary_Data_stock_smart.csv")
status<-read.csv("SIS_stock_status.csv")
surveys<- read.csv('surveys_assessments_sis.csv')
surveys_2<- read.csv('surveys_assessments_sis_secondary.csv')

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

#### FishLife ##################################################################################################
FLfn<-function(x){
  y = Plot_taxa( Search_species(Genus=x["Genus"],Species=x["Species"])$match_taxonomy, mfrow=c(3,2))
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

####now, work with inverts ######################################################################################################################
#use freeR  to query sealifebase
inv_spp <-fishbase(dataset = "species", species = inverts$sci_name, cleaned = T, add_taxa = F)
inv_mat <-fishbase(dataset = "maturity", species = inverts$sci_name, cleaned = T, add_taxa = F)
inv_fec <- fishbase(dataset = "fecundity", species = inverts$sci_name, cleaned = T, add_taxa = F)
inv_lw <-fishbase(dataset = "lw", species = inverts$sci_name, cleaned = T, add_taxa = F)
inv_vb<- fishbase(dataset = "vonb", species = inverts$sci_name, cleaned = T, add_taxa = F)

#make one df
inv_df<-merge(inv_spp,inv_mat,by = c("species","database"))
inv_df<-merge(inv_df,inv_fec,by = c("species","database"))
inv_df<-merge(inv_df,inv_lw,by = c("species","database"))
inv_df<-merge(inv_df,inv_vb,by = c("species","database"))

#remove fishbase NAs
inv_df<-inv_df[!inv_df$database == "FishBase",]



inv_df_red<-inv_df[,c("species","comm_name","location.x","location.y","sex.x","sex.y",
                      "linf_cm", "tl_linf_cm","length_cm_max",
                      "k",
                      "tmax_wild_yr","tmax_yr",
                      "tmat_yr.x","tmat_yr.y","tmat_yr_lo", "tmat_yr_hi",
                      "m",
                      "fecundity_rel_avg","fecundity_abs_min","fecundity_abs_max", "fecundity_rel_min", "fecundity_rel_max")] 

#coalesce columns
inv_df_red<- inv_df_red %>%
  mutate(species = species,
         comm_name = comm_name,
         location = coalesce(location.x, location.y),
         sex = coalesce(sex.x, sex.y),
         linf_cm = linf_cm,
         tl_linf_cm = tl_linf_cm,
         length_cm_max = length_cm_max,
         k = k,
         tmax_wild_yr = tmax_wild_yr,
         tmax_yr = tmax_yr,
         tmat_yr = coalesce(tmat_yr.x, tmat_yr.y),
         tmat_yr_lo = tmat_yr_lo,
         tmat_yr_hi = tmat_yr_hi,
         m=m,
         fecundity_rel_avg = fecundity_rel_avg,
         fecundity_abs_min = fecundity_abs_min, 
         fecundity_abs_max = fecundity_abs_max, 
         fecundity_rel_min = fecundity_rel_min,
         fecundity_rel_max = fecundity_rel_max)


names = c("location.x","location.y","sex.x","sex.y", "tmat_yr.x","tmat_yr.y")
inv_df_red<-inv_df_red[,!names(inv_df_red)%in%names]

#calculate averages 
inv_df_red<-inv_df_red%>%
  group_by(species,comm_name,location,sex)%>%
  summarise_all(mean,na.rm = T)

#now, just group by species for the final DF
inv_df_final<-inv_df_red%>%
  group_by(species,comm_name)%>%
  summarise_all(mean,na.rm = T)

inv_df_final$location<- NULL
inv_df_final$sex<- NULL

#might not have recruitment. r is present on the website but not pulled. manually printed below

#table of r s ############################################################################################################
#Arctica islandica -- 0.47
#Chaceon quinquedens -- NA
#Chionoecetes bairdi -- NA
#Chionoecetes opilio -- 0.58
#Enteroctopus dofleini -- NA
#Farfantepenaeus aztecus --  0.52
#Farfantepenaeus duorarum -- 0.51
#Illex illecebrosus -- 0.50 -- NA, but 0.3 - 2.5 cited for K doubling time; using the midpoint, 1.4
#Lithodes aequispinus -- 6.93 -- NA, but 0.07-0.13 cited for K doubling time
#Litopenaeus setiferus -- 0.56
#Loligo pealeii -- 0.9 -- NA  but 0.59-0.95 cited for K doubling time; using midpoint 0.77
#Panulirus argus -- 0.55
#Paralithodes camtschaticus -- NA
#Paralithodes platypus -- NA
#Patinopecten caurinus -- 1.52 -- NA but 0.39-0.5 cited for K doubling time; using 0.455
#Placopecten magellanicus -- 0.56
#Pleoticus robustus -- 1.78 -- NA, but 0.39 cited for K doubling time
#Ranina ranina -- 0.93
#Sicyonia brevirostris -- 0.81      
#Spisula solidissima -- 0.47

#calculate r from K
calculate_growth_rate <- function(doubling_time) {
  growth_rate <- log(2) / doubling_time
  return(growth_rate)
}

calculate_growth_rate(1.4) #0.50
calculate_growth_rate(0.1) #6.93; this can't be right
calculate_growth_rate(0.77) #0.9
calculate_growth_rate(0.455) #1.52
calculate_growth_rate(0.39) #1.78

#assign values in df
inv_df_final$r<-NA

inv_df_final[1,18]<-0.47
inv_df_final[2,18]<-NA
inv_df_final[3,18]<-NA
inv_df_final[4,18]<-0.58
inv_df_final[5,18]<-NA
inv_df_final[6,18]<-0.52
inv_df_final[6,18]<-0.51
inv_df_final[7,18]<-0.50
inv_df_final[9,18]<-6.93
inv_df_final[10,18]<-0.56
inv_df_final[11,18]<-0.9
inv_df_final[12,18]<-0.55
inv_df_final[13,18]<-NA
inv_df_final[14,18]<-NA
inv_df_final[15,18]<-1.52
inv_df_final[16,18]<-0.56
inv_df_final[17,18]<-1.78
inv_df_final[18,18]<-0.93
inv_df_final[19,18]<-0.81
inv_df_final[20,18]<-0.47

#examine the NAs
inv_df_final[is.na(inv_df_final)]<-NA

#remove all-NA columns
idf<- inv_df_final[, colSums(is.na(inv_df_final)) < nrow(inv_df_final), drop = FALSE]

#finalize the variables
idf<- idf %>%
  mutate(species = species,
         comm_name = comm_name,
         linf_cm = linf_cm,
         k = k,
         tmax = coalesce(tmax_yr, tmax_wild_yr),
         tmat = coalesce(tmat_yr, tmat_yr_hi),
         m=m,
         fecundity_abs_max = fecundity_abs_max)

names2<-c("tl_linf_cm","tmax_yr", "tmax_wild_yr","tmat_yr", "tmat_yr_hi","fecundity_rel_min","fecundity_rel_max")

idf<-idf[!names(idf)%in%names2]

#get stock assessment params for inverts


##### Get productivity scores; low values are low productivity. Following Patrick et al. 2009 tech memo where possible: #######################
#Patrick, W. S., P. Spencer, O. Ormseth, J. Cope, J. Field, D. Kobayashi, T. Gedamke, E. Cortés, K. Bigelow, W. Overholtz,
#J. Link, and P. Lawson. 2009. Use of productivity and susceptibility indices to determine stock vulnerability,
#with example applications to six U.S. fisheries. U.S. Dep. Commer., NOAA Tech. Memo. NMFS-F/SPO-101, 90 p. 
scores<- FL_df[,c(1,2,4:6,18,24,25,10,22,21)] #removed steepness (h, column 13) for now; correlated with M

#unify scores with idf


#Linf
scores<-scores %>%
  mutate(Loo = case_when(Loo < 60 ~ 1, 
                           Loo > 60 & Loo < 150 ~ 2, 
                           Loo > 150 ~ 3))
#K
scores<-scores %>%
  mutate(K = case_when(K < 0.15 ~ 1, 
                         K > 0.15 & K < 0.25 ~ 2, 
                         K > 0.25 ~ 3))
#max age
scores<-scores %>%
  mutate(tmax = case_when(tmax < 10 ~ 3, 
                         tmax > 10 & tmax < 30 ~ 2, 
                         tmax > 30 ~ 1))
#age at maturity
scores<-scores %>%
  mutate(tm = case_when(tm < 2 ~ 3, 
                         tm > 2 & tm < 4 ~ 2, 
                         tm > 4 ~ 1))

#natural mortality
scores<-scores %>%
  mutate(M = case_when(M < 0.2 ~ 1, 
                         M > 0.2 & M < 0.4 ~ 2, 
                         M > 0.4 ~ 3))

#steepness; highly correlated M; excluding for now
#scores<-scores %>%
#  mutate(h = case_when(h < 0.5 ~ 1, 
#                       h > 0.5 & h < 0.7 ~ 2, 
#                       h > 0.7 ~ 3))


#maximum annual spawners per spawner; arbitrary cutoff based on data distribution
scores<-scores %>%
  mutate(MASPS = case_when(MASPS < 1 ~ 1, 
                       MASPS > 1 & MASPS < 5 ~ 2, 
                       MASPS > 5 ~ 3))


#recruitment SD
scores<-scores %>%
  mutate(margsd = case_when(margsd < 0.4 ~ 3, 
                       margsd > 0.4 & margsd < 0.6 ~ 2, 
                       margsd > 0.6 ~ 1))


#population growth rate
scores<-scores %>%
  mutate(r = case_when(r < 0.16 ~ 1, 
                         r > 0.16 & r < 0.5 ~ 2, 
                         r > 0.5 ~ 3))


###### average productivity scores
scores$avg_p_score<-apply(scores[,1:8],1,mean) #changed to 1:8 because of removing 8 (from 1:9)

####susceptibility scores
#score recruitment autocorrelation based on histogram (normally distributed around 0.6)
scores<-scores %>%
  mutate(rho = case_when(rho < 0.5 ~ 1, 
                       rho > 0.5 & rho < 0.7 ~ 2, 
                       rho > 0.7 ~ 3))

#Fmsy/M
scores<-scores %>%
  mutate(Fmsy_over_M = case_when(Fmsy_over_M < 0.5 ~ 3, 
                         Fmsy_over_M > 0.5 & Fmsy_over_M < 1 ~ 2, 
                         Fmsy_over_M > 1 ~ 1))


####incorporate model category (species lvl) ################################################################
##fix stuff
ssm$Scientific.Name<- gsub(" "," _ ", ssm$Scientific.Name)

ssm<-ssm[,c(1,2,5)]
ssm<-ssm[!ssm$Scientific.Name == "",]

#merge with scores
colnames(ssm)<-c('stock_name', 'scientific_name', 'assessment_level')
colnames(scores)<- c("Loo","K","tmax","tm","M","r","MASPS","margsd","rho","Fmsy_over_M","scientific_name",#removed h for now
                     "avg_p_score")


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

#now merge
scores<-left_join(ssm,scores, by = 'scientific_name')

#input an average level if assessment level is unknown, rather than removing species
#scores<-scores[!is.na(scores$assessment_level),]
scores$assessment_level[is.na(scores$assessment_level)]<-2.5 #the midpoint 

#remove non-fish spp for now
scores<-scores[!is.na(scores$avg_p_score),]

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

status<-status[,1:5]

#assign average values or 'unknown' to missing values
status$overfishing[is.na(status$overfishing)]<- "Unknown"
status$overfished[is.na(status$overfished)]<- "Unknown"
status$assessment_level[is.na(status$assessment_level)]<- 2.5

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

#merge with scores ###########################################################################################################################################
scores<-left_join(status,scores, by = c('scientific_name', 'stock_name', 'assessment_level')) 

#remove inverts for now
scores<-scores[!is.na(scores$K),]

#scale model category
scores<-scores %>%
  mutate(assessment_level = case_when(assessment_level <= 2 ~ 1, 
                                      assessment_level > 2 & assessment_level <= 4 ~ 2, 
                                      assessment_level > 4 ~ 3)) #not many 3s
#scale status
scores$status<-NA

scores<-scores %>%
  mutate(status = case_when(overfishing == 'Yes' & overfished == 'Yes' ~ 1, 
                            overfishing == 'Unknown' & overfished == 'Yes' ~ 1, 
                            overfishing == 'Yes' & overfished == 'Unknown' ~ 1,
                            overfishing == 'No' & overfished == 'Yes' ~ 1,
                            overfishing == 'Yes' & overfished == 'No' ~ 1,
                            overfishing == 'Unknown' & overfished == 'Unknown' ~ 2,
                            overfishing == 'No' & overfished == 'Unknown' ~ 2,
                            overfishing == 'Unknown' & overfished == 'No' ~ 2,
                            is.na(overfishing) & is.na(overfished) ~ 2,
                            overfishing == 'No' & overfished == 'No' ~ 3,
                            overfishing == 'No' & overfished == 'No - Rebuilding' ~ 3))
                            

#average susceptibility scores
scores$avg_s_score<-apply(scores[,c(2,14,15,17)],1,mean)

##### avg score overall
scores$score<-apply(scores[,c(16,18)],1,mean)

#### write csv #########################################################################################
setwd(pawd)
#write.csv(scores, "scores_preliminary_just_fishes.csv", row.names = F)

##### plot ############################################################################################
#with scores; not much separation on x-axis
ggplot(scores, aes(x= avg_p_score, y= avg_s_score,label = stock_name)) + 
  geom_hline(yintercept=2, linetype="dashed", color = "red") +
  geom_vline(xintercept=2, linetype="dashed", color = "red")+
  geom_point(stat = "identity") + 
  theme_minimal() + labs(x = "Productivity", y = "Susceptibility") +
 geom_label_repel(aes(x = avg_p_score, 
                     y = avg_s_score, 
                      label = stock_name), 
                  max.overlaps = 200, angle = 45, hjust = 0.6, size = 1.5)

setwd(pawd)
#ggsave(filename = 'PSA_NMFS_fish_spp_stock_name_FishLife.tiff',plot = last_plot() , path = pawd, width = 18, height = 9, device = 'tiff', dpi = 300)

####group by quadrant
#export list of stocks by quadrant
scores_red<- scores[,c(1,5,16,18,19)]

Q1<- as.data.frame(scores_red[scores_red$avg_p_score < 2 & scores_red$avg_s_score < 2,])
Q2<- as.data.frame(scores_red[scores_red$avg_p_score >= 2 & scores_red$avg_s_score < 2,])
Q3<- as.data.frame(scores_red[scores_red$avg_p_score < 2 & scores_red$avg_s_score >=2,])
Q4<- as.data.frame(scores_red[scores_red$avg_p_score >= 2 & scores_red$avg_s_score >=2,])

Q1[172:204,]<-NA
Q2[204:204,]<-NA
Q3[114:204,]<-NA
Q4[40:204,]<-NA

colnames(Q1)<-c('Q1','species_name', 'p_score','s_score','score')
colnames(Q2)<-c('Q2','species_name', 'p_score','s_score','score')
colnames(Q3)<-c('Q3','species_name', 'p_score','s_score','score')
colnames(Q4)<-c('Q4','species_name', 'p_score','s_score','score')

quadrants<-cbind.data.frame(c(Q1,Q2,Q3,Q4))

quadrants<-quadrants[,c('Q1','Q2','Q3','Q4')]

#write.csv(quadrants, "fish_stocks_by_quadrant_PSA.csv", row.names = F)

#now for species
colnames(Q1)<-c('stock_name','Q1', 'p_score','s_score','score')
colnames(Q2)<-c('stock_name','Q2', 'p_score','s_score','score')
colnames(Q3)<-c('stock_name','Q3', 'p_score','s_score','score')
colnames(Q4)<-c('stock_name','Q4', 'p_score','s_score','score')

quadrants<-cbind.data.frame(c(Q1,Q2,Q3,Q4))

quadrants<-quadrants[,c('Q1','Q2','Q3','Q4')]

#write.csv(quadrants, "fish_species_by_quadrant_PSA.csv", row.names = F)

##### link spp to their survey #########################################################################
#exclude non-FINSS surveys
surveys<-surveys[surveys[,2] == "FINSS Survey",] ####revist this to include F-D and state surveys. 
surveys_2<-surveys_2[surveys_2[,2] == "FINSS Survey",]

#fix formatting and merge primary and secondary
surveys<-surveys[,3:4]
colnames(surveys)<-c('survey','stock_name')

surveys_2<-surveys_2[,3:4]
colnames(surveys_2)<-c('survey','stock_name')

surveys<-rbind(surveys,surveys_2)

scores_survey<- left_join(scores,surveys,by = 'stock_name')

#summarize scores by survey; many NAs... go back here to check for misaligned stock names #############################################################################
scores_survey<-scores_survey[!is.na(scores_survey$survey),] 

str(scores_survey)

survey_grouped_scores<-scores_survey%>%
  group_by(survey)%>%
  summarise(p_score = mean(avg_p_score), s_score = mean(avg_s_score), n = n())

#classify n
survey_grouped_scores$spp_number<- survey_grouped_scores$n

survey_grouped_scores$spp_number[survey_grouped_scores$spp_number > 2]<-'3+'

#plot ######################################################################################################
#color by n in the future
ggplot(survey_grouped_scores, aes(x= p_score, y= s_score,label = survey)) + 
  geom_hline(yintercept=2, linetype="dashed", color = "red") +
  geom_vline(xintercept=2, linetype="dashed", color = "red")+
  geom_point(stat = "identity") + 
  theme_minimal() + labs(x = "Productivity", y = "Susceptibility") +
  geom_label_repel(aes(x = p_score, 
                       y = s_score, 
                       label = survey), 
                   max.overlaps = 200, angle = 45, hjust = 0.6, size = 1.5) +
  annotate(geom="text", x=1.25, y=1.15, label="Q1") + 
  annotate(geom="text", x=2.5, y=1.15, label="Q2") + 
  annotate(geom="text", x=1.25, y=2.5, label="Q3") + 
  annotate(geom="text", x=2.5, y=2.5, label="Q4")

setwd(pawd)
#ggsave(filename = 'PSA_fish_surveys_FishLife.tiff',plot = last_plot() , path = pawd, width = 18, height = 9, device = 'tiff', dpi = 300)

#get an average score
survey_grouped_scores$score<-apply(survey_grouped_scores[,c(2,3)],1,mean)

#write csv of survey scores
#write.csv(survey_grouped_scores, "fish_survey_grouped_PSA_scores.csv", row.names = F)

#export list of surveys by quadrant; some getting excluded here...
Q1<- as.data.frame(survey_grouped_scores[survey_grouped_scores$p_score < 2 & survey_grouped_scores$s_score < 2,])
Q2<- as.data.frame(survey_grouped_scores[survey_grouped_scores$p_score >= 2 & survey_grouped_scores$s_score < 2,])
Q3<- as.data.frame(survey_grouped_scores[survey_grouped_scores$p_score < 2 & survey_grouped_scores$s_score >=2,])
Q4<- as.data.frame(survey_grouped_scores[survey_grouped_scores$p_score >= 2 & survey_grouped_scores$s_score >=2,])

Q1[28:28,]<-NA
Q2[15:28,]<-NA
Q3[25:28,]<-NA
Q4[7:28,]<-NA

colnames(Q1)<-c('Q1','p_score','s_score','n','spp_num', 'score')
colnames(Q2)<-c('Q2','p_score','s_score','n','spp_num', 'score')
colnames(Q3)<-c('Q3','p_score','s_score','n','spp_num', 'score')
colnames(Q4)<-c('Q4','p_score','s_score','n','spp_num', 'score')

quadrants<-cbind.data.frame(c(Q1,Q2,Q3,Q4))

quadrants<-quadrants[,c('Q1','Q2','Q3','Q4')]

#write.csv(quadrants, "fish_surveys_by_quadrant_PSA.csv", row.names = F)

#### include FAP information and examine surveys that were excluded and look at logistics
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

write.csv(fap_comp,"FAP_PSA_rank_comparison.csv", row.names = F)
