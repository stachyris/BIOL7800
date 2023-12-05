library(rvertnet)
library(tidyverse)
library(dplyr)
library(lubridate)
library(countrycode)
library(stringr)
library(readr)
library(rvest)


## get the data2 using rvertnet
# bigsearch(class = "aves", rfile = "aves_vertnet_records", email = "vkl1@lsu.edu") 


## Loading the datasets
data1 <- read.csv("data/vertnet_latest_birds.csv", sep = "," , header = TRUE)


# data2 <- read.csv("../data/aves_vertnet_records-43452c1446904be6ba9641c86f891234.tsv", sep = "\t", header = TRUE)


# There seems to be some kind of disordinance with the rvertnet bigsearch - third request produced larger dataset - let's try and see how it pans out. 
data3 <- read.csv("data/vertnet_6mil_records-2ea2e413e6584682a68b08d3a56c8493.tsv", sep ="\t", header = TRUE)


## inspecting the datasets
head(data1)
summary(data1)
str(data1)

head(data3)
summary(data3)
str(data3)


## Column inspection 
#Checking for columns which are missing in data2
diff_cols_df1 <- setdiff(names(data1), names(data3))
cat("Columns in data1 not in data2: ", paste(diff_cols_df1, collapse = ", "), "\n")

## Checking for columns which are missing in data1
diff_cols_df3 <- setdiff(names(data3), names(data1))
cat("Columns in data2 not in data1: ", paste(diff_cols_df3, collapse = ", "), "\n")
diff_cols_df3


## Merge data
# Now that we know both the data sets have some missing columns, lets remove them so that they can be merged. 

common_cols <- intersect(names(data1), names(data3))

data1 <- data1[, common_cols]
#data2 <- data2[, common_cols]
data3 <- data3[, common_cols]

merged_df <- rbind(data1, data3)


## Retain Unique records checking for any duplicate records and keeping unique records since we merged two different source data sets
cleaned_df <- unique(merged_df)

# removing records which are all the fossilrecords
cleaned_df <- subset(cleaned_df, basisofrecord != "FossilSpecimen")

# removing some other records which are not bird specimen.
cleaned_df <- subset(
  cleaned_df, 
  !(collectioncode %in% c("eggs", "Bird eggs/nests", "Nest", "Birds-Eggs", "Fishes"))
)


## Removing Unnecessary rows

columns_to_check <- c("country", "order", "family") #Let's remove question marked rows from only certain columns - like country/family/order

cleaned_df <- cleaned_df[!rowSums(sapply(cleaned_df[columns_to_check], function(col) grepl("\\?", col))), ] # Writing function, applying and removing the rows

cleaned_df <- cleaned_df %>%
  filter(!grepl("^(\\d|\\()|\\[|\\.$", country)) %>% #to remove country column entries which starts with brackets and numbers and periods
  mutate(country = str_to_title(country)) %>%
  mutate(country = str_replace_all(country, "([A-Z])", " \\1")) %>% #lets get them all to same lettering style
  mutate(country = str_replace(country, "\\bI\\b|\\bIs\\b", "Islands")) # Almost all the islands were abbreviated as either I or Is, Let us now rename them as Islands


#combine and count the unique entries of countries, Will be using this df to cross check and manually validate the country names ~ have run this after every cleaning step w.r.t country name is conducted. 
country_counts <- cleaned_df %>%
  group_by(country) %>%
  summarise(count = n())


# Creating a list of all the 'country functions' to store the names and then looping over the column to replace the entries. 

# There's always gonna be a discrepancy on the total number of countries. I am following www.countrycode.org to keep things in check which has an ISO 3166-1 alpha-3 (https://en.wikipedia.org/wiki/ISO_3166-1_alpha-3) these includes Oceania and Island countries.

replace_functions <- list (
  # United States of America
  USA <- function(x) {
    if (grepl("United  States|	
United  States  Of  America|United  States  Minor  Outlying  Islands|Usa|The  United  States  Midway  Atoll ( Usa)| Alaska|  American   Samoa|  Baker   Island| Arctic  America| Commonwealth  Of  The  Northern  Mariana  Islands| Dmns  Grounds, 2001  Colorado  Blvd,  West  Atrium  Sidewalk| Great  Smoky  Mtns.,  Alt 1800';  Cades  Cove,  Tenn| Guam| Howland  Island| Johnston  Atoll| Key  Islands| Key  Islands's| Baker  Island| Wake  Island| Dawn  Roberts| Navassa  Island", x, ignore.case = TRUE)) {
      return("United States of America")
    } else {
      return(x)
    }
  },
  # Philipines
  PHL <- function(x) {
    if (grepl("Philippine| Philippine  Ids| Philippine  Islands| Philippine  Isls| Philippine's|Philippines| Philippines  Isl| Philippines  Islands|Phill| Phillipine  Islands| Phillipines| Philip|  Batan   Island| Batanta  Islands| Bongao  Islands| Filipines| Luzon| P.i| Palawan| Pi| S.  Mindanao| Batan  Island", x, ignore.case = TRUE)) {
      return("Philippines")
    } else {
      return(x)
    }
  },
  # Seychelles
  SYC <- function(x) {
    if (grepl(" Aldabra  Islands| Aldabrar  Islands| Sychelles  Islands| Seychelles  Isles| Seychelles| Seychelle's|Seychelles", x, ignore.case = TRUE)) {
      return("Seychelles")
    } else {
      return(x)
    }
  },
  #Antigua and Barbuda
  ATG <- function(x) {
    if (grepl(" Antigua| Antigua  And  Barbuda", x, ignore.case = TRUE)) {
      return("Antigua  And  Barbuda")
    } else {
      return(x)
    }
  },
  # Portugal
  PRT <- function(x) {
    if (grepl(" Azores  Archipelago;  Portugal| Madeira;  Portugal| Portugal| Azores", x, ignore.case = TRUE)) {
      return("Portugal")
    } else {
      return(x)
    }
  },
  #Bahamas
  BHS <- function(x) {
    if (grepl(" Bahama  Islands| Bahamas| Bahamas,  The", x, ignore.case = TRUE)) {
      return("Bahamas")
    } else {
      return(x)
    }
  },
  # Bhutan
  BTN <- function(x) {
    if (grepl(" Bhutan| Bhutan,  India", x, ignore.case = TRUE)) {
      return("Bhutan")
    } else {
      return(x)
    }
  },
  #Bosnia  And  Hercegovina
  BIH <- function(x) {
    if (grepl("  Bosnia   And   Hercegovina|  Bosnia   And   Herzegovina| Bosnia  And  Hercegovina| Bosnia  And  Herzegovina", x, ignore.case = TRUE)) {
      return("Bosnia  And  Herzegovina")
    } else {
      return(x)
    }
  },
  # British Overseas Territories
  IOT <- function(x) {
    if (grepl(" Anguilla|  Ascension   Island| Saint  Helena,  Ascension  And  Tristan  Da  Cunha| Bermuda| British  Virgin  Islands| Cayman  Islands| Grand  Cayman| Grand  Cayman  Islands| Falkland  Islands| Falkland  Islands ( Malvinas)| Falkland  Islands;  British  Overseas  Territories| Gilbert  Islands| Montserrat| Gough  Island;  St  Helena  Dependency,  Tristan  Da  Cunha  Island  Group;  British  Overseas  Territory| Saint  Helena| Saint  Helena,  Ascension  And  Tristan  Da  Cunha| St.  Helena| Tristan  Da  Cunha| Turks  And  Caicos  Islands| Caicos| Caicos  Islands| Gibraltar;  British  Overseas  Territories| Ascension  Island| Pitcairn| South  Georgia| South  Georgia  Island| Czechozlovakia", x, ignore.case = TRUE)) {
      return("British Overseas Territories")
    } else {
      return(x)
    }
  },
  # Spain
  ESP <- function(x) {
    if (grepl(" Canary  Islands| Canary  Islands;  Spain| Spain", x, ignore.case = TRUE)) {
      return("Spain")
    } else {
      return(x)
    }
  },
  #Indonesia
  IDN <- function(x) {
    if (grepl(" C.  Celebes|Celebes| Celebes  Islands| Celebres| Cent.  Celebes| Centr.  Celebes| Djampea  Islands. ( S.  Of  Celebes)| E  Celebes| E  Of  Celebes| E.  Celebes| E.  Celebes  Islands| E.  Of  Celebes| E.  Of  Celebes  Islands| E.of  Celebes| East  Celebes| East  Of  Celebes| N  Celebes| N.  Celebes| N.  Celebes  Islands| N.  E.of  Celebes| N.  Of  Celebes| N.celebes| N.e.  Celebes| N.of  Celebes| N.w.  Celebes| North  Celebes| Nw.  Celebes| S  Celebes| S.  Celebes| S.  Celebes  Islands| S.  Central  Celebes| S.  Of  Celebes| S.e.  Celebes| S.e.  Of  Celebes| S.e.  Of  Celebes  Islands| S.o.  Celebes| S.of  Celebes| S.w.  Celebes| Se  Celebes| Se.  Celebes| South  Celebes| South  Of  Celebes| Sula- Celebes  Islands| Sulu  Islands. ( N.  Celebes)|Celebes  And  Flores  Islands| Indonesia:  Sulawesi| Indonesia| Ceram| Ceram  Islands| Ceram  Laut| Ceram  Laut  Islands| Cent.  D.  New  Guinea| Cent.  D.n.  Guinea| Cent.  Du.  New  Guinea| Cent.  Dut.  New  Guinea| Cent.  Dutch  N.  Guinea| Cent.  Dutch  New  Guinea| Centr.  Dutch  New  Guinea| D.   New  Guinea| D.  New  Guina| Du  New  Guinea| Du.  New  Guiana| Du.  New  Guinea ( East  Coast)| Dutch  New  Guin| Dutch  New  Guinea| Centr.  Molucca's| Centr.  Moluccas| Brit.  D.  New  Guinea| C.  D.  New  Guinea| C.d.  New  Guinea| Centr.du.  New  Guinea| Du.  New  Guinea ( East  Coast)| E.  C.  D.  New  Guinea| E.  Centr.  Du.  New  Guinea| E.centr.du.  New  Guinea| N.w.  D.  New  Guinea| N.w.du.  New  Guinea| North  Coast  D.  New  Guinea| S.  Of  D.  New  Guinea| S.c.  D.  New  Guinea| S.centr.  Du.  New  Guinea| S.centr.du  New  Guinea| S.centr.du.  New  Guinea| W.  Of  D.  New  Guinea| W.  Of  Du.  New  Guinea| Aru  Islands| Batavia| E.  Java| E.  Java  Islands| East  Of  Java| Java| Java  Islands| N.  Java| N.  Of  Java| N.  Of  Java  Islands| N.e.  Java| N.e.  Of  Java| S.  Java| S.  Java  Islands| S.  Of  Java| W.  Java| W.  Java  Islands| West  Java| Lombok| Lombok  Islands| N.  Lombok| N.  Lombok  Islands|Banda  Islands| Alor  Islands| Banggai| Batjan  Islands| Biak  Islands| N.  Du.  New  Guinea| N.  Of  D.  New  Guinea| N.  Of  Du.  New  Guinea| N.  W.   D.  New  Guinea| N.du.  New  Guinea| Timor| Timor  Islands| Timor  Laut| Timor  Laut  Islands. =  Tenimber| Timor  Laut  Islands. =  Tenimeber| Timor  Laut [ =  Tenimber  Islands.]| Timor- Leste| Toekang  Besi  Islands| Tomia  Islands| Buru| Buru  Islands| Banda  Islands|Banda  Islands'| Between  Celebes  And  Flores  Islands| Between  Celebes &  Flores  Islands| Between  Flores  And  Celebes| West  Borneo| D.  East  Indies| D.e.  Ind| D.e.  Indies| Dammer  Islands| Djampea  Islands| Engano  Islands| Lesser  Sunda  Islands| Goram  Islands| Gorom  Islands| Gorom  Laut| Great  Natuna  Islands| Gt  Natuna  Islands| Gt.  Natuna  Islands| Gt.  Sangi  Islands| Gr.  Sangi| Gy.  Natuna  Islands| Halmahera| Jampea  Islands| Jobi  Islands| Kalao  Islands| Kangean  Islands| Kanjean  Islands| Lambok  Islands| Laoet  Islds| Bunguran  Islands| Lomblen| Molucca| Molucca's| N.  Molucca's| N.  Moluccas| Papua| S.  Molucca's| S.  Moluccas| South  Moluccas| Sula  Besi| Sula  Mangoli| Sumba| Sumbawa| Tenimber| West  Irian| W.  Kangean| D.  N.  Guinea| D.n.  Guinea| Neth.  N.  Guinea| Nias| Nias| S.e.  Of  N.  Guinea| S.w.  N.  Guinea", x, ignore.case = TRUE)) {
      return("Indonesia")
    } else {
      return(x)
    }
  },
  # Sri Lanka
  LKA <- function(x) {
    if (grepl(" Br.  India,  Ceylon| Central  Ceylon| Ceylon| Ceylon  Islands| N.  Ceylon| N.e.  Ceylon| N.w.  Ceylon| S.  Ceylon| S.e.  Coast  Of  Ceylon  Islands| South  Ceylon| W.  Ceylon| West  Ceylon| Sri  Lanka", x, ignore.case = TRUE)) {
      return("Sri Lanka")
    } else {
      return(x)
    }
  },
  # Japan
  JPN <- function(x) {
    if (grepl(" Central  Japan| E.  Of  Japan| Japan| Japan,| Japan,  Okinawa| Japan,  S.  Loo  Choo  Islands| N.  Japan| Off  Japan| S  Japan| S.  Japan| S.  Of  Japan| C.  Loo  Choo  Islands| Cent.  Loo  Choo  Islands| Cent.  Loo- Choo  Islands| Cent.  S.  Loo  Choo  Islands| Central  Loo  Choo  Islands| Central  N.  Loo  Choo  Islands| Japan,  S.  Loo  Choo  Islands| Loo  Choo  Islands| N  Loo  Choo  Islands| N.  Loo  Choo  Islands| N.  Loo  Chov  Islands| N.  Loo- Choo  Islands| N.  Loochoo  Islands| No.  S.  Loo  Choo  Islands| S.  Loo  Choo  Islands| S.  Loo- Choo  Islands| South  Loo  Choo  Islands| Ryukyu  Islands| So.  Ryukyus| Southern  Ryukyus| Bonin  Islands|  Bonin   Isle| Izu  Islands| Bonin  Isle| S.  End  Of  Okinawa  Shima", x, ignore.case = TRUE)) {
      return("Japan")
    } else {
      return(x)
    }
  },
  # Taiwan
  TWN <- function(x) {
    if (grepl(" Formosa,  Taiwan| Taiwan| C.  Formosa| Cent.  Formosa| Cent.  Formosa  Islands| Central  Formosa| Formosa  Islands| Formosa,  Taiwan| N  Formosa| N.  Formosa| S.  Formosa| S.w.  Formosa| South  Formosa", x, ignore.case = TRUE)) {
      return("Taiwan")
    } else {
      return(x)
    }
  },
  # Burma/Myanmar
  MMR <- function(x) {
    if (grepl(" Burma| Burma;  India| Burma;  Malaysia;  Thailand| Myanmar| Malacca| Tenasserim| N.e.burma", x, ignore.case = TRUE)) {
      return("Myanmar")
    } else {
      return(x)
    }
  },
  # Cape verde
  CPV <- function(x) {
    if (grepl(" Cape  Verde| Cape  Verde  Islands", x, ignore.case = TRUE)) {
      return("Cape Verde")
    } else {
      return(x)
    }
  },
  # Costa Rica
  CRI <- function(x) {
    if (grepl(" Cocos  Island ( Costa  Rica)| Costa  Rica| Cocos  Island ( Costa  Rica)| Cocos  Islands", x, ignore.case = TRUE)) {
      return("Costa Rica")
    } else {
      return(x)
    }
  },
  # Brunei
  BRN <- function(x) {
    if (grepl(" Brunei| Brunei  Darussalam| N.  Borneo", x, ignore.case = TRUE)) {
      return("Nation of Brunei")
    } else {
      return(x)
    }
  },
  # Colombia
  COL <- function(x) {
    if (grepl(" Colombia| Columbia| Colòmbia| San  Andres  And  Providencia", x, ignore.case = TRUE)) {
      return("Colombia")
    } else {
      return(x)
    }
  },
  #Norway
  NOR <- function(x) {
    if (grepl(" Centr.  Norway| N.  Norway| North  Norway| Norway| Norway;  Sweden;  Finland| Spitsbergen| Svalbard  And  Jan  Mayen| Svalbard", x, ignore.case = TRUE)) {
      return("Norway")
    } else {
      return(x)
    }
  },
  # Vietnam
  VNM <- function(x) {
    if (grepl(" Cochin  China| Cochin- China| Vietnam| Annam| Annan| Tonkin| Indochina| French  Indochina", x, ignore.case = TRUE)) {
      return("Vietnam")
    } else {
      return(x)
    }
  },
  # Papua New Guinea
  PNG <- function(x) {
    if (grepl("  Br   New   Guinea|  Br.   New   Guinea (  N.   Coast)| Brit.   New  Guinea| E.  Br.  New  Guinea| E.  Germ.  New  Guinea| East  Cent.  New  Guinea| Ger.  New  Guinea ( N.  Coast)| Ger.  New  Guinea,  N.  Coast| Germ.   New  Guinea| Germ.  D.  New  Guinea| Germ.  New  Guinea| German  Papua  New  Guinea| N.  Coast  Of  Ger.  New  Guinea| N.  Coast.  Of  Ger.  New  Guinea| N.  Germ.  New  Guinea| N.  Germ.  New  Guinea| N.br.  New  Guinea| N.e.  British  New  Guinea| N.e.br.  New  Guinea| N.w  New  Guinea| N.w.  Br.  New  Guinea| N.w.  Of   New  Guinea| N.w.  Of  New  Guinea| New  Guinea| Papua  New  Guinea| Papua,  New  Guinea| Rook  Islands. [ E.  Of  Ger.  New  Guinea]| S.e  New  Guinea| S.e.  Of   New  Guinea| S.e.  Of  Br.  New  Guinea| S.e.  Of  Brit.  New  Guinea| S.e.  Of  British  New  Guinea| S.e.  Of  New  Guinea| S.e.  Of  New  Guinea [ Louisiade  Archip.]| S.e.br.  New  Guinea| So.  Centr.  Du.  New  Guinea| South  East  Of  New  Guinea| W.  Of  New  Guinea| West  New  Guinea| Witu  Islands. [ N.e.  Of  New  Guinea]|  Bismarck   Arch|  Bismarck   Archip| Bismark  Arch| Bismark  Archip| Bismark| Islands| Admiralty| Admiralty  Group| Admiralty  Islands| Duke  Of  York  Islands| E.  Of  S.  New  Ireland| East  Of  New  Ireland| East  Of  S.  New  Ireland| East  Of  South  New  Ireland| New  Ireland| New  Ireland  Islands| S.e.  New  Ireland| S.e.  Of  New  Ireland| S.w.  New  Ireland| New  Britian  Islands| N.  Of  New  Hanover| New  Hanover| New  Hanover  Islands|  Br.   N.   Guinea| Louisiade  Arch| Louisiaide  Arch| Bismarck  Arch| Bismarck  Archip| N.  Coast  German  N.  Guinea| N.e.  Brit.  N.  Guinea| S.e.  Brit.  N.  Guinea| N.  Guinea| N.e.  Of  N.  Guinea| N.w.  N.  Guinea| N.w.  Of  N.  Guinea| Brit.   N.  Guinea| S.e.  N.  Guinea", x, ignore.case = TRUE)) {
      return("Papua New Guinea")
    } else {
      return(x)
    }
  },
  # New Zeland
  NZL <- function(x) {
    if (grepl(" Chatham  Islands;  New  Zealand| New  Zealand| The  Snares;  New  Zealand| Chatham  Islands| Tokelau|  Antipodes   Island| Antipodes  Islands| Antipodes  Islands. [ S.e.  Of  New  Zealand]| Antipodes  Islands;  New  Zealand|  Auckland   Island| Auckland  Island| Campbell  Island| Kermadec  Islands| Antipodes  Island", x, ignore.case = TRUE)) {
      return("New Zealand")
    } else {
      return(x)
    }
  },
  # India
  IND <- function(x) {
    if (grepl(" India| S.  India|  Andaman   And   Nicobar   Islands| Goa| Sikkim", x, ignore.case = TRUE)) {
      return("India")
    } else {
      return(x)
    }
  },
  # United Kingdom
  GBR <- function(x) {
    if (grepl(" England| United  Kingdom| Great  Britain| N.  Britain  Islands| N.  Of  New  Britain| Near  N.  New  Britain| New  Britain| New  Britain  Islands| Scotland| Wales| Northern  Ireland| South  Shetland| Southern  N.  Hebrides| St  Andrews| W.  Ireland| Irlanda| Ire", x, ignore.case = TRUE)) {
      return("United Kingdom")
    } else {
      return(x)
    }
  },
  # Australia
  AUS <- function(x) {
    if (grepl(" Cocos ( Keeling)  Islands| Christmas  Island| Australia| S.w.  Australia| Gold  Coast| Gold  Coast  Hinter| Gold  Coast  Hinterland| Hinterland  Gold  Coast| Hinterland  Of  Gold  Coast| Heard  Island  And  Mcdonald  Islands| Lord  Howe  Island| Macquarie  Island| Norfolk  Island", x, ignore.case = TRUE)) {
      return("Australia")
    } else {
      return(x)
    }
  },
  # Russia
  RUS <- function(x) {
    if (grepl(" Russia| Russian  Federation| N.  Russia| N  Russia| E.  Russia| E  Russia| S.  Russia| S.e.  Russia| So.  Russian  Altai| Commander  Islands| New  Siberia| N  Siberia| Pskov| East  Siberia| S  Siberia| S.e.  Siberia", x, ignore.case = TRUE)) {
      return("Russia")
    } else {
      return(x)
    }
  },
  # France
  FRA <- function(x) {
    if (grepl(" France| Territoire  Des  Terres  Australes  Et  Antarctiques  Françaises| França| Kerguelen  Islands;  France| N.  France| French  Guiana ( France)| N.  Coast  Of  France| N.w.  France| S.  France| S.e.  France| S.w.  France| French  Guiana| French  Southern  Territories| French  Guiana ( France)|  Amsterdam   Island| Clipperton  Island| New  Caledonia| Glorioso  Islands| Guadeloupe| Hautes  Pyrenees| Kerguelen  Islands| Martinique| Reunion| Réunion| St  Barthelemy| St  Bartholemew| Talant  Island| St.  Barthelemy| Amsterdam  Island", x, ignore.case = TRUE)) {
      return("France")
    } else {
      return(x)
    }
  },
  #Algeria
  DZA <- function(x) {
    if (grepl(" Algeria| N.  Algeria| S.  Algeria", x, ignore.case = TRUE)) {
      return("Algeria")
    } else {
      return(x)
    }
  },
  # Tonga
  TON <- function(x) {
    if (grepl(" Tonga| Alofa  Islands", x, ignore.case = TRUE)) {
      return("Tonga")
    } else {
      return(x)
    }
  },
  # French Polynesia
  PYF <- function(x) {
    if (grepl(" Austral  Islands| French  Polynesia| E.  Society  Islands| Tahiti  Arch| Tuamotu  Archipelago| Tuamotu", x, ignore.case = TRUE)) {
      return("French Polynesia")
    } else {
      return(x)
    }
  },
  # Venezuela
  VEN <- function(x) {
    if (grepl(" N.e.  Venezuela| Venezuela|  Aves   Island| Aves  Island", x, ignore.case = TRUE)) {
      return("Venezuela")
    } else {
      return(x)
    }
  },
  # China
  CHN <- function(x) {
    if (grepl(" C.  China| Centr.  China| China| E  China| E.  China| East  China| N.  China| N.e.  China| North  China| Off  S.  China| S.  China| S.  Of  China| S..  China| S.e  China| S.e.  China| S.w.  China| W  China| W.  China| Western  China| Hainan| Hainan  Islands| Manchuria| N.  Manchuria| Nord- Kansu| North  Manchuria| S.  Manchuria| S.  Mantchuria| Tang- Ter| Yunnan| Xina| N.  Tibet| N.e.  Tibet| Tibet| Thibet| N.w.  Fokien| N.  Tang.  Terr", x, ignore.case = TRUE)) {
      return("China")
    } else {
      return(x)
    }
  },
  # Greece
  GRC <- function(x) {
    if (grepl(" Greece| Greeks| Cerigo  Islands| Crete| Crete  Islands| Isl.  Of  Skyros| Macedonia", x, ignore.case = TRUE)) {
      return("Greece")
    } else {
      return(x)
    }
  },
  # Mauritius
  MUS <- function(x) {
    if (grepl(" Mauritius| Chagos  Archipelago", x, ignore.case = TRUE)) {
      return("Mauritius")
    } else {
      return(x)
    }
  },
  # Iceland
  ISL <- function(x) {
    if (grepl(" Coast  Of  N.  Iceland| Coast  Of  North  Iceland| Iceland| Iceland ( North)| N.  Iceland| N.e.  Iceland| No.  Iceland| North  Coast  Of  Iceland| North  Iceland", x, ignore.case = TRUE)) {
      return("Iceland")
    } else {
      return(x)
    }
  },
  # Comoros
  COM <- function(x) {
    if (grepl(" Comoro  Islands| Comoros", x, ignore.case = TRUE)) {
      return("Comoros")
    } else {
      return(x)
    }
  },
  # Ivory Coast
  CIV <- function(x) {
    if (grepl(" Cote  D'ivoire| Côte  D'ivoire| Ivory  Coast", x, ignore.case = TRUE)) {
      return("Ivory Coast")
    } else {
      return(x)
    }
  },
  # Ukraine
  UKR <- function(x) {
    if (grepl(" Ukraine| Crimea", x, ignore.case = TRUE)) {
      return("Ukraine")
    } else {
      return(x)
    }
  }, 
  # Libya
  LBY <- function(x) {
    if (grepl(" Libya| N.  Libya| Cyrenaica| Tripoli", x, ignore.case = TRUE)) {
      return("Libya")
    } else {
      return(x)
    }
  },
  # Poland
  POL <- function(x) {
    if (grepl(" N.  Of  Poland| Poland| Danzig  F.s", x, ignore.case = TRUE)) {
      return("Poland")
    } else {
      return(x)
    }
  },
  # DEMOCRATIC REPUBLIC OF THE CONGO
  COD <- function(x) {
    if (grepl(" Democratic  Republic  Congo| Democratic  Republic  Of  Congo| Democratic  Republic  Of  The  Congo| Congo  F.  State| Congo  Free  State| Congo /  Congo,  Dem.  Rep.  Of  The| Congo,  Dem.  Rep.  Of  The| Congo,  Democratic  Republic  Of| Zaire| W.  Congo| Bel.  C| Bel.  Con| Belg.  Congo| Eastern  Congo", x, ignore.case = TRUE)) {
      return("Democratic  Republic  Of  The  Congo")
    } else {
      return(x)
    }
  },
  # Dominica
  DMA <- function(x) {
    if (grepl(" Dominica| W.  Indies,  Dominica  Islands", x, ignore.case = TRUE)) {
      return("Dominica")
    } else {
      return(x)
    }
  },
  # Dominican Republic
  DOM <- function(x) {
    if (grepl(" Dominican  Rep| Dominican  Republic| Santo  Domingo| Sto.  Domingo| W.  Indies,  San  Domingo", x, ignore.case = TRUE)) {
      return("Dominican Republic")
    } else {
      return(x)
    }
  }, 
  # Nigeria
  NGA <- function(x) {
    if (grepl(" N  Nigeria| N'ern  Nigeria| N.  Nigeria| Nigeria| No.  Nigeria| Northern  Nigeria| S  Nigeria| S'ern  Nigeria| S.  Nigera| S.  Nigeria| So  Nigeria| So.  Nigeria| South  Nigeria| Southern  Nigeria| W.  Nigeria| N.w.  Africa", x, ignore.case = TRUE)) {
      return("Nigeria")
    } else {
      return(x)
    }
  },
  #  Vanuatu
  VUT <- function(x) {
    if (grepl(" Vanuatu| New  Hebrides  Islands| Southern  New  Hebrides", x, ignore.case = TRUE)) {
      return("Vanuatu")
    } else {
      return(x)
    }
  },
  # Solomon Islands
  SLB <- function(x) {
    if (grepl(" N.  Of  Solomon  Islands| S.  Solomon  Islands| Solomen  Islands| Solomon  Islands| Solomon  Islands:  New  Georgia  Group| Solomon's| Solomon's  Islands| Solomons| W.  Ofsolomon  Islands", x, ignore.case = TRUE)) {
      return("Solomon Islands")
    } else {
      return(x)
    }
  },
  # Chile
  CHL <- function(x) {
    if (grepl(" Easter  Island| Chile| Juan  Fernandez  Islands| Rapa  Island| San  Ambrosio  Island| San  Felix  Island", x, ignore.case = TRUE)) {
      return("Chile")
    } else {
      return(x)
    }
  },
  # Faroe Islands
  FRO <- function(x) {
    if (grepl(" Faeroe  Islands| Faeroe  Islands;  Denmark| Faroe  Islands", x, ignore.case = TRUE)) {
      return("Faroe Islands")
    } else {
      return(x)
    }
  },
  # Micronesia
  FSM <- function(x) {
    if (grepl(" Federated  States  Of  Micronesia| Micronesia| Caroline  Islands| U.s.  Trust  Territory  Of  The  Pacific", x, ignore.case = TRUE)) {
      return("Micronesia")
    } else {
      return(x)
    }
  },
  # Fiji
  FJI <- function(x) {
    if (grepl(" Fiji  Islands| Fiji", x, ignore.case = TRUE)) {
      return("Fiji")
    } else {
      return(x)
    }
  },
  # Finland
  FIN <- function(x) {
    if (grepl(" Finland| Finnland| Lapland", x, ignore.case = TRUE)) {
      return("Finland")
    } else {
      return(x)
    }
  },
  #Ecuador
  ECU <- function(x) {
    if (grepl(" Ecuador| Galapagos  Archipelago;  Ecuador| Galapagos  Islands|Equador", x, ignore.case = TRUE)) {
      return("Ecuador")
    } else {
      return(x)
    }
  },
  # Malaysia
  MYS <- function(x) {
    if (grepl(" Gt.  Redang  Islands| Malaysia| Malay| Malay  Pen| Malay  Penin| Malay  Penins| Malay  Peninsula| Malaya| N.  Malay  Penin| N.  Malay  Penins| N.  Malay  Peninsular| N.e.  Malay  Pen| N.w.  Malay  Penins| No.  Malay  Penin| Siam,  Malay  Peninsula| Siamese  Malay  Pen| Siamese -  Malay  Peninsula| No.  Malay  Penin", x, ignore.case = TRUE)) {
      return("Malaysia")
    } else {
      return(x)
    }
  },
  # Grenada
  GRD <- function(x) {
    if (grepl(" Grenada| Grenadines| Grenada  Or  Barbados", x, ignore.case = TRUE)) {
      return("Grenada")
    } else {
      return(x)
    }
  },
  # Guatemala
  GTM <- function(x) {
    if (grepl(" Guatemala| Guatemala,  Mexico", x, ignore.case = TRUE)) {
      return("Guatemala")
    } else {
      return(x)
    }
  },
  # Germany
  DEU <- function(x) {
    if (grepl(" Germany| Heligoland", x, ignore.case = TRUE)) {
      return("Germany")
    } else {
      return(x)
    }
  },
  # Netherlands
  NLD <- function(x) {
    if (grepl(" Netherland| Netherlands| The  Netherlands| Holland|  Aruba| Saba| Sint  Maarten| St  Eustatius| St.  Martin| St.  Eustatius| Dutch  West  Indies", x, ignore.case = TRUE)) {
      return("Netherlands")
    } else {
      return(x)
    }
  },
  # Austria
  AUT <- function(x) {
    if (grepl(" Austria| Karinthia", x, ignore.case = TRUE)) {
      return("Austria")
    } else {
      return(x)
    }
  },
  # Kazakhstan
  KAZ <- function(x) {
    if (grepl(" Kazakatan| Kazakhstan| Kazakstan", x, ignore.case = TRUE)) {
      return("Kazakhstan")
    } else {
      return(x)
    }
  },
  # Sudan
  SDN <- function(x) {
    if (grepl(" Sudan| Port  Sudan| Khartum| Soudan", x, ignore.case = TRUE)) {
      return("Sudan")
    } else {
      return(x)
    }
  },
  # Mali
  MLI <- function(x) {
    if (grepl(" Mali| French  Sudan| Fr.  W.  Africa,  Sudan", x, ignore.case = TRUE)) {
      return("Mali")
    } else {
      return(x)
    }
  },
  # Kenya
  KEN <- function(x) {
    if (grepl(" Kikuyu| Kenya", x, ignore.case = TRUE)) {
      return("Kenya")
    } else {
      return(x)
    }
  },
  # Kyrgyzstan
  KGZ <- function(x) {
    if (grepl(" Kyrgyz  Republic| Kyrgyzstan", x, ignore.case = TRUE)) {
      return("Kyrgyzstan")
    } else {
      return(x)
    }
  },
  # Romania
  ROU <- function(x) {
    if (grepl(" E.  Romania| Romani| Romania| Roumania| Romina| Transylvania| S.  Rumania", x, ignore.case = TRUE)) {
      return("Romania")
    } else {
      return(x)
    }
  },
  # Sweden
  SWE <- function(x) {
    if (grepl(" E.  Sweden| N.  Sweden| S.  Sweden| S.w.  Sweden| Sweden| Suècia", x, ignore.case = TRUE)) {
      return("Sweden")
    } else {
      return(x)
    }
  },
  # Egypt
  EGY <- function(x) {
    if (grepl(" Egypt| Upper  Egypt| Sinai| Sinai  Peninsula", x, ignore.case = TRUE)) {
      return("Egypt")
    } else {
      return(x)
    }
  },
  # El Salvador
  SLV <- function(x) {
    if (grepl(" El  Salvador| Salvador", x, ignore.case = TRUE)) {
      return("El Salvador")
    } else {
      return(x)
    }
  },
  #Zambia
  ZMB <- function(x) {
    if (grepl("  Zambia|  Barotseland| Zambia| Barotseland", x, ignore.case = TRUE)) {
      return("Zambia")
    } else {
      return(x)
    }
  },
  # Panama
  PAN <- function(x) {
    if (grepl("  Bay   Of   Panama|  Panama| Panama|Panama", x, ignore.case = TRUE)) {
      return("Panama")
    } else {
      return(x)
    }
  },
  # Random entries like sex, entire continent etc which will be removed later
  RANDOM <- function(x) {
    if (grepl(" A2ef6dd1-8886-48c9-8025- C62bac973cc7| Asia| Bolivia /  Peru| Captive| Female| Lesser  Antilles| Locailty  Unknown| Locality  Unknown| Male| N.g| No  Data| Not  Recorded| Atlantic  Ocean| Australasia| Bengal| Central  America| World  Geodetic  System 1984| Wgs84| South  Atlantic| Soviet  Union| U.s.s.r| Ussr| B.  Con| B.c| Cong| East  Indies| Eastern  Pacific| Europe| Ger| French  Equatorial  Africa| Unknown| Br.  N.  Guinea| Brit  Cent.  Africa| Brit.  Cent.  Protectorate  Africa| Brit.  Centr.  Africa| N.  Siberia| North  Pacific  Ocean| Portugese  Africa| Br.  C.  Africa| Br.  Centr  Africa| North  Africa| S.e.  Guinea| St.  Matthias| Rimgwe| Ind| Sumbo| British  Central  Africa| East  Africa| North  America| South  America| Czechoslovakia| Yugoslavia", x, ignore.case = TRUE)) {
      return("Random")
    } else {
      return(x)
    }
  },
  # Kiribati
  KIR <- function(x) {
    if (grepl(" Kiribati| Line  Island", x, ignore.case = TRUE)) {
      return("Kiribati")
    } else {
      return(x)
    }
  },
  # Morocco
  MAR <- function(x) {
    if (grepl(" Morocco| N.w.  Morocco| W.  Morocco| Magador| Marroc| Western  Sahara", x, ignore.case = TRUE)) {
      return("Morocco")
    } else {
      return(x)
    }
  },
  # Nicaragua
  NIC <- function(x) {
    if (grepl(" Nicarag| Nicaragua Nicaragua [ Northern  Nicaragua]| Nicaraqua| Nicaraugua| Northern  Nicaragua", x, ignore.case = TRUE)) {
      return("Nicaragua")
    } else {
      return(x)
    }
  },
  # Malawi
  MWI <- function(x) {
    if (grepl(" Nyasaland| Nyassaland| Nyssaland| Malawi| Brit.  Cent.  Africa| Br.centr.  Africa| Br.  Centr.  Africa", x, ignore.case = TRUE)) {
      return("Malawi")
    } else {
      return(x)
    }
  },
  # Thailand
  THA <- function(x) {
    if (grepl(" S.  Thailand| Thailand| P.  Siam| S.e.  Siam| S.w.  Siam| Frontier  Of  Siam| Siam| Siam,  Malay  Peninsula| Siamese  Malay  Pen| Siamese -  Malay  Peninsula| Syd  Siam", x, ignore.case = TRUE)) {
      return("Thailand")
    } else {
      return(x)
    }
  },
  # Puerto Rico
  PRI <- function(x) {
    if (grepl(" Puerte  Rico| Puerto  Rico| Puerto  Rio", x, ignore.case = TRUE)) {
      return("Puerto Rico")
    } else {
      return(x)
    }
  },
  # Zimbabwe
  ZWE <- function(x) {
    if (grepl(" Zimbabwe| Rhodesia", x, ignore.case = TRUE)) {
      return("Zimbabwe")
    } else {
      return(x)
    }
  },
  # Rwanda
  RWA <- function(x) {
    if (grepl(" Rwanda| Ruanda| Rwanda", x, ignore.case = TRUE)) {
      return("Rwanda")
    } else {
      return(x)
    }
  },
  # Tanzania
  TZA <- function(x) {
    if (grepl(" Tanzania| Rungwe| Rungwe  Mission| Tanganyika| Tanganyika  Terr| Tanganyka  Terr| Tanguinyika| Tanzania| Zansibar  Insel| Tukuyu| Tukyu| Kilimanjaro| German  E.  Africa| German  E  Africa| Germ.  E.  Africa| Germ  E.  Africa| Ger.  E.  Africa| Sansibar| Brit.  E.  Africa", x, ignore.case = TRUE)) {
      return("Tanzania")
    } else {
      return(x)
    }
  },
  # Saint Kitts and Nevis
  KNA <- function(x) {
    if (grepl(" Saint  Kitts  And  Nevis| St  Kitts| St.  Kitts  And  Nevis", x, ignore.case = TRUE)) {
      return("Saint Kitts and Nevis")
    } else {
      return(x)
    }
  },
  # Sierre Leone
  SLE <- function(x) {
    if (grepl(" Sierra  Leone| Sierra  Leone  Penin| Sierra  Leone  Peninsula| Sierre  Leone| Peninsula  Of  Sierra  Leone| S.e.  Sierra  Leone| S.  Leone", x, ignore.case = TRUE)) {
      return("Sierre Leone")
    } else {
      return(x)
    }
  },
  # Saint Lucia
  LCA <- function(x) {
    if (grepl(" St  Lucia| St.  Lucia| Saint  Lucia", x, ignore.case = TRUE)) {
      return("Saint Lucia")
    } else {
      return(x)
    }
  },
  #  Saint Pierre  And  Miquelon
  SPM <- function(x) {
    if (grepl(" St.  Pierre  And  Miquelon", x, ignore.case = TRUE)) {
      return("Saint Pierre And Miquelon")
    } else {
      return(x)
    }
  },
  # Saint Vincent and The Grenadines
  VCT <- function(x) {
    if (grepl(" St  Vincent| St.  Vincent", x, ignore.case = TRUE)) {
      return("Saint Vincent and The Grenadines")
    } else {
      return(x)
    }
  },
  # Switzerland
  CHE <- function(x) {
    if (grepl(" Switz| Switzerland| Switzland| Suisse", x, ignore.case = TRUE)) {
      return("Switzerland")
    } else {
      return(x)
    }
  },
  # Suriname
  SUR <- function(x) {
    if (grepl(" Surinam| Suriname", x, ignore.case = TRUE)) {
      return("Suriname")
    } else {
      return(x)
    }
  },
  # Syria
  SYR <- function(x) {
    if (grepl(" Syria| Syrian  Arab  Republic", x, ignore.case = TRUE)) {
      return("Syria")
    } else {
      return(x)
    }
  },
  # Nepal
  NPL <- function(x) {
    if (grepl(" E.  Border  Nepal| E.  Nepal| Nepal| S.w.  Nepal| W.  Nepal| E.  Napal", x, ignore.case = TRUE)) {
      return("Nepal")
    } else {
      return(x)
    }
  },
  # South Africa
  ZAF <- function(x) {
    if (grepl(" South  Africa| South  W.  Africa| Zululand| S.  Africa| S.e.  Africa", x, ignore.case = TRUE)) {
      return("South Africa")
    } else {
      return(x)
    }
  },
  # Samoa
  WSM <- function(x) {
    if (grepl(" Western  Samoa| Samoa", x, ignore.case = TRUE)) {
      return("Samoa")
    } else {
      return(x)
    }
  },
  # Pakistan
  PAK <- function(x) {
    if (grepl(" Pakistan| West  Pakistan", x, ignore.case = TRUE)) {
      return("Pakistan")
    } else {
      return(x)
    }
  },
  # Uganda
  UGA <- function(x) {
    if (grepl(" N.w.  Uganda| S.w.  Uganda| Toro  Uganda| Ugand| Uganda| Uganda  Colony| W.  Uganda| Unyoro", x, ignore.case = TRUE)) {
      return("Uganda")
    } else {
      return(x)
    }
  },
  # Mongolia
  MNG <- function(x) {
    if (grepl(" Inner  Mongolia| Mongolia| N.  Mongolia| N.w.  Mongolia| W.  Mongolia", x, ignore.case = TRUE)) {
      return("Mongolia")
    } else {
      return(x)
    }
  },
  # Antarctica
  ATA <- function(x) {
    if (grepl(" Antarctica| Victoria  Land", x, ignore.case = TRUE)) {
      return("Antarctica")
    } else {
      return(x)
    }
  },
  # Brazil
  BRA <- function(x) {
    if (grepl(" Brazil| Trindade  And  Martin  Vas| Fernando  De  Noronha  And  Rocas", x, ignore.case = TRUE)) {
      return("Brazil")
    } else {
      return(x)
    }
  },
  # Iran
  IRN <- function(x) {
    if (grepl(" Iran| Southeastern  Iran ( Kirman)", x, ignore.case = TRUE)) {
      return("Iran")
    } else {
      return(x)
    }
  },
  # Yemen
  YEM <- function(x) {
    if (grepl(" South  Yemen| Yemen| Socotra", x, ignore.case = TRUE)) {
      return("Yemen")
    } else {
      return(x)
    }
  },
  # South Korea
  KOR <- function(x) {
    if (grepl(" Korea| S.e.  Korea| South  Korea", x, ignore.case = TRUE)) {
      return("South Korea")
    } else {
      return(x)
    }
  },
  # North Korea
  PRK <- function(x) {
    if (grepl(" North  Korea", x, ignore.case = TRUE)) {
      return("North Korea")
    } else {
      return(x)
    }
  },
  # Latvia
  LVA <- function(x) {
    if (grepl(" Estonia;  Latvia,  Lithuania| Latvia| Livland", x, ignore.case = TRUE)) {
      return("Latvia")
    } else {
      return(x)
    }
  },
  # Jamaica
  JAM <- function(x) {
    if (grepl(" Jamaica| Jamaica,  West  Indies", x, ignore.case = TRUE)) {
      return("Jamaica")
    } else {
      return(x)
    }
  },
  # Niger
  NER <- function(x) {
    if (grepl(" Fr.  West  Africa| French  West  Africa", x, ignore.case = TRUE)) {
      return("Niger")
    } else {
      return(x)
    }
  },
  # Hungary
  HUN <- function(x) {
    if (grepl(" Hungary| S.  Hungary", x, ignore.case = TRUE)) {
      return("Hungary")
    } else {
      return(x)
    }
  },
  # Italy
  ITA <- function(x) {
    if (grepl(" Italy| Sardinia", x, ignore.case = TRUE)) {
      return("Italy")
    } else {
      return(x)
    }
  },
  # Cyprus
  CYP <- function(x) {
    if (grepl(" Republic  Of  Cyprus| Cyprus", x, ignore.case = TRUE)) {
      return("Cyprus")
    } else {
      return(x)
    }
  },
  # Canada
  CAN <- function(x) {
    if (grepl(" Canada| North  American  Datum 1927", x, ignore.case = TRUE)) {
      return("Canada")
    } else {
      return(x)
    }
  },
  # Mozambique
  MOZ <- function(x) {
    if (grepl(" North  Mozambique| N.  Mozambique| Mozambique| Portuguese  E.  Africa", x, ignore.case = TRUE)) {
      return("Mozambique")
    } else {
      return(x)
    }
  },
  # Guyana
  GUY <- function(x) {
    if (grepl(" Guyana| British  Guiana", x, ignore.case = TRUE)) {
      return("Guyana")
    } else {
      return(x)
    }
  },
  # Gambia
  GMB <- function(x) {
    if (grepl(" The  Gambia| Gambia", x, ignore.case = TRUE)) {
      return("Gambia")
    } else {
      return(x)
    }
  },
  # Equatorial Guinea
  GNQ <- function(x) {
    if (grepl(" Equatorial  Guinea| Guinea  Equatorial", x, ignore.case = TRUE)) {
      return("Equatorial Guinea")
    } else {
      return(x)
    }
  },
  # Croatia
  HRV <- function(x) {
    if (grepl(" Croatia| Serbia  And  Montenegro", x, ignore.case = TRUE)) { #  Serbia  And  Montenegro adding here as the 'highergeography' lists Dalmatia as location which is currently in Croatia
      return("Croatia")
    } else {
      return(x)
    }
  },
  # Somalia - Combining all of somalia including Somaliland and other colonical classification
  SOM <- function(x) {
    if (grepl(" Br.  Somaliland| Brit  Somaliland| Brit.  Somalailand| Brit.  Somaliland| British  Somaliland| Fr.  Somaliland| N.  Somaliland| N.e.  British  Somaliland| N.w.  Somaliland| North  Somaliland| S.  Somaliland| Somalia| Somaliland| W.  Somaliland| Western  Italian  Somaliland| British  Somililand| Italian  Somililand", x, ignore.case = TRUE)) {
      return("Somalia")
    } else {
      return(x)
    }
  }
)

for (i in seq_along(replace_functions)) {
  cleaned_df$country <- sapply(cleaned_df$country, replace_functions[[i]])
}


#to count whether unique entries reducing or not. 
country_counts_check <- cleaned_df %>%
  group_by(country) %>%
  summarise(count = n())

# Let's remove that space starting before names in all of country cells and removing double spaces if any
cleaned_df$country <- sub("^\\s+", "", cleaned_df$country)
cleaned_df$country <- gsub("\\s+", " ", cleaned_df$country)


# There's empty entries - let's add "N/A" for now
cleaned_df$country <- ifelse(is.na(cleaned_df$country) | cleaned_df$country == "", "N/A", cleaned_df$country)


# Assigning country codes to each entry
cleaned_df$countrycode <- countrycode(
  sourcevar = cleaned_df$country,
  origin = "country.name",
  destination = "iso3c"
)

# Since it takes forever to run the entire replace_country function I am creating a sub-function to order some of the ambiguities I found after first round of cleaning. 

replace_functions_2 <- list (
  
  RANDOM <- function(x) {
    if (grepl("Africa|Pacific Ocean|Polynesia|West Africa|Turkestan|Borneo|Melanesia", x, ignore.case = TRUE)) {
      return("Random")
    } else {
      return(x)
    }
  },
  # Senegal
  SEN2 <- function(x) {
    if (grepl("Senagal|Senegal", x, ignore.case = TRUE)) {
      return("Senegal")
    } else {
      return(x)
    }
  },
  # British Overseas Territories
  GBR2 <- function(x) {
    if (grepl("British Overseas Territories", x, ignore.case = TRUE)) {
      return("United Kingdom")
    } else {
      return(x)
    }
  },
  # Sierra Leone
  SLE2 <- function(x) {
    if (grepl("Sierre Leone", x, ignore.case = TRUE)) {
      return("Sierra Leone")
    } else {
      return(x)
    }
  }, 
  # Ecuador
  ECU2 <- function(x) {
    if (grepl("Equador|Ecuador", x, ignore.case = TRUE)) {
      return("Ecuador")
    } else {
      return(x)
    }
  }
)

for (i in seq_along(replace_functions_2)) {
  cleaned_df$country <- sapply(cleaned_df$country, replace_functions_2[[i]])
}

# Re-running the same code as above to check if the ambiguity warnings are resolved beyond "N/A" "Random"
cleaned_df$countrycode <- countrycode(
  sourcevar = cleaned_df$country,
  origin = "country.name",
  destination = "iso3c"
)

# For some reason Micronesia is not being recognized - so manually adding country code to Micronesia

cleaned_df <- cleaned_df %>% mutate(countrycode = ifelse(country == 'Micronesia', 'FSM', countrycode))

#to check last one time 
country_counts_check <- cleaned_df %>%
  group_by(country) %>%
  summarise(count = n())
# Finally it's done - 190 countries

# Some entries have continent data and some dont, Let's fill them up using contrycode package. 

cleaned_df$continent <- countrycode(cleaned_df$country, "country.name", "region")




## Dealing with dates
cleaned_df$eventdate <- as.Date(cleaned_df$eventdate) # Removing all the non standard and ambiguous dates/year


# Since the month and date is not really of any significance in this case - separating year from the eventdate and saving in a different column. 
cleaned_df$year <- gsub(".*([0-9]{4}).*", "\\1", cleaned_df$year)
cleaned_df$year <- as.numeric(cleaned_df$year)

# Displaying individual years is not visually pleasing - so binning them into 5 years range
bin_width <- 5
year_range <- seq(1600, 2023, bin_width)

cleaned_df$YearBin <- cut(cleaned_df$year, breaks = year_range, labels = year_range[1:(length(year_range)-1)])



# Let's deal with the taxon names. 
# Dataframe has Order, Family, Genus and Speceus names. That should be sufficient for our purpose. 

# Let us first remove any records where either genus or species is missing - if anything else is missing we can fill them up. 
cleaned_df <- cleaned_df[complete.cases(cleaned_df$genus, cleaned_df$specificepithet), ]

# removing anything other than `genus` and `species` name from the records
cleaned_df$specificepithet <- gsub("\\s+[A-Za-z]+$", "", cleaned_df$specificepithet)

cleaned_df[!grepl("\\s+[Ss][Pp]$", cleaned_df$specificepithet), ]



# Order names have several issues - let's get rid of them
bird_orders <- c("Struthioniformes","Rheiformes","Tinamiformes","Casuariiformes","Apterygiformes","Anseriformes","Galliformes","Phoenicopteriformes","Podicipediformes","Columbiformes","Mesitornithiformes","Pterocliformes","Otidiformes","Musophagiformes","Cuculiformes","Caprimulgiformes","Opisthocomiformes","Gruiformes","Charadriiformes","Eurypygiformes","Phaethontiformes","Gaviiformes","Sphenisciformes","Procellariiformes","Ciconiiformes","Suliformes","Pelecaniformes","Cathartiformes","Accipitriformes","Strigiformes","Coliiformes","Leptosomiformes","Trogoniformes","Bucerotiformes","Coraciiformes","Galbuliformes","Piciformes","Cariamiformes","Falconiformes","Psittaciformes","Passeriformes")

cleaned_df <- cleaned_df %>%
  filter(grepl(paste(bird_orders, collapse = "|"), order))



# Let's deal with Family names

# For all the taxonomy, we are following IOC list and now pulling Family lists from IOC website. 
url <- "https://www.worldbirdnames.org/new/classification/family-index-2/"

webpage <- read_html(url) # reading the webpage
family_column <- html_table(webpage)#extracting table
family_column <- as.data.frame(family_column) # converting to df
family_column <- family_column[, -1] # removing common/english names
family_column <- as.data.frame(family_column) # converting to df
family_column # checking
colnames(family_column)[1] <- "familynames"


# Now remove ambigious family names

#family_values <- cleaned_df$family # listing all the family entries from the main dataframe

cleaned_df <- cleaned_df[cleaned_df$family %in% family_column$familynames, , drop = FALSE]
#matching and removing the ones from the IOC list

# Seems like there are few double entries as a second part and some "species" name. Let's remove all of them. 

cleaned_df$family <- sapply(strsplit(as.character(cleaned_df$family), " "), function(x) x[1])




#let's write this as a working data
# Selecting only certain columns to write to the cleaned df

columns_to_retain <- c("continent", "country", "countrycode", "decimallatitude", "decimallongitude","eventdate","year","collectioncode","scientificname","kingdom","phylum","class","order","family","genus","specificepithet","hastissue","YearBin")

vert_clean_df <- cleaned_df[columns_to_retain]

file_path <- "data/cleaned_vertnet.csv"

# Check if the file exists
if (file.exists(file_path)) {
  # Remove the existing file
  file.remove(file_path)
}

# Write the new CSV file
write.csv(vert_clean_df, file_path, row.names = FALSE)



