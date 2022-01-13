# --------------------
# Babine Lake scale data, initial analyses
# Celeste Kieran
# 29 July 2021
# --------------------
rm(list=ls())
getwd()

## Latest full babine dataframe
babine <- read.csv("babine_updated_8oct2021.csv")
pdo.iso <- read.csv("pdo.iso.merge.11oct2021.csv")
# merged
full <- read.csv("very.full.babine.df.12oct2021.csv")


# Here we have isotope data and partial biodata for 167 Babine Lake Sockeye salmon from 1913 - 2014
# Isotopic measurement errors are: 
# d13C +/- 0.1
# d15N +/- 0.2
# d34S +/- 0.5 per mil (all at 1 s.d.)
# 155 valid measurements out of 165 samples


#### Checklist ####

### TO DO ## 
## Check with (x) and date when done


# Re-make data frame carefully (x) July 2021
# take care of date inconsistincies (x) July 2021
# Add Suess effect
# Re-run all simple interaction figures with new data, make a fresh document detailing what our data is, basic interactions
# Run some simple models from another paper (Jon's wife or other)
# Find other data variables needed (SST etc), run models
# Figure out Bayesian ellipses and how we will model our niches


##EXTRA:
# Compare all populations from Mike P's biodata, comparing historic and modern marine vs. 
# freshwater growth rates (check "notes on future" for details)

####


#### Switch units of main dataframe oct 8th 2021 ####
# in my first go of everything before I used the dataframe ("babine_bio_iso_suess3Aug2021.csv"), which has inches and lbs, here I will just switch units to cm and g

babine <- read.csv("babine_bio_iso_suess_dates_30sep2021.csv")  # same as august df but I manually fixed date formats

names(babine)

# change units and reorder columns
inch <- function(i) {
  return(i*2.54)
}

lbs <- function(t) {
  return(t*453.59237)
}

babine1 <- babine %>% 
  dplyr::mutate(Length.cm = inch(Length.inches)) %>% 
  dplyr::mutate(Weight.g = lbs(Weight.lbs)) %>% 
  dplyr::select(Year, Fish.Code, Length.cm, Weight.g, Sex, Age, d13C = dC13, Suess.d13C = Suess.dC13, d15N = dN15, d34S = dS34, FW1.GW, FW2.GW, SW1.GW, SW2.GW, SW3.GW, SW4.GW, edge.growth, Date.Caught.Approx, Date.actual.notes, everything()) %>% 
  dplyr::select(!c(Length.inches, Weight.lbs, b))
  

#write.csv(babine1, "babine_updated_8oct2021.csv", row.names = FALSE)


#### The making of my  functional data frame (w/out Suess) july 22 2021####

# So this is the making of my data frame from a previous script, with all the notes included, I then modified this to include the Suess correction. We eventually merged everything, without duplicating or losing fish, but 10 fish did not exist in Mike's full biodata set so we don't have data for them. we can address this in the future moving on. SKIP TO PART 2 to avoid all the mucking around and talking through problems

## 3rd try new fish code ##
# So here is our issue, neither file has complete unique fish numbers, the isotope one doesnt have weight or length data for most of the fish. So our unique fish code needs to be just scale book number + fish number, except for 1968, which has multiple duplicate fish for one scale book.  All we need to do is get these fricken dataframes together, if the fish code is clunky it doesn't matter! Just no duplicates is what we want. Doesnt need to be replicable for future codes, we can rename them. 

# START HERE JULY 22
# Ok
rm(list=ls())


# Mike P's full biodata, for babine
full_scale_data <- read.csv("Full_Biodata_Babine_FromSkeena_13april2021.csv")

biodata <- full_scale_data %>% 
  dplyr::select(Year = year, Fish.No, Population, Proportion, Date.Caught = Date.Caught.mm.dd., Length.inches = Length..inches...1.4.s., Weight.lbs = Weight..lbs., Sex, Scale.Bk.No = Scale.Bk.No., Age, Brood, LH, GW, FW2, Brood.2, SW1, Brood.3, SW2, SW3, SW4, edge.growth)
# Has 2465 obs. (multiple obs./fish in early years)



names(biodata)

# seperate pre and post 1916 data to combine rows, also take out 1968 which is a difficult year where all multiple duplicate fish codes have same scale book number
pre1918 <- biodata %>% 
  dplyr::filter(Year <= 1916)

just1968 <- biodata %>% 
  dplyr::filter(Year == 1968)

after1916 <- biodata %>% 
  dplyr::filter(!Year == 1968) %>% 
  dplyr::filter(Year > 1916) 

names(biodata)





# Now make new fish code name which is fish.no + scalebook number
# ** any fish code we make must be able to be made in isotope results

# for pre 1916
pre1918$Fish.Code <- paste(pre1918$Fish.No, pre1918$Scale.Bk.No, sep=".") 


# Post 1916
after1916$Fish.Code <- paste(after1916$Fish.No, after1916$Scale.Bk.No, sep=".") 


# And for 1968, fish.no + scalebook # + Age (both isotope date and biodata have different ages for the duplicated fish, it's only fish no 4 and 5 that are duplicated)
just1968$Fish.Code <- paste(just1968$Fish.No, just1968$Scale.Bk.No, just1968$Age, sep=".") 



### Do a check for duplicated fish codes

# Pre 1918:
length(pre1918$Fish.Code[duplicated(pre1918$Fish.Code)]) # 87 duplicated fish codes in early years
uniquefish <- pre1918$Fish.Code[duplicated(pre1918$Fish.Code)]
length(unique(uniquefish)) # From 35 unique fish

# Just to triple check what the duplicates are
df <- pre1918
dup <- data.frame(as.numeric(duplicated(df$Fish.Code))) #creates df with binary var for duplicated rows
colnames(dup) <- c("dup") #renames column for simplicity
df2 <- cbind(df, dup) #bind to original df
df3 <- subset(df2, dup == 1)
df3 <- df3 %>% 
  dplyr::select(Fish.Code, everything())
# Checks out, multiple rows for same fish



# After 1916
length(after1916$Fish.Code[duplicated(after1916$Fish.Code)])
# Dang, we've got 21 duplicates here too. Let's look at our duplicates
df <- after1916
dup <- data.frame(as.numeric(duplicated(df$Fish.Code))) #creates df with binary var for duplicated rows
colnames(dup) <- c("dup") #renames column for simplicity
df2 <- cbind(df, dup) #bind to original df
df3 <- subset(df2, dup == 1)
df3 <- df3 %>% 
  dplyr::select(Fish.Code, everything())
# From 2002, 2003, 2004, 2006. No Scale book # is our problem. So how do we merge this with isotope results
# well in isotope results, we have no fish from the 2000's without scale book #'s. So I am going to ignore this for now and see if it's ok. We shouldn't have problems when merging, because all isotope observations should have unique fish #s for the 2000s (fish # + scale bk)



# Just 1968
just1968$Fish.Code[duplicated(just1968$Fish.Code)]
# No duplicates!





# In the early data, each section of circuli growth has its own row, with a column 'LH' which tells life history (Freshwater1styear, saltwater2ndyear etc), and a column 'GW' which has the growth of that circuli. Unsure of in what units, something to ask Mike. 
# Here let's seperate each row so we can combine them
FW1_df <- pre1918 %>% 
  dplyr::filter(LH == "FW1")

FW2_df <- pre1918 %>% 
  dplyr::filter(LH == "FW2")

SW1_df <- pre1918 %>% 
  dplyr::filter(LH == "SW1")

SW2_df <- pre1918 %>% 
  dplyr::filter(LH == "SW2")

SW3_df <- pre1918 %>% 
  dplyr::filter(LH == "SW3")

# Ok, now rename all the GWs of each LH so we can distinguish them once we recombine, also only take what we need from the other ones
FW1_df1 <- FW1_df %>% 
  dplyr::rename(FW1.GW = GW) %>% 
  dplyr::select(!c(LH, FW2, Brood.2, SW1, Brood.3, SW2, SW3, SW4))

FW2_df1 <- FW2_df %>% 
  dplyr::select(Fish.Code, FW2.GW = GW)

SW1_df1 <- SW1_df %>% 
  dplyr::select(Fish.Code, SW1.GW = GW)

SW2_df1 <- SW2_df %>% 
  dplyr::select(Fish.Code, SW2.GW = GW)

SW3_df1 <- SW3_df %>% 
  dplyr::select(Fish.Code, SW3.GW = GW, SW4.GW = SW4)





# Now merge by our unique fish code
fulldf_LH <- merge(FW1_df1, FW2_df1, by = "Fish.Code", all.x = TRUE)
fulldf_LH1 <- merge(fulldf_LH, SW1_df1, by = "Fish.Code", all.x = TRUE)
fulldf_LH2 <- merge(fulldf_LH1, SW2_df1, by = "Fish.Code", all.x = TRUE)
fulldf_LH3 <- merge(fulldf_LH2, SW3_df1, by = "Fish.Code", all.x = TRUE)


fullpre1918 <- fulldf_LH3
# 36 observations! This is 1 more than unique fish found earlier, why?
# compare fullpre1918 with uniquefish
pre1918check <- fullpre1918["Fish.Code"]

unicheck <- as.data.frame(unique(uniquefish))

anti_join(pre1918check,unicheck, by = c("Fish.Code" = "unique(uniquefish)"))
# The extra fish has Fish.Code 327.76889.
# Just checked original data, this fish had no duplicated entries because there were no growth data recorded, not an issue! 











# Now attach pre1918, just1968, and after1916
names(fullpre1918)
names(just1968)
names(after1916)



fullpre1918 <- fullpre1918 %>% 
  dplyr::select(!Brood)

just1968 <- just1968 %>% 
  dplyr::select(!c("Brood", "Brood.3", "Brood.2", "LH")) %>% 
  dplyr::rename(FW1.GW = GW, FW2.GW = FW2, SW1.GW = SW1, SW2.GW = SW2, SW3.GW = SW3, SW4.GW = SW4)

after1916 <- after1916 %>% 
  dplyr::select(!c("Brood", "Brood.3", "Brood.2", "LH")) %>% 
  dplyr::rename(FW1.GW = GW, FW2.GW = FW2, SW1.GW = SW1, SW2.GW = SW2, SW3.GW = SW3, SW4.GW = SW4)

fullbiobio <- rbind(fullpre1918, just1968, after1916)

# just reorder columns
fullbiobiobio <- fullbiobio %>% 
  dplyr::select(Year, Fish.Code, Population, Sex, Weight.lbs, Length.inches, Age, FW1.GW, FW2.GW, SW1.GW, SW2.GW, SW3.GW, SW4.GW, edge.growth, everything())




## Again, check for duplicates
# Ok so here is our full babine biodata. 
fullbiobiobio$Fish.Code[duplicated(fullbiobiobio$Fish.Code)]

# I would like to have code that tells me the year the duplicates occur in
df <- fullbiobiobio
dup <- data.frame(as.numeric(duplicated(df$Fish.Code))) #creates df with binary var for duplicated rows
colnames(dup) <- c("dup") #renames column for simplicity
df2 <- cbind(df, dup) #bind to original df
df3 <- subset(df2, dup == 1) #subsets df using binary var for duplicated
# All in 2000's, lacking scale book #s. We've decided this is not an issue because none of our isotope scales in the 2000s are lacking scale book #s.

# write.csv(fullbiobiobio, "fullbabinebiodata_newFishCode22July2021.csv", row.names = FALSE)

# Now merge with isotope data!

rm(list=ls())

# Full biod data with new fish codes
full_bio <- read.csv("fullbabinebiodata_newFishCode22July2021.csv")


# Raw isotope data
iso_raw <- read.csv("Isotope_Results_Babine_salmon_March_2021-2.csv")


# I need to 1) remove old incomplete biodata from isotope data, 2) give iso dataframe the same fishcodes i did in biodata, 3) then merge together

names(iso_raw)
# 1) Take everything except the incomplete biodata (and Age, which I need for fish code in 1968)
iso1 <- iso_raw %>% 
  dplyr::select(Year, dC13 = X_13CVPDB, dN15 = X_15NAIR, dS34 = X_34SVCDT,  C.Natomic, C.Satomic, Fish.No = Fish.No., Scale.Bk.No = Scale.Bk.No., S.SFU, Sample.code, Sample.weight = Sample.weight..mg., Sample.SFU = Sample..SFU..., Wt.C = wt..C, Wt.N = wt..N, X.S, Age) 


# 2) Make "fish code" (same as for biodata)

pre1918 <- iso1 %>% 
  dplyr::filter(Year <= 1916)

just1968 <- iso1 %>% 
  dplyr::filter(Year == 1968)

after1916 <- iso1 %>% 
  dplyr::filter(!Year == 1968) %>% 
  dplyr::filter(Year > 1916) 



# for pre 1916
pre1918$Fish.Code <- paste(pre1918$Fish.No, pre1918$Scale.Bk.No, sep=".") 


# Post 1916
after1916$Fish.Code <- paste(after1916$Fish.No, after1916$Scale.Bk.No, sep=".") 


# And for 1968, fish.no + scalebook # + Age (both isotope date and biodata have different ages for the duplicated fish, it's only fish no 4 and 5 that are duplicated)
just1968$Fish.Code <- paste(just1968$Fish.No, just1968$Scale.Bk.No, just1968$Age, sep=".") 


fullisodata <- rbind(pre1918, after1916, just1968)
# now 167 obs instead of 170 (we had 3 NA rows, so this is good)







# Check for duplicated fish codes
df <- fullisodata
dup <- data.frame(as.numeric(duplicated(df$Fish.Code))) #creates df with binary var for duplicated rows
colnames(dup) <- c("dup") #renames column for simplicity
df2 <- cbind(df, dup) #bind to original df
df3 <- subset(df2, dup == 1) 
# No duplicates



# NOW MERGE isotope data with biodata
names(fullisodata)
names(full_bio)

fulliso1 <- fullisodata %>% 
  dplyr::select(!c(Fish.No, Scale.Bk.No, Year, Age)) # ****I added year and age so no duplicates, if there is something weird checking back

completedata <- merge(fulliso1, full_bio, by = "Fish.Code")
# 157 observations, we lost 10 fish. Where?


lostfish <- anti_join(fulliso1, completedata, by = "Fish.Code")
# So these 10 fish (4 from 1998, 5 from 2010, 1 from 1968) must have something different in the 'fulliso1' fish code compared to the 'full_bio' code. Let's check each one

# Right, so not in full_bio at all. Check original dataset. 

full_raw_bio_data <- read.csv("Full_Biodata_Babine_FromSkeena_13april2021.csv")

# Weird. So 10 fish just don't exist (or at least 9) in Mike's Babine biodata set. 
# Let's check with Mike about this. Our missing fish (samples that got sent to Mike, but that do not have corresponding observations in the full Babine biodata set) are from Scale book numbers #31480 and #90833.
# Also 1 observation from 1968.
# Maybe they are in full skeena biodata?

fullSKEENAbiodata <- read.csv("Mike.Price.Skeena_biodata_full..csv")
# Oh damn I think this is a typo. We have fish #1,2,4,5 from Babine Lake in skeena frame, but with book number #31488 instead of 31480. Could this have been a typo in the scales sent to the lab?

# Same with 2010, we have no scale book # 90833 in full skeena bio data, but we do have 90832 and 90838.
# I just checked the dataframe SENT to Mike R and they have the same book numbers as the dataframe sent BACK to me from archeology lab, so if it is a typo it must have happened before the frame was sent to isotope lab. Also possible, these samples were taken from an archive not documented in full biodata dataframe the Mike P has.
dataSENTto_arch_lab <- read.csv("Isotope_scale_data_1913-2014-1.csv")
# The lost fish have no biodata attached to them so not possible to match with biodata fish, if it was a scale book typo.

# either way, not the end of the world. I just don't have biodata for these 10 samples. Ask Mike when I'm up north, for now just merge lost fish back into our combo frame. 

names(completedata)
names(lostfish)


# So re-merge, but keep all x.
completedata2 <- merge(fulliso1, full_bio, by = "Fish.Code", all.x = TRUE)
# Ok, that seems to have worked!


# So here is our new merged dataset, A summary:
# 167 observations 
names(completedata2)

completeNA <- completedata2[is.na(completedata2$dC13),]
# 10 rows have no isotope data (failed isotope tests)
# So only 157 observations with isotope data


# weight: 
with(completedata2,
     table(Year, Weight.lbs,useNA = "ifany")) # yearly data until 1957 then nothing 1957 onward

# sex:
with(completedata2,
     table(Year, Sex, useNA = "ifany")) # yearly data until nothing 1957 onward


# length:
length<- with(completedata2,
              table(Year, Length.inches, useNA = "ifany")) # Length data (not complete) throughout entire time frame


# age
with(completedata2,
     table(Year, Age, useNA = "ifany")) # Throughout entire time frame!


# Let's go from here, now we have our dataset. 

#write.csv(completedata2, "Babine_iso_bio_data_flawed_but_full_22July2021.csv", row.names = FALSE)
rm(list=ls())

### A problem - we lost year data for the 10 lost fish because they were not in the bio frame, god is this as complicated as I have made it? 

# So remove 10 lost fish, add columns full of NA for all bio columns, then just use rbind.










#### PART 2 aug 2021 ******************************************************** ####
rm(list=ls())

# Full biod data with new fish codes
full_bio <- read.csv("fullbabinebiodata_newFishCode22July2021.csv")


# Raw isotope data
iso_raw <- read.csv("Isotope_Results_Babine_salmon_March_2021-2.csv")


# I need to 1) remove old incomplete biodata from isotope data, 2) give iso dataframe the same fishcodes i did in biodata, 3) then merge together

names(iso_raw)
# 1) Take everything except the incomplete biodata (and Age, which I need for fish code in 1968)
iso1 <- iso_raw %>% 
  dplyr::select(Year, dC13 = X_13CVPDB, dN15 = X_15NAIR, dS34 = X_34SVCDT,  C.Natomic, C.Satomic, Fish.No = Fish.No., Scale.Bk.No = Scale.Bk.No., S.SFU, Sample.code, Sample.weight = Sample.weight..mg., Sample.SFU = Sample..SFU..., Wt.C = wt..C, Wt.N = wt..N, X.S, Age) 


# 2) Make "fish code" (same as for biodata)

pre1918 <- iso1 %>% 
  dplyr::filter(Year <= 1916)

just1968 <- iso1 %>% 
  dplyr::filter(Year == 1968)

after1916 <- iso1 %>% 
  dplyr::filter(!Year == 1968) %>% 
  dplyr::filter(Year > 1916) 



# for pre 1916
pre1918$Fish.Code <- paste(pre1918$Fish.No, pre1918$Scale.Bk.No, sep=".") 


# Post 1916
after1916$Fish.Code <- paste(after1916$Fish.No, after1916$Scale.Bk.No, sep=".") 


# And for 1968, fish.no + scalebook # + Age (both isotope date and biodata have different ages for the duplicated fish, it's only fish no 4 and 5 that are duplicated)
just1968$Fish.Code <- paste(just1968$Fish.No, just1968$Scale.Bk.No, just1968$Age, sep=".") 


fullisodata <- rbind(pre1918, after1916, just1968)
# now 167 obs instead of 170 (we had 3 NA rows, so this is good)







# Check for duplicated fish codes
df <- fullisodata
dup <- data.frame(as.numeric(duplicated(df$Fish.Code))) #creates df with binary var for duplicated rows
colnames(dup) <- c("dup") #renames column for simplicity
df2 <- cbind(df, dup) #bind to original df
df3 <- subset(df2, dup == 1) 
# No duplicates



# NOW MERGE isotope data with biodata
names(fullisodata)
names(full_bio)


# Remove 10 lost fish from isotope data ( I found these fish in previous section)
unique(lostfish$Fish.Code) 
# "1.31480"   "2.31480"  "4.31480"    "5.31480"    "1.90833"    "2.90833"    "3.90833"    "4.90833"    "5.90833"    "5.Bk. 1.NA"

fullisodata_foundfish <- fullisodata %>% 
  dplyr::filter(!Fish.Code %in% c("1.31480"  , "2.31480" , "4.31480"  ,  "5.31480"  ,  "1.90833"    ,"2.90833"  ,  "3.90833" ,   "4.90833"   , "5.90833"  ,  "5.Bk. 1.NA"))


# NOW merge fullisodata_found fish with fullbiodata, and afterwards we will add in lost fish.
names(fullisodata_foundfish)
names(full_bio)

fulliso1 <- fullisodata_foundfish %>% 
  dplyr::select(!c(Fish.No, Scale.Bk.No, Year, Age)) 


completedata <- merge(fulliso1, full_bio, by = "Fish.Code")
# 157 observations, good


names(lostfish)
names(fullisodata)
fullisodata_lostfish <- fullisodata %>% 
  dplyr::filter(Fish.Code %in% c("1.31480"  , "2.31480" , "4.31480"  ,  "5.31480"  ,  "1.90833"    ,"2.90833"  ,  "3.90833" ,   "4.90833"   , "5.90833"  ,  "5.Bk. 1.NA"))


names(completedata)
names(fullisodata_lostfish)

fullisodata_lostfish1 <- fullisodata_lostfish %>% 
  add_column(Population = NA, Weight.lbs = NA, Length.inches = NA, FW1.GW  = NA, FW2.GW = NA, SW1.GW = NA, SW2.GW = NA, SW3.GW = NA, SW4.GW =  NA, edge.growth = NA, Proportion = NA , Date.Caught = NA , Sex = NA)


# Now bind!

complete2 <- rbind(completedata, fullisodata_lostfish1)

# Ok Dang! I think we are good to go!

#write.csv(complete2, "babine_bio_iso_29July.csv", row.names = FALSE)


with(complete2,
     table(Year, Weight.lbs,useNA = "ifany")) # yearly data until 1957 then nothing 1957 onward

# sex:
with(complete2,
     table(Year, Sex, useNA = "ifany")) # yearly data until nothing 1957 onward


# length:
length<- with(complete2,
              table(Year, Length.inches, useNA = "ifany")) # Length data (not complete) throughout entire time frame


# age
with(complete2,
     table(Year, Age, useNA = "ifany")) # Throughout entire time frame!

#### SUESS CORRECTION *** I think I should edit this with a one year lag, see Feddern et al.####
# I calculated this in May, recalculate now
# I used equation presented in Misarti et. al 2009, also see Gruber et al. 1999
# Gruber presents an approximate decrease of -0.018 permill / year from 1980 - 1995. 

babine <- read.csv("babine_bio_iso_29July.csv")

# Suess Effect Correction Factor: a*exp(b*0.027)
# Where a = maximum annual rate of d13C decrease in the North Pacific (here I will use -0.014 derived from Quay et al. 1992 but I may need to justify a different number later on) 
# b = number of years since 1850
# 0.027 = curve represented by Gruber et al. 1999 for change in d13C in ocean. 



Suess.fun <- function(c, b) {
  return(c - (-0.014)*exp(b*0.027))
}


# Now I need to take our dataframe and add another column that is the number of years since 1850 (b), ie. sample collection year - 1850

data <- babine
class(data$Year)

data1 <- data %>% 
  dplyr::mutate(b = Year - 1850)


head(data1)

# Ok, I think I can run the correction now. 
data2 <- data1 %>% 
  dplyr::mutate(Suess.dC13 = Suess.fun(dC13, b))


# I think that worked!

# Plot corrected values
a <- ggplot(data2, aes(x = Year, y = Suess.dC13)) +
  geom_point() +
  theme_classic() +
  labs(
    x = 'Year',
    y = 'dC13 (corrected)');a 


# And mean corrected values
meanC <- data2 %>% 
  dplyr::select(Year, Suess.dC13) %>% 
  dplyr::group_by(Year) %>% 
  dplyr::summarise(Suess.mean_C13 = mean(Suess.dC13, na.rm = TRUE))

a <- ggplot(meanC, aes(x = Year, y = Suess.mean_C13)) +
  geom_point() +
  geom_line() +
  theme_classic() +
  labs(
    x = 'Year',
    y = 'Mean dC13 (corrected)');a 


# Plot uncorrected values
b <- ggplot(data2, aes(x = Year, y = dC13)) +
  geom_point() +
  theme_classic() +
  labs(
    x = 'Year',
    y = 'dC13 (corrected)');b


# And mean uncorrected
meanC <- data2 %>% 
  dplyr::select(Year, dC13) %>% 
  dplyr::group_by(Year) %>% 
  dplyr::summarise(mean_C13 = mean(dC13, na.rm = TRUE))

b <- ggplot(meanC, aes(x = Year, y = mean_C13)) +
  geom_point() +
  geom_line() +
  theme_classic() +
  labs(
    x = 'Year',
    y = 'Mean dC13 (uncorrected)');b 

plotlist <-list(b,a)

library(sjPlot)
cowplot::plot_grid(plotlist = plotlist, nrow = 2, ncol = 1)

ggsave(
  "Suesscorrected_anduncorrected_d13C_3Aug2021.tiff",
  plot = last_plot(),
  device = NULL,
  path = "/Users/celestekieran/Desktop/BabineScales_and_isotopes_2021/Babine_plots_figures2021/August2021_Babine_plots",
  scale = 1,
  width = 20,
  height =15,
  units = c("cm"),
  dpi = 300,
  limitsize = TRUE
)

# So, the Suess effect is the increase of C13 depleted carbon in the atmosphere and ocean, so our 'corrected' values should show higher C13 values then 'uncorrected' values (higher meaning less negative)


# Our uncorrected data
meanC[which.max(meanC$mean_C13),] # Highest d13C was in 1918 (-16.6)
meanC[which.min(meanC$mean_C13),] # Lowest d13C was in 2014 (-18.0)
# Range of 1.4

yearuncorrect <- meanC %>% 
  dplyr::select(Year, mean_C13)


# data corrected for Suess effect
meanC <- data2 %>% 
  dplyr::select(Year, Suess.dC13) %>% 
  dplyr::group_by(Year) %>% 
  dplyr::summarise(Suess.mean_C13 = mean(Suess.dC13, na.rm = TRUE))

meanC[which.max(meanC$Suess.mean_C13),] # Highest d13C was still in 1918 but with 16.5
meanC[which.min(meanC$Suess.mean_C13),] # Lowest was now in 1957 with 17.3
# Range of 0.8

yearcorrect <- meanC %>% 
  dplyr::select(Year, Suess.mean_C13)

# Interesting, maybe this reveals some pattern in C13 decrease in late 50s? Correcting for Suess reduces the variation in d13C but maybe reveals some patterns. Measurement errors for d13C are +/- 0.1, so still some pattern revealed. Explore this more later


# Now do this with full data spread
c <- ggplot(data2, aes(x = Year, y = Suess.dC13)) +
  geom_point(alpha=0.6, colour = "royalblue2") +
  theme_classic() +
  labs(
    x = 'Year',
    y = 'dC13 (corrected)');c

d <- ggplot(data2, aes(x = Year, y = dC13)) +
  geom_point(alpha=0.6, colour = "royalblue2") + 
  theme_classic() +
  labs(
    x = 'Year',
    y = 'Mean dC13 (uncorrected)');d

plotlist <-list(d,c)

library(sjPlot)
cowplot::plot_grid(plotlist = plotlist, nrow = 2, ncol = 1)


ggsave(
  "full_spread_Suesscorrected_anduncorrected_d13C_3Aug2021.tiff",
  plot = last_plot(),
  device = NULL,
  path = "/Users/celestekieran/Desktop/BabineScales_and_isotopes_2021/Babine_plots_figures2021/August2021_Babine_plots",
  scale = 1,
  width = 20,
  height =15,
  units = c("cm"),
  dpi = 300,
  limitsize = TRUE
)

# write.csv(data2, "babine_bio_iso_suess3Aug2021.csv", row.names = FALSE)
babine <- read.csv("babine_bio_iso_suess3Aug2021.csv")
names(babine)
#### Data frame #3, 19 october 2021 (this was unneccesary because our newer years that mike chose for analysis didn't have biodata for the newer years) ####

## So there is still somehow some missing biodata, the 'full' dataframe I made earlier this month is missing all the newer biodata, such as length, age, sex. So steps:
# 1 - re-edit full skeena bioframe, make sure to keep all new biodata. Check formats (1-male, 2-female, 3-unknown)
# 2 - check all units (inches, lbs etc.)
# 3 - add suess effect
# 4 -  re-run everything (models, visualiztions etc with new df.)



# this all needs to be edited
rm(list=ls())

# Full biod data with new fish codes
full_bio <- read.csv("fullbabinebiodata_newFishCode22July2021.csv")


# Raw isotope data
iso_raw <- read.csv("Isotope_Results_Babine_salmon_March_2021-2.csv")


# I need to 1) remove old incomplete biodata from isotope data, 2) give iso dataframe the same fishcodes i did in biodata, 3) then merge together

names(iso_raw)
# 1) Take everything except the incomplete biodata (and Age, which I need for fish code in 1968)
iso1 <- iso_raw %>% 
  dplyr::select(Year, dC13 = X_13CVPDB, dN15 = X_15NAIR, dS34 = X_34SVCDT,  C.Natomic, C.Satomic, Fish.No = Fish.No., Scale.Bk.No = Scale.Bk.No., S.SFU, Sample.code, Sample.weight = Sample.weight..mg., Sample.SFU = Sample..SFU..., Wt.C = wt..C, Wt.N = wt..N, X.S, Age) 


# 2) Make "fish code" (same as for biodata)

pre1918 <- iso1 %>% 
  dplyr::filter(Year <= 1916)

just1968 <- iso1 %>% 
  dplyr::filter(Year == 1968)

after1916 <- iso1 %>% 
  dplyr::filter(!Year == 1968) %>% 
  dplyr::filter(Year > 1916) 



# for pre 1916
pre1918$Fish.Code <- paste(pre1918$Fish.No, pre1918$Scale.Bk.No, sep=".") 


# Post 1916
after1916$Fish.Code <- paste(after1916$Fish.No, after1916$Scale.Bk.No, sep=".") 


# And for 1968, fish.no + scalebook # + Age (both isotope date and biodata have different ages for the duplicated fish, it's only fish no 4 and 5 that are duplicated)
just1968$Fish.Code <- paste(just1968$Fish.No, just1968$Scale.Bk.No, just1968$Age, sep=".") 


fullisodata <- rbind(pre1918, after1916, just1968)
# now 167 obs instead of 170 (we had 3 NA rows, so this is good)







# Check for duplicated fish codes
df <- fullisodata
dup <- data.frame(as.numeric(duplicated(df$Fish.Code))) #creates df with binary var for duplicated rows
colnames(dup) <- c("dup") #renames column for simplicity
df2 <- cbind(df, dup) #bind to original df
df3 <- subset(df2, dup == 1) 
# No duplicates



# NOW MERGE isotope data with biodata
names(fullisodata)
names(full_bio)


# Remove 10 lost fish from isotope data ( I found these fish in previous section)
unique(lostfish$Fish.Code) 
# "1.31480"   "2.31480"  "4.31480"    "5.31480"    "1.90833"    "2.90833"    "3.90833"    "4.90833"    "5.90833"    "5.Bk. 1.NA"

fullisodata_foundfish <- fullisodata %>% 
  dplyr::filter(!Fish.Code %in% c("1.31480"  , "2.31480" , "4.31480"  ,  "5.31480"  ,  "1.90833"    ,"2.90833"  ,  "3.90833" ,   "4.90833"   , "5.90833"  ,  "5.Bk. 1.NA"))


# NOW merge fullisodata_found fish with fullbiodata, and afterwards we will add in lost fish.
names(fullisodata_foundfish)
names(full_bio)

fulliso1 <- fullisodata_foundfish %>% 
  dplyr::select(!c(Fish.No, Scale.Bk.No, Year, Age)) 


completedata <- merge(fulliso1, full_bio, by = "Fish.Code")
# 157 observations, good


names(lostfish)
names(fullisodata)
fullisodata_lostfish <- fullisodata %>% 
  dplyr::filter(Fish.Code %in% c("1.31480"  , "2.31480" , "4.31480"  ,  "5.31480"  ,  "1.90833"    ,"2.90833"  ,  "3.90833" ,   "4.90833"   , "5.90833"  ,  "5.Bk. 1.NA"))


names(completedata)
names(fullisodata_lostfish)

fullisodata_lostfish1 <- fullisodata_lostfish %>% 
  add_column(Population = NA, Weight.lbs = NA, Length.inches = NA, FW1.GW  = NA, FW2.GW = NA, SW1.GW = NA, SW2.GW = NA, SW3.GW = NA, SW4.GW =  NA, edge.growth = NA, Proportion = NA , Date.Caught = NA , Sex = NA)


# Now bind!

complete2 <- rbind(completedata, fullisodata_lostfish1)

# Ok Dang! I think we are good to go!

#write.csv(complete2, "babine_bio_iso_29July.csv", row.names = FALSE)


with(complete2,
     table(Year, Weight.lbs,useNA = "ifany")) # yearly data until 1957 then nothing 1957 onward

# sex:
with(complete2,
     table(Year, Sex, useNA = "ifany")) # yearly data until nothing 1957 onward


# length:
length<- with(complete2,
              table(Year, Length.inches, useNA = "ifany")) # Length data (not complete) throughout entire time frame


# age
with(complete2,
     table(Year, Age, useNA = "ifany")) # Throughout entire time frame!








#### Data explanation ####

# Here is a dataframe I've made with the isotope data w/ suess
 babine <- read.csv("babine_bio_iso_suess3Aug2021.csv")

# reorder to view dates of samples 
babine <- babine %>% 
  dplyr::select(Fish.Code, Year, Date.Caught, everything())


# We have 167 samples, isotope data for 157 of them, weight and sex data pretty consistent until 1957 then nothing, age (not complete) but throughout entire timeframe. We have some missing biodata and some funky things going on with differences in between dataframes, refer back to "firstlookand clean" script.


#### -----------  3 August 2021: Basic examination with new dataframe, plots and figures, niche plots ####
# 1. Add Suess corrected d13C to new dataframe
# 2. Basic plots and figures (see original sheet for lab: variation over time, variation and isotopes with age length and sex, isotopes over time grouped by life history, niche [c.vs.N over time, by group] )
# 3. (if time) basic glmms with bio traits




#### Plots and Figures ####

# Everything below I did while overcaffeinated for quick results, re-check all work

rm(list=ls())
babine <- read.csv("babine_bio_iso_suess3Aug2021.csv")

### ** in this section we replace weird ages with NA **
# Let's start with niche width because they looked the prettiest
roughniche <- ggplot(babine, aes(x = dC13, y = dN15, colour = Age)) +
  geom_point() +
  theme_classic() +
  labs(
    x = 'C',
    y = 'N') ;roughniche

# Ok fine, but I have weird Age groups, 3M RG and UD
with(babine,
     table(Year, Age, useNA = "ifany"))
# 3M RG is some weird designation in 1998 (fish code 	2.31466 & 5.31466)
#  and UD (undesignated?) is one from 1982  (fish code 5.Bk.  1)
# Make these NA and then ignore NAs (later!)

library(naniar)

# Make string of offending ages
weird_age <- c("3M RG", "UD")

# Turn them into NA
babine1 <- babine %>% 
  replace_with_na_all(condition = ~.x %in% weird_age)


# Try again! Also remove NA in plot
babine1$Age <- as.factor(babine1$Age)

b <- babine1 %>% 
  filter(!is.na(Age)) %>% 
         ggplot(aes(x = dC13, y = dN15, colour = Age)) +
  geom_point() +
  theme_classic() +
  labs(
    x = 'C',
    y = 'N') ;roughniche


# Ok good, also with suess corrected
a <- babine1 %>% 
  filter(!is.na(Age)) %>% 
  ggplot(aes(x = Suess.dC13, y = dN15, colour = Age)) +
  geom_point() +
  theme_classic() +
  labs(
    x = 'C (Suess corrected)',
    y = 'N') ;roughniche



plotlist <-list(b,a)


cowplot::plot_grid(plotlist = plotlist, nrow = 2, ncol = 1)
## NO weird, our ages have become solid numbers, not sure what happened. CHeck later.
ggsave(
  "Suesscorrected_anduncorrected_CvsN_Aug2021.tiff",
  plot = last_plot(),
  device = NULL,
  path = "/Users/celestekieran/Desktop/BabineScales_and_isotopes_2021/Babine_plots_figures2021/August2021_Babine_plots",
  scale = 1,
  width = 20,
  height =15,
  units = c("cm"),
  dpi = 300,
  limitsize = TRUE
)



### Here somehow we
#ok, try with ellipse grouped by life history
roughniche <- babine1 %>% 
  filter(!is.na(Age)) %>% 
  ggplot(aes(x = dC13, y = dN15, colour = Age)) +
  theme_classic() +
  labs(
    x = 'C',
    y = 'N') +
  stat_ellipse(aes(x = dC13, y = dN15, colour = Age, group = Age),type = "norm");roughniche
# not enough data for accuracy (I'll need to do stats), but the ellipses plot and it looks pretty



# now try grouped by year
roughniche <- ggplot(babine, aes(x = Suess.dC13, y = dN15, colour = Year)) +
  theme_classic() +
  labs(
    x = 'C (Suess corrected)',
    y = 'N') +
  stat_ellipse(aes(x = Suess.dC13, y = dN15, colour = Year, group = Year),type = "norm");roughniche
# This looks very cool, but too many ellipse, try seperating by year groups. Also, note that this is using uncorrected for Suess effect d13C values
roughniche2 <- ggplot(babine, aes(x = dC13, y = dN15, colour = Year)) +
  theme_classic() +
  labs(
    x = 'C',
    y = 'N') +
  stat_ellipse(aes(x = dC13, y = dN15, colour = Year, group = Year),type = "norm");roughniche



plotlist <-list(roughniche,roughniche2)


cowplot::plot_grid(plotlist = plotlist, nrow = 2, ncol = 1)
## NO weird, our ages have become solid numbers, not sure what happened. CHeck later.
ggsave(
  "RoughEllipses_time_scale_3Aug2021.tiff",
  plot = last_plot(),
  device = NULL,
  path = "/Users/celestekieran/Desktop/BabineScales_and_isotopes_2021/Babine_plots_figures2021/August2021_Babine_plots",
  scale = 1,
  width = 20,
  height =15,
  units = c("cm"),
  dpi = 300,
  limitsize = TRUE
)








#### Make 3 time frames, early, middle, late. For niche testing! ####


# ** these ellipses are all calculated assuming multivariate normal distribution
unique(poultryplant$Year)
# 17 years, from 
# from 1913 to 2010
# that's 107 years of data, so split into 3

# say, early: 1913 - 1945
# middle: 1946 - 1977
# late: 1978 - 2010

# I've only seperated these by chunks, not taking into account regime shifts. 


# So take the mean d13C and d15N values for each time period
# actually scratch that. I don't think i need to take means, what I need to do is add a 'time period' variable to each category that identifies it as early, middle, or late for the ellipses grouping

#earlymean <- poultryplant %>% 
#dplyr::select(Year, dC13, dN15, dS34) %>% 
#dplyr::filter(Year < 1945) %>% 
#dplyr::group_by(Year) %>% 
#dplyr::summarise(mean_C13 = mean(dC13), mean_N15 = mean(dN15), mean_S34 = mean(dS34))



# I'm going to do this in a cheat way
poultryplant <- babine 
early <- poultryplant %>% 
  dplyr::select(Year, Suess.dC13, dN15, dS34) %>% 
  dplyr::filter(Year < 1946) %>% 
  dplyr::mutate(Period = "early")

middle <- poultryplant %>% 
  dplyr::select(Year, Suess.dC13, dN15, dS34) %>% 
  dplyr::filter(Year > 1945, Year < 1978) %>% 
  dplyr::mutate(Period = "middle")

late <- poultryplant %>% 
  dplyr::select(Year, Suess.dC13, dN15, dS34) %>% 
  dplyr::filter(Year > 1977) %>% 
  dplyr::mutate(Period = "late")

hotcoffee_timeperiod <- rbind(early, middle, late)


# order levels
hotcoffee_timeperiod$Period <- factor(hotcoffee_timeperiod$Period, levels = c("early", "middle", "late"))

period_niche <- ggplot(hotcoffee_timeperiod, aes(x = Suess.dC13, y = dN15, colour = Period)) +
  theme_classic() +
  labs(
    x = 'δ13C (Suess corrected)',
    y = 'δ15N') +
  stat_ellipse(aes(x = Suess.dC13, y = dN15, colour = Period, group = Period),type = "norm");period_niche


ggsave(
  "niche_comparison_6Aug2021.tiff",
  plot = last_plot(),
  device = NULL,
  path = "/Users/celestekieran/Desktop/BabineScales_and_isotopes_2021/Babine_plots_figures2021/August2021_Babine_plots",
  scale = 1,
  width = 20,
  height =15,
  units = c("cm"),
  dpi = 300,
  limitsize = TRUE
)



## Split niches by regime shifts

# According to Overland et al. , we see significant climate indices around 1976, 1989, and 1998
# Cold PDO pre-1977/76, shift to warm PDO phase 1976/77 - 1989, weaker shift to more moderate conditions 1989 - 1998, 1998 onward more warm


poultryplant <- babine 

pre1977 <- poultryplant %>% 
  dplyr::select(Year, Suess.dC13, dN15, dS34) %>% 
  dplyr::filter(Year < 1925) %>% 
  dplyr::mutate(sues = "pre1977") # doesn't matter if its 76 or 77 because we have no samples from the 70s, 82 is closest (127 obs)

from1977to1989 <- poultryplant %>% 
  dplyr::select(Year, Suess.dC13, dN15, dS34) %>% 
  dplyr::filter(Year > 1977, Year < 1989) %>% 
  dplyr::mutate(Period2 = "from1977to1989") # only 19 obs

from1989to1998 <- poultryplant %>% 
  dplyr::select(Year, Suess.dC13, dN15, dS34) %>% 
  dplyr::filter(Year > 1989, Year < 1998) %>% 
  dplyr::mutate(Period2 = "from1989to1998") # only 10 obs

post1998 <- poultryplant %>% 
  dplyr::select(Year, Suess.dC13, dN15, dS34) %>% 
  dplyr::filter(Year < 1998) %>% 
  dplyr::mutate(Period2 = "post1998") # 118 obs

# So definitely not evenly distributed data but lets try just for fun
coldcoffee_timeperiod <- rbind(pre1977,from1977to1989,from1989to1998, post1998)


period_niche2 <- ggplot(coldcoffee_timeperiod, aes(x = Suess.dC13, y = dN15, colour = Period2)) +
  theme_classic() +
  labs(
    x = 'C (Suess corrected)',
    y = 'N') +
  stat_ellipse(aes(x = Suess.dC13, y = dN15, colour = Period2, group = Period2),type = "norm");period_niche2


# with colour adjusted
# order factor levels
unique(hotcoffee_timeperiod$Period)

coldcoffee_timeperiod$Period2 <- factor(coldcoffee_timeperiod$Period2, levels = c("pre1977", "from1977to1989", "from1989to1998", "post1998"))



period_niche2 <- ggplot(coldcoffee_timeperiod, aes(x = Suess.dC13, y = dN15, colour = Period2)) +
  theme_classic() +
  labs(
    x = 'C (Suess corrected)',
    y = 'N') +
  stat_ellipse(aes(x = Suess.dC13, y = dN15, colour = Period2, group = Period2),type = "norm") ;period_niche2


## This is interesting, pre 1977 and post 1998 have almost exact overlapping niche width, don't know if this is at all significant because post 1998 has so few data points.

??stat_ellipse




plotlist <-list(period_niche,period_niche2)


cowplot::plot_grid(plotlist = plotlist, nrow = 2, ncol = 1)


ggsave(
  "Periodchange_niche_6Aug2021.tiff",
  plot = last_plot(),
  device = NULL,
  path = "/Users/celestekieran/Desktop/BabineScales_and_isotopes_2021/Babine_plots_figures2021/August2021_Babine_plots",
  scale = 1,
  width = 20,
  height =15,
  units = c("cm"),
  dpi = 300,
  limitsize = TRUE
)



rm(list=ls())

# Also these regime shift dates taken from Breaker 2008
# 1) 1925 - negative
# 2) 1939 - positive
# 3) 1946 - negative
# 4) 1976/77 - positive, a big guy
# 5) 1989? - more biological than climatic
# 6) 1999 - negative
# 7) 2008



poultryplant <- babine 

pre1925 <- poultryplant %>% 
  dplyr::select(Year, Suess.dC13, dN15, dS34) %>% 
  dplyr::filter(Year < 1925) %>% 
  dplyr::mutate(Period2 = "pre1925") # 30 obs (1913, 1918, 1923)

from1925to1939 <- poultryplant %>% 
  dplyr::select(Year, Suess.dC13, dN15, dS34) %>% 
  dplyr::filter(Year > 1924, Year < 1939) %>% 
  dplyr::mutate(Period2 = "from1925to1939") # 20 obs (1933, 1937)

from1939to1946 <- poultryplant %>% 
  dplyr::select(Year, Suess.dC13, dN15, dS34) %>% 
  dplyr::filter(Year > 1938, Year < 1946) %>% 
  dplyr::mutate(Period2 = "from1939to1946") #10 obs (1943)

from1946to1976 <- poultryplant %>% 
  dplyr::select(Year, Suess.dC13, dN15, dS34) %>% 
  dplyr::filter(Year > 1945, Year < 1977) %>% 
  dplyr::mutate(Period2 = "from1946to1976") #  # 29 obs (1947, 1957, 1968)

from1976to1989 <- poultryplant %>% 
  dplyr::select(Year, Suess.dC13, dN15, dS34) %>% 
  dplyr::filter(Year > 1976, Year < 1989) %>% 
  dplyr::mutate(Period2 = "from1976to1989") # 19 obs. (1982, 1988)

from1989to1999 <- poultryplant %>% 
  dplyr::select(Year, Suess.dC13, dN15, dS34) %>% 
  dplyr::filter(Year > 1988, Year < 1999) %>% 
  dplyr::mutate(Period2 = "from1989to1999") # 19 obs. (1993, 1998)

from1999to2008 <- poultryplant %>% 
  dplyr::select(Year, Suess.dC13, dN15, dS34) %>% 
  dplyr::filter(Year > 1998, Year < 2008) %>% 
  dplyr::mutate(Period2 = "from1999to2008") # 20 obs, (200, 2005)

post2008 <- poultryplant %>% 
  dplyr::select(Year, Suess.dC13, dN15, dS34) %>% 
  dplyr::filter(Year > 2007) %>% 
  dplyr::mutate(Period2 = "post2008") # 20 obs. (2010, 2014)

# So  most have ~20 observations from 2 years, 2 have 30  one has 10

# So not evenly distributed data but lets try just for fun
coldcoffee_timeperiod <- rbind(pre1925, from1925to1939, from1939to1946, from1946to1976, from1976to1989, from1989to1999, from1999to2008, post2008) 
# way too hectic! But there are differences




period_niche2 <- ggplot(coldcoffee_timeperiod, aes(x = Suess.dC13, y = dN15, colour = Period2)) +
  theme_classic() +
  labs(
    x = 'C (Suess corrected)',
    y = 'N') +
  stat_ellipse(aes(x = Suess.dC13, y = dN15, colour = Period2, group = Period2),type = "norm");period_niche2



period_niche2 <- ggplot(coldcoffee_timeperiod, aes(x = Suess.dC13, y = dN15, colour = Period2)) +
  theme_classic() +
  labs(
    x = 'C (Suess corrected)',
    y = 'N') +
  stat_ellipse(aes(x = Suess.dC13, y = dN15, colour = Period2, group = Period2),type = "norm");period_niche2






# try again with negative or positive PDO 
# Also these regime shift dates taken from Breaker 2008
# 1) 1925 - negative
# 2) 1939 - positive
# 3) 1946 - negative
# 4) 1976/77 - positive, a big guy
# 5) 1989? - more biological than climatic
# 6) 1999 - negative
# 7) 2008 - positive


poultryplant <- babine 

pre1925 <- poultryplant %>% 
  dplyr::select(Year, Suess.dC13, dN15, dS34) %>% 
  dplyr::filter(Year < 1925) %>% 
  dplyr::mutate(PDO = "positive") # 30 obs (1913, 1918, 1923)

from1925to1939 <- poultryplant %>% 
  dplyr::select(Year, Suess.dC13, dN15, dS34) %>% 
  dplyr::filter(Year > 1924, Year < 1939) %>% 
  dplyr::mutate(PDO = "negative") # 20 obs (1933, 1937)

from1939to1946 <- poultryplant %>% 
  dplyr::select(Year, Suess.dC13, dN15, dS34) %>% 
  dplyr::filter(Year > 1938, Year < 1946) %>% 
  dplyr::mutate(PDO = "positive") #10 obs (1943)

from1946to1976 <- poultryplant %>% 
  dplyr::select(Year, Suess.dC13, dN15, dS34) %>% 
  dplyr::filter(Year > 1945, Year < 1977) %>% 
  dplyr::mutate(PDO = "negative") #  # 29 obs (1947, 1957, 1968)

from1976to1989 <- poultryplant %>% 
  dplyr::select(Year, Suess.dC13, dN15, dS34) %>% 
  dplyr::filter(Year > 1976, Year < 1989) %>% 
  dplyr::mutate(PDO = "positive") # 19 obs. (1982, 1988)

from1989to1999 <- poultryplant %>% 
  dplyr::select(Year, Suess.dC13, dN15, dS34) %>% 
  dplyr::filter(Year > 1988, Year < 1999) %>% 
  dplyr::mutate(PDO = "positive") # 19 obs. (1993, 1998)

from1999to2008 <- poultryplant %>% 
  dplyr::select(Year, Suess.dC13, dN15, dS34) %>% 
  dplyr::filter(Year > 1998, Year < 2008) %>% 
  dplyr::mutate(PDO = "negative") # 20 obs, (200, 2005)

post2008 <- poultryplant %>% 
  dplyr::select(Year, Suess.dC13, dN15, dS34) %>% 
  dplyr::filter(Year > 2007) %>% 
  dplyr::mutate(PDO = "positive")
# order levels


coldcoffee_timeperiod <- rbind(pre1925, from1925to1939, from1939to1946, from1946to1976, from1976to1989, from1989to1999, from1999to2008, post2008)

coldcoffee_timeperiod$PDO <- factor(coldcoffee_timeperiod$PDO, levels = c("positive", "negative"))

PDOniche <- ggplot(coldcoffee_timeperiod, aes(x = Suess.dC13, y = dN15, colour = PDO)) +
  theme_classic() +
  labs(
    x = 'δ13C (Suess corrected)',
    y = 'δ15N') +
  stat_ellipse(aes(x = Suess.dC13, y = dN15, colour = PDO, group = PDO),type = "norm");PDOniche


### EARLY MIDDLE LATE but withregime shifts in mind

poultryplant <- babine 
early <- poultryplant %>% 
  dplyr::select(Year, Suess.dC13, dN15, dS34) %>% 
  dplyr::filter(Year < 1977) %>% 
  dplyr::mutate(Period = "early")

middle <- poultryplant %>% 
  dplyr::select(Year, Suess.dC13, dN15, dS34) %>% 
  dplyr::filter(Year > 1976, Year < 2008) %>% 
  dplyr::mutate(Period = "middle")

late <- poultryplant %>% 
  dplyr::select(Year, Suess.dC13, dN15, dS34) %>% 
  dplyr::filter(Year > 2007) %>% 
  dplyr::mutate(Period = "late")

hotcoffee_timeperiod <- rbind(early, middle, late)


# order levels
hotcoffee_timeperiod$Period <- factor(hotcoffee_timeperiod$Period, levels = c("early", "middle", "late"))

period_niche <- ggplot(hotcoffee_timeperiod, aes(x = Suess.dC13, y = dN15, colour = Period)) +
  theme_classic() +
  labs(
    x = 'δ13C (Suess corrected)',
    y = 'δ15N') +
  stat_ellipse(aes(x = Suess.dC13, y = dN15, colour = Period, group = Period),type = "norm");period_niche

# don't know about this one, very different results and I don't know enough about ellipses to know how the observation number effects things

#### Sex  length and age niche  ####


babine <- babine %>% 
  dplyr::filter(Sex %in% c("M", "F"))

Sexniche <- ggplot(babine, aes(x = Suess.d13C, y = d15N, colour = Sex, na.rm = TRUE)) +
  theme_classic() +
  labs(
    x = 'δ13C (Suess corrected)',
    y = 'δ15N') +
  stat_ellipse(aes(x = Suess.d13C, y = d15N, colour = Sex, group = Sex),type = "norm");Sexniche


table(babine$Sex)



## Length inches

table(babine$Length.inches) # range from 19.5 inches to 27.5 inches
length(unique(babine$Length.inches)) # only 26 data points
# so i'll split down the middle, less than 23.5 or greater than 23.5 

NA.length <- babine %>% 
  dplyr::select(Length.inches, Suess.dC13, dN15, dS34) %>% 
  dplyr::filter(is.na(Length.inches)) %>% 
  dplyr::mutate(Length = "Unknown")

plus.23.5 <- babine %>% 
  dplyr::select(Length.inches, Suess.dC13, dN15, dS34) %>% 
  dplyr::filter(Length.inches > 23.5) %>% 
  dplyr::mutate(Length = ">23.5")

less23.5 <- babine %>% 
  dplyr::select(Length.inches, Suess.dC13, dN15, dS34) %>% 
  dplyr::filter(Length.inches >= 23.5) %>% 
  dplyr::mutate(Length = "=/<23.5")    



length.df <- rbind(less23.5,plus.23.5)



length_niche <- ggplot(length.df, aes(x = Suess.dC13, y = dN15, colour = Length)) +
  theme_classic() +
  labs(
    x = 'δ13C (Suess corrected)',
    y = 'δ15N') +
  stat_ellipse(aes(x = Suess.dC13, y = dN15, colour = Length, group = Length),type = "norm");length_niche



## Length cm

babine <- full

table(babine$Length.cm) # range from 19.5 inches to 27.5 inches
length(unique(babine$Length.cm)) # only 26 data points
# so i'll split down the middle, less than 23.5 or greater than 23.5 

NA.length <- babine %>% 
  dplyr::select(Length.cm, Suess.d13C, d15N, d34S) %>% 
  dplyr::filter(is.na(Length.cm)) %>% 
  dplyr::mutate(Length.cm = "Unknown")

plus.23.5 <- babine %>% 
  dplyr::select(Length.cm, Suess.d13C, d15N, d34S) %>% 
  dplyr::filter(Length.cm > 59.69) %>% 
  dplyr::mutate(Length.cm = ">59.69")

less23.5 <- babine %>% 
  dplyr::select(Length.cm,Suess.d13C, d15N, d34S) %>% 
  dplyr::filter(Length.cm >= 23.5) %>% 
  dplyr::mutate(Length.cm = "=/<59.69")    



length.df <- rbind(less23.5,plus.23.5)



length_niche <- ggplot(length.df, aes(x = Suess.d13C, y = dN15N, colour = Length.cm)) +
  theme_classic() +
  labs(
    x = 'δ13C (Suess corrected)',
    y = 'δ15N') +
  stat_ellipse(aes(x = Suess.d13C, y = d15N, colour = Length.cm, group = Length.cm),type = "norm");length_niche

plot(d13C ~ d34S, full)


# Age

ages <- babine %>% 
  dplyr::filter(Age %in% c(1.2,1.3))
table(ages$Age)



length_niche <- ggplot(ages, aes(x = Suess.dC13, y = dN15, colour = Age)) +
  theme_classic() +
  labs(
    x = 'δ13C (Suess corrected)',
    y = 'δ15N') +
  stat_ellipse(aes(x = Suess.dC13, y = dN15, colour = Age, group = Age),type = "norm");length_niche



#### ------------ 19 August 2021: Johnson and Schindler 2012 linear models ####
# From Johnson and Schindler: "The response vari- able was the isotope value averaged across individu- als for a year and site. Length was the average length of sockeye for that year as measured from mid-eye to tail fork. We compared 3 models of hierarchical com- plexity. First, the ‘base’ model simply estimated an intercept and did not account for effects of site or fish length. Second, the ‘site’ model has the random effect of site in addition to the intercept. Finally, the ‘length’ model has a random effect of site and length as a fixed effect. Thus, this model estimated the slope of the relationship between length and isotope and accounted for site differences. We compared these models using Akaike’s information criterion, ad- justed for small sample sizes (AICc). Lower AICc scores indicate a better fit of the data to the model, given model complexity. We calculated ΔAICc as the difference between the best model and the model in question. ΔAICc values greater than 2 indicate a model that has substantially less support than the best model (Burnham & Anderson 2002). AICc values were calculated using the ‘AICcmodavg’ package in R (R Core Development Team 2011)."

# Also see Satterfield & Finney 2002, Mackenzie et al. 2011...
rm(list=ls())
# My latest dataframe
babine <- read.csv("babine_bio_iso_suess3Aug2021.csv")
head(babine)


# Johnson used linear mixed effects models to estimate effects of site and fish lenth on average annual isotope values of sockeye salmon 
library(lme4)
library(visreg)
# Simple models eg.
mod1<- lmer(dN15 ~ Length.inches + (1|Year), babine)
summary(mod1)

visreg(mod1)

anova(mod1)

head(babine)

#### Quick bio plots ------------------------------------------------------------####
# Here,  would just like to test effects of biological variables on isotope values, like sex, age, length
library(lme4)
babine <- read.csv("babine_bio_iso_suess3Aug2021.csv") 
class(babine$Year)
babine$Year <- as.factor(babine$Year)
# first, let's look at nitrogen and length
# how many data groups do we have with length data?
rm(list=ls())

#### Length, weight, Age isotopes ####

par(mfrow = c(3,3))  
dev.off()
par(mfrow=c(1,1))
dev.off()


# NITROGEN
# Nitrogen and length
plot(d15N ~ Length.cm, data = babine, xlab = "Length.cm", ylab = "δ15N" , main = "δ15N ~ Length",  pch = 16, las = 1, cex = 1.0, col = "green4") 



# I don't see any clear pattern

# nitrogen and weight
plot(d15N ~ Weight.g, data = babine, xlab = "Weight.g", ylab = "δ15N" , main = "δ15N ~ Weight",
     pch = 16, las = 1, cex = 1.0, col = "green4") 

# N and sex
plot(d15N ~ Sex, data = babine, xlab = "Sex", ylab = "δ15N", main = "δ15N ~ Sex",  pch = 16, las = 1, cex = 1.0, col = "green4") 



# CARBON
# carbon and length
plot(d13C ~ Length.cm, data = babine, xlab = "Length.cm", ylab = "δ13C", main = "δ13C ~ Length",  pch = 16, las = 1, cex = 1.0, col = "firebrick") 

# carbon and weight
plot(d13C ~ Weight.g, data = babine, xlab = "Weight.g", ylab = "δ13C" , main = "δ13C ~ Weight",  pch = 16, las = 1, cex = 1.0, col = "firebrick") 

# carbon and sex
plot(d13C ~ Sex, data = babine, xlab = "Sex", ylab = "δ13C" , main = "δ13C ~ Sex", pch = 16, las = 1, cex = 1.0, col = "firebrick") 


# Something I'm picking up on is that it looks like there is more variance with females, check on number of male and female samples


# SULFUR
# sulfur and length
plot(d34S ~ Length.cm, data = babine, xlab = "Length.cm", ylab = "δ34S" ,   main = "δ34S ~ Length", pch = 16, las = 1, cex = 1.0, col = "royalblue") 


# sulfur and weight
plot(d34S ~ Weight.g, data = babine, xlab = "Weight.g", ylab = "δ34S" , main = "δ34S ~ Weight", pch = 16, las = 1, cex = 1.0, col = "royalblue") 

# sulfur and sex
plot(d34S ~ Sex, data = babine, xlab = "Sex", ylab = "δ34S" , main = "δ34S ~ Sex", pch = 16, las = 1, cex = 1.0, col = "royalblue") 


dev.off()  



# SULFUR (group by age)
# sulfur and length

table(babine$Age) # filter out all ages except 1.2 and 1.3
ages <- babine %>% 
  dplyr::filter(Age %in% c(1.2,1.3))
table(ages$Age)


Length <- ggplot(ages, aes(x = Length.cm, y = d34S, colour = Age)) +
  geom_point(pch = 16, size = 3) +
  geom_line(stat = "smooth", method = lm, se = FALSE, alpha = 0.5, colour = "blue") +
  theme_classic() +
  labs(
    x = 'Length (cm)',
    y = 'δ34S');Length 

Weight <- ggplot(ages, aes(x = Weight.g, y = d34S, colour = Age)) +
  geom_point(pch = 16, size = 3) +
  geom_line(stat = "smooth", method = lm, se = FALSE, alpha = 0.5, colour = "blue") +
  theme_classic() +
  labs(
    x = 'Weight (g)',
    y = 'δ34S');Weight 





# carbon and nitrogen


# and with other isotopes
par(mfrow = c(2,2))  
dev.off()
par(mfrow=c(1,1))
# nitro
Length <- ggplot(ages, aes(x = Length.cm, y = d15N, colour = Age)) +
  geom_point(pch = 16, size = 3) +
  geom_line(stat = "smooth", method = lm, se = FALSE, alpha = 0.5, colour = "blue") +
  theme_classic() +
  labs(
    x = 'Length',
    y = 'δ15N');Length 

Weight <- ggplot(ages, aes(x = Weight.g, y = d15N, colour = Age)) +
  geom_point(pch = 16, size = 3) +
  geom_line(stat = "smooth", method = lm, se = FALSE, alpha = 0.5, colour = "blue") +
  theme_classic() +
  labs(
    x = 'Weight',
    y = 'δ15N');Weight 





# carbon

Length <- ggplot(ages, aes(x = Length.cm, y = d13C, colour = Age)) +
  geom_point(pch = 16, size = 3) +
  geom_line(stat = "smooth", method = lm, se = FALSE, alpha = 0.5, colour = "blue") +
  theme_classic() +
  labs(
    x = 'Length',
    y = 'δ13C');Length 

Weight <- ggplot(ages, aes(x = Weight.g, y = d13C, colour = Age)) +
  geom_point(pch = 16, size = 3) +
  geom_line(stat = "smooth", method = lm, se = FALSE, alpha = 0.5, colour = "blue") +
  theme_classic() +
  labs(
    x = 'Weight',
    y = 'δ13C');Weight 



sex <- ggplot(full, aes(x = Length.cm, y = d15N, colour = Sex)) +
  geom_point(pch = 16, size = 3) +
  geom_line(stat = "smooth", method = lm, se = FALSE, alpha = 0.5, colour = "blue") +
  theme_classic() +
  labs(
    x = 'Length',
    y = 'δ15N');sex
#### Isotopes over time ####
# YEAR 
par(mfrow = c(2,2))  



# Isotopes over time

plot(dC13 ~ Year, data = babine, xlab = "Year", ylab = "d13C" ,  pch = 21, las = 1, cex = 1.0, col = "black") 

plot(Suess.dC13 ~ Year, data = babine, xlab = "Year", ylab = "Suess.d13C" ,  pch = 21, las = 1, cex = 1.0, col = "black") 

plot(d15N ~ Year, data = babine, xlab = "Year", ylab = "d15N" ,  pch = 21, las = 1, cex = 1.0, col = "black") 

plot(dS34 ~ Year, data = babine, xlab = "Year", ylab = "d32S" ,  pch = 21, las = 1, cex = 1.0, col = "black") 



## Mean isotopes over time


meanC.suess<- babine %>% 
  dplyr::select(Year, Suess.d13C) %>% 
  dplyr::group_by(Year) %>% 
  dplyr::summarise(Suess.mean_C13 = mean(Suess.d13C, na.rm = TRUE))

a <- ggplot(meanC.suess, aes(x = Year, y = Suess.mean_C13)) +
  geom_point() +
  geom_line() +
  theme_classic() +
  labs(
    x = 'Year',
    y = 'Mean δC13 (corrected)') +
  ylim(-18.1, -16.3);a 


# Plot uncorrected values
b <- ggplot(babine, aes(x = Year, y = dC13)) +
  geom_point() +
  theme_classic() +
  labs(
    x = 'Year',
    y = 'δC13 (uncorrected)') +
  ylim(-18.4, -15.9);b


# And mean uncorrected
meanC <- babine %>% 
  dplyr::select(Year, dC13) %>% 
  dplyr::group_by(Year) %>% 
  dplyr::summarise(mean_C13 = mean(dC13, na.rm = TRUE))

b <- ggplot(meanC, aes(x = Year, y = mean_C13)) +
  geom_point() +
  geom_line() +
  theme_classic() +
  labs(
    x = 'Year',
    y = 'Mean δC13') +
  ylim(-18.1, -16.3);b 

plotlist <-list(b,a)

library(sjPlot)
cowplot::plot_grid(plotlist = plotlist, nrow = 2, ncol = 1)



## Mean N

meanN <- babine %>% 
  dplyr::select(Year, d15N) %>% 
  dplyr::group_by(Year) %>% 
  dplyr::summarise(mean_N15 = mean(d15N,  na.rm = TRUE))

meanS <- babine %>% 
  dplyr::select(Year, dS34) %>% 
  dplyr::group_by(Year) %>% 
  dplyr::summarise(mean_S34 = mean(dS34,  na.rm = TRUE))



## Plot mean isotope values with lines
# C13
ggplot(meanC, aes(x = Year, y = mean_C13)) +
  geom_point() +
  geom_line() +
  theme_classic() +
  labs(
    x = 'Year',
    y = 'Mean δC13') 


# N15
ggplot(meanN, aes(x = Year, y = mean_N15)) +
  geom_point() +
  geom_line() +
  theme_classic() +
  labs(
    x = 'Year',
    y = 'Mean δN15') 


# S34
ggplot(meanS, aes(x = Year, y = mean_S34)) +
  geom_point() +
  geom_line() +
  theme_classic() +
  labs(
    x = 'Year',
    y = 'Mean δS34') 






#### CIs for means over time (wrong) ####
babine <- read.csv("babine_bio_iso_suess3Aug2021.csv")

library(ggplot2)

# C13
ggplot(meanC, aes(x = Year, y = mean_C13)) +
  geom_point() +
  geom_line() +
  geom_errorbar(aes(ymin=len-ci, ymax=len+ci), width=.1, position=pd) +
  theme_classic() +
  labs(
    x = 'Year',
    y = 'Mean δC13') 


# N15
ggplot(meanN, aes(x = Year, y = mean_N15)) +
  geom_point() +
  geom_line() +
  theme_classic() +
  labs(
    x = 'Year',
    y = 'Mean δN15') 


# S34
ggplot(meanS, aes(x = Year, y = mean_S34)) +
  geom_point() +
  geom_line() +
  theme_classic() +
  labs(
    x = 'Year',
    y = 'Mean δS34') 



## Summarizes data.
## Gives count, mean, standard deviation, standard error of the mean, and confidence interval (default 95%).
##   data: a data frame.
##   measurevar: the name of a column that contains the variable to be summariezed
##   groupvars: a vector containing names of columns that contain grouping variables
##   na.rm: a boolean that indicates whether to ignore NA's
##   conf.interval: the percent range of the confidence interval (default is 95%)
summarySE <- function(data=NULL, measurevar, groupvars=NULL, na.rm=TRUE,
                      conf.interval=.95, .drop=TRUE) {
  library(plyr)
  
  # New version of length which can handle NA's: if na.rm==T, don't count them
  length2 <- function (x, na.rm=FALSE) {
    if (na.rm) sum(!is.na(x))
    else       length(x)
  }
  
  # This does the summary. For each group's data frame, return a vector with
  # N, mean, and sd
  datac <- ddply(data, groupvars, .drop=.drop,
                 .fun = function(xx, col) {
                   c(N    = length2(xx[[col]], na.rm=na.rm),
                     mean = mean   (xx[[col]], na.rm=na.rm),
                     sd   = sd     (xx[[col]], na.rm=na.rm)
                   )
                 },
                 measurevar
  )
  
  # Rename the "mean" column    
  datac <- rename(datac, c("mean" = measurevar))
  
  datac$se <- datac$sd / sqrt(datac$N)  # Calculate standard error of the mean
  
  # Confidence interval multiplier for standard error
  # Calculate t-statistic for confidence interval: 
  # e.g., if conf.interval is .95, use .975 (above/below), and use df=N-1
  ciMult <- qt(conf.interval/2 + .5, datac$N-1)
  datac$ci <- datac$se * ciMult
  
  return(datac)
}



# use function
tgc <- summarySE(babine, measurevar="dC13")



# Now make plot!
library(ggplot2)

ggplot(babine, aes(x=Year, y=dC13)) + 
  geom_errorbar(aes(ymin=dC13-se, ymax=dC13+se), width=.1) +
  geom_line() +
  geom_point()




## box plot

#babine$Year  <- as.factor(babine$Year)

ggplot(babine, aes(x=Year, y=Suess.dC13)) +
  geom_boxplot() +
  theme_classic() +
  labs(
    x = 'Year',
    y = 'δC13') 

ggplot(babine, aes(x=Year, y=d15N)) +
  geom_boxplot() +
  theme_classic() +
  labs(
    x = 'Year',
    y = 'δ15N') 

ggplot(babine, aes(x=Year, y=dS34)) +
  geom_boxplot() +
  theme_classic() +
  labs(
    x = 'Year',
    y = 'δ34S') 

## a la dolph
dev.off()
rm(list = ls())
babine <- read.csv("babine_updated_8oct2021.csv")

meanN <- babine %>% 
  dplyr::select(Year, d15N) %>% 
  dplyr::group_by(Year) %>% 
  dplyr::summarise(mean_N15 = mean(d15N,  na.rm = TRUE))


plot(mean_N15 ~ Year, meanN, pch = 16, ylim = c(9.5, 12), xlab="Year", ylab=" Yearly mean δ15N", type = "b")

newdat <- expand.grid()

ln <- lm(mean_N15 ~ Year, meanN)


pred_meanN <- as.data.frame(cbind(meanN, 
                                  mean_N = predict(ln, meanN, re.form = NULL, type = "response", se.fit = TRUE)))
# Yes this is it@ gives me the fit!

ggplot(pred_meanN, aes(x=Year, y=mean_N15)) + 
  geom_point(size = 2, col = "black") +
  geom_line(lty = "dashed") +
  geom_errorbar(aes(ymin=mean_N15-mean_N.se.fit, ymax=mean_N15+mean_N.se.fit), width=1) +
  labs(x = "Year", y = "δ15N") +
  ggtitle("mean δ15N ~ Year") +
  theme_classic()  # Yesss!


# carbon
names(meanC.suess)

meanC.suess<- babine %>% 
  dplyr::select(Year, Suess.d13C) %>% 
  dplyr::group_by(Year) %>% 
  dplyr::summarise(Suess.meanC13 = mean(Suess.d13C, na.rm = TRUE))


pred_meanC <- as.data.frame(cbind(meanC.suess, 
                                  mean_C = predict(ln, meanC.suess, re.form = NULL, type = "response", se.fit = TRUE)))


names(pred_meanC)

ggplot(pred_meanC, aes(x=Year, y=Suess.meanC13)) + 
  geom_point(size = 2, col = "black") +
  geom_line(lty = "dashed") +
  geom_errorbar(aes(ymin=Suess.meanC13-mean_C.se.fit, ymax=Suess.meanC13+mean_C.se.fit), width=1) +
  labs(x = "Year", y = "δ13C") +
  ggtitle("mean δ13C ~ Year") +
  theme_classic()  # Yesss!




#### Plot data over time with standard error of the mean ####



# make a new dataframe with each year, the mean N15 of that year, and then the sd of that mean N15
newdat <- babine %>% 
  group_by(Year) %>% 
  summarize(mean_N = mean(d15N, na.rm = TRUE),
            sd_N = sd(d15N, na.rm = TRUE),
            N_N = n(),
            se = sd_N/sqrt(N_N),
            upper_limit = mean_N+se,
            lower_limit = mean_N-se
  )



ggplot(newdat, aes(x=Year, y=mean_N)) + 
  geom_point(size = 2, col = "black") +
  geom_line(lty = "dashed") +
  geom_errorbar(aes(ymin=mean_N - se, ymax=mean_N + se), width=1) +
  labs(x = "Year", y = "mean δ15N") +
  theme_classic() 




## carbon
newdat <- babine %>% 
  group_by(Year) %>% 
  summarize(mean = mean(Suess.d13C, na.rm = TRUE),
            sd = sd(Suess.d13C, na.rm = TRUE),
            N_N = n(),
            se = sd/sqrt(N_N),
            upper_limit = mean+se,
            lower_limit = mean-se
  )



ggplot(newdat, aes(x=Year, y=mean)) + 
  geom_point(size = 2, col = "black") +
  geom_line(lty = "dashed") +
  geom_errorbar(aes(ymin=mean - se, ymax=mean + se), width=1) +
  labs(x = "Year", y = "mean δ13C") +
  theme_classic() 
####



# Sulphur
newdat <- babine %>% 
  group_by(Year) %>% 
  summarize(mean = mean(d34S, na.rm = TRUE),
            sd = sd(d34S, na.rm = TRUE),
            N_N = n(),
            se = sd/sqrt(N_N),
            upper_limit = mean+se,
            lower_limit = mean-se
  )



ggplot(newdat, aes(x=Year, y=mean)) + 
  geom_point(size = 2, col = "black") +
  geom_line(lty = "dashed") +
  geom_errorbar(aes(ymin=mean - se, ymax=mean + se), width=1) +
  labs(x = "Year", y = "mean δ34S") +
  theme_classic() 





#### Isotope and scale growth rates multiplot ####

# mai = bottom, left, top, right

par(mfrow = c(3,4), omi=c(0.6,0.6,0,0))  

par(mfrow=c(1,1))



# Growth rates and Nitrogen

# first year freshwater
par(mai=c(0.2,0.2,0.2,0.2))
plot(d15N ~ FW1.GW, data = babine,  xlab = "",  pch = 16, las = 1, cex = 1.0, col = "green4", xaxt = "n") 

# first year saltwater
par(mai=c(0.2,0.2,0.2,0.2))
plot(d15N ~ SW1.GW, data = babine, xlab = "",  ylab = "", pch = 16, las = 1, cex = 1.0, col = "green4",  xaxt = "n", yaxt = "n") 


# second year salt
par(mai=c(0.2,0.2,0.2,0.2))
plot(d15N ~ SW2.GW, data = babine, xlab = "",  ylab = "", pch = 16, las = 1, cex = 1.0, col = "green4",  xaxt = "n", yaxt = "n")

# 3rd year salt
par(mai=c(0.2,0.2,0.2,0.2))
plot(d15N ~ SW3.GW, data = babine, xlab = "",  ylab = "", pch = 16, las = 1, cex = 1.0, col = "green4",  xaxt = "n", yaxt = "n")



# Growth rates and carbon

# first year freshwater
par(mai=c(0.2,0.2,0.2,0.2))
plot(dC13 ~ FW1.GW, data = babine,    xlab = "", pch = 16, las = 1, cex = 1.0, col = "firebrick",  xaxt = "n") 

# first year saltwater
par(mai=c(0.2,0.2,0.2,0.2))
plot(dC13 ~ SW1.GW, data = babine, xlab = "",  ylab = "",  pch = 16, las = 1, cex = 1.0, col = "firebrick",  xaxt = "n", yaxt = "n") 

# second year salt
par(mai=c(0.2,0.2,0.2,0.2))
plot(dC13 ~ SW2.GW, data = babine, xlab = "",  ylab = "",  pch = 16, las = 1, cex = 1.0, col = "firebrick",  xaxt = "n", yaxt = "n") 

# 3rd year salt
par(mai=c(0.2,0.2,0.2,0.2))
plot(dC13 ~ SW3.GW, data = babine, xlab = "",  ylab = "",  pch = 16, las = 1, cex = 1.0, col = "firebrick",  xaxt = "n", yaxt = "n") 



# Growth rates and sulfur

# first year freshwater
par(mai=c(0.2,0.2,0.2,0.2))
plot(dS34 ~ FW1.GW, data = babine,  pch = 16, las = 1, cex = 1.0, col = "royalblue") 

# first year saltwater
par(mai=c(0.2,0.2,0.2,0.2))
plot(dS34 ~ SW1.GW, data = babine, ylab = "", pch = 16, las = 1, cex = 1.0, col = "royalblue", yaxt = "n") 

# second year salt
par(mai=c(0.2,0.2,0.2,0.2))
plot(dS34 ~ SW2.GW, data = babine, ylab = "", pch = 16, las = 1, cex = 1.0, col = "royalblue",  yaxt = "n") 

# third year salt
par(mai=c(0.2,0.2,0.2,0.2))
plot(dS34 ~ SW3.GW, data = babine, ylab = "", pch = 16, las = 1, cex = 1.0, col = "royalblue", yaxt = "n") 


lm <- lm(d34S ~ SW3.GW, full)
abline(lm)
summary(lm)


# So this is interesting?, it looks like sulfur is more related to the last year of growth in salt water then the first year.. I guess because this effects the ratio of sulfur more? I need to think about this

# To do this properly I will maybe need to transform all growth rates into proportions of scale growth?

# second year salt
par(mai=c(0.2,0.2,0.2,0.2))
plot(dS34 ~ SW3.GW, data = babine, ylab = "", pch = 16, las = 1, cex = 1.0, col = "royalblue") 




#### 
class(babine$Year)

#just check sex and scale growth correlation
# first year freshwater
par(mai=c(0.2,0.2,0.2,0.2))
plot(FW1.GW ~ Sex, data = babine,  pch = 16, las = 1, cex = 1.0, col = "royalblue") 

# first year saltwater
par(mai=c(0.2,0.2,0.2,0.2))
plot(SW1.GW ~ Sex, data = babine, ylab = "", pch = 16, las = 1, cex = 1.0, col = "royalblue", yaxt = "n") 

# second year salt
par(mai=c(0.2,0.2,0.2,0.2))
plot(SW2.GW ~ Sex, data = babine, ylab = "", pch = 16, las = 1, cex = 1.0, col = "royalblue",  yaxt = "n") 

# third year salt
par(mai=c(0.2,0.2,0.2,0.2))
plot(SW3.GW ~ Sex, data = babine, ylab = "", pch = 16, las = 1, cex = 1.0, col = "royalblue", yaxt = "n") 



#YEAR
par(mfrow=c(1,4))

# first year freshwater

plot(FW1.GW ~ Year, data = babine,  pch = 16, las = 1, cex = 1.0, col = "royalblue") 

# first year saltwater
par(mai=c(0.2,0.2,0.2,0.2))
plot(SW1.GW ~ Year, data = babine, ylab = "", pch = 16, las = 1, cex = 1.0, col = "royalblue") 

# second year salt
par(mai=c(0.2,0.2,0.2,0.2))
plot(SW2.GW ~ Year, data = babine, ylab = "", pch = 16, las = 1, cex = 1.0, col = "royalblue") 

# third year salt
par(mai=c(0.2,0.2,0.2,0.2))
plot(SW3.GW ~ Year, data = babine, ylab = "", pch = 16, las = 1, cex = 1.0, col = "royalblue") 

#YEAR
# first year freshwater
par(mai=c(0.2,0.2,0.2,0.2))
plot(FW1.GW ~ Year, data = babine,  pch = 16, las = 1, cex = 1.0, col = "royalblue") 

# first year saltwater
par(mai=c(0.2,0.2,0.2,0.2))
plot(SW1.GW ~ Year, data = babine, ylab = "", pch = 16, las = 1, cex = 1.0, col = "royalblue", yaxt = "n") 

# second year salt
par(mai=c(0.2,0.2,0.2,0.2))
plot(SW2.GW ~ Year, data = babine, ylab = "", pch = 16, las = 1, cex = 1.0, col = "royalblue",  yaxt = "n") 

# third year salt
par(mai=c(0.2,0.2,0.2,0.2))
plot(SW3.GW ~ Year, data = babine, ylab = "", pch = 16, las = 1, cex = 1.0, col = "royalblue", yaxt = "n") 


# Could i match growth rates of the same year from fish from different years?  Ie. fish (1.3) marine growth year 2 from fish 2012 , with marine growth from fish in its 1st year of marine life from 20** (same sea year different year of fish life history)





#### Run timing ####

## Dates: I edited the file "babine_bio_iso_suess3Aug2021.csv" and changed the dates to a single format, year-month-day, with NAs in the places where there was no sampling month or day recorded. Many of the middle period samples are also lacking in date data (as well as biodata). Some samples had date.caught as a range, (eg. July 12-13), in these cases I entered date.caught as the first date of the range (eg. July 12), and added a comment in the Date.notes column explaining the actual range of dates. 

# Date-edited csv:
babine.date <- ("babine_bio_iso_suess_dates_30sep2021.csv")


names(babine)
head(babine)



#### PDO (also, NA check of updated dataframe w/ dates) ####

## NEXT try PDO at year of ocean entry and year before

# Here is the PDO data, downloaded from https://www.ncdc.noaa.gov/teleconnections/pdo/

PDO.df <- read.csv("PDO_data_NOAA_raw.csv")
head(PDO.df)


# For a quick look, I want to lag PDO by one year and see how it alligns with scale isotope levels. ie. merge PDO data with isotope data, 1920 fish with 1919 PDO

# Step 1.  Fix dates in PDO
head(PDO.df)
names(PDO.df)
class(PDO.df$Date)
PDO.df$Date <- as.character(PDO.df$Date)

# Subset year
PDO.df1 <- PDO.df %>% 
  dplyr::mutate(Year = substr(Date, 1, 4))

head(PDO.df1)
# Subset month
PDO.df2 <- PDO.df1 %>% 
  dplyr::mutate(Month = substr(Date, 5, 6)) %>% 
  dplyr::select(!Date) %>% 
  dplyr::select(Year, Month, PDO)

head(PDO.df2)

#write.csv(PDO.df2, "PDO.date.edited.30Sept2021.csv", row.names = FALSE)


#rm(list=ls())

# Step 2.  Average isotope values by year
babine <- read.csv("babine_bio_iso_suess_dates_30sep2021.csv")
names(babine)
head(babine)

# Check NAs in our data #
sapply(babine, function(x) sum(is.na(x)))
# So we have 10 NAs for all isotopes, missing 14 growth datas, age from 10, weight and length from 97 

# another way to check
map(babine, ~sum(is.na(.)))



# So make sure to filter for NA values here

meanC <- data %>% 
  dplyr::select(Year, dC13) %>% 
  dplyr::filter(!is.na(dC13)) %>% 
  dplyr::group_by(Year) %>% 
  dplyr::summarise(mean_C13 = mean(dC13))

meanCSuess <- data %>% 
  dplyr::select(Year, Suess.dC13) %>%
  dplyr::filter(!is.na(Suess.dC13)) %>% 
  dplyr::group_by(Year) %>% 
  dplyr::summarise(mean_SuessC13 = mean(Suess.dC13))

meanN <- data %>% 
  dplyr::select(Year, d15N) %>%
  dplyr::filter(!is.na(d15N)) %>% 
  dplyr::group_by(Year) %>% 
  dplyr::summarise(mean_N15 = mean(d15N))

meanS <- data %>% 
  dplyr::select(Year, dS34) %>% 
  dplyr::filter(!is.na(dS34)) %>% 
  dplyr::group_by(Year) %>% 
  dplyr::summarise(mean_S34 = mean(dS34))



# Now merge all together
mean.iso <- merge(meanC, meanCSuess, by = "Year")
mean.iso1 <- merge(mean.iso, meanN, by = "Year")
mean.iso2 <- merge(mean.iso1, meanS, by = "Year")



#write.csv(mean.iso2, "mean.iso.yr.30sept2021.csv", row.names = FALSE)




# Step. 3 Merge dataframes (lag by 1 year)
#rm(list=ls())

mean.iso <- read.csv("mean.iso.yr.30sept2021.csv")
PDO <- read.csv("PDO.date.edited.30Sept2021.csv")


names(mean.iso)
unique(mean.iso$Year)


# For isotope data, we have the years: 1913 1918 1923 1933 1937 1943 1947 1957 1968 1982 1988 1993 1998 2000 2005 2010 2014

library(dplyr)

# Lag iso data
mean.iso <- mean.iso %>% 
  dplyr::mutate(Year_plus_1 = Year + 1)



# OK NEXT STEP (sept 30): We need to figure out which months of PDO we are going to use, or if we will just take the mean of yearly PDO, then we will make a figure of PDO standard deviation over time and isotope SD over time, and also look at some plots of PDO vs isotopes. 



# quick and dirty mean year
pdo <- PDO %>% 
  dplyr::select(Year, PDO) %>%
  dplyr::group_by(Year) %>% 
  dplyr::summarise(mean_PDO = mean(PDO))


pdo.iso.dirty <- merge(mean.iso, pdo, by.x = "Year_plus_1")

# plot dirty
par(mfrow=c(1,1))


plot(mean_N15 ~ mean_PDO, data = pdo.iso.dirty, xlab = "PDO", ylab = "d15N" ,  pch = 16, las = 1, cex = 1.0, col = "firebrick") 

dirty.lm <-lm(mean_N15 ~ mean_PDO, data = pdo.iso.dirty)
abline(dirty.lm)



plot(mean_C13 ~ mean_PDO, data = pdo.iso.dirty, xlab = "PDO", ylab = "dC13" ,  pch = 16, las = 1, cex = 1.0, col = "steelblue") 

dirty.lm <-lm(mean_C13 ~ mean_PDO, data = pdo.iso.dirty)
abline(dirty.lm)




plot(mean_S34 ~ mean_PDO, data = pdo.iso.dirty, xlab = "PDO", ylab = "dS34" ,  pch = 16, las = 1, cex = 1.0, col = "orange3") 

dirty.lm <-lm(mean_S34 ~ mean_PDO, data = pdo.iso.dirty)
abline(dirty.lm)



par(mfrow=c(1,1))



## Try SD of PDO and isotope comp. over time 



# SD PDO and lag 1 year
names(PDO)
head(PDO)
class(PDO$PDO)

PDO$Year <- as.numeric(PDO$Year)

sd_PDO <- PDO %>% 
  dplyr::select(Year, Month, PDO) %>% 
  dplyr::group_by(Year) %>% 
  dplyr::summarise(sd_PDO = sd(PDO)) %>% 
  dplyr::mutate(Year_plus_1 = Year + 1)

head(sd_PDO)

# SD iso
names(babine)


sd_C13 <- babine %>% 
  dplyr::select(Year, dC13) %>% 
  dplyr::filter(!is.na(dC13)) %>% 
  dplyr::group_by(Year) %>% 
  dplyr::summarise(sd_dC13 = sd(dC13))

sd_N15 <- babine %>% 
  dplyr::select(Year, d15N) %>% 
  dplyr::filter(!is.na(d15N)) %>% 
  dplyr::group_by(Year) %>% 
  dplyr::summarise(sd_d15N = sd(d15N))

sd_S34 <- babine %>% 
  dplyr::select(Year, dS34) %>% 
  dplyr::filter(!is.na(dS34)) %>% 
  dplyr::group_by(Year) %>% 
  dplyr::summarise(sd_dS34 = sd(dS34))




sd_iso_pdo <- merge(sd_C13, sd_PDO, by.x = "Year", by.y = "Year_plus_1")

sd_iso_pdo <- sd_iso_pdo %>% 
  dplyr::rename(Year_PDO_lagged_1 = Year.y)

sd_iso_pdo1 <- merge(sd_iso_pdo, sd_N15, by = "Year")

sd_iso_pdo2 <- merge(sd_iso_pdo1, sd_S34, by = "Year")


sd_iso_pdo <- sd_iso_pdo2

names(sd_iso_pdo)


#write.csv(sd_iso_pdo, "sd_iso_PDOyrly_4oct2021.csv", row.names = FALSE)

# Visualize
#rm(list=ls())

sd_iso_pdo <- read.csv("sd_iso_PDOyrly_4oct2021.csv")


# over time (this is a bit of a hack but its ok)
babine <- read.csv("babine_bio_iso_suess_dates_30sep2021.csv")

sd_C13 <- babine %>% 
  dplyr::select(Year, dC13) %>% 
  dplyr::filter(!is.na(dC13)) %>% 
  dplyr::group_by(Year) %>% 
  dplyr::summarise(sd = sd(dC13))

isoC <- rep("C13", 17)

sd_C13$Isotope <- isoC


sd_N15 <- babine %>% 
  dplyr::select(Year, d15N) %>% 
  dplyr::filter(!is.na(d15N)) %>% 
  dplyr::group_by(Year) %>% 
  dplyr::summarise(sd = sd(d15N))


isoN <- rep("N15", 17)

sd_N15$Isotope <- isoN



sd_S34 <- babine %>% 
  dplyr::select(Year, dS34) %>% 
  dplyr::filter(!is.na(dS34)) %>% 
  dplyr::group_by(Year) %>% 
  dplyr::summarise(sd = sd(dS34))


isoS <- rep("S34", 17)

sd_S34$Isotope <- isoS




# now combine for plotting and merge with PDO

IsotopeSD <- rbind(sd_C13, sd_N15, sd_S34)
head(IsotopeSD)

PDO <- read.csv("PDO.date.edited.30Sept2021.csv")

sd_PDO <- PDO %>% 
  dplyr::select(Year, Month, PDO) %>% 
  dplyr::group_by(Year) %>% 
  dplyr::summarise(sd = sd(PDO)) %>% 
  dplyr::mutate(Year_plus_1 = Year + 1)

PDO_d <- rep("PDO", 168)

sd_PDO$Isotope <- PDO_d


# filter PDO data for number of years that we have iso data for
unique(IsotopeSD$Year)

sd_PDO1 <- sd_PDO %>% 
  dplyr::filter(Year %in% c("1913", "1918", "1923", "1933", "1937", "1943", "1947", "1957", "1968", "1982", "1988", "1993", "1998", "2000", "2005", "2010", "2014"))

# select for matching columns
names(sd_PDO1)
names(IsotopeSD)

# switching year_plus1 to year for plotting purposes
sd_PDO2 <- sd_PDO1 %>% 
  dplyr::select(!Year) %>% 
  dplyr::rename(Year = Year_plus_1)

PDO_SD_quickDF <- rbind(IsotopeSD, sd_PDO2)



names(PDO_SD_quickDF)


## Add specififc colours
group.colors <- c(C13 = "firebrick", S34 = "steelblue", N15 ="orange3", PDO = "black")

# levels 
PDO_SD_quickDF$Isotope <- factor(PDO_SD_quickDF$Isotope, levels = c("C13", "N15", "S34", "PDO"))

sdplot_fun <- ggplot(PDO_SD_quickDF, aes(x = Year, y = sd)) +
  geom_line(aes(linetype = Isotope, colour = Isotope)) +
  geom_point(aes(colour = Isotope)) +
  theme_classic() +
  labs(
    x = "Year",
    y = "SD") +
    scale_colour_manual(values=group.colors) +
  scale_linetype_manual(values = c(C13 = "solid", S34 = "solid", N15 ="solid", PDO = "dashed" ));sdplot_fun



# N15 PDO
plot(sd_d15N ~ sd_PDO, data = sd_iso_pdo, xlab = "PDO", ylab = "N15" ,  pch = 16, las = 1, cex = 1.0, col = "orange3") 


#### more pdo (winter) ####


# mean year (winter)

pdo <- PDO %>% 
  dplyr::select(Year, Month, PDO) %>%
  dplyr::filter(Year %in% c("1913", "1918", "1923", "1933", "1937", "1943", "1947", "1957", "1968", "1982", "1988", "1993", "1998", "2000", "2005", "2010", "2014")) %>% 
  dplyr::filter(Month %in% c("9","10","11","12","1","2","3")) %>% 
  dplyr::group_by(Year) %>% 
  dplyr::summarise(mean_PDO = mean(PDO))
#write.csv(pdo, "mean.winter.pdo.csv", row.names = FALSE)

names(mean.iso)
names(pdo)
unique(pdo$Year)
unique(mean.iso$Year_plus_1)


pdo.iso <- merge(mean.iso, pdo, by.x = "Year_plus_1", by.y = "Year", )


# now simple model

str(pdo.iso)

pdo.iso$Year <- as.factor(pdo.iso$Year)


#### sd isotope and PDO plot, regime shifts ####



# SD PDO and lag 1 year
names(PDO)
head(PDO)
class(PDO$PDO)

PDO$Year <- as.numeric(PDO$Year)

sd_PDO <- PDO %>% 
  dplyr::select(Year, Month, PDO) %>% 
  dplyr::filter(Month %in% c("9","10","11","12","1","2","3")) %>% 
  dplyr::group_by(Year) %>% 
  dplyr::summarise(sd_PDO = sd(PDO)) %>% 
  dplyr::mutate(Year_plus_1 = Year + 1)

head(sd_PDO)

# SD iso
names(babine)


sd_C13 <- babine %>% 
  dplyr::select(Year, dC13) %>% 
  dplyr::filter(!is.na(dC13)) %>% 
  dplyr::group_by(Year) %>% 
  dplyr::summarise(sd_dC13 = sd(dC13))

sd_N15 <- babine %>% 
  dplyr::select(Year, dN15) %>% 
  dplyr::filter(!is.na(dN15)) %>% 
  dplyr::group_by(Year) %>% 
  dplyr::summarise(sd_dN15 = sd(dN15))

sd_S34 <- babine %>% 
  dplyr::select(Year, dS34) %>% 
  dplyr::filter(!is.na(dS34)) %>% 
  dplyr::group_by(Year) %>% 
  dplyr::summarise(sd_dS34 = sd(dS34))




sd_iso_pdo <- merge(sd_C13, sd_PDO, by.x = "Year", by.y = "Year_plus_1")

sd_iso_pdo <- sd_iso_pdo %>% 
  dplyr::rename(Year_PDO_lagged_1 = Year.y)

sd_iso_pdo1 <- merge(sd_iso_pdo, sd_N15, by = "Year")

sd_iso_pdo2 <- merge(sd_iso_pdo1, sd_S34, by = "Year")


sd_iso_pdo <- sd_iso_pdo2

names(sd_iso_pdo)


# Visualize
#rm(list=ls())



# over time (this is a bit of a hack but its ok)
babine <- read.csv("babine_bio_iso_suess_dates_30sep2021.csv")

sd_C13 <- babine %>% 
  dplyr::select(Year, dC13) %>% 
  dplyr::filter(!is.na(dC13)) %>% 
  dplyr::group_by(Year) %>% 
  dplyr::summarise(sd = sd(dC13))

isoC <- rep("C13", 17)

sd_C13$Isotope <- isoC


sd_N15 <- babine %>% 
  dplyr::select(Year, d15N) %>% 
  dplyr::filter(!is.na(d15N)) %>% 
  dplyr::group_by(Year) %>% 
  dplyr::summarise(sd = sd(d15N))




sdplot_fun <- ggplot(sd_N15, aes(x = Year, y = sd)) +
  geom_line() +
  geom_point() +
  theme_classic() +
  labs(
    x = "Year",
    y = "SD δN15");sdplot_fun 



# add lines

dates_vline <- as.Date(c("1924", "2000"))                 # Define positions of vline
dates_vline <- which(data$my_dates %in% dates_vline)

sdplot_fun <- ggplot(sd_N15, aes(x = Year, y = sd)) +
  geom_line() +
  geom_point() +
  theme_classic() +
  labs(
    x = "Year",
    y = "SD δN15") +
  geom_vline(xintercept=c(1924,2000), linetype="dotted")
  ;sdplot_fun 


plot(sd ~ Year, data = sd_N15, xlab = "Year",  ylab = "SD δN15", pch = 16, las = 1, cex = 1.0, col = "black", type = "o", lty = "solid")



abline(v=c(1925,1946,1999), col="blue")
abline(v=c(1939, 1976, 2008), col="red")
abline(v=1989, col="grey")

# shade in between line

polygon(c(1925, rev(1939)), c(0.4, rev(1.0)),
        col = "#6BD7AF", lty = 0)





### plot pdo mean

plot(mean_PDO ~ Year, data = full, xlab = "Year",  ylab = "mean PDO", pch = 16, las = 1, cex = 1.0, col = "black", type = "o", lty = "solid")
plot(mean.N ~ mean_PDO , data = full, xlab = "mean PDO",  ylab = "mean d15N", pch = 16, las = 1, cex = 1.0, col = "black")
lm <- lm(mean.N ~ mean_PDO, full)
abline(lm)

####  pdo and isotopes (better) ####
rm(list=ls())
PDO <- read.csv("PDO.date.edited.30Sept2021.csv")
babine <- read.csv("babine_updated_8oct2021.csv")



# mean winter pdo (sept - march) 
mean.winter.pdo <- PDO %>% 
  dplyr::select(Year, Month, PDO) %>%
  dplyr::filter(Month %in% c("9","10","11","12","1","2","3")) %>% 
  dplyr::group_by(Year) %>% 
  dplyr::summarise(mean_PDO = mean(PDO))


# sd winter pdo
sd.winter.pdo <- PDO %>% 
  dplyr::select(Year, Month, PDO) %>%
  dplyr::filter(Month %in% c("9","10","11","12","1","2","3")) %>% 
  dplyr::group_by(Year) %>% 
  dplyr::summarise(sd_PDO = sd(PDO))


# merge sd and mean PDO
sd.mean.winter.pdo <- merge(sd.winter.pdo, mean.winter.pdo, by = "Year")
# write.csv(sd.mean.winter.pdo, "sd.mean.winter.pdo.11oct2021.csv", row.names = FALSE)

# sd.iso
sd.iso <- babine %>% 
  dplyr::select(Year, Suess.d13C, d15N, d34S) %>%
  dplyr::group_by(Year) %>%
  dplyr::mutate(sd.N = sd(d15N, na.rm = TRUE), 
                sd.C = sd(Suess.d13C, na.rm = TRUE), sd.S = sd(d34S, na.rm = TRUE)) %>% 
  dplyr::select(Year, sd.C, sd.N, sd.S) %>% 
  dplyr::group_by(Year, sd.C, sd.N, sd.S) %>% 
  dplyr::summarise()
  
  
# mean.iso
mean.iso <- babine %>% 
  dplyr::select(Year, Suess.d13C, d15N, d34S) %>%
  dplyr::group_by(Year) %>%
  dplyr::mutate(mean.N = mean(d15N, na.rm = TRUE), 
                mean.C = mean(Suess.d13C, na.rm = TRUE), mean.S = mean(d34S, na.rm = TRUE)) %>% 
  dplyr::select(Year, mean.C, mean.N, mean.S) %>% 
  dplyr::group_by(Year, mean.C, mean.N, mean.S) %>% 
  dplyr::summarise()



# merge iso
mean.sd.iso <- merge(mean.iso, sd.iso, by = "Year")
#write.csv(mean.sd.iso, "mean.sd.iso.11oct2021.csv", row.names = FALSE)




### merge iso with 1 year lagged PDO
sd.mean.winter.pdo.yrplus <- sd.mean.winter.pdo %>% 
  dplyr::mutate(yr_plus_1 = Year + 1) %>% 
  dplyr::rename(PDOyear = Year)

pdo.iso <- merge(mean.sd.iso, sd.mean.winter.pdo.yrplus, by.x = "Year", by.y = "yr_plus_1")
                   
#write.csv(pdo.iso, "pdo.iso.merge.11oct2021.csv", row.names = FALSE)


# ** Come back tot his, taking too much time. 2 yr average pdo: averaged over the last two years at sea

# This is not fast but will work!

PDO <- read.csv("PDO.date.edited.30Sept2021.csv")



# here i took the two years before each fish sampling year
pdo2yr <- PDO %>%  
  dplyr::filter(Month %in% c("9","10","11","12","1","2","3")) %>% 
dplyr::filter(Year %in% c("1911", "1912", "1916", "1917", "1921", "1922", "1931", "1932", "1935", "1936", "1941", "1942", "1955", "1956", "1966", "1967", "1980","1981", "1986", "1987", "1991", "1992", "1996", "1997","1998","1999", "2003", "2004", "2008", "2009", "2012", "2013")) %>% 
  dplyr::group_by(Year) %>% 
  dplyr::summarise(mean_PDO = mean(PDO)) %>% 
  dplyr::mutate(mean_last2yr = (mean_PDO + lead(mean_PDO, n=1) ) / 2 )


# try this in a better way. I think I could just make a for loop that runs through and takes the last 2 year mean with the lag function but I don't really have much time to figure it out rght now! later i will
  
 # here are our fish data years ( i think i would make this a string that I'd then pass through the for loop)
fishyear <- c("1913", "1918", "1923", "1933", "1937", "1943", "1947", "1957", "1968", "1982", "1988", "1993", "1998", "2000", "2005", "2010", "2014")

by_ticker %>% 
  mutate(mean_last2y_excl = ( median_data + lag(median_data) ) / 2 )



###try this
data <- PDO %>%  
  dplyr::filter(Month %in% c("9","10","11","12","1","2","3")) %>% 
  dplyr::filter(Year %in% c("1911", "1912", "1916", "1917", "1921", "1922", "1931", "1932", "1935", "1936", "1941", "1942", "1955", "1956", "1966", "1967", "1980","1981", "1986", "1987", "1991", "1992", "1996", "1997","1998","1999", "2003", "2004", "2008", "2009", "2012", "2013")) %>% 
  dplyr::group_by(Year) %>% 
  dplyr::summarise(mean_PDO = mean(PDO))

window_size <- 2 # number of years to average over


##data$mean2yr <- data$mean_PDO, 
 #                        rep(1,window_size)/window_size,  
 #                         sides = 1) 

#### plot PDO iso ####
rm(list=ls())


pdo.iso <- read.csv("pdo.iso.merge.11oct2021.csv")

dev.off()
plot(mean.N ~ mean_PDO, pdo.iso) # dont see much
plot(sd.N ~ sd_PDO, pdo.iso) # hm, maybe a pattern, as PDO sd increases it looks like N 

# N
plot(sd.N ~ sd_PDO, data = pdo.iso, xlab = "sd.PDO", ylab = "sd.δ15N", main = "sd.δ15N ~ sd.PDO",
     pch = 16, las = 1, cex = 1.0, col = "royalblue") 

lm <- lm(sd.N ~ sd_PDO, data = pdo.iso)
summary(lm) # r2 = 0.03861

# C
plot(sd.C ~ sd_PDO, data = pdo.iso, xlab = "sd.PDO", ylab = "sd.δ13C", main = "sd.δ13C ~ sd.PDO",
     pch = 16, las = 1, cex = 1.0, col = "royalblue") 




# plot yearl sd against mean

# N
plot(sd.N ~ mean_PDO, data = pdo.iso, ylab = "sd.δ15N", xlab =  "mean.PDO", main = "sd.δ15N ~ mean winter PDO",
     pch = 16, las = 1, cex = 1.0, col = "royalblue") 
# C
plot(sd.C ~ mean_PDO, data = pdo.iso, xlab = "PDO", ylab = "δ13C", main = "sd.δ13C ~ mean winter PDO",
     pch = 16, las = 1, cex = 1.0, col = "royalblue") 


# mean N ~ sd PDO
plot(mean.N ~ sd_PDO, data = full, ylab = "mean δ15N", xlab =  "sd.PDO", main = "mean.δ15N ~ sd winter PDO",
     pch = 16, las = 1, cex = 1.0, col = "royalblue") 

#### merge Babine df with yearly sd and mean PDO and sd and mean iso and MODEL ####
head(babine)
head(pdo.iso)

pdo1 <- pdo.iso %>% 
  dplyr::select(PDOyear, sd_PDO, mean_PDO, Year)


# All PDO is from the year before the fish sampling
babine.pdo.merge <- merge(babine, pdo.iso, by = "Year") # nice celeste!

# add C:N column
babine.pdo.merge <- babine.pdo.merge %>% 
  dplyr::mutate(CN.ratio = d13C/d15N )
#write.csv(babine.pdo.merge, "very.full.babine.df.12oct2021.csv", row.names = FALSE)




# quick model
library(lme4)
library(sjPlot)
full <- read.csv("very.full.babine.df.12oct2021.csv")


#filter for ages
full <- full %>% 
  dplyr::filter(Age %in% c(1.2,1.3))


# here I would like to model the effects of mean winter PDO against d15N, with fixed effects of length, age, sex

mod1 <- lmer(d15N ~ mean_PDO + Age + Length.cm + Sex + (1|Year),  data=full)
summary(mod1)


set_theme(base = theme_classic())

plot_model(mod1,  type = "std") # not much going on!


#  CN ratio
mod2 <- lmer(CN.ratio ~ mean_PDO + Age + Length.cm + Sex + (1|Year),  data=full)
plot_model(mod2,  type = "std", title = "CN.ratio ~ mean_PDO + Age + Length.cm + (1|Year)")


#  check N15 against sd **
mod3 <- lmer(d15N ~ sd_PDO + Age + Length.cm + Sex + (1|Year),  data=full)
plot_model(mod3,  type = "std", title = "δN15 ~ SD PDO + Age + Length.cm + Sex + (1|Year)") # looks like an effect!

summary(mod3)


mod4 <- lmer(d13C ~ sd_PDO + Age + Length.cm + Sex + (1|Year),  data=full)
plot_model(mod4,  type = "std", title = "δN15 ~ SD PDO + Age + Length.cm + Sex + (1|Year)") # singular



#  check N15 against sd ** with interactions
mod5 <- lmer(d15N ~ sd_PDO + Age * Length.cm * Sex + (1|Year),  data=full)
plot_model(mod5,  type = "std", title = "δN15 ~ SD PDO * Age * Length.cm * Sex + (1|Year)") 



#  check N15 against sd ** with interactions
mod5 <- lmer(d15N ~ mean_PDO + Age + Length.cm + Sex + (1|Year),  data=full)
plot_model(mod5,  type = "std", title = "δN15 ~ mean PDO + Age + Length.cm + Sex + (1|Year)")    


plot(d15N ~ mean_PDO, full)

 # sd sd
mod5 <- lm(sd.N ~ sd_PDO,  data=full)
plot_model(mod5,  type = "std", title = "mean δN15 ~ mean PDO + Age + Length.cm + Sex + (1|Year)") 



plot(d15N ~ sd_PDO, full)
#### regime shifts mean N ####
sd(full$d15N, na.rm = TRUE)

names(pdo.iso)

plot(mean.N ~ Year, data = pdo.iso, xlab = "Year",  ylab = " δN15", pch = 16, las = 1, cex = 1.0, col = "black", type = "o", lty = "solid")


sd(full$d13C, na.rm = TRUE)


abline(v=c(1925,1946,1999), col="blue")
abline(v=c(1939, 1976, 2008), col="red")
abline(v=1989, col="grey")

#### biological lmer ####


babine <- read.csv("babine_updated_8oct2021.csv")

# age model
babine <- babine %>% 
  filter(Age %in% c(1.2, 1.3)) 


glm1 <- glm(d15N ~ Age * Length.cm * Sex + (Year),data = babine)
summary(glm1)

library(sjPlot)
plot_model(glm1)

# Is this anything?

dev.off()


## PDO and N
plot(mean.N ~ sd_PDO, pdo.iso)
lm1  <-  lm(mean.N ~ sd_PDO, pdo.iso)
abline(lm1)



# niche and PDO (C:N ~ PDO)

names(pdo.iso)

niche <- pdo.iso %>% 
  dplyr::mutate(CN = mean.C/mean.N)

plot(CN ~ Age, niche)

#### Messy extra for APR ####

full <- read.csv("very.full.babine.df.12oct2021.csv")

# carbon and weight
plot(CN.ratio ~ mean_PDO, data = full, xlab = "mean_PDO", ylab = "CN.ratio",  pch = 16, las = 1, cex = 1.0, col = "firebrick") 

# carbon and weight
plot(d15N ~ mean_PDO, data = full, xlab = "mean_PDO", ylab = "d15N",  pch = 16, las = 1, cex = 1.0, col = "firebrick") 

#### 14 Dec. Re-try model with scale growth seperated by year (suggested by Jon) ####
#### 14 Dec. Model length from only years with length ####
babine <- full
class(babine$Year)
babine$Year <- as.numeric(babine$Year)



# With alll data
plot(d15N ~ Length.cm, data = babine, xlab = "Length.cm", ylab = "δ15N" , main = "δ15N ~ Length",  pch = 16, las = 1, cex = 1.0, col = "green4") 

library(lme4)



glm1 <- lmer(d15N ~ Length.cm + Age + (1|Year),data = babine)
summary(glm1)

??plot_model
library(sjPlot)

plot_model(glm1)

# with only early year data, is

library(dplyr)

with(babine,
     table(Year, Length.cm,useNA = "ifany"))

early <- babine %>% 
  dplyr::filter(Year < 1957)




glm2 <- lmer(d15N ~ Length.cm + Age + (1|Year),data = babine)
summary(glm2)



plot_model(glm2)

# no change,