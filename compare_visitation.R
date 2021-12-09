#Compare visitation rates
#November 8, 2021

#
install.packages("readxl")

#Read in Tangalooma Data
tg_lh <- readxl::read_excel("Tangalooma Dolphin Data-Updated.xlsx", sheet = "family info") |> as.data.frame()

#I added a column, "date_of_birth_R" where I gave the approximate birthdates a default month and/or day so they could be formatted properly
tg_lh$date_of_birth_R <- as.Date(tg_lh$date_of_birth_R)

tg_visits <- readxl::read_excel("Tangalooma Dolphin Data-Updated.xlsx", sheet = "pivot table", 
                                na = ".") |> as.data.frame()
tg_visits$Date <- as.Date(tg_visits$Date)

#Reshape data into long format with one row per dolphin
tg_visits <- reshape(tg_visits, 
                      ids = tg_visits[,1],
                      varying = list(names(tg_visits)[2:24]), 
                      times = names(tg_visits)[2:24],
                      timevar = "Dolphin.ID", 
                      v.names = "Presence", 
                      idvar = "Date",
                      direction = "long")

#Add age at each day of visit 
tg_visits$Birthdate <- tg_lh$date_of_birth_R[match(tg_visits$Dolphin.ID, 
                                                   tg_lh$Name)]

#age in years
tg_visits$Age <- as.numeric(tg_visits$Date - tg_visits$Birthdate)/365

#Included individuals must be observed at age 4 or greater, and be born after 2-Sep-94
subjects <- unique(tg_visits$Dolphin.ID[which(tg_visits$Age>=8 & tg_visits$Birthdate >= "1994-09-02" &
                                                tg_visits$Presence==1)])

#Set up dataframe to hold results
n <- length(subjects)

results_tang <- data.frame(subjects = subjects, 
                      preweaning_visitation = numeric(n),
                      postweaning_visitation = numeric(n), 
                      sex = character(n))

results_tang$sex <- tg_lh$Sex[match(results_tang$subjects, tg_lh$Name)]

#Days marked 1 / All days in first 4 years of life

alldates <- unique(tg_visits$Date)

for (i in subjects) {
  
  #preweaning (days marked 1 under 4/ all observation days in first 4 years of life)
  prenum <- tg_visits$Presence[which(tg_visits$Dolphin.ID==i & tg_visits$Age < 4)] |> sum(na.rm = TRUE)
  
  predenom <- alldates[which(alldates >= tg_lh$date_of_birth_R[which(tg_lh$Name == i)] &
                            alldates < (tg_lh$date_of_birth_R[which(tg_lh$Name == i)]) + (365*4))] |> length()
  
  results_tang[results_tang$subjects==i, "preweaning_visitation"] <- prenum/predenom
  
  #postweaning (days marked 1 over 4/ all days with 0 or 1 after age 4)
  postnum <- tg_visits$Presence[which(tg_visits$Dolphin.ID==i & tg_visits$Age >= 4)] |> sum(na.rm = TRUE)
  
  postdenom <- sum(tg_visits$Presence[which(tg_visits$Dolphin.ID==i & tg_visits$Age >= 4)] >= 0, na.rm=TRUE)
  
  results_tang[results_tang$subjects==i, "postweaning_visitation"] <- postnum/postdenom
  
  
}

#results

aggregate(preweaning_visitation~sex, data = results_tang, mean)
aggregate(postweaning_visitation~sex, data = results_tang, mean)

# To add
# Minimum years postweaning 
# Death date censoring

#make boxplot

install.packages("ggplot2")
library(ggplot2)

x <- reshape(results, direction = "long", 
             idvar = c("subjects", "sex"), 
             times = c("preweaning_visitation", "postweaning_visitation"),
             varying = list(c("preweaning_visitation", "postweaning_visitation")), 
             timevar = "status", 
             v.names = "visitation_rate",
             ids = NULL)

rownames(x)<-NULL

x$status <- factor(x$status, levels = c("preweaning_visitation", "postweaning_visitation"))

# Remove Zephyr because of data error - not present with mom as a newborn
x <- x[-which(x$subjects=="Zephyr"),]

library(ggplot2)
ggplot(data=x, aes(x=status, y=visitation_rate, fill=sex)) + 
  geom_boxplot()


#Read in Monkey Mia Data
mm_lh <- read.csv("LifeHistory_20211021.csv") 
mm_visits <- readxl::read_excel("2010-2016 Feed Data.xlsx") |> as.data.frame()

#add birthdate column to visitation 
mm_visits <- readxl::read_excel("2010-2016 Feed Data.xlsx", sheet = "1996-2010", 
                                na = ".") |> as.data.frame()

mm_lh$Birth.Date <- as.Date(mm_lh$Birth.Date)

mm_visits$Date <- as.Date(mm_visits$Date)

mm_visits <- mm_visits[!duplicated(mm_visits[,c("Date", "Dolphin Name-Clean")]),]

mm_visits$Birthdate <- mm_lh$Birth.Date[match(mm_visits$`Dolphin Name-Clean`, 
                                                   mm_lh$Dolphin.ID)]

#add in column for presence

mm_visits$Presence <- 1

#age in years
mm_visits$Age <- as.numeric(mm_visits$Date - mm_visits$Birthdate)/365

#included individuals observed at 4 or greater, and born after 2-sept-92

subjects <- unique(mm_visits$`Dolphin Name-Clean`[which(mm_visits$Age>=8 & mm_visits$Birthdate >= "1994-09-02")])


#set up data frame to hold results
n <- length(subjects)

results_mm <- data.frame(subjects = subjects, 
                      preweaning_visitation_mm = numeric(n),
                      postweaning_visitation_mm = numeric(n), 
                      sex = character(n))                  

results_mm$sex <- mm_lh$Sex[match(results_mm$subjects, mm_lh$Dolphin.ID)]

for (i in subjects) {

#preweaning (days there in first 4 years of life / 365*4)

prenum <- mm_visits$Presence[which(mm_visits$`Dolphin Name-Clean`==i & mm_visits$Age < 4)] |> sum(na.rm = TRUE)

predom <- 1460

results_mm[results_mm$subjects==i, "preweaning_visitation_mm"] <- prenum/predenom

#postweaning (days there after 4 years of life / 365*4)

prenum <- mm_visits$Presence[which(mm_visits$`Dolphin Name-Clean`==i & mm_visits$Age >= 4 & mm_visits$Age < 8)] |> sum(na.rm = TRUE)

predom <- 1460

results_mm[results_mm$subjects==i, "postweaning_visitation_mm"] <- prenum/predenom

}

aggregate(preweaning_visitation_mm~sex, data = results_mm, mean)
aggregate(postweaning_visitation_mm~sex, data = results_mm, mean)

#make boxplot 
x <- reshape(results_mm, direction = "long", 
             idvar = c("subjects", "sex"), 
             times = c("preweaning_visitation_mm", "postweaning_visitation_mm"),
             varying = list(c("preweaning_visitation_mm", "postweaning_visitation_mm")), 
             timevar = "status", 
             v.names = "visitation_rate",
             ids = NULL)

rownames(x)<-NULL


x$status <- factor(x$status, levels = c("preweaning_visitation_mm", "postweaning_visitation_mm"))


ggplot(data=x, aes(x=status, y=visitation_rate, fill=sex)) + 
  geom_boxplot()

