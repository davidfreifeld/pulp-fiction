library(ggplot2)
library(caTools)
library(reshape2)
library(plyr)
library(randomForest)
library(rpart)
library(rpart.plot)
library(e1071)
library(datasets)
library(cluster)
library(caret)

# we'll use this function to summarize data by user
Mode <- function(x) {
    xtable <- table(x)
    if (sum(xtable)) {
        names(which.max(table(x)))
    }
    else 
        NA
}

#~~~~~~~~~~~~ GETTING AND CLEANING DATA ~~~~~~~~~~~~#

# first read in the data file (strings will be factors)
setwd("C:/Users/David/workspace/Final Project/")
impData <- read.table('ImpData.txt', header=T, sep='\t', 
        na.strings='(null)', quote="")
siteData <- read.csv('categories.csv', stringsAsFactors=F)

# format some of the variables to dates and factors
impData$logentrytime <- as.POSIXct(impData$logentrytime,
        format='%m/%d/%Y %H:%M', tz = "GMT")
impData$metro <- factor(impData$metro)
impData$logfileid <- NULL

# do some work on the site strings
impData$site <- as.character(impData$site)
impData$site <- sub("^www\\.(.*)", "\\1", impData$site)
impData$site[grepl(".*\\.site-not-provided", impData$site)] <- NA

# now append the site data to our dataset
impData <- merge(x = impData, y = siteData, by.x = 'site', by.y = 'URL', all.x = T)

threshold <- 0.9

# classify each site only if the probability that it is that category is
# greater than the "threshold"
impData$sitecategory <- "Unclassified"
maxSiteScores <- apply(impData[14:23],1,max)
impData$sitecategory[!is.na(maxSiteScores) & maxSiteScores > threshold] <- 
    colnames(impData[14:23])[
        apply(impData[!is.na(maxSiteScores) & maxSiteScores > threshold, 
            c(14:23)], 1, which.max)
    ]

impData$sitecategory <- factor(impData$sitecategory)
impData$site <- factor(impData$site)

# get some more info on when the impression was served
bHourNA <- is.na(impData$userHourOfWeek)
impData$userHourOfDay <- impData$userHourOfWeek %% 24
weeHours <- impData$userHourOfDay < 5 & !bHourNA
normHours <- impData$userHourOfDay >= 5 & !bHourNA
impData$userHourOfDay[weeHours] <- impData$userHourOfDay[weeHours] + 19
impData$userHourOfDay[normHours] <- impData$userHourOfDay[normHours] - 5
impData$userDayOfWeek <- impData$userHourOfWeek %/% 24
impData$userDayOfWeek[weeHours] <- impData$userDayOfWeek[weeHours] - 1
impData$userDayOfWeek[impData$userDayOfWeek == -1] <- 6

impData$userPeriodOfWeek <- impData$userHourOfDay
bSunday <- impData$userDayOfWeek == 0 & !bHourNA
bWeekday <- impData$userDayOfWeek > 0 & impData$userDayOfWeek < 6 & !bHourNA
bSaturday <- impData$userDayOfWeek == 6 & !bHourNA
bSundayMorn <- bSunday & impData$userHourOfDay >= 0 & impData$userHourOfDay <= 5 & !bHourNA
bSundayAft <- bSunday & impData$userHourOfDay >= 6 & impData$userHourOfDay <= 11 & !bHourNA
bSundayEve <- bSunday & impData$userHourOfDay >= 12 & impData$userHourOfDay <= 17 & !bHourNA
bSundayLate <- bSunday & impData$userHourOfDay >= 18 & impData$userHourOfDay <= 23 & !bHourNA
bWeekdayMorn <- bWeekday & impData$userHourOfDay >= 0 & impData$userHourOfDay <= 5 & !bHourNA
bWeekdayAft <- bWeekday & impData$userHourOfDay >= 6 & impData$userHourOfDay <= 11 & !bHourNA
bWeekdayEve <- bWeekday & impData$userHourOfDay >= 12 & impData$userHourOfDay <= 17 & !bHourNA
bWeekdayLate <- bWeekday & impData$userHourOfDay >= 18 & impData$userHourOfDay <= 23 & !bHourNA
bSaturdayMorn <- bSaturday & impData$userHourOfDay >= 0 & impData$userHourOfDay <= 5 & !bHourNA
bSaturdayAft <- bSaturday & impData$userHourOfDay >= 6 & impData$userHourOfDay <= 11 & !bHourNA
bSaturdayEve <- bSaturday & impData$userHourOfDay >= 12 & impData$userHourOfDay <= 17 & !bHourNA
bSaturdayLate <- bSaturday & impData$userHourOfDay >= 18 & impData$userHourOfDay <= 23 & !bHourNA

impData$userPeriodOfWeek[bSundayMorn] <- 0
impData$userPeriodOfWeek[bSundayAft] <- 1
impData$userPeriodOfWeek[bSundayEve] <- 2
impData$userPeriodOfWeek[bSundayLate] <- 3
impData$userPeriodOfWeek[bWeekdayMorn] <- 4
impData$userPeriodOfWeek[bWeekdayAft] <- 5
impData$userPeriodOfWeek[bWeekdayEve] <- 6
impData$userPeriodOfWeek[bWeekdayLate] <- 7
impData$userPeriodOfWeek[bSaturdayMorn] <- 8
impData$userPeriodOfWeek[bSaturdayAft] <- 9
impData$userPeriodOfWeek[bSaturdayEve] <- 10
impData$userPeriodOfWeek[bSaturdayLate] <- 11

impData$userPeriodOfWeek <- factor(impData$userPeriodOfWeek, labels = c("SundayMorn",
                                 "SundayAft", "SundayEve", "SundayLate", "WeekdayMorn",
                                 "WeekdayAft", "WeekdayEve", "WeekdayLate",
                                 "SaturdayMorn", "SaturdayAft", "SaturdayEve", 
                                 "SaturdayLate"))
impData$userDayOfWeek <- factor(impData$userDayOfWeek, labels = c("Sunday", 
                                 "Monday", "Tuesday", "Wednesday", "Thursday", 
                                 "Friday", "Saturday"))

impData$browser <- as.character(impData$browser)
impData$browser <- sub("InternetExplorer.*", "InternetExplorer", impData$browser)
impData$browser <- factor(impData$browser)

# Look at english-speaking countries vs rest of the world
impData$english <- as.character(impData$country) %in%
        c("United States", "United Kingdom", "Canada", "Ireland", 
            "Australia", "New Zealand") 
impData$usa <- as.character(impData$country) == "United States"

# get the region for the 50 states in the US
data(state)
impData$usregion <- sapply(as.character(impData$region), function(x) 
    as.character(state.region[pmatch(x, state.name)]))
impData$usregion[impData$region == "District of Columbia"] <- "Northeast"
impData$usregion[!is.na(impData$usa) & !impData$usa] <- "International"
impData$usregion <- factor(impData$usregion)


#~~~~~~~~~~~~~~~~ EXPLORATORY ANALYSIS ~~~~~~~~~~~~~~~~#

userSiteCounts <- ddply(impData[impData$FavoriteMovieGenre != "?????",],
    .(tdid, site), summarize, count = length(FavoriteMovieGenre),
    FavoriteMovieGenre = FavoriteMovieGenre[1])

siteGenreCounts <- ddply(userSiteCounts, .(site, FavoriteMovieGenre),
    summarize, count = length(count))



#~~~~~~~~~~~~~~~~ CLUSTERING ~~~~~~~~~~~~~~~~#

# Cluster the sites based on overlapping users
numSites <- length(levels(impData$site))
userSiteData <- na.omit(ddply(impData, .(tdid, site), summarize,
    FavoriteMovieGenre = Mode(FavoriteMovieGenre)))
userSiteMerge <- merge(x = userSiteData, y = userSiteData, by = "tdid", all = T)
siteSplits <- split(userSiteMerge$site.x, userSiteMerge$site.y)

# siteOverlapMat <- matrix(rep(0, numSites*numSites), nrow = numSites, 
#     dimnames = list(names(siteSplits), names(siteSplits)))

# for (urli in names(siteSplits)) {
#     for (urlj in as.character(siteSplits[[urli]])) {
#         if (!is.na(urlj)) {
#             siteOverlapMat[urli, urlj] = siteOverlapMat[urli, urlj] + 1
#         }
#     }
# }

siteOverlapMat <- as.matrix(read.csv('siteOverlapMat.csv', row.names=1))
colnames(siteOverlapMat) <- rownames(siteOverlapMat)

maxOverlap <- max(siteOverlapMat) + 1
siteDistMat <- (maxOverlap - siteOverlapMat)^2
siteDist <- as.dist(siteDistMat)

# Set this to get number of clusters
k = 10

pamSite <- pam(siteDist, k)

# append the clusters to the data set
impData$sitecluster <- factor(sapply(as.character(impData$site), function(x) {
    if (is.na(x)) {
        NA
    }
    else {
        pamSite$clustering[[x]]
    }
}))

siteClusterCols <- paste0('sitecluster', 1:k)
impData[,siteClusterCols] <- NA

impData[!is.na(impData$sitecluster),siteClusterCols] <- 
    model.matrix(FavoriteMovieGenre ~ sitecluster - 1, impData)

ggplot(impData[impData$FavoriteMovieGenre != "?????",], 
    aes(x = FavoriteMovieGenre, fill = FavoriteMovieGenre)) +
        geom_bar(stat="bin") + 
        facet_wrap(~ sitecluster)


#~~~~~~~~~~~~~~~~ GET SUMMARY DFs ~~~~~~~~~~~~~~~~#

userData <- ddply(impData, .(tdid), summarize,
    country = Mode(country),
    region = Mode(region),
    metro = Mode(metro),
    city = Mode(city),
    devicetype = Mode(devicetype),
    osfamily = Mode(osfamily),
    os = Mode(os),
    browser = Mode(browser),
    FavoriteMovieGenre = Mode(FavoriteMovieGenre),
    usregion = Mode(usregion),
    usa = sum(usa, na.rm=T) / sum(!is.na(usa)) > 0.5,
    SundayMorn = sum(userPeriodOfWeek == "SundayMorn", na.rm=T)/sum(!is.na(userPeriodOfWeek)),
    SundayAft = sum(userPeriodOfWeek == "SundayAft", na.rm=T)/sum(!is.na(userPeriodOfWeek)),
    SundayEve = sum(userPeriodOfWeek == "SundayEve", na.rm=T)/sum(!is.na(userPeriodOfWeek)),
    SundayLate = sum(userPeriodOfWeek == "SundayLate", na.rm=T)/sum(!is.na(userPeriodOfWeek)),
    WeekdayMorn = sum(userPeriodOfWeek == "WeekdayMorn", na.rm=T)/sum(!is.na(userPeriodOfWeek))/5,
    WeekdayAft = sum(userPeriodOfWeek == "WeekdayAft", na.rm=T)/sum(!is.na(userPeriodOfWeek))/5,
    WeekdayEve = sum(userPeriodOfWeek == "WeekdayEve", na.rm=T)/sum(!is.na(userPeriodOfWeek))/5,
    WeekdayLate = sum(userPeriodOfWeek == "WeekdayLate", na.rm=T)/sum(!is.na(userPeriodOfWeek))/5,
    SaturdayMorn = sum(userPeriodOfWeek == "SaturdayMorn", na.rm=T)/sum(!is.na(userPeriodOfWeek)),
    SaturdayAft = sum(userPeriodOfWeek == "SaturdayAft", na.rm=T)/sum(!is.na(userPeriodOfWeek)),
    SaturdayEve = sum(userPeriodOfWeek == "SaturdayEve", na.rm=T)/sum(!is.na(userPeriodOfWeek)),
    SaturdayLate = sum(userPeriodOfWeek == "SaturdayLate", na.rm=T)/sum(!is.na(userPeriodOfWeek)),
    bArts = "Arts" %in% sitecategory,
    bBusiness = "Business" %in% sitecategory,
    bScience = "Science" %in% sitecategory,
    bComputers = "Computers" %in% sitecategory,
    bRecreation = "Recreation" %in% sitecategory,
    bSports = "Sports" %in% sitecategory,
    bSociety = "Society" %in% sitecategory,
    bHealth = "Health" %in% sitecategory,
    bHome = "Home" %in% sitecategory,
    bGames = "Games" %in% sitecategory, 
    numSites = length(unique(site))
)
rownames(userData) <- userData$tdid
userData[2:11] <- as.data.frame(lapply(userData[2:11], factor))

userData$usregion[userData$usa == "TRUE" & is.na(userData$usregion)] <- "South"

# impute the medians for the periodOfWeek vars
userData <- transform(userData, 
    SundayMorn = ifelse(is.na(SundayMorn), median(SundayMorn, na.rm=TRUE), SundayMorn),
    SundayAft = ifelse(is.na(SundayAft), median(SundayAft, na.rm=TRUE), SundayAft),
    SundayEve = ifelse(is.na(SundayEve), median(SundayEve, na.rm=TRUE), SundayEve),
    SundayLate = ifelse(is.na(SundayLate), median(SundayLate, na.rm=TRUE), SundayLate),
    WeekdayMorn = ifelse(is.na(WeekdayMorn), median(WeekdayMorn, na.rm=TRUE), WeekdayMorn),
    WeekdayAft = ifelse(is.na(WeekdayAft), median(WeekdayAft, na.rm=TRUE), WeekdayAft),
    WeekdayEve = ifelse(is.na(WeekdayEve), median(WeekdayEve, na.rm=TRUE), WeekdayEve),
    WeekdayLate = ifelse(is.na(WeekdayLate), median(WeekdayLate, na.rm=TRUE), WeekdayLate),
    SaturdayMorn = ifelse(is.na(SaturdayMorn), median(SaturdayMorn, na.rm=TRUE), SaturdayMorn),
    SaturdayAft = ifelse(is.na(SaturdayAft), median(SaturdayAft, na.rm=TRUE), SaturdayAft),
    SaturdayEve = ifelse(is.na(SaturdayEve), median(SaturdayEve, na.rm=TRUE), SaturdayEve),
    SaturdayLate = ifelse(is.na(SaturdayLate), median(SaturdayLate, na.rm=TRUE), SaturdayLate)
)

# Get the summary statistics for the site clusters and bind to
# the userData summary df
siteClusterTable <- table(impData$sitecluster) / sum(!is.na(impData$sitecluster))
userSplits <- split(impData[siteClusterCols], impData$tdid)

userSiteClusterList <- lapply(names(userSplits), function(id) {
    DF <- userSplits[[id]]
    summaryDF <- data.frame(row.names = id)
    numRows <- sum(!is.na(DF[1]))
    for (i in 1:length(siteClusterCols)){
        c <- siteClusterCols[i]
        summaryDF[id, c] <- (sum(DF[c], na.rm=T) + 5 * siteClusterTable[[i]]) /
                                (numRows + 5)
    }
    summaryDF
})

userSiteClusterSummary <- ldply(userSiteClusterList)
rownames(userSiteClusterSummary) <- names(userSplits)

userData <- cbind(userData, userSiteClusterSummary)


# split the data into those where we know the genre 
# and those where we do not

userData$FavoriteMovieGenre <- as.character(userData$FavoriteMovieGenre)
#userData$tdid <- as.character(userData$tdid)

userKnown <- subset(userData, FavoriteMovieGenre != "?????")
userKnown$FavoriteMovieGenre <- factor(userKnown$FavoriteMovieGenre)
#userKnown$tdid <- factor(userKnown$tdid)

userUnknown <- subset(userData, FavoriteMovieGenre == "?????")
userUnknown$FavoriteMovieGenre <- NULL
#userUnknown$tdid <- factor(userUnknown$tdid)

#userData$tdid <- factor(userData$tdid)
userData$FavoriteMovieGenre <- factor(userData$FavoriteMovieGenre)


#~~~~~~~~~~~~~~~MORE EXPLORATORY ANALYSIS~~~~~~~~~~~~~~~~#

ggplot(userKnown[!userKnown$usa,], aes(x = FavoriteMovieGenre, 
    fill = FavoriteMovieGenre)) + 
        geom_bar(stat="bin") + 
        facet_wrap(~ country)


#~~~~~~~~~~~~~~~~ MODEL BUILDING ~~~~~~~~~~~~~~~~#

userKnown$tdid <- as.character(userKnown$tdid)

numFolds = trainControl( method = "cv", number = 10 )
cpGrid = expand.grid( .cp = seq(0.0001,0.03,0.005)) 
treeMod <- train(FavoriteMovieGenre ~ usa + devicetype, data = userKnown,
    method='rpart', trControl = numFolds, tuneGrid = cpGrid)

userUnknown$usaTRUE <- as.numeric(userUnknown$usa)
userUnknown$devicetypeOther <- as.numeric(as.character(userUnknown$devicetype) == "Other")
userUnknown$devicetypePC <- as.numeric(as.character(userUnknown$devicetype) == "PC")
userUnknown$devicetypeMobile <- as.numeric(as.character(userUnknown$devicetype) == "Mobile")
userUnknown$devicetypeTablet <- as.numeric(as.character(userUnknown$devicetype) == "Tablet")
treePred <- predict(treeMod$finalModel, newdata=userUnknown)

treePred <- as.data.frame(predict(treeMod$finalModel, newdata=userUnknown, type="class"))
write.csv(treePred, "userGenrePredictions.csv")


#~~~~~~~~~~~~~~~~ FINDING FRIENDS ~~~~~~~~~~~~~~~~#

userSiteGenreKnown <- na.omit(ddply(impData[impData$FavoriteMovieGenre != "?????",],
    .(tdid, site), summarize, FavoriteMovieGenre = Mode(FavoriteMovieGenre)))

userSiteGenreKnown$tdid <- factor(as.character(userSiteGenreKnown$tdid))
userSiteGenreKnown$site <- factor(as.character(userSiteGenreKnown$site))

mergedUserSite <- merge(x = userSiteGenreKnown, y = userSiteGenreKnown, 
    by = c("site", "FavoriteMovieGenre"), all = T)
mergedUserSite <- mergedUserSite[mergedUserSite$tdid.x != mergedUserSite$tdid.y,]

mergedUserSite$tdid.x <- factor(as.character(mergedUserSite$tdid.x))
mergedUserSite$tdid.y <- factor(as.character(mergedUserSite$tdid.y))
mergedUserSite$site <- factor(as.character(mergedUserSite$site))

userSplits <- split(as.character(mergedUserSite$tdid.x), mergedUserSite$tdid.y)
numUsers <- length(userSplits)

userOverlapMat <- matrix(rep(0, numUsers*numUsers), nrow = numUsers, 
    dimnames = list(names(userSplits), names(userSplits)))

for (tdidi in names(userSplits)) {
    for (tdidj in as.character(userSplits[[tdidi]])) {
        if (!is.na(tdidj)) {
            userOverlapMat[tdidi, tdidj] = userOverlapMat[tdidi, tdidj] + 1
        }
    }
}
userOverlapMat[upper.tri(userOverlapMat)] <- 0

userOverlapPropRow <- userOverlapMat
for (tdidi in rownames(userOverlapMat)) {
    for (j in 1:ncol(userOverlapMat)) {
        userOverlapPropRow[tdidi, j] = userOverlapMat[tdidi, j] / 
            userKnown[userKnown$tdid == tdidi, "numSites"]
    }
}
