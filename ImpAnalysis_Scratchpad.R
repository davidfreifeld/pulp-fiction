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

threshold = 0.75

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
impData$FavoriteMovieGenre <- as.character(impData$FavoriteMovieGenre)
impData$tdid <- as.character(impData$tdid)
impData$logfileid <- NULL

# do some work on the site strings
impData$site <- as.character(impData$site)
#impData$site <- sub(".*\\.(.*\\..*)$", "\\1", impData$site)
impData$site <- sub("^www\\.(.*)", "\\1", impData$site)
impData$site[grepl(".*\\.site-not-provided", impData$site)] <- NA

impData <- merge(x = impData, y = siteData, by.x = 'site', by.y = 'URL', all.x = T)

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

# get the region for the 50 states in the US
data(state)
impData$usregion <- sapply(as.character(impData$region), 
                        function(x) state.region[pmatch(x, state.name)])
impData$usregion[impData$region == "District of Columbia"] <- "Northeast"
impData$usregion <- factor(impData$usregion)

# Look at english-speaking countries vs rest of the world
impData$english <- as.character(impData$country) %in%
        c("United States", "United Kingdom", "Canada", "Ireland", 
            "Australia", "New Zealand") 
impData$usa <- as.character(impData$country) == "United States"

# split the data into those where we know the genre 
# and those where we do not
impKnown <- subset(impData, FavoriteMovieGenre != "?????")
impKnown$FavoriteMovieGenre <- as.factor(impKnown$FavoriteMovieGenre)
impKnown$tdid <- factor(impKnown$tdid)

impUnknown <- subset(impData, FavoriteMovieGenre == "?????")
impUnknown$FavoriteMovieGenre <- NULL
impUnknown$tdid <- factor(impUnknown$tdid)

impData$tdid <- factor(impData$tdid)

# we'll use this function to summarize data by user
Mode <- function(x) {
    xtable <- table(x)
    if (sum(xtable)) {
        names(which.max(table(x)))
    }
    else 
        NA
}


#~~~~~~~~~~~~~~~~ SITE CLUSTERING ATTEMPTS ~~~~~~~~~~~~~~~~#

dissimilarityData <- ddply(impData, .(tdid), summarize, 
    country = length(unique(country)),
    site = length(unique(site)),
    region = length(unique(region)),
    devicetype = length(unique(devicetype)),
    osfamily = length(unique(osfamily)),
    browser = length(unique(browser)),
    usregion = length(unique(usregion)),
    english = length(unique(usregion)),
    userPeriodOfWeek = length(unique(userPeriodOfWeek))
)



# HERE WE GO AGAIN ON MY OWN
# numSites <- length(levels(impData$site))
# numUsers <- length(levels(impData$tdid))
# userSiteCounts <- matrix(rep(0, numUsers*numSites), nrow = numUsers, 
#     dimnames = list(levels(impData$tdid), levels(impData$site)))
# userSiteSplits <- split(impData$site, impData$tdid)

# for (iUser in 1:length(userSiteSplits)) {
#     user <- names(userSiteSplits)[iUser]
#     for(site in as.character(userSiteSplits[[iUser]])) {
#         if (!is.na(site)) {
#             userSiteCounts[user, site] = userSiteCounts[user, site] + 1
#         }
#     }
# }



# Try to get a matrix of distances between sites
userSiteGenreData <- na.omit(ddply(impData, .(tdid, site), summarize,
    FavoriteMovieGenre = Mode(FavoriteMovieGenre)))

# userSiteMerge <- merge(x = userSite, y = userSite, by = "tdid", all = T)
# siteSplits <- split(userSiteMerge$site.x, userSiteMerge$site.y)
# numSites <- length(siteSplits)

# siteOverlapMat <- matrix(rep(0,numSites*numSites), nrow = numSites, 
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

k = 8
pamSite <- pam(siteDist, k)

impKnown$sitecluster <- factor(sapply(as.character(impKnown$site), function(x) {
    if (is.na(x)) {
        NA
    }
    else {
        pamSite$clustering[[x]]
    }
}))

ggplot(impKnown, aes(x = FavoriteMovieGenre, fill = FavoriteMovieGenre)) + 
    geom_bar(stat="bin") + 
    facet_grid(~ sitecluster)


userSiteGenreKnown <- ddply(impKnown, .(tdid, site), summarize,
    FavoriteMovieGenre = Mode(FavoriteMovieGenre))

userSiteGenreKnown$sitecluster <- sapply(as.character(userSiteGenreKnown$site), 
    function(x) {
        if (is.na(x)) {
            NA
        }
        else {
            pamSite$clustering[[x]]
        }
    }
)

ggplot(userSiteGenreKnown, aes(x = FavoriteMovieGenre, fill = FavoriteMovieGenre)) + 
    geom_bar(stat="bin") + 
    facet_grid(~ sitecluster)







# # let's try it now with users 
# # WAY 1
# numSites <- length(levels(userSite$site))
# numUsers <- length(levels(userSite$tdid))
# userSiteCounts <- matrix(rep(0, numUsers*numSites), nrow = numUsers, 
#     dimnames = list(levels(userSite$tdid), levels(userSite$site)))

# for (i in 1:nrow(userSite)) {
#     userSiteCounts[as.character(userSite[i,"tdid"]), as.character(userSite[i,"site"])] = 
#         userSiteCounts[as.character(userSite[i,"tdid"]), as.character(userSite[i,"site"])] + 1
# }

# # do clustering
# k = 5
# #kmc <- kmeans(userSiteCounts, centers = k, iter.max = 1000)
# pamUser <- clara(userSiteCounts, k, metric = 'manhattan')

# userGenre <- ddply(impKnown, .(tdid), summarize, 
#     FavoriteMovieGenre = Mode(FavoriteMovieGenre))

# userGenre$sitecluster <- sapply(as.character(userGenre$tdid), function(x) {
#     pamUser$clustering[[x]]
# })

# ggplot(userGenre, aes(x = FavoriteMovieGenre, fill = FavoriteMovieGenre)) + 
#     geom_bar(stat="bin") + 
#     facet_grid(~ sitecluster)




# WAY 2
mergedUserSite <- merge(x = userSiteGenreData, y = userSiteGenreData, 
    by = "site", all = T)

userSplits <- split(mergedUserSite$tdid.x, mergedUserSite$tdid.y)
numUsers <- length(userSplits)

# userOverlapMat <- matrix(rep(0,numUsers*numUsers), nrow = numUsers, 
#     dimnames = list(names(userSplits), names(userSplits)))

# for (tdidi in names(userSplits)) {
#     for (tdidj in as.character(userSplits[[tdidi]])) {
#         if (!is.na(tdidj)) {
#             userOverlapMat[tdidi, tdidj] = userOverlapMat[tdidi, tdidj] + 1
#         }
#     }
# }

# # Write matrix to file so we can re-load it later
# write.csv(userOverlapMat, 'userOverlapMat.csv')

# Read matrix from file
userOverlapMat <- as.matrix(read.csv('userOverlapMat.csv', row.names=1))
colnames(userOverlapMat) <- rownames(userOverlapMat)

maxUserOverlap <- max(userOverlapMat) + 1
userDistMat <- (maxUserOverlap - userOverlapMat)^2
userDist <- as.dist(userDistMat)

k = 12 # 11 is not bad
pamUser <- pam(userDist, k)

userGenreKnown <- ddply(impKnown, .(tdid), summarize, 
    FavoriteMovieGenre = Mode(FavoriteMovieGenre))

userGenreKnown$usercluster <- sapply(as.character(userGenreKnown$tdid), 
    function(x) {
        pamUser$clustering[[x]]
    }
)

ggplot(userGenreKnown, aes(x = FavoriteMovieGenre, fill = FavoriteMovieGenre)) + 
    geom_bar(stat="bin") + 
    facet_grid(~ usercluster)


impKnown$usercluster <- factor(sapply(as.character(impKnown$tdid), 
    function(x) {
        pamUser$clustering[[x]]
    }
))

#~~~~~~~~~~~~~~~~ EXPLORATORY ANALYSIS ~~~~~~~~~~~~~~~~#

# First let's explore favorite genre by hour of the day
impKnown <- ddply(impKnown, .(userHourOfDay), transform, 
                        totalHourOfDay = length(userHourOfDay))
impKnown <- ddply(impKnown, .(FavoriteMovieGenre), transform, 
                        totalFavoriteGenre = length(FavoriteMovieGenre))

genreHourKnown <- ddply(impKnown, .(FavoriteMovieGenre, userHourOfDay), summarize,
                        prop = length(userHourOfDay) / totalFavoriteGenre[1])
ggplot(genreHourKnown, aes(x = userHourOfDay, y = prop)) + 
    geom_histogram(stat="identity") + facet_wrap(~ FavoriteMovieGenre)

# hourGenreKnown <- ddply(impKnown, .(userHourOfDay, FavoriteMovieGenre), summarize, 
#                         prop = length(FavoriteMovieGenre) / totalHourOfDay[1])
# ggplot(hourGenreKnown, aes(x = userHourOfDay, y = prop, fill = FavoriteMovieGenre)) +
#     geom_area()





## Impute means for NA values of site category features:
# impKnown <- transform(impKnown, 
#     Arts = ifelse(is.na(Arts), mean(Arts, na.rm=TRUE), Arts),
#     Business = ifelse(is.na(Business), mean(Business, na.rm=TRUE), Business),
#     Science = ifelse(is.na(Science), mean(Science, na.rm=TRUE), Science),
#     Computers = ifelse(is.na(Computers), mean(Computers, na.rm=TRUE), Computers),
#     Recreation = ifelse(is.na(Recreation), mean(Recreation, na.rm=TRUE), Recreation),
#     Sports = ifelse(is.na(Sports), mean(Sports, na.rm=TRUE), Sports),
#     Society = ifelse(is.na(Society), mean(Society, na.rm=TRUE), Society),
#     Health = ifelse(is.na(Health), mean(Health, na.rm=TRUE), Health),
#     Home = ifelse(is.na(Home), mean(Home, na.rm=TRUE), Home),
#     Games = ifelse(is.na(Games), mean(Games, na.rm=TRUE), Games)
# )

userKnown <- ddply(impKnown, .(tdid), summarize, 
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
    userDayOfWeek = Mode(userDayOfWeek),
    userPeriodOfWeek = Mode(userPeriodOfWeek),
    usercluster = Mode(usercluster),
    sitecluster = Mode(sitecluster),
    english = as.logical(Mode(english)),
    usa = as.logical(Mode(usa)),
    userHourOfDay = mean(userHourOfDay),
    Arts = mean(Arts, na.rm=TRUE),
    Business = mean(Business, na.rm=TRUE),
    Science = mean(Science, na.rm=TRUE),
    Computers = mean(Computers, na.rm=TRUE),
    Recreation = mean(Recreation, na.rm=TRUE),
    Sports = mean(Sports, na.rm=TRUE),
    Society = mean(Society, na.rm=TRUE),
    Health = mean(Health, na.rm=TRUE),
    Home = mean(Home, na.rm=TRUE),
    Games = mean(Games, na.rm=TRUE),
    bArts = "Arts" %in% sitecategory,
    bBusiness = "Business" %in% sitecategory,
    bScience = "Science" %in% sitecategory,
    bComputers = "Computers" %in% sitecategory,
    bRecreation = "Recreation" %in% sitecategory,
    bSports = "Sports" %in% sitecategory,
    bSociety = "Society" %in% sitecategory,
    bHealth = "Health" %in% sitecategory,
    bHome = "Home" %in% sitecategory,
    bGames = "Games" %in% sitecategory
)
rownames(userKnown) <- userKnown$tdid
userKnown[2:15] <- as.data.frame(lapply(userKnown[2:15], factor))


# Just the distribution of favorite genres
ggplot(userKnown, aes(x = FavoriteMovieGenre, fill = FavoriteMovieGenre)) +
        geom_bar(stat="bin")

# English-speaking vs everyone else
ggplot(userKnown, aes(x = FavoriteMovieGenre, fill = FavoriteMovieGenre)) +
        geom_bar(stat="bin") +
        facet_wrap(~ english)

# USA vs everyone else
ggplot(userKnown, aes(x = FavoriteMovieGenre, fill = FavoriteMovieGenre)) +
        geom_bar(stat="bin") +
        facet_wrap(~ usa)

# Breakdown of genre by country for non-US countries
ggplot(userKnown[userKnown$country != "United States",], 
        aes(x = country, fill = FavoriteMovieGenre)) + 
        geom_bar(stat="bin")

# Breakdown of genre by state in the US
ggplot(userKnown[userKnown$country == "United States",], 
        aes(x = region, fill = FavoriteMovieGenre)) + 
        geom_bar(stat="bin")

# Breakdown of genre by region in the US
ggplot(userKnown[userKnown$country == "United States",], 
        aes(x = usregion, fill = FavoriteMovieGenre)) + 
        geom_bar(stat="bin", position = position_dodge())

# Breakdown of devicetype by genre
ggplot(userKnown, aes(x = FavoriteMovieGenre, fill = devicetype)) + 
        geom_bar(stat="bin")

# Breakdown of osfamily by genre
ggplot(userKnown, aes(x = FavoriteMovieGenre, fill = osfamily)) + 
        geom_bar(stat="bin")

# Breakdown of genre by osfamily
ggplot(userKnown, aes(x = osfamily, fill = FavoriteMovieGenre)) + 
        geom_bar(stat="bin")

# Breakdown of browser by genre
ggplot(userKnown, aes(x = FavoriteMovieGenre, fill = browser)) + 
        geom_bar(stat="bin")

# Breakdown of genre by day of week
ggplot(userKnown, aes(x = userDayOfWeek, fill = FavoriteMovieGenre)) + 
        geom_bar(stat="bin")





userKnownCatMelt <- melt(userKnown, measure.vars = 27:36)

ggplot(userKnownCatMelt, aes(x = variable, fill = value)) +
        geom_bar(stat="bin", position = position_dodge()) +
        facet_wrap(~ FavoriteMovieGenre )

# ggplot(userKnownCatMelt, aes(x = variable, y = value, fill = variable)) +
#         geom_boxplot() +
#         facet_grid(. ~ FavoriteMovieGenre)




# But we don't want to double-count sites
# So let's look at one row per user-site pair:
userSiteKnown <- ddply(impKnown, .(tdid, site), summarize,
    FavoriteMovieGenre = Mode(FavoriteMovieGenre),
    siteCategory = siteCategory[1])
userSiteKnown$FavoriteMovieGenre <- factor(userSiteKnown$FavoriteMovieGenre)

# look at the 100 most popular sites and their distributions of genre
# userSiteKnown$top100site <- as.character(userSiteKnown$site)
# topSites <- names(sort(table(userSiteKnown$site), decreasing=T)[1:100])
# userSiteKnown$top100site[!(userSiteKnown$top100site %in% topSites)] <- "other"
# userSiteKnown$top100site <- factor(userSiteKnown$top100site)

# ggplot(userSiteKnown[userSiteKnown$top100site != "other",], 
#         aes(x = FavoriteMovieGenre, fill = FavoriteMovieGenre)) +
#         geom_bar(stat="bin") +
#         facet_wrap(~top100site)


# try to find the most "different" sites
numPairs <- nrow(userSiteKnown)
userSiteKnown <- ddply(userSiteKnown, .(FavoriteMovieGenre), transform,
    genreProp = length(FavoriteMovieGenre) / numPairs)
userSiteKnown <- ddply(userSiteKnown, .(site), transform,
    siteLength = length(site))

userSiteKnown <- ddply(userSiteKnown, .(site, FavoriteMovieGenre), transform,
    diffScore = length(FavoriteMovieGenre) - genreProp[1] * siteLength[1])

siteGenreDiffs <- ddply(userSiteKnown, .(site), summarize,
    score = sum(diffScore^2))


ggplot(na.omit(userSiteKnown), aes(x = FavoriteMovieGenre, fill = siteCategory)) + 
    geom_bar(stat="bin")






#~~~~~~~~~~~~ MODEL BUILDING ~~~~~~~~~~~~#

# split into train and validation sets
impKnown$tdid <- as.character(impKnown$tdid)
split <- sample.split(impKnown$FavoriteMovieGenre, SplitRatio = 0.7, 
                        group = impKnown$tdid)
impTrain <- subset(impKnown,  split)
impVal   <- subset(impKnown, !split)
impTrain$tdid <- as.factor(impTrain$tdid)
impVal$tdid <- as.factor(impVal$tdid)

rfFeatures <- c("tdid", "FavoriteMovieGenre", "userPeriodOfWeek", 
    "english", "devicetype", "usercluster", "usregion", "sitecategory",
    "Arts", "Business", "Science", 
    "Computers", "Recreation", "Sports", "Society", "Health", "Home", "Games")
    # "userHourOfDay", "userDayOfWeek", "sitecategory"

rfImpTrain <- na.omit(impTrain[, rfFeatures])
rfImpVal <- na.omit(impVal[, rfFeatures])

#rfMod <- randomForest(FavoriteMovieGenre ~ . - tdid, data = rfImpTrain)
rfMod <- randomForest(FavoriteMovieGenre ~ . - tdid, 
    data = rfImpTrain, 
    trControl = trainControl(method = "cv"))
rfMod

# model predictions on a per visit 
rfPredVal <- predict(rfMod, newdata=rfImpVal)
table(rfImpVal$FavoriteMovieGenre, rfPredVal)

# training set predictions using the
rfImpTrain$pred <- rfPredTrain
rfUserTrain <- ddply(rfImpTrain, .(tdid), summarize,
    FavoriteMovieGenre = Mode(FavoriteMovieGenre),
    pred = Mode(pred)
)
table(rfUserTrain$FavoriteMovieGenre, rfUserTrain$pred)

rfImpVal$pred <- rfPredVal
rfUserVal <- ddply(rfImpVal, .(tdid), summarize,
    FavoriteMovieGenre = Mode(FavoriteMovieGenre),
    pred = Mode(pred)
)
table(rfUserVal$FavoriteMovieGenre, rfUserVal$pred)

rfPredProbVal <- as.data.frame(predict(rfMod, newdata = rfImpVal, type="prob"))
rfPredProbVal$tdid <- rfImpVal$tdid
rfUserValPred <- ddply(rfPredProbVal, .(tdid), summarize,
    BlindedGenre1 = mean(BlindedGenre1),
    BlindedGenre2 = mean(BlindedGenre2),
    BlindedGenre3 = mean(BlindedGenre3),
    BlindedGenre4 = mean(BlindedGenre4),
    BlindedGenre5 = mean(BlindedGenre5)
)

rfUserVal$meanpred <- colnames(rfUserValPred)[apply(rfUserValPred, 1, which.max)]











meltSiteFmg <- melt(impTrain, id.vars=c("site", "FavoriteMovieGenre"), measure.vars=NULL, na.rm=TRUE)

# this is a tunable parameter!
theta <- 2

n <- nrow(impTrain)
genreTable <- table(impTrain$FavoriteMovieGenre) / n

smoothProps <- ddply(meltSiteFmg, .(site), summarize,
    BlindedGenre1 = (sum(FavoriteMovieGenre == "BlindedGenre1") + theta * (genreTable[[1]]/length(site))) / (length(site) + theta),
    BlindedGenre2 = (sum(FavoriteMovieGenre == "BlindedGenre2") + theta * (genreTable[[2]]/length(site))) / (length(site) + theta),
    BlindedGenre3 = (sum(FavoriteMovieGenre == "BlindedGenre3") + theta * (genreTable[[3]]/length(site))) / (length(site) + theta),
    BlindedGenre4 = (sum(FavoriteMovieGenre == "BlindedGenre4") + theta * (genreTable[[4]]/length(site))) / (length(site) + theta),
    BlindedGenre5 = (sum(FavoriteMovieGenre == "BlindedGenre5") + theta * (genreTable[[5]]/length(site))) / (length(site) + theta)
)

smoothProps <- na.omit(smoothProps)
rownames(smoothProps) <- smoothProps $site

missingSites <- !(levels(impData$site) %in% rownames(smoothProps))
numSites <- sum(missingSites)
otherProps <- data.frame(site = levels(impData$site)[missingSites],
                         BlindedGenre1 = rep(genreTable[[1]], numSites),
                         BlindedGenre2 = rep(genreTable[[2]], numSites),
                         BlindedGenre3 = rep(genreTable[[3]], numSites),
                         BlindedGenre4 = rep(genreTable[[4]], numSites),
                         BlindedGenre5 = rep(genreTable[[5]], numSites),
                         row.names = levels(impData$site)[missingSites]
)

smoothProps <- rbind(smoothProps, otherProps)

impTrain$SiteGenreProb1 <- sapply(as.character(impTrain$site), function(x) smoothProps[x, "BlindedGenre1"])
impTrain$SiteGenreProb2 <- sapply(as.character(impTrain$site), function(x) smoothProps[x, "BlindedGenre2"])
impTrain$SiteGenreProb3 <- sapply(as.character(impTrain$site), function(x) smoothProps[x, "BlindedGenre3"])
impTrain$SiteGenreProb4 <- sapply(as.character(impTrain$site), function(x) smoothProps[x, "BlindedGenre4"])
impTrain$SiteGenreProb5 <- sapply(as.character(impTrain$site), function(x) smoothProps[x, "BlindedGenre5"])
tdidGenreTrain <- ddply(impTrain, .(tdid), summarize, FavoriteMovieGenre = Mode(FavoriteMovieGenre))

impVal$SiteGenreProb1 <- sapply(as.character(impVal$site), function(x) smoothProps[x, "BlindedGenre1"])
impVal$SiteGenreProb2 <- sapply(as.character(impVal$site), function(x) smoothProps[x, "BlindedGenre2"])
impVal$SiteGenreProb3 <- sapply(as.character(impVal$site), function(x) smoothProps[x, "BlindedGenre3"])
impVal$SiteGenreProb4 <- sapply(as.character(impVal$site), function(x) smoothProps[x, "BlindedGenre4"])
impVal$SiteGenreProb5 <- sapply(as.character(impVal$site), function(x) smoothProps[x, "BlindedGenre5"])
tdidGenreVal <- ddply(impVal, .(tdid), summarize, FavoriteMovieGenre = Mode(FavoriteMovieGenre))


# -------Random Forest Model-----------
rfMod <- randomForest(FavoriteMovieGenre ~ userHourOfDay + userDayOfWeek + country + devicetype + osfamily + 
                      browser + SiteGenreProb1 + SiteGenreProb2 + SiteGenreProb3 + SiteGenreProb4 + SiteGenreProb5,
                      data = impTrain, na.action=na.omit)

rfMod
rfDf <- na.omit(impTrain[,c("tdid", "userHourOfDay", "userDayOfWeek", "country", "devicetype", "osfamily", 
                            "browser", "SiteGenreProb1", "SiteGenreProb2", "SiteGenreProb3", "SiteGenreProb4", 
                            "SiteGenreProb5", "FavoriteMovieGenre")])
rfPredTrain <- predict(rfMod)
rfRowPredTrain <- data.frame(tdid = rfDf$tdid, preds = rfPredTrain)
rownames(rfRowPredTrain) <- rownames(rfDf)
rfTdidPredTrain <- ddply(rfRowPredTrain, .(tdid), summarize, pred = Mode(preds))
rfTdidGenreTrain <- ddply(rfDf, .(tdid), summarize, FavoriteMovieGenre = Mode(FavoriteMovieGenre))
table(rfTdidGenreTrain$FavoriteMovieGenre, rfTdidPredTrain$pred)

rfPredVal <- predict(rfMod, newdata = impVal)
rfRowPredVal <- data.frame(tdid = impVal$tdid, preds = rfPredVal)
rownames(rfRowPredVal) <- rownames(impVal)
rfTdidPredVal <- ddply(rfRowPredVal, .(tdid), summarize, pred = Mode(preds))
table(tdidGenreVal$FavoriteMovieGenre, rfTdidPredVal$pred)


# -----Rpart (classification tree) model---------
treeMod <- rpart(FavoriteMovieGenre ~ userHourOfDay + userDayOfWeek + country + devicetype + osfamily + 
                 browser + SiteGenreProb1 + SiteGenreProb2 + SiteGenreProb3 + SiteGenreProb4 + SiteGenreProb5, 
                 data = impTrain)

treePredTrain <- predict(treeMod, type="class")
treeRowPredTrain <- data.frame(tdid = impTrain$tdid, preds = treePredTrain)
rownames(treeRowPredTrain) <- rownames(impTrain)
treeTdidPredTrain <- ddply(treeRowPredTrain, .(tdid), summarize, pred = Mode(preds))
table(tdidGenreTrain$FavoriteMovieGenre, treeTdidPredTrain$pred)

treePredVal <- predict(treeMod, newdata = impVal, type="class")
treeRowPredVal <- data.frame(tdid = impVal$tdid, preds = treePredVal)
rownames(treeRowPredVal) <- rownames(impVal)
treeTdidPredVal <- ddply(treeRowPredVal, .(tdid), summarize, pred = Mode(preds))
table(tdidGenreVal$FavoriteMovieGenre, treeTdidPredVal$pred)


# --------Naive Bayes Model---------

nbMod <- naiveBayes(FavoriteMovieGenre ~ userHourOfDay + userDayOfWeek + country + devicetype + osfamily + 
                 browser + site, 
                 data = impTrain)

nbPredTrain <- predict(nbMod, type="class")
nbRowPredTrain <- data.frame(tdid = impTrain$tdid, preds = nbPredTrain)
rownames(nbRowPredTrain) <- rownames(impTrain)
nbTdidPredTrain <- ddply(nbRowPredTrain, .(tdid), summarize, pred = Mode(preds))
table(tdidGenreTrain$FavoriteMovieGenre, nbTdidPredTrain$pred)

nbPredVal <- predict(nbMod, newdata = impVal, type="class")
nbRowPredVal <- data.frame(tdid = impVal$tdid, preds = nbPredVal)
rownames(nbRowPredVal) <- rownames(impVal)
nbTdidPredVal <- ddply(nbRowPredVal, .(tdid), summarize, pred = Mode(preds))
table(tdidGenreVal$FavoriteMovieGenre, nbTdidPredVal$pred)




# -------Random Forest Model2-----------
rf2Mod <- randomForest(FavoriteMovieGenre ~ SiteGenreProb1 + SiteGenreProb2 + SiteGenreProb3 + SiteGenreProb4 + SiteGenreProb5,
                       data = impTrain, na.action=na.omit)

rf2Df <- na.omit(impTrain[,c("tdid", "SiteGenreProb1", "SiteGenreProb2", "SiteGenreProb3", "SiteGenreProb4", 
                            "SiteGenreProb5", "FavoriteMovieGenre")])
rf2PredTrain <- predict(rf2Mod)
rf2RowPredTrain <- data.frame(tdid = rf2Df$tdid, preds = rf2PredTrain)
rownames(rf2RowPredTrain) <- rownames(rf2Df)
rf2TdidPredTrain <- ddply(rf2RowPredTrain, .(tdid), summarize, pred = Mode(preds))
rf2TdidGenreTrain <- ddply(rf2Df, .(tdid), summarize, FavoriteMovieGenre = Mode(FavoriteMovieGenre))
table(rf2TdidGenreTrain$FavoriteMovieGenre, rf2TdidPredTrain$pred)

rf2PredVal <- predict(rf2Mod, newdata = impVal)
rf2RowPredVal <- data.frame(tdid = impVal$tdid, preds = rf2PredVal)
rownames(rf2RowPredVal) <- rownames(impVal)
rf2TdidPredVal <- ddply(rf2RowPredVal, .(tdid), summarize, pred = Mode(preds))
table(tdidGenreVal$FavoriteMovieGenre, rf2TdidPredVal$pred)








propSiteFmg <- ddply(meltSiteFmg, .(site), summarize,
    BlindedGenre1 = sum(FavoriteMovieGenre == "BlindedGenre1") / length(site),
    BlindedGenre2 = sum(FavoriteMovieGenre == "BlindedGenre2") / length(site),
    BlindedGenre3 = sum(FavoriteMovieGenre == "BlindedGenre3") / length(site),
    BlindedGenre4 = sum(FavoriteMovieGenre == "BlindedGenre4") / length(site),
    BlindedGenre5 = sum(FavoriteMovieGenre == "BlindedGenre5") / length(site)
)
propSiteFmg <- na.omit(propSiteFmg)
rownames(propSiteFmg) <- propSiteFmg$site



theta = 20
n <- nrow(impTrain)
genreTable <- table(impTrain$FavoriteMovieGenre) / n
propSiteFmg2 <- ddply(meltSiteFmg, .(site), summarize,
    BlindedGenre1 = (sum(FavoriteMovieGenre == "BlindedGenre1") + theta * (genreTable[[1]]/length(site))) / (length(site) + theta),
    BlindedGenre2 = (sum(FavoriteMovieGenre == "BlindedGenre2") + theta * (genreTable[[2]]/length(site))) / (length(site) + theta),
    BlindedGenre3 = (sum(FavoriteMovieGenre == "BlindedGenre3") + theta * (genreTable[[3]]/length(site))) / (length(site) + theta),
    BlindedGenre4 = (sum(FavoriteMovieGenre == "BlindedGenre4") + theta * (genreTable[[4]]/length(site))) / (length(site) + theta),
    BlindedGenre5 = (sum(FavoriteMovieGenre == "BlindedGenre5") + theta * (genreTable[[5]]/length(site))) / (length(site) + theta)
)
propSiteFmg2 <- na.omit(propSiteFmg2)
rownames(propSiteFmg2) <- propSiteFmg2$site



SiteAvg <- function(sites, genre, props) {
    #print(sites)
    wgt <- 0
    count <- 0
    for (i in 1:length(sites)) {
        iwgt <- props[as.character(sites[i]), genre]
        #print(iwgt)
        if (!is.na(iwgt)) {
            wgt <- wgt + iwgt
            count <- count + 1
        }
    }
    wgt <- wgt / count
    wgt
}

userTrain <- ddply(impTrain, .(tdid), summarize, 
    country = Mode(country),
    region = Mode(region),
    metro = Mode(metro),
    city = Mode(city),
    devicetype = Mode(devicetype),
    osfamily = Mode(osfamily),
    os = Mode(os),
    browser = Mode(browser),
    FavoriteMovieGenre = Mode(FavoriteMovieGenre),
    favsite = Mode(site),
    userHourOfDay = mean(userHourOfDay),
    SiteWgt1 = SiteAvg(site, "BlindedGenre1", propSiteFmg2),
    SiteWgt2 = SiteAvg(site, "BlindedGenre2", propSiteFmg2),
    SiteWgt3 = SiteAvg(site, "BlindedGenre3", propSiteFmg2),
    SiteWgt4 = SiteAvg(site, "BlindedGenre4", propSiteFmg2),
    SiteWgt5 = SiteAvg(site, "BlindedGenre5", propSiteFmg2)
)
rownames(userTrain) <- userTrain$tdid

# plot boxplots of SiteWgts for each favorite genre
#g <- ggplot(userTrain, aes(FavoriteMovieGenre, SiteWgt5)) + geom_boxplot()
#gsum

userVal <- ddply(impVal, .(tdid), summarize, 
    country = Mode(country),
    region = Mode(region),
    metro = Mode(metro),
    city = Mode(city),
    devicetype = Mode(devicetype),
    osfamily = Mode(osfamily),
    os = Mode(os),
    browser = Mode(browser),
    FavoriteMovieGenre = Mode(FavoriteMovieGenre),
    favsite = Mode(site),
    userHourOfDay = mean(userHourOfDay),
    SiteWgt1 = SiteAvg(site, "BlindedGenre1", propSiteFmg2),
    SiteWgt2 = SiteAvg(site, "BlindedGenre2", propSiteFmg2),
    SiteWgt3 = SiteAvg(site, "BlindedGenre3", propSiteFmg2),
    SiteWgt4 = SiteAvg(site, "BlindedGenre4", propSiteFmg2),
    SiteWgt5 = SiteAvg(site, "BlindedGenre5", propSiteFmg2)
)
rownames(userVal) <- userVal$tdid

userCombine <- rbind(userTrain, userVal)
userCombine[1:11] <- as.data.frame(lapply(userCombine[1:11], factor))
userTrain <- userCombine[rownames(userTrain),]
userVal <- userCombine[rownames(userVal),]

userTrain <- na.omit(userTrain)
userVal <- na.omit(userVal)

rf1 <- randomForest(FavoriteMovieGenre ~ userHourOfDay + osfamily + os + browser + devicetype + country + SiteWgt1 + SiteWgt2 + SiteWgt3 + SiteWgt4 + SiteWgt5, data = userTrain)
rfpred1 <- predict(rf1, newdata = userVal)

tree1 <- rpart(FavoriteMovieGenre ~ userHourOfDay + osfamily + os + browser + devicetype + country + SiteWgt1 + SiteWgt2 + SiteWgt3 + SiteWgt4 + SiteWgt5, data = userTrain)
treepred1 <- predict(tree1, newdata = userVal, type="class")

