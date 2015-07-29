library(ggplot2)
library(caTools)
library(reshape2)
library(plyr)
library(randomForest)
library(rpart)
library(rpart.plot)
library(e1071)
library(datasets)

#~~~~~~~~~~~~ GETTING AND CLEANING DATA ~~~~~~~~~~~~#

# first read in the data file (strings will be factors)
setwd("C:/Users/David/workspace/Final Project/")
impData <- read.table('ImpData.txt', header=T, sep='\t', 
        na.strings='(null)', quote="")

# format some of the variables to dates and factors
impData$logentrytime <- as.POSIXct(impData$logentrytime,
        format='%m/%d/%Y %H:%M', tz = "GMT")
impData$metro <- factor(impData$metro)
impData$FavoriteMovieGenre <- as.character(impData$FavoriteMovieGenre)
impData$tdid <- as.character(impData$tdid)
impData$logfileid <- NULL

# do some work on the site strings
impData$site <- as.character(impData$site)
impData$site <- sub(".*\\.(.*\\..*)$", "\\1", impData$site)
impData$site[grepl(".*\\.site-not-provided", impData$site)] <- NA
impData$site <- factor(impData$site)

# get some more info on when the impression was served
impData$userHourOfDay <- impData$userHourOfWeek %% 24
# weeHours <- impData$userHourOfDay < 5 & !is.na(impData$userHourOfDay)
# normHours <- impData$userHourOfDay >= 5 & !is.na(impData$userHourOfDay)
# impData$userHourOfDay[weeHours] = impData$userHourOfDay[weeHours] + 19
# impData$userHourOfDay[normHours] = impData$userHourOfDay[normHours] - 5
impData$userDayOfWeek <- factor(impData$userHourOfWeek %/% 24, labels = c("Sunday", 
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

# create a variable that only uses the top 30 most popular sites
impData$top50site <- as.character(impData$site)
topSites <- names(sort(table(impKnown$site), decreasing=T)[1:50])
impData$top50site[!(impData$top50site %in% topSites)] <- "other"
impData$top50site <- factor(impData$top50site)

# create a variable that only uses the top 30 most popular sites
impData$top100site <- as.character(impData$site)
topSites <- names(sort(table(impKnown$site), decreasing=T)[1:100])
impData$top100site[!(impData$top100site %in% topSites)] <- "other"
impData$top100site <- factor(impData$top100site)

# split the data into those where we know the genre 
# and those where we do not
impKnown <- subset(impData, FavoriteMovieGenre != "?????")
impKnown$FavoriteMovieGenre <- as.factor(impKnown$FavoriteMovieGenre)
impKnown$tdid <- factor(impKnown$tdid)

impUnknown <- subset(impData, FavoriteMovieGenre == "?????")
impUnknown$FavoriteMovieGenre <- NULL
impUnknown$tdid <- factor(impUnknown$tdid)


#~~~~~~~~~~~~ EXPLORATORY ANALYSIS ~~~~~~~~~~~~#

# we'll use this function to summarize data by user
Mode <- function(x) {
    xtable <- table(x)
    if (sum(xtable)) {
        names(which.max(table(x)))
    }
    else 
        NA
}

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
    site = Mode(site),
    usregion = Mode(usregion),
    userDayOfWeek = Mode(userDayOfWeek),
    userHourOfDay = mean(userHourOfDay)
)
rownames(userKnown) <- userKnown$tdid
userKnown[2:13] <- as.data.frame(lapply(userKnown[2:13], factor))

# Look at english-speaking countries vs rest of the world
userKnown$english <- as.character(userKnown$country) %in%
        c("United States", "United Kingdom", "Canada", "Ireland") 
userKnown$usa <- as.character(userKnown$country) == "United States"

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

# Breakdown of browser by genre
ggplot(userKnown, aes(x = FavoriteMovieGenre, fill = browser)) + 
        geom_bar(stat="bin")

# Breakdown of genre by day of week
ggplot(userKnown, aes(x = userDayOfWeek, fill = FavoriteMovieGenre)) + 
        geom_bar(stat="bin")

ggplot(impKnown[impKnown$top50site != "other",], 
        aes(x = FavoriteMovieGenre, fill = FavoriteMovieGenre)) +
        geom_bar(stat="bin") +
        facet_wrap(~top50site)

ggplot(impKnown[impKnown$top100site != "other",], 
        aes(x = FavoriteMovieGenre, fill = FavoriteMovieGenre)) +
        geom_bar(stat="bin") +
        facet_wrap(~top100site)




#~~~~~~~~~~~~ MODEL BUILDING ~~~~~~~~~~~~#

# split into train and validation sets
impKnown$tdid <- as.character(impKnown$tdid)
split <- sample.split(impKnown$FavoriteMovieGenre, SplitRatio = 0.7, 
                        group = impKnown$tdid)
impTrain <- subset(impKnown,  split)
impVal   <- subset(impKnown, !split)
impTrain$tdid <- as.factor(impTrain$tdid)
impVal$tdid <- as.factor(impVal$tdid)




















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


# ********** EXPLORATORY ANALYSIS ************** #

# get counts of favorite genre based on site visited
meltSiteDF <- melt(impTrain, id.vars=c("FavoriteMovieGenre", "site"), measure.vars=NULL)
siteFMGsum <- dcast(meltSiteDF, site ~ FavoriteMovieGenre)

top20s1 <- siteFMGsum[order(siteFMGsum$BlindedGenre1, decreasing=T)[1:20], c(1,2), drop=F]
top20s2 <- siteFMGsum[order(siteFMGsum$BlindedGenre2, decreasing=T)[1:20], c(1,3), drop=F]
top20s3 <- siteFMGsum[order(siteFMGsum$BlindedGenre3, decreasing=T)[1:20], c(1,4), drop=F]
top20s4 <- siteFMGsum[order(siteFMGsum$BlindedGenre4, decreasing=T)[1:20], c(1,5), drop=F]
top20s5 <- siteFMGsum[order(siteFMGsum$BlindedGenre5, decreasing=T)[1:20], c(1,6), drop=F]

meltCountryDF <- melt(impTrain, id.vars=c("FavoriteMovieGenre", "country"), measure.vars=NULL)
countryFMGsum <- dcast(meltCountryDF, country ~ FavoriteMovieGenre)

top20c1 <- countryFMGsum[order(countryFMGsum$BlindedGenre1, decreasing=T)[1:20], c(1,2), drop=F]
top20c2 <- countryFMGsum[order(countryFMGsum$BlindedGenre2, decreasing=T)[1:20], c(1,3), drop=F]
top20c3 <- countryFMGsum[order(countryFMGsum$BlindedGenre3, decreasing=T)[1:20], c(1,4), drop=F]
top20c4 <- countryFMGsum[order(countryFMGsum$BlindedGenre4, decreasing=T)[1:20], c(1,5), drop=F]
top20c5 <- countryFMGsum[order(countryFMGsum$BlindedGenre5, decreasing=T)[1:20], c(1,6), drop=F]

# NOTE - NEED TO GROUP USERS TOGETHER LIKE IN THE AIRBNB ML POST