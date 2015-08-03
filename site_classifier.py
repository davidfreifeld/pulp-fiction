import urllib2
import xmltodict
import time

apiKey = "zUIGhN5OAVAm"
classifierName = "Topics"

def getSiteClassificationData(apiKey, classifierName, url):
    requestURL = "http://uclassify.com/browse/uClassify/" + \
        classifierName + "/ClassifyUrl?readkey=" + apiKey + \
        "&url=" + url + "&version=1.01"
    try:
        myFile = urllib2.urlopen(requestURL)
        data = myFile.read()
        myFile.close()
        dataDict = xmltodict.parse(data)
        return dataDict
    except urllib2.HTTPError:
        print "Error for url " + url
        return {}

def getCategories(dataDict):
    categoryDict = {}
    if (len(dataDict) and dataDict['uclassify']['status']['@success'] == 'true' and \
            float(dataDict['uclassify']['readCalls']['classify']['classification']['@textCoverage'])):
        classList = dataDict['uclassify']['readCalls']['classify']['classification']['class']
        for category in classList:
            categoryDict[category['@className']] = category['@p']
    return categoryDict
            

def getMaxCat(categories, threshold = 0):
    maxProb = 0
    maxClass = u'Other'
    for cat, prob in categories.iteritems():
        probNum = float(prob)
        if (probNum > threshold and probNum > maxProb):
            maxProb = probNum
            maxClass = cat
    return (maxClass, maxProb)

def run(urls, threshold = 0.8):
    categories = []
    maxCats = []
    for url in urls:
        print "Getting site " + url
        dataDict = getSiteClassificationData(apiKey, classifierName, url)
        category = getCategories(dataDict)
        categories.append(category)
        maxCats.append(getMaxCat(category))
        time.sleep(10)
    return (categories, maxCats)
    
def writeFile(urls, categories, maxCats, fileName):
    myFile = open(fileName, 'a+')
    
    catNames = ['Arts', 'Business', 'Science', 'Computers', 'Recreation', \
        'Sports', 'Society', 'Health', 'Home', 'Games']
    
    myFile.write('URL,MaxCategory,MaxProb,' + ','.join(catNames) + '\n')
    for i, cat in enumerate(categories): 
        writeString = urls[i] + ','
        writeString += maxCats[i][0] + ',' + str(maxCats[i][1])
        if (len(cat)):
            for catName in catNames:
                writeString += ',' + str(cat[catName])
        else:
            writeString += ',,,,,,,,,,'
        myFile.write(writeString + '\n')
    myFile.close()

sitesFile = open('sites.csv')
siteHeader = sitesFile.readline()
sitesString = sitesFile.read()
sitesFile.close()
sitesList = sitesString.split('\n')

urls = sitesList[4000:]
categories2, maxCats2 = run(urls, 0)

