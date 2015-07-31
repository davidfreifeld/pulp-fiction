import urllib2
import xmltodict

apiKey = "zUIGhN5OAVAm"
classifierName = "Topics"

def getSiteClassificationData(apiKey, classifierName, url):
    requestURL = "http://uclassify.com/browse/uClassify/" + \
        classifierName + "/ClassifyUrl?readkey=" + apiKey + \
        "&url=" + url + "&version=1.01"
    file = urllib2.urlopen(requestURL)
    data = file.read()
    file.close()

    dataDict = xmltodict.parse(data)
    return dataDict

def computeCategory(dataDict, threshold = 0.8):
    classList = dataDict['uclassify']['readCalls']['classify']['classification']['class']
    maxProb = 0
    maxClass = 'Other'
    for category in classList:
        prob = float(category['@p'])
        if (prob > threshold and prob > maxProb):
            maxProb = prob
            maxClass = category['@className']
    return (maxClass, maxProb)

def run(urls, threshold = 0.8):
    categories = []
    for url in urls:
        dataDict = getSiteClassificationData(apiKey, classifierName, url)
        categories.append(computeCategory(dataDict, threshold))
    return categories

urls = ["vikings.com", "cnn.com", "wikipedia.org", "pinterest.com"]
categories = run(urls, 0)
