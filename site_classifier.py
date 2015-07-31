import urllib2
import xmltodict

apiKey = "zUIGhN5OAVAm"
classifierName = "Topics"

def getSiteClassificationData(apiKey, classifierName, url):
    requestURL = "http://uclassify.com/browse/uClassify/" + classifierName + "/ClassifyUrl?readkey=" + apiKey + "&url=" + url + "&version=1.01"
    file = urllib2.urlopen(requestURL)
    data = file.read()
    file.close()

    dataDict = xmltodict.parse(data)
    return dataDict
    
url = "www.vikings.com"
dataDict = getSiteClassificationData(apiKey, classifierName, url)
