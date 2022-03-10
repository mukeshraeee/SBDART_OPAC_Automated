# Python script to manipulate raw data from sav file to individual files

# ========================================================================
# CHANGE THESE !!!
# ========================================================================
processedDataPath = '/home/mrai/Desktop/AutomateOpacSbdart/AutomateSbdart/Data/processedData/'
rawDataFolder = '/home/mrai/Desktop/AutomateOpacSbdart/AutomateSbdart/Data/rawData/'

wlbaerColumn = 1
qbaerColumn = 2
wbaerColumn = 5
gbaerColumn = 6
tbaerColumn = 7

numberOfLines = 12
# ========================================================================
# ========================================================================

rawDataFileName = 'OPACFINAL.csv'
rawDataFullPath = rawDataFolder + rawDataFileName
wlbaerFullPath = processedDataPath + 'wlbaer.csv'
qbaerFullPath = processedDataPath + 'qbaer.csv'
wbaerFullPath = processedDataPath + 'wbaer.csv'
gbaerFullPath = processedDataPath + 'gbaer.csv'
tbaerFullPath =processedDataPath + 'tbaer.csv'

with open(rawDataFullPath) as rawDataFile, \
     open(wlbaerFullPath, 'w') as wlbaerFile, \
     open(qbaerFullPath, 'w') as qbaerFile, \
     open(wbaerFullPath, 'w') as wbaerFile, \
     open(gbaerFullPath, 'w') as gbaerFile, \
     open(tbaerFullPath, 'w') as tbaerFile:
    while True:
        wlbaer, qbaer, wbaer, gbaer, tbaer = [], [], [], [], []
        for lines in range(1, numberOfLines + 1):
            lineData = rawDataFile.readline()
            individualData = lineData.split(' ')
            print(individualData)
            wlbaer.append(float(individualData[wlbaerColumn]))
            qbaer.append(float(individualData[qbaerColumn]))
            wbaer.append(float(individualData[wbaerColumn]))
            gbaer.append(float(individualData[gbaerColumn]))
            tbaer.append(float(individualData[tbaerColumn]))
        wlbaerFile.write(str(wlbaer)[1:-1]+ '\n')
        qbaerFile.write(str(qbaer)[1:-1]+ '\n')
        wbaerFile.write(str(wbaer)[1:-1]+ '\n')
        gbaerFile.write(str(gbaer)[1:-1]+ '\n')
        tbaerFile.write(str(tbaer)[1:-1]+ '\n')
        print('COMPLETED')

