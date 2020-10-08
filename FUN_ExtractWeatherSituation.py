#a script to extract the weather situation from a .txt file available on the dwd website
#see: 
#https://www.dwd.de/DE/leistungen/wetterlagenklassifikation/online_wlkdaten.txt?view=nasPublication&nn=16102
#https://www.dwd.de/DE/leistungen/wetterlagenklassifikation/kennzahlen_kennungen.html?nn=16102&lsbId=375412

#------------------------------------------------------------------------------
# 1 SET FOLDER PATH
#------------------------------------------------------------------------------
# workingDir = "C:/Users/tamta/Documents/Studium/02_Master/17_Masterarbeit/03_Data/"

#------------------------------------------------------------------------------
# 2 LOAD THE DATA
#------------------------------------------------------------------------------
# with open(workingDir+"Wetterlagenklassifikation.txt") as weather:
#     file = weather.readlines()
    
# file = file[1:13]
# file = file[11]
# weatherPerDay = file.split(" ")

# weatherPerDay = list(filter(("").__ne__, weatherPerDay))
# weatherPerDay = weatherPerDay[1:-1]

#------------------------------------------------------------------------------
# 3 DEFINE THE FUNCTION
#------------------------------------------------------------------------------

def weatherSituation(workingDir, filename, month):
    """
    Extract the weather situation from the DWD website format.
    
    The historic weather situation can be found here:
    https://www.dwd.de/DE/leistungen/wetterlagenklassifikation/online_wlkdaten.txt?view=nasPublication&nn=16102
    Its interpretation can be found here:
    https://www.dwd.de/DE/leistungen/wetterlagenklassifikation/kennzahlen_kennungen.html?nn=16102&lsbId=375412
    This function extracts first the year 2017 (research period of Master's thesis).
    Subsequently, one month is derived, in which the singel daily resolution.
    
    Parameters
    ----------
    workingDir : string
        The working directory
    filename : string
        The file name of the .txt DWD table
    month : number
        The number of the studied month. January is equivalent to 1 and so on
    
    Returns
    -------
    list with strings
    
    """
    #open the file
    with open(workingDir+filename) as weather:
        file = weather.readlines()
    #drop supplement information
    file = file[1:13]
    #extract the month row
    file = file[month-1]
    #split the list on every space
    weatherPerDay = file.split(" ")
    #drop remaining spaces
    weatherPerDay = list(filter(("").__ne__, weatherPerDay))
    #drop 0, as it indicates month with less than 31 days
    weatherPerDay = list(filter(("0").__ne__, weatherPerDay))
    #drop the date information and the last line break
    weatherPerDay = weatherPerDay[1:-1]
    
    return weatherPerDay    
    