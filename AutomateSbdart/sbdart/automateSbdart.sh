#!/bin/bash

# Automate sbdart model.
# TODO: Parameter 'taerst' doesnot work. Omitted. Find solution.
# TODO: Vary the parameter 'albcon'.

# Paths
# CHANGE!!
# ================================================================
dataFolder="/home/mrai/Desktop/AutomateOpacSbdart/AutomateSbdart/Data/processedData/"
outputFolder="/home/mrai/Desktop/AutomateOpacSbdart/AutomateSbdart/Outputs/"
# ================================================================

# Remove all files contained within the output folder. (To avoid appending to previous output files!)
echo "removing previous output files...
"
rm -f $outputFolder*

# Open all required data files. Files created by 'data.py' (1) script. See inside 'Data' folder.
# 	(1) Except 'julianDay.csv', 'wv.csv' and 'uo3.csv'. These files should be created manually.

exec 2<$dataFolder"julianDay.csv"
exec 3<$dataFolder"wv.csv"
exec 4<$dataFolder"uo3.csv"
exec 5<$dataFolder"wlbaer.csv"
exec 6<$dataFolder"qbaer.csv"
exec 7<$dataFolder"wbaer.csv"
exec 8<$dataFolder"gbaer.csv"
exec 9<$dataFolder"tbaer.csv"
exec 10<$dataFolder"albcon.csv"


# Read all the data files simultaneously one line at a time. 
echo "All the required files are read ... 
"
while read julianDay <&2 && read uw <&3 && read uo <&4 && read wlbaer <&5 && read qbaer <&6 && read wbaer <&7 && read gbaer <&8 && read tbaer <&9 && read albcon <&10


do #Initialize 'while' statement block.

# WITH AEROSOL. iaer=5.
# =======================================================================================================================
for time in {00..23..01}; do  #Initialize 'for' block for varying 'sza' values for each day. i.e. one day has 19 sza values
echo "
&input
time=$time,
iday=$julianDay,
alat=28.21,
alon=85.61,   
wlinf=0.25, 
wlsup=4.0, 
wlinc=0.05,
zout=0,100,
idatm=0,
uw=$uw,
uo3=$uo,
isalb=0,
albcon=$albcon,
iaer=5,
wlbaer=$wlbaer,
qbaer=$qbaer,
wbaer=$wbaer,
gbaer=$gbaer,
iout=10
$end
/" > INPUT 
./sbdart >> $outputFolder"YA_"$julianDay.out
done		# End of 'for' block.
echo "YA_"$julianDay.out" ...created"
# =======================================================================================================================


# WITHOUT AEROSOL. iaer=0.
# =======================================================================================================================
for time in {00..23..01}; do  #TODO: Check if this statement is needed. Output suggests that 'sza' values are ignored when 'iaer'=0
echo "
&input
time=$time,
iday=$julianDay,
alat=28.21,
alon=85.61,   
wlinf=0.25, 
wlsup=4.0, 
wlinc=0.05,
zout=0,100,
idatm=0,
uw=$uw,
uo3=$uo,
isalb=0,
albcon=$albcon,
wlbaer=0,
qbaer=0,
wbaer=0,
gbaer=0,
iout=10
$end
/" > INPUT 
./sbdart >> $outputFolder"NA_"$julianDay.out
done		# End of 'for' block.
echo "NA_"$julianDay.out" ...created"
# =======================================================================================================================

done		# End of 'while' block.

# Close all opened files.
exec 2<&-
exec 3<&-
exec 4<&-
exec 5<&-
exec 6<&-
exec 7<&-
exec 8<&-
exec 9<&-
exec 10<&-

echo "***   COMPLETE   ***"
