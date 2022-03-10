#!/bin/bash

# TODO: Relative Humidity is always on integer. All data in 'rh.csv' files should be integers.

# Data Paths
inputDataFolder="/home/mrai/Desktop/AutomateOpacSbdart/AutomateOpac/inputData/"
outputFolder="/home/mrai/Desktop/AutomateOpacSbdart/AutomateOpac/opacOutput/"
sbdartRawDataFolder="/home/mrai/Desktop/AutomateOpacSbdart/AutomateSbdart/Data/rawData/"

# CAREFUL CHANGING THIS!!
startingLineNumber=22
numberOfLines=12

# Remove all files contained within the output folder. (To avoid appending to previous output files!)
echo "
removing previous output files...
"
rm -f $outputFolder*
rm -f $sbdartRawDataFolder*

# Open all required csv files.
exec 2<$inputDataFolder"bc.csv"
exec 3<$inputDataFolder"rh.csv"
exec 4<$inputDataFolder"ml.csv"
exec 5<$inputDataFolder"mt.csv"
exec 6<$inputDataFolder"ft.csv"
exec 7<$inputDataFolder"st.csv"
exec 8<$inputDataFolder"julianDay.csv"

while read bc <&2 && read rh <&3 && read ml <&4 && read mt <&5 && read ft <&6 && read st <&7 && read julianDay <&8
do
if [ "$rh" -ge 0 ] && [ "$rh" -le 25 ]; then
	rh0=1
	rh50=0
	rh70=0
	rh80=0
	rh90=0
	rh95=0
	rh98=0
	rh99=0
elif [ "$rh" -ge 25 ] && [ "$rh" -le 60 ]; then
	rh0=0
	rh50=1
	rh70=0
	rh80=0
	rh90=0
	rh95=0
	rh98=0
	rh99=0
elif [ "$rh" -ge 60 ] && [ "$rh" -le 75 ]; then
	rh0=0
	rh50=0
	rh70=1
	rh80=0
	rh90=0
	rh95=0
	rh98=0
	rh99=0
elif [ "$rh" -ge 75 ] && [ "$rh" -le 85 ]; then
	rh0=0
	rh50=0
	rh70=0
	rh80=1
	rh90=0
	rh95=0
	rh98=0
	rh99=0
elif [ "$rh" -ge 85 ] && [ "$rh" -le 92 ]; then
	rh0=0
	rh50=0
	rh70=0
	rh80=0
	rh90=1
	rh95=0
	rh98=0
	rh99=0
elif [ "$rh" -ge 92 ] && [ "$rh" -le 96 ]; then
	rh0=0
	rh50=0
	rh70=0
	rh80=0
	rh90=0
	rh95=1
	rh98=0
	rh99=0
elif [ "$rh" -ge 96 ] && [ "$rh" -le 98 ]; then
	rh0=0
	rh50=0
	rh70=0
	rh80=0
	rh90=0
	rh95=0
	rh98=1
	rh99=0
else
	rh0=0
	rh50=0
	rh70=0
	rh80=0
	rh90=0
	rh95=0
	rh98=0
	rh99=1
fi
echo "# INPUT FILE FOR OPAC 3.1 ---------------------------------------------------
#
# In this file, you have to specify which information you want to
# extract from the data base.
#
# 5 parts of desired data have to be selected:
#
# 1. cloud or aerosol type
# 2. height profile
# 3. wavelengths
# 4. relative humidity
# 5. optical parameters
#
# All lines which don't start with a # have to be filled in.
#
# Number and order of lines in this file may not be changed.
#
# ************************* -------------------------------------------------
# 1. cloud or aerosol type:
# *************************
#
# The following cloud or aerosol types may be selected by the number in
# parenthesis. One type may be selected for one program run.
#
#  (0) define new mixture
#
#  (1) continental clean                  (6) maritime clean
#  (2) continental average                (7) maritime polluted
#  (3) continental polluted               (8) maritime tropical
#  (4) urban                              (9) Arctic
#  (5) desert                            (10) Antarctic
#
# (11) stratus (continental)             (16) fog
# (12) stratus (maritime)                (17) cirrus 1 (-25 C)
# (13) cumulus (continental, clean)      (18) cirrus 2 (-50 C)
# (14) cumulus (continental, polluted)   (19) cirrus 3 (-50 C, + small part.)   
# (15) cumulus (maritime)
#
# ---------------------------------------------------------------------------
0                               : give here the selected number
# ---------------------------------------------------------------------------
#
# if you did select 0 (define new mixture), in the next 5 lines the
# component number and the number density in [particles/cm**3] has to be
# given, divided by commas. The component numbers are the following:
#
# There are not more than 5 components allowed to compose one aerosol
# or cloud type.
#
#  (1) insoluble                         (6) mineral (nuclei mode)
#  (2) water-soluble                     (7) mineral (accumulation mode)
#  (3) soot                              (8) mineral (coarse mode)
#  (4) sea-salt (accumulation mode)      (9) mineral (transported)
#  (5) sea-salt (coarse mode)           (10) sulfate
#
# (11) stratus (continental)            (15) cumulus (maritime)
# (12) stratus (maritime)               (16) fog (small drops)
# (13) cumulus (continental, clean)     (17) fog (large drops)
# (14) cumulus (continental, polluted)
#
# ---------------------------------------------------------------------------
Continental_Average			: name of new type (max. 30 characters)
3,6119.4          			: number of 1. component, number density
1,0.4                          		: number of 2. component, number density
2,7000					: number of 3. component, number density
9,0.0053                                : number of 4. component, number density
10,0.0053                       		: number of 5. component, number density               
# ****************** --------------------------------------------------------
# 2. height profile:
# ******************
#
# In the following 5 lines, you can change the default height profiles
# of default clouds or aerosol types. If the thickness of all layers is
# 0., the default values are used. If you don't want to calculate the
# optical depth, these values are not used, but the lines must not be
# empty!
#
# If you decided to define your own mixture, you should also define a
# height profile if you want to calculate optical depths.
#
# The following data must be given separated by commas, as indicated:
#
# Hmin: lower boundary in [km] above ground
# Hmax: upper boundary in [km] above ground
#    Z: scale height in [km]
#    N: number density in [particles/cm**3]
#
# ---------------------------------------------------------------------------
$ml                      :        mixing layer: Hmin, Hmax, Z   
$mt                     : mineral transported: Hmin, Hmax, N
$ft                      :    free troposphere: Hmin, Hmax, Z
$st                      :        stratosphere: Hmin, Hmax, Z
0., 0.,0.                      :               cloud: Hmin, Hmax, Z
# ***************-----------------------------------------------------------
# 3. wavelengths:
# ***************
#
# In the following list of all available wavelengths, the desired
# wavelengths must be selected by typing 1 in the first column. 0
# deselects the wavelength. You may select as many wavelengths
# as you like.
#
# There are two different wavelength lists, one for aerosols and water
# clouds, and one for cirrus clouds. Only wavelengths belonging to one
# of these lists may be selected.
#
# All wavelengths are given in micrometer.
#
# A. aerosols and water clouds ----------------------------------------------
61                              : number of wavelengths for A. (DON'T CHANGE!)
1                               : 0.25
1                               : 0.3
1                               : 0.35
0                               : 0.4
1                               : 0.45
1                               : 0.5
1                               : 0.55
0                               : 0.6
1                               : 0.65
0                               : 0.7
1                               : 0.75
1                               : 0.8
0                               : 0.9
1                               : 1.0
0                               : 1.25
0                               : 1.5
0                               : 1.75
1                               : 2.0
0                               : 2.5
0                               : 3.0
0                               : 3.2
0                               : 3.39
1                               : 3.5
0                               : 3.75
1                               : 4.0
0                               : 4.5
0                               : 5.0
0                               : 5.5
0                               : 6.0
0                               : 6.2
0                               : 6.5
0                               : 7.2
0                               : 7.9
0                               : 8.2
0                               : 8.5
0                               : 8.7
0                               : 9.0
0                               : 9.2
0                               : 9.5
0                               : 9.8
0                               : 10.0
0                               : 10.6
0                               : 11.0
0                               : 11.5
0                               : 12.5
0                               : 13.0
0                               : 14.0
0                               : 14.8
0                               : 15.0
0                               : 16.4
0                               : 17.2
0                               : 18.0
0                               : 18.5
0                               : 20.0
0                               : 21.3
0                               : 22.5
0                               : 25.0
0                               : 27.9
0                               : 30.0
0                               : 35.0
0                               : 40.0
# B. cirrus clouds ----------------------------------------------------------
32                              : number of wavelengths for B. (DON'T CHANGE!)
0                               : 0.280
0                               : 0.300
0                               : 0.320
0                               : 0.350
0                               : 0.355
0                               : 0.400
0                               : 0.423
0                               : 0.532
0                               : 0.535
0                               : 0.550
0                               : 0.635
0                               : 0.780
0                               : 0.830
0                               : 0.873
0                               : 1.015
0                               : 1.064
0                               : 1.100
0                               : 1.200
0                               : 1.400
0                               : 1.449
0                               : 1.504
0                               : 1.555
0                               : 1.615
0                               : 1.850
0                               : 1.905
0                               : 2.000
0                               : 2.190
0                               : 2.600
0                               : 3.077
0                               : 3.413
0                               : 3.700
0                               : 10.00
# ********************* -----------------------------------------------------
# 4. relative humidity:
# *********************
#
# In the following list of all available classes of relative humidity,
# the desired classes are selected by typing 1 in the first column. 0
# deselects the humidity class. You may select as many humidity classes
# as you like.
#
# ---------------------------------------------------------------------------
8                               : number of humidity classes  (DON'T CHANGE!)
$rh0                               :  0%
$rh50                               : 50%
$rh70                               : 70%
$rh80                               : 80%
$rh90                               : 90%
$rh95                               : 95%
$rh98                               : 98%
$rh99                               : 99%
# ********************** ----------------------------------------------------
# 5. optical parameters:
# **********************
#
# In the following list of all available optical parameters,
# the desired parameters are selected by typing 1 in the first column. 0
# deselects the parameter. You may select as many parameters as you like.
#
# ---------------------------------------------------------------------------
18                              : number of optical parameters (DON'T CHANGE!)
1                               : extinction coefficient    (extco)
1                               : scattering               (scaco)
1                               : absorption               (absco)
1                               : single-scattering albedo  (sisca)
1                               : asymmetry factor          (asymf)
1                               : optical depth             (odepe)
0                               :           -----------
0                               : spectral turbidity factor (turbf)
0                               : lidar ratio               (lirat)
0                               : phase function
0                               : mass ext. cross section   (extra)
0                               : mass abs. cross section   (absra)
0                               :           -----------
0                               : norm. ext. coefficient    (exnor)
0                               : spectrally weighted coefficients
1                               : Angstrom coefficients
0                               : visibility
0                               : refractive index     (refre,refim)
# END OF INPUT FILE ---------------------------------------------------------" > opac.inp
./opac >> $outputFolder"opacMessages.txt"
cp opac.out $outputFolder"opac$julianDay.out"
tail -n+$startingLineNumber opac.out | head -n$numberOfLines >> $sbdartRawDataFolder"OPACFINAL.csv"
done
rm -f opac.inp
# Close all opened files.
exec 2<&-
exec 3<&-
exec 4<&-
exec 5<&-
exec 6<&-
exec 7<&-
exec 8<&-

echo "

OPAC has finished running. Proceed to run 'processSbdartRawData.py' file.

"
