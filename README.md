### README

So that everyone is on the same page, we thought that maybe we could state some main points here that we would like everyone to take into account when working on scripts.

### MOVEMENT DATA

-   data will be subsampled to 1 location per hour.
-   migration will be excluded from this first analysis (probably using cumulative daily distance/euclidean distance ?)
-   data will be filtered excluding days that do not contain "enough" locations.

### SCRIPTS - PROCESSING STEPS MOVEMENT DATA

-   `M1_dataDownload.R`: In this script we browse Movebank from the TeamWikelski account and excluded studies that are "test" or that do not contain GPS data. On July 29th this list contains 706 studies. The script goes through each individual in each study, downloads the GPS data, removes duplicated locations (duplicated across time, lat and long), rbinds all individuals and saves locally one table per study in .rds format. Some stuies could not be downloaded through the API (due e.g. to missing individual ID). These studies were thus downloaded manually from the Movebank interface and then formatted as the studies downloaded via API for including them in the following steps. (Martina)

-   `M2_duplicatedIndividualsAcrossStudies.R`: This script sources the function `referenceTableStudies.R`. We create and save locally a table with one entry per individualID-tagID combination that summarises information such as individual, tag and deployment IDs, start and end of the tracking (for each tag on that individual), tracking duration, number of locations etc. Note that multiple deployments of the same individual are therefore treated separately, which is important for finding duplicated tags across studies or within a study (e.g. same tag removed from one individual and put on a different individual few hours later). The rest of the script looks for duplicated individuals. These are the types of duplicates accounted for:

    -   Across studies: Same individual published in two/several studies. IndividualID might be same or different but tagID is identical across studies and the time between start and end of the deployment overlaps at least partially. We keep the one with more GPS points.
    -   Within study:
        1.  Multiple tags are deployed at the same time on the same individual and data are therefore duplicated at least in part. These can happen when the purpose of the study is to compare two tag types, but it could also happen when a tag wants to be replaced after a few years, but instead of removing the old one for a portion of time both old and new tag record data simultaneously. This code just chooses the tag with more GPS points, but future codes could identify this case and delete only the overlapping time rather than the whole tracking time of one of the two tags. By checking for overlapping time we detect this case and exclude the duplicate with shorter tracking duration. This case has to be distinguished from the following two cases..
        2.  Same tag deployed on the same individual multiple times, therefore deployment time does not overlap and it is not considered as a duplicate.
        3.  Same tag deployed on different individuals. By checking the overlapping time between start and end of the deployment on the several individual we can distinguish this from a real duplicate.

Specifically, the code looks for duplicated combinations of either individualID-tagID-species or tagID-species or studyID-individualID-species (multiple tags simultaneously on the same individual within the same study). Once these duplicated combinations are found it checks if the start time and end time of the GPS tracking of the duplicates overlap. If it overlaps, the duplicate containing less locations gets "excluded" (the column "excluded" corresponding to that duplicate gets the value "yes"). Checking for overlapping tracking time allows to identify duplicates also within the same studyID as the same tag cannot be applied on two different individuals at the exact same time. The output of this script is therefore a copy of the reference table created at the beginning of the script, with an added column called "excluded", which takes values "yes/no" depending on whether the individualID-tagID combination was classified as duplicate. (Anne and Martina)

-   `M3_createMoveObject_dataSubsampling.R`: this script works on the non-duplicates, meaning it takes the individualID-tagID combinations that were not excluded from the previous steps. From now on the unit of the analyses is the individualID, meaning that the studyID and the tagID are not important anymore. Therefore multiple deployments of a same individual will be included in one single move object. Gaps between consecutive deployments will be dealt with in the following analyses as all metrics are calculated per day, and will be filtered a posteriori based on the number of locations collected on that day. Before saving each individual as a move object we subsample the locations to 1 hour using the amt package. The files produced in this script are stored in the folder "MoveObjects_1hourSubsample" and named as "MBid_indiv.rds". (Martina)

-   `M4_removeOutliers_plotGlobalDistribution.R`: this script remove outliers from the individual move objects saved in the previous step. Outliers are identifier and filtered out based on the distribution of ground speeds within each study. We identify the 99.95% quantile in speed. If this quantile corresponds to a speed value \> 50 m/s we identify the higher quantile that is \< 50 m/s and remove recursively (with a while loop) all observations above this threshold. The reason for choosing 50 m/s as threshold is that in horizontal flight, swifts and brazilian bats (Teague) are considered the fastest flyers with 110 and 160 km/h (44.4 m/s). <https://en.wikipedia.org/wiki/Fastest_animals> After filtering out the outliers, the spatial distribution of the trajectories of each study are plot on a global maps in groups of 100 studies. The files produced in this script are stored in the folder "MoveObjects_1hour_noOutliers" and named as "MBid_indiv.rds". (Martina)

-   `M5_IndivRefTable_addLocomotoryMode.R`: this script applies function to create a reference table per individual including nb of days tracked, total and daily nb of gps points, mean timelag per day, total tracking time, etc (Using `referenceTable_Individuals()`). Also the species get associated to a locomotory mode according to the species lists downloaded from IUCN.  (Anne)

-   `M6_applyDailyMovementFunctions.R`: this script applies the functions written by Anne to calculate cumulative daily distance travelled, daily maximum net displacement (using  `dailyDispl()`), daily motion variance, daily dBB and daily UDs (and some derived metrics as area, centroids) (using `dailydBBud()`). Also identifying migration/non-migration days is done in this script. (Martina & Anne)

### METRIC FUNCTIONS

Martina and me (Anne) decided that the best way to proceed is:

-   our unit is the day (to calculate metric X per day)

-   create **one function for each metric** to be calculated. This way fixing and rerunning is easiest. This function will be lapplyed on a list of moveobjects per individual. See "ExampleData" folder for example files.

-   the result table of all functions should have a common column called **"commonID"** that contains the following character string **"MovebankID_individual.local.identifier_Date".** This way all results can be combined as needed and we can work with smaller units at a time.  
    "MovebankID": `moveObj@idData$study.id`  
    "individual.local.identifier": `namesIndiv(moveObj)`

-   remember to list all libraries needed to run the function. And maybe a small comment of what the function does and returns

-   so we are all doing the same thing, make sure that you are actually selecting the locations for a day corresponding to a date. E.g.: in `lubridate::round_date()` the changing point is not 12AM, but 12PM. So one "day" goes from 12PM to 11:59AM of the next date. Using `lubridate::floor_date()` the actual date is a "day".

#### Overview of functions for summary/reference tables

-   `referenceTableStudies()`: get a reference table with 1 line per individual. I.e. loop around all individuals and rbind result. Large table can be used e.g. to find duplicated individuals across studies, add column with info why individuals got removed from analysis. Individuals with multiple tags/deployments are kept in one lines and the different tag IDs/deployment IDs separated by a "\|". Gaps between deployments will be dealt with and filtered out at a later stage as all metrics will be calculated per day and associated to the number of locations. (Anne)

-   `referenceTable_Individuals()`: gives one table per individual. Including MBid, individual, tag, species, date, tracking_duration_in_days, GPSpts_total, locsperday, median_timelag_mins_day, locomotion mode. Saves table as "RefTableIndiv_MBid_indiv.name.rds". (Anne) *(removed "tag id" for now as it needs time to be adapted for individuals with multiple tags)*

#### Overview of functions for movement metrics

-   `dailyDispl()`: includes the calculation of: *cumulativeDist_km*: sum of all step lenghts (in Km) per day; *maxNetDispl_km*: maximum distance (in Km) between any 2 locations per day & *straightnessIndex*: maxNetDispl_km/cumulativeDist_km. between 0-1, 1 is moving in straight line, idea is to use it as an indication of migratory day, cut-off value to be determined. Saves table per individual called "dailyDisplacement\_\_MBid_indiv.name.rds". (Anne)

-   `dailydBBud()`: calculates mean(?) daily motion variance. Saved as "dailyMotionVar\_.....rds". Removes days that have less locations than "minLocationsDay", by setting \@interest==F. Segments longer than 5h are also excluded by setting \@interest==F. Calculates dBB per day looping through the splitted dbbvarburst object. Calculates ud size, geometric centroid of UD and weighted lat/long coords of UD, saved as "dailyUDcalc\_...rds". Extracts coordinates and values from dbb, as SPDF, saved as "dailyDBBcoordinatesSPDF\_...rds". (Anne)

### GRACE VARIABLES

The data we get from GRACE are a measure of "monthly change in total water storage (tws)" from a baseline value (negative change = decrease in water, positive change = increase in water relative to the baseline; the baseline from which change is calculated being the mean over the period 2002-04 - 2020-03). ALL the water in the surface, on the surface, everywhere – total water storage – cannot be derived by GRACE as an absolute measure since there is no 0-measurement for Earth without water to which one could add change. Just the change in mass/gravity across time can be measured from GRACE. For estimating how much overall water is stored you need a model product. Therefore on the GRACE change dataset, a super "wet" area with water loss would look the same as a dry area becoming even dryer by the same amount. For a model product of absolute water content we could use GLDAS or any other water storage model.

### SCRIPTS - PROCESSING STEPS GRACE DATA

-   `G1_open_grace_data_asDF.R`: in the end for the analytical part we needed the layers as rasters so we used a different script (see below). This was used for building Fig. 1. (Elham)

-   `G2_calculate_metrics.R`: we calculated, per month, the average change in water storage, and the variance of this change across the period 2002-2021. Used for building Fig. 1. (Elham)

-   `G1_open_grace_data_asBRICK_formatCollectionPeriods.R`: in this script the .nc files are opened as rasters and all grace layers are formatted in one big brick file, where each layer represents one grace collection period (file named "GRACEraw_atlantic_allYearsAllMonths_brick.tif"). The layers in the brick are named after the time slot in the corresponding .nc file. Grace "times" represent the middle of point of the collection period. In this script we also format the table provided by Eva Boergens, where each grace time is associated to two columns indicating the exact start and end of the collection period used to build the layer (file named "grace_tws_collectionPeriods.rds"). This table is used in the script MG1 to associate the daily movement data to the correct grace layer. (Martina)

### SCRIPTS - PROCESSING STEPS MOVEMENT METRICS & GRACE

-   `MG1_weightedGRACE_per_dailyDBB_byGRACEcollectionPeriod.R`: this script calculates the weighted daily GRACE experienced per individual. To do this we:

    1.  assign each day of movement to the correct grace layer using the table of collection periods sent by Eva. Movement metrics in days that are outside the grace collection period (before April 2002 and after May 2022) are excluded. Movement metrics in days that correspond to temporal gaps during the grace collection period are also excluded;
    2.  annotate the DBB file (spatial points df) of each individual (files named "dailyDBBcoordinatesSPDF_MBid_indiv.rds") with the correct grace layer (extracted grace value corresponding to each DBB pixel in each day);
    3.  multiply the extracted grace value by the column "layer" (dbb_val) and save each file per individual (files "dailyDBBgrace_MBid_indiv.rds"), we call this variable "graceExperienced".
    4.  finally sum the graceExperienced per day, to obtained a daily value per individual. All days of all individuals are merged in one table named "nonFlying_allDailyGraceExperienced.rds". (Martina)

-   `MG2_averageByGracePeriods_buildModelDF.R`: In the doings... this script will merge the table containing the daily movement metrics (daily UD size, daily cum distance and daily directness) with the table containing the daily GRACE experienced. All these variables will be averaged by grace collection periods (usually about a month) to build the final model dataframe. (Martina)

-   `MG3_diseaseOutbreaks.R`: this script associate the occurrence of the disease outbreaks sent by Louis to the closest grace layer. Outbreaks happened outside the grace collection period (before April 2002 and after May 2022) are excluded. Outbreaks in dates that correspond to temporal gaps during the grace collection period are not excluded. Instead they are associate to the closest grace layer and the time difference from the closest layer is reported in a column named "timeDiff_daysFromCollection", for potential future filtering. (Martina)

### MODELLING IDEAS

##### **Models for Martin's presentation**

**M1**: monthly average of daily.movement \~

monthly.Grace.experience(mGe) + *(weighted GRACE by avg dailyUD or monthlyUD)*  
now availability should actually be "change in tws" and daily GRACE will be linearly interpolated for all days in all years.

s(lat,long)+ *(weighted coordinates from dayilyUD)*

s(time)+ *(time since first measurement of Grace)*

species (random fac)+

locomotory.mode

-   *monthly average of daily.movement*:

    -   average of daily.cumulative.distance

    -   average of daily UDsize

    -   monthly directness of movement per pixel (motion type within pixel) (calculated by averaging the DBB variance per segment by month and pixel... or use 1st passage time as proxy?)

-   *expectation*: increase in grace variability/unpredictability -\> increase in daily movement, decrease in dbb variance (direct movement)

-   *prediction Maps* - see pics in Minerva messenger

**M2**: monthly.Grace(mG) \~

s(lat,long)

**final map = M1 - M2**

------------------------------------------------------------------------

**M1.1**: monthly/annual.movement \~

diff.calculations.of.dGe+

species+

loc.mode+

s(time) *(time since first measurement of Grace)*

-   *monthly/annual.movement*:

    -   cumulative range shift (using geometric centroid of each dailyUD)

    -   means of "*daily.movement*" of M1

    -   dailyUD size mean, var, etc?

-   *diff.calculations.of.dGe*:

    -   all sorts of calculations per month or year that we can come up with, e.g:

        -   dryest month

        -   min-max

        -   mean

        -   var

        -   etc

### FIGURES

-   `Fig1_mapSept22.R`: This script builds a first descriptive figure. The monthly average change and the monthly variance of this change were averaged across all months. The pixels in the map were coloured according to these two variables (the legend being a 2D matrix of colours):
-   yellow areas represent areas that have negative change in water availability and low variance in this change (decreasing total water storage of a predictable extent).
-   turquoise areas have a negative change in water but a high variation in this change (less predictable decrease in tws, more fluctuation).
-   dark green areas have a positive change and a low variation in this change (predictable increase in tws).
-   magenta areas have a positive change in water and a high variation in this change (less predictable increase in tws, more fluctuation).

Areas with predictable increase in water storage are very rare (dark green), most areas (magenta and turquoise) experience large fluctuations in change of water storage (increase or decrease by a changing amount), and many others (yellow) are predictably drying up...

Some note about the movement data we used: the trajectories that you see in the map are from 15'173 individuals, 349 species and 623 movebank studies. The storks seem to be a very promising dataset to study movement response to the predictability in water storage, as their data cover very large areas and in Africa cover some areas of high predictability (in turquoise) and many areas of high fluctuations both in positive and negative change (yellow and magenta areas).

##### **ADDITIONAL FOLLOW UP PROJECTS:**

-   changes in migration routes \~ changes in water (?)

-   re-visitation time of specific areas/pixels

-   fluctuation in population sizes

-   outbreaks of vector borne diseases

-   wintering and breeding range variation over time (shift of the centroid of the range) \~ changes in water predictability/fluctuations

-   water availability in winter range when they leave - similar to water availability in breeding range when they arrive?

-   Martin mentioned something about looking at GRACE time series specifically in Krueger (?)
