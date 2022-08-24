So that everyone is on the same page, I thought that maybe we could state some main points here that we would like everyone to take into account when working on scripts.

### MOVEMENT DATA

-   data will be subsampled to 1 location per hour.
-   data will be filtered excluding days that do not contain "enough" locations.

### SCRIPTS - PROCESSING STEPS MOVEMENT DATA

-   `M1_dataDownload.R`: In this script we browse Movebank from the TeamWikelski account and excluded studies that are "test" or that do not contain GPS data. On July 29th this list contains 706 studies. The script goes through each individual in each study, downloads the GPS data, removes duplicated locations (duplicated across time, lat and long), rbinds all individuals and saves locally one table per study in .rds format. Some stuies could not be downloaded through the API (due e.g. to missing individual ID). These studies were thus downloaded manually from the Movebank interface and then formatted as the studies downloaded via API for including them in the following steps.

-   `M2_duplicatedIndividualsAcrossStudies.R`: This script sources the function `referenceTableStudies.R`. We create and save locally a table with one entry per individualID-tagID combination that summarises information such as individual, tag and deployment IDs, start and end of the tracking (for each tag on that individual), tracking duration, number of locations etc. Note that multiple deployments of the same individual are therefore treated separately, which is important for finding duplicated tags across studies or within a study (e.g. same tag removed from one individual and put on a different individual few hours later). The rest of the script looks for duplicated individuals. These are the types of duplicates accounted for:

    -   Across studies: Same individual published in two/several studies. IndividualID might be same or different but tagID is identical across studies and the time between start and end of the deployment overlaps at least partially. We keep the one with more GPS points.
    -   Within study:
        1.  Multiple tags are deployed at the same time on the same individual and data are therefore duplicated at least in part. These can happen when the purpose of the study is to compare two tag types, but it could also happen when a tag wants to be replaced after a few years, but instead of removing the old one for a portion of time both old and new tag record data simultaneously. This code just chooses the tag with more GPS points, but future codes could identify this case and delete only the overlapping time rather than the whole tracking time of one of the two tags. By checking for overlapping time we detect this case and exclude the duplicate with shorter tracking duration. This case has to be distinguished from the following two cases..
        2.  Same tag deployed on the same individual multiple times, therefore deployment time does not overlap and it is not considered as a duplicate.
        3.  Same tag deployed on different individuals. By checking the overlapping time between start and end of the deployment on the several individual we can distinguish this from a real duplicate.

Specifically, the code looks for duplicated combinations of either individualID-tagID-species or tagID-species or studyID-individualID-species (multiple tags simultaneously on the same individual within the same study). Once these duplicated combinations are found it checks if the start time and end time of the GPS tracking of the duplicates overlap. If it overlaps, the duplicate containing less locations gets "excluded" (the column "excluded" corresponding to that duplicate gets the value "yes"). Checking for overlapping tracking time allows to identify duplicates also within the same studyID as the same tag cannot be applied on two different individuals at the exact same time. The output of this script is therefore a copy of the reference table created at the beginning of the script, with an added column called "excluded", which takes values "yes/no" depending on whether the individualID-tagID combination was classified as duplicate.

-   `M3_createMoveObject_dataSubsampling.R`: This script works on the non-duplicates, meaning it takes the individualID-tagID combinations that were not excluded from the previous steps. From now on the unit of the analyses is the individualID, meaning that the studyID and the tagID are not important anymore. Therefore multiple deployments of a same individual will be included in one single move object. Gaps between consecutive deployments will be dealt with in the following analyses as all metrics are calculated per day, and will be filtered a posteriori based on the number of locations collected on that day. Before saving each individual as a move object we subsample the locations to 1 hour using the amt package.

-   `M4_removeOutliers_plotGlobalDistribution.R`: This script remove outliers from the individual move objects saved in the previous step. Outliers are identifier and filtered out based on the distribution of ground speeds within each study. We identify the 99.95% quantile in speed. If this quantile corresponds to a speed value \> 50 m/s we identify the higher quantile that is \< 50 m/s and remove recursively (with a while loop) all observations above this threshold. The reason for choosing 50 m/s as threshold is that in horizontal flight, swifts and brazilian bats (Teague) are considered the fastest flyers with 110 and 160 km/h (44.4 m/s). <https://en.wikipedia.org/wiki/Fastest_animals> After filtering out the outliers, the spatial distribution of the trajectories of each study are plot on a global maps in groups of 100 studies.

-   `M5_applyDailyMovementFunctions.R`: This script applies the functions written by Anne to calculate cumulative daily distance travelled, daily maximum net displacement, daily motion variance, daily UDs (in the doing).

### METRIC FUNCTIONS

Martina and me (Anne) decided that the best way to proceed is:

-   our unit is to calculate metric X per day

-   create **one function for each metric** to be calculated. This way fixing and rerunning is easiest. This function will be lapplyed on a list of moveobjects per individual. See "ExampleData" folder for example files.

-   the result table of all functions should have a common column called **"commonID"** that contains the following character string **"MovebankID_individual.local.identifier_Date".** This way all results can be combined as needed and we can work with smaller units at a time.  
    "MovebankID": `moveObj@idData$study.id`  
    "individual.local.identifier": `namesIndiv(moveObj)`

-   remember to list all libraries needed to run the function. And maybe a small comment of what the function does and returns

-   so we are all doing the same thing, make sure that you are actually selecting the locations for a day corresponding to a date. E.g.: in `lubridate::round_date()` the changing point is not 12AM, but 12PM. So one "day" goes from 12PM to 11:59AM of the next date. Using `lubridate::floor_date()` the actual date is a "day".

#### Overview of functions for summary/reference tables

-   `referenceTableStudies()`: get a reference table with 1 line per individual. I.e. loop around all individulas and rbind result. Large table can be used e.g. to find duplicated individuals across studies, add colum with info why individuals got removed from analysis. Individuals with multiple tags/deployments are kept in one lines and the different tag IDs/deployment IDs separated by a "\|". Gaps between deployments will be dealt with and filtered out at a later stage as all metrics will be calculated per day and associated to the number of locations. (Anne)

-   `referenceTable_Individuals()`: gives one table per individual. This table can be used to filter out days with "to few" locations, etc. Saves table as "RefTableIndiv_MBid_indiv.loc.ident.RData", object contained is called `RefTableIndiv`. (Anne)

-   `duplicatedIndividualsAcrossStudies.R`: not a function, but a script that aims to find individuals that are duplicated in two or more studies, and then keeping the one with most gps fixes. (Anne)

#### Overview of functions for movement metrics

-   `cumulativeDist()`: calculates sum of all step lenghts per day, saves table per individual called "cumDistDay_MBid_indiv.name.RData", object contained is called `cumDistDay`. (Anne)

-   `maxNetDisp()`: calculates the maximum distance between any 2 locations per day, saves table per individual called "maxNetDisplDay_MBid_indiv.name.RData", object contained is called `maxNetDisplDay`. (Anne)

-   cumDistDay/maxNetDisplDay \~ 1 =\> migratory day, cut-off value to be determined, not a function, just so idea does not get lost. (Anne & Martina)

-   daily motion variance - in the doings (Anne)

-   daily UD (and size) - in the doings (Anne) include: UD size, geometric centroid of UD, weighted grace availability in UD, weighted lat/log of UD

### GRACE VARIABLES

### SCRIPTS - PROCESSING STEPS GRACE DATA

### MODELLING IDEAS

**M1**: daily.movement \~

daily.Grace.experience(dGe) + *(weighted G. availability in dailyUD)*

s(lat,long)+ *(weighted coordinates from dayilyUD)*

s(time)+ *(time since first measurement of Grace)*

species (random fac)+

locomotory.mode

------------------------------------------------------------------------

*daily.movement*:

-   daily.cumulative.distance

-   daily dBMvariance

-   motion type (brownian, levy, etc), use 1st passage time as proxy?

*prediction*: increase in grace variability -\> increase in daily movement

*prediction Maps* - see pics in Minerva messenger

------------------------------------------------------------------------

**M2**: monthly/annual.movement \~

diff.calculations.of.dGe+

nb.species+

loc.mode+

s(time) *(time since first measurement of Grace)*

------------------------------------------------------------------------

*monthly/annual.movement*:

-   cumulative range shift (using geometric centroid of each dailyUD)

-   means of "*daily.movement*" of M1

-   dailyUD size mean, var, etc?

*diff.calculations.of.dGe*:

all sorts of calculations per month or year that we can come up with, e.g:

-   dryest month

-   min-max

-   mean

-   var

-   etc

------------------------------------------------------------------------

**ADDITIONAL FOLLOW UP PROJECTS:**

-   changes in migration routes \~ changes in water availability

-   wintering and breeding range variation over time \~ changes in water availability

-   water availability in winter range when they leave - similar to water availability in breeding range when they arrive?
