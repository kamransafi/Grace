So that everyone is on the same page, I thought that maybe we could state some main points here that we would like everyone to take into account when working on scripts.

### MOVEMENT DATA

-   data will be subsampled to 1 location per hour.
-   data will be filtered excluding days that do not contain "enough" locations.

### METRIC FUNCTIONS

Martina and me (Anne) decided that the best way to proceed is:

-   our unit is to calculate metric X per day

-   create **one function for each metric** to be calculated. This way fixing and rerunning is easiest. This function will be lapplyed on a list of moveobjects per individual. See "ExampleData" folder for example files.

-   the result table of all functions should have a common column called **"commonID"** that contains the following character string **"MovebankID_deploymentID_Date".** This way all results can be combined as needed and we can work with smaller units at a time.    
"MovebankID": `moveObj@idData$study.id`  
"deploymentID": `moveObj@idData$deployment.id`

-   remember to list all libraries needed to run the function. And maybe a small comment of what the function does and returns

-   so we are all doing the same thing, make sure that you are actually selecting the locations for a day corresponding to a date. E.g.: in `lubridate::round_date()` the changing point is not 12AM, but 12PM. So one "day" goes from 12PM to 11:59AM of the next date. Using `lubridate::floor_date()` the actual date is a "day".

### GRACE VARIABLES

### ...
