# TRS Telemac2D Subroutine

Nicolas Hanousek
Bin Guo

## SUMMARY


## HISTORY


## FUNDAMENTALS
The system is designed to be able to handle multiple tidal range schemes in one model. Therefore everything non-temporary is an array of the appropriate number of dimensions, in general the first index of the array is for the tidal range scheme number.
For things that can be shared such as hill charts different schemes can use the same ones (though it is easiest to just give the same data to each).

## FILES
### Fortran Files
#### buse.F
A modified version of the culvert control system originally developed by Smolders et al 

#### lecbus.F
Primarily the data loader for the culvert procedure of buse, thus the loading of the tidal range scheme data is moved to here to prevent the need to determine the state of loading etc. at time of operation resulting in a marginal performance gain, and a tidier concept.

#### T2D-TRS.F
New file, defines the T2D-TRS module - and as many of the modifications and functions as possible, long term there are additional features that would be good to move here from buse, but the this is not a priority at time of writing.

### Input Files
#### Culverts

#### TRS-*.txt

### Output Files


## VARIABLES
### Module Control
  - FIRST
    - Logical, save, (TRUE)
    - Answers the question, is this the first call of the module, if yes, we do all the initialization and data reading.
  - WRITE_TRS
    - Logical, save, (FALSE)
    - Print the TRS data to the file this timestep
  - G_PRNT_TRS
    - Logical, save, (TRUE)
    - Print the TRS data on the graphical printout period?
  - NUM_TRS
    - Int, save, (0)
    - How many tidal range schemes are there in the model?
  - TRS_TIME
    - Real, save, (0)
    - What is the model ‘time’ in hours?
### Temporal Controls
  - RAMPTIME
    - Real Array(NUM_TRS)
    - How long to ramp between modes for (hours)
  - PREV_SWITCH
    - Real Array(NUM_TRS)
    - The model time (hours) of the previous switch of modes.
  - MAX_GEN
    - Real Array(NUM_TRS)
    - Maximum time in hours of generation (safety option)
  - MAX_HOLD
    - Real Array(NUM_TRS)
    - Maximum time in hours of holding (safety option)
  - MAX_SLUICE
    - Real Array(NUM_TRS)
    - Maximum time in hours of sluicing (safety option)
  - MAX_PUMP
    - Real Array(NUM_TRS)
    - Maximum time in hours of pumping (safety option)
  - ORIG_DIAM_T
    - Real Array(NUM_TRS)
    - Original Diameter of the turbines given in the hill charts
### Operational Configuration
  - IS2WAY
    - Logical Array(NUM_TRS)
    - Is this scheme operating in two way generation
  - ISFLEXIBLE
    - Real Array(NUM_TRS)
    - Is the scheme operating in flexible or fixed mode?
    - POSSIBLY DEPRECATED
  - ISPUMPING
    - Logical Array(NUM_TRS)
    - Is this scheme using pumping as part of the operation
  - PUMPTOLEVEL
    - Logical Array(NUM_TRS)
    - Is the scheme pumping to a datum level (or a head difference)?
  - MODE
    - Integer Array(NUM_TRS)
    - What is the current operating mode of the tidal range scheme
  - MODE_PREV
    - Integer Array(NUM_TRS)
    - What is the previous mode of the tidal range scheme

### Water Levels
  - WL_IN
    - Real Array(NUM_TRS)
    - The water level within the scheme (aka upstream)
  - WL_OUT
    - Real Array(NUM_TRS)
    - The water level outside the scheme (aka downstream)
  - HEADDIFF
    - Real Array(NUM_TRS)
    - The head difference across the scheme IN – OUT
  - Q_TURB
    - Real Array(NUM_TRS)
    - The total flow out of the scheme in m³/s through the turbines
  - Q_SLUICE
    - Real Array(NUM_TRS)
    - The total flow out of the scheme in m³/s through the sluices
  - POWER
    - Real Array(NUM_TRS)
    - The total from or used by the scheme

### Hill Chart
  - N_HILLS_[G/P]
    - Integer Arrays(NUM_TRS)
    - Number of hill charts for pumps and turbines for each scheme
  - LEN_HILLS_[G/P]
    - Integer Arrays(NUM_TRS)
    - Number of points in the hill charts for each scheme
  - HEAD_[G/P]
    - Real Array(NUM_TRS,LEN_HILLS_[G/P])
    - The head differences for the hill charts for gen and pump for the schemes
  - POWER_[G/P]
    - Real Array(NUM_TRS,LEN_HILLS_[G/P], N_HILLS)
    - The power ratings for the hill charts for gen and pump for the schemes
  - FLOW_[G/P]
    - Real Array(NUM_TRS,LEN_HILLS_[G/P], N_HILLS)
    - The flow ratings for the hill charts for gen and pump for the schemes
  - PREV_I_[POWER/FLOW]_[G/P]
    - Integer Arrays(NUM_TRS)
    - Track the last index of the flow/head/power calculations for speedup
    - NOT YET USED

### Flexibility Controls
  - N_FLEX
    - Integer Array(NUM_TRS)
  - FLX_TIMES
    - Real Array(NUM_TRS,N_FLEX)
  - H_STARTS
    - Real Array()
  - H_ENDS
    - Real Array()
  - PUMP_TARGS
    - Real Array()
  - TURB_CHARTS
    - Real Array()
  - PUMP_CHARTS
    - Real Array()
  - PREV_I_FLX_TIMES
    - Real Array()
  - H_START
    - Real Array()
  - H_END
    - Real Array()
  - TURB_CHART
    - Real Array()
  - PUMP_CHART	
    - Real Array()
  - PUMP_TARG
    - Real Array()

### Dimensionless
  - RAMP

### Temporary
  - TMP_REAL
  - SCHEME
  - TMP_INT
  - I,J,K,L,M,STATUS
  - TMP_BOOL

### FUNCTIONS
  - TRS_INTERP
    - Inputs(X_NEW,X1,X2,Y1,Y2)
    - Returns real
  - TRS_CALC
    - Inputs(HEAD,HEADS,VALUES)
    - Returns real
  - TRS_RAMP
    - Inputs(RAMPTIME,PHASETIME)
    - Returns real
  - TRS_ORIFICE
    - Inputs(CD,AREA,HDIFF)
    - Returns real
  - TO_UPPER
    - Inputs(strIn)
    - Returns string

### SUBROUTINES
  - TRS_STAT
    - Inputs(STAT,MSG)
    - No return
    - Prints message if Status is not zero.
  - TRS_FLEX_VALEUS
    - Inputs(CT)
    - No return
    - Sets all the flex controls for the schemes to the ones required at the time.
  - TRS_NEW_MODE
    - Inputs(CT)
    - Updates the operating modes of all the tidal range schemes if required, and updates the ramps accordingly.
  - TRS_PARSE
    - Inputs(LN,KWRD,VRBL)
    - Returns (KWRD,VRBL)
    - Parses the line into a key variable pair for value initialization
  - TRS_KV_WARN
    - Inputs(K,V)
    - No return
    - Prints a warning if an unexpected key variable pair are read by the parser.
  - TRS_LOAD_HILLCHART
    - Inputs(HEADS,ARRAY,NF_TRS)
    - Returns(HEADS,ARRAY)
    - Pushes the values from the file to the hill charts.
    - MAY NEED TWEAKING DUE TO THE MULTI-DIMENSIONAL ARRAYS
  - TRS_PRINT_HILL:CHART
    - Inputs(HEADS,ARRAY,KWRD,VRBL)
    - No returns
    - Prints the hill chart(s) to the user via the console
  - TRS_READ_DATAFILES
    - Reads the datafile(s) describing the scheme(s) and allocates space where needed.
  - TRS_WRITE_RESULTS
    - Prints results to a CSV file for each scheme
  - TRS_WRITE_STATUS
    - Writes TRS Status to the console
  - TRS_FLOWS
    - Calculates the flow rates through the TRS culverts in their current state
  - TRS_POWER
    - Calculates the power produced or used by the TRS in it’s current state
  - TRS_W_LEVELS
    - Calculates the control water levels for each TRS
  - TRS_COLLATE_Q
    - Calculates the total flows across each TRS

## USAGE

## FLOW

## EXAMPLES

