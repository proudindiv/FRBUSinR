README.TXT
===========================================================================
DESCRIPTION:
This zip file contains MATLAB code for processing the FRED-MD dataset.
Monthly and quarterly versions of the dataset can be found at
http://research.stlouisfed.org/econ/mccracken/fred-md/.  The code  loads
in the data, transforms it, removes outliers from the transformed data, and
saves it as a MAT-file to be used for factor construction.

===========================================================================
LIST OF FILES:

generate_freddata.m     

    This is the main MATLAB script. It performs all the tasks mentioned 
    above using auxiliary functions that are described below. It produces 
    the MAT-file freddata.mat.

---------------------------------------------------------------------------

prepare_missing.m           

    MATLAB function that transforms the raw data into stationary form. 
    Used in generate_freddata.m.

---------------------------------------------------------------------------

remove_ouliers.m            

    MATLAB function that removes outliers from the data. A data point x is 
    considered an outlier if |x-median|>10*interquartile_range. Used in 
    generate_freddata.m.

---------------------------------------------------------------------------