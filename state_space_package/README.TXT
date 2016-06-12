The STATE SPACE PACKAGE is a self contained set of programs, data, and documentation
that allows a user to estimate and run the model used to make the historical 
supply-side trends in the FRB/US model.

BACKGROUND NOTE

The PDF file latent_note describes the FRB/US state-space model.

DATA

The model uses the database included in the "zip" file called state_space_data.  
This Eviews database includes all of the series used to construct the signals in the model.

PROGRAMS

There are two programs and three subroutines.

frbus_supply_estimation.prg - This program loads the supplied database, makes some
data transformations, estimates the model and saves the estimated model in a workfile.  
The estimation takes some time, we have found it easier to have a saved model that can be
loaded for filtering.

frbus_supply_filter.prg - This program loads the supplied database, makes some
data transformations, loads the previously saved model, and runs the kalman
filter to create estimates of the states.

data_transformations.prg - This subroutine transforms the raw data (taken from the FRB/US 
database) into the signal series in the model. Also included is the creation of the 
estimation priors.  

estimation_code.prg - This subroutine includes the model specification and the parameter 
starting values for the estimation scheme. The model is saved for later use with the 
estimated coefficients substituted into the model.

initial_values.prg - The initial values of the coefficients in the state space model.

INSTRUCTIONS

Open EViews

To estimate the model, in the white command space type:

    cd ".../path_to_unzipped_directory/"

    run frbus_supply_estimation

Then wait.  Then estimation can take ~15 min or more depending on your computer.

To create the estimates of the states, type:

    run frbus_supply_filter
