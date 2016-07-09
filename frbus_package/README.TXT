The FRBUS PACKAGE is a self-contained set of programs, equation code,
data, and documentation that provides detailed information about the
structure of FRB/US and is designed to enable anyone with access to
EViews versions 7 or 8 to execute simulations of the FRB/US model.

I. INFORMATION ABOUT THE STRUCTURE OF FRB/US

The key resource in the FRB/US package for information about the
structure of the model is the EQUATIONS HTML file in the documentation
subdirectory.  This document presents the exact form of each equation,
descriptive text for many equations and for each sectoral group of
equations, and cross-reference information that can be used to trace
the interrelationships among variables and equations.


II. GETTING STARTED WITH THE SIMULATION PROGRAMS

The documentation of the included programs assumes that the user
already has some familiarity with EViews, especially its programming
environment, the use of add-in commands, and the commands and
procedures associated with its "model" object.  The folowing four
steps are recommended before trying to run any of the programs.

(1) To facilitate the use of the FRB/US package with EViews, some may
find it best to place the package in a directory whose pathname does
not contain any blank spaces.

(2) Some of the routine tasks associated with loading FRB/US equations
and coefficients into EViews are carried out with EViews add-in
commands.  The add-in code is in the ADDINS subdirectory.  Before
running any of the included programs, install these add-ins by
following the instructions described below in the ADDINS section.
This needs to be done only once.

(3) All of the programs in the PROGRAMS subdirectory use relative
pathnames to declare the locations of equations, coefficients, data,
and subroutines.  These pathnames will work properly only if the
EViews default directory is set to the PROGRAMS subdirectory.  The
current value of the default directory is shown in the status line at
the bottom of the EViews window.  One way to (re)set it is with the
"cd" command.  For example, if the FRB/US package is located in
c:/abc/frbus_package, open EViews and enter the command:

        cd c:/abc/frbus_package/programs

Alternatively, the default directory can be (re)set by checking the
"Update Default Directory" box that appears when using the File
dropdown menu to open a program.

(4) For an introduction to the design of the FRB/US simulation
programs, read the SIMULATION BASICS PDF in the documentation
subdirectory.  The PDF is organized around the three examples of
simple simulation programs that are in the programs subdirectory.


III. ORGANIZATION OF THE FRB/US PACKAGE

The FRB/US package is organized into six subdirectories.

ADDINS

Six EViews add-in commands must be installed before running any of
the FRB/US simulation programs supplied in this package.  The
installation is accomplished by starting EViews and then executing the
program named regadd.prg that is located in the addins subdirectory.

DATA

The data subdirectory contains a database named LONGBASE, which is in
EViews format and contains actual observations on all FRB/US variables
from 1968q1 to 2015q2 and projected observations on all variables from
2015q3 to roughly 100 years in the future.  For the period from 2016
to 2018, the projections of real GDP growth, core and overall PCE
price inflation, the unemployment rate, and the federal funds rate are
based on the June 2016 "Economic Projections of Federal Reserve
Board Members and Federal Reserve Bank Presidents." Simple procedures
are used to interpolate the annual SEP projections to quarterly
frequency.  Other aspects of the projections included in the database are
constructed by mechanical procedures and should not be construed as
reflecting the views of the FOMC or any of its participants.  The
mechanical procedures cause the projection to converge to an
illustrative but nonetheless arbitrary steady-state growth path.

The historical values of the BBB bond rate, RBBB, are from
BofA Merrill Lynch Global Research, used with permission.

PROGRAMS

The pathnames in the supplied programs assume that the user's default
directory is the programs subdirectory.

Three programs (EXAMPLE1, EXAMPLE2, and EXAMPLE3) provide simple
examples of how to set up FRB/US simulations under each of its
expectations settings and its various policy options.  These programs
should be examined in conjunction with the document named SIMULATION
BASICS.

Additional programs illustrate how to execute more complex types of
simulations.  PINGS simulates IRFs for each of eight key shocks.
STOCHSIM executes a set of stochastic simulations.  OCPOLICY is an
optimal control experiment that runs a large number of simulations
to find the path for the federal funds rate that minimizes an
intertemporal losss function.  Each of these programs includes some
explanatory notes at its start.

The PLOT_RESIDS program plots the historical residuals of key
FRB/US equations.

DOCUMENTATION

The EQUATIONS HTML document presents the complete structure of every
equation in FRB/US, along with some descriptive text.  This document
contains links to several PDF files that describe some key aspects of
the model in greater detail (PAC_BASICS, VAR_EXPECTATIONS_BASICS,
PRICE_WAGE_SECTOR_083013, and LATENT_NOTE).  The PDFs can, of course,
be viewed independently of the EQUATIONS HTML file.

The SIMULATION BASICS PDF is an introduction to the design of FRB/US
simulation programs.

MCE SOLVE USERS GUIDE describes the set of EViews subroutines that are
used to run FRB/US simulations with model-consistent expectations.
These subroutines are part of the subs subdirectory.

MODS

This subdirectory contains equations and coefficients for two
versions of FRB/US that differ only in how expectations are assumed to
be formed.  For more information, see the SIMULATION_BASICS document.

SUBS

This subdirectory contains two sets (libraries) of EViews subroutines.
For more information, see the SIMULATION BASICS document.

