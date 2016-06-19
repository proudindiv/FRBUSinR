' Program for simulation under VAR expectations that illustrates how
' to set the monetary policy options that impose the zero lower bound
' on the funds rate and delay the liftoff of the funds rate from the
' ZLB until either the unemployment rate falls below a threshold or
' inflation rises above a threshold.
'
' See FRB/US Simulation Basics document for general information about
' this program.

' Additional notes:

' 1. The scenario involves a set of negative aggregate demand 
' shocks and a positive risk premium shock that start in 2003q3,
' when the baseline (historical) funds rate is about one percent.
' The shocks are equal to the equation errors actually observed
' in the four quarters starting in 2008q4.

' 2. To impose the ZLB set %zb = "yes" (rather than "no")

' 3. To impose the policy liftfoff threshold conditions set both
' %zb = "yes" and %threshold = "yes".  For illustrative purposes
' and reflecting the baseline conditions in 2003 and the years
' that immediately follow, the inflation threshold is set to 3.0
' and the unemployment threshold is set to 7.0, subject to the
' the adjustments described next.

' 4. Because the threshold conditions only make sense once the ZLB is
' binding, unemployment is above its threshold level (lurtrsh),
' and inflation is below its threshold (pitrsh), which is not the
' case in the initial simulation quarters, the program turns on the
' threshold code (using dmptrsh) in the 5th simulation quarter,
' at which point these conditions hold. In addition, for the threshold 
' code to work properly, the endogenous switch variable dmptr must be 
' zero in the quarter prior to the quarter in which the threshold code is 
' turned on.  This is accomplished by setting the baseline data on dmptr 
' to zero and by setting the unemployment and inflation thresholds
' (lurtrsh, pitrsh) to values in the first four simulation quarters that
' would not flip the dmptr switch to one. 

' 4. Choose one of the five available policy rules by setting
' %policy to one of rffintay, rfftay, rfftlr, rffalt, or rffgen.

' 5. If neither the ZLB or thresholds are imposed, the monetary policy
' equations have baseline-tracking adds and the simulation is
' a standard deviations-from-baseline exercise. 

' 6. If either the ZLB or thresholds are imposed, the add factors on 
' monetary policy equations are set to zero after the tracking adds
' are computed so that the ZLB and threshold conditions are based on the
' actual simulated outcomes for the funds rate and inflation and unemployment,
' not their deviations from baseline. 

' *************************************************************
' Initial filename and parameter settings
' *************************************************************

' Subroutines
  include ../subs/master_library

' Workfile    
  %wfstart = "1975q1"
  %wfend = "2012q4"
  %mainpage = "main"
  wfcreate(wf=aaa,page={%mainpage}) q {%wfstart} {%wfend}

' FRB/US model name and location
  %varmod = "stdver"
  %varpath = "../mods/"

' Input datbase
  %dbin  = "../data/longbase"

' Simulation range
  %simstart = "2003q3"
  %simend   = "2008q2"

' Policy 
  %zb = "yes"
  %threshold = "yes"
  %policy = "rfftay"


' ****************************************************************
' Retrieve data, model equations and coefficients, set
' policy options, and compute tracking residuals 
' ****************************************************************

' Load equations and coefficients
  ld_frbus_eqs(modelname=%varmod,modelpath=%varpath)  
  ld_frbus_cfs(modelname=%varmod,modelpath=%varpath)

' Load data
  dbopen %dbin as longbase
  fetch(d=longbase) *

' Set monetary policy rule
  smpl @all
  %policydmp = @replace(%policy,"rff","dmp")
  call set_mp(%policydmp)

' Set ZLB
  if %zb = "yes" then
    rffmin = .125
    else
    rffmin = -9999
    endif

' Set threshold variables 
  if %threshold = "yes" then
    if %zb = "no" then
      @uiprompt("When policy thresholds are imposed, the zero bound must also be imposed")
      stop
      endif
    smpl @all
    call dateshift(%simstart,%quarter4,3)
  ' thresholds (dmptrsh and dmptr) not active in first 4 qtrs
    smpl %simstart - 1 %quarter4
    dmptrsh = 0
    lurtrsh = -9999
    pitrsh = 9999
    dmptr = 0
  ' thresholds (dmptrsh and dmptr) active starting in qtr 5
    smpl %quarter4 + 1 %simend
    dmptrsh = 1
    lurtrsh = 7.0
    pitrsh = 3.0
    smpl @all
    else
    smpl @all
    dmptrsh = 0
    endif

  smpl @all
  drstar = 0

' Set fiscal policy
  smpl @all
  call set_fp("dfpsrp")

' Set _aerr variables to zero
  smpl @all
  {%varmod}.makegroup(a,n) endog @endog
  call groupnew("endog","_aerr")
  call group2zero("endog_aerr")

' Standard solution options
  {%varmod}.solveopt(o=b,g=12,z=1e-12)

' Assign baseline tracking add factors
  %suftrk = "_0"
  smpl %simstart 2012q4
  {%varmod}.addassign @all
  {%varmod}.addinit(v=n) @all
  {%varmod}.scenario(n,a={%suftrk}) "track"
  {%varmod}.solve
  scalar mm = @max(@abs(xgap{%suftrk}-xgap))
  if mm > .0001 then
    statusline dynamic tracking simulation failed for {%varmod}
    stop
    endif

' Set monetary policy add factors to zero when ZLB or threshold are
' imposed

  if %zb = "yes" then
    smpl @all
    {%policy}_a = 0
    rffrule_a = 0
    rffe_a = 0
    if %threshold = "yes" then
      dmptpi_a = 0
      dmptlur_a = 0
      dmptmax_a = 0
      dmptr_a = 0
      endif
    endif

' *************************************************************
' Simulation
' *************************************************************

  %sufsim = "_1"
  {%varmod}.scenario(n,a={%sufsim}) "sim"

' shock values are taken from equation residuals for 2008q4-2009q3
  eco_a.fill(o=%simstart) -.006, -.006, -.011, -.001
  ecd_a.fill(o=%simstart) -.091, -.018, -.021,  .029
  eh_a.fill(o=%simstart)  -.076, -.078, -.040,  .073
  epd_a.fill(o=%simstart) -.096, -.062,  .014,  .032
  eps_a.fill(o=%simstart) -.018, -.046, -.036, -.017
  rbbbp_a.fill(o=%simstart) 2.70, 0.38, -0.89, -1.35

  smpl %simstart %simend
  {%varmod}.solve


'***********************************************************
' Make a graph
'***********************************************************

  smpl %simstart %simend

  graph fig1a.line rff rff{%sufsim}
  fig1a.addtext(t,just(c),font("arial",12)) Federal Funds Rate
  fig1a.legend -display

  graph fig1b.line rg10 rg10{%sufsim}
  fig1b.addtext(t,just(c),font("arial",12)) 10-Year Treasury Yield
  fig1b.legend -display

  graph fig1c.line lur lur{%sufsim}
  fig1c.addtext(t,just(c),font("arial",12)) Unemployment Rate
  fig1c.legend -display

  graph fig1d.line pic4 pic4{%sufsim}
  fig1d.addtext(t,just(c),font("arial",12)) Inflation Rate (4-Quarter)
  fig1d.legend -display

  %title = "Macroeconomic Effects of Negative AD Shock\r(VAR Expectations"
  %title = %title + "; Policy = " + %policy + ")"
  if %zb = "yes" and %threshold = "no" then
    %title = %title + "\r(ZLB Imposed)"
    endif
  if %zb = "yes" and %threshold = "yes" then
    %title = %title + "\r(ZLB and Thresholds Imposed)"
    endif

  graph fig1.merge fig1a fig1b fig1c fig1d
  fig1.addtext(t,just(c),font("Arial",16)) {%title}
  fig1.addtext(b,just(c),font("Arial",16)) Blue:  Actual;  Red:  Simulated
  fig1.align(2,1,1.25)
  show fig1


