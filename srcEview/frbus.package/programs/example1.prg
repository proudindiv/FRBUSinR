' Program for simple simulation under VAR expectations
'
' See FRB/US Simulation Basics document for information about
' this program

' *************************************************************
' Initial filename and parameter settings
' *************************************************************

' Subroutines
  include ../subs/master_library

' Workfile    
  %wfstart = "1975q1"
  %wfend = "2030q4"
  %mainpage = "main"
  wfcreate(wf=aaa,page={%mainpage}) q {%wfstart} {%wfend}

' FRB/US model name and location
  %varmod = "stdver"
  %varpath = "../mods/"

' Input datbase
  %dbin  = "../data/longbase"

' Simulation range
  %simstart = "2020q1"
  %simend   = "2025q4"


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
  call set_mp("dmpintay")

' Turn off zero bound and policy thresholds; hold policymaker's
' perceived equilibrium real interest rate constant
  smpl @all
  dmptrsh = 0
  rffmin = -9999
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
  smpl %simstart %simend 
  {%varmod}.addassign @all
  {%varmod}.addinit(v=n) @all
  {%varmod}.scenario(n,a={%suftrk}) "track"
  {%varmod}.solve
  scalar mm = @max(@abs(xgap{%suftrk}-xgap))
  if mm > .0001 then
    statusline dynamic tracking simulation failed for {%varmod}
    stop
    endif

' *************************************************************
' Simulate a shock to monetary policy rule
' *************************************************************

  %sufsim = "_1"
  {%varmod}.scenario(n,a={%sufsim}) "sim"

  smpl %simstart %simstart
  rffintay_aerr = rffintay_aerr + 1

  smpl %simstart %simend
  {%varmod}.solve


'***********************************************************
' Make a graph
'***********************************************************

  smpl %simstart %simend
  series zero = 0
  series d_rff = rff{%sufsim} - rff
  series d_rg10 = rg10{%sufsim} - rg10
  series d_lur = lur{%sufsim} - lur
  series d_pic4 = pic4{%sufsim} - pic4

  graph fig1a.line zero d_rff
  fig1a.addtext(t,just(c),font("arial",12)) Federal Funds Rate
  fig1a.legend -display

  graph fig1b.line zero d_rg10
  fig1b.addtext(t,just(c),font("arial",12)) 10-Year Treasury Yield
  fig1b.legend -display

  graph fig1c.line zero d_lur
  fig1c.addtext(t,just(c),font("arial",12)) Unemployment Rate
  fig1c.legend -display

  graph fig1d.line zero d_pic4
  fig1d.addtext(t,just(c),font("arial",12)) Inflation Rate (4-Quarter)
  fig1d.legend -display

  graph fig1.merge fig1a fig1b fig1c fig1d
  fig1.addtext(t,just(c),font("Arial",16)) Macroeconomic Effects of Funds Rate Perturbation\r(VAR Expectations)
  fig1.align(2,1,1.25)
  show fig1


