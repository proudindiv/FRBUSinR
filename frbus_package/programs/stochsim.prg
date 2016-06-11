' Program for stochastic sims under VAR expectations

' The stochastic shocks are bootstrapped from the de-meaned
' historical errors of stochastic equations.  The parameters
' %residstart and %residend declare the historical error range.
' A list of stochastic equations is extracted from the file
' pointed to by %varinfo.

' The bootstrap procedure randomly draws one historical quarter 
' at a time when the parameter %errorblock = 1; alternatively, if
' %errorblock = 2, then the procedure would randomly draw two
' successive quarters at a time.

' The stochastic replications are simulated in a simple loop,
' rather than using the built-in EViews stochastic simulation
' procedure, so that shocks in the first simulation quarter can
' be scaled down by the parameter %q1_shock_damp.  This feature
' is useful when uncertainty about the first simulation quarter
' in real-time analysis by known information.  The shocks are
' not rescaled when %q1_shock_damp = 1. 

' Similarly, the parameter %rff_weight_q1 is also designed to be
' used in real-time analysis when the first simulation quarter
' corresponds to a quarter that is already under way.  The
' parameter provides the fractional value to be given to the 
' monetary policy rule; the remaining fractional value is given to
' an exogenous value.

' The document Simulation Basics discusses the effects of
' imposing the zero lower bound (ZLB) on the federal funds rate
' (%zerobound parameter) and imposing threshold conditions
' on the liftoff of the funds rate from a ZLB episode 
' (%threshold parameter).

' *************************************************************
' Initial filename and parameter settings
' *************************************************************

' Subroutines
  include ../subs/master_library

' Workfile    
  %wfstart = "1965q1"
  %wfend = "2020q4"
  %mainpage = "main"
  wfcreate(wf=aaa,page={%mainpage}) q {%wfstart} {%wfend}

' FRB/US model name and location
  %varmod = "stdver"
  %varpath = "../mods/"
  %varinfo = "../mods/stdver_varinfo"

' Input database
  %dbin  = "../data/longbase"

' Simulation range
  %simstart = "2014q1"
  %simend   = "2018q4"

' Stochastic parameters
  rndseed 12345
  %errorblock = "1"
  %residstart = "1970q1"
  %residend   = "2012q4"
  %nsims      = "1000"
  %q1_shock_damp = ".5"
  %dbout_series = "rff lur picxfe picnia picx4 xgap2 hggdp anngr"

' Monetary policy
  %zerobound  = "yes"
  %threshold  = "yes"
  %rff_weight_q1  = ".25"

' ****************************************************************
' Retrieve data, model equations and coefficients, set
' policy options, and compute tracking residuals 
' ****************************************************************

' Load equations, coefficients, and variable information
  ld_frbus_eqs(modelname=%varmod,modelpath=%varpath)  
  ld_frbus_cfs(modelname=%varmod,modelpath=%varpath)
  ld_varinfo(pathname=%varinfo)

' add 4-qtr gdp growth equation
  {%varmod}.append anngr - anngr_aerr = 100*((xgdp/xgdp(-4))-1)

' Load data
  dbopen %dbin as longbase
  fetch(d=longbase) *
  smpl @all
  series anngr = 100*((xgdp/xgdp(-4))-1)

' Set monetary policy to inertial Taylor rule (dmpintay, rffintay)
  smpl @all
  call set_mp("dmpintay")
  if %zerobound = "yes" then
    rffmin = .250
    else
    rffmin = -9999
    endif
  if %threshold = "yes" and %zerobound = "no" then
    %err = "Error: policy threshold conditions can only be used when the ZLB is imposed"
    @uiprompt(%err)
    stop
    endif
  if %threshold = "yes" and %zerobound = "yes" then
    dmptrsh = 1
    dmptr = 0
    else
    dmptrsh = 0
    endif
  drstar = 0

  smpl {%simstart} {%simstart}
  dmpintay = @val(%rff_weight_q1)
  dmpex = 1 - dmpintay

' Set fiscal policy
  smpl @all
  call set_fp("dfpex")

  dmpstb = 1

' Set _aerr variables to zero
  smpl @all
  {%varmod}.makegroup(a,n) endog @endog
  call groupnew("endog","_aerr")
  call group2zero("endog_aerr")

' Standard solution options
  {%varmod}.solveopt(o=b,g=14,z=1e-14)

' Assign baseline tracking add factors
  %suftrk = "_0"
  smpl %residstart %simend 
  {%varmod}.addassign @all
  {%varmod}.addinit(v=n) @all
  {%varmod}.scenario(n,a={%suftrk}) "track"
  {%varmod}.solve
  scalar mm = @max(@abs(xgap{%suftrk}-xgap))
  if mm > .0001 then
    statusline dynamic tracking simulation failed for {%varmod}
    stop
    endif

' ****************************************************************
' More monetary policy settings
' ****************************************************************


' if policy thresholds are turned on, set add factors on endogenous
' threshold switch variables to zero
  if %threshold = "yes" then
    smpl @all
    dmptpi_a = 0
    dmptlur_a = 0
    dmptmax_a = 0
    dmptr_a = 0
    endif

' if the zero bound is binding in part or all of the baseline, the
' adds (_a) on the policy rule and the funds rate equations are
' determined so as to satisfy the following conditions.
'
' a.  the stochastic funds rate equals the maximum of the zero bound and
'     the prediction of the chosen policy rule (this simply requires that
'     rffe_a and rffe_aerr be zero)
' b.  the prediction of the chosen policy rule is subject to _a add factors
'     that are determined as follows:
'     (1) in quarters when the zero bound is not binding in the baseline,
'         the associated add factors equal the values that make
'         the policy rule equation match the baseline funds rate under
'         baseline conditions (this is satisfied by the tracking adds on
'         the policy rule as long as the baseline value of the policy rule
'         variable equals the baseline funds rate);
'     (2) in quarters when the zero bound is binding in the baseline,
'         the associated add factors are determined by linear interpolation
'         of the add factors generated according to b(1) for the
'         unbound quarters;
'     (3) when the zero bound is binding in all baseline quarters, the
'         policy rule add factors are zero;
'     (4) the zero bound is assumed to be binding in the baseline whenever
'         the baseline funds rate (rffe) is within 25 basis points of the
'         zero bound variable (rffmin).

  smpl %simstart %simend
  series not_constrnd = ((rffe - rffmin) >= .25)
  !tmp_max = @max(not_constrnd)
  !tmp_min = @min(not_constrnd)

' zero bound binding in some quarters
  if (!tmp_max = 1) and (!tmp_min = 0) then
    smpl %simstart %simend
    rffe_a = 0
    rffe_aerr = 0
    rffintay_aerr = 0
    smpl %simstart %simend if (not_constrnd = 0)
    rffintay_a = NA
    %series_in = "rffintay_a"
    %series_out = %series_in + "_int"
    call interp_lin(%series_in,%series_out,%simstart,%simend)
    rffintay_a = {%series_out}
    endif
 
' zero bound binding in all quarters
  if (!tmp_max = 0) and (!tmp_min = 0) then
    smpl %simstart %simend
    rffe_a = 0
    rffe_aerr = 0
    rffintay_aerr = 0
    rffintay_a = 0
    endif

' ****************************************************************
' Stochastic shocks
' ****************************************************************

' copy historical residuals into series whose names have _err suffixes
  smpl %residstart %residend
  copy *_a *_err

' use vinfo table to create list/group of equations to receive shocks
  %tmp = " "
  for !i = 1 to vinfo_size 
    %vname = @word(vinfo_vname,!i)
    %stoch = @word(vinfo_stoch,!i)
    if %stoch <> "NO" then
      %tmp = %tmp + " " + %vname
      endif
    next
  group shock {%tmp}

' demean historical residuals and store them in a matrix

  smpl %residstart %residend
  %error_names = " "
  for !i = 1 to shock.@count
    %temp = shock.@seriesname(!i) + "_err"
    scalar mm = @mean({%temp})
      series {%temp} =  {%temp} - mm
      %error_names = %error_names + " " + %temp 
      next

  group errors {%error_names}
  smpl %residstart %residend
  stom(errors,errormat)

' create table of error statistics
  !nrows = 3 + errors.@count
  !ncols = 3
  table(!nrows,!ncols) error_tab
  error_tab.setjust(r1c1:r{!nrows}c1) left
  error_tab.setwidth(1) 20
  error_tab(1,1) = "error"
  error_tab(1,2) = "mean"
  error_tab(1,3) = "std-dev"
  smpl %residstart %residend
  for !i = 1 to errors.@count
    series tseries = errors(!i)
    error_tab(!i+3,1) = errors.@seriesname(!i)
    error_tab(!i+3,2) = @mean(tseries)
    error_tab(!i+3,3) = @stdev(tseries)
    next

' miscellaneous

  smpl %simstart %simend
  scalar nqtrs = @obssmpl
  scalar nrepl = {%nsims}
  scalar nsims = {%nsims}
  scalar nerrors = @rows(errormat)
  scalar bbbb = nqtrs-nerrors

  call groupnew("shock","_aerr")

  group track {%dbout_series}
  call groupnames2string("track",%tracknames)  

' for tracked variables, set up matrices to hold stochastic results
  for !i = 1 to track.@count
    %temp = track.@seriesname(!i)
    matrix(nqtrs,nrepl) {%temp}_mat
    next


' ****************************************************************
' Stochastic sims
' ****************************************************************

  %sufstoch = "_1"
  {%varmod}.scenario(n,a={%sufstoch}) "stoch sims"

  
' *********************************************
' stochastic simulation loop (sims are run one at a time)

  smpl %simstart %simend
  for !i = 1 to nrepl
    statusline running stochastic sim number  !i
  ' draw nqtrs random rows from the matrix of historical errors,
  ' damp the shocks in the first drawn row, and load the shocks
  ' into the respective  _aerr error series
    matrix stocherrors = @resample(errormat,bbbb,{%errorblock})
    for !j = 1 to @columns(stocherrors)
      stocherrors(1,!j) = @val(%q1_shock_damp) * stocherrors(1,!j)
      next
    mtos(stocherrors,shock_aerr)
    {%varmod}.solve
  ' store solution values 
    for !j = 1 to track.@count
      %temp = track.@seriesname(!j)
      %temp1 = %temp + "_mat"
      stom({%temp}{%sufstoch},tmp)
      colplace({%temp1},tmp,!i)  
      next
    next

' ****************************************************************
' Statistics
' ****************************************************************

  statusline computing statistics

  smpl %simstart %simend
  !index = 2
  series year = @year
  series quarter = @quarter
  alpha yyyyqq = @str(year) + "Q" + @str(quarter)
  !lqtr = @dtoo(%simstart) - 1
  !nstats = 8

'create a summary table in which to store key results

  call tableform("summary_tab","100")

' loop over each tracked variable, 
  for !ii1 = 1 to track.@count
    %trkname = track.@seriesname(!ii1)
  ' compute statistics
    call makestats(%trkname)

  ' load statistics into variable-specific table 
    %tabname = %trkname + "_tab"
    call tableform(%tabname,@str(nqtrs+2))
    for !ii2 = 1 to nqtrs
      {%trkname}_tab(!ii2+2,1) = yyyyqq(!lqtr + !ii2)
      for !ii3 = 1 to !nstats
        {%trkname}_tab(!ii2+2,!ii3+1) = {%trkname}_stats(!ii2,!ii3)
        next
      next

  ' load statistics for each q4 observation into summary table 
    !index = !index + 1
    summary_tab(!index,1) = %trkname
    !index = !index + 1
    for !ii2 = 1 to nqtrs
      if quarter(!lqtr+!ii2) = 4 then
        for !ii3 = 1 to !nstats + 1
          summary_tab(!index,!ii3) = {%trkname}_tab(!ii2+2,!ii3)
          next
        !index = !index + 1
        endif
      next

  ' make graph showing 70 and 90 percent bands
    graph {%trkname}_graph.band {%trkname}_lo90 {%trkname}_hi90 {%trkname}_lo70 _
        {%trkname}_hi70 {%trkname}_base
    {%trkname}_graph.addtext(t) %trkname
    {%trkname}_graph.options size(6,4.5)

  next


' **************************************
' summary graph

  lur_graph.addtext(t,just(c),font("arial",12)) Unemployment Rate
  rff_graph.addtext(t,just(c),font("arial",12)) Federal Funds Rate
  picx4_graph.addtext(t,just(c),font("arial",12)) 4-qtr Core Inflation Rate
  anngr_graph.addtext(t,just(c),font("arial",12)) 4-qtr Real GDP Growth Rate
  lur_graph.legend -display
  rff_graph.legend -display
  picx4_graph.legend -display
  anngr_graph.legend -display

  graph summary_graph.merge lur_graph rff_graph picx4_graph anngr_graph
  summary_graph.legend -display
  summary_graph.addtext(t,just(c),font("Arial",16)) Stochastic Simulations\r(70 and 90 percent bands)
  show summary_graph


' **************************************
' summary spool
  spool results
  summary_tab.deleterow(!index) 100
  results.append summary_tab
  results.append summary_graph
  results.append error_tab
  results.display

'**************************************************************************
'**************************************************************************
'**************************************************************************
'**************************************************************************
'Subroutines


'**************************************************************************
'**************************************************************************
subroutine tableform(string %tabname, string %nrows)

  table(@val(%nrows),9) {%tabname}
  
  {%tabname}.setwidth(1:9) 8
  {%tabname}.setjust(r1c1:r{%nrows}c9) right
  {%tabname}.setformat(r2c2:r{%nrows}c9) f.3
  {%tabname}(1,1) = "qtr"
  {%tabname}(1,2) = "baseline"
  {%tabname}(1,3) = "mean"
  {%tabname}(1,4) = "median"
  {%tabname}(1,5) = "stdev"
  {%tabname}(1,6) = "90%-low"
  {%tabname}(1,7) = "90%-hi"
  {%tabname}(1,8) = "70%-low"
  {%tabname}(1,9) = "70%-hi"

  endsub 


'**************************************************************************
'**************************************************************************
subroutine makestats(string %trkname)

  %trkmat = %trkname + "_mat"
  %statsmat = %trkname + "_stats"
  matrix(nqtrs,8) {%statsmat}

  smpl {%simstart} {%simend}

' loop over each simulation quarter
  for !ii2 = 1 to nqtrs
  ' put simulation replications for this quarter into matrix tempm1
    matrix tempm1 = @sort(@rowextract({%trkmat},!ii2))
    {%statsmat}(!ii2,1) = {%trkname}(!lqtr + !ii2)
    {%statsmat}(!ii2,2) = @mean(tempm1) 
    {%statsmat}(!ii2,3) = tempm1(1,@floor(.50*nrepl))
    {%statsmat}(!ii2,4) = @stdev(tempm1)
    {%statsmat}(!ii2,5) = tempm1(1,@floor(.05*nrepl))
    {%statsmat}(!ii2,6) = tempm1(1,@floor(.95*nrepl))
    {%statsmat}(!ii2,7) = tempm1(1,@floor(.15*nrepl))
    {%statsmat}(!ii2,8) = tempm1(1,@floor(.85*nrepl))

    next

' also create individual series for each statistic
  series {%trkname}_base = 0
  series {%trkname}_mn = 0
  series {%trkname}_med  = 0
  series {%trkname}_se  = 0
  series {%trkname}_lo90 = 0
  series {%trkname}_hi90 = 0
  series {%trkname}_lo70 = 0
  series {%trkname}_hi70 = 0

  group {%trkname}_group {%trkname}_base {%trkname}_mn {%trkname}_med _
        {%trkname}_se {%trkname}_lo90 {%trkname}_hi90 {%trkname}_lo70 _
        {%trkname}_hi70
  mtos({%statsmat},{%trkname}_group)

  endsub
