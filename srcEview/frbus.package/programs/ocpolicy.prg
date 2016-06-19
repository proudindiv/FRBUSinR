' Routine to simulate how the SEP baseline forecast would change if
' policymakers commit to a path for the federal funds rate that is 
' determined by optimal-control (OC) techniques to minimize a 
' quadratic loss function.
'
' Detailed information on the mechanics of the OC algorithm and the
' various required and optional parameters that set up and guide its
' execution is available in the MCE Solve Users Guide in the
' documentation directory.  Most relevant is the part of section 5
' that describes the "opt" simulation type as well as table 7.  

' As specified below, the loss function penalizes equally weighted 
' squared deviations of the unemployment rate from the natural rate,
' squared deviations of inflation from a 2 percent, and squared 
' quarterly changes in the funds rate.

' In the SEP baseline, agents with model-consistent (MC) expectations
' are initially assumed to project that the funds rate will follow the
' baseline path and to set their baseline expectations 
' accordingly.  At the start of the optimal control simulation, however, 
' these agents immediately and fully revise their expectations to be 
' consistent with the revision to the funds rate path that occurs under 
' optimal control -- that is, agents have rational expectations and 
' announced policy actions are completely credible.
'
' The experiment can be run with the zero lower bound (ZLB) imposed
' (%zerobound = "yes") or not imposed (%zerobound = "no").  When
' the ZLB is imposed, a penalty term is added to the loss function.

' *************************************************************
' Initial filename and parameter settings
' *************************************************************

' Subroutines
  include ../subs/master_library
  include ../subs/mce_solve_library

' Workfile    
  %wfstart = "1975q1"
  %wfend = "2100q4"
  %mainpage = "main"
  wfcreate(wf=aaa,page={%mainpage}) q {%wfstart} {%wfend}

' FRB/US model names and locations
  %varmod = "stdver"
  %varpath = "../mods/"
  %mcemod = "pfver"
  %mcepath = "../mods/"

' Input datbase
  %dbin  = "../data/longbase"

' Simulation range
  %simstart = "2014q4"
  %simend   = "2070q4"

' Primary loss function parameters:  The value of the policy instrument
' is chosen optimally from %drvstart to %drvend (60 qtrs) to minimize
' the loss function from %evlstart to %evlend (80 qtrs).  The three
' arguments of the period loss function are weighted by the the
' weight parameters and over time losses are discounted at the rate
' %discount
  %evlstart = %simstart
  %drvstart = %simstart
  call dateshift(%evlstart,%evlend,79)
  call dateshift(%drvstart,%drvend,59)
  %discount = ".99"
  %u_weight = "1.0"
  %p_weight = "1.0"
  %r_weight = "1.0"
  
' Optionally impose the zero lower bound
  %zerobound = "yes"


' ****************************************************************
' Retrieve data, model equations and coefficients, set
' policy options, and compute tracking residuals 
' ****************************************************************

' Specify MC expectations variables
  %mcvars_wp = "yes"
  %mcvars_all = "no"
 
' MCE asset pricing
  %zvars = "zdivgr zgap05 zgap10 zgap30 zrff5 zrff10 zrff30 zpi10 zpi10f zpic30 zpib5 zpic58 "

' MCE elsewhere
  if %mcvars_wp = "yes" and %mcvars_all = "no" then
    %zvars = %zvars + "zpicxfe zpieci "
    endif
  if %mcvars_all = "yes" then
    %zvars = %zvars + "zpicxfe zpieci "
    %zvars = %zvars + "zecd zeco zeh zgapc2 zlhp zpi5 zvpd zvpi zvps zxbd zxnfbi zxnfbs zyh zyhp zyht zynid "
    endif

' Load equations and coefficients
  call mce_load_frbus("mce_vars=%zvars,mod_b=%varmod,path_b=%varpath,mod_f=%mcemod,path_f=%mcepath")

' Add a ugap equation
  {%varmod}.append ugap - ugap_aerr = lur - lurnat

' Load data
  dbopen %dbin as longdata
  fetch(d=longdata) *

' Define SEP-consistent ustar and ugap series; this step is needed because 
' the baseline value of lurnat may not be fully SEP-consistent in the 
' short-to-medium ruun
  smpl @all
  series ustar = lurnat
  smpl %simstart 2025q4
  ustar = 5.35
  smpl @all
  series ugap = lur-ustar
  series ugap_aerr = 0

' Data for extra variables associated with MC expectations
  smpl @all
  call make_frbus_mcevars(%zvars)

' Set monetary policy option (the residual on the equation of
' the chosen option is the OC policy instrument)
  smpl @all
  call set_mp("dmptay")

' Initially turn off zero lower bound; if %zerobound = "yes", it will be
' imposed below by adding a penalty term to the loss function.
  smpl @all
  rffmin = -9999
' Turn off policy thresholds
  dmptrsh = 0
' Let the perceived equilibrium real interest rate vary
  drstar = 1

' Set fiscal policy so that it is exogenous for first 20 qtrs and then
' turns on debt targeting rule
  smpl %simstart %simstart + 19
  call set_fp("dfpex")
  smpl %simstart + 20 %simend
  call set_fp("dfpdbt")

' Set _aerr variables to zero
  smpl @all
  {%varmod}.makegroup(a,n) endog @endog
  call groupnew("endog","_aerr")
  call group2zero("endog_aerr")

' Standard solution options
  {%varmod}.solveopt(o=b,g=12,z=1e-12)
  {%mcemod}.solveopt(o=b,g=12,z=1e-12)

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
  {%mcemod}.addassign @all
  {%mcemod}.addinit(v=n) @all



'*************************************************************
' optimal policy setup
'*************************************************************

' The policy instrument is a time varying constant in the equation
' for the selected policy rule
  group opt_instrus rfftay_aerr

' Loss function variables (unemployment gap, 4-qtr PCE inflation, 
' and the first difference of the federal funds rate)
  group opt_targs ugap pic4 delrff

' The desired paths of the loss function variables are specified in
' series with "_t" suffix.
  smpl @all
  series ugap_t   = 0
  series delrff_t = 0
  series pic4_t   = 2.0

' The weights on the loss function arguments are specified in
' series with "_w" suffix.
  series ugap_w   = @val(%u_weight)
  series pic4_w   =  @val(%p_weight)
  series delrff_w =  @val(%r_weight)

  !discount = @val(%discount)
  smpl %simstart+1 %simend
  ugap_w   = !discount * ugap_w(-1)     
  pic4_w   = !discount * pic4_w(-1)     
  delrff_w = !discount * delrff_w(-1)     

' Zero bound penalty function
  if %zerobound = "yes" then
    {%varmod}.append penalty - penalty_aerr = _
       @recode(rff<(rff_lo_bnd+rff_lo_shift), _ 
         3.0*((rff_lo_bnd+rff_lo_shift)-rff),  _
         0) _
     + @recode(rff<(rff_lo_bnd+rff_lo_shift), _
       .10*exp(10*(rff-(rff_lo_bnd+rff_lo_shift))), _
       .10*exp(-20*(rff-(rff_lo_bnd+rff_lo_shift))))
    smpl @all
    series rff_lo_bnd = .125
    series rff_lo_shift = .00
    series penalty = 0
    series penalty_aerr = 0
    series rffmin = -9999
    %penalty_weight = "10.0"
    opt_targs.add penalty
    smpl @all
    series penalty_a = 0
    series penalty_t = 0
    series penalty_w = @val(%penalty_weight)
    smpl %simstart+1 %simend
    penalty_w = !discount * penalty_w(-1)     
    endif



'*************************************************************
' optimal policy simulation
'*************************************************************

  %sufcontrol = "_1"

' In %simstr, the "type=opt" string designates a commitment-based 
' OC simulation.  The required "instrus" and "targs" keywords point to
' the groups containing the policy instrument(s) and target variables.
' FRB/US simulations of this type generally run much more quickly with
' the newton MCE algorithm than they do with qnewton. 
  %modstr = "mod_b=%varmod,mod_f=%mcemod,mce_vars=%zvars"
  %algstr = "jinit=interp(4), meth=newton"
  %simstr = "type=opt,instrus=opt_instrus,targs=opt_targs"
  %simstr = %simstr + ",scen,suf=" + %sufcontrol+ ",solveopt=%sopt"
  %simstr = %simstr + ",lend=" + %evlend + ",iend=" + %drvend + ",lmax=20"
  smpl {%simstart} {%simend}
  call mce_run(%modstr,%algstr,%simstr)

' When the ZLB is imposed, run the OC algorithm a second time, 
' after adjusting the intercept of the penalty function,
' to hit ZLB more closely
  if %zerobound = "yes" then
    smpl if rff{%sufcontrol} <.2
    rff_lo_shift = .125 - rff{%sufcontrol} 
    %modstr = ""
    %algstr = ""
    %simstr = "type=opt,instrus=opt_instrus,targs=opt_targs"
    %simstr = %simstr + ",solveopt=%sopt"
    %simstr = %simstr + ",lend=" + %evlend + ",iend=" + %drvend + ",lmax=20"
    smpl {%simstart} {%simend}
    call mce_run(%modstr,%algstr,%simstr)
    endif


'***********************************************************
' graph results
'***********************************************************

  call dateshift(%simstart,%graphstart,-8)
  call dateshift(%simstart,%graphend,32)

  smpl %graphstart %graphend
  graph fig1a.line rff{%sufcontrol} rff
  fig1a.options size(7,4.2)
  fig1a.legend display -inbox position(3.8,2.8) font("arial",15)
  fig1a.datelabel format(yy)
  fig1a.addtext(6.4,-.30,font("arial",13),keep) percent
  fig1a.axis(left) font("arial",15)
  fig1a.axis(bottom) font("arial",15)
  fig1a.setelem(1) lcolor(red)  legend("optimal control") lwidth(2)
  fig1a.setelem(2) lcolor(black) legend("SEP-consistent baseline") lwidth(2)
  fig1a.addtext(t,just(c),font("arial",18)) Federal Funds Rate


  smpl %graphstart %graphend
  graph fig1b.line rg10{%sufcontrol} rg10
  fig1b.options size(7,4.2)
  fig1b.legend display -inbox position(3.8,2.8) font("arial",15)
  fig1b.datelabel format(yy)
  fig1b.addtext(6.4,-.30,font("arial",13),keep) percent
  fig1b.axis(left) font("arial",15)
  fig1b.axis(bottom) font("arial",15)
  fig1b.setelem(1) lcolor(red)  legend("optimal control") lwidth(2)
  fig1b.setelem(2) lcolor(black) legend("SEP-consistent baseline") lwidth(2)
  fig1b.addtext(t,just(c),font("arial",18)) 10-Year Treasury Yield


  smpl %graphstart %graphend
  graph fig1c.line lur{%sufcontrol} lur
  fig1c.options size(7,4.2)
  fig1c.legend display -inbox position(3.9,0.3) font("arial",15)
  fig1c.datelabel format(yy)
  fig1c.addtext(6.4,-.30,font("arial",13),keep) percent
  fig1c.axis(left) font("arial",15)
  fig1c.axis(bottom) font("arial",15)
  fig1c.setelem(1) lcolor(red)  legend("optimal control") lwidth(2)
  fig1c.setelem(2) lcolor(black) legend("SEP-consistent baseline") lwidth(2)
  fig1c.addtext(t,just(c),font("arial",18)) Unemployment Rate


  smpl %graphstart %graphend
  graph fig1d.line pic4{%sufcontrol} pic4
  fig1d.options size(7,4.2)
  fig1d.legend display -inbox position(0.5,0.2) font("arial",15)
  fig1d.datelabel format(yy)
  fig1d.addtext(6.4,-.30,font("arial",13),keep) percent
  fig1d.axis(left) font("arial",15)
  fig1d.axis(bottom) font("arial",15)
  fig1d.setelem(1) lcolor(red)  legend("optimal control") lwidth(2)
  fig1d.setelem(2) lcolor(black) legend("SEP-consistent baseline") lwidth(2)
  fig1d.addtext(t,just(c),font("arial",18)) PCE Inflation Rate (4-Quarter)


  graph fig1.merge fig1a fig1b fig1c fig1d
  if %mcvars_wp = "no" and %mcvars_all = "no" then
    %title = "Macroeconomic Effects of Optimal-Control Policy with Rational Expectations only in Financial Markets"
    endif
  if %mcvars_wp = "yes" and %mcvars_all = "no" then
    %title = "Macroeconomic Effects of Optimal-Control Policy\n With Rational Expectationsin Financial Markets and Wage-Price Setting"
    endif 
  if %mcvars_all = "yes" then
    %title = "Macroeconomic Effects of Optimal-Control Policy With Full Rational Expectations"
    endif

  if %zerobound = "yes" then
    %title = %title + "\rZLB Imposed"
    else
    %title = %title + "\rZLB not Imposed"
    endif    

  fig1.addtext(t,just(c),font("Arial",20)) {%title}
  fig1.align(2,1,1.25)
  show fig1



