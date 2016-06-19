' Simulate six ping simulations (AKA simple IRFs) 
'
' Notes:
'
' 1. Choose between VAR expectations and several MCE alternatives
' with the %mcevars parameter.
'
'   - %mcevars = "none"   => VAR expectations everywhere
'   - %mcevars = "mcap"   => MCE in asset pricing, VAR expectations elsewhere
'   - %mcevars = "mcapwp" => MCE in asset pricing and price-wage setting;
'        VAR expectations elsewhere
'   - %mcevars = "all"    => MCE everywhere
'
' Note that even when %mcevars = "none", the program does many
' of the setup steps for an MCE simulation even though it never
' uses what they create.

' 2. Seven of the pings are one-time shocks to the residual of an
' equation whose structure contains a large autoregressive
' element.  The remaining ping involves a permanent increase in the
' level of trend MFP.

' 3. The eight pings are:
'
'   - A 100 basis point upward shock to the rffintay monetary
'     policy rule
'   - An increase in federal purchases equal to one percent of
'     baseline GDP
'   - A one percent permanent increase in the level of trend MPF 
'   - A 100 bp increase in the equity premium
'   - A $10 per barrel increase in the price of oil
'   - A 1 percent (ar) increase in the growth rate of 
'     multifactor productivity
'   - Increases of 100 basis points to the 10-year Treasury term premium, 
'     75 basis points to the 5-year premium, and 30 basis points to the
'     30-year premium 
'   - A 10 percent increase in the (real) exchange rate

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
  %simstart = "2020q1"
  %simend   = "2069q4"

' Choose an expectations option ("none" => VAR expectations, "mcap", "mcapwp", "all") 
  %mcevars = "none"

' ****************************************************************
' Retrieve data, model equations and coefficients, set
' policy options, and compute tracking residuals 
' ****************************************************************

' MCE variable setup 
  if %mcevars = "none" then
    %zvars = "zpic58 "
    else
    %zvars = "zdivgr zgap05 zgap10 zgap30 zrff5 zrff10 zrff30 zpi10 zpi10f zpic30 zpib5 zpic58 "
    if %mcevars = "mcapwp" or %mcevars = "all" then
      %zvars = %zvars + "zpicxfe zpieci "
      endif
    if %mcevars = "all" then
      %zvars = %zvars + "zecd zeco zeh zgapc2 zlhp zpi5 zvpd zvpi zvps zxbd zxbi zxbs zyh zyhp zyht zynid "
      endif
    endif

' Load equations and coefficients
  call mce_load_frbus("mce_vars=%zvars,mod_b=%varmod,path_b=%varpath,mod_f=%mcemod,path_f=%mcepath")

' Load data
  dbopen %dbin as longdata
  fetch(d=longdata) *

' Data for extra variables associated with MC expectations
  smpl @all
  call make_frbus_mcevars(%zvars)

' Set monetary policy
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




  
' ****************************************************************
' Ping simulations
' ****************************************************************

  %suf = "_1"
  {%varmod}.scenario(n,a={%suf}) "ping"
  {%mcemod}.scenario(n,a={%suf}) "ping"
  
' **********************************
' Federal Funds Rate: RFF ping 

  %ping = "rff"

  smpl %simstart %simstart
  rffintay_aerr = rffintay_aerr + 1
  smpl %simstart %simend
  if %mcevars = "none" then
    {%varmod}.solve
    else
    %modstr = "mod_b=%varmod,mod_f=%mcemod,mce_vars=%zvars"
    %algstr = "meth=qnewton"
    %simstr = "type=single,solveopt=%sopt,suf=" + %suf
    call mce_run(%modstr,%algstr,%simstr)
    endif
  smpl %simstart %simstart
  rffintay_aerr = rffintay_aerr - 1
  call copyit

' **********************************
' Treasury Term Premium: RG10P, RG5P, and RG30P ping 

  %ping = "prem"
  smpl @all
  series rg30p_aerr = 0
  smpl %simstart %simstart
  rg10p_aerr = rg10p_aerr + 1
  rg5p_aerr = rg5p_aerr + .75
  rg30p_aerr = rg30p_aerr + .35
  smpl %simstart %simend
  if %mcevars = "none" then
    {%varmod}.solve
    else
    %modstr = "mod_b=%varmod,mod_f=%mcemod,mce_vars=%zvars"
    %algstr = "meth=qnewton"
    %simstr = "type=single,solveopt=%sopt,suf=" + %suf
    call mce_run(%modstr,%algstr,%simstr)
    endif
  smpl %simstart %simstart
  rg10p_aerr = rg10p_aerr - 1
  rg5p_aerr = rg5p_aerr - 0.75
  rg30p_aerr = rg30p_aerr - .35
  call copyit
  smpl %simstart %simend
  series rg10p_{%ping} = rg10p{%suf} - rg10p

' **********************************
' Federal Purchases: EGFO ping

  %ping = "eg"
  smpl %simstart %simstart
  egfo_aerr = egfo_aerr + .01*xgdpn/egfon
  smpl %simstart %simend
  if %mcevars = "none" then
    {%varmod}.solve
    else
    %modstr = "mod_b=%varmod,mod_f=%mcemod,mce_vars=%zvars"
    %algstr = "meth=qnewton"
    %simstr = "type=single,solveopt=%sopt,suf=" + %suf
    call mce_run(%modstr,%algstr,%simstr)
    endif
  smpl %simstart %simstart
  egfo_aerr = egfo_aerr - .01*xgdpn/egfon
  call copyit
  smpl %simstart %simend
  series egfn_shr_{%ping} = 100*(egfn{%suf}/xgdpn{%suf} - egfn/xgdpn)

' **********************************
' Equity Premium: REQP ping

  %ping = "reqp"
  smpl %simstart %simstart
  reqp_aerr = reqp_aerr + 1
  smpl %simstart %simend
  if %mcevars = "none" then
    {%varmod}.solve
    else
    %modstr = "mod_b=%varmod,mod_f=%mcemod,mce_vars=%zvars"
    %algstr = "meth=qnewton"
    %simstr = "type=single,solveopt=%sopt,suf=" + %suf
    call mce_run(%modstr,%algstr,%simstr)
    endif
  smpl %simstart %simstart
  reqp_aerr = reqp_aerr - 1
  call copyit
  smpl %simstart %simend
  series reqp_{%ping} = reqp{%suf} - reqp


' **********************************
' Oil Prices: POILR ping

  %ping = "oil"
  smpl %simstart %simstart
  poilr_aerr = poilr_aerr + 10/pxb
  smpl %simstart %simend
  if %mcevars = "none" then
    {%varmod}.solve
    else
    %modstr = "mod_b=%varmod,mod_f=%mcemod,mce_vars=%zvars"
    %algstr = "meth=qnewton"
    %simstr = "type=single,solveopt=%sopt,suf=" + %suf
    call mce_run(%modstr,%algstr,%simstr)
    endif
  smpl %simstart %simstart
  poilr_aerr = poilr_aerr - 10/pxb
  call copyit
  smpl %simstart %simend
  series poil_{%ping} = poil{%suf} - poil



' **********************************
' Exchange Rate: FPXRR ping

  %ping = "exch"
  smpl %simstart %simstart
  series shock_fpxr = log(1.1)
  fpxrr_aerr = fpxrr_aerr + shock_fpxr

  smpl %simstart %simend
  if %mcevars = "none" then
    {%varmod}.solve
    else
    %modstr = "mod_b=%varmod,mod_f=%mcemod,mce_vars=%zvars"
    %algstr = "meth=qnewton"
    %simstr = "type=single,solveopt=%sopt,suf=" + %suf
    call mce_run(%modstr,%algstr,%simstr)
    endif
   
  series fpxr_{%ping} = fpxr{%suf} - fpxr 
  smpl %simstart %simstart
  fpxrr_aerr = fpxrr_aerr - shock_fpxr
  call copyit


' **********************************
' HMFPT ping

  %ping = "hmfp"
  smpl %simstart %simstart
  hmfpt_aerr = hmfpt_aerr + 1
  smpl %simstart %simend
  if %mcevars = "none" then
    {%varmod}.solve
    else
    %modstr = "mod_b=%varmod,mod_f=%mcemod,mce_vars=%zvars"
    %algstr = "meth=qnewton"
    %simstr = "type=single,solveopt=%sopt,suf=" + %suf
    call mce_run(%modstr,%algstr,%simstr)
    endif
  smpl %simstart %simstart
  hmfpt_aerr = hmfpt_aerr - 1
  call copyit
  smpl %simstart %simend
  series hmfpt_{%ping} = hmfpt{%suf} - hmfpt


' **********************************
' MFPT ping

  %ping = "mfp"
  smpl %simstart %simstart
  mfpt_aerr = mfpt_aerr + .01
  smpl %simstart %simend
  if %mcevars = "none" then
    {%varmod}.solve
    else
    %modstr = "mod_b=%varmod,mod_f=%mcemod,mce_vars=%zvars"
    %algstr = "meth=qnewton"
    %simstr = "type=single,solveopt=%sopt,suf=" + %suf
    call mce_run(%modstr,%algstr,%simstr)
    endif
  smpl %simstart %simstart
  mfpt_aerr = mfpt_aerr - .01
  call copyit
  smpl %simstart %simend
  series mfpt_{%ping} = 100*(mfpt{%suf}/mfpt - 1)


' ****************************************************************
' Individual ping graphs
' ****************************************************************
  call graphit


' ****************************************************************
' Composite figures
' ****************************************************************

  if %mcevars = "none" then
    %exp = "VAR Expectations"
    endif
  if %mcevars = "mcap" then
    %exp = "MC (MCAP) Expectations"
    endif
  if %mcevars = "mcapwp" then
    %exp = "MC (MCAP+WP) Expectations"
    endif
  if %mcevars = "all" then
    %exp = "MC (ALL) Expectations"
    endif

  %t1 = "FRB/US Ping Simulations:  " + %exp + " -- I"
  %t2 = "FRB/US Ping Simulations:  " + %exp + " -- II"
  %t3 = "FRB/US Ping Simulations:  " + %exp + " -- III"

' Figure 1

  graph fig_1.merge gr_rff gr_eg gr_reqp
  fig_1.align(3,.4,1.0)
  fig_1.addtext(t,just(c),font(12)) %t1
  show fig_1



' Figure 2

  graph fig_2.merge gr_oil gr_hmfp gr_mfp
  fig_2.align(3,.4,1.0)
  fig_2.addtext(t,just(c),font(12)) %t2
  show fig_2


' Figure 3

  graph fig_3.merge gr_prem gr_exch
  fig_3.align(3,.4,1.0)
  fig_3.addtext(t,just(c),font(12)) %t3
  show fig_3
'************************************************************
'************************************************************
'************************************************************
  subroutine copyit

  smpl %simstart %simend
  series picnia_{%ping} = picnia{%suf} - picnia
  series pic4_{%ping} = pic4{%suf} - pic4
  series picx4_{%ping} = picx4{%suf} - picx4
  series picxfe_{%ping} = picxfe{%suf} - picxfe
  series xgap2_{%ping} = xgap2{%suf} -xgap2
  series lur_{%ping} = lur{%suf} - lur
  series rff_{%ping} = rff{%suf} - rff

  endsub

'************************************************************
'************************************************************
'************************************************************
  subroutine plotit(string %grname, string %width, string %height, string %var1, string %title, string %units)


  graph {%grname}.line  {%var1} zero
  {%grname}.options size({%width},{%height}) -inbox
  {%grname}.setelem(1) linewidth(3) linepattern(1) linecolor(black)
  {%grname}.addtext(t,just(c),font(9)) %title
  {%grname}.addtext(0,-.15,font(8),just("r")) %units
  {%grname}.datelabel format(yyyy)
  {%grname}.legend -display
  {%grname}.axis(b) font(9)
  {%grname}.axis(l) font(9)

  endsub


'************************************************************
'************************************************************
'************************************************************
  subroutine graphit

  smpl %simstart %simstart + 39
  series zero = 0

  delete(noerr) gr_*


' RFF ping

  %ping = "rff"

  %name = %ping + "a"
  %var1 =  "xgap2_" + %ping
  %tt = "Response of Output Gap\rto Funds Rate"
  call plotit(%name,"2","1.5",%var1,%tt,"percentage points")

  %name = %ping + "b"
  %var1 =  "picxfe_" + %ping
  %tt = "Response of Core Inflation\rto Funds Rate"
  call plotit(%name,"2","1.5",%var1,%tt,"percentage points")

  %name = %ping + "c"
  %var1 =  "rff_" + %ping
  %tt = "Response of Funds Rate\rto Funds Rate"
  call plotit(%name,"2","1.5",%var1,%tt,"percentage points")

  graph gr_{%ping}.merge  {%ping}a {%ping}b {%ping}c
  gr_{%ping}.align(3,.40,.40)


  
' EGFO ping


  %ping = "eg"

  %name = %ping + "a"
  %var1 =  "xgap2_" + %ping
  %tt = "Response of Output Gap\rto Federal Purch"
  call plotit(%name,"2","1.5",%var1,%tt,"percentage points")

  %name = %ping + "b"
  %var1 =  "picxfe_"  + %ping
  %tt = "Response of Core Inflation\rto Federal Purch"
  call plotit(%name,"2","1.5",%var1,%tt,"percentage points")

  %name = %ping + "c"
  %var1 =  "egfn_shr_" + %ping 
  %tt = "Response of Federal Purch\rto Federal Purch"
  call plotit(%name,"2","1.5",%var1,%tt,"percent of GDP")

  graph gr_{%ping}.merge  {%ping}a {%ping}b {%ping}c
  gr_{%ping}.align(3,.40,.40)

  
' REQP ping

  %ping = "reqp"

  %name = %ping + "a"
  %var1 =  "xgap2_" + %ping 
  %tt = "Response of Output Gap\rto Equity Premium"
  call plotit(%name,"2","1.5",%var1,%tt,"percentage points")

  %name = %ping + "b"
  %var1 =  "picxfe_" + %ping 
  %tt = "Response of Core Inflation\rto Equity Premium"
  call plotit(%name,"2","1.5",%var1,%tt,"percentage points")

  %name = %ping + "c"
  %var1 =  "reqp_" + %ping 
  %tt = "Response of Equity Premium\rto Equity Premium"
  call plotit(%name,"2","1.5",%var1,%tt,"percentage points")

  graph gr_{%ping}.merge  {%ping}a {%ping}b {%ping}c
  gr_{%ping}.align(3,.40,.40)


  
' POILR ping

  %ping = "oil"

  %name = %ping + "a"
  %var1 =  "xgap2_" + %ping 
  %tt = "Response of Output Gap\rto Oil Price"
  call plotit(%name,"2","1.5",%var1,%tt,"percentage points")

  %name = %ping + "b"
  %var1 =  "picxfe_" + %ping 
  %tt = "Response of Core Inflation\rto Oil Price"
  call plotit(%name,"2","1.5",%var1,%tt,"percentage points")

  %name = %ping + "c"
  %var1 =  "poil_" + %ping 
  %tt = "Response of Oil Price\rto Oil Price"
  call plotit(%name,"2","1.5",%var1,%tt,"dollars per barrel")

  graph gr_{%ping}.merge  {%ping}a {%ping}b {%ping}c
  gr_{%ping}.align(3,.40,.40)


  
' HMFPT ping

  %ping = "hmfp"

  %name = %ping + "a"
  %var1 =  "xgap2_" + %ping 
  %tt = "Response of Output Gap\rto MFP Growth"
  call plotit(%name,"2","1.5",%var1,%tt,"percentage points")

  %name = %ping + "b"
  %var1 =  "picxfe_" + %ping 
  %tt = "Response of Core Inflation\rto MFP Growth"
  call plotit(%name,"2","1.5",%var1,%tt,"percentage points")

  %name = %ping + "c"
  %var1 =  "hmfpt_" + %ping 
  %tt = "Response of MFP Growth\rto MFP Growth"
  call plotit(%name,"2","1.5",%var1,%tt,"percentage points")

  graph gr_{%ping}.merge  {%ping}a {%ping}b {%ping}c
  gr_{%ping}.align(3,.40,.40)


  
' MFPT ping

  %ping = "mfp"

  %name = %ping + "a"
  %var1 =  "xgap2_" + %ping 
  %tt = "Response of Output Gap\rto MFP Level"
  call plotit(%name,"2","1.5",%var1,%tt,"percentage points")

  %name = %ping + "b"
  %var1 =  "picxfe_" + %ping 
  %tt = "Response of Core Inflation\rto MFP Level"
  call plotit(%name,"2","1.5",%var1,%tt,"percentage points")

  %name = %ping + "c"
  %var1 =  "mfpt_" + %ping 
  %tt = "Response of MFP Level\rto MFP Level"
  call plotit(%name,"2","1.5",%var1,%tt,"percent")

  graph gr_{%ping}.merge  {%ping}a {%ping}b {%ping}c
  gr_{%ping}.align(3,.40,.40)

  
%ping = "prem"

  %name = %ping + "a"
  %var1 =  "xgap2_" + %ping 
  %tt = "Response of Output Gap\rto Term Premium"
  call plotit(%name,"2","1.5",%var1,%tt,"percentage points")

  %name = %ping + "b"
  %var1 =  "picxfe_" + %ping 
  %tt = "Response of Core Inflation\rto Term Premium"
  call plotit(%name,"2","1.5",%var1,%tt,"percentage points")

  %name = %ping + "c"
  %var1 =  "rg10p_" + %ping 
  %tt = "Response of Term Premium (10-year) \rto Term Premium"
  call plotit(%name,"2","1.5",%var1,%tt,"percent")

  graph gr_{%ping}.merge  {%ping}a {%ping}b {%ping}c
  gr_{%ping}.align(3,.40,.40)

%ping = "exch"

  %name = %ping + "a"
  %var1 =  "xgap2_" + %ping 
  %tt = "Response of Output Gap\rto Exchange Rate"
  call plotit(%name,"2","1.5",%var1,%tt,"percentage points")

  %name = %ping + "b"
  %var1 =  "picxfe_" + %ping 
  %tt = "Response of Core Inflation\rto Exchange Rate"
  call plotit(%name,"2","1.5",%var1,%tt,"percentage points")

  %name = %ping + "c"
  %var1 =  "fpxr_" + %ping 
  %tt = "Response of  Exchange Rate \rto Exchange Rate"
  call plotit(%name,"2","1.5",%var1,%tt,"percent")

  graph gr_{%ping}.merge  {%ping}a {%ping}b {%ping}c
  gr_{%ping}.align(3,.40,.40)
  endsub


