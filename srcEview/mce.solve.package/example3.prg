' This example illustrates:
'
'   - Another manual approach to constructing the two operational
'     models that introduces new endogenous variables for the
'     expectations leads along with simple equations for the new
'     endogenous variables.
'   - The MCE instruments are the add factors on the equations for
'     the new endogenous variables; this circumstance requires that
'     add factors be assigned to the operational models prior to 
'     the call to mce_run.
'   - The option of defining the multiplier shock in commands that
'     are executed prior to the call to mce_run, when the manual
'     approach is used.
'   - The option of declaring model scenarios and the scenario
'     scenario alias prior to the call to mce_run, when the manual
'     approach is used.
'   - The model no longer satisfies the conditions for which the
'     "jinit=linear" option of the "newton" algorithm is designed.
'     This example uses "jinit=interp(4)" to specify a
'     particular approximate Jacobian.
'
'   - This example runs three simulations.  In the second and third 
'     simulations, the assignment of null strings to the first two 
'     arguments of the mce_run subroutine causes the simulations to be
'     run with the same internal models and algorithm (including
'     the Newton MCE Jacobian) that were created or declared in the
'     first simulation.  

'************************************************************
'************************************************************
'************************************************************
' Section 1:  Workfile, model name, simulation range

  include  mce_solve_library

' Workfile
  %wfstart = "2000q1"
  %wfend = "2100q4"
  %mainpage = "main"
  wfcreate(wf=aaa,page={%mainpage}) q {%wfstart} {%wfend}

' Model names
  %modb = "simpleb"
  %modf = "simplef"

' Simulation range
  %simstart = "2001q1"
  %simend = "2025q4"


'************************************************************
'************************************************************
'************************************************************
' Section 2:  Model, coefficients, and data

' equations in backward-looking model

  model {%modb}
  {%modb}.append pinf = cp(1) * pinf(-1) + (.98-cp(1))*zpinf+ cp(2) * ygap
  {%modb}.append rate = cr(1)*rate(-1)+(1-cr(1))*(cr(2)*pinf + cr(3)*ygap)
  {%modb}.append ygap = cy(1) * ygap(-1) + (.98-cy(1))*zygap + cy(2) * (rate - zpinf)
  {%modb}.append zpinf = @movav(pinf(-1),4)
  {%modb}.append zygap = @movav(ygap(-1),4)

' equations in expectations errors model

  model {%modf}
  {%modf}.append ezpinf = zpinf - pinf(1)
  {%modf}.append ezygap = zygap - ygap(1)

' coefficients

  coef(2) cy
  cy.fill .50, -.02
  coef(2) cp
  cp.fill .50, .02
  coef(3) cr
  cr.fill .75, 1.5, 0.5 

' set all data to zero
  smpl @all
  %vars = {%modb}.@varlist
  for !i = 1 to @wcount(%vars)
    %tmp = @word(%vars,!i)
    series {%tmp} = 0
    next
  %vars = {%modf}.@varlist
  for !i = 1 to @wcount(%vars)
    %tmp = @word(%vars,!i)
    series {%tmp} = 0
    next

' declare mce variables and instruments
  %instrus = "zpinf_a zygap_a"
  %errs = "ezpinf ezygap"

' assign tracking add factors
  smpl %simstart %simend
  {%modb}.addassign @all
  {%modb}.addinit(v=n) @all
  {%modf}.addassign @all
  {%modf}.addinit(v=n) @all

'************************************************************
'************************************************************
'************************************************************
' Section 3: Simulations

  %sufm = "_1"
  {%modb}.scenario(n,a=%sufm) "multiplier"
  {%modf}.scenario(n,a=%sufm) "multiplier"


' Sim 1:  interest rate shock

  smpl {%simstart} {%simstart}
  rate_a = rate_a + 1

  %mopts = "mod_b=%modb,mod_f=%modf,mce_instrus=%instrus,mce_errs=%errs"
  %aopts = "jinit=interp(4)"
  %sopts = "type=single"
  smpl {%simstart} {%simend}
  call mce_run(%mopts,%aopts,%sopts)
  smpl {%simstart} {%simstart}
  rate_a = rate_a - 1

  series zero = 0
  smpl %simstart %simstart + 39
  graph gr1.line zero rate{%sufm} pinf{%sufm} ygap{%sufm}
  gr1.addtext(t,c,font(14)) "Positive interest rate shock"
  show gr1


' Sim 2:  output gap shock

  smpl {%simstart} {%simstart}
  ygap_a = ygap_a + 1

  %mopts = ""
  %aopts = ""
  %sopts = "type=single"
  smpl {%simstart} {%simend}
  call mce_run(%mopts,%aopts,%sopts)
  smpl {%simstart} {%simstart}
  ygap_a = ygap_a - 1

  series zero = 0
  smpl %simstart %simstart + 39
  graph gr2.line zero rate{%sufm} pinf{%sufm} ygap{%sufm}
  gr2.addtext(t,c,font(14)) "Positive output gap shock"
  show gr2

' Sim 3:  inflation shock

  smpl {%simstart} {%simstart}
  pinf_a = pinf_a + 1

  %mopts = ""
  %aopts = ""
  %sopts = "type=single"
  smpl {%simstart} {%simend}
  call mce_run(%mopts,%aopts,%sopts)
  smpl {%simstart} {%simstart}
  pinf_a = pinf_a - 1

  series zero = 0
  smpl %simstart %simstart + 39
  graph gr3.line zero rate{%sufm} pinf{%sufm} ygap{%sufm}
  gr3.addtext(t,c,font(14)) "Positive inflation shock"
  show gr3



