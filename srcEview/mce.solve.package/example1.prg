' This example illustrates:
'
'   - The automated approach to constructing the two operational
'     models
'   - How to define the multiplier shock in a text
'     file whose lines are executed from within the call to
'     mce_run
'   - How to declare the model scenario and the scenario
'     scenario alias within the call to mce_run
'   - The use of the "linear" option of the "newton" algorithm
'     for a linear model in which the maximum endogenous lead and
'     lag is one period

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

' Model name
  %mod = "simple"

' Simulation range
  %simstart = "2001q1"
  %simend = "2025q4"


'************************************************************
'************************************************************
'************************************************************
' Section 2:  Model, coefficients, and data

' equations

  model {%mod}
  {%mod}.append pinf = cp(1) * pinf(-1) + (.98-cp(1))*pinf(1)+ cp(2) * ygap
  {%mod}.append rate = cr(1)*rate(-1)+(1-cr(1))*(cr(2)*pinf + cr(3)*ygap)
  {%mod}.append ygap = cy(1) * ygap(-1) + (.98-cy(1))*ygap(1) + cy(2) * (rate - pinf(1))


' coefficients

  coef(2) cy
  cy.fill .50, -.02
  coef(2) cp
  cp.fill .50, .02
  coef(3) cr
  cr.fill .75, 1.5, 0.5 

' set all data to zero
  smpl @all
  %vars = {%mod}.@varlist
  for !i = 1 to @wcount(%vars)
    %tmp = @word(%vars,!i)
    series {%tmp} = 0
    next


'************************************************************
'************************************************************
'************************************************************
' Section 3: Simulation

  text shock1
  shock1.append smpl {%simstart} {%simstart}
  shock1.append series rate_a = rate_a + 1

  %mopts = "create,mod=%mod,adds,track"
  %aopts = "jinit=linear"
  %sopts = "type=single,txt=shock1,scen,suf=_1"
  smpl {%simstart} {%simend}
  call mce_run(%mopts,%aopts,%sopts)
  copy mce_sim_spool mce_sim_spool_1
  show mce_sim_spool_1

  series zero = 0
  smpl %simstart %simstart + 39
  graph gr1.line zero rate_1 pinf_1 ygap_1
  gr1.addtext(t,c,font(14)) "Positive interest rate shock"
  show gr1



