' This example illustrates the two optimal policy simulation types
' 
'   - Simulate the effects of a positive shock to the output gap
'     using sequentially 
'
'     (a) the model's interest rate rule
'
'     (b) the opt simulation type to find the optimal interest rate
'     path under commitment
'
'     (c) the opttc simulation type to find the optimal time-consistent
'     or discretionary interest rate path; note that the solution in 
'     this case is only approximate
'
'     In both (b) and (c) the policy instrument is the residual of
'     the interest rule
'
'   - The illustrative loss function penalizes equally weighted,
'     discounted, squared deviations of the output gap,
'     inflation, and the first difference of the interest rate.
'

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
  {%mod}.append rate - rate_aerr = cr(1)*rate(-1)+(1-cr(1))*(cr(2)*pinf + cr(3)*ygap)
  {%mod}.append ygap = cy(1) * ygap(-1) + (.98-cy(1))*ygap(1) + cy(2) * (rate - pinf(1))
  {%mod}.append drate = rate - rate(-1)


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
' Section 3: Optimal policy setup

' targets, instruments
  group opt_instrus rate_aerr
  group opt_targs pinf ygap drate

' desired target trajectories 
  smpl @all
  series pinf_t = 0
  series ygap_t = 0
  series drate_t = 0

' loss function weights
  %discount = ".99"
  %y_weight = "1.0"
  %p_weight = "1.0"
  %r_weight = "1.0"
  smpl @all
  series ygap_w = @val(%y_weight)
  series pinf_w =  @val(%p_weight)
  series drate_w  =  @val(%r_weight)
  !discount = @val(%discount)
  smpl %simstart+1 %simend
  ygap_w = !discount * ygap_w(-1)     
  pinf_w = !discount * pinf_w(-1)     
  drate_w = !discount * drate_w(-1)     


'************************************************************
'************************************************************
'************************************************************
' Section 3: Simulations

  text shock1
  shock1.append smpl {%simstart} {%simstart}
  shock1.append series ygap_a = ygap_a + 3

' run the simulation using the monetary policy rule

  %mopts = "create,mod=%mod,adds,track"
  %aopts = "jinit=linear"
  %sopts = "type= single,txt=shock1,scen,suf=_1"
  smpl {%simstart} {%simend}
  call mce_run(%mopts,%aopts,%sopts)

  smpl @all
  series rate_rule = rate_1
  series pinf_rule = pinf_1
  series ygap_rule = ygap_1


' run the simulation using opt

  %mopts = ""
  %aopts = ""
  %sopts = "type=opt,instrus=opt_instrus,targs=opt_targs"
  smpl {%simstart} {%simend}
  call mce_run(%mopts,%aopts,%sopts)

  smpl @all
  series rate_opt = rate_1
  series pinf_opt = pinf_1
  series ygap_opt = ygap_1


' run the simulation using opttc

  %mopts = ""
  %aopts = ""
  %sopts = "type=opttc,instrus=opt_instrus,targs=opt_targs"
  smpl {%simstart} {%simend}
  call mce_run(%mopts,%aopts,%sopts)

  smpl @all
  series rate_opttc = rate_1
  series pinf_opttc = pinf_1
  series ygap_opttc = ygap_1

'graph

  smpl %simstart %simstart + 39
  series zero = 0
  graph gr1.line zero rate_rule rate_opt rate_opttc
  graph gr2.line zero pinf_rule pinf_opt pinf_opttc
  graph gr3.line zero ygap_rule ygap_opt ygap_opttc
  graph gr4.merge gr1 gr2 gr3
  %title = "Effects of Positive Output Shock Under Three Policy Responses"
  %title = %title + "\r1.  An intertial interest rate rule (_rule)"
  %title = %title + "\r2.  Optimal policy under committment (_opt)"
  %title = %title + "\r3.  Optimal time-consistent policy (_opttc)"
  gr4.addtext(t,c,font(14)) %title
  show gr4
