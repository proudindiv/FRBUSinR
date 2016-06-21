' This example illustrates:
'
'   - The simulation of a nonlinear model (zero bound imposed)
'     (because the baseline data is set to zero; the zero-bound
'     is set illustratively to -1)
'   - The use of the qnewton algorithm

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
  {%modb}.append rate_u = cr(1)*rate(-1)+(1-cr(1))*(cr(2)*pinf + cr(3)*ygap)
  {%modb}.append rate = @recode( rate_u>rate_min,rate_u,rate_min)
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
' Section 3: Simulation

  %zb = "yes"

  %sufm = "_1"
  {%modb}.scenario(n,a=%sufm) "multiplier"
  {%modf}.scenario(n,a=%sufm) "multiplier"

  if %zb = "yes" then
    smpl @all
    rate_min = -1
    else
    rate_min = -9999
    endif

  smpl {%simstart} {%simstart}
  ygap_a = ygap_a - 5

  %mopts = "mod_b=%modb,mod_f=%modf,mce_instrus=%instrus,mce_errs=%errs"
  %aopts = "meth=qnewton"
  %sopts = "type=single"
  smpl {%simstart} {%simend}
  tic
  call mce_run(%mopts,%aopts,%sopts)
  scalar elapsed = @toc
  show elapsed

  series zero = 0
  smpl %simstart %simstart + 39
  graph gr1.line zero rate{%sufm} pinf{%sufm} ygap{%sufm}
  %title = "Negative Output Shock"
  if %zb = "yes" then
    %title = %title + "\r(zero bound imposed)"
    else
    %title = %title + "\r(zero bound not imposed)"
    endif
  gr1.addtext(t,c,font(14)) %title
  show gr1



