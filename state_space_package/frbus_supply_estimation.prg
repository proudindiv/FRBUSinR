' Program to estimate the FRB/US state-space model and
' generate estimates of model states.  


' Subroutines:
'    To transform data
include "./data_transformations"

'    To estimate the model
include "./estimation_code"
include "./initial_values"

close @all
wfcreate kf_data q 1949:1 2020:4

%estend    = "2013q4"
%eststart  = "1963q2" 
%datastart = "1949q1"



sample estsample  %eststart  %estend
sample datasample %datastart %estend
%datasmpl = %datastart + " " + %estend
%modname = "ss_model"


'**********************************************************************************
' Retrieve variables from the database

' Definitions of series in database
'    See FRB/US model documentation for more complete descriptions

' XGDP   - GDP, cw 2009$
' XGDPN  - GDP
' XB   - BLS Business output, 2009$ 
' XBN  - BLS Business output 
' XGDIN  - GDI

' PGDP   - Price index for GDP, cw
' PXB  - BLS Business price 

' LEP    - Employment in business sector (employee and self-employed)
' LHP    - Aggregate labor hours, business sector (employee and self-employed) 
' LUR    - Civilian unemployment rate (break adjusted) 
' LF     - Civilian labor force (break adjusted) 
' N16    - Noninstitutional population, aged 16 and over (break adjusted) 

' KS     - Capital services, 2009 $ 
' LQUALT - Labor quality, trend level
' VEOA   - Average energy-output ratio of existing capital stock 

' PCXFE  - Price index for personal consumption expendits ex. food and energy, cw (NIA def.) 
' PCER   - Price index for personal consumption expenditures on energy (relative to PCXFE) 

' PMO    - Price index for imports ex. petroleum, cw 
' UCES   - Energy share of nominal consumption expenditures 
' EMON   - Imports of goods and services ex. petroleum 
' XGDEN  - Nominal Absorption 
' PTR    - 10-year expected inflation (Hoey/Philadelphia survey) 


%dbin = "state_space_data"
dbopen %dbin as dbin

string varlist = " xgdpn xbn pgdp pxb pcxfe pcer xgdp xb lep uces emon xgden ptr "
varlist = varlist + " ks lqualt veoa lhp lur lf n16 pmo xgdin "

fetch(d=dbin) {varlist}



'**************************************************
' Make model observable series, set priors

smpl datasample

call data_transformations

'**************************************************
' Specify and estimate model, save for re-use

smpl estsample

statusline Estimating model
call ss_estimation

{%modname}.makefilter saved_results



'*************************************************************
' Create states 

{%modname}.makestates(t=pred) *_prs        ' one-step ahead state predictions
{%modname}.makestates(t=filt) *_1          ' filter states
{%modname}.makestates(t=smooth) *_2        ' smoothed states
{%modname}.makestates(t=disturb) *err      ' estimate of the disturbances
{%modname}.makestates(t=filtse) *se        ' RMSE of the filtered states
{%modname}.makestates(t=smoothse) *se2     ' RMSE of the smoothed states

{%modname}.makesignals(t=pred) *_pr        ' one-step ahead signal predictions
{%modname}.makesignals(t=resid) *_res      ' error in one-step ahead signal predictions  
{%modname}.makesignals(t=stdresid) *_sres  ' standardized one-step ahead prediction residual


'*************************************************************
' Transformation into FRB/US mnemonics

series xbt  = exp((tmfp_2/.965 + 0.725*(terate_2 + tlfpr_2  + thtfactor_2 + tww_2) + _
                0.275*lks + 0.725*llqualt + (.035/.965)*lveoa +lpop)/100)
series hmfpt  = gtmfp_2
series qlfpr  = exp(tlfpr_2/100)
series hqlfpr = (gtlfpr_2/400)*qlfpr(-1)
series hqlww  = gtww_2
series huxb   = gtotfactor_2
series leppot = exp((tlfpr_2+terate_2+thtfactor_2)/100) * n16
series lurnat = 100*(1-exp(terate_2/100))
series qlww   = exp(tww_2/100)
series uxbt   = exp(totfactor_2/100)
series xgdpt  = xbt * uxbt
series xgdo   = exp(cycle_2/100)*xgdpt


wfsave saved_results_new


