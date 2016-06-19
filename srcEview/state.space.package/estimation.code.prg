subroutine ss_estimation()

' Subroutine for FRB/US state-space model package to 
' estimate the model parameters.

'**********************************************************************************

' 1. Starting values for parameters.

' All of the coefficients in equations appear as beta or phi.  
' Note that some values that are set here are hard coded in the estimation code below.

call initial_values

'**********************************************************************************
'  Model

sspace {%modname}  ' Declare a new state-space model object

' 2. Output equations.

' In the output equations, trend output is related to the capital stock, energy intensity,
' and trends for labor input using a production function.  The parameters of the production 
' function are hard-coded to the values in the FRB/US model.

' GDP observable
{%modname}.append @signal gdp = totfactor + tmfp/.965 + 0.725*(terate + tlfpr + thtfactor + tww) _
    		      	      + 0.275*lks + 0.725*llqualt + (.035/.965)*lveoa _
    		      	      + cycle + beta(11)*beta(6) + beta(11)*e3p  + rexo
{%modname}.append @state rexo = [var=0.0000001^2]

' Buisines sector product-side observable
{%modname}.append @signal bp = tmfp/.965 + 0.725*(terate + tlfpr  + thtfactor + tww) _
    		      	      + 0.275*lks + 0.725*llqualt + (.035/.965)*lveoa  _
    		      	      + beta(10)*cycle  +   beta(6) + e3p
{%modname}.append @state e3p =  beta(602)*e3p(-1) + re3p
{%modname}.append @ename re3p 'business prod error
{%modname}.append @evar var(re3p) =  (beta(125)^2)

' Business sector income-side observable
{%modname}.append @signal bi =  tmfp/.965  + 0.725*(terate + tlfpr  + thtfactor + tww) _
    		      	      + 0.275*lks + 0.725*llqualt + (.035/.965)*lveoa _
 		      	      + beta(10)*cycle - beta(6) + e3i   
{%modname}.append @state e3i =   beta(602)*e3i(-1) + re3i
{%modname}.append @ename re3i 'business income error
{%modname}.append @evar var(re3i) =  (beta(126)^2)

' 3. Labor equations.
 
' Workweek observable
' To make this equation more consistent with the FRB/US workweek specification,
' a contemporaneous change in the cycle is included as well as the level of the
' cycle.  In addition, the coefficient on the lagged gap term in this equation is
' hard-coded at a value similar to that implicit in the FRB/US model.

{%modname}.append @signal bww =  tww _
	     + phi(20)*(cycle-cycle1) _
             + phi(22)*cycle1 _
	     + 0.82*(bww(-1)-tww1) _	     
	     + eww
{%modname}.append @state eww =  [var=beta(104)^2]

' Employment observable
{%modname}.append @signal eb = terate + tlfpr + thtfactor + _
	     + phi(30)*cycle _
	     + phi(31)*(eb(-1)-(terate1 + tlfpr1 +thtfactor1)) _
	     + eeb
{%modname}.append @state eeb = [var=beta(105)^2]

' Employment rate observable
{%modname}.append @signal erate = terate + phi(50)*cycle _
   		     + phi(51)*(erate(-1)-terate1) _
		     + eerate
{%modname}.append @state eerate =  [var=beta(106)^2]

' Labor force participation observable
{%modname}.append @signal lfpr =  tlfpr + phi(40)*cycle _
   		     + phi(41)*(lfpr(-1)-tlfpr1) _
		     + elfpr
{%modname}.append @state elfpr = [var=beta(107)^2]

' 4. Price observable
        {%modname}.append @ename ec
        {%modname}.append @evar var(ec) = (beta(109)^2)
          {%modname}.append @signal c1pcex = _
	  beta(401)*c1pcex(-1) + (1-beta(401))*(ptr(-1)+.1) _
	+ beta(404)*(((.50*cycle + .33*cycle1 + .17*cycle2 )) )  _
                + beta(405)*@movav(engylag_pcex(-1),6) _
		+ beta(406)*@movav(dum84*engylag_pcex(-1),6)  _
	+ beta(407)*frzbulg _
	+ beta(408)*sw_coreimp_pcex + beta(409)*sw_coreimp_pcex(-1) _
	+ ec
     
' 5. State equations          

' Cycle state
{%modname}.append @state cycle = beta(1)*cycle(-1) + beta(2)*cycle1(-1) + ecycle 
{%modname}.append @state cycle1 = cycle(-1)
{%modname}.append @state cycle2 = cycle1(-1)
{%modname}.append @ename ecycle
{%modname}.append @evar var(ecycle) = (beta(111)^2)

' Trends

'  Totfactor = OSR* in F.R.(2011)
'  tau_oti is taken from FR(2011)
{%modname}.append @state totfactor = totfactor(-1) + 0.25*.95*(gtotfactor(-1) ) + 0.25*.05*beta(213) + _
                                     etotfactor + 0.25*egtotfactor
{%modname}.append @state gtotfactor = .95*gtotfactor(-1) + .05*beta(213) + egtotfactor
{%modname}.append @ename etotfactor
{%modname}.append @evar var(etotfactor) = (beta(112)^2)
{%modname}.append @ename egtotfactor 
{%modname}.append @evar var(egtotfactor) = (4*((tau_oti))*beta(112))^2

' Multi-Factor Productivity (OPH*)
{%modname}.append @state tmfp = tmfp(-1) +  etmfp + 0.25*.95*(gtmfp(-1)) + 0.25*0.05*beta(214) + 0.25*egtmfp
{%modname}.append @state gtmfp = 0.95*gtmfp(-1) + 0.05*beta(214) + egtmfp 
{%modname}.append @ename etmfp
{%modname}.append @evar var(etmfp) = beta(114)^2
{%modname}.append @ename egtmfp
{%modname}.append @evar var(egtmfp) = 0.14^2 ' beta(115)^2

'     For the workweek, employment-sector ratio, and LFPR, the level variances are hard-coded.  
'     This hard-coding is done for convenience in production work.  In particular, the imposed 
'     values are close to the values these parameters take on when they are freely estimated.  
'     However, the t-ratios for these parameters are not very high, and that imprecision leads 
'     to sluggish convergence; imposing these values shortens the time needed for estimation.

' Workweek (WW*)
{%modname}.append @state tww = tww(-1) + 0.25*.95*(gtww(-1) ) +  etww + 0.25*.05*beta(216)+ 0.25*egtww
{%modname}.append @state tww1 = tww(-1)
{%modname}.append @state gtww = .95*gtww(-1) + .05*beta(216) + egtww
{%modname}.append @ename etww
{%modname}.append @ename egtww
{%modname}.append @evar var(etww)  =  0.1^2
{%modname}.append @evar var(egtww) =  beta(117)^2

' Employment Sector Ratio (ESR*)
{%modname}.append @state thtfactor = thtfactor(-1) + 0.25*.95*gthtfactor(-1) + _
                                     0.25*egthtfactor + ethtfactor 
{%modname}.append @state thtfactor1 = thtfactor(-1)
{%modname}.append @state gthtfactor = 0.95*gthtfactor(-1) + egthtfactor
{%modname}.append @ename ethtfactor
{%modname}.append @evar var(ethtfactor) = .01^2 ' beta(118)^2
{%modname}.append @ename egthtfactor 
{%modname}.append @evar var(egthtfactor) = (beta(119)^2)

' Labor Force Participation (LFPR*)
{%modname}.append @state tlfpr = tlfpr(-1) + 0.25*(.95*gtlfpr(-1) + egtlfpr) +  etlfpr 
{%modname}.append @state tlfpr1 = tlfpr(-1)
{%modname}.append @state gtlfpr = 0.95*gtlfpr(-1) + egtlfpr
{%modname}.append @ename etlfpr 
{%modname}.append @evar var(etlfpr) = .05^2 ' beta(122)^2
{%modname}.append @ename egtlfpr 
{%modname}.append @evar var(egtlfpr) = beta(123)^2

' Employment Rate (ER*), no drift
{%modname}.append @state terate = terate(-1) + eterate
{%modname}.append @state terate1 = terate(-1)
{%modname}.append @ename eterate
{%modname}.append @evar var(eterate) = beta(124)^2


{%modname}.append @vprior vpriors

{%modname}.append @mprior mpriors

freeze({%modname}_results) {%modname}.ml(m=100,showopts)

endsub
