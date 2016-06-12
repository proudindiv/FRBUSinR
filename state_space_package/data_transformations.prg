subroutine data_transformations()

' Subroutine for FRB/US state-space model package to
' transform raw data from EViews database to observables
' used in the estimation of the state-space model.

' 1. Data transformations

series lpop = 100*log(n16)
 
'Business - product side
series nbp = xbn
series bp = log(xb)*100 -lpop

'GDP
series gdp = log(xgdp)*100 -lpop

'Buseness - income side
series dsnst_q = xgdpn - xgdin
series nbi = nbp - dsnst_q
series rbi = nbi/(pxb/100)
series bi = log(rbi)*100 -lpop

' Employment Business sector
series eb = log(lep)*100  - lpop

' Workweek
series hb = log(lhp)*100 - lpop
series bww = hb - eb

' Employment Rate
series erate = 100*log((100-lur)/100)

' Participation Rate
series lfpr = 100*log(lf/n16)

' Variables for TMFP:
series lks = 100*log(ks) - lpop
series llqualt = 100*log(lqualt)
series lveoa = 100*log(veoa)

' Series for the price equation
series c1pcex = @pca(pcxfe)
series engylag_pcex = 0.5*(uces(-1)+uces(-2))*@pca(pcer(-1))
series sw_coreimp_pcex = 0.5*((emon/xgden)+(emon(-1)/xgden(-1)))*@pca(pmo/pcxfe)
series dum84 = @year>=1985
series frzbulg = 0   ''' Nixon wage-price control programs
smpl 1971q3 1974q1
frzbulg = 1
smpl 1974q2 1974q4
frzbulg = -3.666



' For the output-sector ratio, we use the median unbiased approach of Stock 
' and Watson (1998). Thus, tau_oti is the ratio of the variances of the level 
' and drift shocks.

scalar tau_oti = .033260  ' modifier for the drift totfactor (OSR*) error term


'**********************************************************************************

' 2. Prior starting values for states.

'    For mean zero level states, set prior to zero.
'    For other level states, set prior to a data-based
'    	 value near the start of the sample.
'    For most drift terms, set prior to zero.  
'    	 Exception is trend MFP, set equal to sample average.

' Cycle
scalar     icycle  =      0 
scalar     icycle1 =      0 
scalar     icycle2 = 	  0 

' Measurement error
scalar     ie3p = 	  0 
scalar     ie3i =         0 

' Levels of trends
scalar     itotfactor =	        @elem(gdp,%eststart) - @elem(bp,%eststart) 
scalar     itmfp =	       .965*(@elem(bp,%eststart) - 0.725*(@elem(eb,%eststart) _
	   	 	           + @elem(bww,%eststart))  - 0.275*@elem(lks,%eststart) _
				   - 0.725*@elem(llqualt,%eststart) _
				   - (.035/.965)*@elem(lveoa,%eststart))
scalar     itww =	        @elem(bww,%eststart) 
scalar     ithtfactor =         @elem(eb,%eststart) - @elem(erate,%eststart) _
	   	      		- @elem(lfpr,%eststart) 
scalar     itlfpr =	        @elem(lfpr,%eststart) 
scalar     iterate = 	        @elem(erate,%eststart) 

' Initial drift terms
scalar     igtotfactor =        0.0
scalar     igtmfp = 	        1.7
scalar     igtww =	        0.0
scalar     igthtfactor =        0.0
scalar     igtlfpr = 	        0.0

scalar nstate = 25

' The mpriors *must* be in the same order as the states are in the model object
vector(nstate) mpriors
mpriors.fill   0, ie3p, ie3i, 0, 0, _
               0, 0, icycle, icycle1, icycle2, _
	       itotfactor, igtotfactor, itmfp, igtmfp, itww, _
               itww, igtww, ithtfactor, ithtfactor, igthtfactor, _
               itlfpr, itlfpr, igtlfpr, iterate, iterate
     

' Set starting values for variance priors
'     Variance priors set at a high value.  In estimation, variance 
'     drops sharply in early periods of estimation sample.


sym(nstate) vpriors
for !d = 1 to nstate
   vpriors(!d, !d) = 3
   next

' A tighter prior for drift variances,
vpriors(12,12) = 1    ' igtotfactor
vpriors(14,14) = 1    ' igtmfp
vpriors(17,17) = 1    ' igtww
vpriors(20,20) = 1    ' igthtfactor

endsub
