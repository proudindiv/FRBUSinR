
var RC RK WC WK YC YK MCC MCK KC  KK PKB R L QK HC  HSC  HK HSK  UHC UHSC UHK UHSK empC  HrC  empK  HrK  empSC  HrSC  empSK  HrSK  unemp  EIK EC INFWC INFWK INFC INFK DIFFNORMGDP NORMINFGDP DIFFREALGDP DIFFREALEC DIFFREALEIK DIFFREALW AH INFGDP INFCNA INFCOR GAP PFGAP INFC10 ECD KD RCD QCD KCH RCH ECH QCH LAGKD LAGKCH UK UC DIFFREALECH DIFFREALECD betas XiL Lpref EFFK MUZK MUZM HG MUC MUK EFFECD EFFECH STAR RL1 RL2 RL3 RL4 RL5 RL6 RL7 RT2
 DIFFREALGDP_obs DIFFREALEC_obs DIFFREALEIK_obs DIFFREALECD_obs DIFFREALECH_obs DIFFREALW_obs AH_obs INFCNA_obs INFCOR_obs INFK_obs R_obs RT2_obs unemp_obs ;


varexo eHG eXiL eLpref eR eMUZK eMUZM ePMKC ePMKK eEFFECH eEFFECD eEFFK eB eSTAR;


parameters
h r_inf r_y r_dy phi_pc phi_H phi_wc phi_ic phi_cd phi_ech gam_pc gam_wc gam_ic gam_icd rho_R rho_B rho_STAR  rho_EFFK
rho_EFFECD rho_HG rho_EFFECH tp2 ONE MUZMSS MUZKSS  r_dinf rpr phi_u rho_MUZK rho_MUZM pbeta delta_ h_cd h_ch delta_cd delta_ch  alpha_ theta_c
theta_k theta_wc theta_wk g_y a_ks s_AS gam_h gam_ech  s_k s_ecdc eta_cnn eta_cd eta_ch
icoef mu_ betarl MUZCSS RCSS RKSS WCSS WKSS YCSS YKSS MCCSS MCKSS KCSS KKSS LSS HCSS HKSS QKSS PKBSS RSS ECSS EIKSS INFCSS INFKSS INFWCSS INFWKSS
MUCSS MUKSS AHSS ECDSS KCDSS QCDSS RCDSS ECHSS KCHSS QCHSS RCHSS UKSS UCSS USS MUKSShabit MUCSShabit
INFCNASS INFCORSS INFC10SS  RT2SS beta_0 beta_2 beta_ PYSS AA DD RR
eta_cd_eta_cnn eta_ch_eta_cnn Rnr ycbi_ykb hc_hk HSS ycbi ykb YYSS s_k_ecd s_c_ech s_k_eik s_yc IMPHSSS INFGDPSS
sig_HG sig_XiL sig_lpref sig_R sig_MUZK sig_MUZM sig_PMKC sig_PMKK sig_EFFECH  sig_EFFECD sig_EFFK sig_B sig_STAR
HSKSS HSCSS HrCSS HrKSS A_HC sigman sigmah A_HK xsi_NC xsi_HrC xsi_NK xsi_HrK rho_XiL rho_lpref //ADDED
empCSS empKSS HrSKSS HrSCSS empSCSS empSKSS UHCSS UHKSS UHSCSS UHSKSS unempSS DIFFREALGDPSS DIFFREALECSS DIFFREALECDSS //ADDED
DIFFREALECHSS DIFFREALEIKSS DIFFREALWSS  RL1SS RL2SS RL3SS RL4SS RL5SS
RL6SS RL7SS DIFFREALGDPSS_obs DIFFREALECSS_obs DIFFREALEIKSS_obs DIFFREALECDSS_obs
DIFFREALECHSS_obs DIFFREALWSS_obs INFCNASS_obs INFCORSS_obs INFKSS_obs
RSS_obs RT2SS_obs unempSS_obs;


//estimated_params;
h		= 0.715162417869797;
r_inf		= 1.46344163969035;
r_y		= 0.263123294207851;
phi_pc		= 3.54471453295450;
phi_H		= 3.22894079106560;
phi_wc		= 5.49395755514723;
phi_ic		= 0.253308786976374;
phi_cd		= 0.470089385005009;
phi_ech		= 9.13986886546163;
gam_pc		= 0.314488926051065;
gam_wc		= -0.230018833252054;
sigman		= 39.4075260618789;
sigmah		= 21.8859803402692;
rho_R		= 0.833200065745674;
rho_XiL		= 0.263567746111198;
rho_lpref	= 0.979092048897712;
rho_B		= 0.895267027146152;
rho_STAR	= 0.909187927454138;
rho_EFFK	= 0.937829274540004;
rho_EFFECD	= -0.240286975088701;
rho_HG		= 0.582395471123139;
rho_EFFECH	= 0.877235725078934;
tp2		= 0.000307314910763576;
sig_HG		= 0.579315931803017;
sig_XiL		= 2.49313873916751;
sig_lpref	= 5.66476748114241;
sig_R		= 0.124100461010359;
sig_MUZK	= 0.936167718269030;
sig_MUZM	= 0.597390920898135;
sig_PMKC	= 0.451830653200989;
sig_PMKK	= 0.685376191952156;
sig_EFFECH	= 0.514704527091087;
sig_EFFECD	= 9.11199585973990;
sig_EFFK	= 0.402779878811407;
sig_B		= 0.295232712196573;
sig_STAR	= 0.104877885500673;
//end_estimated_params;

//calibrated_params;
r_dy = 0;
ONE = 1;
MUZKSS = 1.009250;
MUZMSS = 1.001000;
gam_ic = 1.0;
gam_icd = 1.0;
r_dinf = 0;
rpr = 0.965;
phi_u =	1;
rho_MUZK = 0;
rho_MUZM = 0;
pbeta =	0.99862;
delta_ = 0.03;
h_cd = 0.0;
h_ch = 0.0;
delta_cd = 0.055;
delta_ch = 0.0035;
alpha_ = 0.26;
theta_c = 7;
theta_k = 7;
unempSS = .06;
g_y = 0.0;
a_ks = 0.2;
s_AS = 0.2;
gam_h = 1;
gam_ech = 1;
icoef = 3;
betarl = .958;
//end_calibrated_params;

//free_params;
//A_HC;
//A_HK;
//xsi_NC;
//xsi_HrC;
//xsi_NK;
//xsi_HrK;
//theta_wc;
//theta_wk;
//infkbar;
//infcbar;
//infwcbar;
//infwkbar;
//Pybar;
//Yybar;
//mu_yc;
//mu_yk;
//s_k;
//s_ecdc;
//eta_cnn;
//eta_cd;
//eta_ch;
//mu_;
//end_free_params;

//calibrated ME


//****************************
//MODEL BLOCK
//****************************

model;
RC-MCC*YC/UC/KC(-1)*alpha_*MUK=0;
RK-MCK*YK/UK/KK(-1)*alpha_*MUK=0;
WC-MCC*YC/HC*(1-alpha_)=0;
WK-MCK*YK/HK*(1-alpha_)=0;
YC-(UC*KC(-1)/MUK)^alpha_*(HC)^(1-alpha_)=0;
YK-(UK*KK(-1)/MUK)^alpha_*(HK)^(1-alpha_)=0;
MCC*YC*theta_c-(theta_c-1)*YC-100*phi_pc*(INFC-gam_pc*INFC(-1)-(1-gam_pc)*INFCSS)*INFC*YC+beta_*100*phi_pc*(INFC(+1)-gam_pc*INFC-(1-gam_pc)*INFCSS)*L(+1)/L*INFC(+1)*YC(+1)+100*YCSS*ePMKC=0;
MCK*YK*theta_k/PKB-(theta_k-1)*YK-100*phi_pc*(INFK-gam_pc*INFK(-1)-(1-gam_pc)*INFKSS)*INFK*YK+beta_*100*phi_pc*(INFK(+1)-gam_pc*INFK-(1-gam_pc)*INFKSS)*L(+1)/L*YK(+1)*INFK(+1)+100*YKSS*ePMKK=0;
QK-beta_*(1/EFFK)*(((1-delta_)*QK(+1)+RC(+1)*UC(+1))*L(+1)/MUK(+1)/L)=0;
QK-beta_*(1/EFFK)*(((1-delta_)*QK(+1)+RK(+1)*UK(+1))*L(+1)/MUK(+1)/L)=0;
L-betas*R/rpr/INFC(+1)/MUC(+1)*L(+1)=0;
ln(R/RSS)-rho_R*ln(R(-1)/RSS)-(1-rho_R)*(r_inf*ln(INFCNA/INFCNASS)+r_dinf*(ln(INFCNA)-ln(INFCNA(-1)))+r_y*(ln(PFGAP)))-eR=0;
L-eta_cnn/(EC-h*EC(-1)/MUC)+eta_cnn*beta_*h/(MUC(+1)*EC(+1)-h*EC)=0;
KK-(1-delta_)*KK(-1)/MUK+KC-(1-delta_)*KC(-1)/MUK-1*EIK+mu_*((UK^(1+1/phi_u)-1)/(1+1/phi_u))*KKSS+mu_*((UC^(1+1/phi_u)-1)/(1+1/phi_u))*KCSS=0;

// XXXXXXXXXXXXXXXXXXXXXXXXXXXX
// labor block
// TOTAL LABOR INPUT (called "L" in the paper, I kept the "H" notation of the original EDO prg)
-100+UHC*theta_wc-(theta_wc-1)*WC-100*phi_wc*(INFWC-gam_wc*INFWC(-1)-(1-gam_wc)*INFWCSS)*INFWC*WC+beta_*100*phi_wc*(INFWC(+1)-(gam_wc*INFWC+(1-gam_wc)*INFWCSS))*L(+1)/L*INFWC(+1)*WC(+1)+theta_wc*phi_H/10*(HC/HK-gam_h*HC(-1)/HK(-1)-(1-gam_h)*HCSS/HKSS)+100*XiL=0;
UHSC-WC+phi_H/10*(HSC/HSK-gam_h*HSC(-1)/HSK(-1)-(1-gam_h)*HSCSS/HSKSS);//+100*eXiL=0;
-100+UHK*theta_wk-(theta_wk-1)*WK-100*phi_wc*(INFWK-gam_wc*INFWK(-1)-(1-gam_wc)*INFWKSS)*INFWK*WK+beta_*100*phi_wc*(INFWK(+1)-(gam_wc*INFWK+(1-gam_wc)*INFWKSS))*L(+1)/L*INFWK(+1)*WK(+1)-theta_wc*phi_H/10*(HC/HK-gam_h*HC(-1)/HK(-1)-(1-gam_h)*HCSS/HKSS)+100*XiL=0;
UHSK-WK-phi_H/10*(HSC/HSK-gam_h*HSC(-1)/HSK(-1)-(1-gam_h)*HSCSS/HSKSS);//+100*eXiL=0;
UHC*L*Lpref-A_HC*((1+sigman)/(1+sigman/(1+sigmah)))*(HC)^(-1+(1+sigman)/(1+sigman/(1+sigmah)))=0;
UHSC*L*Lpref-A_HC*((1+sigman)/(1+sigman/(1+sigmah)))*(HSC)^(-1+(1+sigman)/(1+sigman/(1+sigmah)))=0;
UHK*L*Lpref-A_HK*((1+sigman)/(1+sigman/(1+sigmah)))*(HK)^(-1+(1+sigman)/(1+sigman/(1+sigmah)))=0;
UHSK*L*Lpref-A_HK*((1+sigman)/(1+sigman/(1+sigmah)))*(HSK)^(-1+(1+sigman)/(1+sigman/(1+sigmah)))=0;
empC-((1+sigmah)/sigmah*xsi_NC/xsi_HrC)^(-1/(1+sigmah+sigman))*HC^(1/(1+sigman/(1+sigmah)))=0;
HrC-((1+sigmah)/sigmah*xsi_NC/xsi_HrC)^(1/(1+sigmah))*empC^(sigman/(1+sigmah))=0;
empK-((1+sigmah)/sigmah*xsi_NK/xsi_HrK)^(-1/(1+sigmah+sigman))*HK^(1/(1+sigman/(1+sigmah)))=0;
HrK-((1+sigmah)/sigmah*xsi_NK/xsi_HrK)^(1/(1+sigmah))*empK^(sigman/(1+sigmah))=0;
empSC-((1+sigmah)/sigmah*xsi_NC/xsi_HrC)^(-1/(1+sigmah+sigman))*HSC^(1/(1+sigman/(1+sigmah)))=0;
HrSC-((1+sigmah)/sigmah*xsi_NC/xsi_HrC)^(1/(1+sigmah))*empSC^(sigman/(1+sigmah))=0;
empSK-((1+sigmah)/sigmah*xsi_NK/xsi_HrK)^(-1/(1+sigmah+sigman))*HSK^(1/(1+sigman/(1+sigmah)))=0;
HrSK-((1+sigmah)/sigmah*xsi_NK/xsi_HrK)^(1/(1+sigmah))*empSK^(sigman/(1+sigmah))=0;
unemp-(empSC+empSK-(empC+empK))/(empSC+empSK)=0;
PKB-(1-100*phi_ic*(EIK-gam_ic*EIK(-1)-(1-gam_ic)*EIKSS)/(KC(-1)+KK(-1))*MUK)*QK-beta_*(1/EFFK)*100*phi_ic*gam_ic*(EIK(+1)-(gam_ic*EIK+(1-gam_ic)*EIKSS))/(KC+KK)*MUK(+1)*QK(+1)*L(+1)/L=0;
YC-EC-ECH-0.2*YCSS*HG=0;
ln(INFWC)-ln(WC)+ln(WC(-1))-ln(MUC)-ln(INFC)=0;
ln(INFWK)-ln(WK)+ln(WK(-1))-ln(MUC)-ln(INFC)=0;
ln(INFK)-ln(INFC)-ln(PKB)+ln(PKB(-1))+ln(MUK)-ln(MUC)=0;
YK-EIK-ECD-0.2*YKSS*HG=0;
ln(DIFFNORMGDP)-(1-s_k)*(ln(YC)-ln(YC(-1)))-s_k*(ln(YK)-ln(YK(-1)))=0;
ln(NORMINFGDP)-s_k*(ln(PKB)-ln(PKB(-1)))=0;
ln(DIFFREALGDP)-ln(DIFFNORMGDP)-(1-s_k)*ln(MUC)-s_k*ln(MUK)=0;
ln(DIFFREALEC)-ln(EC)+ln(EC(-1))-ln(MUC)=0;
ln(DIFFREALEIK)-ln(EIK)+ln(EIK(-1))-ln(MUK)=0;

// Identities
ln(DIFFREALW)-HCSS/AHSS*(ln(INFWC))-HKSS/AHSS*(ln(INFWK))+ln(INFC)=0;


// XXXXXXXXXXXXXXXXXXXXX
// Aggregate hours equals agg hours in each sector
AH-HC-HK=0;
ln(INFGDP)-ln(INFC)-ln(YC*MUC/YC(-1))+ln(DIFFREALGDP)-ln((1+PKB*YK/YC)/(1+PKB(-1)*YK(-1)/YC(-1)))=0;
ln(INFCNA)-(1-s_ecdc)*ln(INFC)-s_ecdc*ln(INFK)=0;
ln(INFCOR)-(1-s_ecdc)*ln(INFC)-s_ecdc*ln(INFK)=0;
ln(GAP)-(1-s_k)*ln(YC/YCSS)-s_k*ln(YK/YKSS)=0;
ln(PFGAP)-(1-alpha_)*((1-s_k)*ln(HC/HCSS)+s_k*ln(HK/HKSS))-alpha_*((1-s_k)*ln(UC/USS)+s_k*ln(UK/USS))=0;
ln(INFC10)-betarl*ln(INFC10(+1))-(1-betarl)*ln(INFCOR)=0;

// See Section 8: Data Identities

// new equations
// Durable Block

KD-(1-delta_cd)*KD(-1)/MUK-ECD=0;
L*RCD-eta_cd/(KD(-1)/MUK-h_cd*LAGKD(-1)/(MUK(-1)*MUK))+beta_*eta_cd*h_cd/(KD-h_cd*KD(-1)/MUK)=0;
QCD-beta_*(1/EFFECD)*L(+1)/L/MUK(+1)*(RCD(+1)+(1-delta_cd)*QCD(+1))=0;
PKB-QCD*(1-100*phi_cd*(ECD-gam_icd*ECD(-1)-(1-gam_icd)*ECDSS)/KD(-1)*MUK) - beta_*(1/EFFECD)*100*gam_icd*phi_cd*(ECD(+1)-(gam_icd*ECD+(1-gam_icd)*ECDSS))/KD*QCD(+1)*L(+1)/L*MUK(+1)=0;

// Housing Block
L*RCH-eta_ch/(KCH(-1)/MUC-h_ch*LAGKCH(-1)/(MUC*MUC(-1)))+beta_*eta_ch*h_ch/(KCH-h_ch*KCH(-1)/MUC)=0;
QCH-beta_*(1/EFFECH)*L(+1)/L/MUC(+1)*(RCH(+1)+(1-delta_ch)*QCH(+1))=0;
1*ECH+(1-delta_ch)*KCH(-1)/MUC-KCH=0;
1-QCH*(1-100*phi_ech*(ECH-gam_ech*ECH(-1)-(1-gam_ech)*ECHSS)/KCH(-1)*MUC) - beta_*(1/EFFECH)*100*gam_ech*phi_ech*(ECH(+1)-(gam_ech*ECH+(1-gam_ech)*ECHSS))/KCH*QCH(+1)*L(+1)/L*MUC(+1)=0;
ln(KD(-1))-ln(LAGKD)=0;
ln(KCH(-1))-ln(LAGKCH)=0;
RK-QK*mu_*UK^(1/phi_u)=0;
RC-QK*mu_*UC^(1/phi_u)=0;
ln(DIFFREALECH)-ln(MUC)-ln(ECH)+ln(ECH(-1))=0;
ln(DIFFREALECD)-ln(MUK)-ln(ECD)+ln(ECD(-1))=0;
ln(betas/beta_)-rho_B*ln(betas(-1)/beta_)-eB=0;
ln(XiL)-rho_XiL*ln(XiL(-1))-eXiL=0;
ln(Lpref)-rho_lpref*ln(Lpref(-1))-eLpref=0;
ln(EFFK)-rho_EFFK*ln(EFFK(-1))-eEFFK=0;
ln(MUZK/MUZKSS)-eMUZK=0;
ln(MUZM/MUZMSS)-eMUZM=0;
ln(HG)-rho_HG*ln(HG(-1))-eHG=0;
ln(MUC)-ln(MUZM)-alpha_*ln(MUZK)=0;
ln(MUK)-ln(MUZM)-ln(MUZK)=0;
ln(EFFECD)-rho_EFFECD*ln(EFFECD(-1))-eEFFECD=0;
ln(EFFECH)-rho_EFFECH*ln(EFFECH(-1))-eEFFECH=0;
ln(STAR)-rho_STAR*ln(STAR(-1))-eSTAR=0;
ln(RL1) - ln(R(+1))=0;
ln(RL2) - ln(RL1(+1))=0;
ln(RL3) - ln(RL2(+1))=0;
ln(RL4) - ln(RL3(+1))=0;
ln(RL5) - ln(RL4(+1))=0;
ln(RL6) - ln(RL5(+1))=0;
ln(RL7) - ln(RL6(+1))=0;
ln(RT2) - tp2 - 0.125*(ln(R) + ln(RL1) + ln(RL2) + ln(RL3) + ln(RL4) + ln(RL5) + ln(RL6) + ln(RL7)) - ln(STAR)=0;

//measurement_equations;
ln(DIFFREALGDP_obs/DIFFREALGDPSS_obs) = ln(DIFFREALGDP/DIFFREALGDPSS);
ln(DIFFREALEC_obs/DIFFREALECSS_obs)   = ln(DIFFREALEC/DIFFREALECSS);
ln(DIFFREALEIK_obs/DIFFREALEIKSS_obs) = ln(DIFFREALEIK/DIFFREALEIKSS);
ln(DIFFREALECD_obs/DIFFREALECDSS_obs) = ln(DIFFREALECD/DIFFREALECDSS);
ln(DIFFREALECH_obs/DIFFREALECHSS_obs) = ln(DIFFREALECH/DIFFREALECHSS);
ln(DIFFREALW_obs/DIFFREALWSS_obs)     = ln(DIFFREALW/DIFFREALWSS);
ln(AH_obs)	                      = ln(AH/AHSS);
ln(INFCNA_obs/INFCNASS_obs) 	      = ln(INFCNA/INFCNASS);
ln(INFCOR_obs/INFCORSS_obs)	      = ln(INFCOR/INFCORSS);
ln(INFK_obs/INFKSS_obs)	              = ln(INFK/INFKSS);
ln(R_obs/RSS_obs)		      = ln(R/RSS);
ln(RT2_obs/RT2SS_obs)		      = ln(RT2/RT2SS);
ln(unemp_obs/unempSS_obs)	      = ln(unemp/unempSS);
//end_measurement_equations;
end;

varobs DIFFREALGDP_obs DIFFREALEC_obs DIFFREALEIK_obs DIFFREALECD_obs DIFFREALECH_obs DIFFREALW_obs AH_obs INFCNA_obs INFCOR_obs INFK_obs R_obs RT2_obs unemp_obs;

shocks;
var eHG;
stderr sig_HG;
var eXiL;
stderr sig_XiL;
var eLpref;
stderr sig_lpref;
var eR;
stderr sig_R;
var eMUZK;
stderr sig_MUZK;
var eMUZM;
stderr sig_MUZM;
var ePMKC;
stderr sig_PMKC;
var ePMKK;
stderr sig_PMKK;
var eEFFECH;
stderr sig_EFFECH;
var eEFFECD;
stderr sig_EFFECD;
var eEFFK;
stderr sig_EFFK;
var eB;
stderr sig_B;
var eSTAR;
stderr sig_STAR;


var  DIFFREALGDP_obs;
stderr 0.3;
var  DIFFREALEC_obs;
stderr 0.1;
var  DIFFREALEIK_obs;
stderr 1.5;
var DIFFREALECD_obs;
stderr 1.5;
var DIFFREALECH_obs;
stderr 1.5;
var DIFFREALW_obs;
stderr 0.3;
var AH_obs;
stderr 0.3;
var INFCNA_obs;
stderr 0.5;
var INFCOR_obs;
stderr 0.05;
var INFK_obs;
stderr 0.2;
var RT2_obs;
stderr 0.1;
var unemp_obs;
stderr 4;           
end;

steady;

estimated_params;
h		, .673		, -1		, 1	, uniform_pdf   ,,,-1		,1;
r_inf		, 1.461		, -999		, 999	, normal_pdf	, 1.5000        , 0.0625000;
r_y		, 0.214		, -999		, 999	, normal_pdf	, 0.125         , 0.125000;
phi_pc		, 3.126		, 0		, 999	, gamma_pdf	, 4.0000        , 4.0000^.5;
phi_H		, 4.064		, 0		, 999	, gamma_pdf	, 4.0000        , 4.0000^.5;
phi_wc		, 5.119		, 0		, 999	, gamma_pdf	, 4.0000        , 4.0000^.5;
phi_ic		, .325		, 0		, 999	, gamma_pdf	, 4.0000        , 4.0000^.5;
phi_cd		, .651		, 0		, 999	, gamma_pdf	, 4.0000        , 4.0000^.5;
phi_ech		, 10.948	, 0		, 999	, gamma_pdf	, 4.0000        , 4.0000^.5;
gam_pc		, 0.386		, -999		, 999	, normal_pdf	, 0.000         , 0.250;
gam_wc		, 0.213		, -999		, 999	, normal_pdf	, 0.000         , 0.250;
sigman		, 1.25		, 0		, 999	, gamma_pdf	, 1.25          , 12.5^.5;
sigmah		, 10		, 0		, 999	, gamma_pdf	, 10            , 100^.5;
rho_R		, 0.654		, -1		, 1	, normal_pdf	, 0.5           , 0.25;
rho_XiL		, 0.654		, -1		, 1	, normal_pdf	, 0.5           , 0.25;
rho_lpref	, 0.954		, -1		, 1	, normal_pdf	, 0.5           , 0.25;
rho_B		, 0.825		, -1		, 1	, normal_pdf	, 0             , 0.5;
rho_STAR	, 0.825		, -1		, 1	, normal_pdf	, 0             , 0.5;
rho_EFFK	, 0.850		, -1		, 1	, normal_pdf	, 0             , 0.5;
rho_EFFECD	, .230		, -1		, 1	, normal_pdf	, 0             , 0.5;
rho_HG		, 0.596		, 0		, 1	, beta_pdf      , 0.5           , 0.015^.5;
rho_EFFECH	, 0.844		, -1		, 1	, normal_pdf	, 0             , 0.5;
tp2		, 0.001		, -999		, 999	, normal_pdf	, 0.0		, 0.0005;

stderr eHG      , .745		, 0.0001	, 999	, inv_gamma_pdf	, 1.772454	, Inf;
stderr eXiL     , 3.621		, 0.0001	, 999	, inv_gamma_pdf	, 1.772454	, Inf;
stderr eLpref   , 1.621		, 0.0001	, 999	, inv_gamma_pdf	, 1.772454	, Inf;
stderr eR       , 0.165		, 0.0001	, 999	, inv_gamma_pdf	, 0.354491	, Inf;
stderr eMUZK    , .834		, 0.0001	, 999	, inv_gamma_pdf	, 0.443113	, Inf;
stderr eMUZM    , .484		, 0.0001	, 999	, inv_gamma_pdf	, 0.443113	, Inf;
stderr ePMKC    , .391		, 0.0001	, 999	, inv_gamma_pdf	, 0.354491	, Inf;
stderr ePMKK    , .552		, 0.0001	, 999	, inv_gamma_pdf	, 0.354491	, Inf;
stderr eEFFECH  , .526		, 0.0001	, 999	, inv_gamma_pdf	, 1.772454	, Inf;
stderr eEFFECD  , 13.349	, 0.0001	, 999	, inv_gamma_pdf	, 1.772454	, Inf;
stderr eEFFK    , .499		, 0.0001	, 999	, inv_gamma_pdf	, 1.772454	, Inf;
stderr eB       , 0.5		, 0.0001	, 999	, inv_gamma_pdf	, 1.772454	, Inf;
stderr eSTAR	, 0.05		, 0.0001	, 999	, inv_gamma_pdf	, 0.354491	, Inf;
end;


options_.order = 1;
options_.jacobian_flag = 1;
options_.nonlin = 1;

stoch_simul(order=1,irf=40,nograph);

