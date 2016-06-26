
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
(RCSS*exp(RC))-(MCCSS*exp(MCC))*(YCSS*exp(YC))/(USS*exp(UC))/(KCSS*exp(KC(-1)))*alpha_*(MUKSS*exp(MUK))=0;
(RKSS*exp(RK))-(MCKSS*exp(MCK))*(YKSS*exp(YK))/(USS*exp(UK))/(KKSS*exp(KK(-1)))*alpha_*(MUKSS*exp(MUK))=0;
(WCSS*exp(WC))-(MCCSS*exp(MCC))*(YCSS*exp(YC))/(HCSS*exp(HC))*(1-alpha_)=0;
(WKSS*exp(WK))-(MCKSS*exp(MCK))*(YKSS*exp(YK))/(HKSS*exp(HK))*(1-alpha_)=0;
(YCSS*exp(YC))-((USS*exp(UC))*(KCSS*exp(KC(-1)))/(MUKSS*exp(MUK)))^alpha_*((HCSS*exp(HC)))^(1-alpha_)=0;
(YKSS*exp(YK))-((USS*exp(UK))*(KKSS*exp(KK(-1)))/(MUKSS*exp(MUK)))^alpha_*((HKSS*exp(HK)))^(1-alpha_)=0;
(MCCSS*exp(MCC))*(YCSS*exp(YC))*theta_c-(theta_c-1)*(YCSS*exp(YC))-100*phi_pc*((INFCSS*exp(INFC))-gam_pc*(INFCSS*exp(INFC(-1)))-(1-gam_pc)*INFCSS)*(INFCSS*exp(INFC))*(YCSS*exp(YC))+beta_*100*phi_pc*((INFCSS*exp(INFC(+1)))-gam_pc*(INFCSS*exp(INFC))-(1-gam_pc)*INFCSS)*(LSS*exp(L(+1)))/(LSS*exp(L))*(INFCSS*exp(INFC(+1)))*(YCSS*exp(YC(+1)))+100*YCSS*ePMKC=0;
(MCKSS*exp(MCK))*(YKSS*exp(YK))*theta_k/(PKBSS*exp(PKB))-(theta_k-1)*(YKSS*exp(YK))-100*phi_pc*((INFKSS*exp(INFK))-gam_pc*(INFKSS*exp(INFK(-1)))-(1-gam_pc)*INFKSS)*(INFKSS*exp(INFK))*(YKSS*exp(YK))+beta_*100*phi_pc*((INFKSS*exp(INFK(+1)))-gam_pc*(INFKSS*exp(INFK))-(1-gam_pc)*INFKSS)*(LSS*exp(L(+1)))/(LSS*exp(L))*(YKSS*exp(YK(+1)))*(INFKSS*exp(INFK(+1)))+100*YKSS*ePMKK=0;
(QKSS*exp(QK))-beta_*(1/(ONE*exp(EFFK)))*(((1-delta_)*(QKSS*exp(QK(+1)))+(RCSS*exp(RC(+1)))*(USS*exp(UC(+1))))*(LSS*exp(L(+1)))/(MUKSS*exp(MUK(+1)))/(LSS*exp(L)))=0;
(QKSS*exp(QK))-beta_*(1/(ONE*exp(EFFK)))*(((1-delta_)*(QKSS*exp(QK(+1)))+(RKSS*exp(RK(+1)))*(USS*exp(UK(+1))))*(LSS*exp(L(+1)))/(MUKSS*exp(MUK(+1)))/(LSS*exp(L)))=0;
(LSS*exp(L))-(beta_*exp(betas))*(RSS*exp(R))/rpr/(INFCSS*exp(INFC(+1)))/(MUCSS*exp(MUC(+1)))*(LSS*exp(L(+1)))=0;
ln((RSS*exp(R))/RSS)-rho_R*ln((RSS*exp(R(-1)))/RSS)-(1-rho_R)*(r_inf*ln((INFCNASS*exp(INFCNA))/INFCNASS)+r_dinf*(ln((INFCNASS*exp(INFCNA)))-ln((INFCNASS*exp(INFCNA(-1)))))+r_y*(ln((ONE*exp(PFGAP)))))-eR=0;
(LSS*exp(L))-eta_cnn/((ECSS*exp(EC))-h*(ECSS*exp(EC(-1)))/(MUCSS*exp(MUC)))+eta_cnn*beta_*h/((MUCSS*exp(MUC(+1)))*(ECSS*exp(EC(+1)))-h*(ECSS*exp(EC)))=0;
(KKSS*exp(KK))-(1-delta_)*(KKSS*exp(KK(-1)))/(MUKSS*exp(MUK))+(KCSS*exp(KC))-(1-delta_)*(KCSS*exp(KC(-1)))/(MUKSS*exp(MUK))-1*(EIKSS*exp(EIK))+mu_*(((USS*exp(UK))^(1+1/phi_u)-1)/(1+1/phi_u))*KKSS+mu_*(((USS*exp(UC))^(1+1/phi_u)-1)/(1+1/phi_u))*KCSS=0;

// XXXXXXXXXXXXXXXXXXXXXXXXXXXX
// labor block
// TOTAL LABOR INPUT (called "(LSS*exp(L))" in the paper, I kept the "H" notation of the original EDO prg)
-100+(UHCSS*exp(UHC))*theta_wc-(theta_wc-1)*(WCSS*exp(WC))-100*phi_wc*((INFWCSS*exp(INFWC))-gam_wc*(INFWCSS*exp(INFWC(-1)))-(1-gam_wc)*INFWCSS)*(INFWCSS*exp(INFWC))*(WCSS*exp(WC))+beta_*100*phi_wc*((INFWCSS*exp(INFWC(+1)))-(gam_wc*(INFWCSS*exp(INFWC))+(1-gam_wc)*INFWCSS))*(LSS*exp(L(+1)))/(LSS*exp(L))*(INFWCSS*exp(INFWC(+1)))*(WCSS*exp(WC(+1)))+theta_wc*phi_H/10*((HCSS*exp(HC))/(HKSS*exp(HK))-gam_h*(HCSS*exp(HC(-1)))/(HKSS*exp(HK(-1)))-(1-gam_h)*HCSS/HKSS)+100*(ONE*exp(XiL))=0;
(UHSCSS*exp(UHSC))-(WCSS*exp(WC))+phi_H/10*((HSCSS*exp(HSC))/(HSKSS*exp(HSK))-gam_h*(HSCSS*exp(HSC(-1)))/(HSKSS*exp(HSK(-1)))-(1-gam_h)*HSCSS/HSKSS);//+100*eXiL=0;
-100+(UHKSS*exp(UHK))*theta_wk-(theta_wk-1)*(WKSS*exp(WK))-100*phi_wc*((INFWKSS*exp(INFWK))-gam_wc*(INFWKSS*exp(INFWK(-1)))-(1-gam_wc)*INFWKSS)*(INFWKSS*exp(INFWK))*(WKSS*exp(WK))+beta_*100*phi_wc*((INFWKSS*exp(INFWK(+1)))-(gam_wc*(INFWKSS*exp(INFWK))+(1-gam_wc)*INFWKSS))*(LSS*exp(L(+1)))/(LSS*exp(L))*(INFWKSS*exp(INFWK(+1)))*(WKSS*exp(WK(+1)))-theta_wc*phi_H/10*((HCSS*exp(HC))/(HKSS*exp(HK))-gam_h*(HCSS*exp(HC(-1)))/(HKSS*exp(HK(-1)))-(1-gam_h)*HCSS/HKSS)+100*(ONE*exp(XiL))=0;
(UHSKSS*exp(UHSK))-(WKSS*exp(WK))-phi_H/10*((HSCSS*exp(HSC))/(HSKSS*exp(HSK))-gam_h*(HSCSS*exp(HSC(-1)))/(HSKSS*exp(HSK(-1)))-(1-gam_h)*HSCSS/HSKSS);//+100*eXiL=0;
(UHCSS*exp(UHC))*(LSS*exp(L))*(ONE*exp(Lpref))-A_HC*((1+sigman)/(1+sigman/(1+sigmah)))*((HCSS*exp(HC)))^(-1+(1+sigman)/(1+sigman/(1+sigmah)))=0;
(UHSCSS*exp(UHSC))*(LSS*exp(L))*(ONE*exp(Lpref))-A_HC*((1+sigman)/(1+sigman/(1+sigmah)))*((HSCSS*exp(HSC)))^(-1+(1+sigman)/(1+sigman/(1+sigmah)))=0;
(UHKSS*exp(UHK))*(LSS*exp(L))*(ONE*exp(Lpref))-A_HK*((1+sigman)/(1+sigman/(1+sigmah)))*((HKSS*exp(HK)))^(-1+(1+sigman)/(1+sigman/(1+sigmah)))=0;
(UHSKSS*exp(UHSK))*(LSS*exp(L))*(ONE*exp(Lpref))-A_HK*((1+sigman)/(1+sigman/(1+sigmah)))*((HSKSS*exp(HSK)))^(-1+(1+sigman)/(1+sigman/(1+sigmah)))=0;
(empCSS*exp(empC))-((1+sigmah)/sigmah*xsi_NC/xsi_HrC)^(-1/(1+sigmah+sigman))*(HCSS*exp(HC))^(1/(1+sigman/(1+sigmah)))=0;
(HrCSS*exp(HrC))-((1+sigmah)/sigmah*xsi_NC/xsi_HrC)^(1/(1+sigmah))*(empCSS*exp(empC))^(sigman/(1+sigmah))=0;
(empKSS*exp(empK))-((1+sigmah)/sigmah*xsi_NK/xsi_HrK)^(-1/(1+sigmah+sigman))*(HKSS*exp(HK))^(1/(1+sigman/(1+sigmah)))=0;
(HrKSS*exp(HrK))-((1+sigmah)/sigmah*xsi_NK/xsi_HrK)^(1/(1+sigmah))*(empKSS*exp(empK))^(sigman/(1+sigmah))=0;
(empSCSS*exp(empSC))-((1+sigmah)/sigmah*xsi_NC/xsi_HrC)^(-1/(1+sigmah+sigman))*(HSCSS*exp(HSC))^(1/(1+sigman/(1+sigmah)))=0;
(HrSCSS*exp(HrSC))-((1+sigmah)/sigmah*xsi_NC/xsi_HrC)^(1/(1+sigmah))*(empSCSS*exp(empSC))^(sigman/(1+sigmah))=0;
(empSKSS*exp(empSK))-((1+sigmah)/sigmah*xsi_NK/xsi_HrK)^(-1/(1+sigmah+sigman))*(HSKSS*exp(HSK))^(1/(1+sigman/(1+sigmah)))=0;
(HrSKSS*exp(HrSK))-((1+sigmah)/sigmah*xsi_NK/xsi_HrK)^(1/(1+sigmah))*(empSKSS*exp(empSK))^(sigman/(1+sigmah))=0;
(unempSS*exp(unemp))-((empSCSS*exp(empSC))+(empSKSS*exp(empSK))-((empCSS*exp(empC))+(empKSS*exp(empK))))/((empSCSS*exp(empSC))+(empSKSS*exp(empSK)))=0;
(PKBSS*exp(PKB))-(1-100*phi_ic*((EIKSS*exp(EIK))-gam_ic*(EIKSS*exp(EIK(-1)))-(1-gam_ic)*EIKSS)/((KCSS*exp(KC(-1)))+(KKSS*exp(KK(-1))))*(MUKSS*exp(MUK)))*(QKSS*exp(QK))-beta_*(1/(ONE*exp(EFFK)))*100*phi_ic*gam_ic*((EIKSS*exp(EIK(+1)))-(gam_ic*(EIKSS*exp(EIK))+(1-gam_ic)*EIKSS))/((KCSS*exp(KC))+(KKSS*exp(KK)))*(MUKSS*exp(MUK(+1)))*(QKSS*exp(QK(+1)))*(LSS*exp(L(+1)))/(LSS*exp(L))=0;
(YCSS*exp(YC))-(ECSS*exp(EC))-(ECHSS*exp(ECH))-0.2*YCSS*(ONE*exp(HG))=0;
ln((INFWCSS*exp(INFWC)))-ln((WCSS*exp(WC)))+ln((WCSS*exp(WC(-1))))-ln((MUCSS*exp(MUC)))-ln((INFCSS*exp(INFC)))=0;
ln((INFWKSS*exp(INFWK)))-ln((WKSS*exp(WK)))+ln((WKSS*exp(WK(-1))))-ln((MUCSS*exp(MUC)))-ln((INFCSS*exp(INFC)))=0;
ln((INFKSS*exp(INFK)))-ln((INFCSS*exp(INFC)))-ln((PKBSS*exp(PKB)))+ln((PKBSS*exp(PKB(-1))))+ln((MUKSS*exp(MUK)))-ln((MUCSS*exp(MUC)))=0;
(YKSS*exp(YK))-(EIKSS*exp(EIK))-(ECDSS*exp(ECD))-0.2*YKSS*(ONE*exp(HG))=0;
ln((ONE*exp(DIFFNORMGDP)))-(1-s_k)*(ln((YCSS*exp(YC)))-ln((YCSS*exp(YC(-1)))))-s_k*(ln((YKSS*exp(YK)))-ln((YKSS*exp(YK(-1)))))=0;
ln((ONE*exp(NORMINFGDP)))-s_k*(ln((PKBSS*exp(PKB)))-ln((PKBSS*exp(PKB(-1)))))=0;
ln((DIFFREALGDPSS*exp(DIFFREALGDP)))-ln((ONE*exp(DIFFNORMGDP)))-(1-s_k)*ln((MUCSS*exp(MUC)))-s_k*ln((MUKSS*exp(MUK)))=0;
ln((DIFFREALECSS*exp(DIFFREALEC)))-ln((ECSS*exp(EC)))+ln((ECSS*exp(EC(-1))))-ln((MUCSS*exp(MUC)))=0;
ln((DIFFREALEIKSS*exp(DIFFREALEIK)))-ln((EIKSS*exp(EIK)))+ln((EIKSS*exp(EIK(-1))))-ln((MUKSS*exp(MUK)))=0;

// Identities
ln((DIFFREALWSS*exp(DIFFREALW)))-HCSS/AHSS*(ln((INFWCSS*exp(INFWC))))-HKSS/AHSS*(ln((INFWKSS*exp(INFWK))))+ln((INFCSS*exp(INFC)))=0;


// XXXXXXXXXXXXXXXXXXXXX
// Aggregate hours equals agg hours in each sector
(AHSS*exp(AH))-(HCSS*exp(HC))-(HKSS*exp(HK))=0;
ln((INFGDPSS*exp(INFGDP)))-ln((INFCSS*exp(INFC)))-ln((YCSS*exp(YC))*(MUCSS*exp(MUC))/(YCSS*exp(YC(-1))))+ln((DIFFREALGDPSS*exp(DIFFREALGDP)))-ln((1+(PKBSS*exp(PKB))*(YKSS*exp(YK))/(YCSS*exp(YC)))/(1+(PKBSS*exp(PKB(-1)))*(YKSS*exp(YK(-1)))/(YCSS*exp(YC(-1)))))=0;
ln((INFCNASS*exp(INFCNA)))-(1-s_ecdc)*ln((INFCSS*exp(INFC)))-s_ecdc*ln((INFKSS*exp(INFK)))=0;
ln((INFCORSS*exp(INFCOR)))-(1-s_ecdc)*ln((INFCSS*exp(INFC)))-s_ecdc*ln((INFKSS*exp(INFK)))=0;
ln((ONE*exp(GAP)))-(1-s_k)*ln((YCSS*exp(YC))/YCSS)-s_k*ln((YKSS*exp(YK))/YKSS)=0;
ln((ONE*exp(PFGAP)))-(1-alpha_)*((1-s_k)*ln((HCSS*exp(HC))/HCSS)+s_k*ln((HKSS*exp(HK))/HKSS))-alpha_*((1-s_k)*ln((USS*exp(UC))/USS)+s_k*ln((USS*exp(UK))/USS))=0;
ln((INFC10SS*exp(INFC10)))-betarl*ln((INFC10SS*exp(INFC10(+1))))-(1-betarl)*ln((INFCORSS*exp(INFCOR)))=0;

// See Section 8: Data Identities

// new equations
// Durable Block

(KCDSS*exp(KD))-(1-delta_cd)*(KCDSS*exp(KD(-1)))/(MUKSS*exp(MUK))-(ECDSS*exp(ECD))=0;
(LSS*exp(L))*(RCDSS*exp(RCD))-eta_cd/((KCDSS*exp(KD(-1)))/(MUKSS*exp(MUK))-h_cd*(KCDSS*exp(LAGKD(-1)))/((MUKSS*exp(MUK(-1)))*(MUKSS*exp(MUK))))+beta_*eta_cd*h_cd/((KCDSS*exp(KD))-h_cd*(KCDSS*exp(KD(-1)))/(MUKSS*exp(MUK)))=0;
(QCDSS*exp(QCD))-beta_*(1/(ONE*exp(EFFECD)))*(LSS*exp(L(+1)))/(LSS*exp(L))/(MUKSS*exp(MUK(+1)))*((RCDSS*exp(RCD(+1)))+(1-delta_cd)*(QCDSS*exp(QCD(+1))))=0;
(PKBSS*exp(PKB))-(QCDSS*exp(QCD))*(1-100*phi_cd*((ECDSS*exp(ECD))-gam_icd*(ECDSS*exp(ECD(-1)))-(1-gam_icd)*ECDSS)/(KCDSS*exp(KD(-1)))*(MUKSS*exp(MUK))) - beta_*(1/(ONE*exp(EFFECD)))*100*gam_icd*phi_cd*((ECDSS*exp(ECD(+1)))-(gam_icd*(ECDSS*exp(ECD))+(1-gam_icd)*ECDSS))/(KCDSS*exp(KD))*(QCDSS*exp(QCD(+1)))*(LSS*exp(L(+1)))/(LSS*exp(L))*(MUKSS*exp(MUK(+1)))=0;

// Housing Block
(LSS*exp(L))*(RCHSS*exp(RCH))-eta_ch/((KCHSS*exp(KCH(-1)))/(MUCSS*exp(MUC))-h_ch*(KCHSS*exp(LAGKCH(-1)))/((MUCSS*exp(MUC))*(MUCSS*exp(MUC(-1)))))+beta_*eta_ch*h_ch/((KCHSS*exp(KCH))-h_ch*(KCHSS*exp(KCH(-1)))/(MUCSS*exp(MUC)))=0;
(QCHSS*exp(QCH))-beta_*(1/(ONE*exp(EFFECH)))*(LSS*exp(L(+1)))/(LSS*exp(L))/(MUCSS*exp(MUC(+1)))*((RCHSS*exp(RCH(+1)))+(1-delta_ch)*(QCHSS*exp(QCH(+1))))=0;
1*(ECHSS*exp(ECH))+(1-delta_ch)*(KCHSS*exp(KCH(-1)))/(MUCSS*exp(MUC))-(KCHSS*exp(KCH))=0;
1-(QCHSS*exp(QCH))*(1-100*phi_ech*((ECHSS*exp(ECH))-gam_ech*(ECHSS*exp(ECH(-1)))-(1-gam_ech)*ECHSS)/(KCHSS*exp(KCH(-1)))*(MUCSS*exp(MUC))) - beta_*(1/(ONE*exp(EFFECH)))*100*gam_ech*phi_ech*((ECHSS*exp(ECH(+1)))-(gam_ech*(ECHSS*exp(ECH))+(1-gam_ech)*ECHSS))/(KCHSS*exp(KCH))*(QCHSS*exp(QCH(+1)))*(LSS*exp(L(+1)))/(LSS*exp(L))*(MUCSS*exp(MUC(+1)))=0;
ln((KCDSS*exp(KD(-1))))-ln((KCDSS*exp(LAGKD)))=0;
ln((KCHSS*exp(KCH(-1))))-ln((KCHSS*exp(LAGKCH)))=0;
(RKSS*exp(RK))-(QKSS*exp(QK))*mu_*(USS*exp(UK))^(1/phi_u)=0;
(RCSS*exp(RC))-(QKSS*exp(QK))*mu_*(USS*exp(UC))^(1/phi_u)=0;
ln((DIFFREALECHSS*exp(DIFFREALECH)))-ln((MUCSS*exp(MUC)))-ln((ECHSS*exp(ECH)))+ln((ECHSS*exp(ECH(-1))))=0;
ln((DIFFREALECDSS*exp(DIFFREALECD)))-ln((MUKSS*exp(MUK)))-ln((ECDSS*exp(ECD)))+ln((ECDSS*exp(ECD(-1))))=0;
ln((beta_*exp(betas))/beta_)-rho_B*ln((beta_*exp(betas(-1)))/beta_)-eB=0;
ln((ONE*exp(XiL)))-rho_XiL*ln((ONE*exp(XiL(-1))))-eXiL=0;
ln((ONE*exp(Lpref)))-rho_lpref*ln((ONE*exp(Lpref(-1))))-eLpref=0;
ln((ONE*exp(EFFK)))-rho_EFFK*ln((ONE*exp(EFFK(-1))))-eEFFK=0;
ln((MUZKSS*exp(MUZK))/MUZKSS)-eMUZK=0;
ln((MUZMSS*exp(MUZM))/MUZMSS)-eMUZM=0;
ln((ONE*exp(HG)))-rho_HG*ln((ONE*exp(HG(-1))))-eHG=0;
ln((MUCSS*exp(MUC)))-ln((MUZMSS*exp(MUZM)))-alpha_*ln((MUZKSS*exp(MUZK)))=0;
ln((MUKSS*exp(MUK)))-ln((MUZMSS*exp(MUZM)))-ln((MUZKSS*exp(MUZK)))=0;
ln((ONE*exp(EFFECD)))-rho_EFFECD*ln((ONE*exp(EFFECD(-1))))-eEFFECD=0;
ln((ONE*exp(EFFECH)))-rho_EFFECH*ln((ONE*exp(EFFECH(-1))))-eEFFECH=0;
ln((ONE*exp(STAR)))-rho_STAR*ln((ONE*exp(STAR(-1))))-eSTAR=0;
ln((RL1SS*exp(RL1))) - ln((RSS*exp(R(+1))))=0;
ln((RL2SS*exp(RL2))) - ln((RL1SS*exp(RL1(+1))))=0;
ln((RL3SS*exp(RL3))) - ln((RL2SS*exp(RL2(+1))))=0;
ln((RL4SS*exp(RL4))) - ln((RL3SS*exp(RL3(+1))))=0;
ln((RL5SS*exp(RL5))) - ln((RL4SS*exp(RL4(+1))))=0;
ln((RL6SS*exp(RL6))) - ln((RL5SS*exp(RL5(+1))))=0;
ln((RL7SS*exp(RL7))) - ln((RL6SS*exp(RL6(+1))))=0;
ln((RT2SS*exp(RT2))) - tp2 - 0.125*(ln((RSS*exp(R))) + ln((RL1SS*exp(RL1))) + ln((RL2SS*exp(RL2))) + ln((RL3SS*exp(RL3))) + ln((RL4SS*exp(RL4))) + ln((RL5SS*exp(RL5))) + ln((RL6SS*exp(RL6))) + ln((RL7SS*exp(RL7)))) - ln((ONE*exp(STAR)))=0;

//measurement_equations;
DIFFREALGDP_obs = DIFFREALGDP + DIFFREALGDPSS_obs;
DIFFREALEC_obs = DIFFREALEC + DIFFREALECSS_obs;
DIFFREALEIK_obs = DIFFREALEIK + DIFFREALEIKSS_obs;
DIFFREALECD_obs = DIFFREALECD + DIFFREALECDSS_obs;
DIFFREALECH_obs = DIFFREALECH + DIFFREALECHSS_obs;
DIFFREALW_obs = DIFFREALW + DIFFREALWSS_obs;
AH_obs = AH;
INFCNA_obs = INFCNA + INFCNASS_obs;
INFCOR_obs = INFCOR + INFCORSS_obs;
INFK_obs = INFK + INFKSS_obs;
R_obs = R + RSS_obs;
RT2_obs = RT2 + RT2SS_obs;
unemp_obs = unemp + unempSS_obs;

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

