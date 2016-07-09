subroutine ld_mce_cfs(string %pfname, string %pfpath, string %mceeqs)

'This subroutine is used for setting up the particular type of frbus simulations
'with model-consistent expectations in which a separate model is created
'containing only those expectations equations chosen to have model-consistent
'solutions.  For those expectations variables, the initial z character is replaced 
'with w.

'Load coefficients for frbus version %pfname from a text file in directory %pfpath
'that has been previously created by the script eq_docs2eviews.  Only those 
'coefficient vectors whose names are in the string %mceeqs are stored.



  %cpath = %pfpath + %pfname  + "_coeffs.txt"
  delete(noerr) coefpathpv
  text coefpathpv
  coefpathpv.append(file) %cpath
  svector coefpathv = coefpathpv.@svectornb

  for !i = 1 to 900
    svector cofname = @wsplit(coefpathv(!i))
    %y1 = cofname(1)
      if @left(%y1,6) = "theend" then
        exitloop
      endif
    %y2 = cofname(2)
    %y3 = cofname(3)
    for !j = 1 to @wcount(%mceeqs)
      %z = @lower(@word(%mceeqs,!j))
      %z1 = @mid(%y1,3)
      if %z = %z1 then
        %y1 = @replace(%y1,"z","w")
        coef({%y2}) {%y1}
        {%y1}.fill {%y3}
        exitloop
      endif
    next
  next


endsub

if @len(@option(1)) < 1  or @len(@option(2)) < 1 or @len(@option(3)) < 1  then
  @uiprompt("Error: ld_mce_cfs requires four parameters: pf model  name, pf model path, mceeqs of selected eqs")
  stop
endif

%temp = @equaloption("pfname")
if @len(%temp)>0 then
	%pfname = %temp
endif
%temp = @equaloption("pfpath")
if @len(%temp)>0 then
	%pfpath = %temp
endif
%temp = @equaloption("mceeqs")
if @len(%temp)>0 then
	%mceeqs = %temp
endif
'    ' Added so that users may ask directly for a group of forward-looking equations instead of 
'    ' passing in a list that may change in the future.
' '   ' Model consistent Asset Pricing
'    %s_mcap = " zdivgr zgap05 zgap10 zpi10f zpic30 zrff10 zrff5 zgap30 zrff30 zpi10 zpib5 "
' '   ' Wages and prices
'    %s_wp = " zpicxfe zpieci "
' '   ' Others - all PAC expectations 
'    %s_other = " zecd zeco zeh zgapc2 zlhp zpi5 zpi10 zpib5 zvpd zvps zvpi zxnfbd zxnfbs zxnfbi zyh zyhp zyht zynid "
'    if %mceeqs = "-all" then
'      %mceeqs = %s_mcap + %s_wp + %s_other
'    endif
'    if %mceeqs = "-mcap" then
'      %mceeqs = %s_mcap
'    endif
'    if %mceeqs = "-wp" then
'      %mceeqs = %s_wp
'    endif
'    if %mceeqs = "-mcap+wp" then
'      %mceeqs = %s_mcap + %s_wp
'    endif
'    if @left(%mceeqs, 7) = "-allbut" then
'      %tmp = %mceeqs
'      %s_remove = @replace(%tmp, "-allbut", "")
'      %mceeqs = @wnotin(%s_mcap + %s_wp + %s_other, %s_remove)
'    endif
'    string zvar_list = %mceeqs
'    scalar nzvars = @wcount(%mceeqs)
'    group zvars {%mceeqs}

call ld_mce_cfs(%pfname, %pfpath, %mceeqs)
