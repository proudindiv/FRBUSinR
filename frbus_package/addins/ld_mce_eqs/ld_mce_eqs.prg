subroutine ld_mce_eqs(string %pfname, string %pfpath, string %mcename, string %mceeqs)

'This subroutine is used for setting up the particular type of frbus simulations
'with model-consistent expectations in which a separate model is created
'containing only those expectations equations chosen to have model-consistent
'solutions.  For those expectations variables, the initial z character is replaced 
'with w.

'Create eviews model %mcename and load it with the equations for frbus version
'%pfname that are in a text file in directory %pfpath that was previously created
'by the script eq_docs2eviews.  Only those equations whose names are in the
'string %mceeqs are included.  

' revised 2/11/13 to add a check that version %pfname contains an equation for each
' name in %mceeqs


  %epath = %pfpath + %pfname  + "_eqs.txt"
  delete(noerr) eqtextp
  text eqtextp
  eqtextp.append(file) %epath
  svector eqtextpv = eqtextp.@svectornb

  %coded = " "
  
  model {%mcename}

  !eqnum = 0
  %eqnew = " "
  %eqold = " "
  %eqcode = " "
  %eqcodeold = " "

  for !i = 1 to 3000
  
    %appendit = "no"
    %exitloop = "no"
    %y = eqtextpv(!i)
    if @isempty(%y) = 0 then
      'string is not blank
      if @left(%y,6) = "theend" then
        'string contains end-of-file flag
        %appendit = "yes"
        %exitloop = "yes"
        %eqold = %eqnew
        %eqcodeold = %eqcode
        endif

      !k = @instr(%y,":")
      if !k > 0 then
        'string contains the start of a new equation
        %appendit = "yes"
        !eqnum = !eqnum + 1
        if !eqnum > 0 then
          %eqold = %eqnew
          %eqcodeold = %eqcode
          endif
        %eqnew  = @left(%y,!k-1)
        %eqcode = @mid(%y,!k+1)
        else
        'string contains the continuation of an equation
        %eqcode = %eqcode + %y
        endif

      if %appendit = "yes" then
        'add equation to model only if it is one with mce expectations
        for !j = 1 to @wcount(%mceeqs)
          %z = @lower(@word(%mceeqs,!j))
          if %z = %eqold then
            %x = @replace(%eqcodeold," _"," ")
            %x = @replace(%x,"z","w")
            {%mcename}.append {%x}
            %coded = %coded + " " + %z
            exitloop
            endif
          next
        endif

      if %exitloop = "yes" then
        exitloop
        endif

      endif
    next

' check that model %mcename contains an equation for each variable in %mceeqs
  !j = @wcount(%mceeqs)
  !k = @wcount(%coded)
  if !j <> !k then
    %z = @wnotin(@lower(%mceeqs),@lower(%coded))
    %errstring = "Error in ld_mce_eqs addin: Model " + %mcename 
    %errstring = %errstring + " does not contain equation(s) for variable(s):  " + %z
    %errstring = %errstring + ".  Execution stopped."
    @uiprompt(%errstring)
    stop
    endif

endsub
if @len(@option(1)) < 1  or @len(@option(2)) < 1 or @len(@option(3)) < 1 or @len(@option(4)) < 1 then
  @uiprompt("Error: ld_mce_eqs requires five parameters: pf model  name, pf model path, mce model name, mce_table of selected eqs and  number of selected eqs")
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
%temp = @equaloption("mcename")
if @len(%temp)>0 then
	%mcename = %temp
endif
%temp = @equaloption("mceeqs")
if @len(%temp) >0 then
	%mceeqs = %temp
endif
'    ' Added so that users may ask directly for a group of forward-looking equations instead of 
'    ' passing in a list that may change in the future.
' '   ' Model consistent Asset Pricing
'    %s_mcap = " zdivgr zgap05 zgap10 zpi10f zpic30 zrff10 zrff5 zgap30 zrff30 zpi10 zpib5 "
' '   ' Wages and prices
'    %s_wp = " zpicxfe zpieci "
' '   ' Others - all PAC expectations 
'    %s_other = " zecd zeco zeh zgapc2 zlhp zpi5 zpi10 zpib5 zvpd zvps zvpi zxbd zxbs zxbi zyh zyhp zyht zynid "
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

call ld_mce_eqs(%pfname, %pfpath, %mcename, %mceeqs)
