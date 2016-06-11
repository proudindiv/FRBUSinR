' Program to plot the historical residuals of key FRB/US equations
'
' Each residual has the same units of measurement as the
' the left hand side of its equation

' *************************************************************
' Initial filename and parameter settings
' *************************************************************

' Subroutines
  include ../subs/master_library

' Workfile    
  %wfstart = "1975q1"
  %wfend = "2030q4"
  %mainpage = "main"
  wfcreate(wf=aaa,page={%mainpage}) q {%wfstart} {%wfend}

' FRB/US model name and location
  %varmod = "stdver"
  %varpath = "../mods/"
  %varinfo = "../mods/stdver_varinfo"

' Input datbase
  %dbin  = "../data/longbase"

' Plot range
  %plotstart = "1980q1"
  %plotend = "2014q2"


' ****************************************************************
' Retrieve data, model equations and coefficients
' ****************************************************************

' Load equations and coefficients
  ld_frbus_eqs(modelname=%varmod,modelpath=%varpath)  
  ld_frbus_cfs(modelname=%varmod,modelpath=%varpath)
  ld_varinfo(pathname=%varinfo)

' Load data
  dbopen %dbin as longbase
  smpl %plotstart-12 %plotend
  fetch(d=longbase) *

' Set _aerr variables to zero
  smpl @all
  {%varmod}.makegroup(a,n) endog @endog
  call groupnew("endog","_aerr")
  call group2zero("endog_aerr")

' Compute baseline tracking add factors
  smpl %plotstart %plotend 
  {%varmod}.addassign @all
  {%varmod}.addinit(v=n) @all

' *************************************************************
' Plots
' *************************************************************

  %plotvars = "eco ecd eh epd epi eps ki ex emo lfpr lhp leo lww"
  %plotvars = %plotvars + " picxfe pieci pcer pcfr pcengr"
  %plotvars = %plotvars + " rg5p rg10p rg30p rbbbp rcar rme rtbe rcgain"
  %plotvars = %plotvars + " ynidn yniin yhibn"

  smpl %plotstart %plotend 
  series zero = 0

  spool plot_vars

  !counter = 0
  for !i = 1 to @wcount(%plotvars)
    %vname = @word(%plotvars,!i)
    call find_var_description
    !counter = !counter + 1
    graph gr_{%vname}.line zero {%vname}_a
    %title = %vname + ": " + %desc
    gr_{%vname}.addtext(t) %title
    gr_{%vname}.axis range(minmax)
    gr_{%vname}.options size(4,3)
    gr_{%vname}.legend -display

    plot_vars.append gr_{%vname}
    %index = "000" + @str(!counter)
    if !counter < 100 then
      %index = @right(%index,2)
      else
      %index = @right(%index,3)
      endif
    %name = "untitled" + %index
    plot_vars.name {%name} {%vname}
    next

  plot_vars.display


' *************************************************************
' *************************************************************
  subroutine find_var_description

  for !j = 1 to 500
    %vline = vinfo_text.@line(!j)
    !eq = @instr(%vline,"=")
    !zz = !eq-4
    %name = @lower(@rtrim(@ltrim(@mid(%vline,4,!zz))))

    if %vname = %name then
      %desc = " "
      !ii = !eq + 1
      !kk = @instr(%vline,"sector_")
      if !kk = 0 then
        !kk = @instr(%vline,"X.")
        !jj = !kk - 1 - !ii
        else
        !jj = !kk - 7 - !ii 
        endif
      %desc = @rtrim(@mid(%vline,!ii,!jj))
      exitloop
      endif
    next
  endsub  
