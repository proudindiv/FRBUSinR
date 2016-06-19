subroutine ld_frbus_cfs(string %mname, string %mpath)

'Load coefficients for frbus version %mname from a text file in directory %mpath
'that has been previously created by the script eq_docs2eviews.

  %cpath = %mpath + %mname  + "_coeffs.txt"
  delete(noerr) coefpath
  text coefpath
  coefpath.append(file) %cpath
  svector coefpathv = coefpath.@svectornb

  for !i = 1 to 900
    svector cofname = @wsplit(coefpathv(!i))
    %y1 = cofname(1)
      if @left(%y1,6) = "theend" then
        exitloop
      endif
    %y2 = cofname(2)
    %y3 = cofname(3)
    coef({%y2}) {%y1}
    {%y1}.fill {%y3}
    next

endsub

if @len(@option(1)) < 1  or @len(@option(2)) < 1 then
  @uiprompt("Error: ld_frbus_cfs requires model name and model path")
  stop
endif

%temp = @equaloption("modelname")
if @len(%temp)>0 then
	%mname = %temp
endif
%temp = @equaloption("modelpath")
if @len(%temp)>0 then
	%mpath = %temp
endif

call ld_frbus_cfs(%mname, %mpath)
