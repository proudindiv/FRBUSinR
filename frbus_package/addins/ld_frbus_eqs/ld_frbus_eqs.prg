subroutine ld_frbus_eqs(string %mname, string %mpath)

'Create eviews model %mname and load it with the equations for frbus version
'%mname that are in a text file in directory %mpath that was previously created
'by the script eq_docs2eviews.

  %epath = %mpath + %mname
  if @fileexist(%epath) <> 1 then
   %epath = %mpath + %mname  + "_eqs.txt"
  endif 
  delete(noerr) eqtext
  text eqtext
  eqtext.append(file) %epath
  svector eqtextv = eqtext.@svectornb

  model {%mname}

  !eqnum = 0
  %eqcode = " "

  for !i = 1 to 3000
    %y = eqtextv(!i)
    if @isempty(%y) = 0 then
      'string is not blank
      if @left(%y,6) = "theend" then
        %x = @replace(%eqcode," _"," ")
        {%mname}.append {%x}
        exitloop
        endif

      !k = @instr(%y,":")
      if !k > 0 then
        'string contains the start of a new equation
        !eqnum = !eqnum + 1
        if !eqnum > 0 then
          %x = @replace(%eqcode," _"," ")
          {%mname}.append {%x}
          endif
        %eqcode = @mid(%y,!k+1)
        
        else
        'string contains the continuation of an equation
        %eqcode = %eqcode + %y
        endif

      endif
    next

endsub


if @len(@option(1)) < 1  or @len(@option(2)) < 1 then
  @uiprompt("Error: ld_frbus_eqs requires model name and model path")
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

call ld_frbus_eqs(%mname, %mpath)
