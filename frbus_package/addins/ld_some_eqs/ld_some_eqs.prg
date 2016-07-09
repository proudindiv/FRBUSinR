subroutine ld_some_eqs(string %mname, string %mpath, string %eqnames)

' Create eviews model %mname and load it with selected equations for frbus version
' %mname that are in a text file in directory %mpath that was previously created
' by the script eq_docs2eviews.  Only those equations whose names match or do not match the
' equation names in the string %eqnames are included.  If the first "word" in %eqnames is
' "allbut", then all equations but those listed will be included.

'**********************************
'parse equation string and put equation names in a table

  %allbut = "no"

  string zlist = " "
  zlist = zlist + %eqnames
  zlist = @trim(zlist)
     if @isempty(zlist) = 1 then
      @uiprompt("Error:  input string to subroutine load_selected_equtions is empty!!")
      stop
      endif

  if  @wfind(@upper(zlist),"ALLBUT") = 1 then
     %allbut = "yes"
     zlist = @wdrop(zlist,"ALLBUT")
     zlist = @wdrop(zlist,"allbut")
  endif


'**********************************
'parse equation file

  %epath = %mpath + %mname  + "_eqs.txt"
  delete(noerr) eqtext
  text eqtext
  eqtext.append(file) %epath
  svector eqtextv = eqtext.@svectornb
  
  model {%mname}

  !eqnum = 0
  %eqnew = " "
  %eqold = " "
  %eqcode = " "
  %eqcodeold = " "


  for !i = 1 to 3000
    %appendit = "no"
    %exitloop = "no"
    %y = eqtextv(!i)
    if @isempty(%y) = 0 then
      'string is not blank
      if @left(%y,6) = "theend" then
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
        'check whether equation should be added to model
        !zswitch = 0
        for !j = 1 to @wcount(zlist)
          %z = @lower(@word(zlist,!j))
          if %z = %eqold then
            !zswitch = 1
            endif
          if  %allbut = "no" and !zswitch = 1 then
            %x = @replace(%eqcodeold," _"," ")
            {%mname}.append {%x}
            exitloop
            endif
          if  %allbut = "yes" and !j = @wcount(zlist) and !zswitch = 0 then
            %x = @replace(%eqcodeold," _"," ")
            {%mname}.append {%x}
            endif
          next
        endif

      if %exitloop = "yes" then
        exitloop
        endif

      endif
    next



endsub
if @len(@option(1)) < 1  or @len(@option(2)) < 1 or @len(@option(3)) < 1 then
  @uiprompt("Error: ld_some_eqs requires model name and model pathand  and eqnames")
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
%temp = @equaloption("eqnames")
if @len(%temp)>0 then
	%eqnames = %temp
endif

call ld_some_eqs(%mname, %mpath, %eqnames)
