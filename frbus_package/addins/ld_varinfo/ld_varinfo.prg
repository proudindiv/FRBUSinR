subroutine ld_varinfo(string %pathname)

'Load varinfo information from %pathname in strings

  text vinfo_text
  vinfo_text.append(file) %pathname
  svector varinfo = vinfo_text.@svectornb
  string vinfo_vname = " "
  string vinfo_vtype = " "
  string vinfo_vrule = " "
  string vinfo_sector = " "
  string vinfo_stoch = " "
  string vinfo_decomp = " "
  for !i = 1 to 900
    %y1 = varinfo(!i)
    !ss = @instr(%y1," ")
    %vname = @mid(%y1,!ss+1,8)
    %vname = @rtrim(%vname)
    %vtype = @mid(%y1,!ss+107,1)
    %vrule = @mid(%y1,!ss+109,1)
    if %vrule = " " then
      %vrule = "0"
    endif
    %sector = @mid(%y1,!ss+120,1)
    if %sector = " " then
      %sector = "0"
    endif
    %stoch = @mid(%y1,!ss+128,2)
    %decomp = @mid(%y1,!ss+135,2)
    if %vname = "ZZZBLANK" then
      scalar vinfo_size = !i-1
      exitloop
      endif
   vinfo_vname = vinfo_vname + " " + %vname
   vinfo_vtype = vinfo_vtype + " " + %vtype
   vinfo_vrule = vinfo_vrule + " " + %vrule
   vinfo_sector = vinfo_sector + " " + %sector
   vinfo_stoch = vinfo_stoch + " " + %stoch
   vinfo_decomp = vinfo_decomp + " " + %decomp
  next

endsub

if @len(@option(1)) < 1  then
  @uiprompt("Error: ld_varinfo requires varinfo path")
  stop
endif

%temp = @equaloption("pathname")
if @len(%temp)>0 then
	%pathname = %temp
endif


call ld_varinfo(%pathname)
