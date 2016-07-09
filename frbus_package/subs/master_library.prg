'**************************************************************************
'**************************************************************************
'**************************************************************************
subroutine dateshift(string %indate, string %outdate, scalar qtrshift)

' this subroutine takes the quarterly date string in %indate (ie, "2001q1") and shifts 
' it qtrshift quarters, returning the result in %outdate

  !dddd1   = @dateval(%indate,"yyyyfq")
  !dddd2   = @dateadd(!dddd1,qtrshift,"q")
  %outdate = @datestr(!dddd2,"yyyyfq")  

  endsub
  

'**************************************************************************
'**************************************************************************
'**************************************************************************
subroutine group2group(string %fromgroup, string %togroup, string %to_type)

' copies a group of series into another group.
'
' If %to_type = "suffix", then %togroup is interpreted as a suffix to be applied to
' %fromgroup and its series.
'
' If %to_type = "prefix", then %togroup is interpreted as a prefix to be applied to
' %fromgroup and its series.
'
' If %to_type = "group", the %togroup is interpreted as the name of a group that
' already exists.

  if %to_type = "group" then
    if {%fromgroup}.@count <> {%togroup}.@count then 
      statusline ERROR in GROUP2GROUP:  the two groups do not contain the same number of series
      endif
    for !ik1 = 1 to {%fromgroup}.@count
      %tmp = {%fromgroup}.@seriesname(!ik1)
      %tmp1 = {%togroup}.@seriesname(!ik1)
      {%tmp1} = {%tmp}
      next
    endif

  if %to_type = "suffix" then
    %tmpa = " "
    %tmpb = %fromgroup + %togroup
    for !ik1 = 1 to {%fromgroup}.@count
      %tmp = {%fromgroup}.@seriesname(!ik1) + %togroup
      %tmpa = %tmpa + " " + %tmp
      if @isobject(%tmp) then
        {%tmp} = {%fromgroup}(!ik1)
        else
        series {%tmp} = {%fromgroup}(!ik1)
        endif
      next
    group {%tmpb} {%tmpa}
    endif

  if %to_type = "prefix" then
    %tmpa = " "
    %tmpb = %togroup + %fromgroup
    for !ik1 = 1 to {%fromgroup}.@count
      %tmp =  %togroup + {%fromgroup}.@seriesname(!ik1)
      %tmpa = %tmpa + " " + %tmp
      if @isobject(%tmp) then
        {%tmp} = {%fromgroup}(!ik1)
        else
        series {%tmp} = {%fromgroup}(!ik1)
        endif
      next
    group {%tmpb} {%tmpa}
    endif

endsub
'**************************************************************************
'**************************************************************************
'**************************************************************************
subroutine group2zero(string %group)

' set all series in an existin %group to zero

  for !ik1 = 1 to {%group}.@count
    %tmp = {%group}.@seriesname(!ik1)
    {%tmp} = 0
    next

endsub
'**************************************************************************
'**************************************************************************
'**************************************************************************
subroutine groupnames2string(string %group, string %groupnames)

' creates a string of the names of all the series in a group

  %groupnames = " "
  for !ik1 = 1 to {%group}.@count
    %groupnames = %groupnames + " " + {%group}.@seriesname(!ik1)
    next

endsub
'**************************************************************************
'**************************************************************************
'**************************************************************************
subroutine groupnew(string %fromgroup, string %to)

' Creates a new group.  The names of the new group and associated series are built up
' from %fromgroup with %to as a suffix.  Series that do not yet exist are set to zero.

  %tmpa = " "
  %tmpb = %fromgroup + %to
  for !ik1 = 1 to {%fromgroup}.@count
    %tmp = {%fromgroup}.@seriesname(!ik1) + %to
    %tmpa = %tmpa + " " + %tmp
    if @isobject(%tmp) <> 1 then
      series {%tmp} = 0
      endif
    next
  group {%tmpb} {%tmpa}

endsub
'**************************************************************************
'**************************************************************************
'**************************************************************************
subroutine interp_lin(string %series_in, string %series_out, string %substart, string %subend)

' Subroutine that replaces NA values in a series with
' interpolated observations.  NA values at the beginning or end of the series are
' replaced with the first or last non-NA value.

  smpl %substart %subend

  ' ******************************************************
  ' check that series is not all NAs
  series tmp_check = ({%series_in} = NA)
  if @sum(tmp_check) = @obssmpl then
    statusline Error in interp_lin subroutine:  interpolation cannot be performed because series has only NAs in selected sample period
    stop
    endif 

  series tmp_ser = ({%series_in}<>NA)
  series tmp_id = @cumsum(tmp_ser)
  series tmp_next = @sumsby({%series_in},tmp_id(-1))
  series tmp_prev = @sumsby({%series_in},tmp_id)

  ' ******************************************************
  ' check for NAs at either beginning or end of sample

  ' test for NAs at beginning of sample
  series tmp_naprev = (tmp_prev = NA)
  !flag_prev = @max(tmp_naprev)
  if !flag_prev = 1 then
    'at this point, tmp_next will have an undesired NA in its first observation;
    'change it to equal its second observation
    smpl %substart %substart
    call dateshift(%substart,%nextqtr,1)
    tmp_next = @elem(tmp_next,%nextqtr)
    smpl %substart %subend if (tmp_prev = NA)
    tmp_prev = tmp_next
    smpl %substart %subend
    endif

  ' test for NAs at end of sample
  series tmp_nanext = (tmp_next = NA)
  !flag_next = @max(tmp_nanext)
  if !flag_next = 1 then
    smpl %substart %subend if (tmp_next = NA)
    tmp_next = tmp_prev
    smpl %substart %subend
    endif


  ' ******************************************************
  series tmp_lambda = (@obsid-@minsby(@obsid,tmp_id))/@sumsby(1,tmp_id)
  series {%series_out} = tmp_lambda*tmp_next + (1-tmp_lambda)*tmp_prev


  delete tmp_ser tmp_prev tmp_next tmp_lambda tmp_id tmp_nanext tmp_naprev tmp_check

endsub
'**************************************************************************
'**************************************************************************
'**************************************************************************
subroutine set_fp(string dfpxxx)

  %policy_options = "dfpex dfpsrp dfpdbt"
  %dfpxxx = @lower(dfpxxx)
  %dfpxxx = @replace(%dfpxxx," ","")

  !kz = @wfind(%policy_options,%dfpxxx)
  if !kz > 0 then
    for !izzz = 1 to @wcount(%policy_options)
      %ppp = @word(%policy_options,!izzz)
      if !izzz = !kz then
        series {%ppp} = 1
        else
        series {%ppp} = 0
        endif
      next
    else
    %err = %dfpxxx + " is not a valid fiscal policy option; execution terminated"
    @uiprompt(%err)
    stop
    endif

endsub
'**************************************************************************
'**************************************************************************
'**************************************************************************
subroutine set_mp(string dmpxxx)

  %policy_options = "dmpex dmprr dmptay dmptlr dmpintay dmpalt dmpgen"
  %dmpxxx = @lower(dmpxxx)
  %dmpxxx = @replace(%dmpxxx," ","")

  !kz = @wfind(%policy_options,%dmpxxx)
  if !kz > 0 then
    for !izzz = 1 to @wcount(%policy_options)
      %ppp = @word(%policy_options,!izzz)
      if !izzz = !kz then
        series {%ppp} = 1
        else
        series {%ppp} = 0
        endif
      next
    else
    %err = %dmpxxx + " is not a valid monetary policy option; execution terminated"
    @uiprompt(%err)
    stop
    endif

endsub
'**************************************************************************
'**************************************************************************
'**************************************************************************
subroutine set_mpvars2rff

  rfffix = rff
  rfftay = rffe
  rfftlr = rffe
  rffalt = rff
  rffintay = rffe
  rffgen = rffe
  rrfix = rffe - @movav(picxfe,4)

endsub
