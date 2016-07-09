' Changes (1/25/14)
'
' 1.  Removed defaults from mce_load_frbus subroutine
'
' 2.  Added make_frbus_mcevars subroutine
'
' Changes (1/22/14)
'
' 1.  Added code so that the _$_sufsim string variable is assigned the
'     alias of the currently active scenario when the %mopts argument
'     is a null string.
'
' 2.  In subroutine mcz_sim, put the contents of string variable 
'     mcz_sim_options into another string, a change which for unknown
'     reasons eliminates an unexplanined Eviews shutdown when running
'     a pair of simulations of which the first is type=single and the
'     second is type=opt.

' Changes (1/8/14)
'
' 1.  Added the "dontstop" option to the %sopt argument.  When invoked,
'     this option causes the eviews program that calls mce_run
'     to continue executing when running a type=single simulation if
'     (a) the solution iterations do not
'     converge within the maximum number of permitted iterations or
'     (b) the eviews solver generates an error when solving either
'     the backward-looking or forward-looking models in subroutine
'     mcz_solvit.  In the case of nonconvergence the call to mce_run
'     terminates with the string value %mce_finish = "no".  In the
'     case of a solver error, the call to mce_run terminates with the
'     string value %mce_finish = "failed_solve".  Otherwise, 
'     %mce_finish = "yes".  
'
'
' Changes (2/21/12)
'
' 1. Fixed problem with code for unconstrained TC policy
'    calculated via EViews matrices -- the solution at period t
'    (t = 1, ... !ndrv) must go out through period 
'    the farthest period ever solved -- (!nevl + !ndrv - 1) --
'    which requires that the opt derivative 
'    matrix must span this many quarters.  The constrained TC
'
' 2. Wrote code for constrained TC policy in EViews when there is
'    a single instrument
'
' 3. Fixed code for constrained TC policy in R -- still need to do matlab 

' Changes (2/16/12)
'
' 1. Fixed problem with constrained optimization when
'    number of evaluation periods is not the same as
'    the number of instrument periods
'    (subroutine mcz_opt_qp).  This undoes part of the
'    1/17/12 change #3
'
' Changes (2/13/12)
' 
' 1. Added "d=" option for TC damping factor (!tcdamp)

' Changes (1/30/12)
'
' 1. Modified subroutine mca_opt_qp to set options in matlab 
'    quadprog function call
'
' Changes (1/17/12)
'
' 1. Added new subroutine (mcz_opt_tc) and new simtype (opttc)
' 2. Added new keywords:  ideriv, for sopt string
'                /xopen, /xclose for R and matlab
' 3. Modified subroutine mcz_opt_qp so that the dimensions of the 
'    initial and transformed constraint matrices are based on !ndrv not !nevl
' 4. Dropped the explicit optqp simtype
'

' ****************************************************************** '
' ****************************************************************** '
' ******************************************************************
  subroutine mce_run(string m_opts, string a_opts, string s_opts)

' Driver program

  %mcestart = @word(@pagesmpl,1)
  %mceend = @word(@pagesmpl,2)


' ************************************************
' 1. Examine m_opts string (defines or creates models, mce errors and instruments)
' ************************************************
'
' Case 1: null string (ie, "") 
'         => use existing models whose names are contained in %_$_mod_b and
'            %_$_mod_f; use existing objects _$_mce_instrus
'            and _$_mce_errs.
' Case 2: string contains the keywords "create" and "mod=<modname>"
'         => a model named <modname> is in the workfile and contains explicit
'            leads; parse it to create the objects _$_mod_b, _$_mod_f,
'            _$_mce_instrus, and _$_mce_errs
' Case 3: string contains the keywords "mod_b=", "mod_f=", "mce_errs=", "mce_instrus=";
'         or the keywords "mod_b=", "mod_f=", "mce_vars="
'         => each keyword must be assigned to a string variable, whose contents are 
'            used to define _$_mod_b, _$_mod_f, _$_mce_errs, and
'            _$_mce_instrus


  if @isempty(m_opts) = 1 then
  ' ****************************************
  ' case 1 code
    !m_case = 1
    !z1 = @isobject(%_$_mod_b)
    !z2 = @isobject(%_$_mod_f)
    !z3 = @isobject("_$_mce_instrus")
    !z4 = @isobject("_$_mce_errs")
    !zsum = !z1+!z2+!z3+!z4
    if !zsum <> 4 then
      %err = "When the first argument to mce_run is a null string, a previous call to "
      %err = %err + "mce_run must have placed the names of the backword and "
      %err = 5err + "forward looking models, list of mce instruments names "
      %err = %err + "and list of mce error names in various strings; at least one "
      %err = %err + "these strings either does not exist or contains the name of "
      %err = %err + "an object that does not exist.  Execution terminates."
      @uiprompt(%err)
      stop
      endif
    'find alias of active scenario
    %endog_active = {%_$_mod_b}.@endoglist("@active")
    %endog_actual = {%_$_mod_b}.@endoglist
    %word1_active = @word(%endog_active,1)
    %word1_actual = @word(%endog_actual,1)
    !a1 = @strlen(%word1_active)- @strlen(%word1_actual)
    string _$_sufsim = @right(%word1_active,!a1)

    else
    m_opts = @lower(m_opts)
    m_opts = @replace(m_opts," ","")
    m_opts = @replace(m_opts,","," ")
    m_opts = " " + m_opts + " "

    if @instr(m_opts,"create") and @instr(m_opts,"mod=") then
    ' ****************************************
    ' case 2 code
      !m_case = 2
      call mcz_equalopt("mod",m_opts)
      if @len(%temp)>0 then
        %temp1 = @left(%temp,1)
        if %temp1 = "%" then
          %mod  = @lower({%temp})
          else
          %mod  = @lower(%temp)
          endif
        call mcz_parsemod({%mod})
        endif
      else
      !z1 = @instr(m_opts,"mod_b=")
      !z2 = @instr(m_opts,"mod_f=")
      !z3 = @instr(m_opts,"mce_errs=")
      !z4 = @instr(m_opts,"mce_instrus=")
      !z5 = @instr(m_opts,"mce_vars=")
      !zsum1 = (!z1>0)+(!z2>0)+(!z3>0)+(!z4>0) 
      !zsum2 = (!z1>0)+(!z2>0)+(!z5>0) 
      if !zsum1 = 4 or !zsum2 = 3 then
      ' ****************************************
      ' case 3 code
        !m_case = 3
        call mcz_equalopt("mod_b",m_opts)
        if @len(%temp)>0 then
          %temp = @lower({%temp})
          %_$_mod_b = %temp
          endif
        call mcz_equalopt("mod_f",m_opts)
        if @len(%temp)>0 then
          %temp = @lower({%temp})
          %_$_mod_f = %temp
          endif
        call mcz_equalopt("mce_errs",m_opts)
        if @len(%temp)>0 then
          %temp = @lower({%temp})
          group _$_mce_errs {%temp}
          endif
        call mcz_equalopt("mce_instrus",m_opts)
        if @len(%temp)>0 then
          %temp = @lower({%temp})
          group _$_mce_instrus {%temp}
          endif

        else
      ' ****************************************
      ' m_opt string does not conform to a valid case
        @uiprompt("first argument to subroutine mce_run is incorrectly specified")
        stop
        endif
      endif
    endif

  if !m_case = 2 or !m_case = 3 then
    call mcz_hasopt("adds",m_opts)
    if !hasflag = 1 then
      {%_$_mod_b}.addassign @all
      {%_$_mod_f}.addassign @all
      endif
    call mcz_hasopt("track",m_opts)
    if !hasflag = 1 then
      %track = "yes"
      %track_start = @word(@pagesmpl,1)
      %track_end = @word(@pagesmpl,2)
      call mcz_equalopt("tstart",m_opts)
      if @len(%temp)>0 then
        %track_start  = @lower(%temp)
        endif
      call mcz_equalopt("tend",m_opts)
      if @len(%temp)>0 then
        %track_end  = @lower(%temp)
        endif
      smpl %track_start %track_end 
      {%_$_mod_b}.addinit(v=n) @all
      {%_$_mod_f}.addinit(v=n) @all
      endif
    endif

  if !m_case = 2 then
    group _$_mce_errs {{%mod}_targs}
    group _$_mce_instrus {{%mod}_instrus}
    endif

  if !m_case = 3 then
    call mcz_equalopt("mce_vars",m_opts)
    if @len(%temp)>0 then
      %temp = @lower({%temp})
      %errs = @wcross("e",%temp)
      group _$_mce_errs {%errs}
      %instrus = @wcross(%temp,"_a")
      group _$_mce_instrus {%instrus}
      endif
    endif

' ************************************************
' 2.  Examine a_opts string (specifies mce algorithm)
' ************************************************
'
' Case 1:  blank string and %existing_algos = "yes"
'          => do not call mcz_algo (use existing settings)
' Case 2:  blank string and %existing_algos <> "yes"
'          => call mcz_algo to set default options
' Case 3:  nonblank string
'          => call mcz_algo using string to set options overrides

  if @isempty(a_opts) = 1 then
    if %existing_algos = "yes" then
      !a_case = 1
      else
      !a_case = 2
      call mcz_algo(%_$_mod_b,%_$_mod_f," ",_$_mce_instrus,_$_mce_errs)
      endif
    else
    !a_case = 3
    call mcz_algo(%_$_mod_b,%_$_mod_f,a_opts,_$_mce_instrus,_$_mce_errs)
    endif
      

' ************************************************
' 3. Examine s_opts string (specifies type of simulation)
' ************************************************

  if @isempty(s_opts) = 1 then
    @uiprompt("error:  no simulation action requested")
    stop
    endif

' make a copy of s_opts for parsing within this subroutine
' (the original is passed to other subroutines for additional
' parsing)
  string _$_opts = s_opts
  _$_opts = @lower(_$_opts)
  _$_opts = @replace(_$_opts," ","")
  _$_opts = @replace(_$_opts,","," ")
  _$_opts = " " + _$_opts + " "

' check for keywords that pertain to all simulation types
  call mcz_hasopt("scen",_$_opts)
  if !hasflag = 1 then
    call mcz_equalopt("suf",_$_opts)
    if @len(%temp)>0 then
      string _$_sufsim  = @lower(%temp)
      else
      string _$_sufsim = "_1"
      endif
    %sufsim = _$_sufsim
    %scenname = "mce_sim" + _$_sufsim
    {%_$_mod_b}.scenario(n,a=%sufsim) %scenname
    {%_$_mod_f}.scenario(n,a=%sufsim) %scenname
    endif
  call mcz_equalopt("solveopt",_$_opts)
  {%_$_mod_b}.solveopt(o=n,g=12,z=1e-12)
  {%_$_mod_f}.solveopt(o=n,g=12,z=1e-12)
  if @len(%temp)>0 then
    {%_$_mod_b}.solveopt({{%temp}})
    {%_$_mod_f}.solveopt({{%temp}})
    endif
  call mcz_equalopt("txt",_$_opts)
  if @len(%temp)>0 then
    for !j = 1 to {%temp}.@linecount
      %tmp = @lower({%temp}.@line(!j))
      {%tmp}
      next
    endif
  !mceshow = 1 
  call mcz_equalopt("o",_$_opts)
  if @len(%temp)>0 then
    !mceshow = @val(%temp)
    endif
  call mcz_equalopt("sstart",_$_opts)
  if @len(%temp)>0 then
    %mcestart = @lower(%temp)
    endif
  call mcz_equalopt("send",_$_opts)
  if @len(%temp)>0 then
    %mceend = @lower(%temp)
    endif
  smpl %mcestart %mceend
  !nqtrs = @obssmpl
  call mcz_hasopt("cleanup",_$_opts)
  if !hasflag = 1 then
    %cleanup = "yes"
    else
    %cleanup = "no"
    endif
  call mcz_hasopt("dontstop",_$_opts)
  if !hasflag = 1 then
    %dontstop = "yes"
    if @maxerrs - @errorcount < 2 then
      !tt = @errorcount + 2
      setmaxerrs !tt
      endif
    else
    %dontstop = "no"
    endif

' create various program variables and objects that are needed by
' all simulation types; compute initial Jacobian in some cases
  call mcz_sim_setup
  if %mcz_sim_setup = "err" then
    %mce_finish = "failed_solve"
    return
    endif

' determine simulation type 
  call mcz_equalopt("type",_$_opts)
  if @len(%temp) > 0 then
    %simtype = @lower(%temp)
    if %simtype = "single" then
      call mcz_sim(_$_opts)
      if %mcz_sim = "err" then
        %mce_finish = "failed_solve"
        return
        endif
      else
      if %simtype = "opt" or %simtype = "opttc" then
        call mcz_equalopt("targs",_$_opts)
        if @len(%temp)>0 then
          %targs = @lower(%temp)
          else
          @uiprompt("error:  targs keyword is missing")
          stop
          endif
        call mcz_equalopt("instrus",_$_opts)
        if @len(%temp)>0 then
          %instrus = @lower(%temp)
          else
          @uiprompt("error:  instrus keyword is missing")
          stop
          endif
        call mcz_equalopt("cnstr",_$_opts)
        if @len(%temp)>0 then
          %cnstr = @lower(%temp)
          %cnstrflag = "yes"
          else
          text _$_blanktext
          %cnstr = "_$_blanktext"
          %cnstrflag = "no"
          endif
        call mcz_opt_setup(s_opts,{%instrus},{%targs},{%cnstr})
        else
        @uiprompt("error:  invalid simtype")
        stop
        endif
      endif
    else
    @uiprompt("error:  required simtype keyword not found")
    stop
    endif

endsub


'**************************************************************************
'**************************************************************************
'**************************************************************************
subroutine mcz_parsemod(model modo)
'
' This subroutine takes modo, a model with explicit leads, and creates
' four objects.  The name of each created object starts with the name of the
' input model, which is denoted by <modname>.
'
' 1.  Model _$_<modname>_b is the same as modo but with all leads replaced 
'     with exogenous variables
' 2.  Model _$_<modname>_f contains the MCE error equations
' 3.  String <modname>_instrus of the names of the added exogenous variables,
'     which are the instruments to be used to drive the MCE errors to zero
' 4.  String <modname>_targs of the names of the endogenous variables in
'     <modname>_f, which are the names of MCE error variables 

' preliminaries
  %ok_chars = "=-+*(^ "
  freeze(_$_modtext) modo.text
  string _$_endog = @lower(modo.@endoglist)
  !nvars = @wcount(_$_endog)
  %leadnames = " "
  %errnames = " "

  %mm = @lower(modo.@name)
  %_$_mod_b = "_$_" + %mm + "_b"
  %_$_mod_f = "_$_" + %mm + "_f"
  model {%_$_mod_b}
  model {%_$_mod_f}


' create model _$_<modname>_b
  smpl @all
  for !i2 = 1 to _$_modtext.@linecount
      %tmp2 = @ltrim(@lower(_$_modtext.@line(!i2)))
      %aa = @left(%tmp2,1)
      if %aa <> "@" then
          for !i1 = 1 to !nvars
              !occurrence = 1
              %tmp1 = @word(_$_endog,!i1) + "("
    	      !kk = @instr(%tmp2,%tmp1)
    	      if !kk > 0 then
	          while !kk > 0
                      'three possibilities
                      '  1. it is part of another variable name
                      '  2. it is a lag
                      '  3. it is a lead
                      '
                      'look at character in %tmp2 just before %tmp1 to make sure that %tmp1
                      'is not part of a longer variable or coefficient name 
	              %before = @mid(%tmp2, !kk-1,1)
                      if @instr(%ok_chars,%before) > 0 then
                	  %tmp3 = @mid(%tmp2,!kk)
 	           	  !kkll = @instr(%tmp3,"(") 
          	          !kkrr = @instr(%tmp3,")") 
          	          %laglead = @mid(%tmp3,!kkll, !kkrr-!kkll+1)
          	          !ll = 0 + {%laglead}
                          if !ll <= 0 then   'it is a lag -- skip it
                             !occurrence = !occurrence + 1
                             endif
		          if !ll > 0 then   'it is a lead -- define a new variable 
            		      %aaa = @word(_$_endog,!i1) + %laglead
            		      %bbb = @word(_$_endog,!i1) + "_ld_" + @str(!ll)
                              series {%bbb} = {%aaa}
            		      %tmp4 = @replace(%tmp2,%aaa,%bbb,1)
            		      %tmp2 = %tmp4
                              %leadnames = %leadnames + " " + %bbb
		  	      endif
                          else    'it is part of another variable name -- skip it
                          !occurrence = !occurrence + 1
                          endif
		      !kk = @instr(%tmp2,%tmp1,!occurrence)
		      wend
	          endif
	      next
          endif   
        {%_$_mod_b}.append {%tmp2}
      next

' create _$_<modname>_f
  %leadnames = @wunique(%leadnames)
  'smpl @all
  for !i1 = 1 to @wcount(%leadnames)
      %tmp1 = @word(%leadnames,!i1)
      !k1 = @instr(%tmp1,"_ld_")
      %tmp2 = @left(%tmp1,!k1-1)
      %tmp3 = @mid(%tmp1,!k1+4)
      %tmp4 =  "err_" + %tmp2 + "_" + @str(%tmp3)
      series {%tmp4} = 0
      %errnames = %errnames + " " + %tmp4
      %eqstring = %tmp4 + "=" + %tmp1 + "-" + %tmp2 + "(" + @str(%tmp3) + ")"
      {%_$_mod_f}.append {%eqstring}
      next

' create mce instrument and target strings

  string {%mm}_instrus = %leadnames
  string {%mm}_targs = %errnames


endsub



' ****************************************************************** '
' ****************************************************************** '
' ******************************************************************
  subroutine mcz_algo(string mcz_mod_b, string mcz_mod_f, string mcz_algo_opts, group mcz_instrus, group mcz_errs)

  %existing_algos = "yes"

  mcz_algo_opts = @lower(mcz_algo_opts)
  mcz_algo_opts = @replace(mcz_algo_opts," ","")
  mcz_algo_opts = @replace(mcz_algo_opts,","," ")
  mcz_algo_opts = " " + mcz_algo_opts + " "

' default values for method options
  %meth = "newton"
  %jinit = "interp"
  !nskip = 12
  !jtrigger = .5

' default values for linesearch options
  %linemeth = "armijo"
  !linetrigger  = .9
  !mcelinemax = 10
  !lambda = 1.0
  !lrat = .5
  !mcz_step_max = 1.0

' default values for other options
  !mceconv = .00001
  !mcemaxiter = 20
  !mceptrb = .001
  !broymax = 600

' just in case
  %terminal = "no"

' are there overrides to defaults?
  if @len(mcz_algo_opts) > 0 then

  ' **********************************
  ' look for method option
    call mcz_equalopt("meth",mcz_algo_opts)
    if @len(%temp)>0 then
      %meth  = @lower(%temp)
      if %meth = "broy" then
        %jinit = "bd"
        %jupdate = "na"
        %linemeth = "lmr"
        !mcemaxiter = 200
        endif
      if %meth = "qnewton" then
        %jinit = "bd"
        %jupdate = "na"
        %linemeth = "lmr"
        !mcemaxiter = 200
        call mcz_equalopt("broymax",mcz_algo_opts)
        if @len(%temp)>0 then
          !broymax = @val(%temp)
          endif
        endif
      if %meth = "ft" then
        %jinit = "identity"
        %jupdate = "na"
        %linemeth = "na"
        !mcemaxiter = 500
        endif
      endif

  ' **********************************
  ' look for jinit and jt options
    call mcz_equalopt("jinit",mcz_algo_opts)
    if @len(%temp)>0 then
      %temp = @lower(%temp)
      if @instr(%temp,"interp(") then
        !k1 = @instr(%temp,"(")
        !nskip = -@val(@mid(%temp,!k1))
        %tmp1 = @mid(%temp,!k1)
        %jinit = "interp"
        else
        %jinit = @lower(%temp)
        endif
      endif
    call mcz_equalopt("jt",mcz_algo_opts)
    if @len(%temp)>0 then
      !jtrigger  = @val(%temp)
      endif

  ' **********************************
  ' look for jupdate option
    %jupdate = %jinit
    call mcz_equalopt("jupdate",mcz_algo_opts)
    if @len(%temp)>0 then
      %temp = @lower(%temp)
      if @instr(%temp,"interp(") then
        !k1 = @instr(%temp,"(")
        !nskip = -@val(@mid(%temp,!k1))
        %tmp1 = @mid(%temp,!k1)
        %jupdate = "interp"
        else
        %jupdate = @lower(%temp)
        endif
      endif

  ' **********************************
  ' look for options related to linesearch
    call mcz_equalopt("lmeth",mcz_algo_opts)
    if @len(%temp)>0 then
      %linemeth  = @lower(%temp)
      endif
    call mcz_equalopt("lt",mcz_algo_opts)
    if @len(%temp)>0 then
      !linetrigger = @val(%temp)
      endif
    call mcz_equalopt("lmax",mcz_algo_opts)
    if @len(%temp)>0 then
      !mcelinemax = @val(%temp)
      endif
    call mcz_equalopt("lambda",mcz_algo_opts)
    if @len(%temp)>0 then
      !lambda = @val(%temp)
      endif
    call mcz_equalopt("stepmax",mcz_algo_opts)
    if @len(%temp)>0 then
      !mcz_step_max = @val(%temp)
      endif


  ' **********************************
  ' look for other options
    call mcz_equalopt("c",mcz_algo_opts)
    if @len(%temp)>0 then
      !mceconv = @val(%temp)
       endif
    call mcz_equalopt("m",mcz_algo_opts)
    if @len(%temp)>0 then
      !mcemaxiter = @val(%temp)
      endif
    call mcz_equalopt("p",mcz_algo_opts)
    if @len(%temp)>0 then
      !mceptrb = @val(%temp)
      endif

    endif


' **********************************
' verify the MCE instrument and error arguments

  !nmceinstrus = _$_mce_instrus.@count
  !nmcetargs = _$_mce_errs.@count

  if !nmcetargs <> !nmceinstrus then
    @uiprompt("Error:  There must be as many mce errors as there are mce instruments.")
    stop
    endif

' **********************************
' check that mce instruments are exogenous variables or add factors
' in the lag model
  %exog_vnames = {%_$_mod_b}.@exoglist
  %adds_vnames = {%_$_mod_b}.@addfactors
  %exog_vnames = %exog_vnames + " " + %adds_vnames
  %endog_vnames = {%_$_mod_b}.@endoglist
  for !i = 1 to _$_mce_instrus.@count
    %vvv = _$_mce_instrus.@seriesname(!i)
    !cc = @wfindnc(%exog_vnames,%vvv)
    if !cc = 0 then
      %errstring = "mce control variable " + %vvv + " is not an exogenous variable or add factor"
      @uiprompt(%errstring)
      stop
      endif
    next

' **********************************
' check that mce errors are endogenous variables in the lead model
  %endog_lnames = {%_$_mod_f}.@endoglist
  for !i = 1 to _$_mce_errs.@count
    %vvv = _$_mce_errs.@seriesname(!i)
    !cc = @wfindnc(%endog_lnames,%vvv)
    if !cc = 0 then
      %errstring = "mce error variable " +  %vvv + " is not an endogenous variable in the lead model"
      @uiprompt(%errstring)
      stop
      endif
    next

endsub


 
' ************************************************************
' ************************************************************
' ************************************************************
  subroutine mcz_equalopt(string optionstext,string opts)

' parse an option that contains an "=" sign
  optionstext = " " + optionstext + "="
  !k10 = @instr(opts,optionstext)
  if !k10 > 0 then
    !k11 = @len(optionstext)
    %tmp10 = @mid(opts,!k10+!k11)
    !k12 = @instr(%tmp10," ")
    %temp = @left(%tmp10,!k12-1)
    else
    %temp = ""
    endif

  endsub    

 
' ************************************************************
' ************************************************************
' ************************************************************
  subroutine mcz_hasopt(string optionstext,string opts)

' parse an option that does not contain an "=" sign
  !k10 = @wfind(opts,optionstext)
  if !k10 > 0 then
    !hasflag = 1
    else
    !hasflag = 0
    endif

  endsub    


' ******************************************************************
' ******************************************************************
' ******************************************************************
' ******************************************************************
  subroutine mcz_sim_setup

' create various program variables, strings, matrices, vectors, and tables
' that are common to all simulation types

  !tmcetargs = !nmcetargs * !nqtrs
  !tmceinstrus = !nmceinstrus * !nqtrs
  %mce_targ_names = _$_mce_errs.@members
  %mce_instru_names = _$_mce_instrus.@members

  string _$_mod_f_exog = @lower({%_$_mod_f}.@exoglist)
  string _$_mod_b_endog = @lower({%_$_mod_b}.@endoglist)
  %fvars = @wintersect(_$_mod_f_exog,_$_mod_b_endog)
  {%_$_mod_f}.override {%fvars}

  !re_counter = 0


' **************************************************************************
' create additional matrix/vector objects

  vector(!mcemaxiter+1) _$_mce_loss_vec
  vector(!mcemaxiter+1) _$_mce_conv_vec
  vector( !nmceinstrus*!nqtrs) _$_mce_direction = 0
  vector( !nmceinstrus*!nqtrs) _$_mce_instru_vec = 0
  vector(!nmcetargs*!nqtrs) _$_mce_gap_vec 
  matrix _$_mce_ptrb_mat = @filledmatrix(!nqtrs,!nmceinstrus,!mceptrb)
  matrix(!nmcetargs*!nqtrs,1) _$_mce_targ_vec 
  matrix(!tmcetargs,1) _$_mce_targ_dvec 


' **************************************************************************
'  misc

  table(!mcemaxiter+2,6) mce_sim_stats


' **************************************************************************
' compute initial jacobian except when it is an identity matrix
' or when its been created by a previous call to mce_run
  if %jinit <> "identity" and !a_case <> 1 then
    !mcetry = 1
    smpl %mcestart %mceend
    call mcz_solvit
    if %mcz_solvit = "err" then
      %mcz_sim_setup = "err"
      return
      endif

    call mcz_derivs
    endif


  endsub


' ****************************************************************** '
' ****************************************************************** '
' ******************************************************************
  subroutine mcz_sim(string mcz_sim_options)

  %mcz_sim = "ok"

' ************************************************
' 1. set options based on defaults and overrides in string mcz_sim_options
' ************************************************

' for some unknown reason, sometimes eviews bombs unless mcz_sim_options
' is assigned to another string before processing it
  %mso = mcz_sim_options

  %mso = @lower(%mso)
  %mso = @replace(%mso," ","")
  %mso = @replace(%mso,","," ")
  %mso = " " + %mso + " "

  %terminal = "no"
  %mcevars_b = " "
  %mcevars_f = " "

  if @len(%mso) > 0 then
    call mcz_hasopt("terminal",%mso) 
    if !hasflag = 1 then
      %terminal = "yes"
      call mcz_equalopt("mcevars_b",%mso)
      if @len(%temp)>0 then
        %mcevars_b  = @lower(%temp)
        call mcz_equalopt("mcevars_f",%mso)
        if @len(%temp)>0 then
          %mcevars_f  = @lower(%temp)
          if @wcount(%mcevars_b) <> @wcount(%mcevars_f) then
            %estring = "Error: mcevars_b and mcevars_f have different numbers of variables"
            @uiprompt(%estring)
            stop
            endif 
          endif
        endif
      endif
    endif

  if %linemeth = "lmr" then
    !mhistory = 4
    !tmin = .1
    !tmax = .5
    !gammak = 10^(-4)
    endif


' ************************************************
' 2. set up table of iteration-by-iteration statistics
' ************************************************

  if @isobject("mce_sim_stats") then
    delete mce_sim_stats
    endif
  table(!mcemaxiter+2,6) mce_sim_stats
  mce_sim_stats.setwidth(1:1) 6
  mce_sim_stats.setwidth(2:6) 11
  mce_sim_stats.setlines(a2:f2) +b
  setcell(mce_sim_stats,1,1,"iter")
  setcell(mce_sim_stats,1,2,"converge")
  setcell(mce_sim_stats,2,2,"stat")
  setcell(mce_sim_stats,1,3,"SSR")
  setcell(mce_sim_stats,2,3,"stat")
  setcell(mce_sim_stats,1,4,"step")
  setcell(mce_sim_stats,2,4,"length")
  setcell(mce_sim_stats,1,5,"step")
  setcell(mce_sim_stats,2,5,"iters")
  setcell(mce_sim_stats,1,6,"Newton MCE")
  setcell(mce_sim_stats,2,6,"deriv's?")


' ************************************************
' 3. information text file
' ************************************************

  if @isobject("mce_sim_text") then
    delete mce_sim_text
    endif
  text mce_sim_text
  mce_sim_text.append Simulation start = {%mcestart}
  mce_sim_text.append Simulation end   = {%mceend}
  mce_sim_text.append MCE method = %meth
  if %meth = "newton" then
    mce_sim_text.append -- Initial jacobian = %jinit
    if %jinit = "interp" then
      mce_sim_text.append ---- Jacobian interpolation parameter = {!nskip}
      endif
    mce_sim_text.append  -- Recompute Jacobian based on jtrigger = {!jtrigger}
    mce_sim_text.append  -- Recompute jacobian using method = {%jupdate}
    endif
  if %meth = "broy" or %meth = "qnewton" then
    mce_sim_text.append -- Initial Jacobian approximation = %jinit
    if %jinit = "interp" then
      mce_sim_text.append ---- Interpolation parameter = {!nskip}
      endif
    if %meth = "qnewton" then
      mce_sim_text.append ---- QNewton iteration switch = {!broymax}
      endif
    endif
  if %meth = "ft" then
    mce_sim_text.append -- Fixed step size =   {!lambda}
    endif
  mce_sim_text.append Linesearch method = {%linemeth}
  if %linemeth <> "na" then
    mce_sim_text.append -- Linesearch trigger = {!linetrigger}
    mce_sim_text.append -- Maximum linesearch iterations =   {!mcelinemax}
    endif
  mce_sim_text.append Convergence criteria = {!mceconv}
  mce_sim_text.append Maximum number of MCE iterations = {!mcemaxiter}
  mce_sim_text.append MCE instrument perturbation factor = {!mceptrb}
  mce_sim_text.append Intermediate output level factor = {!mceshow}

  mce_sim_text.append MCE instrument variables  = {%mce_instru_names}
  mce_sim_text.append MCE error variables  = {%mce_targ_names}
  mce_sim_text.append There are {!tmceinstrus} instrument and {!tmcetargs} error observations

  !re_counter = !re_counter + 1

' ************************************************
' 4. solution iterations
' ************************************************

  '***********************************
  ' initialize counters, switches, etc.

  !mcetry = 0
  %mce_converge = "no"
  smpl %mcestart %mceend
  _$_mce_instru_vec = @vec(@convert(_$_mce_instrus))

  '***********************************
  ' start of iteration loop

  while !mcetry <= !mcemaxiter and %mce_converge = "no"

    !mcetry = !mcetry + 1
    setcell(mce_sim_stats,!mcetry+2,1,!mcetry-1,0)

    vector _$_instru_prev = _$_mce_instru_vec
    vector _$_gap_prev = _$_mce_gap_vec

    !mcz_step = !mcz_step_max
    call mcz_solvit
    if %mcz_solvit = "err" then
      %mcz_sim = "err"
      return
      endif

    if !mcetry > 1 then
      !gamma =  _$_mce_loss_vec(!mcetry)/_$_mce_loss_vec(!mcetry-1)
      !loss_prev = _$_mce_loss_vec(!mcetry-1)
      setcell(mce_sim_stats,!mcetry+2,5,1,0)
      if %linemeth <> "none" and !gamma > !linetrigger then
        if %linemeth = "lmr" then
          call mcz_lmr
          if %mcz_lmr = "err" then
            %mcz_sim = "err"
            return
            endif
          endif
        if %linemeth = "armijo"  then
          call mcz_armijo
          if %mcz_armijo = "err" then
            %mcz_sim = "err"
            return
            endif
          endif
        endif
      endif

    if !mceshow < 3 then
      statusline mce solution, iteration !mcetry, f(x) = !nconv
      endif

  ' test for convergence or iteration limit
    mce_sim_stats(!mcetry+2,2) =  _$_mce_conv_vec(!mcetry)
    if _$_mce_conv_vec(!mcetry) < !mceconv then
      %mce_converge = "yes"
      mce_sim_text.append At iteration {!mcetry}, convergence
      %mce_finish = "yes"
      endif
    if !mcetry = !mcemaxiter and  _$_mce_conv_vec(!mcetry) >= !mceconv then
      mce_sim_text.append No convergence in {!mcemaxiter} iterations
      if %dontstop = "yes" then
        %mce_finish = "no"
        %mce_converge = "yes"
        mce_sim_text.append Terminating call to mce_run, but execution continues
        else
        @uiprompt("No convergence in maximum number of iterations.  Execution terminating.")
        stop
        endif
      endif


  ' continue if not converged 
    if %mce_converge = "no" then

    ' ******************************
    ' Newton algorithm (optionally update MCE jacobian)
      if %meth = "newton" then
        if !mcetry = 1 then
          if %jinit = "identity" then
            matrix _$_mce_der_mat = !dfactor*@identity(!nmceinstrus*!nqtrs)
            endif
          if %jinit = "bd" then
            for !ijj = 1 to !nmcetargs
              !r = (!ijj-1)*!nqtrs
              matrix _$_mce_der_mat_{!ijj} = @subextract(_$_mce_der_mat,!r+1,!r+1,!r+!nqtrs,!r+!nqtrs)
              next
            delete(noerr) _$_mce_der_mat
            endif
          if %jinit <> "identity" then
            mce_sim_stats(!mcetry+3,6) = "yes"
            endif
          else
          if (!gamma >= !jtrigger and %jupdate <> "none") then
            %jinit_bac = %jinit
            %jinit = %jupdate
            call mcz_derivs
            %jinit = %jinit_bac
            endif
          endif

        if %jinit = "bd" then 
          for !ijj = 1 to !nmcetargs
            vector _$_vec_adds = -_$_mce_der_mat_{!ijj}*@subextract(_$_mce_gap_vec,(!ijj-1)*!nqtrs+1,1,!ijj*!nqtrs,1)
            matplace(_$_mce_direction,_$_vec_adds,(!ijj-1)*!nqtrs+1,1)
            next
          else
          _$_mce_direction = -(_$_mce_der_mat*_$_mce_gap_vec)
          endif

        endif       

    ' ******************************
    ' Broyden algorithms
      if %meth = "broy" or %meth = "qnewton" then
        if !mcetry = 1 then
          if %jinit <> "identity" then
            _$_mce_direction = -(_$_mce_der_mat*_$_mce_gap_vec)
            else
            matrix _$_mce_der_mat = @identity(!nmceinstrus*!nqtrs)
            _$_mce_direction = -_$_mce_gap_vec
            endif
          if %jinit = "bd" then
            for !ijj = 1 to !nmcetargs
              !r = (!ijj-1)*!nqtrs
              matrix _$_mce_der_mat_{!ijj} = @subextract(_$_mce_der_mat,!r+1,!r+1,!r+!nqtrs,!r+!nqtrs)
              next
            if %bmeth = "qnewton" then
              delete(noerr) _$_mce_der_mat
              endif
            endif
          endif
        if !mcetry > 1 then
          vector _$_instru_delta = _$_mce_instru_vec - _$_instru_prev
          vector _$_gap_delta = _$_mce_gap_vec - _$_gap_prev

        ' ***************  
          if %meth = "broy" then
            matrix _$_jy = _$_mce_der_mat*_$_gap_delta
            matrix _$_sj = @transpose(_$_instru_delta)*_$_mce_der_mat
            scalar _$_sjf = @sum(@transpose(_$_instru_delta)*_$_jy)
            _$_mce_der_mat = _$_mce_der_mat + ((_$_instru_delta - _$_jy)*_$_sj)/_$_sjf
            _$_mce_direction = -(_$_mce_der_mat*_$_mce_gap_vec)
            endif

        ' ***************  
          if %meth = "qnewton" then
            if !mcetry = 2 then
            ' create some matrices on first pass
              vector(!broymax) _$_stp_nrm
              matrix(!tmceinstrus,!broymax) _$_stp
              vector (!broymax) _$_lam_rec
              matrix(!tmceinstrus,1) _$_z
              vector(!broymax) _$_counter
              for !iq = 1 to !broymax
                _$_counter(!iq) = !iq
                next
              endif

            if !mcetry <= !broymax + 1 then
              !q = !mcetry-1
              !f = !q
              else
              !q = @mod(!mcetry-1,!broymax) + 1
              !f = !broymax 
              call shiftleft(_$_counter,1)
              endif

            colplace(_$_stp,_$_instru_delta,!q)
            _$_stp_nrm(!q) = @norm(_$_instru_delta,2)
            _$_lam_rec(!q) = !mcz_step

            if %jinit = "bd" and !mcetry <= !broymax + 1 then 
              for !ijj = 1 to !nmcetargs
                vector _$_vec_adds = -_$_mce_der_mat_{!ijj}* @subextract(_$_mce_gap_vec,(!ijj-1)*!nqtrs+1,1,!ijj*!nqtrs,1)
                matplace(_$_z,_$_vec_adds,(!ijj-1)*!nqtrs+1,1)
                next
              else
              _$_z = -_$_mce_gap_vec
              endif

            if !mcetry > 2 then
              for !kbroy = 2 to !f
                !k0 = _$_counter(!kbroy)
                !k1 = _$_counter(!kbroy - 1)
                !a = _$_lam_rec(!k1)/_$_lam_rec(!k0)
                !b = _$_lam_rec(!k1) - 1
                _$_z = _$_z + (!a*@columnextract(_$_stp,!k0)+!b*@columnextract(_$_stp,!k1))*(@transpose(@columnextract(_$_stp,!k1))*_$_z)/(_$_stp_nrm(!k1)^2)
                next
              endif
            !nrm2 = _$_stp_nrm(!q)^2
            !lam = _$_lam_rec(!q)
            !stz = @sum(@transpose(_$_instru_delta)*_$_z)
            _$_mce_direction = (!nrm2*_$_z-(1-!lam)*!stz*_$_instru_delta)/(!nrm2-!lam*!stz)
            endif
          endif
        endif

    ' ******************************
    ' Fair-Taylor algorithm
      if %meth = "ft" then
        _$_mce_direction = -!lambda*_$_mce_gap_vec
        endif
    ' ******************************

      endif

    wend

' ************************************************
' 5. final steps
' ************************************************

  scalar _$_iterations = !mcetry

  if !mceshow = 2 then
    close mce_sim_stats
    endif

  if !mceshow = 1 or !mceshow = 2 then
    if @isobject("mce_sim_spool") then
      delete mce_sim_spool
      endif
    spool mce_sim_spool
    mce_sim_spool.append mce_sim_stats
    mce_sim_spool.append mce_sim_text
    mce_sim_spool.name untitled01 mce_sim_stats
    mce_sim_spool.name untitled02 mce_sim_text
    show mce_sim_spool
    endif


  if !mceshow < 3 then
    statusline mcz_sim finished
    endif


  if %simtype = "single" and %cleanup = "yes" then
    delete(noerr) _$_*
    endif

endsub


'****************************************************************
'****************************************************************
'****************************************************************
subroutine mcz_solvit

' This subroutine first sets the MCE instrument values based on the current
' optimal direction and choice of step size, and then solves the models

  %mcz_solvit = "ok"

' update instrument values based on current direction and step size
  if !mcetry > 1 then
    mce_sim_stats(!mcetry+2,4) = !mcz_step
    _$_mce_instru_vec = _$_instru_prev + !mcz_step*_$_mce_direction
    matrix _$_tmp_mat = @unvec(_$_mce_instru_vec,!nqtrs)
    mtos(_$_tmp_mat,_$_mce_instrus)
    endif

' solve lag model
  smpl %mcestart %mceend
  !err_before = @errorcount 
  {%_$_mod_b}.solve
  !err_after = @errorcount
  if !err_after > !err_before then
    if %dontstop = "yes" then
      %mcz_solvit = "err"
      return
      else
      @uiprompt("Error in solving lag model: execution terminating")
      stop
      endif
    endif

' optionally set terminal conditions on first iteration
  if !mcetry = 1 and %terminal = "yes" then
    call mcz_terminal
    endif

' solve lead model
  smpl %mcestart %mceend 
  !err_before = @errorcount 
  {%_$_mod_f}.solve
  !err_after = @errorcount
  if !err_after > !err_before then
    if %dontstop = "yes" then
      %mcz_solvit = "err"
      return
      else
      @uiprompt("Error in solving lead model: execution terminating")
      stop
      endif
    endif

' create group of the solution values of the mce target variables
' on the first iteration
  if !mcetry = 1 then
    {%_$_mod_f}.makegroup _$_mce_errs_sols {%mce_targ_names}
    endif

' compute mce error functions
  _$_mce_targ_vec = @vec(@convert(_$_mce_errs_sols))
  _$_mce_gap_vec = _$_mce_targ_vec 
  !nloss = @norm(_$_mce_gap_vec,2)^2
  mce_sim_stats(!mcetry+2,3) = !nloss
  _$_mce_loss_vec(!mcetry) = !nloss
  !nconv =  @max(@abs(_$_mce_gap_vec))
  _$_mce_conv_vec(!mcetry) = !nconv
  if !mcetry > 1 then
    !gamma = !nloss/_$_mce_loss_vec(!mcetry-1)
    endif
  
  if !mceshow = 2 then
    show mce_sim_stats
    endif

endsub


'****************************************************************
'****************************************************************
'****************************************************************
subroutine mcz_derivs
 
'This subroutine computes the derivatives of the mce targets wrt the mce instruments

  mce_sim_stats(!mcetry+2,6) = "yes"
  matrix _$_mce_der_mat = @filledmatrix(!nmceinstrus*!nqtrs,!nmcetargs*!nqtrs,0)

  if !mcetry > 1 then
    '_$_mce_ptrb_mat = @abs(@unvec(!mcz_step*_$_mce_direction,!nqtrs)) + 1e-6
    _$_mce_ptrb_mat = @abs(@unvec(!mcz_step*_$_mce_direction,!nqtrs)) + 1e-4
    endif

  smpl %mcestart %mceend
  _$_mce_targ_vec = @vec(@convert(_$_mce_errs_sols))

  !maxlead = 1

' *********************************
' construct vector that determines when exact derivatives 
' need to be computed

' _$_dvec > 0 => period in which derivatives are to be simulated
' _$_dvec = 1 => but derivatives do not have to be spread/interpolated forward or back
' _$_dvec = 2 => and derivatives have to be spread forward down diagonals
' _$_dvec = 3 => and derivatives have to be spread backward up diagonals
' _$_dvec = 4 => and derivaties have to be interpolated backward along diagonals
' _$_dvec = 5 => hybrid 
  vector(!nqtrs) _$_dvec = 0

  ' *********************************
  if %jinit = "every" then
    _$_dvec = _$_dvec + 1
    endif

  ' *********************************
  if %jinit = "interp" then
    _$_dvec(1) = 1
    _$_dvec(!nqtrs-!maxlead) = 4
    for !ij0 = (1+!nskip) to (!nqtrs-!maxlead-1) step !nskip
      _$_dvec(!ij0) = 4
      next
    if !maxlead > 0 then
      for !ij0 = (!nqtrs-!maxlead+1) to !nqtrs
        _$_dvec(!ij0) = 1
        next
      endif
    if _$_dvec(!nqtrs-!maxlead-1) = 4 then
      _$_dvec(!nqtrs-!maxlead)  = 1
      endif
    endif

' *********************************
  if %jinit = "bd" then
    !bd_col = @floor(!nqtrs/2)
    _$_dvec(!bd_col) = 5
    endif


' *********************************
' code modified 5/27/10 to reduce the number of derivative
' sims by 1 per mc variable -- will work only when 
' maxlead = 1
  if %jinit = "linear" then
    _$_dvec(1) = 2
    _$_dvec(!nqtrs) = 3
    '_$_dvec(!nqtrs-!maxlead) = 3
    'for !ij0 = !nqtrs - !maxlead + 1 to !nqtrs
    '  _$_dvec(!ij0) = 1
    '  next
    endif

' *********************************
' *********************************
' Outer loop:  specifies which instrument is shocked
  for !ij1 = 1 to !nmceinstrus
    %instru_name = _$_mce_instrus.@seriesname(!ij1)
    statusline computing MCE derivatives for instrument %instru_name

  ' *********************************
  ' Middle loop:  simulates effects of instrument shock for selected time periods
    !skip = 0
    for !ij2 = 1 to !nqtrs
      !skip = !skip + 1
      if _$_dvec(!ij2) > 0 then
        !perturbit =  _$_mce_ptrb_mat(!ij2,!ij1)
        !col = (!ij1-1)*!nqtrs + !ij2
        smpl %mcestart + !ij2-1 %mcestart + !ij2-1
        {%instru_name} = {%instru_name} + !perturbit
        smpl %mcestart %mceend
        {%_$_mod_b}.solve
        {%_$_mod_f}.solve
        _$_mce_targ_dvec = @vec(@convert(_$_mce_errs_sols))
        matplace(_$_mce_der_mat,(_$_mce_targ_dvec-_$_mce_targ_vec)/!perturbit,1,!col)
        smpl %mcestart + !ij2-1 %mcestart + !ij2-1
        {%instru_name} = {%instru_name} - !perturbit

      ' *********************************
      ' Inner loop:  place and/or interpolates derivatives
        if %jinit <> "every" then
          for !ij3 = 1 to !nmceinstrus
            !row = (!ij3-1)*!nqtrs + 1
            matrix _$_tempa = @subextract(_$_mce_der_mat,!row,!col,!row+!nqtrs-1,!col)

          ' *****************************************************************
            if %jinit = "linear" then

            ' forward loop moves the derivative diagonally down to the right
            ' when _$_dvec = 2
              if _$_dvec(!ij2) = 2 then
              for !ij4 = 1 to (!nqtrs - !maxlead - 1)
                matrix _$_tempb = @subextract(_$_tempa,1,1,!nqtrs-!ij4,1)
                matplace(_$_mce_der_mat,_$_tempb,!row+!ij4,!col+!ij4)
                next
              endif

            ' backward loop moves the derivative diagonally up to the left
            ' when _$_dvec = 3
              if _$_dvec(!ij2) = 3 then
                !ij5 =  !nqtrs - !maxlead - 1
                for !ij4 = 1 to !ij5
                  matrix _$_tempb = @subextract(_$_tempa,(1+!ij4),1,!nqtrs-!maxlead,1)
                  matplace(_$_mce_der_mat,_$_tempb,!row,!col-!ij4)
                  next
                endif

            ' fill unassigned elements of last !maxlead rows with zeros when _$_dvec = 3
              if _$_dvec(!ij2) = 3 then
                !ij5 =  !nqtrs - 2
                matrix(1,!ij5) _$_tempc = 0
                for !ij4 = 1 to !maxlead
                  matplace(_$_mce_der_mat,_$_tempc,!row+!nqtrs-!ij4,!col-!nqtrs+2)
                  next
                endif

              endif

          ' *****************************************************************
            if (%jinit = "interp") and (_$_dvec(!ij2) = 4) then
              !nn2 = !nqtrs-!skip+1
              'fill in columns between current and previous derivative
              '_$_tempa holds current derivative, _$_tempb holds previous derivative

              matrix _$_tempa1 = @subextract(_$_tempa,1,1,!skip,1)
              matrix _$_tempa2 = @subextract(_$_tempa,!skip+1,1,!nqtrs,1)

              'previous derivative
              matrix _$_tempb = @subextract(_$_mce_der_mat,!row,!col-!skip,!row+!nqtrs-1,!col-!skip)
              matrix _$_tempb1 = @subextract(_$_tempb,1,1,!nn2-1,1)
              matrix _$_tempb2 = @subextract(_$_tempb,!nn2,1,!nqtrs,1)

              'forward loop moves lower (ie, nonoverlapping) portion of _$_tempb diagonally down to right
              for !ij4 = 1 to !skip-1
                matrix _$_tempb2a = @subextract(_$_tempb2,1,1,!skip-!ij4,1)
                matplace(_$_mce_der_mat,_$_tempb2a,!row+!nn2-1+!ij4,!col-!skip+!ij4)
                next

              'backward loop moves upper (ie, nonoverlapping) portion of _$_tempa diagonally up to the left
              for !ij4 = 1 to !skip-1
                matrix _$_tempa1a = @subextract(_$_tempa1,1+!ij4,1,!skip,1)
                matplace(_$_mce_der_mat,_$_tempa1a,!row,!col-!ij4)
                next

              'overlap loop interpolates upper part of _$_tempb and lower part of _$_tempa
              for !ij4 = 1 to !skip-1
                matrix _$_tempc = ((!skip-!ij4)/!skip)*_$_tempb1 + (!ij4/!skip)*_$_tempa2
                matplace(_$_mce_der_mat,_$_tempc,!row+!ij4,!col-!skip+!ij4) 
                next

              'optionally interpolate elements of last !maxlead rows across each row
              !qqqq = 0
              if !qqqq = 1 then
                !nn3 = !nqtrs - !maxlead + 1
                matrix _$_tempa3 = @subextract(_$_tempa,!nn3,1,!nqtrs,1)
                matrix _$_tempb3 = @subextract(_$_tempb,!nn3,1,!nqtrs,1)
                for !ij4 = 1 to !skip-1
                  matrix _$_tempc = ((!skip-!ij4)/!skip)*_$_tempb3 + (!ij4/!skip)*_$_tempa3
                  matplace(_$_mce_der_mat,_$_tempc,!row+!nn3-1,!col-!skip+!ij4) 
                  next
                endif

              endif

         '*****************************************************************
         'The bd method takes account of only two own-derivative elements --
         'one on the diagonal and one on the first super diagonal.  The latter is 
         'important for one-lead euler equations.
         '
         'there is some crude coding here, reflecting the fact that the full column
         'of the derivatives matrix has already been filled in and needs to be 
         'zeroed out
      
          if (%jinit = "bd") and (_$_dvec(!ij2) = 5) and (!ij1=!ij3) then
            matplace(_$_mce_der_mat,0*_$_mce_targ_dvec,1,!col)
            !diag = _$_tempa(!bd_col,1)
            vector(!nqtrs-1) _$_tempb = _$_tempa(!bd_col-1,1)
            'vector(!nqtrs-1) _$_tempb1 = _$_tempa(!bd_col+1,1)
            matrix(!nqtrs,!nqtrs) _$_tempc
            matrix _$_tempc = !diag*@identity(!nqtrs) + @makediagonal(_$_tempb,1)
            'matrix _$_tempc = !diag*@identity(!nqtrs) + @makediagonal(_$_tempb,1) + @makediagonal(_$_tempb1,-1)
            _$_tempc = @inverse(_$_tempc)
            matplace(_$_mce_der_mat,_$_tempc,!row,!row) 
            endif
               

           '*****************************************************************

            next
          !skip = 0
          endif
        endif
      next
    next
' *********************************

  delete(noerr) _$_tempa _$_tempb _$_tempc _$_tempd

  'recalculate current solution
  smpl %mcestart %mceend
  {%_$_mod_b}.solve
  {%_$_mod_f}.solve


' *********************************
' invert derivative matrix (unless using bd method, in which case
' it's already inverted)
  if %jinit <> "bd" then
    statusline inverting MCE derivative matrix
    _$_mce_der_mat = @inverse(_$_mce_der_mat)
    endif

endsub



'************************************************************
'************************************************************
'************************************************************
subroutine mcz_lmr

  %mcz_lmr = "ok"

  !step_pos = !mcz_step
  !step_neg = !mcz_step
  %line_converge = "no"
  !nk = _$_mce_loss_vec(1)/(!mcetry)^2
  !loss_prev = _$_mce_loss_vec(!mcetry-1)
  if !mhistory >= (!mcetry-1) then
    vector losshist =  @subextract(_$_mce_loss_vec,1,1,!mcetry-1,1)
    else
    vector losshist = @subextract(_$_mce_loss_vec,!mcetry-!mhistory,1,!mcetry-1,1)
    endif
  !fkbar = @max(losshist)
  for !j = 1 to !mcelinemax
    if !j > 1 then
      !mcz_step = !step_pos
      call mcz_solvit
      if %mcz_solvit = "err" then
        %mcz_lmr = "err"
        return
        endif
      endif
    if !nloss <= (!fkbar + !nk - !gammak*!step_pos^2*!loss_prev) then
      %line_converge = "yes"
      exitloop
      else
      !nloss_pos = !nloss
      !mcz_step = -!step_neg
      call mcz_solvit
      if %mcz_solvit = "err" then
        %mcz_lmr = "err"
        return
        endif
      if !nloss <= (!fkbar + !nk - !gammak*!step_neg^2*!loss_prev) then
        %line_converge = "yes"
        exitloop
        endif
      !nloss_neg = !nloss
      endif
    !alphat = !step_pos^2*!loss_prev/(!nloss_pos + (2*!step_pos-1)*!loss_prev)
    if !alphat < (!tmin*!step_pos) then
      !step_pos = !tmin*!step_pos
      else
      if !alphat > !tmax*!step_pos then
        !step_pos = !tmax*!step_pos
        else
        !step_pos = !alphat
        endif
      endif
    !alphat = !step_neg^2*!loss_prev/(!nloss_neg + (2*!step_neg-1)*!loss_prev)
    if !alphat < (!tmin*!step_neg) then
      !step_neg = !tmin*!step_neg
      else
      if !alphat > !tmax*!step_neg then
        !step_neg = !tmax*!step_neg
        else
        !step_neg = !alphat
        endif
      endif
    next
  setcell(mce_sim_stats,!mcetry+2,5,!j,0)

endsub



'************************************************************
'************************************************************
'************************************************************
subroutine mcz_armijo
  
  %mcz_armijo = "ok"

  !iarm = 0
  while ((!nloss > (1-.01*!mcz_step)*!loss_prev) and !iarm < !mcelinemax)
    !mcz_step = !lrat * !mcz_step
    call mcz_solvit
    if %mcz_solvit = "err" then
      %mcz_armijo = "err"
      return
      endif
    !iarm = !iarm + 1
    wend
  setcell(mce_sim_stats,!mcetry+2,5,!iarm,0)

endsub



'**************************************************************************
'**************************************************************************
'**************************************************************************
subroutine mcz_terminal

' Set terminal values for all variables in _$_mod_f that might
' appear with leads, based on the simulated values of the corresponding 
' variables in _$_mod_b at the end of the simulation period.
' Terminal values of stationary variables equal their last simulated 
' level.  Terminal values of nonstationary are based on extrapolating their
' simulated growth rates.
'
' This subroutine is usually called only once per MCE simulation and only
' for MCE simulations of permanent shocks.  Multiple calls to this
' subroutine may result in convergence problems as a result of terminal
' values that drift from iteration to iteration.


' %fvars (variables that are exogenous in _$_mod_f and
' also endogenous in _$_mod_b)

  {%_$_mod_b}.makegroup _$_fvars_sols {%fvars}

  for !ijj = 1 to @wcount(%fvars)
    %tmpa = @lower(@word(%fvars,!ijj))
    %tmpb = _$_fvars_sols.@seriesname(!ijj)
    smpl  %mceend + 8 %mceend + 8
    !tmpmean = @abs(@mean({%tmpa}))
    if !tmpmean > .001 then
      series _$_tmpgrowth = @abs(@movav(d({%tmpa},0,1)/{%tmpa}(-1),8))
      else
      series _$_tmpgrowth = 0
      endif
    !tmpscal = @mean(_$_tmpgrowth)
    if (!tmpscal < .001) then
      'stationary case
      smpl %mceend + 1  @last
      {%tmpb} = {%tmpb}(-1)
      else
      'nonstationary case
      smpl %mceend  %mceend
      series _$_tmpgrowth = @movav(d({%tmpb},0,1)/{%tmpb}(-1),8)
      !tmpscal = @mean(_$_tmpgrowth) + 1
      smpl %mceend + 1  @last
      {%tmpb} = !tmpscal*{%tmpb}(-1)
      endif
    {%tmpa} = {%tmpb}
    next

' mcevars_f (mce variables in _$_mod_f, if any) have terminal conditions
' based on the values of mcevars_b (proxies for mce variables in _$_mod_b)

  if @wcount(%mcevars_f) > 0 then
    {%_$_mod_b}.makegroup _$_mcevars_b_sols {{%mcevars_b}}
    %mcevars_f1 = {%mcevars_f}
    for !ijj = 1 to  @wcount(%mcevars_f1)
      %tmpa = @lower(@word(%mcevars_f1,!ijj))
      %tmpb = _$_mcevars_b_sols.@seriesname(!ijj)
      smpl %mceend + 8 %mceend + 8
      !tmpmean = @abs(@mean({%tmpa}))
      if !tmpmean > .001 then
        series _$_tmpgrowth = @abs(@movav(d({%tmpa},0,1)/{%tmpa}(-1),8))
        else
        series _$_tmpgrowth = 0
        endif
      !tmpscal = @mean(_$_tmpgrowth)
      if (!tmpscal < .001) then
        'stationary case
        smpl %mceend + 1  @last
        {%tmpb} = {%tmpb}(-1)
        else
        'nonstationary case
        smpl %mceend  %mceend
        series _$_tmpgrowth = @movav(d({%tmpb},0,1)/{%tmpb}(-1),8)
        !tmpscal = @mean(_$_tmpgrowth) + 1
        smpl %mceend + 1  @last
        {%tmpb} = !tmpscal*{%tmpb}(-1)
        endif
      smpl %mceend + 1  %mceend + 8
      {%tmpa} = {%tmpb}
      next
    endif

  mce_sim_text.append Terminal conditions set at iteration {!mcetry}

' once terminal conditions have been set once, turn the switch that
' calls this subroutine off
  %terminal = "no"

endsub



'**************************************************************************
'**************************************************************************
'**************************************************************************

subroutine mcz_opt_setup(string mcz_opt_options, group mcz_opt_instrus, group mcz_opt_targs, text mcz_opt_cnstr)

  statusline mcz_opt_setup


' ************************************************************************
' set up values of options based on defaults and overrides in string mcz_opt_options

  mcz_opt_options = @lower(mcz_opt_options)
  mcz_opt_options = @replace(mcz_opt_options," ","")
  mcz_opt_options = @replace(mcz_opt_options,","," ")
  mcz_opt_options = " " + mcz_opt_options + " "

' default values for options

  if %simtype <> "opttc" then
    !optmaxiter = 15
    !optconv = 1e-05
    else
    !optmaxiter = 50
    !optconv = 1e-06
    !tcdamp = 1
    endif

  !optlinemax = 10
  !optptrb = .01
  !opt_step_max = 1.0
  !optshow = 3
  !mceshow = 3

  %evlstart = %mcestart
  %drvstart = %mcestart
  %freq = @pagefreq
  %evlend = @datestr(@dateadd(@dateval(%evlstart),59,%freq))
  %drvend = @datestr(@dateadd(@dateval(%drvstart),39,%freq))

  %ideriv = "yes"
  %xopen = "yes"
  %xclose = "yes"

' if imposing constraints (mcz_opt_qp)
  %qpswitch = "r"

' are there overrides to defaults?
  if @len(mcz_opt_options) > 0 then
    %opts = "yes"
    else
    %opts = "no"
    endif

  if %opts = "yes" then
    call mcz_equalopt("m",mcz_opt_options)
    if @len(%temp)>0 then
      !optmaxiter  = @val(%temp)
      endif
    call mcz_equalopt("d",mcz_opt_options)
    if @len(%temp)>0 then
      !tcdamp  = @val(%temp)
      endif
    call mcz_equalopt("c",mcz_opt_options)
    if @len(%temp)>0 then
      !optconv  = @val(%temp)
      endif
    call mcz_equalopt("lmax",mcz_opt_options)
    if @len(%temp)>0 then
      !optlinemax  = @val(%temp)
      endif
    call mcz_equalopt("p",mcz_opt_options)
    if @len(%temp)>0 then
      !optptrb  = @val(%temp)
      endif
    call mcz_equalopt("stepmax",mcz_opt_options)
    if @len(%temp)>0 then
      !optstepmax  = @val(%temp)
      endif
    call mcz_equalopt("oo",mcz_opt_options)
    if @len(%temp)>0 then
      !optshow  = @val(%temp)
      !mceshow  = @val(%temp)
      endif
    call mcz_equalopt("lstart",mcz_opt_options)
    if @len(%temp)>0 then
      %evlstart = @lower(%temp)
      endif
    call mcz_equalopt("lend",mcz_opt_options)
    if @len(%temp)>0 then
      %evlend = @lower(%temp)
      endif
    call mcz_equalopt("istart",mcz_opt_options)
    if @len(%temp)>0 then
      %drvstart = @lower(%temp)
      endif
    call mcz_equalopt("iend",mcz_opt_options)
    if @len(%temp)>0 then
      %drvend = @lower(%temp)
      endif
    call mcz_equalopt("ideriv",mcz_opt_options)
    if @len(%temp)>0 then
      %ideriv = @lower(%temp)
      endif
    call mcz_hasopt("terminal",mcz_opt_options) 
    if !hasflag = 1 then
      %terminal = "yes"
      endif
    call mcz_hasopt("matlab",mcz_opt_options) 
    if !hasflag = 1 then
      %qpswitch = "matlab"
      endif
    call mcz_hasopt("/xopen",mcz_opt_options)
    if !hasflag = 1 then
      %xopen = "no"
      endif
    call mcz_hasopt("/xclose",mcz_opt_options)
    if !hasflag = 1 then
      %xclose = "no"
      endif

    endif


' ************************************************************************
' Some preliminaries

' copy group subroutine arguments into objects with fixed names so that they can be
' easily accessed by other subroutines
  copy mcz_opt_instrus     _$_opt_instrus
  copy mcz_opt_targs       _$_opt_targs

  smpl %evlstart %evlend
  !nevl = @obssmpl
  smpl %drvstart %drvend
  !ndrv = @obssmpl


' ***************************************
' Examine the inequality constraints, put them in a table,
' and, if necessary, augment the list of target variables to
' include all constraint variables
  if %cnstrflag = "yes" then
    svector _$_opt_cnstr = mcz_opt_cnstr.@svectornb
    !nconstraints = @rows(_$_opt_cnstr)
    if !nconstraints > 0 then
      table(1,1) _$_opt_cnstr_tab
      string _$_opt_cnstr_vars = " "
      call mcz_constraints(_$_opt_cnstr,_$_opt_cnstr_tab,_$_opt_cnstr_vars)
      %extra_targets = @wnotin(@upper(_$_opt_cnstr_vars),_$_opt_targs.@members)
      %extra_targets = @wunique(%extra_targets)
      !nextra = @wcount(%extra_targets)
      if !nextra > 0 then
        for !qq = 1 to !nextra
          %newtarg = @word(%extra_targets,!qq)
          _$_opt_targs.add {%newtarg}
          %newtarg_t = %newtarg + "_t"
          %newtarg_w = %newtarg + "_w"
          smpl @all
          series {%newtarg_t} = 0
          series {%newtarg_w} = 0
          next
        endif
      else
      @uiprompt("Error:  The cnstr keyword is assigned to an empty text file")
      stop
      endif
    endif



' ************************************************************************
' More preliminaries

  !noptinstrus = _$_opt_instrus.@count
  !nopttargs = _$_opt_targs.@count  

  !topttargs = !nopttargs * !nevl
  !toptinstrus = !noptinstrus * !ndrv
  if !topttargs < !toptinstrus then
    statusline Error: more instruments than targets
    stop
    endif

  matrix _$_opt_ptrb_mat = @filledmatrix(!ndrv,!noptinstrus,!optptrb)
  vector(!optmaxiter+1) _$_opt_loss_vec

  if %ideriv <> "no" then
    matrix _$_opt_der_mat = @filledmatrix(!toptinstrus,!topttargs,0)
    endif

  %opt_targ_names = _$_opt_targs.@members
  %opt_des_names = @wcross(%opt_targ_names,"_t")
  group _$_opt_des {%opt_des_names}
  smpl %evlstart %evlend
  matrix _$_opt_des_vec = @vec(@convert(_$_opt_des))

' if number of instruments and targets is the same, the optimal loss is zero,
' shortcut formulas can be used, and weights are not necessary
  if !toptinstrus = !topttargs then
    !zero_loss = 1
    matrix _$_opt_wt_mat = @identity(!topttargs)
    else
    !zero_loss = 0
    smpl %evlstart %evlend
    %opt_wt_names = @wcross(%opt_targ_names,"_w")
    group _$_opt_wts {%opt_wt_names}
    matrix _$_opt_wt_mat = @makediagonal(@vec(@convert(_$_opt_wts)))
    endif




' ************************************************************************
' text file

  text mce_opt_text

  if %cnstrflag = "yes" then
    mce_opt_text.append constrained optimization using {%qpswitch}:  xopen = {%xopen}; xclose = {%xclose}
    else
    mce_opt_text.append unconstrained optimization (EViews)
    endif
  if %simtype = "opt" then
    mce_opt_text.append optimization type = committment
    mce_opt_text.append simulation period:  {%mcestart} - {%mceend}
    mce_opt_text.append loss evalation period: {%evlstart} - {%evlend}
    mce_opt_text.append instrument setting period: {%drvstart} - {%drvend}
    mce_opt_text.append max number of optimization iterations = !optmaxiter
    mce_opt_text.append max number of line search steps per iteration = !optlinemax
    endif
  if %simtype = "opttc" then
    mce_opt_text.append optimization type = time-consistent nash (discretion)
    mce_opt_text.append first instrument setting period: {%drvstart}
    mce_opt_text.append first simulation period:  {%mcestart} - {%mceend}
    mce_opt_text.append first loss evalation period: {%evlstart} - {%evlend}
    mce_opt_text.append last instrument setting period: {%drvend}
    mce_opt_text.append max number of backward-induction iterations = !optmaxiter
    endif
  mce_opt_text.append convergence criteria = !optconv
  mce_opt_text.append output control parameter = !optshow
  mce_opt_text.append compute instrument derivs? = {%ideriv}
  mce_opt_text.append instrument perturbation factor = !optptrb


' ************************************************************************
' table of iteration-by-iteration statistics

  if @isobject("mce_opt_stats") then
    delete mce_opt_stats
    endif
  table(!optmaxiter+2,7) mce_opt_stats
  mce_opt_stats.setwidth(1:1) 5
  mce_opt_stats.setwidth(1:5) 12
  mce_opt_stats.setlines(a2:g2) +b
  setcell(mce_opt_stats,1,1,"iter")
  setcell(mce_opt_stats,1,2,"f(x)")
  setcell(mce_opt_stats,1,3,"step size")
  setcell(mce_opt_stats,1,4,"convergence")
  setcell(mce_opt_stats,2,4,"statistic")
  setcell(mce_opt_stats,1,5,"linearity")
  setcell(mce_opt_stats,2,5,"statistic")

' ************************************************************************
' call appropriate optimization subroutine

  if %simtype = "opt" and %cnstrflag = "no" then
    call mcz_opt
    endif
  if %simtype = "opt" and %cnstrflag = "yes" then
    call mcz_opt_qp
    endif
  if %simtype = "opttc" then
    call mcz_opt_tc
    endif


' ************************************************************************
' spool
  if @isobject("mce_opt_spool") then
    delete mce_opt_spool
    endif
  spool mce_opt_spool
  if %simtype = "opt" then
    mce_opt_spool.append mce_opt_stats
    mce_opt_spool.append mce_opt_text
    mce_opt_spool.name untitled01 mce_opt_stats
    mce_opt_spool.name untitled02 mce_opt_text
    endif
  if %simtype = "opttc" then
    mce_opt_spool.append mce_opt_text
    mce_opt_spool.name untitled01 mce_opt_text
    endif

  if !optshow <> 4 then
    show mce_opt_spool
    close mce_opt_stats
    endif

  if %cleanup = "yes" then
    delete(noerr) _$_*
    endif


  endsub


'**************************************************************************
'**************************************************************************
'**************************************************************************
subroutine mcz_opt

'Main subroutine for unconstrained optimal control simulations


' **************************************************************************
' initialize counters and switches

  !opttry = 0
  %opt_converge = "no"
  !optnonlin = 0
  !optpchloss = 100



' **************************************************************************
' initial solution

  smpl %mcestart %mceend
  !opt_step = 0
  !opt_step_prev = 0
  call mcz_opt_solve
  if !opttry = 0 and !optloss = 0 then
    %opt_converge = "yes"
    mce_opt_text.append At iteration {!opttry}, convergence
    endif



' **************************************************************************
' iterate to minimize loss
  smpl %mcestart %mceend
  while !opttry <= !optmaxiter and %opt_converge = "no"

    !opttry = !opttry + 1

  ' *******************************
  ' compute instrument derivatives, hessian, gradient, direction, and
  ' predicted loss assuming the model is linear
    if (%ideriv <> "no" and (!opttry = 1 or @abs(!optnonlin) > .1)) then
      call mcz_opt_deriv
      endif
    if !zero_loss = 0 then
      matrix _$_opt_hess = 2*_$_opt_der_mat*_$_opt_wt_mat*@transpose(_$_opt_der_mat)
      else
      matrix _$_opt_hess = 2*@transpose(_$_opt_der_mat)
      endif
    if @issingular(_$_opt_hess) = 1 then
      %errstring = "Hessian is singular at iteration " + @str(!opttry)
      @uiprompt(%errstring)
      stop
      endif
    matrix _$_opt_hessinv = @inverse(_$_opt_hess) 
    if !zero_loss = 0 then
      matrix _$_opt_grad = 2*_$_opt_der_mat*_$_opt_wt_mat*_$_opt_gap_vec
      else
      matrix _$_opt_grad = 2*_$_opt_gap_vec
      endif
    vector _$_opt_direction = _$_opt_hessinv*_$_opt_grad
    matrix _$_opt_gap_vec_p = _$_opt_gap_vec - @transpose(_$_opt_der_mat) * _$_opt_direction
    !optloss_p = @sum(@transpose(_$_opt_gap_vec_p)*_$_opt_wt_mat*_$_opt_gap_vec_p)

  ' *******************************
  ' solve model and compute loss
    !opt_step = !opt_step_max
    !opt_step_prev = 0
    call mcz_opt_solve

  ' *******************************
  ' test for nonlinearity
  ' !optnonlin is the ratio of the actual to predicted percentage reduction in loss less 1.0;
  '  the closer the model is to being linear, the closer !optnonlin is to zero
    !ddloss = (_$_opt_loss_vec(!opttry)-_$_opt_loss_vec(!opttry+1))
    if !ddloss <> 0 then
      !optnonlin = (_$_opt_loss_vec(!opttry+1)-!optloss_p) / !ddloss
      else
      !optnonlin = 100
      endif
    mce_opt_stats(!opttry+3,5) = !optnonlin

  ' *******************************
  ' Search for a better a step size (Armijo condition and backtracking)
    !opt_loss_dderiv = -@transpose(_$_opt_grad)*_$_opt_direction
    !kk = .01
    !iarm = 0
    while (_$_opt_loss_vec(!opttry+1) > _$_opt_loss_vec(!opttry) + !kk*!opt_step*!opt_loss_dderiv) _
      and !iarm < !optlinemax
      !iarm = !iarm + 1
      !opt_step_prev = !opt_step
      !opt_step = .5*!opt_step
      call mcz_opt_solve
      wend

  ' *******************************
  ' Test for convergence
    if !zero_loss = 1 then
    ' equal number of targets and controls, full rank derivative matrix
    ' => loss has to be less than !optconv
      mce_opt_stats(!opttry+3,4) =  _$_opt_loss_vec(!opttry+1)
      if _$_opt_loss_vec(!opttry+1) < !optconv then
        %opt_converge = "yes"
        endif
      else
    ' !zero_loss = 0 => percentage change in loss from previous iteration has
    ' to be less than !optconv
      !optpchloss =  1-(_$_opt_loss_vec(!opttry+1)/_$_opt_loss_vec(!opttry))
      mce_opt_stats(!opttry+3,4) =  !optpchloss
      if !optpchloss < !optconv then
        %opt_converge = "yes"
        mce_opt_text.append At iteration {!opttry}, convergence
        endif
      endif

    if !opttry = 1 and @abs(!optnonlin) < 1e-7 and !opt_step = 1 then
      %opt_converge = "yes"
      mce_opt_text.append At iteration 1, convergence assumed because model is linear
      endif

    if !opttry = !optmaxiter then
      mce_opt_text.append At iteration {!opttry}, no convergence in {!optmaxiter} iterations
      !continue = @uiprompt("Maximum number of optimization iterations reached:  Continue execution?","YN")
      if !continue = 2 then
        stop
        else
        %opt_converge = "yes"
        endif
      endif

    if !optshow <> 4 then
      show mce_opt_stats
      endif
    wend



endsub


'**************************************************************************
'**************************************************************************
'**************************************************************************
subroutine mcz_opt_qp

' Main subroutine for optimal control simulations with inequality constraints
' (requires either R or matlab)


' The original problem:
'
'   choose x to minimize z = (y-y*)'W(y-y*) 
'   under the constraint Cy >= c
' 
'   where at each iteration the linearized relationship between
'   the target variables (y) and instrument variables (x) is
'   given by
'
'   y = B'x + k
' 
' The transformed problem:  Solve out y
'
'   choose x to minimize z = x'BWB'x + 2(k-y*)'WB'x + const
'   under the constraint CB'x >= c - Ck
'
' The R command -- quadprog::solve.QP(D,d,A,b) -- solves
'
'   min [0.5 * x' D x - d'x] with the constraint A'x >= b
' 
' The matlab command -- x = quadprog(D,d,A,b) -- solves
'
'   min [0.5 * x' D x + d'x] with the constraint Ax <= b
'
'             R             matlab
'   D     2BWB'             same
'   d'    2(k-y*)'WB'      -2(k-y*)'WB'
'   A'    CB'              -CB'
'   b     c-Ck             -(c-Ck)
'
'   where k = y - B'x
'

' ***************************************
' Create constraint matrices (C,c)-- dimensions are based on the number
' of periods in which the instruments are set (!ndrv) not the number of
' periods in which the loss function is evaluated (!nevl)

  if %cnstrflag = "yes" then
    matrix(!nevl * !nconstraints,!nevl*!nopttargs) _$_opt_cnstr_mat = 0
    matrix(!nevl * !nconstraints,1) _$_opt_cnstr_vec = 0
    for !i = 1 to !nconstraints
      !nterms = @val(_$_opt_cnstr_tab(!i,1))
      for !k = 1 to !nterms
        %var = @upper(_$_opt_cnstr_tab(!i,2*!k))
        !coef =  @val(_$_opt_cnstr_tab(!i,2*!k+1))
        !j = @wfindnc(%opt_targ_names,%var)
        for !l = 1 to !nevl
          _$_opt_cnstr_mat((!i-1)*!nevl+!l,(!j-1)*!nevl+!l) = !coef
          next
        next
      for !l = 1 to !nevl
        !coef =  @val(_$_opt_cnstr_tab(!i,2*!nterms+2))
        _$_opt_cnstr_vec((!i-1)*!nevl+!l) = !coef
        next
      next
    else
    !nconstraints = 1
    matrix(!nevl,!nevl*!nopttargs) _$_opt_cnstr_mat = 0
    matrix(!nevl,1) _$_opt_cnstr_vec = 0
    endif


' **************************************************************************
' initialize counters and switches

  !opttry = 0
  %opt_converge = "no"
  !optnonlin = 0
  !optpchloss = 100

  if %qpswitch = "r" and %xopen = "yes" then
    xopen(type=r, case=lower)
    endif
  if %qpswitch = "matlab" and %xopen = "yes" then
    xopen(type=m, case=lower)
    endif


' **************************************************************************
' compute an initial solution, without reference to any constraints

  smpl %mcestart %mceend
  !opt_step = 0
  !opt_step_prev = 0
  call mcz_opt_solve


' **************************************************************************
' iterate to minimize loss 
  smpl %mcestart %mceend
  while !opttry <= !optmaxiter and %opt_converge = "no"

    !opttry = !opttry + 1

  ' *******************************
  ' compute instrument derivatives
    if %ideriv <> "no" then
      call mcz_opt_deriv
      endif

  ' *******************************
  ' compute qp matrices
    smpl %drvstart %drvend
    vector _$_opt_instru_vec = @vec(@convert(_$_opt_instrus))
    matrix _$_qp_k_vec = _$_opt_targ_vec-@transpose(_$_opt_der_mat)*_$_opt_instru_vec
    matrix qp_d_mat = 2 * _$_opt_der_mat * _$_opt_wt_mat * @transpose(_$_opt_der_mat)
    matrix qp_d_vec = -2 * _$_opt_der_mat*@transpose(_$_opt_wt_mat) _
                       * (_$_qp_k_vec - _$_opt_des_vec)

    matrix qp_a_mat = @transpose(_$_opt_cnstr_mat * @transpose(_$_opt_der_mat))
    matrix qp_b_vec = _$_opt_cnstr_vec - _$_opt_cnstr_mat _
                      * (_$_qp_k_vec - _$_opt_des_vec)

    if !ndrv <> !nevl then
      qp_a_mat = @subextract(qp_a_mat,1,1,!ndrv,!ndrv)
      qp_b_vec = @subextract(qp_b_vec,1,1,!ndrv,1)
      endif

    xput qp_d_mat qp_a_mat qp_d_vec qp_b_vec

  ' *******************************
  ' *******************************
  ' R code

    if %qpswitch = "r" then
      if ((!ndrv = 1) and (!noptinstrus = 1)) then
        xrun "dim(qp_d_mat) <- c(1,1);"
        xrun "dim(qp_a_mat) <- c(1,1);"
        xrun "dim(qp_d_vec) <- c(1,1);"
        xrun "dim(qp_b_vec) <- c(1,1);"
        endif
      xrun "QP.results <- quadprog::solve.QP(qp_d_mat,qp_d_vec,qp_a_mat,qp_b_vec);"
      xrun "r_xhat <- QP.results$solution;"
      xrun "r_xhat <- as.matrix(r_xhat);"
      xget(type = matrix, name = _$_qp_instru_vec) r_xhat
      endif
 
  ' *******************************
  ' *******************************
  ' matlab code

    if %qpswitch = "matlab" then
      xrun "qp_d_vec = -qp_d_vec;" 
      xrun "qp_a_mat = -qp_a_mat';"
      xrun "qp_b_vec = -qp_b_vec;"
      xrun  "aeq = [];"
      xrun  "beq = [];"
      xrun  "lb = [];"
      xrun  "ub = [];"
      xrun  "xx0 = [];"
      xrun "options = optimset('Algorithm','active-set','LargeScale','off');"
      xrun "matlab_xhat = quadprog(qp_d_mat,qp_d_vec,qp_a_mat,qp_b_vec,aeq,beq,lb,ub,xx0,options);"
      xget matlab_xhat
      %tmp = matlab_xhat.@type
      if %tmp = "SCALAR" then
        matrix(1,1) _$_qp_instru_vec = matlab_xhat
        else
        matrix _$_qp_instru_vec = matlab_xhat
        delete matlab_xhat
        endif
      endif
 
  ' *******************************
  ' *******************************

  ' values of target variables in qp (linearized) solution
    matrix _$_qp_targ_vec = @transpose(_$_opt_der_mat)*_$_qp_instru_vec + _$_qp_k_vec

  ' values of target variables in original model
    smpl %drvstart %drvend
    matrix _$_tt1 = @unvec(@vec(_$_qp_instru_vec),!ndrv)
    mtos(_$_tt1,_$_opt_instrus)
    smpl %mcestart %mceend
    call mcz_sim(" ")
    smpl %evlstart %evlend
    matrix _$_opt_targ_vec = @vec(@convert(_$_opt_targs_sols))
    matrix _$_opt_gap_vec = _$_opt_targ_vec - _$_opt_des_vec
    !optloss = @sum(@transpose(_$_opt_gap_vec)*_$_opt_wt_mat*_$_opt_gap_vec)
    setcell(mce_opt_stats,!opttry+2,1,!opttry,0)

  ' convergence test is based on the maximum difference between
  ' the target variable values in the linearized and original models
    !conv_stat = @max(abs(_$_qp_targ_vec-_$_opt_targ_vec))
    mce_opt_stats(!opttry+2,2) = !optloss
    mce_opt_stats(!opttry+2,4) = !conv_stat

    if !optshow <> 4 then
      show mce_opt_stats
      endif

    if !conv_stat < !optconv then
      %opt_converge = "yes"
      else
      endif

    wend

  delete(noerr) qp_d_mat qp_a_mat qp_d_vec qp_b_vec


endsub


'**************************************************************************
'**************************************************************************
'**************************************************************************
subroutine mcz_opt_tc


' 1. For a linear model, this code finds the exact time-consistent solution;
'    for a nonlinear model, the calculated solution is based on the
'    linearization of the model as given by the derivatives of the target
'    variables wrt the instrument variables along the baseline.
'
' 2. The basic data matrices include observations for (!nevl+!ndrv-1) periods,
'    which is also the interval over which tracking adds are required.
'
' 3. The simulation, derivative and evaluation periods are assumed to have the
'    same starting date
'
' 4. The solution method is backward induction; if there are no inequality constraints,
'    the method executes in EViews (using matrices); if inequality constraints
'    are present, the method executes in EViews if there is a single
'    instrument and in Matlab or R (depending on the 
'    setting of a switch) if there are multiple instruments (which requires
'    a quadratic programming algorithm),


' ***************************************
' Create constraint matrices (C,c)-- dimensions are based on the fact that
' each policymaker sets the instruments for a single period

  if %cnstrflag = "yes" then
    matrix(!nconstraints,!nopttargs) _$_opt_cnstr_mat = 0
    matrix(!nconstraints,1) _$_opt_cnstr_vec = 0
    for !i = 1 to !nconstraints
      !nterms = @val(_$_opt_cnstr_tab(!i,1))
      for !k = 1 to !nterms
        %var = @upper(_$_opt_cnstr_tab(!i,2*!k))
        !coef =  @val(_$_opt_cnstr_tab(!i,2*!k+1))
        !j = @wfindnc(%opt_targ_names,%var)
        _$_opt_cnstr_mat(!i,!j) = !coef
        next
      !coef =  @val(_$_opt_cnstr_tab(!i,2*!nterms+2))
      _$_opt_cnstr_vec(!i) = !coef
      next
    endif


' **************************************************************************
' initial solution, instrument derivatives, and data matrices


' initial simulation
  smpl %mcestart %mceend
  if %terminal = "yes" then
    call mcz_sim("terminal")
    %terminal = "no"
    else
    call mcz_sim("  ")
    endif
  {%_$_mod_b}.makegroup _$_opt_targs_sols {%opt_targ_names}
  !ntot = !ndrv + !nevl - 1

' derivative matrices and submatrices
' _$_opt_der_mat (this is the basic matrix; tc_dmat is its transpose)
' tc_dmat        effect of !ndrv periods of instruments on !ntot periods of targets
' tc_dmat_short  effect of !ndrv periods of instruments on !nevl periods of targets
' tc_dmat1       effect of period 1 instrument on !nevl periods of targets
' tc_dmat2       effect of period 1 instrument on period 1 targets
  if %ideriv <> "no" then
    !opttry = 1
    matrix _$_opt_der_mat = @filledmatrix(!toptinstrus,!nopttargs*!ntot,0)
    %evlend_b = %evlend
    %freq = @pagefreq
    !ntot1 = !ntot-1
    %evlend = @datestr(@dateadd(@dateval(%simstart),!ntot1,%freq))
    'call dateshift(%simstart,%evlend,!ntot-1)
    call mcz_opt_deriv
    %evlend = %evlend_b
    matrix tc_dmat = @transpose(_$_opt_der_mat)
    matrix tc_dmat_short = @filledmatrix(!nevl*!nopttargs,!ndrv*!noptinstrus,0)
    for !i = 1 to !toptinstrus
      for !j = 1 to !nopttargs
        !r1 = !ntot*(!j-1)+1
        !r2 = !r1 + !nevl - 1
        matplace(tc_dmat_short,@subextract(tc_dmat,!r1,!i,!r2,!i),!nevl*(!j-1)+1,!i)
        next
      next
    matrix tc_dmat1 = @filledmatrix(!nevl*!nopttargs,!noptinstrus,0)
    for !i = 1 to !noptinstrus
      matplace(tc_dmat1,@columnextract(tc_dmat_short,(!i-1)*!ndrv+1),1,!noptinstrus)
      next
    matrix tc_dmat2 = @filledmatrix(!noptinstrus,!nopttargs,0)
    for !i = 1 to !noptinstrus
      for !j = 1 to !nopttargs
        tc_dmat2(!i,!j) = tc_dmat1((!j-1)*!nevl+1,!i)
        next
      next
    endif

' initial simulation again (is this necessary)
  call mcz_sim(" ")

' data matrices 
  smpl %simstart {%evlend} + !ndrv - 1
  stom(_$_opt_targs_sols,tc_ymat)
  stom(_$_opt_des,tc_ystarmat)
  stom(_$_opt_instrus,tc_xmat)
  matrix tc_wmat = _$_opt_wt_mat

' (no constraints) => eviews;
' (1 constraint -- single period) => eviews
' (multiple constraints) => R or Matlab
  if %cnstrflag = "no" then
    %iii = "eviews"
    else
    if !noptinstrus = 1 then
      %iii = "eviews"
      else
      %iii = %qpswitch 
      endif
    endif

  scalar tc_iter = 0
  scalar tc_stat = 100
  scalar tc_itmax = !optmaxiter
  scalar tc_conv = !optconv


' **************************************************************************
' unconstrained backward induction => EViews

  if %cnstrflag = "no" then
    while (tc_stat > tc_conv) and tc_iter <= tc_itmax

      matrix tc_xb = tc_xmat
      matrix tc_yb = tc_ymat

      for !i = !ndrv to 1 step -1
        !n1 = !i+!nevl-1
        !n2 = !i+!ndrv-1
        vector tc_y0v = @vec(@subextract(tc_ymat,!i,1,!n1,!nopttargs))
        vector tc_ys0v = @vec(@subextract(tc_ystarmat,!i,1,!n1,!nopttargs))
        vector tc_x0v =  @vec(@subextract(tc_xmat,!i,1,!n2,!noptinstrus))
        vector tc_x0_1v = @vec(@subextract(tc_xmat,!i,1,!i,!noptinstrus))
        vector tc_xbv =  @vec(@subextract(tc_xb,!i,1,!n2,!noptinstrus))

        vector tc_y1v = tc_y0v + tc_dmat_short*(tc_x0v-tc_xbv)

        matrix tc_g = @transpose(tc_dmat1)*tc_wmat*(tc_y1v-tc_ys0v)
        matrix tc_h = @transpose(tc_dmat1)*tc_wmat*tc_dmat1
        vector tc_x1_1v = tc_x0_1v - !tcdamp*@inverse(tc_h)*tc_g
        matrix tc_x1_1 = @unvec(tc_x1_1v,!noptinstrus)
        matplace(tc_xmat,tc_x1_1,!i,1)
        next

      vector tc_x0v = @vec(@subextract(tc_xmat,1,1,!ndrv,!noptinstrus))
      vector tc_y0v = @vec(@subextract(tc_ymat,1,1,!ntot,!nopttargs))
      vector tc_y1v = tc_y0v + tc_dmat*(tc_x0v-tc_xbv)
      matrix tc_y1 = @unvec(tc_y1v,!ntot)
      matplace(tc_ymat,tc_y1,1,1)

      tc_iter = tc_iter + 1
      if tc_iter > 1 then
        tc_stat = @max(@abs(tc_xmat-tc_xmatprev))
        endif

      matrix tc_xmatprev = tc_xmat
      wend
    endif


' **************************************************************************
' constrained backward induction

  if %cnstrflag = "yes" then

    matrix tc_cmat = _$_opt_cnstr_mat
    matrix tc_cvec =  _$_opt_cnstr_vec


  ' **************************************************************************
  ' single-period constraint => eviews
    if %iii = "eviews" then

      statusline constrained TC optimization in EViews

      matrix mat_a = tc_dmat2*@transpose(tc_cmat)

      while (tc_stat > tc_conv) and tc_iter <= tc_itmax

        matrix tc_xb = tc_xmat
        matrix tc_yb = tc_ymat

        for !i = !ndrv to 1 step -1
          !n1 = !i+!nevl-1
          !n2 = !i+!ndrv-1
     
        ' vectors of observations in the current loss and instrument periods
          vector tc_y0v = @vec(@subextract(tc_ymat,!i,1,!n1,!nopttargs))
          vector tc_ys0v = @vec(@subextract(tc_ystarmat,!i,1,!n1,!nopttargs))
          vector tc_x0v =  @vec(@subextract(tc_xmat,!i,1,!n2,!noptinstrus))
          vector tc_x0_1v = @vec(@subextract(tc_xmat,!i,1,!i,!noptinstrus))
          vector tc_xbv =  @vec(@subextract(tc_xb,!i,1,!n2,!noptinstrus))

        ' vector of target variable values based on current instrument values
        ' (ie, solve model)
          vector tc_y1v = tc_y0v + tc_dmat_short*(tc_x0v-tc_xbv)

        ' unconstrained current optimal instrument value
          matrix tc_g = @transpose(tc_dmat1)*tc_wmat*(tc_y1v-tc_ys0v)
          matrix tc_h = @transpose(tc_dmat1)*tc_wmat*tc_dmat1
          vector tc_x1_1v = tc_x0_1v - @inverse(tc_h)*tc_g
        
        ' check constraint
          matrix tc_y1m = @unvec(tc_y1v,!nevl)
          matrix tc_ysm = @unvec(tc_ys0v,!nevl)
          matrix mattemp = @rowextract(tc_y1m,1) - (@transpose(tc_x0_1v)*tc_dmat2) - @rowextract(tc_ysm,1)
          matrix mat_b = tc_cvec - tc_cmat*@transpose(mattemp)
          scalar testit = @sum(mat_b)/@sum(mat_a)
          if @sum(tc_x1_1v) < testit then
            tc_x1_1v = testit
            !tc_iter = tc_iter
            endif
          vector tc_x1_1v = (1-!tcdamp)*tc_x0_1v + !tcdamp* tc_x1_1v

        ' place current optimal instrument value in instrument matrix
          matrix tc_x1_1 = @unvec(tc_x1_1v,!noptinstrus)
          matplace(tc_xmat,tc_x1_1,!i,1)

          next


      ' solve model
        vector tc_x0v = @vec(@subextract(tc_xmat,1,1,!ndrv,!noptinstrus))
        vector tc_y0v = @vec(@subextract(tc_ymat,1,1,!ntot,!nopttargs))
        vector tc_y1v = tc_y0v + tc_dmat*(tc_x0v-tc_xbv)
        matrix tc_y1 = @unvec(tc_y1v,!ntot)
        matplace(tc_ymat,tc_y1,1,1)

        tc_iter = tc_iter + 1

        if tc_iter > 1 then
          tc_stat = @max(@abs(tc_xmat-tc_xmatprev))
          endif

        matrix tc_xmatprev = tc_xmat
        wend
      endif


  ' **************************************************************************
  ' multiple constraints => R or matlab
  
    if %iii = "r" or %iii = "matlab" then

      scalar tc_noptinstrus = !noptinstrus
      scalar tc_nopttargs = !nopttargs
      scalar tc_nevl = !nevl
      scalar tc_ndrv = !ndrv
      scalar tc_damp = !tcdamp

      if %xopen = "yes" then
        if %iii = "r" then
          xopen(type=r)
          xrun library("quadprog")
          %wd = """" + %rpath + """"
          xrun setwd({%wd})
          else
          xopen(type=m)
          xrun addpath {%mpath}
          endif
        endif

      xput tc_ymat tc_ystarmat tc_xmat tc_wmat tc_cmat tc_cvec
      xput  tc_nopttargs tc_noptinstrus tc_nevl tc_ndrv tc_itmax tc_conv
      xput tc_dmat tc_dmat1 tc_dmat2 tc_dmat_short tc_damp

      if %iii = "r" then
        xrun source("tcoc_r.R")
        else
        xrun tcoc_m
        endif

      xget tc_xmat
      xget tc_ymat
      xget tc_iter
      !tc_iter = tc_iter

      if %xclose = "yes" then
        xclose
        endif

      delete(noerr) tc_ystarmat tc_dmat tc_wmat tc_cmat tc_cvec
      delete(noerr) tc_nopttargs tc_noptinstrus tc_nevl tc_ndrv tc_conv
      delete(noerr) tc_damp tc_dmat1 tc_dmat2 tc_dmatshort

      endif
    endif

' **************************************************************************
' examine linearized solution

  if tc_iter >= tc_itmax then
    @uiprompt("iteration limit reached:  time-consistent iterations did not converge")
    'stop
    endif

' write instrument and target values back into their corresponding series
  smpl %evlstart {%evlstart} + !nevl + !ndrv - 2
  mtos(tc_xmat,_$_opt_instrus)
  mtos(tc_ymat,_$_opt_targs_sols)

' value of loss function for linearized solution
  smpl %evlstart %evlend
  matrix _$_opt_targ_vec = @vec(@convert(_$_opt_targs_sols))
  matrix _$_opt_gap_vec = _$_opt_targ_vec - _$_opt_des_vec
  !optloss_lin = @sum(@transpose(_$_opt_gap_vec)*_$_opt_wt_mat*_$_opt_gap_vec)
  stom(_$_opt_targs_sols,tc_ymat_lin)

' simulate EViews model
  smpl %mcestart %mceend
  call mcz_sim("  ")
  smpl %evlstart %evlend
  stom(_$_opt_targs_sols,tc_ymat_ev)
  !maxdiff = @max(@abs(tc_ymat_ev-tc_ymat_lin))
  matrix _$_opt_targ_vec = @vec(@convert(_$_opt_targs_sols))
  matrix _$_opt_gap_vec = _$_opt_targ_vec - _$_opt_des_vec
  !optloss_ev = @sum(@transpose(_$_opt_gap_vec)*_$_opt_wt_mat*_$_opt_gap_vec)

  !tciter = tc_iter
  mce_opt_text.append time-consistent solution
  mce_opt_text.append --- backward-induction iterations = !tciter
  mce_opt_text.append --- TC damping factor = !tcdamp
  mce_opt_text.append --- linearized solution loss = !optloss_lin
  mce_opt_text.append --- EViews solution loss = !optloss_ev
  mce_opt_text.append --- max diff btwn target vars in linear and EViews sols = !maxdiff
  
  delete(noerr) tc_ymat tc_xmat tc_iter tc_ymat_lin tc_ymat_ev
  delete(noerr) tc_itmax tc_stat

endsub


'**************************************************************************
'**************************************************************************
'**************************************************************************
subroutine mcz_opt_deriv
'
' requires that the following be defined
'
' !optshow, mce_opt_stats, %evlstart, %evlend
' _$_opt_targs_sols, !noptinstrus, _$_opt_instrus, !drv,
' _$_opt_ptrb_mat, _$_opt_der_mat

' This subroutine computes the derivatives of the loss function targets wrt the instruments

  smpl @all

  !optshow_bac = !optshow
  !optshow = 3

  'make a copy of the current values of the mce instruments; 
  'they will be restored after each derivative is computed
  smpl %mcestart %mceend
  stom(_$_mce_instrus,_$_instrus_bac)

  'do not reset terminal conditions when computing derivatives
  %set_terminal = "no"

  smpl %evlstart %evlend 
  matrix _$_opt_targ_vec = @vec(@convert(_$_opt_targs_sols))

  '*********************************************
  'derivatives loop
  statusline computing instrument derivatives at optimization iteration !opttry
  for !zi = 1 to !noptinstrus
    %instru_name = _$_opt_instrus.@seriesname(!zi)
    for !zj = 1 to !ndrv
      !perturbit =  _$_opt_ptrb_mat(!zj,!zi)
      smpl %mcestart + !zj-1 %mcestart + !zj-1
      {%instru_name} = {%instru_name} + !perturbit
      call mcz_sim(" ")
      smpl %evlstart %evlend 
      matrix _$_opt_targ_dvec = @vec(@convert(_$_opt_targs_sols))
      matplace(_$_opt_der_mat,@transpose(_$_opt_targ_dvec-_$_opt_targ_vec)/!perturbit,(!zi-1)*!ndrv+!zj,1)
      smpl %mcestart + !zj-1 %mcestart + !zj-1
      {%instru_name} = {%instru_name} - !perturbit 
      'restore the original values of the add factors on 
      'the z variable equations
      smpl %mcestart %mceend
      mtos(_$_instrus_bac,_$_mce_instrus)
      next
    next

  smpl %mcestart %mceend

  !optshow = !optshow_bac

endsub


'****************************************************************
'****************************************************************
'****************************************************************
subroutine mcz_opt_solve

' This subroutine first sets the instrument values, based on the current
' optimal direction and choice of step size, and then solves the model

  statusline optimization solution, iteration !opttry
  smpl %mcestart %mceend

  'compute instrument values based on current direction and step size
  if !opttry > 0 then
    mce_opt_stats(!opttry+3,3) = !opt_step
    smpl %drvstart %drvend
    for !i = 1 to !noptinstrus
      vector _$_temp_adj = @subextract(_$_opt_direction,(!i-1)*!ndrv+1,1,!i*!ndrv,1)
      mtos(_$_temp_adj,_$_temp_ser)
      %y1 = _$_opt_instrus.@seriesname(!i)
      {%y1} = {%y1} - (!opt_step-!opt_step_prev)*_$_temp_ser
      next
    endif

  'solve model
  if !opttry = 0 and %terminal = "yes" then
    call mcz_sim("terminal")
    %terminal = "no"
    else
    call mcz_sim("  ")
    endif

  if !opttry = 0 then
    {%_$_mod_b}.makegroup _$_opt_targs_sols {%opt_targ_names}
    endif

  'compute value of loss function
  smpl %evlstart %evlend
  matrix _$_opt_targ_vec = @vec(@convert(_$_opt_targs_sols))
  matrix _$_opt_gap_vec = _$_opt_targ_vec - _$_opt_des_vec
  !optloss = @sum(@transpose(_$_opt_gap_vec)*_$_opt_wt_mat*_$_opt_gap_vec)
  setcell(mce_opt_stats,!opttry+3,1,!opttry,0)
  mce_opt_stats(!opttry+3,2) = !optloss
  _$_opt_loss_vec(!opttry+1) = !optloss
  
  if !optshow = 2 then
    show mce_opt_stats
    endif

endsub


'****************************************************************
'****************************************************************
'****************************************************************
subroutine local mcz_constraints(svector sss, table ctable,string cvarnames)


'This subroutine converts the inequality constraint text into a table 

' subroutine arguments:
'
' inputs:
'
'   sss                svector of constraint text
' 
' outputs:
'
'   _$_opt_cnstr_tab      table of constraint variables and coefficients
'   _$_opt_cnstr_vars     string of names of variables in constraints

' ***************************************

  !nconstraints = @rows(sss)
  scalar loc
  scalar sign

  for !i = 1 to !nconstraints
    %rrr = sss(!i)

  ' *****************************************
  ' *****************************************
  ' separate the left and right sides of each constraint

    !kk1 = @instr(%rrr,">=")
    if !kk1 = 0 then
      @uiprompt("each constraint must contain an "">="" term")
      stop
      endif
    %rrrl = @left(%rrr,!kk1-1)
    if @isempty(%rrrl) = 1 then
      @uiprompt("each constraint must contain terms to the left of "">=""")
      stop 
      endif
    %rrrr = @mid(%rrr,!kk1+2)
    if @isempty(%rrrr) = 1 then
      @uiprompt("each constraint must contain a value to the right of "">=""")
      stop 
      endif

  
  ' *****************************************
  ' *****************************************
  ' process the left side of each constraint
  ' 
  ' 1. split %rrrl into individual terms (based on + and - characters);
  '    the main challenge concerns the first term, which may or may not
  '    have a leading + or - attached 
  ' 2. then split each term into 2 parts (based on * character)
  ' 3. then identify which part is coefficient and which is variable

    !zz = 0    'flag to indicate end of left hand side of a constraint
    !nterms = 1
    vector(100) split_locs = 0
    vector(100) split_signs = 0
    %rrrlx = %rrrl

  ' find boundaries and signs of each term
    while !zz = 0

    ' first term: determine whether its sign is explicit or implicit
      if !nterms = 1 then                
        call find_next_delimit(%rrrlx,loc,sign)
        if loc = 0 then                  'implicit leading sign
          split_locs(!nterms) = 0
          split_signs(!nterms) = 1
          else
          %rrrll = @left(%rrrlx,loc-1)
          if @isempty(%rrrll) = 1 then   'explicit leading sign
            split_locs(!nterms) = loc
            split_signs(!nterms) = sign
            %rrrlx = @mid(%rrrlx,loc+1)
            else                         'implicit leading sign
            split_locs(!nterms) = 0
            split_signs(!nterms) = 1
            endif
          endif
        endif

      call find_next_delimit(%rrrlx,loc,sign)
      if loc = 0 then                  'last term
        split_locs(!nterms+1) = @length(%rrrlx)
        !zz = 1
        else
        split_locs(!nterms+1) = loc
        split_signs(!nterms+1) = sign
        %rrrlx = @mid(%rrrlx,loc+1)
        endif

      !nterms = !nterms + 1
      wend

    for !k = 2 to !nterms 
      split_locs(!k) = split_locs(!k-1) + split_locs(!k)
      next

    !nterms = !nterms - 1

     
  ' parse each term into coefficient times variable (they must 
  ' appear in that order, although coefficients = 1 can be 
  ' omitted), and store coefficient and variable in ctable
    for !k = 1 to !nterms
      %term = @mid(%rrrl,split_locs(!k)+1,split_locs(!k+1)-split_locs(!k)-1)
      if @isempty(%term) = 1 then
        @uiprompt("term is empty:  illegal constraint specification")
        stop
        else
        !ii = @instr(%term,"*")
        if !ii > 0 then
          %coef = @left(%term,!ii-1)
          !coef = split_signs(!k) * @val(%coef)
          %var =  @mid(%term,!ii+1)
          else
          !coef = split_signs(!k)
          %var = %term
          endif
        %var = @trim(%var)
        endif
  
      ctable(!i,2*!k) = %var
      ctable(!i,2*!k+1) = !coef
      cvarnames = cvarnames + " " + %var
      next


  ' *****************************************
  ' *****************************************
  ' process the right side of each constraint

    ctable(!i,2*!nterms+2) = @val(%rrrr)
    ctable(!i,1) = !nterms
  
  next

  cvarnames = @wunique(cvarnames)

endsub


 
'************************************************************
'************************************************************
'************************************************************
subroutine local shiftleft(vector abc, scalar nshift)

  !rows = @rows(abc)
  vector v1 = @subextract(abc,1,1,nshift,1)
  vector v2 = @subextract(abc,nshift+1,1,!rows,1)
  matplace(abc,v2,1)
  matplace(abc,v1,!rows - nshift + 1)
  
endsub


'****************************************************************
'****************************************************************
'****************************************************************
subroutine local find_next_delimit(string %instring,scalar loc,scalar sign) 

  !ttp = @instr(%instring,"+")
  !ttm = @instr(%instring,"-")
  if !ttp= 0 or !ttm = 0 then
    loc = !ttp + !ttm
    if !ttp = 0 and !ttm = 0 then
      sign = 1
      else
      sign = (!ttp > 0) - (!ttm > 0)
      endif
    else
    loc = !ttp*(!ttp < !ttm) + !ttm*(!ttm < !ttp)
    sign = (!ttp < !ttm) - (!ttm < !ttp)
  endif

endsub
  



' ****************************************************************** '
' ****************************************************************** '
' ******************************************************************
  subroutine mce_load_frbus(string frbus_opts)

' take two corresponding FRB/US models (eg, stdver and pfver) and make 
' the transformations needed to the second model so that the pair of 
' models can be used with the mcz_solve_subs programs

' parameters
'   required:  mce_vars, mod_b, mod_f, path_b, path_f
'   optional:  allbut, only
'     

  !allbut = 0
  !only = 0
 
  frbus_opts = @lower(frbus_opts)
  frbus_opts = @replace(frbus_opts," ","")
  frbus_opts = @replace(frbus_opts,","," ")
  frbus_opts = " " + frbus_opts + " "

  if @isempty(frbus_opts) = 0 then
    call mcz_equalopt("mce_vars",frbus_opts)
    if @len(%temp)>0 then
      %mce_vars  = @lower({%temp})
      else
      @uiprompt("Error:  mce_load_frbus sub requires the mce_vars argument")
      stop
      endif
    call mcz_equalopt("mod_b",frbus_opts)
    if @len(%temp)>0 then
      %mod_b  = {%temp}
      else
      @uiprompt("Error:  mce_load_frbus sub requires the mod_b argument")
      stop
      endif
    call mcz_equalopt("mod_f",frbus_opts)
    if @len(%temp)>0 then
      %mod_f  = {%temp}
      else
      @uiprompt("Error:  mce_load_frbus sub requires the mod_f argument")
      stop
      endif
    call mcz_equalopt("path_b",frbus_opts)
    if @len(%temp)>0 then
      %path_b  = {%temp}
      else
      @uiprompt("Error:  mce_load_frbus sub requires the path_b argument")
      stop
      endif
    call mcz_equalopt("path_f",frbus_opts)
    if @len(%temp)>0 then
      %path_f  = {%temp}
      else
      @uiprompt("Error:  mce_load_frbus sub requires the path_f argument")
      stop
      endif
    call mcz_equalopt("allbut",frbus_opts)
    if @len(%temp)>0 then
      !allbut = 1
      %allbut  = {%temp}
      else
      call mcz_equalopt("only",frbus_opts)
      if @len(%temp)>0 then
        !only = 1
        %only  = {%temp}
        endif
      endif
    endif

' Added so that users may ask directly for a group of forward-looking equations instead of 
' passing in a list that may change in the future.

' Model consistent Asset Pricing
  %s_mcap = " zdivgr zgap05 zgap10 zpi10f zpic30 zrff10 zrff5 zgap30 zrff30 zpi10 zpib5 zpic58"

' Wages and prices
  %s_wp = " zpicxfe zpieci "

' Others - all PAC expectations 
  %s_other = " zecd zeco zeh zgapc2 zlhp zpi5 zvpd zvps zvpi zxnfbd zxnfbs zxnfbi zyh zyhp zyht zynid "

  if %mce_vars = "-all" then
    %mce_vars = %s_mcap + %s_wp + %s_other
    endif
  if %mce_vars = "-mcap" then
    %mce_vars = %s_mcap
    endif
  if %mce_vars = "-wp" then
    %mce_vars = %s_wp
    endif
  if %mce_vars = "-mcap+wp" then
    %mce_vars = %s_mcap + %s_wp
    endif
  if @left(%mce_vars, 7) = "-allbut" then
    %tmp = %mce_vars
    %s_remove = @replace(%tmp, "-allbut", "")
    %mce_vars = @wnotin(%s_mcap + %s_wp + %s_other, %s_remove)
    endif
  string zvar_list = %mce_vars
  %zvars = %mce_vars


' backward-looking model

  ld_frbus_cfs(modelname=%mod_b,modelpath=%path_b)
  if !allbut = 1 then
    %tmp = "allbut " + %allbut
    ld_some_eqs(modelname=%mod_b,modelpath=%path_b,eqnames=%tmp)
    endif
  if !only = 1 then
    %tmp = %only
    ld_some_eqs(modelname=%mod_b,modelpath=%path_b,eqnames=%tmp)
    endif
  if !allbut = 0 and !only = 0 then
    ld_frbus_eqs(modelname=%mod_b,modelpath=%path_b)
    endif

' model with mce equations and errors 

  ld_mce_eqs(pfname=%mod_f,pfpath=%path_f,mcename=%mod_f,mceeqs=%mce_vars)
  ld_mce_cfs(pfname=%mod_f,pfpath=%path_f,mceeqs=%mce_vars)

  !nmcevars = @wcount(%mce_vars)
  %evars = @wcross("e",%mce_vars)
  for !i = 1 to !nmcevars
    %tmp = @word(%mce_vars,!i)
    %tmpw = " w" + @mid(%tmp,2)
    if %tmp <> "zyh" and %tmp <> "zyhp" and %tmp <> "zyht" then
      %tmp1 = @word(%evars,!i) + "=" + @word(%mce_vars,!i) + "-" + %tmpw
      else
      %tmp1 = @word(%evars,!i) + "= log(" + @word(%mce_vars,!i) + "/" + %tmpw + ")"
      endif
    {%mod_f}.append {%tmp1}
    next

endsub


' ****************************************************************** '
' ****************************************************************** '
' ******************************************************************
  subroutine make_frbus_mcevars(string frbus_mcevars)

' Create data for the w and e variables in the operational forward looking
' model over the workfile sample currently in effect
'
' The input string contains the names of the expectations variables that
' are to have MC solutions

  %mce_vars = frbus_mcevars

' The user may ask directly for a group of forward-looking equations instead of 
' passing in a list that may change in the future.

' Model consistent Asset Pricing
  %s_mcap = " zdivgr zgap05 zgap10 zpi10f zpic30 zrff10 zrff5 zgap30 zrff30 zpi10 zpib5 zpic58"

' Wages and prices
  %s_wp = " zpicxfe zpieci "

' Others - all PAC expectations 
  %s_other = " zecd zeco zeh zgapc2 zlhp zpi5 zvpd zvps zvpi zxnfbd zxnfbs zxnfbi zyh zyhp zyht zynid "

  if %mce_vars = "-all" then
    %mce_vars = %s_mcap + %s_wp + %s_other
    endif
  if %mce_vars = "-mcap" then
    %mce_vars = %s_mcap
    endif
  if %mce_vars = "-wp" then
    %mce_vars = %s_wp
    endif
  if %mce_vars = "-mcap+wp" then
    %mce_vars = %s_mcap + %s_wp
    endif
  if @left(%mce_vars, 7) = "-allbut" then
    %tmp = %mce_vars
    %s_remove = @replace(%tmp, "-allbut", "")
    %mce_vars = @wnotin(%s_mcap + %s_wp + %s_other, %s_remove)
    endif


' Data for extra variables associated with MC expectations
  smpl @all
  for !i = 1 to @wcount(%mce_vars)
    %tmp = @word(%mce_vars,!i)
    %wtmp = "w" + @mid(%tmp,2)
    %wtmp_aerr = %wtmp + "_aerr"
    %etmp = "e" + @word(%mce_vars,!i)
    series {%wtmp} = {%tmp}
    series {%wtmp_aerr} = 0
    series {%etmp} = 0
    next

endsub
