C|    ALEPH : remove UBRGEN code to interface to RNDM so we can use RANMAR
CDECK  ID>, UBPSRV. 
c Server skeleton for the lookup table UBP
      subroutine ubpsrv (result, action, name,
     &                   type, ival, rval, dval, lval)
      implicit none
      character*(*) result, action, name, type
      integer ival
      real rval
      double precision dval
      logical lval
      external ubpini
      integer i, n
      save n
      double precision ahpla
      common /ubpcom/  ahpla
      double precision alpha
      common /ubpcom/  alpha
      double precision mass1e
      common /ubpcom/  mass1e
      double precision mass2e
      common /ubpcom/  mass2e
      double precision mass1z
      common /ubpcom/  mass1z
      double precision mass2z
      common /ubpcom/  mass2z
      double precision mass1t
      common /ubpcom/  mass1t
      double precision mass1h
      common /ubpcom/  mass1h
      double precision mass1w
      common /ubpcom/  mass1w
      double precision gamm1z
      common /ubpcom/  gamm1z
      double precision gamm2z
      common /ubpcom/  gamm2z
      double precision sin2w
      common /ubpcom/  sin2w
      double precision alphas
      common /ubpcom/  alphas
      double precision ebeam
      common /ubpcom/  ebeam
      double precision epol
      common /ubpcom/  epol
      double precision ppol
      common /ubpcom/  ppol
      double precision ctsmin
      common /ubpcom/  ctsmin
      double precision ctsmax
      common /ubpcom/  ctsmax
      double precision ecut
      common /ubpcom/  ecut
      double precision acocut
      common /ubpcom/  acocut
      double precision evisct
      common /ubpcom/  evisct
      integer          nevent, pad022
      common /ubpcom/  nevent, pad022
      integer          rseed , pad023
      common /ubpcom/  rseed , pad023
      logical          tchann, pad024
      common /ubpcom/  tchann, pad024
      logical          qedvtx, pad025
      common /ubpcom/  qedvtx, pad025
      logical          qedbox, pad026
      common /ubpcom/  qedbox, pad026
      logical          weak  , pad027
      common /ubpcom/  weak  , pad027
      logical          boxes , pad028
      common /ubpcom/  boxes , pad028
      integer          isrtyp, pad029
      common /ubpcom/  isrtyp, pad029
      integer          fsrtyp, pad030
      common /ubpcom/  fsrtyp, pad030
      double precision epsiln
      common /ubpcom/  epsiln
      double precision taumin
      common /ubpcom/  taumin
      double precision taumax
      common /ubpcom/  taumax
      double precision emin
      common /ubpcom/  emin
      double precision eps2
      common /ubpcom/  eps2
      double precision qsq
      common /ubpcom/  qsq
      integer          bstyle, pad037
      common /ubpcom/  bstyle, pad037
      logical          nonlog, pad038
      common /ubpcom/  nonlog, pad038
      integer          stdin , pad039
      common /ubpcom/  stdin , pad039
      integer          stdout, pad040
      common /ubpcom/  stdout, pad040
      integer          stderr, pad041
      common /ubpcom/  stderr, pad041
      integer          errcnt, pad042
      common /ubpcom/  errcnt, pad042
      integer          errmax, pad043
      common /ubpcom/  errmax, pad043
      integer          verbos, pad044
      common /ubpcom/  verbos, pad044
      integer          status, pad045
      common /ubpcom/  status, pad045
      integer          hepdat, pad046
      common /ubpcom/  hepdat, pad046
      integer          heprev, pad047
      common /ubpcom/  heprev, pad047
      integer          runid , pad048
      common /ubpcom/  runid , pad048
      logical          dbgcol, pad049
      common /ubpcom/  dbgcol, pad049
      logical          dbghep, pad050
      common /ubpcom/  dbghep, pad050
      logical          dbgini, pad051
      common /ubpcom/  dbgini, pad051
      logical          dbgmas, pad052
      common /ubpcom/  dbgmas, pad052
      logical          dbgmup, pad053
      common /ubpcom/  dbgmup, pad053
c Declarations for the lookup table UBP
c   UBPLEN    is the size of the table (to be
c             used by the lookup routines).
c   UBPIVL(N) is the value which is equivalenced
c             with the common block used by the
c             client subroutines.
c   UBPNAM(N) is the name of the variable which
c             is used for the table lookup.
c   UBPTYP(N) is the data type of the variable:
c             'int':  integer
c             'real': real
c             'dble': double precision
c             'lgcl': logical
c   UBPPRM(N) are the 'permissions' of the
c             variable:
c             'w': this variable may be modified
c                  (written) by a client.
c             'r': this variable may be used (read)
c                  by a client (acually this will be
c                  true for almost all variables).
c             'x': if this variable is changed by a
c                  client, other clients will have to
c                  be reinitialized.
      integer          UBPLEN,
     $                 UBPLN2,
     $                 UBPLN3
      parameter       (UBPLEN =  53,
     $                 UBPLN2 = 2*UBPLEN,
     $                 UBPLN3 = 2*UBPLEN - 1)
      integer          ubpivl(UBPLN2)
      real             ubprvl(UBPLN2)
      double precision ubpdvl(UBPLEN)
      logical          ubplvl(UBPLN2)
      equivalence     (ubpivl(1),
     $                 ubprvl(1),
     $                 ubpdvl(1),
     $                 ubplvl(1)),
     $                (ubpivl(UBPLN3),
     $                 ubprvl(UBPLN3),
     $                 ubpdvl(UBPLEN),
     $                 ubplvl(UBPLN3))
      character*6      ubpnam(UBPLEN)
      character*4      ubptyp(UBPLEN)
      character*3      ubpprm(UBPLEN)
c Equivalence statements for the lookup table UBP
c (Actually we only need the first of these since
c the preprocessor takes care of alignment etc.,
c but having the others is a nice way of checking
c for strange word lengths and manually introduced
c inconsistencies.)
      equivalence (ubpdvl(001),ahpla )
      equivalence (ubpdvl(002),alpha )
      equivalence (ubpdvl(003),mass1e)
      equivalence (ubpdvl(004),mass2e)
      equivalence (ubpdvl(005),mass1z)
      equivalence (ubpdvl(006),mass2z)
      equivalence (ubpdvl(007),mass1t)
      equivalence (ubpdvl(008),mass1h)
      equivalence (ubpdvl(009),mass1w)
      equivalence (ubpdvl(010),gamm1z)
      equivalence (ubpdvl(011),gamm2z)
      equivalence (ubpdvl(012),sin2w )
      equivalence (ubpdvl(013),alphas)
      equivalence (ubpdvl(014),ebeam )
      equivalence (ubpdvl(015),epol  )
      equivalence (ubpdvl(016),ppol  )
      equivalence (ubpdvl(017),ctsmin)
      equivalence (ubpdvl(018),ctsmax)
      equivalence (ubpdvl(019),ecut  )
      equivalence (ubpdvl(020),acocut)
      equivalence (ubpdvl(021),evisct)
      equivalence (ubpivl(043),nevent)
      equivalence (ubpivl(045),rseed )
      equivalence (ubplvl(047),tchann)
      equivalence (ubplvl(049),qedvtx)
      equivalence (ubplvl(051),qedbox)
      equivalence (ubplvl(053),weak  )
      equivalence (ubplvl(055),boxes )
      equivalence (ubpivl(057),isrtyp)
      equivalence (ubpivl(059),fsrtyp)
      equivalence (ubpdvl(031),epsiln)
      equivalence (ubpdvl(032),taumin)
      equivalence (ubpdvl(033),taumax)
      equivalence (ubpdvl(034),emin  )
      equivalence (ubpdvl(035),eps2  )
      equivalence (ubpdvl(036),qsq   )
      equivalence (ubpivl(073),bstyle)
      equivalence (ubplvl(075),nonlog)
      equivalence (ubpivl(077),stdin )
      equivalence (ubpivl(079),stdout)
      equivalence (ubpivl(081),stderr)
      equivalence (ubpivl(083),errcnt)
      equivalence (ubpivl(085),errmax)
      equivalence (ubpivl(087),verbos)
      equivalence (ubpivl(089),status)
      equivalence (ubpivl(091),hepdat)
      equivalence (ubpivl(093),heprev)
      equivalence (ubpivl(095),runid )
      equivalence (ubplvl(097),dbgcol)
      equivalence (ubplvl(099),dbghep)
      equivalence (ubplvl(101),dbgini)
      equivalence (ubplvl(103),dbgmas)
      equivalence (ubplvl(105),dbgmup)
c Initialize the lookup table UBP
      data ubptyp(001) /'dble'/,
     $     ubpnam(001) /'ahpla '/,
     $     ubpprm(001) /'rwx'/
      data ubptyp(002) /'dble'/,
     $     ubpnam(002) /'alpha '/,
     $     ubpprm(002) /'r  '/
      data ubptyp(003) /'dble'/,
     $     ubpnam(003) /'mass1e'/,
     $     ubpprm(003) /'rwx'/
      data ubptyp(004) /'dble'/,
     $     ubpnam(004) /'mass2e'/,
     $     ubpprm(004) /'r  '/
      data ubptyp(005) /'dble'/,
     $     ubpnam(005) /'mass1z'/,
     $     ubpprm(005) /'rwx'/
      data ubptyp(006) /'dble'/,
     $     ubpnam(006) /'mass2z'/,
     $     ubpprm(006) /'r  '/
      data ubptyp(007) /'dble'/,
     $     ubpnam(007) /'mass1t'/,
     $     ubpprm(007) /'rwx'/
      data ubptyp(008) /'dble'/,
     $     ubpnam(008) /'mass1h'/,
     $     ubpprm(008) /'rwx'/
      data ubptyp(009) /'dble'/,
     $     ubpnam(009) /'mass1w'/,
     $     ubpprm(009) /'r  '/
      data ubptyp(010) /'dble'/,
     $     ubpnam(010) /'gamm1z'/,
     $     ubpprm(010) /'r  '/
      data ubptyp(011) /'dble'/,
     $     ubpnam(011) /'gamm2z'/,
     $     ubpprm(011) /'r  '/
      data ubptyp(012) /'dble'/,
     $     ubpnam(012) /'sin2w '/,
     $     ubpprm(012) /'r  '/
      data ubptyp(013) /'dble'/,
     $     ubpnam(013) /'alphas'/,
     $     ubpprm(013) /'rwx'/
      data ubptyp(014) /'dble'/,
     $     ubpnam(014) /'ebeam '/,
     $     ubpprm(014) /'rwx'/
      data ubptyp(015) /'dble'/,
     $     ubpnam(015) /'epol  '/,
     $     ubpprm(015) /'rwx'/
      data ubptyp(016) /'dble'/,
     $     ubpnam(016) /'ppol  '/,
     $     ubpprm(016) /'rwx'/
      data ubptyp(017) /'dble'/,
     $     ubpnam(017) /'ctsmin'/,
     $     ubpprm(017) /'rwx'/
      data ubptyp(018) /'dble'/,
     $     ubpnam(018) /'ctsmax'/,
     $     ubpprm(018) /'rwx'/
      data ubptyp(019) /'dble'/,
     $     ubpnam(019) /'ecut  '/,
     $     ubpprm(019) /'rwx'/
      data ubptyp(020) /'dble'/,
     $     ubpnam(020) /'acocut'/,
     $     ubpprm(020) /'rwx'/
      data ubptyp(021) /'dble'/,
     $     ubpnam(021) /'evisct'/,
     $     ubpprm(021) /'rwx'/
      data ubptyp(022) /'int '/,
     $     ubpnam(022) /'nevent'/,
     $     ubpprm(022) /'rw '/
      data ubptyp(023) /'int '/,
     $     ubpnam(023) /'rseed '/,
     $     ubpprm(023) /'rwx'/
      data ubptyp(024) /'lgcl'/,
     $     ubpnam(024) /'tchann'/,
     $     ubpprm(024) /'rwx'/
      data ubptyp(025) /'lgcl'/,
     $     ubpnam(025) /'qedvtx'/,
     $     ubpprm(025) /'rwx'/
      data ubptyp(026) /'lgcl'/,
     $     ubpnam(026) /'qedbox'/,
     $     ubpprm(026) /'rwx'/
      data ubptyp(027) /'lgcl'/,
     $     ubpnam(027) /'weak  '/,
     $     ubpprm(027) /'rwx'/
      data ubptyp(028) /'lgcl'/,
     $     ubpnam(028) /'boxes '/,
     $     ubpprm(028) /'rwx'/
      data ubptyp(029) /'int '/,
     $     ubpnam(029) /'isrtyp'/,
     $     ubpprm(029) /'rwx'/
      data ubptyp(030) /'int '/,
     $     ubpnam(030) /'fsrtyp'/,
     $     ubpprm(030) /'rwx'/
      data ubptyp(031) /'dble'/,
     $     ubpnam(031) /'epsiln'/,
     $     ubpprm(031) /'rwx'/
      data ubptyp(032) /'dble'/,
     $     ubpnam(032) /'taumin'/,
     $     ubpprm(032) /'r  '/
      data ubptyp(033) /'dble'/,
     $     ubpnam(033) /'taumax'/,
     $     ubpprm(033) /'r  '/
      data ubptyp(034) /'dble'/,
     $     ubpnam(034) /'emin  '/,
     $     ubpprm(034) /'r  '/
      data ubptyp(035) /'dble'/,
     $     ubpnam(035) /'eps2  '/,
     $     ubpprm(035) /'rwx'/
      data ubptyp(036) /'dble'/,
     $     ubpnam(036) /'qsq   '/,
     $     ubpprm(036) /'r  '/
      data ubptyp(037) /'int '/,
     $     ubpnam(037) /'bstyle'/,
     $     ubpprm(037) /'rwx'/
      data ubptyp(038) /'lgcl'/,
     $     ubpnam(038) /'nonlog'/,
     $     ubpprm(038) /'rwx'/
      data ubptyp(039) /'int '/,
     $     ubpnam(039) /'stdin '/,
     $     ubpprm(039) /'rw '/
      data ubptyp(040) /'int '/,
     $     ubpnam(040) /'stdout'/,
     $     ubpprm(040) /'rw '/
      data ubptyp(041) /'int '/,
     $     ubpnam(041) /'stderr'/,
     $     ubpprm(041) /'rw '/
      data ubptyp(042) /'int '/,
     $     ubpnam(042) /'errcnt'/,
     $     ubpprm(042) /'r  '/
      data ubptyp(043) /'int '/,
     $     ubpnam(043) /'errmax'/,
     $     ubpprm(043) /'rw '/
      data ubptyp(044) /'int '/,
     $     ubpnam(044) /'verbos'/,
     $     ubpprm(044) /'rw '/
      data ubptyp(045) /'int '/,
     $     ubpnam(045) /'status'/,
     $     ubpprm(045) /'r  '/
      data ubptyp(046) /'int '/,
     $     ubpnam(046) /'hepdat'/,
     $     ubpprm(046) /'r  '/
      data ubptyp(047) /'int '/,
     $     ubpnam(047) /'heprev'/,
     $     ubpprm(047) /'r  '/
      data ubptyp(048) /'int '/,
     $     ubpnam(048) /'runid '/,
     $     ubpprm(048) /'rw '/
      data ubptyp(049) /'lgcl'/,
     $     ubpnam(049) /'dbgcol'/,
     $     ubpprm(049) /'rwx'/
      data ubptyp(050) /'lgcl'/,
     $     ubpnam(050) /'dbghep'/,
     $     ubpprm(050) /'rw '/
      data ubptyp(051) /'lgcl'/,
     $     ubpnam(051) /'dbgini'/,
     $     ubpprm(051) /'rw '/
      data ubptyp(052) /'lgcl'/,
     $     ubpnam(052) /'dbgmas'/,
     $     ubpprm(052) /'rwx'/
      data ubptyp(053) /'lgcl'/,
     $     ubpnam(053) /'dbgmup'/,
     $     ubpprm(053) /'rwx'/
      if (name.eq.'1') then
        n = 1
        i = 1
        name = ubpnam(1)
        goto 11
      elseif (name.eq.'+') then
        if (n.ge.UBPLEN) then
          result = 'enoent'
          return
        endif
        n = n + 1
        i = n
        name = ubpnam(i)
        goto 11
      else
        do 10 i = 1, UBPLEN
          if (name.eq.ubpnam(i)) goto 11
   10   continue
      endif
c No entry.
      result = 'enoent'
      return
   11 continue
      if (action.eq.'write') then
        if (index (ubpprm(i), 'w').eq.0) then
          result = 'enoperm'
          return
        else
          if (index (ubpprm(i), 'x').ne.0) status = 0
          if (type.ne.ubptyp(i)) then
            result = 'enotype'
            return
          elseif (type.eq.'int') then
            ubpivl(2*i-1) = ival
          elseif (type.eq.'real') then
            ubprvl(2*i-1) = rval
          elseif (type.eq.'dble') then
            ubpdvl(i)     = dval
          elseif (type.eq.'lgcl') then
            ubplvl(2*i-1) = lval
          endif
          result = ' '
          return
        endif
      elseif (action.eq.'read') then
        if (index (ubpprm(i), 'r').eq.0) then
c Well, that's ridiculous, but anyway ...
          result = 'enoperm'
          return
        else
          type = ubptyp(i)
          if (type.eq.'int') then
            ival = ubpivl(2*i-1)
          elseif (type.eq.'real') then
            rval = ubprvl(2*i-1)
          elseif (type.eq.'dble') then
            dval = ubpdvl(i)
          elseif (type.eq.'lgcl') then
            lval = ubplvl(2*i-1)
          endif
          result = ' '
          return
        endif
      else
        result = 'enoarg'
        return
      endif
      end
CDECK  ID>, UBPINI. 
c Initialize the variables in the lookup table UBP
      block data ubpini
      double precision ahpla
      common /ubpcom/  ahpla
      double precision alpha
      common /ubpcom/  alpha
      double precision mass1e
      common /ubpcom/  mass1e
      double precision mass2e
      common /ubpcom/  mass2e
      double precision mass1z
      common /ubpcom/  mass1z
      double precision mass2z
      common /ubpcom/  mass2z
      double precision mass1t
      common /ubpcom/  mass1t
      double precision mass1h
      common /ubpcom/  mass1h
      double precision mass1w
      common /ubpcom/  mass1w
      double precision gamm1z
      common /ubpcom/  gamm1z
      double precision gamm2z
      common /ubpcom/  gamm2z
      double precision sin2w
      common /ubpcom/  sin2w
      double precision alphas
      common /ubpcom/  alphas
      double precision ebeam
      common /ubpcom/  ebeam
      double precision epol
      common /ubpcom/  epol
      double precision ppol
      common /ubpcom/  ppol
      double precision ctsmin
      common /ubpcom/  ctsmin
      double precision ctsmax
      common /ubpcom/  ctsmax
      double precision ecut
      common /ubpcom/  ecut
      double precision acocut
      common /ubpcom/  acocut
      double precision evisct
      common /ubpcom/  evisct
      integer          nevent, pad022
      common /ubpcom/  nevent, pad022
      integer          rseed , pad023
      common /ubpcom/  rseed , pad023
      logical          tchann, pad024
      common /ubpcom/  tchann, pad024
      logical          qedvtx, pad025
      common /ubpcom/  qedvtx, pad025
      logical          qedbox, pad026
      common /ubpcom/  qedbox, pad026
      logical          weak  , pad027
      common /ubpcom/  weak  , pad027
      logical          boxes , pad028
      common /ubpcom/  boxes , pad028
      integer          isrtyp, pad029
      common /ubpcom/  isrtyp, pad029
      integer          fsrtyp, pad030
      common /ubpcom/  fsrtyp, pad030
      double precision epsiln
      common /ubpcom/  epsiln
      double precision taumin
      common /ubpcom/  taumin
      double precision taumax
      common /ubpcom/  taumax
      double precision emin
      common /ubpcom/  emin
      double precision eps2
      common /ubpcom/  eps2
      double precision qsq
      common /ubpcom/  qsq
      integer          bstyle, pad037
      common /ubpcom/  bstyle, pad037
      logical          nonlog, pad038
      common /ubpcom/  nonlog, pad038
      integer          stdin , pad039
      common /ubpcom/  stdin , pad039
      integer          stdout, pad040
      common /ubpcom/  stdout, pad040
      integer          stderr, pad041
      common /ubpcom/  stderr, pad041
      integer          errcnt, pad042
      common /ubpcom/  errcnt, pad042
      integer          errmax, pad043
      common /ubpcom/  errmax, pad043
      integer          verbos, pad044
      common /ubpcom/  verbos, pad044
      integer          status, pad045
      common /ubpcom/  status, pad045
      integer          hepdat, pad046
      common /ubpcom/  hepdat, pad046
      integer          heprev, pad047
      common /ubpcom/  heprev, pad047
      integer          runid , pad048
      common /ubpcom/  runid , pad048
      logical          dbgcol, pad049
      common /ubpcom/  dbgcol, pad049
      logical          dbghep, pad050
      common /ubpcom/  dbghep, pad050
      logical          dbgini, pad051
      common /ubpcom/  dbgini, pad051
      logical          dbgmas, pad052
      common /ubpcom/  dbgmas, pad052
      logical          dbgmup, pad053
      common /ubpcom/  dbgmup, pad053
      data ahpla  /137.0359895d0       /
      data alpha  /0.0d0               /
      data mass1e /0.51099906d-3       /
      data mass2e /0.0d0               /
      data mass1z /91.1887d0           /
      data mass2z /0.0d0               /
      data mass1t /174.0               /
      data mass1h /300.0               /
      data mass1w /0.0d0               /
      data gamm1z /0.0d0               /
      data gamm2z /0.0d0               /
      data sin2w  /0.0d0               /
      data alphas /0.124d0             /
      data ebeam  /46.0d0              /
      data epol   /0.0                 /
      data ppol   /0.0                 /
      data ctsmin /-0.9                /
      data ctsmax /0.9                 /
      data ecut   /20.                 /
      data acocut /20.                 /
      data evisct /0.                  /
      data nevent /10000               /
      data rseed  /54217137            /
      data tchann /.true.              /
      data qedvtx /.true.              /
      data qedbox /.false.             /
      data weak   /.true.              /
      data boxes  /.true.              /
      data isrtyp /1                   /
      data fsrtyp /3                   /
      data epsiln /1.0d-5              /
      data taumin /0.0d0               /
      data taumax /0.0d0               /
      data emin   /0.0d0               /
      data eps2   /1.0d-3              /
      data qsq    /0.0d0               /
      data bstyle /3                   /
      data nonlog /.false.             /
      data stdin  /5                   /
      data stdout /6                   /
      data stderr /0                   /
      data errcnt /0                   /
      data errmax /100                 /
      data verbos /0                   /
      data status /0                   /
      data hepdat /0                   /
      data heprev /0                   /
      data runid  /0                   /
      data dbgcol /.false.             /
      data dbghep /.false.             /
      data dbgini /.false.             /
      data dbgmas /.false.             /
      data dbgmup /.false.             /
      end
CDECK  ID>, UBPPRN. 
c Print the name, type, and value of each entry
c in the table UBP
      subroutine ubpprn ()
      implicit none
      character*8 result, name, type
      integer ival
      real rval
      double precision dval
      logical lval
      write (6, 1000)
      name = '1'
      call ubpsrv (result, 'read', name,
     &             type, ival, rval, dval, lval)
   10 if(result.eq.' ') then
        if (type.eq.'int') then
          write (6, 1001) name, ival
        elseif (type.eq.'real') then
          write (6, 1002) name, rval
        elseif (type.eq.'dble') then
          write (6, 1003) name, dval
        elseif (type.eq.'lgcl') then
          write (6, 1004) name, lval
        endif
        name = '+'
        call ubpsrv (result, 'read', name,
     &               type, ival, rval, dval, lval)
      goto 10
      endif
 1000 format (3X,'Name  ',2X,'Data Type',2X,'Value'/,
     &        3X,'------',2X,'---------',2X,12('-'))
 1001 format (3X,A6,2X,'(integer)',2X,I12)
 1002 format (3X,A6,2X,'(real)   ',2X,E12.5)
 1003 format (3X,A6,2X,'(double) ',2X,E12.5)
 1004 format (3X,A6,2X,'(logical)',2X,L12)
      end
CDECK  ID>, *UNIBAB.
*CMZ :  1.02/99 03/05/93  18.02.11  by  Unknown
CDECK  ID>, BLANKDEK.   
*CMZ :  1.99/00 06/05/93  17.52.18  by  Harald Anlauf & Helge Meinhard
*CMZ :  1.02/99 03/05/93  18.02.11  by  Unknown
*-- Author :
CDECK  ID>, UBFLAGS.
*CMZ :  1.02/99 03/05/93  18.02.11  by  Unknown
CDECK  ID>, BLANKDEK.   
*CMZ :  2.01/05 23/03/95  14.27.43  by  Harald Anlauf
*CMZ :  2.01/00 30/09/94  17.01.17  by  Harald Anlauf
*CMZ :  1.99/07 02/06/93  23.58.11  by  Harald Anlauf & Helge Meinhard
*CMZ :  1.99/05 01/06/93  22.03.14  by  Harald Anlauf & Helge Meinhard
*CMZ :  1.99/00 06/05/93  17.57.23  by  Harald Anlauf & Helge Meinhard
*CMZ :  1.02/99 03/05/93  18.02.11  by  Thorsten Ohl
*-- Author :
CDECK  ID>, UBSOURCE.   
*CMZ :  1.02/99 03/05/93  18.02.11  by  Unknown
CDECK  ID>, BLANKDEK.   
*CMZ :  2.01/02 14/03/95  14.51.52  by  Harald Anlauf
*CMZ :  1.99/07 02/06/93  20.57.44  by  Harald Anlauf & Helge Meinhard
*CMZ :  1.99/06 01/06/93  23.55.20  by  Harald Anlauf & Helge Meinhard
*CMZ :  1.99/05 01/06/93  22.00.58  by  Harald Anlauf & Helge Meinhard
*CMZ :  1.99/00 06/05/93  17.53.31  by  Harald Anlauf & Helge Meinhard
*CMZ :  1.02/99 03/05/93  18.02.11  by  Unknown
*-- Author :
CDECK  ID>, UBCDES. 
*CMZ :- 1.02/99- 3/07/92  02.09.30  by  Thorsten Ohl <ohl@ips105>
CDECK  ID>, BLANKDEK.   
*CMZ :  2.01/06 30/03/95  22.08.38  by  Harald Anlauf
*CMZ :  1.02/99 03/05/93  18.02.11  by  Unknown
*-- Author :    Thorsten Ohl <ohl@ips105>

C This patch contains all common sequences used in UNIBAB, such as
C COMMON block declarations, EQUIVALENCE statements and some useful
C PARAMETER constants.

CDECK  ID>, HEPEVT. 
*CMZ :  1.99/05 01/06/93  15.57.08  by  Harald Anlauf & Helge Meinhard
*CMZ :- 1.02/99- 3/07/92  02.09.30  by  Thorsten Ohl <ohl@ips105>
*-- Author :    Thorsten Ohl <ohl@ips105>

c For the LEP Monte Carlos, a standard common block has been proposed
c in AKV89.  We strongly recommend its use.  (The description is an
c abbreviated transcription of AKV89, Vol. 3, pp. 327-330).
c
c
c NMXHEP is the maximum number of entries:
c
c
c NEVHEP is normally the event number, but may take special
c values as follows:
c
c    0   the program does not keep track of event numbers.
c   -1   a special initialization record.
c   -2   a special final record.
c
c
c NHEP holds the number of entries for this event.
c
c
c The entry ISTHEP(N) gives the status code for the Nth entry,
c with the following semantics:
c    0       a null entry.
c    1       an existing entry, which has not decayed or fragmented.
c    2       a decayed or fragmented entry, which is retained for
c            event history information.
c    3       documentation line.
c    4- 10   reserved for future standards.
c   11-200   at the disposal of each model builder.
c  201-      at the disposal of users.
c
c
c The Particle Data Group has proposed standard particle codes,
c which are to be stored in IDHEP(N).
c
c
c JMOHEP(1,N) points to the Nth entry's mother, if any.
c It is set to zero for initial entries.
c JMOHEP(2,N) points to the second mother, if any.
c
c
c JDAHEP(1,N) and JDAHEP(2,N) point to the Nth entry's first and
c last daughter, if any.  These are zero for entries which have not
c yet decayed.  The other daughters are stored in between these two.
c
c
c In PHEP we store the momentum of the particle, more specifically
c this means that PHEP(1,N), PHEP(2,N), and PHEP(3,N) contain the
c momentum in the x, y, and z direction (as defined by the machine
c people), measured in GeV/c.  PHEP(4,N) contains the energy in GeV
c and PHEP(5,N) the mass in GeV/c**2.  The latter may be negative for
c spacelike partons.
c
c
c Finally VHEP is the place to store the position of the production
c vertex.  VHEP(1,N), VHEP(2,N), and VHEP(3,N) contain the x, y,
c and z coordinate (as defined by the machine people), measured in mm.
c VHEP(4,N) contains the production time in mm/c.
c
c
c As an amendment to the proposed standard common block HEPEVT, we
c also have a polarisation common block HEPSPN, as described in
c AKV89.  SHEP(1,N), SHEP(2,N), and SHEP(3,N) give the x, y, and z
c component of the spinvector s of a fermion in the fermions
c restframe.
c
c By convention, SHEP(4,N) is always 1.

CDECK  ID>, IMPLNONE.   
*CMZ :- 1.02/99- 3/07/92  02.09.30  by  Thorsten Ohl <ohl@ips105>
*-- Author :    Thorsten Ohl <ohl@ips105>

c Almost all compilers will swallow this, but you
c can say 'select PEDANTIC' anyway.

CDECK  ID>, PDGCODES.   
*CMZ :- 1.02/99- 6/07/92  19.20.40  by  Thorsten Ohl <ohl@ips105>
*-- Author :    Thorsten Ohl <ohl@ips105>

c Symbolic names for some of the Particle Data Group particle codes

CDECK  ID>, UBCNST. 
*CMZ :  2.01/01 30/01/95  13.37.20  by  Harald Anlauf
*CMZ :  1.99/00 07/05/93  10.09.23  by  Harald Anlauf & Helge Meinhard
*CMZ :- 1.02/99-16/07/92  22.01.22  by  Thorsten Ohl <ohl@ips104>
*-- Author :    Thorsten Ohl <ohl@ips104>

c Commonly used constants:
c   PI       pi

CDECK  ID>, UBPCOM. 
*CMZ :  2.02/00 06/12/95  14.52.49  by  Harald Anlauf
*CMZ :  2.01/08 29/11/95  16.25.42  by  Harald Anlauf
*CMZ :  2.01/03 17/03/95  15.47.17  by  Harald Anlauf
*CMZ :  2.01/01 30/01/95  22.18.47  by  Harald Anlauf
*CMZ :  2.01/00 03/10/94  12.12.05  by  Harald Anlauf
*CMZ :  1.99/03 26/05/93  17.13.30  by  Harald Anlauf & Helge Meinhard
*CMZ :  1.99/00 11/05/93  17.59.56  by  Harald Anlauf & Helge Meinhard
*CMZ :- 1.02/99-16/07/92  00.46.04  by  Thorsten Ohl <ohl@ips104>
*-- Author :    Thorsten Ohl <ohl@ips104>

c Common block for the lookup table UBP
c NB: the entries PAD001, ..., PAD053 are never
c used, they're here just to allow the handling of
c variables of different lengths in the same table.
CDECK  ID>, UBCBRN. 
*CMZ :  2.01/01 30/01/95  13.37.20  by  Harald Anlauf
*CMZ :- 1.02/99- 6/07/92  22.37.48  by  Thorsten Ohl <ohl@ips105>
*-- Author :    Thorsten Ohl <ohl@ips105>

CDECK  ID>, UBCEVT. 
*CMZ :  2.01/08 29/11/95  16.24.16  by  Harald Anlauf
*CMZ :  1.99/05 01/06/93  15.48.59  by  Harald Anlauf & Helge Meinhard
*-- Author :    Harald Anlauf & Helge Meinhard   01/06/93

c For internal use we need the particle momenta with double precision.

CDECK  ID>, UBCMSC. 
*CMZ :  2.01/01 30/01/95  13.37.20  by  Harald Anlauf
*CMZ :  1.99/05 01/06/93  21.47.23  by  Harald Anlauf & Helge Meinhard
*CMZ :  1.99/01 14/05/93  12.18.43  by  Harald Anlauf & Helge Meinhard
*CMZ :- 1.02/99-15/07/92  12.29.39  by  Thorsten Ohl <ohl@ips104>
*-- Author :    Thorsten Ohl <ohl@ips104>

c Varia that don't fit elsewhere

CDECK  ID>, UBCSTA. 
*CMZ :  2.01/06 29/03/95  14.31.05  by  Harald Anlauf
*CMZ :  2.01/05 24/03/95  00.42.45  by  Harald Anlauf
*CMZ :  2.01/01 30/01/95  21.38.49  by  Harald Anlauf
*CMZ :  1.99/05 01/06/93  14.04.32  by  Harald Anlauf & Helge Meinhard
*CMZ :  1.99/01 14/05/93  11.06.54  by  Harald Anlauf & Helge Meinhard
*CMZ :  1.99/00 07/05/93  10.07.04  by  Harald Anlauf & Helge Meinhard
*CMZ :- 1.02/99- 6/07/92  22.37.17  by  Thorsten Ohl <ohl@ips105>
*-- Author :    Thorsten Ohl <ohl@ips105>

c Statistics

CDECK  ID>, UBCTRI. 
*CMZ :- 1.02/99-16/07/92  00.32.15  by  Thorsten Ohl <ohl@ips104>
*-- Author :    Thorsten Ohl <ohl@ips104>

CDECK  ID>, EWCDES. 
*CMZ :  1.99/00 06/05/93  17.46.22  by  Harald Anlauf & Helge Meinhard
CDECK  ID>, IMPLICIT.   
*CMZ :  1.99/00 04/05/93  18.12.27  by  Harald Anlauf & Helge Meinhard
*-- Author :    Harald Anlauf & Helge Meinhard   04/05/93

c In principle it is a good (Fortran) programming practice to use
c     IMPLICIT NONE
c however, the orginal code taken from ALIBABA still uses:
c     IMPLICIT REAL*8(A-H,O-Z)

CDECK  ID>, PARAMS. 
*CMZ :  1.99/00 06/05/93  12.38.44  by  Harald Anlauf & Helge Meinhard
*-- Author :    Harald Anlauf & Helge Meinhard   06/05/93

c Constant parameters

CDECK  ID>, MASSES. 
*CMZ :  2.01/08 29/11/95  16.20.33  by  Harald Anlauf
*CMZ :  1.99/00 06/05/93  12.38.44  by  Harald Anlauf & Helge Meinhard
*-- Author :    Harald Anlauf   04/05/93

c Masses and couplings of standard model fermions

CDECK  ID>, FORMFA. 
*CMZ :  1.99/00 04/05/93  18.11.14  by  Harald Anlauf & Helge Meinhard
*-- Author :    Harald Anlauf   04/05/93

c Formfactors

CDECK  ID>, FORMMZ. 
*CMZ :  1.99/00 04/05/93  18.11.14  by  Harald Anlauf & Helge Meinhard
*-- Author :    Harald Anlauf   04/05/93

c Formfactors at Q**2 = MZ**2

CDECK  ID>, BOSONS. 
*CMZ :  2.01/08 24/11/95  02.03.23  by  Harald Anlauf
*CMZ :  1.99/00 04/05/93  18.05.45  by  Harald Anlauf & Helge Meinhard
*-- Author :    Harald Anlauf   04/05/93

c (renormalized) parameters of the weak bosons etc.

CDECK  ID>, ADHOC.  
*CMZ :  2.01/08 29/11/95  16.18.29  by  Harald Anlauf
*CMZ :  2.00/01 29/09/94  17.31.35  by  Harald Anlauf
*CMZ :  1.99/00 06/05/93  15.34.08  by  Harald Anlauf & Helge Meinhard
*-- Author :    Harald Anlauf   04/05/93

c some parameters and constants internal to electroweak library
c
c HBARC2 from Particle Data Group Publ. 1994.

CDECK  ID>, OPTION. 
*CMZ :  1.99/00 06/05/93  20.58.59  by  Harald Anlauf & Helge Meinhard
*-- Author :    Harald Anlauf   04/05/93

c Run-time options for cross section calculations

CDECK  ID>, RENORM. 
*CMZ :  1.99/00 04/05/93  18.04.21  by  Harald Anlauf & Helge Meinhard
*-- Author :    Harald Anlauf   04/05/93

c Renormalization constants

CDECK  ID>, QCDCOM. 
*CMZ :  2.01/08 17/11/95  19.03.05  by  Harald Anlauf
*-- Author :    Harald Anlauf   09/11/95

c QCDCOM - Common block with parameters relevant to QCD corrections

CDECK  ID>, DRIVER. 
*CMZ :- 1.02/99-15/07/92  14.55.42  by  Thorsten Ohl <ohl@ips104>
CDECK  ID>, BLANKDEK.   
*CMZ :  2.01/06 30/03/95  22.06.14  by  Harald Anlauf
*CMZ :  1.02/99 03/05/93  18.02.11  by  Unknown
*-- Author :    Thorsten Ohl <ohl@ips104>

c This patch contains the main command interpreter.  Most of this
c code is identical to the KRONOS command interpreter.

CDECK  ID>, UBDRIV. 
*CMZ :  2.01/08 18/10/95  17.26.45  by  Harald Anlauf
*CMZ :  2.01/06 01/04/95  18.31.53  by  Harald Anlauf
*CMZ :  2.01/05 23/03/95  14.21.12  by  Harald Anlauf
*CMZ :  2.01/02 14/03/95  14.57.15  by  Harald Anlauf
*CMZ :  2.01/00 10/01/95  15.03.15  by  Harald Anlauf
*CMZ :  2.00/00 31/08/93  14.43.05  by  Harald Anlauf & Helge Meinhard
*CMZ :  1.99/01 11/05/93  20.17.57  by  Harald Anlauf & Helge Meinhard
*CMZ :  1.99/00 11/05/93  11.29.50  by  Harald Anlauf & Helge Meinhard
*CMZ :  1.00/13 03/04/92  19.47.21  by  Thorsten Ohl
*CMZ : 00.99/09 16/09/91  19.22.44  by  Thorsten Ohl
*-- Author :    Thorsten Ohl   09/04/91

c Main program, reading commands from initialization files (if
c available) and from standard input.

      program ubdriv
      implicit none

      double precision ahpla
      common /ubpcom/  ahpla
      double precision alpha
      common /ubpcom/  alpha
      double precision mass1e
      common /ubpcom/  mass1e
      double precision mass2e
      common /ubpcom/  mass2e
      double precision mass1z
      common /ubpcom/  mass1z
      double precision mass2z
      common /ubpcom/  mass2z
      double precision mass1t
      common /ubpcom/  mass1t
      double precision mass1h
      common /ubpcom/  mass1h
      double precision mass1w
      common /ubpcom/  mass1w
      double precision gamm1z
      common /ubpcom/  gamm1z
      double precision gamm2z
      common /ubpcom/  gamm2z
      double precision sin2w
      common /ubpcom/  sin2w
      double precision alphas
      common /ubpcom/  alphas
      double precision ebeam
      common /ubpcom/  ebeam
      double precision epol
      common /ubpcom/  epol
      double precision ppol
      common /ubpcom/  ppol
      double precision ctsmin
      common /ubpcom/  ctsmin
      double precision ctsmax
      common /ubpcom/  ctsmax
      double precision ecut
      common /ubpcom/  ecut
      double precision acocut
      common /ubpcom/  acocut
      double precision evisct
      common /ubpcom/  evisct
      integer          nevent, pad022
      common /ubpcom/  nevent, pad022
      integer          rseed , pad023
      common /ubpcom/  rseed , pad023
      logical          tchann, pad024
      common /ubpcom/  tchann, pad024
      logical          qedvtx, pad025
      common /ubpcom/  qedvtx, pad025
      logical          qedbox, pad026
      common /ubpcom/  qedbox, pad026
      logical          weak  , pad027
      common /ubpcom/  weak  , pad027
      logical          boxes , pad028
      common /ubpcom/  boxes , pad028
      integer          isrtyp, pad029
      common /ubpcom/  isrtyp, pad029
      integer          fsrtyp, pad030
      common /ubpcom/  fsrtyp, pad030
      double precision epsiln
      common /ubpcom/  epsiln
      double precision taumin
      common /ubpcom/  taumin
      double precision taumax
      common /ubpcom/  taumax
      double precision emin
      common /ubpcom/  emin
      double precision eps2
      common /ubpcom/  eps2
      double precision qsq
      common /ubpcom/  qsq
      integer          bstyle, pad037
      common /ubpcom/  bstyle, pad037
      logical          nonlog, pad038
      common /ubpcom/  nonlog, pad038
      integer          stdin , pad039
      common /ubpcom/  stdin , pad039
      integer          stdout, pad040
      common /ubpcom/  stdout, pad040
      integer          stderr, pad041
      common /ubpcom/  stderr, pad041
      integer          errcnt, pad042
      common /ubpcom/  errcnt, pad042
      integer          errmax, pad043
      common /ubpcom/  errmax, pad043
      integer          verbos, pad044
      common /ubpcom/  verbos, pad044
      integer          status, pad045
      common /ubpcom/  status, pad045
      integer          hepdat, pad046
      common /ubpcom/  hepdat, pad046
      integer          heprev, pad047
      common /ubpcom/  heprev, pad047
      integer          runid , pad048
      common /ubpcom/  runid , pad048
      logical          dbgcol, pad049
      common /ubpcom/  dbgcol, pad049
      logical          dbghep, pad050
      common /ubpcom/  dbghep, pad050
      logical          dbgini, pad051
      common /ubpcom/  dbgini, pad051
      logical          dbgmas, pad052
      common /ubpcom/  dbgmas, pad052
      logical          dbgmup, pad053
      common /ubpcom/  dbgmup, pad053

      integer ubdcmd

      character*72 buffer
      logical batchp
      integer i
      logical isatty

      integer SIGTERM
      parameter (SIGTERM = 15)
      external ubdsig
      integer flag

      data batchp /.false./

c Try to find out whether we're interactive.
c Definition: we're interactive iff standard input is from
c a terminal.
      batchp = .not. isatty (0)

c Be polite and say hello (showing your ID).
      if (ubdcmd ('banner').ne.0) goto 20

c *************************** User Interface ***************************
c Initialize the analyzer.  (Example: HEPAWK will compile its script.)
c     call hepawk ('init')

c Install our signal handler, so we can die gracefully (at least under
c UNIX).
      flag = -1
      call signal (SIGTERM, ubdsig, flag)

c Read startup files.
      call getenv ('HOME', buffer)
      i = index (buffer, ' ')
      if (i.gt.0) then
         buffer(i:) = '/.unibab'
         call ubdloo (-1, buffer, .false.)
      endif
      call ubdloo (-1, '.unibab', .false.)

c Read commands.
      call ubdloo (stdin, ' ', .not. batchp)

 20   continue
      call ubumsg ('ubdriv', -1, 'bye.')
      stop

      end
CDECK  ID>, UBDLOO. 
*CMZ :  2.01/05 23/03/95  14.48.48  by  Harald Anlauf
*CMZ :  2.01/02 06/03/95  21.04.29  by  Harald Anlauf
*CMZ :  2.00/00 31/08/93  14.43.05  by  Harald Anlauf & Helge Meinhard
*CMZ :  1.99/02 26/05/93  12.28.32  by  Harald Anlauf & Helge Meinhard
*CMZ :- 1.02/99-26/04/92  16.44.10  by  Thorsten Ohl <ohl@ips01>
*-- Author :    Thorsten Ohl <ohl@ips01>

c UNIBAB command loop, if FD < 0, read commands from file FNAME
c (using unit -FD), else read commands from unit FD and prompt the
c user iff PROMPT is true.

      subroutine ubdloo (fd, fname, prompt)
      implicit none
      integer fd
      character*(*) fname
      logical prompt

      double precision ahpla
      common /ubpcom/  ahpla
      double precision alpha
      common /ubpcom/  alpha
      double precision mass1e
      common /ubpcom/  mass1e
      double precision mass2e
      common /ubpcom/  mass2e
      double precision mass1z
      common /ubpcom/  mass1z
      double precision mass2z
      common /ubpcom/  mass2z
      double precision mass1t
      common /ubpcom/  mass1t
      double precision mass1h
      common /ubpcom/  mass1h
      double precision mass1w
      common /ubpcom/  mass1w
      double precision gamm1z
      common /ubpcom/  gamm1z
      double precision gamm2z
      common /ubpcom/  gamm2z
      double precision sin2w
      common /ubpcom/  sin2w
      double precision alphas
      common /ubpcom/  alphas
      double precision ebeam
      common /ubpcom/  ebeam
      double precision epol
      common /ubpcom/  epol
      double precision ppol
      common /ubpcom/  ppol
      double precision ctsmin
      common /ubpcom/  ctsmin
      double precision ctsmax
      common /ubpcom/  ctsmax
      double precision ecut
      common /ubpcom/  ecut
      double precision acocut
      common /ubpcom/  acocut
      double precision evisct
      common /ubpcom/  evisct
      integer          nevent, pad022
      common /ubpcom/  nevent, pad022
      integer          rseed , pad023
      common /ubpcom/  rseed , pad023
      logical          tchann, pad024
      common /ubpcom/  tchann, pad024
      logical          qedvtx, pad025
      common /ubpcom/  qedvtx, pad025
      logical          qedbox, pad026
      common /ubpcom/  qedbox, pad026
      logical          weak  , pad027
      common /ubpcom/  weak  , pad027
      logical          boxes , pad028
      common /ubpcom/  boxes , pad028
      integer          isrtyp, pad029
      common /ubpcom/  isrtyp, pad029
      integer          fsrtyp, pad030
      common /ubpcom/  fsrtyp, pad030
      double precision epsiln
      common /ubpcom/  epsiln
      double precision taumin
      common /ubpcom/  taumin
      double precision taumax
      common /ubpcom/  taumax
      double precision emin
      common /ubpcom/  emin
      double precision eps2
      common /ubpcom/  eps2
      double precision qsq
      common /ubpcom/  qsq
      integer          bstyle, pad037
      common /ubpcom/  bstyle, pad037
      logical          nonlog, pad038
      common /ubpcom/  nonlog, pad038
      integer          stdin , pad039
      common /ubpcom/  stdin , pad039
      integer          stdout, pad040
      common /ubpcom/  stdout, pad040
      integer          stderr, pad041
      common /ubpcom/  stderr, pad041
      integer          errcnt, pad042
      common /ubpcom/  errcnt, pad042
      integer          errmax, pad043
      common /ubpcom/  errmax, pad043
      integer          verbos, pad044
      common /ubpcom/  verbos, pad044
      integer          status, pad045
      common /ubpcom/  status, pad045
      integer          hepdat, pad046
      common /ubpcom/  hepdat, pad046
      integer          heprev, pad047
      common /ubpcom/  heprev, pad047
      integer          runid , pad048
      common /ubpcom/  runid , pad048
      logical          dbgcol, pad049
      common /ubpcom/  dbgcol, pad049
      logical          dbghep, pad050
      common /ubpcom/  dbghep, pad050
      logical          dbgini, pad051
      common /ubpcom/  dbgini, pad051
      logical          dbgmas, pad052
      common /ubpcom/  dbgmas, pad052
      logical          dbgmup, pad053
      common /ubpcom/  dbgmup, pad053

      integer ubdcmd
      character*72 buffer
      logical filex

      if (fd.lt.0) then
         inquire (file=fname, exist=filex)
         if (filex) then
            open (-fd, file=fname)
 10         continue
              read (-fd, '(A72)', err=20, end=20) buffer
              if (ubdcmd (buffer).ne.0) goto 20
              goto 10
 20         continue
            close (-fd)
         endif
      else
 110     continue
c Prompt the user and get the next command
           if (prompt) write (stderr, '(A,$)') 'UNIBAB> '
           read (fd, '(A72)', err=120 , end=120 ) buffer
           if (ubdcmd (buffer).ne.0) goto 120
           goto 110
 120     continue
      endif

      end
CDECK  ID>, UBDCMD. 
*CMZ :  2.01/08 18/10/95  16.55.45  by  Harald Anlauf
*CMZ :  2.01/01 27/01/95  15.22.43  by  Harald Anlauf
*CMZ :  2.01/00 10/01/95  15.03.16  by  Harald Anlauf
*CMZ :  2.00/01 30/09/94  16.49.51  by  Harald Anlauf
*CMZ :  1.99/06 02/06/93  10.37.28  by  Harald Anlauf & Helge Meinhard
*CMZ :  1.99/02 26/05/93  12.11.51  by  Harald Anlauf & Helge Meinhard
*CMZ :  1.99/00 11/05/93  10.35.42  by  Harald Anlauf & Helge Meinhard
*CMZ :  1.01/06 20/07/92  20.55.58  by  Thorsten Ohl <ohl@ips104.desy.de>
*CMZ :          20/04/92  23.48.31  by  Thorsten Ohl
*-- Author :    Thorsten Ohl   16/05/91

c Interpret a user command.  Return 0 if further commands shall be
c executed, < 0 on error, and > 0 on exit.

c At the moment, the following commands are understood:
c
c    * init
c      Force initialization of UNIBAB and write an initialization
c      record into /hepevt/, which should trigger the necessary
c      initializations in the analyzer.
c
c    * gen
c      Generate NEVENT events and call hepawk () to analyze them.
c
c    * close
c      Write a deinitialization record to /hepevt/, which should
c      trigger the necessary cleanups in the analyzer.
c
c    * quit
c      Terminate UNIBAB without writing a deinitialization
c      record.
c
c    * exit|bye
c      Write a deinitialization record and terminate UNIBAB.
c
c    * set <variable> <ival>|<rval>
c      Set physical or internal parameters.
c
c    * print <variable>|all
c      Print the value of physical or internal variables.
c
c    * debug|nodebug <flag>
c      Switch debugging flags.
c
c    * testran
c      Test the portability of the random number generator.
c
c    * banner
c      print a sring identifying this version of UNIBAB.


      integer function ubdcmd (cmdlin)
      implicit none
      character*(*) cmdlin

      double precision ahpla
      common /ubpcom/  ahpla
      double precision alpha
      common /ubpcom/  alpha
      double precision mass1e
      common /ubpcom/  mass1e
      double precision mass2e
      common /ubpcom/  mass2e
      double precision mass1z
      common /ubpcom/  mass1z
      double precision mass2z
      common /ubpcom/  mass2z
      double precision mass1t
      common /ubpcom/  mass1t
      double precision mass1h
      common /ubpcom/  mass1h
      double precision mass1w
      common /ubpcom/  mass1w
      double precision gamm1z
      common /ubpcom/  gamm1z
      double precision gamm2z
      common /ubpcom/  gamm2z
      double precision sin2w
      common /ubpcom/  sin2w
      double precision alphas
      common /ubpcom/  alphas
      double precision ebeam
      common /ubpcom/  ebeam
      double precision epol
      common /ubpcom/  epol
      double precision ppol
      common /ubpcom/  ppol
      double precision ctsmin
      common /ubpcom/  ctsmin
      double precision ctsmax
      common /ubpcom/  ctsmax
      double precision ecut
      common /ubpcom/  ecut
      double precision acocut
      common /ubpcom/  acocut
      double precision evisct
      common /ubpcom/  evisct
      integer          nevent, pad022
      common /ubpcom/  nevent, pad022
      integer          rseed , pad023
      common /ubpcom/  rseed , pad023
      logical          tchann, pad024
      common /ubpcom/  tchann, pad024
      logical          qedvtx, pad025
      common /ubpcom/  qedvtx, pad025
      logical          qedbox, pad026
      common /ubpcom/  qedbox, pad026
      logical          weak  , pad027
      common /ubpcom/  weak  , pad027
      logical          boxes , pad028
      common /ubpcom/  boxes , pad028
      integer          isrtyp, pad029
      common /ubpcom/  isrtyp, pad029
      integer          fsrtyp, pad030
      common /ubpcom/  fsrtyp, pad030
      double precision epsiln
      common /ubpcom/  epsiln
      double precision taumin
      common /ubpcom/  taumin
      double precision taumax
      common /ubpcom/  taumax
      double precision emin
      common /ubpcom/  emin
      double precision eps2
      common /ubpcom/  eps2
      double precision qsq
      common /ubpcom/  qsq
      integer          bstyle, pad037
      common /ubpcom/  bstyle, pad037
      logical          nonlog, pad038
      common /ubpcom/  nonlog, pad038
      integer          stdin , pad039
      common /ubpcom/  stdin , pad039
      integer          stdout, pad040
      common /ubpcom/  stdout, pad040
      integer          stderr, pad041
      common /ubpcom/  stderr, pad041
      integer          errcnt, pad042
      common /ubpcom/  errcnt, pad042
      integer          errmax, pad043
      common /ubpcom/  errmax, pad043
      integer          verbos, pad044
      common /ubpcom/  verbos, pad044
      integer          status, pad045
      common /ubpcom/  status, pad045
      integer          hepdat, pad046
      common /ubpcom/  hepdat, pad046
      integer          heprev, pad047
      common /ubpcom/  heprev, pad047
      integer          runid , pad048
      common /ubpcom/  runid , pad048
      logical          dbgcol, pad049
      common /ubpcom/  dbgcol, pad049
      logical          dbghep, pad050
      common /ubpcom/  dbghep, pad050
      logical          dbgini, pad051
      common /ubpcom/  dbgini, pad051
      logical          dbgmas, pad052
      common /ubpcom/  dbgmas, pad052
      logical          dbgmup, pad053
      common /ubpcom/  dbgmup, pad053

      character*72 buffer, chatok
      character*4 type
      character*6 varnam
      character*8 rcode
      character*55 msg

      integer i, n, ival, inttok, period, cmd
      integer ubtlup, ubutim
      logical ubtins
      real rval
      double precision dval, dbltok
      logical newflg, lval

      character*8  VERSQQ
      integer IDATQQ, ITIMQQ, IVERSQ

c patchy kludge:
      character*17 VERQQQ

      logical first
      save first
      data first /.true./

      if (first) then
         first = .false.

c Commands:

         if (.not. ubtins ('banner',        11)) goto 99
         if (.not. ubtins ('echo',          12)) goto 99
         if (.not. ubtins ('print',         13)) goto 99

         if (.not. ubtins ('set',           22)) goto 99

         if (.not. ubtins ('initialize',    31)) goto 99

         if (.not. ubtins ('generate',      41)) goto 99

         if (.not. ubtins ('close',         51)) goto 99
         if (.not. ubtins ('quit',          52)) goto 99
         if (.not. ubtins ('bye',           53)) goto 99
         if (.not. ubtins ('exit',          53)) goto 99
         if (.not. ubtins ('statistics',    54)) goto 99

         if (.not. ubtins ('debug',         61)) goto 99
         if (.not. ubtins ('nodebug',       62)) goto 99
         if (.not. ubtins ('testrandom',    63)) goto 99

         goto 100

c Error:
 99      continue
         call ubumsg ('ubdcmd', 100,
     $                'can''t enter keywords into lookup table')
         stop

c Success:
 100     continue

      endif

      buffer = cmdlin
      call ubulwr (buffer)
      if (verbos.ge.2) call ubumsg ('ubdcmd', 0, buffer)

c Assume continuation
      ubdcmd = 0

c     Extract the first token
      i = 1
      call ubdlxs (chatok, buffer, i)

      if (chatok(1:1).eq.'#' .or. buffer.eq.' ') then
c Do nothing, it's a comment

      else
         cmd = ubtlup (chatok)
         if (cmd.eq.-1) then
            call ubumsg ('ubdcmd', 10, 'ambiguous keyword: '//chatok)

         else if (cmd.eq.0) then
            call ubumsg ('ubdcmd', 10, 'undefined keyword: '//chatok)


         else if (cmd.eq.11) then
c This first version is for PATCHY compatibility, which doesn't support
c +SEQ, VERSQQ.
            write (VERQQQ, 8000)
 8000       format (
     + 17HUNIBAB    2.02/00                                                 HOLD
     &              )
            VERSQQ = VERQQQ(10:17)
*KEEP,VERSQQ.
      VERSQQ = ' 2.02/00'
      IVERSQ =  20200
*KEND.
      IDATQQ= 960509
      ITIMQQ=   1730
            write (msg, 10300) VERSQQ, IDATQQ, ITIMQQ
10300       format ('Starting UNIBAB, Version', A,
     $              ', (build ',I6,'/',I4.4,')')
            call ubumsg ('ubdcmd', -1, msg)

         else if (cmd.eq.12) then
            write (stdout, '(
     $                          A)') buffer(i+1:72)

         else if (cmd.eq.13) then
c The user wants to investigate a variable
            call ubdlxs (chatok, buffer, i)
            if (i.ne.-1) then
               if (chatok.eq.'all') then
                  call ubpprn ()
               else
                  type = '?'
                  call ubpsrv (rcode, 'read', chatok, type,
     $                         ival, rval, dval, lval)
                  if(type.eq.'int') then
                     write (msg, 10000) chatok, ival
10000                format (A6,' = ', I12)
                     call ubumsg ('ubdcmd', -1, msg)
                  else if (type.eq.'dble') then
                     write (msg, 10100) chatok, dval
10100                format (A6,' = ', E12.5)
                     call ubumsg ('ubdcmd', -1, msg)
                  else if (type.eq.'lgcl') then
                     if (lval) then
                        write (msg, 10200) chatok
10200                   format (A6,' = .true.')
                     else
                        write (msg, 10201) chatok
10201                   format (A6,' = .false.')
                     endif
                     call ubumsg ('ubdcmd', -1, msg)
                  else
                     call ubumsg ('ubdcmd', 10,
     +                            'undefined variable: '//chatok)
                  endif
               endif
            else
               call ubumsg ('ubdcmd', 10, 'print <variable name>')
               return
            endif


         else if (cmd.eq.22) then
c The user wants to set a variable
            call ubdlxs (chatok, buffer, i)
            if (i.ne.-1) then
               varnam = chatok(1:6)
               type = '?'
               call ubpsrv (rcode, 'read', varnam, type,
     $                      ival, rval, dval, lval)
               if (type.eq.'int') then
                  call ubdlxi (inttok, buffer, i)
                  if (i.ne.-1) then
                     call ubpsrv (rcode, 'write', varnam, type,
     $                            inttok, rval, dval, lval)
                  else
                     call ubumsg ('ubdcmd', 10,
     $                            'usage: set <name> <value>')
                  endif
               else if (type.eq.'dble') then
                  call ubdlxd (dbltok, buffer, i)
                  if (i.ne.-1) then
                     call ubpsrv (rcode, 'write', varnam, type,
     $                            ival, rval, dbltok, lval)
                  else
                     call ubumsg ('ubdcmd', 10,
     $                            'usage: set <name> <value>')
                  endif
               else if (type.eq.'lgcl') then
                  call ubdlxs (chatok, buffer, i)
                  lval = .true.
                  if (i.ne.-1 .and. (index (chatok, 'false').ne.0
     $                               .or. index (chatok, 'off').ne.0))
     $               lval = .false.
                  call ubpsrv (rcode, 'write', varnam, type,
     $                         ival, rval, dbltok, lval)
               else
                  call ubumsg ('ubdcmd', 10, 'undefined variable: '
     +                                       //varnam)
               endif
            endif


         else if (cmd.eq.31) then
c The user wants to reinitialize UNIBAB
            call unibab (0)
c *************************** User Interface ***************************
c           call hepawk ('scan')


         else if (cmd.eq.41) then
c The user wants to generate (and analyze) events
c An optional argument implies changing NEVENT!
            call ubdlxi (ival, buffer, i)
            if (i.ne.-1) call ubpsrv (rcode, 'write', 'nevent', 'int',
     $                                ival, rval, dval, lval)



c Trap for wrong bstyle (negativ bstyle etc.)
c For unknown bstyle set bstyle=3
      if (bstyle .lt. 0 .or. bstyle .gt. 3) then
         call ubumsg ('ubdcmd', 10, 'unknown bstyle ')
         call ubumsg ('ubdcmd', 10, 'continue with bstyle = 3 ')
         bstyle = 3
      endif



c Event loop:
            period = ubutim (0)
            do 10 n = 1,nevent
c check remaining time
               period = period - 1
               if (period.le.0) then
                  period = ubutim (n)
                  if (period.le.0) then
                     call ubumsg ('ubdcmd', 9,
     $                            'time-out during event generation.')
                     return
                  endif
               endif
c call generator
               call unibab (1)
c *************************** User Interface ***************************
c              call hepawk ('scan')

 10         continue


         else if (cmd.eq.51) then
c Usually, this will print some results
            call unibab (2)
c *************************** User Interface ***************************
c           call hepawk ('scan')


         else if (cmd.eq.52) then
c The user got tired of using Unibab, return immediately
            ubdcmd = 1

         else if (cmd.eq.53) then
c The user got tired of using Unibab, return after producing
c some output
            call unibab (2)
c *************************** User Interface ***************************
c           call hepawk ('scan')
            ubdcmd = 1


         else if (cmd.eq.54) then
c Print statistics:
            call ubstat ()


         else if (cmd.eq.61 .or. cmd.eq.62) then
c Debugging:
c decide which action to take
            if (cmd.eq.61) then
c The user wants to see more output ...
               newflg = .true.
               call ubdlxs (chatok, buffer, i)
               if (i.eq.-1)  then
                  call ubumsg ('ubdcmd', 10, 'usage: debug <flag>')
                  return
               endif
            else if (cmd.eq.62) then
c The user wants to see less output ...
               newflg = .false.
               call ubdlxs (chatok, buffer, i)
               if (i.eq.-1)  then
                  call ubumsg ('ubdcmd', 10, 'usage: nodebug <flag>')
                  return
               endif
            endif
c Decide, which flag to set.
            if (chatok.eq.'all') then
               dbghep = newflg
               dbgini = newflg
            else if (chatok.eq.'col') then
               dbgcol = newflg
            else if (chatok.eq.'hep') then
               dbghep = newflg
            else if (chatok.eq.'ini') then
               dbgini = newflg
            else if (chatok.eq.'mas') then
               dbgmas = newflg
            else if (chatok.eq.'mup') then
               dbgmup = newflg
            else
               call ubumsg ('ubdcmd', 10, 'unknown debugging flag: '
     +                      //chatok)
            endif

         else if (cmd.eq.63) then
c The user wants to test the portability of the random
c number generator
            call ubrtst ()

         else
c What's that?????
            call ubumsg ('ubdcmd', 10, 'undefined command: '//chatok)

         endif

      endif

      end
CDECK  ID>, UBDLXS. 
*CMZ :  0.05/01 22/05/91  20.22.25  by  Thorsten Ohl
*-- Author :    Thorsten Ohl   09/04/91

c UBDLXS (TOKEN, STRING, I) reads the next whitespace
c separated TOKEN, starting from position I in STRING.
c I is updated to point after the read token.

      subroutine ubdlxs (token, string, i)
      implicit none
      integer i
      character*(*) token, string

      integer j, imax, jmax

      imax = len (string)
      jmax = len (token)

      j = 1
      token = ' '

   10 if (string(i:i).eq.' ' .and. i.le.imax) then
         i = i + 1
         goto 10
      endif

      if (i.gt.imax) then
         i = -1
         return
      endif

   20 if (string(i:i).ne.' ' .and. i.le.imax .and. j.le.jmax) then
         token(j:j) = string(i:i)
         i = i + 1
         j = j + 1
         goto 20
      endif

      if ((i.gt.imax).or.(j.gt.jmax)) then
         i = -1
         return
      endif

      end


CDECK  ID>, UBDLXI. 
*CMZ :  0.05/01 22/05/91  20.22.25  by  Thorsten Ohl
*-- Author :    Thorsten Ohl   09/04/91

c UBDLXI (TOKEN, STRING, I) reads the next whitespace separated
c integer starting from position I in STRING and stores it in TOKEN.
c I is updated to point after the read token.

      subroutine ubdlxi (token, string, i)
      implicit none
      integer token, i
      character*(*) string

      character*72 buffer

      call ubdlxs (buffer, string, i)
      if (i.ne.-1) then
         read (buffer, '(bn,i72)', err = 10 ) token
         return
      endif

   10 i = -1

      end


CDECK  ID>, UBDLXD. 
*CMZ :  0.05/01 22/05/91  20.22.25  by  Thorsten Ohl
*-- Author :    Thorsten Ohl   09/04/91

c UBDLXD (TOKEN, STRING, I, IMAX) reads the next whitespace separated
c double precision number starting from position I in STRING and
c stores it in TOKEN.  I is updated to point after the read token.

      subroutine ubdlxd (token, string, i)
      implicit none
      double precision token
      integer i
      character*(*) string

      character*72 buffer

      call ubdlxs (buffer, string, i)
      if (i.ne.-1) then
         read (buffer, '(bn,e72.0)', err = 10 ) token
         return
      endif

   10 i = -1

      end


CDECK  ID>, UBDSIG. 
*CMZ :  2.01/05 23/03/95  14.21.12  by  Harald Anlauf
*CMZ :  2.01/02 06/03/95  21.04.29  by  Harald Anlauf
*CMZ :  2.01/00 30/09/94  17.07.35  by  Harald Anlauf
*CMZ :  1.99/00 11/05/93  10.35.42  by  Harald Anlauf & Helge Meinhard
*CMZ :  0.06/03 26/06/91  00.03.10  by  Thorsten Ohl
*-- Author :    Thorsten Ohl   07/06/91

c UNIX signal handler for UNIBAB.

      subroutine ubdsig ()
      implicit none

      double precision ahpla
      common /ubpcom/  ahpla
      double precision alpha
      common /ubpcom/  alpha
      double precision mass1e
      common /ubpcom/  mass1e
      double precision mass2e
      common /ubpcom/  mass2e
      double precision mass1z
      common /ubpcom/  mass1z
      double precision mass2z
      common /ubpcom/  mass2z
      double precision mass1t
      common /ubpcom/  mass1t
      double precision mass1h
      common /ubpcom/  mass1h
      double precision mass1w
      common /ubpcom/  mass1w
      double precision gamm1z
      common /ubpcom/  gamm1z
      double precision gamm2z
      common /ubpcom/  gamm2z
      double precision sin2w
      common /ubpcom/  sin2w
      double precision alphas
      common /ubpcom/  alphas
      double precision ebeam
      common /ubpcom/  ebeam
      double precision epol
      common /ubpcom/  epol
      double precision ppol
      common /ubpcom/  ppol
      double precision ctsmin
      common /ubpcom/  ctsmin
      double precision ctsmax
      common /ubpcom/  ctsmax
      double precision ecut
      common /ubpcom/  ecut
      double precision acocut
      common /ubpcom/  acocut
      double precision evisct
      common /ubpcom/  evisct
      integer          nevent, pad022
      common /ubpcom/  nevent, pad022
      integer          rseed , pad023
      common /ubpcom/  rseed , pad023
      logical          tchann, pad024
      common /ubpcom/  tchann, pad024
      logical          qedvtx, pad025
      common /ubpcom/  qedvtx, pad025
      logical          qedbox, pad026
      common /ubpcom/  qedbox, pad026
      logical          weak  , pad027
      common /ubpcom/  weak  , pad027
      logical          boxes , pad028
      common /ubpcom/  boxes , pad028
      integer          isrtyp, pad029
      common /ubpcom/  isrtyp, pad029
      integer          fsrtyp, pad030
      common /ubpcom/  fsrtyp, pad030
      double precision epsiln
      common /ubpcom/  epsiln
      double precision taumin
      common /ubpcom/  taumin
      double precision taumax
      common /ubpcom/  taumax
      double precision emin
      common /ubpcom/  emin
      double precision eps2
      common /ubpcom/  eps2
      double precision qsq
      common /ubpcom/  qsq
      integer          bstyle, pad037
      common /ubpcom/  bstyle, pad037
      logical          nonlog, pad038
      common /ubpcom/  nonlog, pad038
      integer          stdin , pad039
      common /ubpcom/  stdin , pad039
      integer          stdout, pad040
      common /ubpcom/  stdout, pad040
      integer          stderr, pad041
      common /ubpcom/  stderr, pad041
      integer          errcnt, pad042
      common /ubpcom/  errcnt, pad042
      integer          errmax, pad043
      common /ubpcom/  errmax, pad043
      integer          verbos, pad044
      common /ubpcom/  verbos, pad044
      integer          status, pad045
      common /ubpcom/  status, pad045
      integer          hepdat, pad046
      common /ubpcom/  hepdat, pad046
      integer          heprev, pad047
      common /ubpcom/  heprev, pad047
      integer          runid , pad048
      common /ubpcom/  runid , pad048
      logical          dbgcol, pad049
      common /ubpcom/  dbgcol, pad049
      logical          dbghep, pad050
      common /ubpcom/  dbghep, pad050
      logical          dbgini, pad051
      common /ubpcom/  dbgini, pad051
      logical          dbgmas, pad052
      common /ubpcom/  dbgmas, pad052
      logical          dbgmup, pad053
      common /ubpcom/  dbgmup, pad053

      integer rc, ubdcmd

      if (status.eq.0) then
         call ubumsg ('ubdsig', 100,
     $                'received SIGTERM during setup, killed.')
         stop
      elseif (status.eq.1) then
         call ubumsg ('ubdsig',  99,
     $                'received SIGTERM, trying to clean up.')
         rc = ubdcmd ('exit')
         call ubumsg ('ubdsig', -1, 'done.')
         stop
      elseif (status.eq.2) then
         call ubumsg ('ubdsig',  99,
     $                'received SIGTERM during cleanup, ignored.')
      else
         call ubumsg ('ubdsig', 100,
     $                'received SIGTERM, killed.')
         stop
      endif

      end
CDECK  ID>, UNIBAB. 
*CMZ :- 1.02/99- 3/07/92  01.51.55  by  Thorsten Ohl <ohl@ips105>
CDECK  ID>, BLANKDEK.   
*CMZ :  2.01/06 30/03/95  22.01.41  by  Harald Anlauf
*CMZ :  1.02/99 03/05/93  18.02.12  by  Unknown
*-- Author :    Thorsten Ohl <ohl@ips105>

c Top level of the low level event generator.


CDECK  ID>, UNIBAB. 
*CMZ :  1.99/02 26/05/93  11.47.56  by  Harald Anlauf & Helge Meinhard
*CMZ :- 1.02/99-14/07/92  22.26.19  by  Thorsten Ohl <ohl@ips104>
*-- Author :    Thorsten Ohl <ohl@ips104>

c Main (low level) entry point
c
c    CODE = 0:  initialize
c           1:  generate
c           2:  deinitialize
c

      subroutine unibab (code)
      implicit none
      integer code

      double precision ahpla
      common /ubpcom/  ahpla
      double precision alpha
      common /ubpcom/  alpha
      double precision mass1e
      common /ubpcom/  mass1e
      double precision mass2e
      common /ubpcom/  mass2e
      double precision mass1z
      common /ubpcom/  mass1z
      double precision mass2z
      common /ubpcom/  mass2z
      double precision mass1t
      common /ubpcom/  mass1t
      double precision mass1h
      common /ubpcom/  mass1h
      double precision mass1w
      common /ubpcom/  mass1w
      double precision gamm1z
      common /ubpcom/  gamm1z
      double precision gamm2z
      common /ubpcom/  gamm2z
      double precision sin2w
      common /ubpcom/  sin2w
      double precision alphas
      common /ubpcom/  alphas
      double precision ebeam
      common /ubpcom/  ebeam
      double precision epol
      common /ubpcom/  epol
      double precision ppol
      common /ubpcom/  ppol
      double precision ctsmin
      common /ubpcom/  ctsmin
      double precision ctsmax
      common /ubpcom/  ctsmax
      double precision ecut
      common /ubpcom/  ecut
      double precision acocut
      common /ubpcom/  acocut
      double precision evisct
      common /ubpcom/  evisct
      integer          nevent, pad022
      common /ubpcom/  nevent, pad022
      integer          rseed , pad023
      common /ubpcom/  rseed , pad023
      logical          tchann, pad024
      common /ubpcom/  tchann, pad024
      logical          qedvtx, pad025
      common /ubpcom/  qedvtx, pad025
      logical          qedbox, pad026
      common /ubpcom/  qedbox, pad026
      logical          weak  , pad027
      common /ubpcom/  weak  , pad027
      logical          boxes , pad028
      common /ubpcom/  boxes , pad028
      integer          isrtyp, pad029
      common /ubpcom/  isrtyp, pad029
      integer          fsrtyp, pad030
      common /ubpcom/  fsrtyp, pad030
      double precision epsiln
      common /ubpcom/  epsiln
      double precision taumin
      common /ubpcom/  taumin
      double precision taumax
      common /ubpcom/  taumax
      double precision emin
      common /ubpcom/  emin
      double precision eps2
      common /ubpcom/  eps2
      double precision qsq
      common /ubpcom/  qsq
      integer          bstyle, pad037
      common /ubpcom/  bstyle, pad037
      logical          nonlog, pad038
      common /ubpcom/  nonlog, pad038
      integer          stdin , pad039
      common /ubpcom/  stdin , pad039
      integer          stdout, pad040
      common /ubpcom/  stdout, pad040
      integer          stderr, pad041
      common /ubpcom/  stderr, pad041
      integer          errcnt, pad042
      common /ubpcom/  errcnt, pad042
      integer          errmax, pad043
      common /ubpcom/  errmax, pad043
      integer          verbos, pad044
      common /ubpcom/  verbos, pad044
      integer          status, pad045
      common /ubpcom/  status, pad045
      integer          hepdat, pad046
      common /ubpcom/  hepdat, pad046
      integer          heprev, pad047
      common /ubpcom/  heprev, pad047
      integer          runid , pad048
      common /ubpcom/  runid , pad048
      logical          dbgcol, pad049
      common /ubpcom/  dbgcol, pad049
      logical          dbghep, pad050
      common /ubpcom/  dbghep, pad050
      logical          dbgini, pad051
      common /ubpcom/  dbgini, pad051
      logical          dbgmas, pad052
      common /ubpcom/  dbgmas, pad052
      logical          dbgmup, pad053
      common /ubpcom/  dbgmup, pad053
CBB
      CHARACTER     VERSQQ*8 
      integer idatqq
CBB 
      integer nev
      save nev
      data nev /0/

      if (code.eq.0) then
         call ubinit (idatqq,VERSQQ)
         nev = 0
      else if (code.eq.1) then
         if (status.eq.0) then
            call ubinit (idatqq,VERSQQ)
            nev = 0
         endif
         nev = nev + 1
         call ubgen (nev)
      else if (code.eq.2) then
         call ubclos (nev)
      else
         call ubumsg ('unibab', 100, 'invalid argument')
         stop
      endif

      end


CDECK  ID>, UBINIT. 
*CMZ :  2.01/08 29/11/95  14.51.11  by  Harald Anlauf
*CMZ :  2.01/06 30/03/95  22.00.07  by  Harald Anlauf
*CMZ :  2.01/05 24/03/95  00.43.27  by  Harald Anlauf
*CMZ :  2.01/03 17/03/95  10.59.38  by  Harald Anlauf
*CMZ :  2.01/01 30/01/95  21.38.49  by  Harald Anlauf
*CMZ :  2.01/00 03/10/94  14.05.13  by  Harald Anlauf
*CMZ :  2.00/01 30/09/94  15.04.19  by  Harald Anlauf
*CMZ :  1.99/05 01/06/93  21.47.23  by  Harald Anlauf & Helge Meinhard
*CMZ :  1.99/03 26/05/93  13.20.04  by  Harald Anlauf & Helge Meinhard
*CMZ :  1.99/02 26/05/93  12.25.16  by  Harald Anlauf & Helge Meinhard
*CMZ :  1.99/01 14/05/93  12.46.13  by  Harald Anlauf & Helge Meinhard
*CMZ :  1.99/00 11/05/93  17.52.34  by  Harald Anlauf & Helge Meinhard
*CMZ :- 1.02/99- 6/07/92  22.07.37  by  Thorsten Ohl <ohl@ips105>
*-- Author :    Thorsten Ohl <ohl@ips105>

      subroutine ubinit (idatqq,versqq)
      implicit none

      double precision ahpla
      common /ubpcom/  ahpla
      double precision alpha
      common /ubpcom/  alpha
      double precision mass1e
      common /ubpcom/  mass1e
      double precision mass2e
      common /ubpcom/  mass2e
      double precision mass1z
      common /ubpcom/  mass1z
      double precision mass2z
      common /ubpcom/  mass2z
      double precision mass1t
      common /ubpcom/  mass1t
      double precision mass1h
      common /ubpcom/  mass1h
      double precision mass1w
      common /ubpcom/  mass1w
      double precision gamm1z
      common /ubpcom/  gamm1z
      double precision gamm2z
      common /ubpcom/  gamm2z
      double precision sin2w
      common /ubpcom/  sin2w
      double precision alphas
      common /ubpcom/  alphas
      double precision ebeam
      common /ubpcom/  ebeam
      double precision epol
      common /ubpcom/  epol
      double precision ppol
      common /ubpcom/  ppol
      double precision ctsmin
      common /ubpcom/  ctsmin
      double precision ctsmax
      common /ubpcom/  ctsmax
      double precision ecut
      common /ubpcom/  ecut
      double precision acocut
      common /ubpcom/  acocut
      double precision evisct
      common /ubpcom/  evisct
      integer          nevent, pad022
      common /ubpcom/  nevent, pad022
      integer          rseed , pad023
      common /ubpcom/  rseed , pad023
      logical          tchann, pad024
      common /ubpcom/  tchann, pad024
      logical          qedvtx, pad025
      common /ubpcom/  qedvtx, pad025
      logical          qedbox, pad026
      common /ubpcom/  qedbox, pad026
      logical          weak  , pad027
      common /ubpcom/  weak  , pad027
      logical          boxes , pad028
      common /ubpcom/  boxes , pad028
      integer          isrtyp, pad029
      common /ubpcom/  isrtyp, pad029
      integer          fsrtyp, pad030
      common /ubpcom/  fsrtyp, pad030
      double precision epsiln
      common /ubpcom/  epsiln
      double precision taumin
      common /ubpcom/  taumin
      double precision taumax
      common /ubpcom/  taumax
      double precision emin
      common /ubpcom/  emin
      double precision eps2
      common /ubpcom/  eps2
      double precision qsq
      common /ubpcom/  qsq
      integer          bstyle, pad037
      common /ubpcom/  bstyle, pad037
      logical          nonlog, pad038
      common /ubpcom/  nonlog, pad038
      integer          stdin , pad039
      common /ubpcom/  stdin , pad039
      integer          stdout, pad040
      common /ubpcom/  stdout, pad040
      integer          stderr, pad041
      common /ubpcom/  stderr, pad041
      integer          errcnt, pad042
      common /ubpcom/  errcnt, pad042
      integer          errmax, pad043
      common /ubpcom/  errmax, pad043
      integer          verbos, pad044
      common /ubpcom/  verbos, pad044
      integer          status, pad045
      common /ubpcom/  status, pad045
      integer          hepdat, pad046
      common /ubpcom/  hepdat, pad046
      integer          heprev, pad047
      common /ubpcom/  heprev, pad047
      integer          runid , pad048
      common /ubpcom/  runid , pad048
      logical          dbgcol, pad049
      common /ubpcom/  dbgcol, pad049
      logical          dbghep, pad050
      common /ubpcom/  dbghep, pad050
      logical          dbgini, pad051
      common /ubpcom/  dbgini, pad051
      logical          dbgmas, pad052
      common /ubpcom/  dbgmas, pad052
      logical          dbgmup, pad053
      common /ubpcom/  dbgmup, pad053
      double precision maxws, maxwt,
     &                 maxwnl, wnlvs, wnlhrd, wnlavh, wnlbare
      common /ubcsta/  maxws, maxwt,
     &                 maxwnl, wnlvs, wnlhrd, wnlavh, wnlbare

      integer          isbad, iscall, isfail, isrej, callct, acccut
      common /ubcsta/  isbad, iscall, isfail, isrej, callct, acccut

      integer          itbad, itcall, itfail, nlbad, nlcall, nlfail
      common /ubcsta/  itbad, itcall, itfail, nlbad, nlcall, nlfail

      integer          nlvs, nlhrd
      common /ubcsta/  nlvs, nlhrd
      double precision cacomx, logeps, lneps2, nlgsca
      common /ubcmsc/ cacomx, logeps, lneps2, nlgsca
      double precision PI
      parameter (PI = 3.141592653589793238d0)

      double precision ubrgen, dummy
      external ubrgen
      double precision MAXCTS
      parameter (MAXCTS = 0.985d0)

      integer date, time, IDATQQ, IVERSQ, mcid, lrseed
      character*8  VERSQQ
      real         IVERQQ
      character*17 VERQQQ
      character*55 msg

c Require block data subprogram
      external ubpini

c Empirical parameters for Monte Carlo tuning.
c The reason for these paramters is the following:
c For strictly collinear photon emission, they could be set to one,
c since the Monte Carlo internal steering parameters can be calculated
c exactly in this case.  But if one allows for finite photon emission
c angles, one would lose a few events at the edge of phase space and
c therefore underestimate the cross section.  Using values slightly
c smaller than one cures this problem at the cost of Monte Carlo speed.
c The values given below are a good compromise between speed and
c practical accuracy.
c All of FUDGE1, FUDGE2 and FUDGE3 must be smaller than 1 !!!
      double precision FUDGE1, FUDGE2, FUDGE3
      parameter (FUDGE1 = 0.9d0, FUDGE2 = FUDGE1, FUDGE3 = 0.9d0)

      save lrseed
      data lrseed /0/

      call datime (date, time)
      time = time * 100

c Here we import the CMZ Version control strings.
c IDATQQ = yymmdd, IVERSQQ = vvrrll, VERSQQ = 'vv.rr/ll'.
      write (VERQQQ, 8000)
 8000 format (
     + 17HUNIBAB    2.02/00                                                 HOLD
     &       )
      read (VERQQQ, 9000) IVERQQ
 9000 format (9X,F5.2)
      IVERSQ = int (10000.0 * IVERQQ)
*KEEP,VERSQQ.
      VERSQQ = ' 2.02/00'
      IVERSQ =  20200
*KEND.
      IDATQQ= 951201
      hepdat = IDATQQ
      heprev = IVERSQ/100

c Do we need to reinitialize the random number generator ?
      if (rseed .ne. lrseed) then
c Initialize before first call and after changing the seed
         dummy = ubrgen (-rseed)
         lrseed = rseed
      endif

c UNIBAB's Monte Carlo ID number has not been set ...
      mcid  = 0
c if the run ID is zero, choose a random one!
      if (runid.eq.0) runid = int (2**30 * ubrgen (0))

c Store all this information in a /hepevt/ initialization record
      call ubeeni (mcid, heprev, hepdat, runid, date, time)


c Reset statistics
      isbad = 0
      iscall = 0
      isfail = 0
      isrej = 0
      itbad = 0
      itcall = 0
      itfail = 0
      nlbad = 0
      nlcall = 0
      nlfail = 0
      callct = 0
      acccut = 0
      maxws = 0
      maxwt = 0
      maxwnl = 0
      nlvs = 0
      nlhrd = 0
      wnlvs = 0
      wnlhrd = 0
      wnlavh = 0
      wnlbare = 0

c Warn innocuous users:
      if (isrtyp .le. 0 .or. isrtyp .gt. 1) then
         call ubumsg ('ubinit', 1,
     &        'isrtyp out of range, reset to 1.')
         isrtyp = 1
      endif

      if (fsrtyp .le. 0 .or. fsrtyp .gt. 3) then
         call ubumsg ('ubinit', 1,
     &        'fsrtyp out of range, reset to 3.')
         fsrtyp = 3
      endif

      if (bstyle .ne. 3) then
c The user has changed the obsolete variable bstyle from the default:
         call ubumsg ('ubinit', 1,
     &        'bstyle is obsolete.  Use isrtyp and fsrtyp instead.')
         if (bstyle .eq. 0) then
            isrtyp = 0
            fsrtyp = 0
         elseif (bstyle .eq. 1) then
            isrtyp = 1
            fsrtyp = 0
         elseif (bstyle .eq. 2) then
            isrtyp = 0
            fsrtyp = 3
         endif
      endif

      if (epsiln .gt. 1.d-2) then
         call ubumsg ('ubinit', 1,
     &        'epsiln larger than 1.d-2 may give bad accuracy.')
      endif
      if (eps2 .lt. epsiln) then
         write (msg,5) epsiln
 5       format ('eps2 can''t be smaller than epsiln, reset to ',f6.3)
         call ubumsg ('ubinit', 1, msg)
         eps2 = epsiln
      endif
      if (eps2 .gt. 1.d-2) then
         call ubumsg ('ubinit', 1,
     &        'eps2 larger than 1.d-2 may give bad accuracy.')
      endif

c initialize the Monte Carlo Parameters

c Set branching scale
      qsq = 4 * ebeam**2

c Time savers:
      logeps = log (epsiln)
      lneps2 = log (eps2)

c Transform given cuts into MC internal parameters:

      if (ctsmax .gt. MAXCTS) then
         write (msg,10) MAXCTS
 10      format ('ctsmax too large, reset to ',f6.3)
         call ubumsg ('ubinit', 1, msg)
         ctsmax = MAXCTS
      endif

c Angle cuts from cos(theta*)
      taumin = (1.d0 - ctsmax) / 2.d0
      taumax = (1.d0 - ctsmin) / 2.d0
      if (isrtyp .ne. 0 .or. fsrtyp .ne. 0) then
         taumin = taumin * FUDGE1
         taumax = 1.d0 - (1.d0 - taumax) * FUDGE2
      endif

c Minimal invariant mass from energy and acollinearity cuts
      emin = ecut * cos (acocut * (PI / 360.d0))
      if (isrtyp .ne. 0 .or. fsrtyp .ne. 0) then
         emin = emin * FUDGE3
      endif
      cacomx = cos (acocut * (PI / 180.d0))

c Apply semi-physical cut on the "visible energy", which is defined as
c the invariant mass of the final state excluding initial state photons
      if (evisct .ge. 2*ebeam) then
         call ubumsg ('ubinit', 10,
     &        '"Visible energy" cut EVISCT too large, reset to 0.')
         evisct = 0
      endif
      emin = max (emin, 0.5d0 * evisct)

c Set parameters of the Glashow-Salam-Weinberg model
      call ubigsw ()

c Special infos and warnings:
      if (dbgcol .or. dbgmup .or. dbgmas) then
         call ubumsg ('ubinit', 1,
     &        'You have touched the internal debugging options !')
         call ubumsg ('ubinit', 1,
     &        'You must *NOT* use the results in your publications !!!')
      endif

      if (nonlog .or. qedbox) then
         call ubumsg ('ubinit', 1,
     &        'You are using unfinished and untested development code!')
         call ubumsg ('ubinit', 1,
     &        'You must *NOT* use the results in your publications !!!')
      endif

c Find maximal Born X-section
      call ubibmx ()

      if (nonlog) then
         nlgsca = 0.5d0
      else
         nlgsca = 1
      endif

c We're ready:
      status = 1

      end
CDECK  ID>, UBIGSW. 
*CMZ :  2.01/05 24/03/95  16.42.07  by  Harald Anlauf
*CMZ :  2.01/03 17/03/95  13.41.28  by  Harald Anlauf
*CMZ :  1.99/03 26/05/93  17.32.22  by  Harald Anlauf & Helge Meinhard
*CMZ :  1.99/00 06/05/93  21.06.43  by  Harald Anlauf & Helge Meinhard
*CMZ :- 1.02/99- 6/07/92  22.07.59  by  Thorsten Ohl <ohl@ips105>
*-- Author :    Thorsten Ohl <ohl@ips105>

c UBIGSW - Initialize electroweak library

      subroutine ubigsw ()
      implicit none

      double precision ahpla
      common /ubpcom/  ahpla
      double precision alpha
      common /ubpcom/  alpha
      double precision mass1e
      common /ubpcom/  mass1e
      double precision mass2e
      common /ubpcom/  mass2e
      double precision mass1z
      common /ubpcom/  mass1z
      double precision mass2z
      common /ubpcom/  mass2z
      double precision mass1t
      common /ubpcom/  mass1t
      double precision mass1h
      common /ubpcom/  mass1h
      double precision mass1w
      common /ubpcom/  mass1w
      double precision gamm1z
      common /ubpcom/  gamm1z
      double precision gamm2z
      common /ubpcom/  gamm2z
      double precision sin2w
      common /ubpcom/  sin2w
      double precision alphas
      common /ubpcom/  alphas
      double precision ebeam
      common /ubpcom/  ebeam
      double precision epol
      common /ubpcom/  epol
      double precision ppol
      common /ubpcom/  ppol
      double precision ctsmin
      common /ubpcom/  ctsmin
      double precision ctsmax
      common /ubpcom/  ctsmax
      double precision ecut
      common /ubpcom/  ecut
      double precision acocut
      common /ubpcom/  acocut
      double precision evisct
      common /ubpcom/  evisct
      integer          nevent, pad022
      common /ubpcom/  nevent, pad022
      integer          rseed , pad023
      common /ubpcom/  rseed , pad023
      logical          tchann, pad024
      common /ubpcom/  tchann, pad024
      logical          qedvtx, pad025
      common /ubpcom/  qedvtx, pad025
      logical          qedbox, pad026
      common /ubpcom/  qedbox, pad026
      logical          weak  , pad027
      common /ubpcom/  weak  , pad027
      logical          boxes , pad028
      common /ubpcom/  boxes , pad028
      integer          isrtyp, pad029
      common /ubpcom/  isrtyp, pad029
      integer          fsrtyp, pad030
      common /ubpcom/  fsrtyp, pad030
      double precision epsiln
      common /ubpcom/  epsiln
      double precision taumin
      common /ubpcom/  taumin
      double precision taumax
      common /ubpcom/  taumax
      double precision emin
      common /ubpcom/  emin
      double precision eps2
      common /ubpcom/  eps2
      double precision qsq
      common /ubpcom/  qsq
      integer          bstyle, pad037
      common /ubpcom/  bstyle, pad037
      logical          nonlog, pad038
      common /ubpcom/  nonlog, pad038
      integer          stdin , pad039
      common /ubpcom/  stdin , pad039
      integer          stdout, pad040
      common /ubpcom/  stdout, pad040
      integer          stderr, pad041
      common /ubpcom/  stderr, pad041
      integer          errcnt, pad042
      common /ubpcom/  errcnt, pad042
      integer          errmax, pad043
      common /ubpcom/  errmax, pad043
      integer          verbos, pad044
      common /ubpcom/  verbos, pad044
      integer          status, pad045
      common /ubpcom/  status, pad045
      integer          hepdat, pad046
      common /ubpcom/  hepdat, pad046
      integer          heprev, pad047
      common /ubpcom/  heprev, pad047
      integer          runid , pad048
      common /ubpcom/  runid , pad048
      logical          dbgcol, pad049
      common /ubpcom/  dbgcol, pad049
      logical          dbghep, pad050
      common /ubpcom/  dbghep, pad050
      logical          dbgini, pad051
      common /ubpcom/  dbgini, pad051
      logical          dbgmas, pad052
      common /ubpcom/  dbgmas, pad052
      logical          dbgmup, pad053
      common /ubpcom/  dbgmup, pad053
      double precision RMZ,RMW,RMH,RMT, ZWID, SIN2TH, sw, cw, cw2, pizmz
      COMMON/ BOSONS / RMZ,RMW,RMH,RMT, ZWID, SIN2TH, sw, cw, cw2, pizmz
      integer IWEAK, IFERM, ICHANN, IBOXES, IOUT
      COMMON/ OPTION / IWEAK, IFERM, ICHANN, IBOXES, IOUT

      call ewinit (mass1z, mass1h, mass1t, alphas)

c parameters of the electroweak bosons (in GeV):
      mass2z = mass1z**2
      gamm1z = zwid
      gamm2z = gamm1z**2
      mass1w = rmw

      sin2w  = sin2th

c QED coupling and electron mass squared:
      alpha  = 1.d0/ahpla
      mass2e = mass1e**2

c miscellaneous options for electroweak library
      if (dbgmup) then
         iferm = 2
         if (tchann) then
            call ubumsg ('ubigsw', 1,
     &           'Muon pair production mode: t-channel switched off !')
            tchann = .false.
         endif
         if (nonlog) then
            call ubumsg ('ubigsw', 1,
     &           'Muon pair production mode: no non-log corrections !')
            nonlog = .false.
         endif
      else
         iferm = 1
      endif

      if (tchann) then
         ichann = 0
      else
         ichann = 1
      endif

      if (weak) then
         iweak = 1
      else
         iweak = 0
      endif
      if (boxes) then
         iboxes = 1
      else
         iboxes = 0
      endif
      iout = stdout

      end
CDECK  ID>, UBIBMX. 
*CMZ :  2.01/00 30/09/94  18.14.01  by  Harald Anlauf
*CMZ :  1.99/00 11/05/93  17.52.34  by  Harald Anlauf & Helge Meinhard
*CMZ :- 1.02/99- 6/07/92  22.09.16  by  Thorsten Ohl <ohl@ips105>
*-- Author :    Thorsten Ohl <ohl@ips105>

      subroutine ubibmx ()
      implicit none

      double precision ahpla
      common /ubpcom/  ahpla
      double precision alpha
      common /ubpcom/  alpha
      double precision mass1e
      common /ubpcom/  mass1e
      double precision mass2e
      common /ubpcom/  mass2e
      double precision mass1z
      common /ubpcom/  mass1z
      double precision mass2z
      common /ubpcom/  mass2z
      double precision mass1t
      common /ubpcom/  mass1t
      double precision mass1h
      common /ubpcom/  mass1h
      double precision mass1w
      common /ubpcom/  mass1w
      double precision gamm1z
      common /ubpcom/  gamm1z
      double precision gamm2z
      common /ubpcom/  gamm2z
      double precision sin2w
      common /ubpcom/  sin2w
      double precision alphas
      common /ubpcom/  alphas
      double precision ebeam
      common /ubpcom/  ebeam
      double precision epol
      common /ubpcom/  epol
      double precision ppol
      common /ubpcom/  ppol
      double precision ctsmin
      common /ubpcom/  ctsmin
      double precision ctsmax
      common /ubpcom/  ctsmax
      double precision ecut
      common /ubpcom/  ecut
      double precision acocut
      common /ubpcom/  acocut
      double precision evisct
      common /ubpcom/  evisct
      integer          nevent, pad022
      common /ubpcom/  nevent, pad022
      integer          rseed , pad023
      common /ubpcom/  rseed , pad023
      logical          tchann, pad024
      common /ubpcom/  tchann, pad024
      logical          qedvtx, pad025
      common /ubpcom/  qedvtx, pad025
      logical          qedbox, pad026
      common /ubpcom/  qedbox, pad026
      logical          weak  , pad027
      common /ubpcom/  weak  , pad027
      logical          boxes , pad028
      common /ubpcom/  boxes , pad028
      integer          isrtyp, pad029
      common /ubpcom/  isrtyp, pad029
      integer          fsrtyp, pad030
      common /ubpcom/  fsrtyp, pad030
      double precision epsiln
      common /ubpcom/  epsiln
      double precision taumin
      common /ubpcom/  taumin
      double precision taumax
      common /ubpcom/  taumax
      double precision emin
      common /ubpcom/  emin
      double precision eps2
      common /ubpcom/  eps2
      double precision qsq
      common /ubpcom/  qsq
      integer          bstyle, pad037
      common /ubpcom/  bstyle, pad037
      logical          nonlog, pad038
      common /ubpcom/  nonlog, pad038
      integer          stdin , pad039
      common /ubpcom/  stdin , pad039
      integer          stdout, pad040
      common /ubpcom/  stdout, pad040
      integer          stderr, pad041
      common /ubpcom/  stderr, pad041
      integer          errcnt, pad042
      common /ubpcom/  errcnt, pad042
      integer          errmax, pad043
      common /ubpcom/  errmax, pad043
      integer          verbos, pad044
      common /ubpcom/  verbos, pad044
      integer          status, pad045
      common /ubpcom/  status, pad045
      integer          hepdat, pad046
      common /ubpcom/  hepdat, pad046
      integer          heprev, pad047
      common /ubpcom/  heprev, pad047
      integer          runid , pad048
      common /ubpcom/  runid , pad048
      logical          dbgcol, pad049
      common /ubpcom/  dbgcol, pad049
      logical          dbghep, pad050
      common /ubpcom/  dbghep, pad050
      logical          dbgini, pad051
      common /ubpcom/  dbgini, pad051
      logical          dbgmas, pad052
      common /ubpcom/  dbgmas, pad052
      logical          dbgmup, pad053
      common /ubpcom/  dbgmup, pad053
      double precision brnmax
      common /ubcbrn/ brnmax


      double precision z, zp, zm, zmax, zdum, tmp
      character*55 msgbuf
      double precision EPS
      parameter (EPS = 1.d-5)

      double precision ubibn, ubxtot, ubumin
      external ubibn, ubxtot, ubumin

c find Z0 peak (search slightly below and slightly above peak):
      zm = (mass1z - gamm1z)**2
      zp =  mass2z
      z  = (zp+zm)/2
      brnmax = - ubumin (zm, z, zp, ubibn, EPS, zdum)
      zmax = z

      zm =  mass2z
      zp = (mass1z + gamm1z)**2
      z  = (zp+zm)/2
      tmp = - ubumin (zm, z, zp, ubibn, EPS, zdum)

      if (tmp .gt. brnmax) then
         brnmax = tmp
         zmax = z
      endif

      if (dbgini) then
         write (msgbuf,10) zmax, brnmax
 10      format ('Z0 peak @ s = ',e9.3,' GeV**2, brnmax = ',e9.3,' pb')
         call ubumsg ('ubibmx', 0, msgbuf)
      endif

c check lower end
      tmp = ubxtot (4*emin*emin, taumin, taumax)
      brnmax = max (brnmax, tmp)

      if (dbgini) then
         write (msgbuf,20) 4*emin*emin, tmp
 20      format ('At lower end: ',e9.3,' GeV**2: sigma  = ',e9.3,' pb')
         call ubumsg ('ubibmx', 0, msgbuf)
      endif

c account for numerical accuracy:
      brnmax = brnmax * (1 + EPS)

      end
CDECK  ID>, UBIBN.  
*CMZ :  1.99/00 11/05/93  17.52.34  by  Harald Anlauf & Helge Meinhard
*CMZ :- 1.02/99- 6/07/92  22.09.31  by  Thorsten Ohl <ohl@ips105>
*-- Author :    Thorsten Ohl <ohl@ips105>

c UBIBN - returns minus the integrated trial cross section for
c         maximum search

      double precision function ubibn (s)
      implicit none
      double precision s

      double precision ahpla
      common /ubpcom/  ahpla
      double precision alpha
      common /ubpcom/  alpha
      double precision mass1e
      common /ubpcom/  mass1e
      double precision mass2e
      common /ubpcom/  mass2e
      double precision mass1z
      common /ubpcom/  mass1z
      double precision mass2z
      common /ubpcom/  mass2z
      double precision mass1t
      common /ubpcom/  mass1t
      double precision mass1h
      common /ubpcom/  mass1h
      double precision mass1w
      common /ubpcom/  mass1w
      double precision gamm1z
      common /ubpcom/  gamm1z
      double precision gamm2z
      common /ubpcom/  gamm2z
      double precision sin2w
      common /ubpcom/  sin2w
      double precision alphas
      common /ubpcom/  alphas
      double precision ebeam
      common /ubpcom/  ebeam
      double precision epol
      common /ubpcom/  epol
      double precision ppol
      common /ubpcom/  ppol
      double precision ctsmin
      common /ubpcom/  ctsmin
      double precision ctsmax
      common /ubpcom/  ctsmax
      double precision ecut
      common /ubpcom/  ecut
      double precision acocut
      common /ubpcom/  acocut
      double precision evisct
      common /ubpcom/  evisct
      integer          nevent, pad022
      common /ubpcom/  nevent, pad022
      integer          rseed , pad023
      common /ubpcom/  rseed , pad023
      logical          tchann, pad024
      common /ubpcom/  tchann, pad024
      logical          qedvtx, pad025
      common /ubpcom/  qedvtx, pad025
      logical          qedbox, pad026
      common /ubpcom/  qedbox, pad026
      logical          weak  , pad027
      common /ubpcom/  weak  , pad027
      logical          boxes , pad028
      common /ubpcom/  boxes , pad028
      integer          isrtyp, pad029
      common /ubpcom/  isrtyp, pad029
      integer          fsrtyp, pad030
      common /ubpcom/  fsrtyp, pad030
      double precision epsiln
      common /ubpcom/  epsiln
      double precision taumin
      common /ubpcom/  taumin
      double precision taumax
      common /ubpcom/  taumax
      double precision emin
      common /ubpcom/  emin
      double precision eps2
      common /ubpcom/  eps2
      double precision qsq
      common /ubpcom/  qsq
      integer          bstyle, pad037
      common /ubpcom/  bstyle, pad037
      logical          nonlog, pad038
      common /ubpcom/  nonlog, pad038
      integer          stdin , pad039
      common /ubpcom/  stdin , pad039
      integer          stdout, pad040
      common /ubpcom/  stdout, pad040
      integer          stderr, pad041
      common /ubpcom/  stderr, pad041
      integer          errcnt, pad042
      common /ubpcom/  errcnt, pad042
      integer          errmax, pad043
      common /ubpcom/  errmax, pad043
      integer          verbos, pad044
      common /ubpcom/  verbos, pad044
      integer          status, pad045
      common /ubpcom/  status, pad045
      integer          hepdat, pad046
      common /ubpcom/  hepdat, pad046
      integer          heprev, pad047
      common /ubpcom/  heprev, pad047
      integer          runid , pad048
      common /ubpcom/  runid , pad048
      logical          dbgcol, pad049
      common /ubpcom/  dbgcol, pad049
      logical          dbghep, pad050
      common /ubpcom/  dbghep, pad050
      logical          dbgini, pad051
      common /ubpcom/  dbgini, pad051
      logical          dbgmas, pad052
      common /ubpcom/  dbgmas, pad052
      logical          dbgmup, pad053
      common /ubpcom/  dbgmup, pad053

      double precision ubxtot
      external ubxtot

      ubibn =  - ubxtot (s, taumin, taumax)

      end
CDECK  ID>, UBGEN.  
*CMZ :  2.01/08 29/11/95  16.37.33  by  Harald Anlauf
*CMZ :  2.01/06 15/05/95  09.57.06  by  Harald Anlauf
*CMZ :  2.01/05 24/03/95  17.46.33  by  Harald Anlauf
*CMZ :  2.01/01 30/01/95  21.56.55  by  Harald Anlauf
*CMZ :  1.99/07 02/06/93  20.52.49  by  Harald Anlauf & Helge Meinhard
*CMZ :  1.99/05 01/06/93  14.57.03  by  Harald Anlauf & Helge Meinhard
*CMZ :  1.99/04 27/05/93  15.53.21  by  Harald Anlauf & Helge Meinhard
*CMZ :  1.99/02 26/05/93  12.07.46  by  Harald Anlauf & Helge Meinhard
*CMZ :  1.99/01 14/05/93  12.10.59  by  Harald Anlauf & Helge Meinhard
*CMZ :  1.99/00 11/05/93  17.52.34  by  Harald Anlauf & Helge Meinhard
*CMZ :  1.02/01  6/08/92  01.31.46  by  Thorsten Ohl <ohl@ips105.desy.de>
*-- Author :    Thorsten Ohl <ohl@ips105>

      subroutine ubgen (n)
      implicit none
      integer n

      integer NMXHEP
      parameter (NMXHEP = 2000)
      integer nevhep, nhep, isthep(NMXHEP), idhep(NMXHEP),
     $     jmohep(2,NMXHEP), jdahep(2,NMXHEP)
      real phep(5,NMXHEP), vhep(4,NMXHEP), shep(4,NMXHEP)
      common /hepevt/ nevhep, nhep, isthep, idhep, jmohep, jdahep,
     $     phep, vhep
      common /hepspn/ shep
      integer NMXINT
      parameter (NMXINT = 100)
      double precision p
      common /ubcevt/  p(5,NMXINT)

      double precision ipele, ippos, opele, oppos, savp1, savp2, savk
      common /ubcevt/  ipele(4), ippos(4), opele(4), oppos(4)
      common /ubcevt/  savp1(4), savp2(4), savk(4)
c Quarks
      integer PDGDWN, PDGUP, PDGSTR, PDGCHM, PDGBOT, PDGTOP
      parameter (PDGDWN =    1, PDGUP  =    2, PDGSTR =    3,
     $           PDGCHM =    4, PDGBOT =    5, PDGTOP =    6)
c Leptons
      integer PDGELE, PDGNUE, PDGMU, PDGNUM, PDGTAU, PDGNUT
      parameter (PDGELE =   11, PDGNUE =   12, PDGMU =    13,
     $           PDGNUM =   14, PDGTAU =   15, PDGNUT =   16)
c Bosons
      integer PDGGLU, PDGGLV, PDGGAM, PDGZ0, PDGWPL, PDGH0
      parameter (PDGGLU =   21, PDGGLV =    9, PDGGAM =   22,
     $           PDGZ0  =   23, PDGWPL =   24, PDGH0  =   25)
c Mesons
      integer PDGPIP, PDGPI0, PDGETA, PDGKPL, PDGK0, PDGK0S, PDGK0L
      parameter (PDGPIP =  211, PDGPI0 =  111, PDGETA =  221,
     $           PDGKPL =  321, PDGK0  =  311, PDGK0S =  310,
     $           PDGK0L =  130)
c Baryons
      integer PDGPRO, PDGNEU
      parameter (PDGPRO = 2212, PDGNEU = 2112)


      double precision maxws, maxwt,
     &                 maxwnl, wnlvs, wnlhrd, wnlavh, wnlbare
      common /ubcsta/  maxws, maxwt,
     &                 maxwnl, wnlvs, wnlhrd, wnlavh, wnlbare

      integer          isbad, iscall, isfail, isrej, callct, acccut
      common /ubcsta/  isbad, iscall, isfail, isrej, callct, acccut

      integer          itbad, itcall, itfail, nlbad, nlcall, nlfail
      common /ubcsta/  itbad, itcall, itfail, nlbad, nlcall, nlfail

      integer          nlvs, nlhrd
      common /ubcsta/  nlvs, nlhrd
      double precision ahpla
      common /ubpcom/  ahpla
      double precision alpha
      common /ubpcom/  alpha
      double precision mass1e
      common /ubpcom/  mass1e
      double precision mass2e
      common /ubpcom/  mass2e
      double precision mass1z
      common /ubpcom/  mass1z
      double precision mass2z
      common /ubpcom/  mass2z
      double precision mass1t
      common /ubpcom/  mass1t
      double precision mass1h
      common /ubpcom/  mass1h
      double precision mass1w
      common /ubpcom/  mass1w
      double precision gamm1z
      common /ubpcom/  gamm1z
      double precision gamm2z
      common /ubpcom/  gamm2z
      double precision sin2w
      common /ubpcom/  sin2w
      double precision alphas
      common /ubpcom/  alphas
      double precision ebeam
      common /ubpcom/  ebeam
      double precision epol
      common /ubpcom/  epol
      double precision ppol
      common /ubpcom/  ppol
      double precision ctsmin
      common /ubpcom/  ctsmin
      double precision ctsmax
      common /ubpcom/  ctsmax
      double precision ecut
      common /ubpcom/  ecut
      double precision acocut
      common /ubpcom/  acocut
      double precision evisct
      common /ubpcom/  evisct
      integer          nevent, pad022
      common /ubpcom/  nevent, pad022
      integer          rseed , pad023
      common /ubpcom/  rseed , pad023
      logical          tchann, pad024
      common /ubpcom/  tchann, pad024
      logical          qedvtx, pad025
      common /ubpcom/  qedvtx, pad025
      logical          qedbox, pad026
      common /ubpcom/  qedbox, pad026
      logical          weak  , pad027
      common /ubpcom/  weak  , pad027
      logical          boxes , pad028
      common /ubpcom/  boxes , pad028
      integer          isrtyp, pad029
      common /ubpcom/  isrtyp, pad029
      integer          fsrtyp, pad030
      common /ubpcom/  fsrtyp, pad030
      double precision epsiln
      common /ubpcom/  epsiln
      double precision taumin
      common /ubpcom/  taumin
      double precision taumax
      common /ubpcom/  taumax
      double precision emin
      common /ubpcom/  emin
      double precision eps2
      common /ubpcom/  eps2
      double precision qsq
      common /ubpcom/  qsq
      integer          bstyle, pad037
      common /ubpcom/  bstyle, pad037
      logical          nonlog, pad038
      common /ubpcom/  nonlog, pad038
      integer          stdin , pad039
      common /ubpcom/  stdin , pad039
      integer          stdout, pad040
      common /ubpcom/  stdout, pad040
      integer          stderr, pad041
      common /ubpcom/  stderr, pad041
      integer          errcnt, pad042
      common /ubpcom/  errcnt, pad042
      integer          errmax, pad043
      common /ubpcom/  errmax, pad043
      integer          verbos, pad044
      common /ubpcom/  verbos, pad044
      integer          status, pad045
      common /ubpcom/  status, pad045
      integer          hepdat, pad046
      common /ubpcom/  hepdat, pad046
      integer          heprev, pad047
      common /ubpcom/  heprev, pad047
      integer          runid , pad048
      common /ubpcom/  runid , pad048
      logical          dbgcol, pad049
      common /ubpcom/  dbgcol, pad049
      logical          dbghep, pad050
      common /ubpcom/  dbghep, pad050
      logical          dbgini, pad051
      common /ubpcom/  dbgini, pad051
      logical          dbgmas, pad052
      common /ubpcom/  dbgmas, pad052
      logical          dbgmup, pad053
      common /ubpcom/  dbgmup, pad053
      double precision brnmax
      common /ubcbrn/ brnmax


      double precision s, tau, ws, wt, wnlg, weight, ecms, ecms2
      double precision factor, ubr, fsrsca
      double precision beta(4)
      integer i, j, l, savptr(3)
      character*55 msgbuf

      double precision ubxtot, ubrgen
      logical ubgacc
      external ubxtot, ubrgen, ubgacc
      double precision EE, MASS1M
      parameter (EE=2.71828182845904523536d0, MASS1M = 0.105658389d0)

 1    continue

c Start new /hepevt/
      call ubenew (n)

      nhep = 10
      do 100 i = 1, nhep
         call ubenul (i)
 100  continue

c Electron beam
      isthep(1) = 101
      idhep(1) = PDGELE
      p(1,1) = 0.d0
      p(2,1) = 0.d0
      p(3,1) = ebeam
      p(4,1) = ebeam
      p(5,1) = 0.d0

c Positron beam
      isthep(2) = 102
      idhep(2) = - PDGELE
      p(1,2) = 0.d0
      p(2,2) = 0.d0
      p(3,2) = - ebeam
      p(4,2) = ebeam
      p(5,2) = 0.d0

c Collider CMS
      isthep(3) = 103
      idhep(3) = 0
      do 110 i = 1, 4
         p(i,3) = p(i,1) + p(i,2)
 110  continue
      p(5,3) = sqrt (p(4,3)**2 - p(1,3)**2 - p(2,3)**2 - p(3,3)**2)

c Temporary four-vectors for electron and positron momentum
c after initial state radiation
      isthep(4) = 111
      idhep(4) = PDGELE
      isthep(5) = 112
      idhep(5) = - PDGELE

c Effective c.m. momentum after initial state radiation
      isthep(6) = 110
      idhep(6) = PDGGAM

c Reserved for lepton momenta after hard scattering
      isthep(7) = 113
      idhep(7) = PDGELE
      isthep(8) = 114
      idhep(8) = - PDGELE

c outgoing leptons
      isthep(9) = 1
      idhep(9) = PDGELE
      isthep(10) = 1
      idhep(10) = - PDGELE
      p(5,9)  = 0
      p(5,10) = 0

c the main rejection loop
 10   continue
      iscall = iscall + 1

c Delete the photons from the last try
      nhep = 10

c Copy Electron and Positron Beam
      do 200 i = 1, 5
         p(i,4) = p(i,1)
         p(i,5) = p(i,2)
 200  continue

c Radiate initial state photons
      if (isrtyp .gt. 0) then
         call ubbini (4, qsq)
c Save pointer to last photon emitted from incoming electron:
         savptr(1) = nhep
         call ubbini (5, qsq)
c Save pointer to last photon emitted from incoming positron:
         savptr(2) = nhep
      else
         savptr(1) = nhep
         savptr(2) = nhep
      endif

c Partonic CMS
      do 210 i = 1, 4
         p(i,6) = p(i,4) + p(i,5)
 210  continue
      s = p(4,6)**2 - p(1,6)**2 - p(2,6)**2 - p(3,6)**2

c Reject event if c.m.s. energy unphysical:
      if (s .le. 4*emin**2) then
         isfail = isfail + 1
         goto 10
      endif

      ecms = sqrt (s)
      p(5,6) = ecms

c Calculate weight from integrated Born X-section as a check
      ws = ubxtot (s, taumin, taumax) / brnmax
      maxws = max (maxws, ws)

c Check for events out of range
      if (ws .gt. 1.d0) then
         write (msgbuf,250) s, ws
 250     format ('Bad weight in s generation, s =',e9.3,', w =',f6.4)
         call ubumsg ('ubgen', 9, msgbuf)
         isbad = isbad + 1
      endif

cHMH Reject event here if s weight is already smaller than random number,
cHMH hence the product of s and t weight will anyway be smaller
      ubr = ubrgen (0)
      if (ubr .gt. ws) then
         isrej = isrej + 1
         goto 10
      endif

c Generate t and calculate event weight
      itcall = itcall + 1
      call ubgt (s, tau, wt)
      maxwt = max (maxwt, wt)

c Check for events out of range
      if (wt .gt. 1.d0) then
         call ubumsg ('ubgen', 9, 'Bad weight in t generation')
         write (msgbuf,260) s, tau, wt
 260     format ('s =',e9.3,', tau =', e9.3,', w =',f6.4)
         call ubumsg ('ubgen', 9, msgbuf)
         itbad = itbad + 1
      endif

c The total event weight up to now is the product of weights from s and
c t generation:
      weight = ws * wt

cHMH Do the rejection
      if (ubr .gt. weight) then
         itfail = itfail + 1
         goto 10
      endif

c Calculate four-velocity of effective c.m.s.

      do 300 i = 1, 4
         beta(i) = p(i,6) / ecms
 300  continue

c Boost incoming leptons to partonic c.m.s.

      call ububoo (beta, p(1,4), ipele)
      call ububoo (beta, p(1,5), ippos)

c Construct final momenta in the c.m.s. from T(S)

      ecms2 = 0.5d0 * ecms
      call ubgppr (ecms2, tau, ipele, ippos, opele, oppos)

c Reverse boost direction
      beta(1) = - beta(1)
      beta(2) = - beta(2)
      beta(3) = - beta(3)

c Boost scattered leptons back to lab system:
      call ububoo (beta, opele, p(1,7))
      call ububoo (beta, oppos, p(1,8))

c Electron and Positron in c.m.s. before final state radiation
      do 320 i = 1, 4
         p(i,9)  = p(i,7)
         p(i,10) = p(i,8)
 320  continue

c Generate final state radiation
      if (fsrtyp .gt. 0) then

c Save pointer to first photon emitted from first final state lepton.
         savptr(3) = nhep + 1

c Select effective "Big Log" for FSR:
         if (fsrtyp .eq. 1) then
c log( s/m_e^2) - 1
            fsrsca = s
         elseif (fsrtyp .eq. 2) then
c log(-t/m_e^2)
            fsrsca = s * tau * EE
         else
c log( s/m_e^2) - 1 + log(t/u)
            fsrsca = s * tau / (1-tau)
         endif
         call ubbfin (fsrsca, 9, 10, beta)

      else

c No final state radiation: just leave electron and positron momenta

         savptr(3) = nhep + 1
      endif

c Check if event passes our given cuts (before doing the expensive
c nonlog corrections):
      callct = callct + 1
      if (.not. ubgacc()) then
         goto 10
      endif
      acccut = acccut + 1

c Finally take care of special mode for muon pair production:
      if (dbgmup) then
         idhep(9)  =  PDGMU
         idhep(10) = -PDGMU
         p(5,9)    = MASS1M
         p(5,10)   = MASS1M
c in principle one should rescale the momenta of the outgoing muons,
c since the electrons were on mass shell (with mass = 0) ...
c but this mode is supposed to not be used by the normal user anyway
      endif

c Copy particle momenta to /HEPEVT/ common block.
      do 600 i = 1, nhep
         do 610 j = 1, 5
            phep(j,i) = p(j,i)
 610     continue
 600  continue

      end
CDECK  ID>, UBGT.   
*CMZ :  1.99/00 11/05/93  17.52.34  by  Harald Anlauf & Helge Meinhard
*CMZ :- 1.02/99- 6/07/92  22.10.12  by  Thorsten Ohl <ohl@ips105>
*-- Author :    Thorsten Ohl <ohl@ips105>

      subroutine ubgt (s, tau, weight)
      implicit none
      double precision s, tau, weight

      double precision ahpla
      common /ubpcom/  ahpla
      double precision alpha
      common /ubpcom/  alpha
      double precision mass1e
      common /ubpcom/  mass1e
      double precision mass2e
      common /ubpcom/  mass2e
      double precision mass1z
      common /ubpcom/  mass1z
      double precision mass2z
      common /ubpcom/  mass2z
      double precision mass1t
      common /ubpcom/  mass1t
      double precision mass1h
      common /ubpcom/  mass1h
      double precision mass1w
      common /ubpcom/  mass1w
      double precision gamm1z
      common /ubpcom/  gamm1z
      double precision gamm2z
      common /ubpcom/  gamm2z
      double precision sin2w
      common /ubpcom/  sin2w
      double precision alphas
      common /ubpcom/  alphas
      double precision ebeam
      common /ubpcom/  ebeam
      double precision epol
      common /ubpcom/  epol
      double precision ppol
      common /ubpcom/  ppol
      double precision ctsmin
      common /ubpcom/  ctsmin
      double precision ctsmax
      common /ubpcom/  ctsmax
      double precision ecut
      common /ubpcom/  ecut
      double precision acocut
      common /ubpcom/  acocut
      double precision evisct
      common /ubpcom/  evisct
      integer          nevent, pad022
      common /ubpcom/  nevent, pad022
      integer          rseed , pad023
      common /ubpcom/  rseed , pad023
      logical          tchann, pad024
      common /ubpcom/  tchann, pad024
      logical          qedvtx, pad025
      common /ubpcom/  qedvtx, pad025
      logical          qedbox, pad026
      common /ubpcom/  qedbox, pad026
      logical          weak  , pad027
      common /ubpcom/  weak  , pad027
      logical          boxes , pad028
      common /ubpcom/  boxes , pad028
      integer          isrtyp, pad029
      common /ubpcom/  isrtyp, pad029
      integer          fsrtyp, pad030
      common /ubpcom/  fsrtyp, pad030
      double precision epsiln
      common /ubpcom/  epsiln
      double precision taumin
      common /ubpcom/  taumin
      double precision taumax
      common /ubpcom/  taumax
      double precision emin
      common /ubpcom/  emin
      double precision eps2
      common /ubpcom/  eps2
      double precision qsq
      common /ubpcom/  qsq
      integer          bstyle, pad037
      common /ubpcom/  bstyle, pad037
      logical          nonlog, pad038
      common /ubpcom/  nonlog, pad038
      integer          stdin , pad039
      common /ubpcom/  stdin , pad039
      integer          stdout, pad040
      common /ubpcom/  stdout, pad040
      integer          stderr, pad041
      common /ubpcom/  stderr, pad041
      integer          errcnt, pad042
      common /ubpcom/  errcnt, pad042
      integer          errmax, pad043
      common /ubpcom/  errmax, pad043
      integer          verbos, pad044
      common /ubpcom/  verbos, pad044
      integer          status, pad045
      common /ubpcom/  status, pad045
      integer          hepdat, pad046
      common /ubpcom/  hepdat, pad046
      integer          heprev, pad047
      common /ubpcom/  heprev, pad047
      integer          runid , pad048
      common /ubpcom/  runid , pad048
      logical          dbgcol, pad049
      common /ubpcom/  dbgcol, pad049
      logical          dbghep, pad050
      common /ubpcom/  dbghep, pad050
      logical          dbgini, pad051
      common /ubpcom/  dbgini, pad051
      logical          dbgmas, pad052
      common /ubpcom/  dbgmas, pad052
      logical          dbgmup, pad053
      common /ubpcom/  dbgmup, pad053

      double precision az, chi0, chi, chi1, a, b, prefac

      double precision ubrgen, ubxdif, ubxtri
      external ubrgen, ubxdif, ubxtri

      weight = 0.d0
c fetch coefficients of the trial cross section
      call ubxcof (s, a, b, prefac)

c mapping (motivated by Berends et al., Nucl. Phys. B304, p.722)
      az = a / b
      chi1 = az*taumax - 1.d0/taumax
      chi0 = az*taumin - 1.d0/taumin
      chi = (chi1 - chi0) * ubrgen (0) + chi0
*      tau = (chi + sqrt (chi**2 + 4.d0 * az)) / (2.d0*az)
      tau = 2.d0 / (sqrt (chi**2 + 4.d0 * az) - chi)

c quit here if we get bad tau values from touchy numerics:
      if (tau .lt. taumin .or. tau .gt. taumax) return

c rejection weight for t rejection:
      weight = ubxdif (s, tau) / ubxtri (s, tau)

      end
CDECK  ID>, UBGPPR. 
*CMZ :  1.99/04 27/05/93  15.54.26  by  Harald Anlauf & Helge Meinhard
*CMZ :  1.99/00 11/05/93  10.28.32  by  Harald Anlauf & Helge Meinhard
*CMZ :- 1.02/99- 28/08/92  15.10.19  by  Dirk Uwe Sauer and Angelika Himmler
*-- Author :    Dirk Uwe Sauer

      subroutine ubgppr (e, tau, ipele, ippos, opele, oppos)
      implicit none
      double precision e, tau
      double precision ipele(4), ippos(4), opele(4), oppos(4)

      double precision PI
      parameter (PI = 3.141592653589793238d0)

      double precision ubrgen
      external ubrgen
      double precision costh, sinth, phi, cosphi, sinphi
      double precision e1(3), e2(3), e3(3)
      integer i


c Construct an orthonormal basis of 3-vectors in the cm - system
c Direction of the incoming electron is chosen as the z-direction.

      call ubuort (ipele, e1, e2, e3)

c Calculate the four-momentum of the outgoing fermions in the cms.
c (actually only the directions are needed by UBGEN).

      costh = 1.d0 - 2.d0 * tau
      sinth = 2.d0 * sqrt (tau * (1.d0 - tau))
      phi = 2.d0 * PI * ubrgen (0)
      cosphi = cos (phi)
      sinphi = sin (phi)
      opele(4) = e
      oppos(4) = opele(4)
      do 10 i = 1, 3
         opele(i) = opele(4) * ( e3(i) * costh
     &                + sinth * (e1(i) * cosphi + e2(i) * sinphi))
         oppos(i) = - opele(i)
 10   continue

      end
CDECK  ID>, UBGACC. 
*CMZ :  1.99/05 01/06/93  14.34.28  by  Harald Anlauf & Helge Meinhard
*CMZ :  1.99/01 13/05/93  18.57.36  by  Harald Anlauf & Helge Meinhard
*-- Author :    Harald Anlauf & Helge Meinhard   13/05/93

c UBGACC - logical function which check if event passed given cuts.

      logical function ubgacc ()
      implicit none

      integer NMXINT
      parameter (NMXINT = 100)
      double precision p
      common /ubcevt/  p(5,NMXINT)

      double precision ipele, ippos, opele, oppos, savp1, savp2, savk
      common /ubcevt/  ipele(4), ippos(4), opele(4), oppos(4)
      common /ubcevt/  savp1(4), savp2(4), savk(4)
      double precision ahpla
      common /ubpcom/  ahpla
      double precision alpha
      common /ubpcom/  alpha
      double precision mass1e
      common /ubpcom/  mass1e
      double precision mass2e
      common /ubpcom/  mass2e
      double precision mass1z
      common /ubpcom/  mass1z
      double precision mass2z
      common /ubpcom/  mass2z
      double precision mass1t
      common /ubpcom/  mass1t
      double precision mass1h
      common /ubpcom/  mass1h
      double precision mass1w
      common /ubpcom/  mass1w
      double precision gamm1z
      common /ubpcom/  gamm1z
      double precision gamm2z
      common /ubpcom/  gamm2z
      double precision sin2w
      common /ubpcom/  sin2w
      double precision alphas
      common /ubpcom/  alphas
      double precision ebeam
      common /ubpcom/  ebeam
      double precision epol
      common /ubpcom/  epol
      double precision ppol
      common /ubpcom/  ppol
      double precision ctsmin
      common /ubpcom/  ctsmin
      double precision ctsmax
      common /ubpcom/  ctsmax
      double precision ecut
      common /ubpcom/  ecut
      double precision acocut
      common /ubpcom/  acocut
      double precision evisct
      common /ubpcom/  evisct
      integer          nevent, pad022
      common /ubpcom/  nevent, pad022
      integer          rseed , pad023
      common /ubpcom/  rseed , pad023
      logical          tchann, pad024
      common /ubpcom/  tchann, pad024
      logical          qedvtx, pad025
      common /ubpcom/  qedvtx, pad025
      logical          qedbox, pad026
      common /ubpcom/  qedbox, pad026
      logical          weak  , pad027
      common /ubpcom/  weak  , pad027
      logical          boxes , pad028
      common /ubpcom/  boxes , pad028
      integer          isrtyp, pad029
      common /ubpcom/  isrtyp, pad029
      integer          fsrtyp, pad030
      common /ubpcom/  fsrtyp, pad030
      double precision epsiln
      common /ubpcom/  epsiln
      double precision taumin
      common /ubpcom/  taumin
      double precision taumax
      common /ubpcom/  taumax
      double precision emin
      common /ubpcom/  emin
      double precision eps2
      common /ubpcom/  eps2
      double precision qsq
      common /ubpcom/  qsq
      integer          bstyle, pad037
      common /ubpcom/  bstyle, pad037
      logical          nonlog, pad038
      common /ubpcom/  nonlog, pad038
      integer          stdin , pad039
      common /ubpcom/  stdin , pad039
      integer          stdout, pad040
      common /ubpcom/  stdout, pad040
      integer          stderr, pad041
      common /ubpcom/  stderr, pad041
      integer          errcnt, pad042
      common /ubpcom/  errcnt, pad042
      integer          errmax, pad043
      common /ubpcom/  errmax, pad043
      integer          verbos, pad044
      common /ubpcom/  verbos, pad044
      integer          status, pad045
      common /ubpcom/  status, pad045
      integer          hepdat, pad046
      common /ubpcom/  hepdat, pad046
      integer          heprev, pad047
      common /ubpcom/  heprev, pad047
      integer          runid , pad048
      common /ubpcom/  runid , pad048
      logical          dbgcol, pad049
      common /ubpcom/  dbgcol, pad049
      logical          dbghep, pad050
      common /ubpcom/  dbghep, pad050
      logical          dbgini, pad051
      common /ubpcom/  dbgini, pad051
      logical          dbgmas, pad052
      common /ubpcom/  dbgmas, pad052
      logical          dbgmup, pad053
      common /ubpcom/  dbgmup, pad053
      double precision cacomx, logeps, lneps2, nlgsca
      common /ubcmsc/ cacomx, logeps, lneps2, nlgsca

      integer BEAME, BEAMP, INDEXE, INDEXP
      parameter (BEAME = 1, BEAMP = 2, INDEXE = 9, INDEXP = 10)

      double precision tmp, cosaco, cosths, thetam, thetap
      integer i

      ubgacc = .false.

c First cut to impose: lepton energies

      if (p(4,INDEXE) .lt. ecut .or.
     &    p(4,INDEXP) .lt. ecut)  return

c Next cut: acollinearity

      tmp = 0.d0
      do 10 i = 1, 3
         tmp = tmp + p(i,INDEXE) * p(i,INDEXP)
 10   continue
      cosaco = - tmp / (p(4,INDEXE) * p(4,INDEXP))

      if (cosaco .lt. cacomx)  return

c And finally cut in cos(theta*), which is defined by:
c  cos(theta*) = cos(0.5*(theta- + theta+)) / cos(0.5*(theta- + theta+))
c where theta- is the scattering angle of e- and theta+ that of e+.

      tmp = 0.d0
      do 20 i = 1, 3
         tmp = tmp + p(i,INDEXE) * p(i,BEAME)
 20   continue
      thetam = acos (tmp / (p(4,INDEXE) * p(4,BEAME)))

      tmp = 0.d0
      do 30 i = 1, 3
         tmp = tmp + p(i,INDEXP) * p(i,BEAMP)
 30   continue
      thetap = acos (tmp / (p(4,INDEXP) * p(4,BEAMP)))

      cosths = cos (0.5d0*(thetam+thetap)) / cos (0.5d0*(thetam-thetap))

      if (cosths .gt. ctsmax .or. cosths .lt. ctsmin)  return

c We have passed all cuts, accept it!

      ubgacc = .true.

      end
CDECK  ID>, UBCLOS. 
*CMZ :  2.02/00 12/12/95  18.38.19  by  Harald Anlauf
*CMZ :  2.01/06 15/05/95  09.53.50  by  Harald Anlauf
*CMZ :  2.01/05 24/03/95  16.30.06  by  Harald Anlauf
*CMZ :  2.01/01 30/01/95  22.12.35  by  Harald Anlauf
*CMZ :  2.00/01 29/09/94  16.36.24  by  Harald Anlauf
*CMZ :  1.99/05 01/06/93  15.42.48  by  Harald Anlauf & Helge Meinhard
*CMZ :  1.99/01 14/05/93  12.15.23  by  Harald Anlauf & Helge Meinhard
*CMZ :  1.99/00 10/05/93  14.55.36  by  Harald Anlauf & Helge Meinhard
*CMZ :- 1.02/99- 6/07/92  22.09.51  by  Thorsten Ohl <ohl@ips105>
*-- Author :    Thorsten Ohl <ohl@ips105>

      subroutine ubclos (nev)
      implicit none
      integer nev

      double precision brnmax
      common /ubcbrn/ brnmax

      double precision cacomx, logeps, lneps2, nlgsca
      common /ubcmsc/ cacomx, logeps, lneps2, nlgsca
      double precision maxws, maxwt,
     &                 maxwnl, wnlvs, wnlhrd, wnlavh, wnlbare
      common /ubcsta/  maxws, maxwt,
     &                 maxwnl, wnlvs, wnlhrd, wnlavh, wnlbare

      integer          isbad, iscall, isfail, isrej, callct, acccut
      common /ubcsta/  isbad, iscall, isfail, isrej, callct, acccut

      integer          itbad, itcall, itfail, nlbad, nlcall, nlfail
      common /ubcsta/  itbad, itcall, itfail, nlbad, nlcall, nlfail

      integer          nlvs, nlhrd
      common /ubcsta/  nlvs, nlhrd

      double precision xsct, errmc, CONV

c The conversion factor from pbarn to mbarn
      parameter (CONV = 1.d-9)

      if (iscall .eq. 0) then
         call ubumsg ('ubclos', 100, 'No events generated!')
      endif

c Calculate the total X-section:

      xsct  = brnmax / nlgsca * CONV * dble (nev) / dble (iscall)

c Estimate the error of the Monte Carlo integration:

      errmc = brnmax / nlgsca * CONV
     &      * sqrt ((dble (iscall-nev) * nev) / dble (iscall)**3)

      call ubeens (nev, xsct, errmc)

      end
CDECK  ID>, UBSTAT. 
*CMZ :  2.01/06 29/03/95  14.39.10  by  Harald Anlauf
*CMZ :  2.01/05 24/03/95  00.49.13  by  Harald Anlauf
*CMZ :  2.01/01 30/01/95  22.05.27  by  Harald Anlauf
*CMZ :  2.00/01 29/09/94  16.34.13  by  Harald Anlauf
*CMZ :  1.99/05 01/06/93  21.57.13  by  Harald Anlauf & Helge Meinhard
*CMZ :  1.99/01 14/05/93  11.06.54  by  Harald Anlauf & Helge Meinhard
*CMZ :  1.99/00 07/05/93  10.24.24  by  Harald Anlauf & Helge Meinhard
*CMZ :- 1.02/99-15/07/92  15.11.08  by  Thorsten Ohl <ohl@ips104>
*-- Author :    Thorsten Ohl <ohl@ips104>

      subroutine ubstat ()
      implicit none

      double precision maxws, maxwt,
     &                 maxwnl, wnlvs, wnlhrd, wnlavh, wnlbare
      common /ubcsta/  maxws, maxwt,
     &                 maxwnl, wnlvs, wnlhrd, wnlavh, wnlbare

      integer          isbad, iscall, isfail, isrej, callct, acccut
      common /ubcsta/  isbad, iscall, isfail, isrej, callct, acccut

      integer          itbad, itcall, itfail, nlbad, nlcall, nlfail
      common /ubcsta/  itbad, itcall, itfail, nlbad, nlcall, nlfail

      integer          nlvs, nlhrd
      common /ubcsta/  nlvs, nlhrd

      character*55 buf

      if (iscall .eq. 0) then
         call ubumsg ('ubstat', 99,
     &        'No events generated, so no statistics!')
         return
      endif

      call ubumsg ('ubstat', -1, 'Statistics:')
      write (buf, 501) (1 - dble (isfail) / iscall) * 100
 501  format ('branching efficiency:           ', F7.3, ' %')
      call ubumsg ('ubstat', -1, buf)
      write (buf, 502) dble (isbad) / (iscall - isfail) * 100
 502  format ('bad weights in s generation:    ', F7.3, ' %')
      call ubumsg ('ubstat', -1, buf)
      write (buf, 503) maxws
 503  format ('largest weight in s generation: ', F7.3)
      call ubumsg ('ubstat', -1, buf)
      write (buf, 504) dble (isrej) / (iscall - isfail - isbad) * 100
 504  format ('s-channel rejection efficiency: ', F7.3, ' %')
      call ubumsg ('ubstat', -1, buf)
      write (buf, 511) (1 - dble (itfail) / itcall) * 100
 511  format ('diff. x-section efficiency:     ', F7.3, ' %')
      call ubumsg ('ubstat', -1, buf)
      write (buf, 512) dble (itbad) / (itcall - itfail) * 100
 512  format ('bad weights in diff. x-section: ', F7.3, ' %')
      call ubumsg ('ubstat', -1, buf)
      write (buf, 513) maxwt
 513  format ('largest weight in t generation: ', F7.3)
      call ubumsg ('ubstat', -1, buf)

      if (nlcall .gt. 0) then
      write (buf, 521) (1 - dble (nlfail) / nlcall) * 100
 521  format ('non-log correction efficiency:  ', F7.3, ' %')
      call ubumsg ('ubstat', -1, buf)
      write (buf, 522) dble (nlbad) / (nlcall - nlfail) * 100
 522  format ('bad weights for non-log terms:  ', F7.3, ' %')
      call ubumsg ('ubstat', -1, buf)
      write (buf, 523) maxwnl
 523  format ('largest wgt. for non-log terms: ', F7.3)
      call ubumsg ('ubstat', -1, buf)

      write (buf, 524) wnlvs / nlvs
 524  format ('average for non-log v+s terms:  ', F7.3)
      call ubumsg ('ubstat', -1, buf)
      write (buf, 525) wnlhrd / nlhrd
 525  format ('average for non-log hard terms: ', F7.3)
      call ubumsg ('ubstat', -1, buf)
      write (buf, 526) wnlbare / nlhrd
 526  format ('average of bare hard weight:    ', F7.3)
      call ubumsg ('ubstat', -1, buf)
      write (buf, 527) wnlavh / nlhrd
 527  format ('average of modified hard weight:', F7.3)
      call ubumsg ('ubstat', -1, buf)

      endif

      write (buf, 530) (dble (acccut) / callct) * 100
 530  format ('Cut efficiency:                 ', F7.3, ' %')
      call ubumsg ('ubstat', -1, buf)

      end
CDECK  ID>, XSECTION.   
*CMZ :- 1.02/99-15/07/92  14.58.18  by  Thorsten Ohl <ohl@ips104>
CDECK  ID>, BLANKDEK.   
*CMZ :  1.99/00 07/05/93  14.50.09  by  Harald Anlauf & Helge Meinhard
*CMZ :  1.02/99 03/05/93  18.02.13  by  Unknown
*-- Author :    Thorsten Ohl <ohl@ips104>

C Bhabha cross sections.


CDECK  ID>, UBXTOT. 
*CMZ :  2.00/01 29/09/94  16.34.43  by  Harald Anlauf
*CMZ :  1.99/00 07/05/93  14.50.09  by  Harald Anlauf & Helge Meinhard
*CMZ :- 1.02/99- 6/07/92  22.11.07  by  Thorsten Ohl <ohl@ips105>
*-- Author :    Thorsten Ohl <ohl@ips105>

c UBXTOT - the integrated trial cross section

      function ubxtot (s, taumin, taumax)
      implicit none
      double precision ubxtot, s, taumin, taumax

      double precision a, b, prefac

c setup of the coefficients:
      call ubxcof (s, a, b, prefac)

      ubxtot = prefac * (a*(taumax-taumin) + b*(1/taumin-1/taumax))

      end


CDECK  ID>, UBXTRI. 
*CMZ :  1.99/00 07/05/93  14.50.09  by  Harald Anlauf & Helge Meinhard
*-- Author :    Harald Anlauf & Helge Meinhard   06/05/93

c UBXTRI - the differential trial cross section  d(sigma)/d(cos(theta))
c          which is a majorant of the full differential cross section.

      function ubxtri (s, tau)
      implicit none
      double precision ubxtri, s, tau

      double precision a, b, prefac

c setup of the coefficients:
      call ubxcof (s, a, b, prefac)

c calculate the differential trial cross section
c Remember: d(sigma)/d(cos(theta)) = 0.5 * d(sigma)/d(tau)
      ubxtri = 0.5d0 * prefac * (a + b / tau**2)

      end
CDECK  ID>, UBXCOF. 
*CMZ :  2.01/01 30/01/95  21.56.56  by  Harald Anlauf
*CMZ :  2.00/01 30/09/94  16.24.50  by  Harald Anlauf
*CMZ :  1.99/05 01/06/93  12.33.09  by  Harald Anlauf & Helge Meinhard
*CMZ :  1.99/00 07/05/93  15.21.11  by  Harald Anlauf & Helge Meinhard
*-- Author :    Harald Anlauf & Helge Meinhard   06/05/93

c UBXCOF - calculate the coefficients of the trial cross section,
c          where the differential trial cross section has the form
c          d(sigma)/d(tau) = prefac * ( A(s) + B(s)/tau**2 )

      subroutine ubxcof (s, a, b, prefac)
      implicit none
      double precision s, a, b, prefac
      double precision ahpla
      common /ubpcom/  ahpla
      double precision alpha
      common /ubpcom/  alpha
      double precision mass1e
      common /ubpcom/  mass1e
      double precision mass2e
      common /ubpcom/  mass2e
      double precision mass1z
      common /ubpcom/  mass1z
      double precision mass2z
      common /ubpcom/  mass2z
      double precision mass1t
      common /ubpcom/  mass1t
      double precision mass1h
      common /ubpcom/  mass1h
      double precision mass1w
      common /ubpcom/  mass1w
      double precision gamm1z
      common /ubpcom/  gamm1z
      double precision gamm2z
      common /ubpcom/  gamm2z
      double precision sin2w
      common /ubpcom/  sin2w
      double precision alphas
      common /ubpcom/  alphas
      double precision ebeam
      common /ubpcom/  ebeam
      double precision epol
      common /ubpcom/  epol
      double precision ppol
      common /ubpcom/  ppol
      double precision ctsmin
      common /ubpcom/  ctsmin
      double precision ctsmax
      common /ubpcom/  ctsmax
      double precision ecut
      common /ubpcom/  ecut
      double precision acocut
      common /ubpcom/  acocut
      double precision evisct
      common /ubpcom/  evisct
      integer          nevent, pad022
      common /ubpcom/  nevent, pad022
      integer          rseed , pad023
      common /ubpcom/  rseed , pad023
      logical          tchann, pad024
      common /ubpcom/  tchann, pad024
      logical          qedvtx, pad025
      common /ubpcom/  qedvtx, pad025
      logical          qedbox, pad026
      common /ubpcom/  qedbox, pad026
      logical          weak  , pad027
      common /ubpcom/  weak  , pad027
      logical          boxes , pad028
      common /ubpcom/  boxes , pad028
      integer          isrtyp, pad029
      common /ubpcom/  isrtyp, pad029
      integer          fsrtyp, pad030
      common /ubpcom/  fsrtyp, pad030
      double precision epsiln
      common /ubpcom/  epsiln
      double precision taumin
      common /ubpcom/  taumin
      double precision taumax
      common /ubpcom/  taumax
      double precision emin
      common /ubpcom/  emin
      double precision eps2
      common /ubpcom/  eps2
      double precision qsq
      common /ubpcom/  qsq
      integer          bstyle, pad037
      common /ubpcom/  bstyle, pad037
      logical          nonlog, pad038
      common /ubpcom/  nonlog, pad038
      integer          stdin , pad039
      common /ubpcom/  stdin , pad039
      integer          stdout, pad040
      common /ubpcom/  stdout, pad040
      integer          stderr, pad041
      common /ubpcom/  stderr, pad041
      integer          errcnt, pad042
      common /ubpcom/  errcnt, pad042
      integer          errmax, pad043
      common /ubpcom/  errmax, pad043
      integer          verbos, pad044
      common /ubpcom/  verbos, pad044
      integer          status, pad045
      common /ubpcom/  status, pad045
      integer          hepdat, pad046
      common /ubpcom/  hepdat, pad046
      integer          heprev, pad047
      common /ubpcom/  heprev, pad047
      integer          runid , pad048
      common /ubpcom/  runid , pad048
      logical          dbgcol, pad049
      common /ubpcom/  dbgcol, pad049
      logical          dbghep, pad050
      common /ubpcom/  dbghep, pad050
      logical          dbgini, pad051
      common /ubpcom/  dbgini, pad051
      logical          dbgmas, pad052
      common /ubpcom/  dbgmas, pad052
      logical          dbgmup, pad053
      common /ubpcom/  dbgmup, pad053
      integer nrmass
      PARAMETER ( NRMASS = 9 )
      double precision RMASS, RMASS2, VF, AF, QF
      COMMON/ MASSES / RMASS(0:NRMASS), RMASS2(0:NRMASS),
     &          VF(0:NRMASS), AF(0:NRMASS), QF(0:NRMASS)
      double precision t3f, mq2mz
      common/ masses / t3f(0:NRMASS), mq2mz(0:NRMASS)
      integer IWEAK, IFERM, ICHANN, IBOXES, IOUT
      COMMON/ OPTION / IWEAK, IFERM, ICHANN, IBOXES, IOUT
      double precision HBARC2, PI
      parameter (HBARC2 = 0.38937966d9, PI = 3.141592653589793238d0)
      double precision ALFA, ALFAS, FACQCD, FACQED, dens
      COMMON/ ADHOC  / ALFA, ALFAS, FACQCD, FACQED, dens(-1:1,-1:1)

      double precision alfeff, chiz2

c "Effective" number of fermions for the photonic vacuum polarization
c (running alpha) in the LEP energy range, where 3 light leptons
c (e,mu,tau) and five quarks (u,c; d,s,b) contribute.
      double precision NEFF
      parameter (NEFF = (3.d0 + 11.d0/3.d0))

c expirical fudge factor in order to get a majorant of the full x-section
      double precision FUDGE
      parameter (FUDGE = 0.1d0)


      if (iweak .eq. 0) then
         alfeff = alfa
      else
c take running alpha into account
c (to stay on the save side, use m_e as mass of the effective fermions)
         alfeff = alfa / (1.d0 - NEFF * alfa/(3.d0*PI)
     &                           * (log (s/mass2e) - 5.d0/3.d0))
      endif

      chiz2 = s**2 / ((s-mass2z)**2 + mass2z*gamm2z)

c the constant coefficient:
      a = chiz2 * (dens( 1,-1) + dens(-1, 1))
     &  * ((vf(1)**2 - af(1)**2 + (s-mass2z)/s)**2 + mass2z*gamm2z/s**2)

c the coefficient of 1/tau**2:
      b = 1 + FUDGE * chiz2 * abs (s-mass2z)/s * 2 *
     &     ( dens( 1,-1) * (vf(1)-af(1))**2
     &     + dens(-1, 1) * (vf(1)+af(1))**2 )

      prefac = 4 * HBARC2 * PI * alfeff**2 / s

      end
CDECK  ID>, UBXDIF. 
*CMZ :  1.99/00 07/05/93  14.50.09  by  Harald Anlauf & Helge Meinhard
*CMZ :- 1.02/99- 6/07/92  22.10.58  by  Thorsten Ohl <ohl@ips105>
*-- Author :    Thorsten Ohl <ohl@ips105>

c UBXDIF - differential Bhabha cross section d(sigma)/d(cos(theta))
c          including weak corrections.
c          This function is intended as as interface to the
c          electroweak library.

      double precision function ubxdif (s, tau)
      implicit none
      double precision s, tau

      double precision costh
      double precision eeeew
      external eeeew

c Call the (modified) function from ALIBABA
      costh = 1.d0 - 2.d0 * tau
      ubxdif = eeeew (s, costh)

      end
CDECK  ID>, DILOG.  
*CMZ :  2.01/01 30/01/95  15.02.08  by  Harald Anlauf
*-- Author :    Harald Anlauf   30/01/95

c DILOG - Dilogarithm for real argument (faster than complex version)

      FUNCTION DILOG(X)
      implicit none
      double precision dilog, x, a, b, s, t, y, z
      Z=-1.644934066848226D0
      IF(ABS(X-1.D0) .LE. 1.D-17) THEN
         DILOG=1.644934066848226D0
      ELSE
         IF(X.LE.-1.D0 .OR. X.GT.2.D0)THEN
            IF(X.GT.2.D0) Z=3.289868133696453D0
            T=1.D0/X
            S=-0.5D0
            Z=Z-0.5D0*LOG(ABS(X))**2
         ELSEIF(X .LE. 0.5D0)THEN
            T=X
            S=0.5D0
            Z=0.D0
         ELSEIF(X .LE. 2.D0)THEN
            T=1.D0-X
            S=-0.5D0
            Z=1.644934066848226D0-LOG(X)*LOG(ABS(T))
         ENDIF
         Y=2.666666666666667D0*T+0.666666666666667D0
         B=      0.000000000000001D0
         A=Y*B  +0.000000000000004D0
         B=Y*A-B+0.000000000000011D0
         A=Y*B-A+0.000000000000037D0
         B=Y*A-B+0.000000000000121D0
         A=Y*B-A+0.000000000000398D0
         B=Y*A-B+0.000000000001312D0
         A=Y*B-A+0.000000000004342D0
         B=Y*A-B+0.000000000014437D0
         A=Y*B-A+0.000000000048274D0
         B=Y*A-B+0.000000000162421D0
         A=Y*B-A+0.000000000550291D0
         B=Y*A-B+0.000000001879117D0
         A=Y*B-A+0.000000006474338D0
         B=Y*A-B+0.000000022536705D0
         A=Y*B-A+0.000000079387055D0
         B=Y*A-B+0.000000283575385D0
         A=Y*B-A+0.000001029904264D0
         B=Y*A-B+0.000003816329463D0
         A=Y*B-A+0.000014496300557D0
         B=Y*A-B+0.000056817822718D0
         A=Y*B-A+0.000232002196094D0
         B=Y*A-B+0.001001627496164D0
         A=Y*B-A+0.004686361959447D0
         B=Y*A-B+0.024879322924228D0
         A=Y*B-A+0.166073032927855D0
         A=Y*A-B+1.935064300869969D0
         DILOG=S*T*(A-B)+Z
      ENDIF
      END
CDECK  ID>, UBFFAC. 
*CMZ :  2.01/08 29/11/95  14.16.07  by  Harald Anlauf
*-- Author :    Harald Anlauf   21/11/95

      subroutine ubffac (s, t, u, FFs, FFt,
     &                   Vggs, Aggs, VgZs, AgZs, Vggt, Aggt, VgZt, AgZt)
c     -----------------
c Virtual+soft corrections via form factors and K-factors.
c
c We implement the 1-loop expressions for the virtual corrections
c through form-factors in the invidual amplitudes.
c
      implicit none
      double precision s, t, u
      double complex FFs, FFt
      double complex Vggs, Aggs, VgZs, AgZs, Vggt, Aggt, VgZt, AgZt
      double precision ahpla
      common /ubpcom/  ahpla
      double precision alpha
      common /ubpcom/  alpha
      double precision mass1e
      common /ubpcom/  mass1e
      double precision mass2e
      common /ubpcom/  mass2e
      double precision mass1z
      common /ubpcom/  mass1z
      double precision mass2z
      common /ubpcom/  mass2z
      double precision mass1t
      common /ubpcom/  mass1t
      double precision mass1h
      common /ubpcom/  mass1h
      double precision mass1w
      common /ubpcom/  mass1w
      double precision gamm1z
      common /ubpcom/  gamm1z
      double precision gamm2z
      common /ubpcom/  gamm2z
      double precision sin2w
      common /ubpcom/  sin2w
      double precision alphas
      common /ubpcom/  alphas
      double precision ebeam
      common /ubpcom/  ebeam
      double precision epol
      common /ubpcom/  epol
      double precision ppol
      common /ubpcom/  ppol
      double precision ctsmin
      common /ubpcom/  ctsmin
      double precision ctsmax
      common /ubpcom/  ctsmax
      double precision ecut
      common /ubpcom/  ecut
      double precision acocut
      common /ubpcom/  acocut
      double precision evisct
      common /ubpcom/  evisct
      integer          nevent, pad022
      common /ubpcom/  nevent, pad022
      integer          rseed , pad023
      common /ubpcom/  rseed , pad023
      logical          tchann, pad024
      common /ubpcom/  tchann, pad024
      logical          qedvtx, pad025
      common /ubpcom/  qedvtx, pad025
      logical          qedbox, pad026
      common /ubpcom/  qedbox, pad026
      logical          weak  , pad027
      common /ubpcom/  weak  , pad027
      logical          boxes , pad028
      common /ubpcom/  boxes , pad028
      integer          isrtyp, pad029
      common /ubpcom/  isrtyp, pad029
      integer          fsrtyp, pad030
      common /ubpcom/  fsrtyp, pad030
      double precision epsiln
      common /ubpcom/  epsiln
      double precision taumin
      common /ubpcom/  taumin
      double precision taumax
      common /ubpcom/  taumax
      double precision emin
      common /ubpcom/  emin
      double precision eps2
      common /ubpcom/  eps2
      double precision qsq
      common /ubpcom/  qsq
      integer          bstyle, pad037
      common /ubpcom/  bstyle, pad037
      logical          nonlog, pad038
      common /ubpcom/  nonlog, pad038
      integer          stdin , pad039
      common /ubpcom/  stdin , pad039
      integer          stdout, pad040
      common /ubpcom/  stdout, pad040
      integer          stderr, pad041
      common /ubpcom/  stderr, pad041
      integer          errcnt, pad042
      common /ubpcom/  errcnt, pad042
      integer          errmax, pad043
      common /ubpcom/  errmax, pad043
      integer          verbos, pad044
      common /ubpcom/  verbos, pad044
      integer          status, pad045
      common /ubpcom/  status, pad045
      integer          hepdat, pad046
      common /ubpcom/  hepdat, pad046
      integer          heprev, pad047
      common /ubpcom/  heprev, pad047
      integer          runid , pad048
      common /ubpcom/  runid , pad048
      logical          dbgcol, pad049
      common /ubpcom/  dbgcol, pad049
      logical          dbghep, pad050
      common /ubpcom/  dbghep, pad050
      logical          dbgini, pad051
      common /ubpcom/  dbgini, pad051
      logical          dbgmas, pad052
      common /ubpcom/  dbgmas, pad052
      logical          dbgmup, pad053
      common /ubpcom/  dbgmup, pad053
      double precision PI
      parameter (PI = 3.141592653589793238d0)
      double precision cacomx, logeps, lneps2, nlgsca
      common /ubcmsc/ cacomx, logeps, lneps2, nlgsca
      double precision RMZ,RMW,RMH,RMT, ZWID, SIN2TH, sw, cw, cw2, pizmz
      COMMON/ BOSONS / RMZ,RMW,RMH,RMT, ZWID, SIN2TH, sw, cw, cw2, pizmz

      double complex mz2s, mz2t, Lse, Lsf, Lsl, lamdai, lamdaf, lamdas
      double complex Gst, Gsu, Gts, Gtu, Ast, Asu, Ats, Atu
      double complex VIRs, VIRt, FFs1, Cphase
      double precision scale, Lsubs, Lsubt, Lslam, Ltlam, Lte, Ltl,
     &     lamdat, Be, Bf, Bint, softi, softf, softif, llvs, fab,
     &     FF1uni, FF1int, FFt1
      double precision alfapi, alf2pi

      double precision dilog
      double complex spence
      external dilog, spence

c Statement functions for the QED box contributions:
      double complex G, A, m2
      double precision sl, tl, ul, OFFSET
      parameter (OFFSET = 1d-10)
      G(sl,tl,ul)    = sl/(2*ul) * ( log (dcmplx (sl, OFFSET)/tl) -
     &                  (sl+2*tl)/(2*ul) * (
     &                  log (dcmplx (sl, OFFSET)/tl)**2 + PI**2 ) )
      A(sl,tl,ul,m2) = (m2-sl)/ul * ( log (tl/(sl-m2)) +
     &                  m2/sl*log (1-sl/m2) + (2*ul+sl-m2)/ul*(
     &                   log (-tl/m2)*log ((m2-sl)/(m2+tl)) +
     &                   spence (sl/m2) - spence (-tl/m2) ) )

      if (isrtyp .eq. 0 .or. fsrtyp .eq. 0 .or. .not. qedvtx) then
c No full QED corrections, thus trivial form factors:
         FFs = 1
         FFt = 1
         Vggs = 0
         Aggs = 0
         VgZs = 0
         AgZs = 0
         Vggt = 0
         Aggt = 0
         VgZt = 0
         AgZt = 0
         return
      endif

c The Large Logarithms:
      Lsubs = log (s/mass2e)
      Lsubt = log (-t/mass2e)
c arbitrary scale to check cancellation of auxiliary photon regulator mass
      scale = s
*      scale = s*t/u
      Lslam = log (s/scale)
      Ltlam = log (-t/scale)

c --- Soft photon corrections: Beenakker et al., (A.6)
      Be = Lsubs - 1
      Bf = Be
      Bint = 2 * log (t/u)
      fab  = dilog (1+s/u) - dilog (1+s/t)
      softi  = (2 * logeps + Lslam) * Be
     &         - 0.5d0 * Be * Be + 0.5d0 - PI**2/3
*      softf  = softi
      softf  = (2 * logeps + Lslam) * Bf
     &         - 0.5d0 * Bf * Bf + 0.5d0 - PI**2/3
      softif = (2 * logeps + Lslam) * Bint + 2 * fab

c The (universal) virtual+soft corrections already taken into account in
c the leading log structure functions (radiators):
      if (fsrtyp .eq. 1) then
         llvs = (2 * logeps + 1.5d0) * Be
     &        + (2 * logeps + 1.5d0) * Bf + 0.5d0
      elseif (fsrtyp .eq. 2) then
         llvs = (2 * logeps + 1.5d0) * Be
     &        + (2 * logeps + 1.5d0) * Lsubt + 0.5d0
      else
         llvs = (2 * logeps + 1.5d0) * Be
     &        + (2 * logeps + 1.5d0) * (Bf + 0.5d0*Bint) + 0.5d0
      endif

c Since we have not yet implemented initial/final interference for
c real photon emission, we have to *pretend* the presence of the
c corresponding contributions in the LL soft+virtual corrections,
c so that the cross section is at least independent of epsiln:
      if (fsrtyp .eq. 1) then
         llvs = llvs + 2 * fab + 2 * logeps *  Bint
      elseif (fsrtyp .eq. 2) then
         llvs = llvs + 2 * fab + 2 * logeps * (Bint + Bf - Lsubt)
      else
         llvs = llvs + 2 * fab +     logeps *  Bint
      endif

c (As for any LL calculation, this contribution is arbitrary and
c could be "matched" to a more accurate calculation to get the
c forward-backward asymmetry right on the Z peak).

c --- Photonic vertex correction: (A.1)
c "Initial state; s-channel":
      Lse = dcmplx (Lsubs, -PI)
      Lsl = dcmplx (Lslam, -PI)
      lamdai = -Lsl*(Lse-1) + 0.5d0*Lse*(Lse+1) + PI**2/6 - 2
c "Final state; s-channel":
      Lsf = dcmplx (Lsubs, -PI)
      lamdaf = -Lsl*(Lsf-1) + 0.5d0*Lsf*(Lsf+1) + PI**2/6 - 2
      lamdas = lamdai + lamdaf
c "t-channel":
      Lte = Lsubt
      Ltl = Ltlam
      lamdat = -2*Ltl*(Lte-1) + Lte*(Lte+1) + PI**2/3 - 4

c Remove the Coulomb phase explicitly from the vertex correction:
      Cphase = dcmplx (0.d0, 2*PI*Lslam)
      lamdas = lamdas - Cphase

c (Note that for t-channel diagrams the Coulomb phase appears
c in the box diagrams! (see below)).


c --- The Box corrections:
      if (.not. qedbox) then
c Just the "universal" infrared-divergent pieces of the QED boxes:
         VIRt = - 2 * Ltl * dcmplx (log (-s/u), -PI)
         VIRs = - Lsl * Bint
         Vggs = VIRs
         Aggs = 0
         VgZs = VIRs
         AgZs = 0
         Vggt = VIRt
         Aggt = 0
         VgZt = VIRt
         AgZt = 0
         goto 10
      endif

c gamma-gamma boxes: (A.3)
      Gst  = G (s,t,u)
      Gsu  = G (s,u,t)
      Aggs = Gst + Gsu
      Vggs = Gst - Gsu - Lsl * Bint
      Gts  = G (t,s,u)
      Gtu  = G (t,u,s)
      Aggt = Gts + Gtu
      Vggt = Gts - Gtu - 2 * Ltl * dcmplx (log (-s/u), -PI)

c gamma-Z boxes: (A.5)
      mz2s = dcmplx (mass2z , - mass1z*zwid)
      Ast  = A (s,t,u,mz2s)
      Asu  = A (s,u,t,mz2s)
      AgZs = Ast + Asu
      VgZs = Ast - Asu + 2 * (spence (1+mz2s/t) - spence (1+mz2s/u)) -
     &       (lslam + 2 * log (mz2s/s-1) - log (mz2s/s)) * Bint

      mz2t = dcmplx (mass2z , - OFFSET     )
      Ats  = A (t,s,u,mz2t)
      Atu  = A (t,u,s,mz2t)
      AgZt = Ats + Atu
      VgZt = Ats - Atu + 2 * (spence (1+mz2t/s) - spence (1+mz2t/u)) -
     &       2*(ltlam + log ((mz2t-t)**2/(-t*mz2t))) *
     &          dcmplx (log (-s/u), -PI)

c A resonant interference factor has to be factored out for the
c s-channel, since we do not yet have initial/final interference
c included.  This factor controls the cut-off of i/f-interference
c effects in the hard photon radiation.
c (See e.g. Caffo & Remiddi, coefficient C^(10)_infra).
      llvs = llvs - 2 * Bint * log (abs (1 - mz2s/s))


 10   continue


c Remove the Coulomb phase explicitly from the t-channel boxes:
      Vggt = Vggt - Cphase
      VgZt = VgZt - Cphase

c The form-factors, as they will be applied to the amplitudes:
      FF1uni = softi + softf - llvs
      FF1int = softif

      FFs1 = lamdas + FF1uni + FF1int
      FFt1 = lamdat + FF1uni + FF1int

      alfapi = alpha/PI
      alf2pi = 0.5d0 * alfapi

      FFs = 1 + alf2pi * FFs1
      FFt = 1 + alf2pi * FFt1

      Vggs = alf2pi * Vggs
      Aggs = alf2pi * Aggs
      VgZs = alf2pi * VgZs
      AgZs = alf2pi * AgZs
      Vggt = alf2pi * Vggt
      Aggt = alf2pi * Aggt
      VgZt = alf2pi * VgZt
      AgZt = alf2pi * AgZt

      end
CDECK  ID>, BRANCH. 
*CMZ :- 1.02/99-15/07/92  14.59.06  by  Thorsten Ohl <ohl@ips104>
CDECK  ID>, BLANKDEK.   
*CMZ :  2.01/06 30/03/95  22.13.17  by  Harald Anlauf
*CMZ :  1.02/99 03/05/93  18.02.13  by  Unknown
*-- Author :    Thorsten Ohl <ohl@ips104>

C This patch contains the implementation of the forward branching
C algorithms for initial and final state radiation.

CDECK  ID>, UBBINI. 
*CMZ :  2.01/08 29/11/95  16.03.08  by  Harald Anlauf
*CMZ :  2.01/07 17/05/95  16.49.59  by  Harald Anlauf
*CMZ :  2.01/06 16/05/95  14.24.59  by  Harald Anlauf
*CMZ :  2.00/01 29/09/94  16.35.36  by  Harald Anlauf
*CMZ :  1.99/06 02/06/93  10.24.30  by  Harald Anlauf & Helge Meinhard
*CMZ :  1.99/05 01/06/93  21.51.50  by  Harald Anlauf & Helge Meinhard
*CMZ :  1.99/04 27/05/93  13.03.03  by  Harald Anlauf & Helge Meinhard
*CMZ :  1.99/01 13/05/93  17.15.17  by  Harald Anlauf & Helge Meinhard
*CMZ :  1.99/00 11/05/93  18.08.33  by  Harald Anlauf & Helge Meinhard
*CMZ :- 1.02/99-26/08/92  11.02.45  by  Angelika Himmler and Dirk Uwe Sauer
*-- Author :    Angelika Himmler and Dirk Uwe Sauer

c Perform a simple forward branching on the lepton stored as particle
c number PTR in /hepevt/.   Store the generated photons directly
c in /ubcevt/.

      subroutine ubbini (ptr, scale)
      implicit none
      integer ptr
      double precision scale

      integer NMXINT
      parameter (NMXINT = 100)
      double precision p
      common /ubcevt/  p(5,NMXINT)

      double precision ipele, ippos, opele, oppos, savp1, savp2, savk
      common /ubcevt/  ipele(4), ippos(4), opele(4), oppos(4)
      common /ubcevt/  savp1(4), savp2(4), savk(4)
c Quarks
      integer PDGDWN, PDGUP, PDGSTR, PDGCHM, PDGBOT, PDGTOP
      parameter (PDGDWN =    1, PDGUP  =    2, PDGSTR =    3,
     $           PDGCHM =    4, PDGBOT =    5, PDGTOP =    6)
c Leptons
      integer PDGELE, PDGNUE, PDGMU, PDGNUM, PDGTAU, PDGNUT
      parameter (PDGELE =   11, PDGNUE =   12, PDGMU =    13,
     $           PDGNUM =   14, PDGTAU =   15, PDGNUT =   16)
c Bosons
      integer PDGGLU, PDGGLV, PDGGAM, PDGZ0, PDGWPL, PDGH0
      parameter (PDGGLU =   21, PDGGLV =    9, PDGGAM =   22,
     $           PDGZ0  =   23, PDGWPL =   24, PDGH0  =   25)
c Mesons
      integer PDGPIP, PDGPI0, PDGETA, PDGKPL, PDGK0, PDGK0S, PDGK0L
      parameter (PDGPIP =  211, PDGPI0 =  111, PDGETA =  221,
     $           PDGKPL =  321, PDGK0  =  311, PDGK0S =  310,
     $           PDGK0L =  130)
c Baryons
      integer PDGPRO, PDGNEU
      parameter (PDGPRO = 2212, PDGNEU = 2112)


      double precision PI
      parameter (PI = 3.141592653589793238d0)
      double precision ahpla
      common /ubpcom/  ahpla
      double precision alpha
      common /ubpcom/  alpha
      double precision mass1e
      common /ubpcom/  mass1e
      double precision mass2e
      common /ubpcom/  mass2e
      double precision mass1z
      common /ubpcom/  mass1z
      double precision mass2z
      common /ubpcom/  mass2z
      double precision mass1t
      common /ubpcom/  mass1t
      double precision mass1h
      common /ubpcom/  mass1h
      double precision mass1w
      common /ubpcom/  mass1w
      double precision gamm1z
      common /ubpcom/  gamm1z
      double precision gamm2z
      common /ubpcom/  gamm2z
      double precision sin2w
      common /ubpcom/  sin2w
      double precision alphas
      common /ubpcom/  alphas
      double precision ebeam
      common /ubpcom/  ebeam
      double precision epol
      common /ubpcom/  epol
      double precision ppol
      common /ubpcom/  ppol
      double precision ctsmin
      common /ubpcom/  ctsmin
      double precision ctsmax
      common /ubpcom/  ctsmax
      double precision ecut
      common /ubpcom/  ecut
      double precision acocut
      common /ubpcom/  acocut
      double precision evisct
      common /ubpcom/  evisct
      integer          nevent, pad022
      common /ubpcom/  nevent, pad022
      integer          rseed , pad023
      common /ubpcom/  rseed , pad023
      logical          tchann, pad024
      common /ubpcom/  tchann, pad024
      logical          qedvtx, pad025
      common /ubpcom/  qedvtx, pad025
      logical          qedbox, pad026
      common /ubpcom/  qedbox, pad026
      logical          weak  , pad027
      common /ubpcom/  weak  , pad027
      logical          boxes , pad028
      common /ubpcom/  boxes , pad028
      integer          isrtyp, pad029
      common /ubpcom/  isrtyp, pad029
      integer          fsrtyp, pad030
      common /ubpcom/  fsrtyp, pad030
      double precision epsiln
      common /ubpcom/  epsiln
      double precision taumin
      common /ubpcom/  taumin
      double precision taumax
      common /ubpcom/  taumax
      double precision emin
      common /ubpcom/  emin
      double precision eps2
      common /ubpcom/  eps2
      double precision qsq
      common /ubpcom/  qsq
      integer          bstyle, pad037
      common /ubpcom/  bstyle, pad037
      logical          nonlog, pad038
      common /ubpcom/  nonlog, pad038
      integer          stdin , pad039
      common /ubpcom/  stdin , pad039
      integer          stdout, pad040
      common /ubpcom/  stdout, pad040
      integer          stderr, pad041
      common /ubpcom/  stderr, pad041
      integer          errcnt, pad042
      common /ubpcom/  errcnt, pad042
      integer          errmax, pad043
      common /ubpcom/  errmax, pad043
      integer          verbos, pad044
      common /ubpcom/  verbos, pad044
      integer          status, pad045
      common /ubpcom/  status, pad045
      integer          hepdat, pad046
      common /ubpcom/  hepdat, pad046
      integer          heprev, pad047
      common /ubpcom/  heprev, pad047
      integer          runid , pad048
      common /ubpcom/  runid , pad048
      logical          dbgcol, pad049
      common /ubpcom/  dbgcol, pad049
      logical          dbghep, pad050
      common /ubpcom/  dbghep, pad050
      logical          dbgini, pad051
      common /ubpcom/  dbgini, pad051
      logical          dbgmas, pad052
      common /ubpcom/  dbgmas, pad052
      logical          dbgmup, pad053
      common /ubpcom/  dbgmup, pad053
      double precision cacomx, logeps, lneps2, nlgsca
      common /ubcmsc/ cacomx, logeps, lneps2, nlgsca

      integer MAXPHO
      parameter (MAXPHO=20)

      double precision ubrgen
      external ubrgen

      double precision pgam(5), zphot(MAXPHO), tphot(MAXPHO)
      double precision alf2pi, biglog, nbar, p0, r, betall, eta, etamin,
     &                 x, wgt, mef, ztmp, ttmp, tmp1, tmp2, sth, cth,
     &                 kt, phi, beta0, big
      integer n, nphot, i, j


c Calculate the average photon number for "emission from this line":

      big = scale / mass2e
      biglog = log (big) - 1.d0
      alf2pi = alpha / (2 * PI)
      nbar = alf2pi * biglog * (- 2 * logeps - 1.5d0 + 2 * epsiln)
      if (dbgmas) then
         nbar = nbar + alf2pi * 0.5d0
      endif

c Standard Poisson algorithm

      p0 = exp (-nbar)
      r = ubrgen (0)
      nphot = 0

 4    if (r .gt. p0 .and. nphot .lt. MAXPHO) then
         r = r * ubrgen (0)
         nphot = nphot + 1
         goto 4
      endif

      if (nphot .eq. 0) then
c Smoothly continue with a soft photon (exponentiated) part.
         betall = 2 * alf2pi * biglog
         r = ubrgen (0)
         x = 1.d0 - epsiln * r**(1.d0/betall)
         do 10 n = 1, 4
            p(n,ptr) = p(n,ptr) * x
 10      continue

         return
      endif

      beta0 = sqrt (1.d0 - 4.d0 / big)
      tmp1 = 1.d0 + beta0
      ttmp = 4.d0 / (tmp1**2 * big)

      if (dbgmas) then
         etamin = ttmp
         tmp2 = 1.d0 / beta0
      else
c (The pseudo-massless case is formally recovered by letting beta0 -> 1):
         etamin = 0
         tmp1 = 2
         tmp2 = 1
      endif

      do 30 n = 1, nphot

 40      continue

c Generate energy fraction x for each photon: (1+(1-x)**2)/x
         x = exp (ubrgen (0) * logeps)
         wgt = 0.5d0 * (1.0d0 + (1.d0 - x)**2)
         if (ubrgen (0) .ge. wgt) goto 40

c Transversal momenta: generate eta = (1 - beta0*cos(theta))/(1 + beta0)
         eta = ttmp ** ubrgen (0)

         if (dbgmas) then
c Weight to generate correct distribution including mass-effect factor:
c (This weight is always 1 in the pseudo-massless case)
            mef = 1 - (1.d0 - x) * etamin / (wgt * eta)
            if (ubrgen (0) .ge. mef) goto 40
         endif

         tphot(n) = eta
         zphot(n) = x
 30   continue


c Now we must sort virtualities in ascending order.
c We do it by straight insertion, as the number of photons is small.
      do 45 j = 2, nphot
         ztmp = zphot(j)
         ttmp = tphot(j)
         do 46 i = j-1, 1,-1
            if (zphot(i)*tphot(i) .le. ztmp*ttmp) goto 48
            zphot(i+1) = zphot(i)
            tphot(i+1) = tphot(i)
 46      continue
         i=0
 48      continue
         zphot(i+1) = ztmp
         tphot(i+1) = ttmp
 45   continue

c Any finite-mass effects of the electron are taken into account only
c in the photon angular distribution, but not (yet) in the momenta.

c Generating the photons

      pgam(5) = 0
      do 55 n = 1, nphot
         phi = 2.d0 * PI * ubrgen (0)
         pgam(4) = sqrt (p(1,ptr)**2 + p(2,ptr)**2 + p(3,ptr)**2)
     &           * zphot(n)
         eta = tphot(n)
         if (dbgcol) then
c Strict collinear approximation (for debugging purposes only)
            cth = 1
            sth = 0
         else
            cth = tmp2 * (1.0d0 - tmp1 * eta)
            sth = tmp1 * tmp2 * sqrt ((1.0d0 - eta) * (eta-etamin))
         endif
         kt = pgam(4) * sth
         pgam(1) = cos(phi) * kt
         pgam(2) = sin(phi) * kt
         pgam(3) = cth * sign(pgam(4), p(3,ptr))

c Store the photon
         call ubeent (1, PDGGAM, ptr, pgam)

c Update the electron, using strict four-momentum conservation

         p(1,ptr) = p(1,ptr) - pgam(1)
         p(2,ptr) = p(2,ptr) - pgam(2)
         p(3,ptr) = p(3,ptr) - pgam(3)
         p(4,ptr) = p(4,ptr) - pgam(4)

 55   continue

      end
CDECK  ID>, UBBFIN. 
*CMZ :  2.01/08 29/11/95  16.03.08  by  Harald Anlauf
*-- Author :    Harald Anlauf

      subroutine ubbfin (scale, ptr1, ptr2, beta)

c Generate final-state QED radiation from an outgoing
c fermion-antifermion system.
c
c We now use a "global" algorithm (a modified time-like parton shower)
c which is inspired by an exact algorithm to generate 3-jet events
c from a 2-jet decay of a vector boson (originally in QCD).
c
c This algorithm has the following features:
c 1) Exact angular distribution for corrections due to single photon
c    radiation for V -> f f-bar.
c 2) Full many-body phase space is generated correctly in the relevant
c    kinematic region.
c 3) Interference between emission from the fermion line and from the
c    antifermion line is fully included also for multi-photon events,
c but:
c    the matrix element is assumed to factorize completely as suggested
c    by the process: V -> (f f-bar)* -> f f-bar gamma
c
c Stores the generated photons directly in /ubcevt/.

      implicit none
      double precision scale
      integer ptr1, ptr2
      double precision beta(4)

      integer NMXINT
      parameter (NMXINT = 100)
      double precision p
      common /ubcevt/  p(5,NMXINT)

      double precision ipele, ippos, opele, oppos, savp1, savp2, savk
      common /ubcevt/  ipele(4), ippos(4), opele(4), oppos(4)
      common /ubcevt/  savp1(4), savp2(4), savk(4)
c Quarks
      integer PDGDWN, PDGUP, PDGSTR, PDGCHM, PDGBOT, PDGTOP
      parameter (PDGDWN =    1, PDGUP  =    2, PDGSTR =    3,
     $           PDGCHM =    4, PDGBOT =    5, PDGTOP =    6)
c Leptons
      integer PDGELE, PDGNUE, PDGMU, PDGNUM, PDGTAU, PDGNUT
      parameter (PDGELE =   11, PDGNUE =   12, PDGMU =    13,
     $           PDGNUM =   14, PDGTAU =   15, PDGNUT =   16)
c Bosons
      integer PDGGLU, PDGGLV, PDGGAM, PDGZ0, PDGWPL, PDGH0
      parameter (PDGGLU =   21, PDGGLV =    9, PDGGAM =   22,
     $           PDGZ0  =   23, PDGWPL =   24, PDGH0  =   25)
c Mesons
      integer PDGPIP, PDGPI0, PDGETA, PDGKPL, PDGK0, PDGK0S, PDGK0L
      parameter (PDGPIP =  211, PDGPI0 =  111, PDGETA =  221,
     $           PDGKPL =  321, PDGK0  =  311, PDGK0S =  310,
     $           PDGK0L =  130)
c Baryons
      integer PDGPRO, PDGNEU
      parameter (PDGPRO = 2212, PDGNEU = 2112)


      double precision PI
      parameter (PI = 3.141592653589793238d0)
      double precision ahpla
      common /ubpcom/  ahpla
      double precision alpha
      common /ubpcom/  alpha
      double precision mass1e
      common /ubpcom/  mass1e
      double precision mass2e
      common /ubpcom/  mass2e
      double precision mass1z
      common /ubpcom/  mass1z
      double precision mass2z
      common /ubpcom/  mass2z
      double precision mass1t
      common /ubpcom/  mass1t
      double precision mass1h
      common /ubpcom/  mass1h
      double precision mass1w
      common /ubpcom/  mass1w
      double precision gamm1z
      common /ubpcom/  gamm1z
      double precision gamm2z
      common /ubpcom/  gamm2z
      double precision sin2w
      common /ubpcom/  sin2w
      double precision alphas
      common /ubpcom/  alphas
      double precision ebeam
      common /ubpcom/  ebeam
      double precision epol
      common /ubpcom/  epol
      double precision ppol
      common /ubpcom/  ppol
      double precision ctsmin
      common /ubpcom/  ctsmin
      double precision ctsmax
      common /ubpcom/  ctsmax
      double precision ecut
      common /ubpcom/  ecut
      double precision acocut
      common /ubpcom/  acocut
      double precision evisct
      common /ubpcom/  evisct
      integer          nevent, pad022
      common /ubpcom/  nevent, pad022
      integer          rseed , pad023
      common /ubpcom/  rseed , pad023
      logical          tchann, pad024
      common /ubpcom/  tchann, pad024
      logical          qedvtx, pad025
      common /ubpcom/  qedvtx, pad025
      logical          qedbox, pad026
      common /ubpcom/  qedbox, pad026
      logical          weak  , pad027
      common /ubpcom/  weak  , pad027
      logical          boxes , pad028
      common /ubpcom/  boxes , pad028
      integer          isrtyp, pad029
      common /ubpcom/  isrtyp, pad029
      integer          fsrtyp, pad030
      common /ubpcom/  fsrtyp, pad030
      double precision epsiln
      common /ubpcom/  epsiln
      double precision taumin
      common /ubpcom/  taumin
      double precision taumax
      common /ubpcom/  taumax
      double precision emin
      common /ubpcom/  emin
      double precision eps2
      common /ubpcom/  eps2
      double precision qsq
      common /ubpcom/  qsq
      integer          bstyle, pad037
      common /ubpcom/  bstyle, pad037
      logical          nonlog, pad038
      common /ubpcom/  nonlog, pad038
      integer          stdin , pad039
      common /ubpcom/  stdin , pad039
      integer          stdout, pad040
      common /ubpcom/  stdout, pad040
      integer          stderr, pad041
      common /ubpcom/  stderr, pad041
      integer          errcnt, pad042
      common /ubpcom/  errcnt, pad042
      integer          errmax, pad043
      common /ubpcom/  errmax, pad043
      integer          verbos, pad044
      common /ubpcom/  verbos, pad044
      integer          status, pad045
      common /ubpcom/  status, pad045
      integer          hepdat, pad046
      common /ubpcom/  hepdat, pad046
      integer          heprev, pad047
      common /ubpcom/  heprev, pad047
      integer          runid , pad048
      common /ubpcom/  runid , pad048
      logical          dbgcol, pad049
      common /ubpcom/  dbgcol, pad049
      logical          dbghep, pad050
      common /ubpcom/  dbghep, pad050
      logical          dbgini, pad051
      common /ubpcom/  dbgini, pad051
      logical          dbgmas, pad052
      common /ubpcom/  dbgmas, pad052
      logical          dbgmup, pad053
      common /ubpcom/  dbgmup, pad053
      double precision cacomx, logeps, lneps2, nlgsca
      common /ubcmsc/ cacomx, logeps, lneps2, nlgsca

      integer MAXPHO
      parameter (MAXPHO=25)

      double precision ubrgen
      external ubrgen

      double precision biglog, nbar, p0, r, eta, eta1, etamax,
     &                 loginv, tmp, wgt, z, onemz
      double precision eo1(3), eo2(3), eo3(3)
      double precision phi, cosphi, sinphi, costh, sinth

      double precision x1, x2, x3, taum, tmpxi1, tmpxi2
      double precision boostv(4), pf1(4), pf2(4), pgam(4), pcm(4)
      double precision xi1(MAXPHO), xi2(MAXPHO)
      double precision boost(4,0:MAXPHO-1), k(5,MAXPHO), p1(4,0:MAXPHO),
     &     p2(4,0:MAXPHO)

      integer n, nphot, i, j, leg, n1, mother(MAXPHO)

      double precision EE, MASS2M
      parameter (EE = 2.71828182845904523536d0)
      parameter (MASS2M = 0.105658389d0**2)


      if (dbgmup) then
c Final state corrections for muon pair production: (debugging only)
         taum = MASS2M * EE / scale
      else
         taum = mass2e * EE / scale
      endif

c Calculate mean # of photons for "radiation from final-state fermions":

c biglog = log(scale/m_e^2) - 1:
      biglog = - log (taum)
      nbar = alpha / PI * (biglog * (- 2.0d0 * logeps - 1.5d0) - 0.5d0)

c Standard Poisson algorithm

      p0 = exp (-nbar)
      r = ubrgen (0)
      nphot = 0

 10   if (r .gt. p0 .and. nphot .lt. MAXPHO) then
         r = r * ubrgen (0)
         nphot = nphot + 1
         goto 10
      endif

      if (nphot .eq. 0) then
c (Soft tail not implemented, so you better keep epsiln small... ;-)
         return
      endif

c Precompute some common subexpressions:
      loginv = 1 / biglog
      etamax = 1 - taum
      tmp = taum / etamax

      do 20 n = 1, nphot

 30      continue

c Generate energy fraction z (later: x3) for each photon:
         z = exp (ubrgen (0) * logeps)
         onemz = 1 - z
         wgt = 0.5d0 * (1 + onemz**2 - z**2 * loginv)
         if (ubrgen (0) .ge. wgt) goto 30

 40      continue

c The angular-like variable eta, with  taum < eta < 1-taum:
         eta = tmp ** ubrgen (0) * etamax
         wgt = 1 - z**2 * eta / (1 + onemz**2)
         if (ubrgen (0) .ge. wgt) goto 40

         eta1 = 1 - eta
         if (eta1 .lt. taum) goto 40

c We need xi_1 = z * min(eta, 1-eta),  xi_2 = z * max(eta, 1-eta)
         if (eta .lt. eta1) then
            xi1(n) = z * eta
            xi2(n) = z * eta1
         else
            xi1(n) = z * eta1
            xi2(n) = z * eta
         endif

 20   continue

c Now we must sort the pseudo-virtualities (xi1) in descending order.
c We do this by straight insertion, as the number of photons is small.
c (Taken and modified from Numerical Recipes).
      do 50 j = 2, nphot
         tmpxi1 = xi1(j)
         tmpxi2 = xi2(j)
         do 51 i = j-1, 1,-1
            if (xi1(i) .ge. tmpxi1) goto 52
            xi1(i+1) = xi1(i)
            xi2(i+1) = xi2(i)
 51      continue
         i=0
 52      continue
         xi1(i+1) = tmpxi1
         xi2(i+1) = tmpxi2
 50   continue


c The new "final-state photon shower algorithm":

      do 100 n = 1, nphot

c Boost to the effective c.m.s. of the current fermion-antifermion pair:

         if (n .eq. 1) then
c In the case of the first radiation we don't have to do much work,
c since we can get this information from subroutine UBGEN:
            do i = 1,4
               pf1(i) = opele(i)
               pf2(i) = oppos(i)
               boost(i,0) = beta(i)
            enddo
         else
            n1 = n-1
            do i = 1,4
               pcm(i) = p1(i,n1) + p2(i,n1)
            enddo
            tmp = pcm(4)**2 - (pcm(1)**2 + pcm(2)**2 + pcm(3)**2)
            tmp = 1 / sqrt (tmp)
            do i = 1,3
               boostv(i) = tmp * pcm(i)
               boost(i,n1) = - boostv(i)
            enddo
            boostv(4) = tmp * pcm(4)
            boost(4,n1) = boostv(4)
            call ububoo (boostv, p1(1,n1), pf1)
c we're in the c.m.s., so ...
            pf2(4) = pf1(4)
            do i = 1,3
               pf2(i) = -pf1(i)
            enddo
         endif

         tmpxi1 = xi1(n)
         tmpxi2 = xi2(n)
c Determine the photon's energy fraction:
         x3 = tmpxi1 + tmpxi2
c The energy fractions of the fermions:
         x1 = 1 - tmpxi1
         x2 = 1 - tmpxi2
c (Note that we have not yet really assigned the labels 1 and 2 to the
c fermions because we still have to symmetrize.)

c Determine which "leg has radiated":
         if (ubrgen (0) * (x1**2 + x2**2) .gt. x1**2) then
c The radiating leg is always #2, so swap x_1, x_2 if it should be #1:
            tmp = x1
            x1 = x2
            x2 = tmp
         endif
c Determine angles of photon w.r.t. the "non-radiating" leg:
         tmp = 2 / (x1*x3)
         sinth = tmp * sqrt (tmpxi1 * tmpxi2 * (1-x3))
         costh = 1 - (1 - x2) * tmp

         if (dbgcol) then
c Strict collinear approximation (for debugging purposes only)
            sinth = 0
            costh = -1
         endif

c Now it's really time to decide the labelling of the legs!
         if (ubrgen (0) .lt. 0.5d0) then
            leg = 1
            mother(n) = ptr2
            call ubuort (pf1, eo1, eo2, eo3)
         else
            leg = 2
            mother(n) = ptr1
            call ubuort (pf2, eo1, eo2, eo3)
         endif

         phi = 2 * PI * ubrgen (0)
         cosphi = cos (phi)
         sinphi = sin (phi)

c The photon 4-momentum:
         pgam(4) = pf1(4) * x3
         do 150 i = 1, 3
            pgam(i) = (costh *  eo3(i) +
     &                 sinth * (eo1(i) * cosphi + eo2(i) * sinphi))
     &              * pgam(4)
 150     continue

         if (leg .eq. 1) then
c Scale p1:
            do i = 1,4
               p1(i,n) = x1 * pf1(i)
            enddo
c p2 is fixed by momentum conservation:
            p2(4,n) = x2 * pf2(4)
            do i = 1,3
               p2(i,n) = - (p1(i,n) + pgam(i))
            enddo
         else
c Don't get confused by the mapping of label <-> fermion!
c (x1 is the scale-factor of the non-radiating particle!)
c Scale p2:
            do i = 1,4
               p2(i,n) = x1 * pf2(i)
            enddo
c Now p1 is fixed by momentum conservation:
            p1(4,n) = x2 * pf1(4)
            do i = 1,3
               p1(i,n) = - (p2(i,n) + pgam(i))
            enddo
         endif

         do i = 1,4
            k(i,n) = pgam(i)
         enddo

 100  continue

c Keep momenta (in the c.m.s.) after first branching for the calculation
c of the "lon-logarithmic corrections" (processed elsewhere):
      do i = 1,4
         savp1(i) = p1(i,1)
         savp2(i) = p2(i,1)
         savk(i)  = k(i,1)
      enddo

c Now that we have generated the full sequence of emissions,
c we have to go back to the lab system:

      do n = nphot, 1, -1
         n1 = n-1

c Revert boost # n-1:
         call ububoo (boost(1,n1), p1(1,n), p1(1,n1))
         call ububoo (boost(1,n1), p2(1,n), p2(1,n1))

c At this stage we have to boost the photons numbered  n ... nphot:
         do j = n, nphot
            call ububoo (boost(1,n1), k(1,j), pgam)
            do i = 1,4
               k(i,j) = pgam(i)
            enddo
         enddo
      enddo

c Finally we are in the lab system.  Store the photons:

      do n = 1, nphot
         k(5,n) = 0
         call ubeent (1, PDGGAM, mother(n), k(1,n))
      enddo

c And return the fermions:
      do i = 1,4
         p(i,ptr1) = p1(i,0)
         p(i,ptr2) = p2(i,0)
      enddo

      end
CDECK  ID>, UTILS.  
*CMZ :- 1.02/99-14/07/92  20.31.26  by  Thorsten Ohl <ohl@ips104>
CDECK  ID>, BLANKDEK.   
*CMZ :  2.01/06 30/03/95  22.16.23  by  Harald Anlauf
*CMZ :  1.02/99 03/05/93  18.02.13  by  Unknown
*-- Author :    Thorsten Ohl <ohl@ips104>

C Several general purpose utility routines are collected in this patch.
CDECK  ID>, UBRGEN. 
*CMZ :  1.00/14 21/04/92  14.39.47  by  Thorsten Ohl
*CMZ :  1.00/13 14/04/92  18.07.21  by  Harald Anlauf
*CMZ :  0.05/00 21/05/91  19.50.15  by  Thorsten Ohl
      double precision function ubrgen (iseed)
      implicit none
      integer iseed

C   version interfaced to RNDM in order to avoid dupliaction 
C   of RANMAR
      real rndm
      ubrgen = dble(rndm(iseed))
      return
      end
CDECK  ID>, UBRTST. 
*CMZ : 00.99/03 01/09/91  15.08.53  by  Thorsten Ohl
*-- Author :    Thorsten Ohl   09/04/91

      subroutine ubrtst()
      implicit none

      double precision ubrgen

      integer IJMAX, KLMAX, IJDFLT, KLDFLT
      parameter (IJMAX  = 31328, KLMAX  = 30081,
     $           IJDFLT =  1802, KLDFLT =  9373)

      integer i
      double precision r, goal(6)

      character*55 msg

      data goal/ 6533892.0, 14220222.0,  7275067.0,
     $           6172232.0,  8354498.0, 10633180.0/

      call ubumsg ('ubrtst', -1, 'Testing the random number generator')

c     force initialization with the right seed:
      i = IJDFLT * (KLMAX + 1) + KLDFLT
      write (msg, 9000) i
 9000 format ('set rseed ',I20)
      r = ubrgen (-i)

c     warm up:
      do 10 i = 2, 20000
         r = ubrgen(0)
   10 continue

c     compare:
      do 20 i= 20001, 20006
         r = ubrgen(0)*(2**24)
         if (r.eq.goal(i-20000)) then
            write (msg, 10000) i, r
10000       format ('test #',I5,' passed (',F10.1,')')
            call ubumsg ('ubrtst', -1, msg)
         else
            write (msg, 10100) i, r, goal(i-20000)
10100       format ('test #',I5,' failed (',F10.1,' != ',F10.1,')')
            call ubumsg ('ubrtst', 10, msg)
         endif
   20 continue

      end

CDECK  ID>, UBUBOO. 
*CMZ :- 1.02/99-27/08/92  16.10.39  by  Dirk Uwe Sauer and Harald Anlauf
*-- Author :    Harald Anlauf

      subroutine ububoo (v, xinit, xfinal)
      implicit none
      double precision v(4), xinit(*), xfinal(*)

c Boost four-vector XINIT by four-velocity V, result is XFINAL

      integer i
      double precision scprod, tmp

      scprod = 0.d0
      do 10 i = 1, 3
         scprod = scprod + v(i) * xinit(i)
 10   continue

c Timelike component
      xfinal(4) = v(4) * xinit(4) - scprod

c Spatial components
      tmp = xinit(4) - scprod / (1.d0 + v(4))

      do 20 i = 1, 3
         xfinal(i) = xinit(i) - tmp * v(i)
 20   continue

      end


CDECK  ID>, UBUPRO. 
*CMZ :  2.01/08 30/10/95  18.13.29  by  Harald Anlauf
*CMZ :  2.01/01 30/01/95  13.50.26  by  Harald Anlauf
*-- Author :    Harald Anlauf   30/01/95

c UBUPRO - Project (massless) 4-momenta p1,p2 onto mass shell in their
c          center of mass system

      subroutine ubupro (p1, p2)
      implicit none
      double precision p1(*), p2(*)

      double precision pcms(4), beta(4), ecms, p1cm(4), p2cm(4), tmp
      integer i

      do 10 i = 1,4
         pcms(i) = p1(i) + p2(i)
 10   continue

      ecms = sqrt (pcms(4)**2 - pcms(1)**2 - pcms(2)**2 - pcms(3)**2)

c Boost into center of mass system:
      tmp = 1 / ecms
      do 20 i = 1,4
         beta(i) = pcms(i) * tmp
 20   continue

      call ububoo (beta, p1, p1cm)

c We simply assume massless particles:
      p1cm(4) = 0.5d0 * ecms
      p2cm(4) = p1cm(4)

c Rescale spatial components in c.m.s.:
      tmp = p1cm(4) / sqrt (p1cm(1)**2 + p1cm(2)**2 + p1cm(3)**2)
      do 30 i = 1,3
         p1cm(i) = p1cm(i) * tmp
         p2cm(i) = - p1cm(i)
 30   continue

c Revert boost:
      do 40 i = 1,3
         beta(i) = - beta(i)
 40   continue

      call ububoo (beta, p1cm, p1)
      call ububoo (beta, p2cm, p2)
      end
CDECK  ID>, UBUMIN. 
*CMZ :  2.01/08 29/11/95  14.57.41  by  Harald Anlauf
*CMZ :- 1.02/99- 6/07/92  22.09.38  by  Thorsten Ohl <ohl@ips105>
*-- Author :    Thorsten Ohl <ohl@ips105>

c Find the minimum of F in the interval (AX, CX), using a golden
c section search algorithm.

c Adapted from the routine 'GOLDEN' from 'NUMERICAL RECIPES'

      double precision function ubumin (ax, bx, cx, f, tol, xmin)
      implicit none
      double precision ax, bx, cx, f, tol, xmin
      external f

      double precision R, C, FUDGE
      parameter (R = 0.61803399d0,
     &           C = 1 - r,
     &           FUDGE = 1.d-8)

      double precision x0, x1, x2, x3
      double precision f0, f1, f2, f3

      x0 = ax
      x3 = cx

      if (abs (cx - bx).GT.abs (bx - ax)) then
         x1 = bx
         x2 = bx + C * (cx - bx)
      else
         x2 = bx
         x1 = bx - C * (bx - ax)
      endif

      f1 = f (x1)
      f2 = f (x2)
c Here we deviate from NR to avoid underflow on
c improper usage
c 1    IF(ABS(X3-X0).GT.TOL*(ABS(X1)+ABS(X2)))THEN
c (This is a while (...) loop)
 1    if (abs (x3 - x0) .gt. tol * (FUDGE + abs (x1) + abs (x2))) then
         if (f2.lt.f1) then
            x0 = x1
            x1 = x2
            x2 = R * x1 + C * x3
            f0 = f1
            f1 = f2
            f2 = f (x2)
         else
            x3 = x2
            x2 = x1
            x1 = R * x2 + C * x0
            f3 = f2
            f2 = f1
            f1 = f (x1)
         endif
         goto 1
      endif

      if (f1.lt.f2) then
         ubumin = f1
         xmin = x1
      else
         ubumin = f2
         xmin = x2
      endif

      end
CDECK  ID>, UBUMSG. 
*CMZ :  2.00/00 31/08/93  15.04.32  by  Harald Anlauf
*CMZ :  1.00/11 19/03/92  19.06.08  by  Thorsten Ohl
*CMZ :  0.05/01 22/05/91  17.36.06  by  Thorsten Ohl
*-- Author :    Thorsten Ohl   22/05/91

c Write the message MSG to the STDERR stream.

      subroutine ubumsg (origin, level, msg)
      implicit none
      character*(*) origin, msg
      integer level

      double precision ahpla
      common /ubpcom/  ahpla
      double precision alpha
      common /ubpcom/  alpha
      double precision mass1e
      common /ubpcom/  mass1e
      double precision mass2e
      common /ubpcom/  mass2e
      double precision mass1z
      common /ubpcom/  mass1z
      double precision mass2z
      common /ubpcom/  mass2z
      double precision mass1t
      common /ubpcom/  mass1t
      double precision mass1h
      common /ubpcom/  mass1h
      double precision mass1w
      common /ubpcom/  mass1w
      double precision gamm1z
      common /ubpcom/  gamm1z
      double precision gamm2z
      common /ubpcom/  gamm2z
      double precision sin2w
      common /ubpcom/  sin2w
      double precision alphas
      common /ubpcom/  alphas
      double precision ebeam
      common /ubpcom/  ebeam
      double precision epol
      common /ubpcom/  epol
      double precision ppol
      common /ubpcom/  ppol
      double precision ctsmin
      common /ubpcom/  ctsmin
      double precision ctsmax
      common /ubpcom/  ctsmax
      double precision ecut
      common /ubpcom/  ecut
      double precision acocut
      common /ubpcom/  acocut
      double precision evisct
      common /ubpcom/  evisct
      integer          nevent, pad022
      common /ubpcom/  nevent, pad022
      integer          rseed , pad023
      common /ubpcom/  rseed , pad023
      logical          tchann, pad024
      common /ubpcom/  tchann, pad024
      logical          qedvtx, pad025
      common /ubpcom/  qedvtx, pad025
      logical          qedbox, pad026
      common /ubpcom/  qedbox, pad026
      logical          weak  , pad027
      common /ubpcom/  weak  , pad027
      logical          boxes , pad028
      common /ubpcom/  boxes , pad028
      integer          isrtyp, pad029
      common /ubpcom/  isrtyp, pad029
      integer          fsrtyp, pad030
      common /ubpcom/  fsrtyp, pad030
      double precision epsiln
      common /ubpcom/  epsiln
      double precision taumin
      common /ubpcom/  taumin
      double precision taumax
      common /ubpcom/  taumax
      double precision emin
      common /ubpcom/  emin
      double precision eps2
      common /ubpcom/  eps2
      double precision qsq
      common /ubpcom/  qsq
      integer          bstyle, pad037
      common /ubpcom/  bstyle, pad037
      logical          nonlog, pad038
      common /ubpcom/  nonlog, pad038
      integer          stdin , pad039
      common /ubpcom/  stdin , pad039
      integer          stdout, pad040
      common /ubpcom/  stdout, pad040
      integer          stderr, pad041
      common /ubpcom/  stderr, pad041
      integer          errcnt, pad042
      common /ubpcom/  errcnt, pad042
      integer          errmax, pad043
      common /ubpcom/  errmax, pad043
      integer          verbos, pad044
      common /ubpcom/  verbos, pad044
      integer          status, pad045
      common /ubpcom/  status, pad045
      integer          hepdat, pad046
      common /ubpcom/  hepdat, pad046
      integer          heprev, pad047
      common /ubpcom/  heprev, pad047
      integer          runid , pad048
      common /ubpcom/  runid , pad048
      logical          dbgcol, pad049
      common /ubpcom/  dbgcol, pad049
      logical          dbghep, pad050
      common /ubpcom/  dbghep, pad050
      logical          dbgini, pad051
      common /ubpcom/  dbgini, pad051
      logical          dbgmas, pad052
      common /ubpcom/  dbgmas, pad052
      logical          dbgmup, pad053
      common /ubpcom/  dbgmup, pad053

      character*55 buf
      character*8 lstr

      if (level.lt.0) then
         lstr = 'message:'
      elseif (level.eq.0) then
         lstr = 'trace:'
      elseif (level.lt.10) then
         lstr = 'warning:'
      elseif (level.lt.100) then
         lstr = 'error:'
      else
         lstr = 'fatal:'
      endif

c Left justification.
      buf = msg

      write (stderr, 1000) origin, lstr, buf
 1000 format (
     $        A6,':',1X,A8,1X,A55)

      end
CDECK  ID>, UBUORT. 
*CMZ :  2.00/00 31/08/93  15.05.38  by  Harald Anlauf
*CMZ :- 1.02/99- Tue Aug 25 14:18:23 1992  by  Dirk Uwe Sauer & Angelika Himmler
*-- Author :    Dirk Uwe Sauer

      subroutine ubuort (vector, e1, e2, e3)
      implicit none
      double precision vector(4), e1(3), e2(3), e3(3)

c This routine contructs an orthogonal system of 3 vectors
c with the z-direction specified by the spacelike part of VECTOR.
c Results are the 3 vectors E1, E2, E3 !

      double precision tmp, prohel, pro2
      integer i

      tmp = sqrt (vector(1)**2 + vector(2)**2 + vector(3)**2)

      do 10 i = 1, 3
         e3(i) = vector(i) / tmp
 10   continue

c Searching for a starting - vector
      if (e3(3) .ne. 0.d0) then
         e2(3) = 0d0
         e2(1) = 1d0
         e2(2) = 0d0
      else
         e2(3) = 1d0
         e2(2) = 0d0
         e2(1) = 0d0
      endif

      prohel = 0.d0
      do 20 i =1, 3
         prohel = prohel + e2(i)*e3(i)
 20   continue

c Construct the second vector
      pro2 = 0.d0
      do 30 i = 1, 3
         e2(i) = e2(i) - prohel*e3(i)
         pro2 = pro2 + e2(i)**2
 30   continue

      do 40 i = 1, 3
         e2(i) = e2(i) / sqrt(pro2)
 40   continue

c Construct the third vector
      e1(1) = e2(2)*e3(3) - e2(3)*e3(2)
      e1(2) = e2(3)*e3(1) - e2(1)*e3(3)
      e1(3) = e2(1)*e3(2) - e2(2)*e3(1)

      end
CDECK  ID>, UBEENI. 
*CMZ :  1.99/05 01/06/93  15.08.53  by  Harald Anlauf & Helge Meinhard
*-- Author :    Thorsten Ohl   09/04/91

c Enter the identification of the Monte Carlo and the Run

      subroutine ubeeni (mcid, mcrev, mcdat, runid, rundat, runtim)
      implicit none
      integer mcid, mcrev, mcdat, runid, rundat, runtim

      integer NMXHEP
      parameter (NMXHEP = 2000)
      integer nevhep, nhep, isthep(NMXHEP), idhep(NMXHEP),
     $     jmohep(2,NMXHEP), jdahep(2,NMXHEP)
      real phep(5,NMXHEP), vhep(4,NMXHEP), shep(4,NMXHEP)
      common /hepevt/ nevhep, nhep, isthep, idhep, jmohep, jdahep,
     $     phep, vhep
      common /hepspn/ shep

      nevhep = -1
      idhep(1) = mcid
      jdahep(1,1) = mcrev
      jdahep(2,1) = mcdat
      isthep(1) = runid
      jmohep(1,1) = rundat
      jmohep(2,1) = runtim

      end
CDECK  ID>, UBEENS. 
*CMZ :  1.99/05 01/06/93  15.08.53  by  Harald Anlauf & Helge Meinhard
*-- Author :    Thorsten Ohl   09/04/91

c Create a summary record.
c Write the estimated error to PHEP(1,2) as HERACLES does.

      subroutine ubeens (totevt, xsect, error)
      implicit none
      integer totevt
      double precision xsect, error

      integer NMXHEP
      parameter (NMXHEP = 2000)
      integer nevhep, nhep, isthep(NMXHEP), idhep(NMXHEP),
     $     jmohep(2,NMXHEP), jdahep(2,NMXHEP)
      real phep(5,NMXHEP), vhep(4,NMXHEP), shep(4,NMXHEP)
      common /hepevt/ nevhep, nhep, isthep, idhep, jmohep, jdahep,
     $     phep, vhep
      common /hepspn/ shep

      nevhep = -2
      isthep(1) = totevt
      phep(1,1) = xsect
      phep(1,2) = error

      end
CDECK  ID>, UBEENT. 
*CMZ :  2.00/01 29/09/94  16.37.26  by  Harald Anlauf
*CMZ :  1.99/05 01/06/93  15.18.14  by  Harald Anlauf & Helge Meinhard
*CMZ :- 1.02/99-30/04/92  19.57.57  by  Thorsten Ohl <ohl@ips02>
*-- Author :    Thorsten Ohl <ohl@ips02>

      subroutine ubeent (status, id, mother, pp)
      implicit none
      integer status, id, mother
      double precision pp(5)

      integer NMXHEP
      parameter (NMXHEP = 2000)
      integer nevhep, nhep, isthep(NMXHEP), idhep(NMXHEP),
     $     jmohep(2,NMXHEP), jdahep(2,NMXHEP)
      real phep(5,NMXHEP), vhep(4,NMXHEP), shep(4,NMXHEP)
      common /hepevt/ nevhep, nhep, isthep, idhep, jmohep, jdahep,
     $     phep, vhep
      common /hepspn/ shep
      integer NMXINT
      parameter (NMXINT = 100)
      double precision p
      common /ubcevt/  p(5,NMXINT)

      double precision ipele, ippos, opele, oppos, savp1, savp2, savk
      common /ubcevt/  ipele(4), ippos(4), opele(4), oppos(4)
      common /ubcevt/  savp1(4), savp2(4), savk(4)

      if (nhep.ge.NMXHEP) then
         call ubumsg ('ubeent', 100, '/hepevt/ to small!')
         stop
      endif

      nhep = nhep + 1

      isthep(nhep) = status
      idhep(nhep) = id

      jmohep(1,nhep) = mother
      jmohep(2,nhep) = 0
      jdahep(1,nhep) = 0
      jdahep(2,nhep) = 0

      p(1,nhep) = pp(1)
      p(2,nhep) = pp(2)
      p(3,nhep) = pp(3)
      p(4,nhep) = pp(4)
      p(5,nhep) = pp(5)

      vhep(1,nhep) = 0
      vhep(2,nhep) = 0
      vhep(3,nhep) = 0
      vhep(4,nhep) = 0

      shep(1,nhep) = 0
      shep(2,nhep) = 0
      shep(3,nhep) = 0
      shep(4,nhep) = 1

      end


CDECK  ID>, UBENUL. 
*CMZ :  2.00/01 29/09/94  16.37.26  by  Harald Anlauf
*CMZ :  1.99/05 01/06/93  14.52.17  by  Harald Anlauf & Helge Meinhard
*CMZ :- 1.02/99-30/04/92  19.57.57  by  Thorsten Ohl <ohl@ips02>
*-- Author :    Thorsten Ohl <ohl@ips02>

      subroutine ubenul (ihep)
      implicit none
      integer ihep

      integer NMXHEP
      parameter (NMXHEP = 2000)
      integer nevhep, nhep, isthep(NMXHEP), idhep(NMXHEP),
     $     jmohep(2,NMXHEP), jdahep(2,NMXHEP)
      real phep(5,NMXHEP), vhep(4,NMXHEP), shep(4,NMXHEP)
      common /hepevt/ nevhep, nhep, isthep, idhep, jmohep, jdahep,
     $     phep, vhep
      common /hepspn/ shep
      integer NMXINT
      parameter (NMXINT = 100)
      double precision p
      common /ubcevt/  p(5,NMXINT)

      double precision ipele, ippos, opele, oppos, savp1, savp2, savk
      common /ubcevt/  ipele(4), ippos(4), opele(4), oppos(4)
      common /ubcevt/  savp1(4), savp2(4), savk(4)

      isthep(ihep) = 0
      idhep(ihep) = 0

      jmohep(1,ihep) = 0
      jmohep(2,ihep) = 0
      jdahep(1,ihep) = 0
      jdahep(2,ihep) = 0

      p(1,ihep) = 0
      p(2,ihep) = 0
      p(3,ihep) = 0
      p(4,ihep) = 0
      p(5,ihep) = 0

      vhep(1,ihep) = 0
      vhep(2,ihep) = 0
      vhep(3,ihep) = 0
      vhep(4,ihep) = 0

      shep(1,ihep) = 0
      shep(2,ihep) = 0
      shep(3,ihep) = 0
      shep(4,ihep) = 1

      end


CDECK  ID>, UBENEW. 
*CMZ :- 1.02/99-30/04/92  20.21.34  by  Thorsten Ohl <ohl@ips02>
*-- Author :    Thorsten Ohl <ohl@ips02>

      subroutine ubenew (n)
      implicit none
      integer n
      integer NMXHEP
      parameter (NMXHEP = 2000)
      integer nevhep, nhep, isthep(NMXHEP), idhep(NMXHEP),
     $     jmohep(2,NMXHEP), jdahep(2,NMXHEP)
      real phep(5,NMXHEP), vhep(4,NMXHEP), shep(4,NMXHEP)
      common /hepevt/ nevhep, nhep, isthep, idhep, jmohep, jdahep,
     $     phep, vhep
      common /hepspn/ shep
      nevhep = n
      nhep = 0
      end


CDECK  ID>, UBULWR. 
*CMZ : 00.99/05 06/09/91  17.36.44  by  Thorsten Ohl
*-- Author :    Thorsten Ohl   06/09/91

c portable (but VERY slow) tolower (): convert string
c to lower case.

      subroutine ubulwr (s)
      implicit none
      character*(*) s

      integer i, n
      character*26 upper, lower

      data upper /'ABCDEFGHIJKLMNOPQRSTUVWXYZ'/
      data lower /'abcdefghijklmnopqrstuvwxyz'/

      do 10 n = 1, len (s)
         i = index (upper, s(n:n))
         if (i.ne.0) s(n:n) = lower(i:i)
   10 continue

      end

CDECK  ID>, UBUTIM. 
*CMZ :  2.01/01 25/01/95  15.48.58  by  Harald Anlauf
*CMZ : 00.99/12 15/10/91  21.38.28  by  Thorsten Ohl
*-- Author :    Thorsten Ohl   15/10/91

c timing routine, return 0 if timeout, else the recommended
c number of events until a further invocation.

      integer function ubutim (n)
      implicit none
      integer n

c Having all the time of the world ...
      ubutim = 1000000


      end
CDECK  ID>, TRIE.   
*CMZ :  1.00/13 03/04/92  18.33.32  by  Thorsten Ohl
CDECK  ID>, BLANKDEK.   
*CMZ :  1.02/99 03/05/93  18.02.13  by  Unknown
*-- Author :

c The algorithm for the keyword search with abbreviations has
c beed taken from
c
c    John A. Dundas III: 'Dynamic Minimal-prefix Tries',
c    Software--Practice and Experience, 21 (1991), p. 1027
c
c The implementation is a translation of the C functions given
c in the cited article to Fortran 77.


CDECK  ID>, UBTNEW. 
*CMZ :  1.00/13 03/04/92  18.35.54  by  Thorsten Ohl
*-- Author :    Thorsten Ohl   03/04/92

c Insert a new node with value VALUE into the trie.  FRAG is some
c keyword fragment.  Return a pointer to the new node or 0 on
c failure.

      integer function ubtnew (frag, value)
      implicit none
      integer frag(*)
      integer value

c node structure
      integer NEXT, CHILD, VAL, STR, NODELN
      parameter (NEXT = 0, CHILD = 1, VAL = 2, STR = 3, NODELN = 4)
c storage for the trie
      integer TRIESZ
      parameter (TRIESZ = 1000)
      integer trie(TRIESZ)
      common /ubctri/ trie

      integer i, fragln
      integer ubtlen

c pointer to free space
      integer ptr
      integer PTR0
      parameter (PTR0 = NODELN + 1)
      data ptr /PTR0/

      fragln = ubtlen (frag)

c stupid malloc
      ubtnew = ptr
      ptr = ptr + NODELN + fragln
      if (ptr.gt.TRIESZ) then
        ubtnew = 0
        return
      endif

c insert the node
      trie(ubtnew+NEXT) = 0
      trie(ubtnew+CHILD) = 0
      trie(ubtnew+VAL) = value
c copy (including the terminating '\0')
      do 10 i = 0, fragln
        trie(ubtnew+STR+i) = frag(1+i)
   10 continue

      end

CDECK  ID>, UBTINS. 
*CMZ :  1.00/13 03/04/92  18.35.54  by  Thorsten Ohl
*-- Author :    Thorsten Ohl   03/04/92
c Install CHKEY and VALUE as keyword/value pair in the trie.
c Retun .TRUE. on success, .FALSE. on failure.

      logical function ubtins (chkey, value)
      implicit none
      character *(*) chkey
      integer value

c node structure
      integer NEXT, CHILD, VAL, STR, NODELN
      parameter (NEXT = 0, CHILD = 1, VAL = 2, STR = 3, NODELN = 4)
c storage for the trie
      integer TRIESZ
      parameter (TRIESZ = 1000)
      integer trie(TRIESZ)
      common /ubctri/ trie

      integer BUFLEN
      parameter (BUFLEN = 73)

      integer s, key(BUFLEN), keyp, node1, node2
      integer ubtnew

      if (len (chkey).ge.BUFLEN) then
        ubtins = .false.
        return
      endif
      call ubtc2a (key, chkey)
      keyp = 1

      if (value.eq.0 .or. value.eq.-1) then
        ubtins = .false.
        return
      endif

      node1 = trie(1+NEXT)
      if (node1.eq.0) then
c Step 1
        node2 = ubtnew (key(keyp), value)
        if (node2.eq.0) then
          ubtins = .false.
          return
        endif
        trie(1+NEXT) = node2
        ubtins = .true.
        return
      endif

   10 continue

   20   if (node1.ne.0) then
          if (trie(node1+STR).eq.key(keyp)) then
            s = node1 + STR
   30       if (trie(s).ne.0 .and. trie(s).eq.key(keyp)) then
              s = s + 1
              keyp = keyp + 1
            goto 30
            endif
            if (key(keyp).eq.0) then
              if (trie(s).eq.0) then
                if (trie(node1+VAL).ne.0) then
c Step 4
                  ubtins = .true.
                  return
                else
c Step 4a
                  trie(node1+VAL) = value
                  ubtins = .true.
                  return
                endif
              else
c Step 4b
                node2 = ubtnew (trie(s), trie(node1+VAL))
                if (node2.eq.0) then
                  ubtins = .false.
                  return
                endif
                trie(node2+CHILD) = trie(node1+CHILD)
                trie(node1+CHILD) = node2
                trie(node1+VAL) = value
                trie(s) = 0
                ubtins = .true.
                return
              endif
            endif
            if (trie(s).ne.0) then
c Step 3
              node2 = ubtnew (trie(s), trie(node1+VAL))
              if (node2.eq.0) then
                ubtins = .false.
                return
              endif
              trie(node2+CHILD) = trie(node1+CHILD)
              trie(node1+CHILD) = node2
              trie(node1+VAL) = 0
              trie(s) = 0
c Step 3a
              node1 = ubtnew (key(keyp), value)
              if (node1.eq.0) then
                ubtins = .false.
                return
              endif
              trie(node2+NEXT) = node1
              ubtins = .true.
              return
            endif
            if (trie(node1+CHILD).eq.0) then
c Step 5
              node2 = ubtnew (key(keyp), value)
              if (node2.eq.0) then
                ubtins = .false.
                return
              endif
              trie(node1+CHILD) = node2
              ubtins = .true.
              return
            else
              node1 = trie(node1+CHILD)
              goto 21
            endif
          endif

          node2 = node1
          node1 = trie(node1+NEXT)
        goto 20
        endif
   21   continue

        if (node1.eq.0) then
           node1 = ubtnew (key(keyp), value)
           if (node1.eq.0) then
             ubtins = .false.
             return
           endif
           trie(node2+NEXT) = node1
           ubtins = .true.
           return
        endif

      goto 10

      end

CDECK  ID>, UBTLUP. 
*CMZ :  1.00/13 03/04/92  18.35.54  by  Thorsten Ohl
*-- Author :    Thorsten Ohl   03/04/92
c Look up CHPAT as a possibly abbreviated keyword.  Return the
c stored value or 0 if not found or -1 if ambigious.

      integer function ubtlup (chpat)
      implicit none
      character*(*) chpat

c node structure
      integer NEXT, CHILD, VAL, STR, NODELN
      parameter (NEXT = 0, CHILD = 1, VAL = 2, STR = 3, NODELN = 4)
c storage for the trie
      integer TRIESZ
      parameter (TRIESZ = 1000)
      integer trie(TRIESZ)
      common /ubctri/ trie

      integer BUFLEN
      parameter (BUFLEN = 73)

      integer s, node, pat(BUFLEN), patp

      if (len (chpat).ge.BUFLEN) then
        ubtlup = 0
        return
      endif
      call ubtc2a (pat, chpat)
      patp = 1

c Step 1
      node = trie(1+NEXT)
      if (node.eq.0) then
         ubtlup = 0
         return
      endif

   10 continue

   20   continue
          if (trie(node+STR).eq.pat(patp)) then
            s = node + STR
c Step 2
   30       if (trie(s).ne.0 .and. trie(s).eq.pat(patp)) then
              s = s + 1
              patp = patp + 1
            goto 30
            endif
            if (pat(patp).eq.0) then
              if(trie(node+VAL).eq.0) then
c Step 3
                ubtlup = -1
                return
              else
c Step 3a
                ubtlup = trie(node+VAL)
                return
              endif
            endif
c Step 4
            if (trie(s).ne.0) then
              ubtlup = 0
              return
            endif
            node = trie(node+CHILD)
            if (node.eq.0) then
              ubtlup = 0
              return
            else
              goto 21
            endif
          endif
          node = trie(node+NEXT)
        if (node.ne.0) goto 20
   21   continue

c Step 1
        if (node.eq.0) then
          ubtlup = 0
          return
        endif
      goto 10

      end

CDECK  ID>, UBTC2A. 
*CMZ :  1.00/13 03/04/92  18.35.54  by  Thorsten Ohl
*-- Author :    Thorsten Ohl   03/04/92
      subroutine ubtc2a (s, c)
      implicit none

      integer s(*)
      character*(*) c

      integer i

      do 10 i = 1, len (c)
         if (c(i:i).eq.char (0) .or. c(i:i).eq.' ') goto 20
         s(i) = ichar (c(i:i))
 10   continue
 20   continue
      s(i) = 0

      end

CDECK  ID>, UBTLEN. 
*CMZ :  1.00/13 03/04/92  18.35.54  by  Thorsten Ohl
*-- Author :    Thorsten Ohl   03/04/92
      integer function ubtlen (s)
      implicit none

      integer s(*)

      ubtlen = 1
 10   if (s(ubtlen).ne.0) then
        ubtlen = ubtlen + 1
        goto 10
      endif
      ubtlen = ubtlen - 1

      end


CDECK  ID>, EWLIB.  
*CMZ :  1.99/00 06/05/93  17.46.27  by  Harald Anlauf & Helge Meinhard
CDECK  ID>, EWINIT. 
*CMZ :  2.01/08 29/11/95  14.26.44  by  Harald Anlauf
*CMZ :  2.01/03 17/03/95  13.52.21  by  Harald Anlauf
*CMZ :  2.01/00 10/01/95  14.25.41  by  Harald Anlauf
*CMZ :  2.00/01 29/09/94  18.14.12  by  Harald Anlauf
*CMZ :  1.99/00 11/05/93  11.13.17  by  Harald Anlauf & Helge Meinhard
*-- Author :
      SUBROUTINE EWINIT (XMZ, XMH, XMT, ALFQCD)
*     -----------------
* Initialization routine.
* The parameters are the masses of the Z, Higgs and top in GeV,
* as well as ALPHA_S(M_Z^2).  All variables are input.
      implicit none
      double precision XMZ, XMH, XMT, ALFQCD
      double precision ahpla
      common /ubpcom/  ahpla
      double precision alpha
      common /ubpcom/  alpha
      double precision mass1e
      common /ubpcom/  mass1e
      double precision mass2e
      common /ubpcom/  mass2e
      double precision mass1z
      common /ubpcom/  mass1z
      double precision mass2z
      common /ubpcom/  mass2z
      double precision mass1t
      common /ubpcom/  mass1t
      double precision mass1h
      common /ubpcom/  mass1h
      double precision mass1w
      common /ubpcom/  mass1w
      double precision gamm1z
      common /ubpcom/  gamm1z
      double precision gamm2z
      common /ubpcom/  gamm2z
      double precision sin2w
      common /ubpcom/  sin2w
      double precision alphas
      common /ubpcom/  alphas
      double precision ebeam
      common /ubpcom/  ebeam
      double precision epol
      common /ubpcom/  epol
      double precision ppol
      common /ubpcom/  ppol
      double precision ctsmin
      common /ubpcom/  ctsmin
      double precision ctsmax
      common /ubpcom/  ctsmax
      double precision ecut
      common /ubpcom/  ecut
      double precision acocut
      common /ubpcom/  acocut
      double precision evisct
      common /ubpcom/  evisct
      integer          nevent, pad022
      common /ubpcom/  nevent, pad022
      integer          rseed , pad023
      common /ubpcom/  rseed , pad023
      logical          tchann, pad024
      common /ubpcom/  tchann, pad024
      logical          qedvtx, pad025
      common /ubpcom/  qedvtx, pad025
      logical          qedbox, pad026
      common /ubpcom/  qedbox, pad026
      logical          weak  , pad027
      common /ubpcom/  weak  , pad027
      logical          boxes , pad028
      common /ubpcom/  boxes , pad028
      integer          isrtyp, pad029
      common /ubpcom/  isrtyp, pad029
      integer          fsrtyp, pad030
      common /ubpcom/  fsrtyp, pad030
      double precision epsiln
      common /ubpcom/  epsiln
      double precision taumin
      common /ubpcom/  taumin
      double precision taumax
      common /ubpcom/  taumax
      double precision emin
      common /ubpcom/  emin
      double precision eps2
      common /ubpcom/  eps2
      double precision qsq
      common /ubpcom/  qsq
      integer          bstyle, pad037
      common /ubpcom/  bstyle, pad037
      logical          nonlog, pad038
      common /ubpcom/  nonlog, pad038
      integer          stdin , pad039
      common /ubpcom/  stdin , pad039
      integer          stdout, pad040
      common /ubpcom/  stdout, pad040
      integer          stderr, pad041
      common /ubpcom/  stderr, pad041
      integer          errcnt, pad042
      common /ubpcom/  errcnt, pad042
      integer          errmax, pad043
      common /ubpcom/  errmax, pad043
      integer          verbos, pad044
      common /ubpcom/  verbos, pad044
      integer          status, pad045
      common /ubpcom/  status, pad045
      integer          hepdat, pad046
      common /ubpcom/  hepdat, pad046
      integer          heprev, pad047
      common /ubpcom/  heprev, pad047
      integer          runid , pad048
      common /ubpcom/  runid , pad048
      logical          dbgcol, pad049
      common /ubpcom/  dbgcol, pad049
      logical          dbghep, pad050
      common /ubpcom/  dbghep, pad050
      logical          dbgini, pad051
      common /ubpcom/  dbgini, pad051
      logical          dbgmas, pad052
      common /ubpcom/  dbgmas, pad052
      logical          dbgmup, pad053
      common /ubpcom/  dbgmup, pad053
      integer nrmass
      PARAMETER ( NRMASS = 9 )
      double precision RMASS, RMASS2, VF, AF, QF
      COMMON/ MASSES / RMASS(0:NRMASS), RMASS2(0:NRMASS),
     &          VF(0:NRMASS), AF(0:NRMASS), QF(0:NRMASS)
      double precision t3f, mq2mz
      common/ masses / t3f(0:NRMASS), mq2mz(0:NRMASS)
      COMPLEX*16 FZV,FZA,FGV,FGA
      COMMON/ FORMFA /FZV(0:NRMASS),FZA(0:NRMASS),
     +                FGV(0:NRMASS),FGA(0:NRMASS)
      COMPLEX*16 FZVMZ,FZAMZ,FGVMZ,FGAMZ
      COMMON/ FORMMZ /FZVMZ(0:NRMASS),FZAMZ(0:NRMASS),
     +                FGVMZ(0:NRMASS),FGAMZ(0:NRMASS)
      double precision RMZ,RMW,RMH,RMT, ZWID, SIN2TH, sw, cw, cw2, pizmz
      COMMON/ BOSONS / RMZ,RMW,RMH,RMT, ZWID, SIN2TH, sw, cw, cw2, pizmz
      double precision HBARC2, PI
      parameter (HBARC2 = 0.38937966d9, PI = 3.141592653589793238d0)
      double precision ALFA, ALFAS, FACQCD, FACQED, dens
      COMMON/ ADHOC  / ALFA, ALFAS, FACQCD, FACQED, dens(-1:1,-1:1)
      integer IWEAK, IFERM, ICHANN, IBOXES, IOUT
      COMMON/ OPTION / IWEAK, IFERM, ICHANN, IBOXES, IOUT
      double precision BIGPIZ, bigpz2, PHADPI, PILEPT, PWIDTH
      double precision SIGG, SIGGZ, IMSIGG, IMSGGZ, IMZ
      double precision RMWOLD, Z, ALFAZ, S2WEFF, S2WB, ALR, AFB
      double complex cmix
*      double precision IMSIGZ, IMZ2, KAPPAE, RHOE, DELTAR
      integer i, niter
      CHARACTER*13 NAMES(0:NRMASS+1)
      DATA NAMES /'     neutrino','     electron','         muon',
     +            '          tau','     up quark','   down quark',
     +            '  charm quark','strange quark',
     +            '    top quark',' bottom quark','      hadrons'/
      RMZ    = XMZ
      RMH    = XMH
      RMT    = XMT
      ALFAS  = ALFQCD
*
* A few options that can be set by the user. These determine what is
* calculated and what is not.
* IWEAK  : 0/1 switches weak (non-QED) corrections off/on.
      IWEAK  = 1
*
* IFERM : final state fermion label. IFERM=1 means Bhabha scattering,
*         for the other ones see the data statement. The choices
*         8 (top) and 10 (hadrons) are not allowed. Neutrino
*         production can be calculated in s channel only, for one
*         species only.
      IFERM  = 1
*
* ICHANN: enables calculating only certain channels:
*         =0 calculates s + t channels plus interference,
*         =1 calculates s channel only,
*         =2 calculates t channel only,
      ICHANN = 0
*
* IBOXES: 0/1 switches WW,ZZ boxes off/on
      IBOXES = 1
*
* IOUT  : the unit nr where the output goes to (6=screen).
      IOUT   = 6
*
* From here on: NOT user settable
*
* ALFA from Particle Data Group Publ. 1994.
      ALFA   = 1.d0 / 137.0359895D0
* Set up some starting values:
      SIN2TH = 0.2310D0
      RMW = RMZ*SQRT( 1D0 - SIN2TH )
      Z = RMZ**2
c The (old-fashioned) QCD correction factor:
      FACQCD = ALFAS/PI +  1.40923d0 * (ALFAS/PI)**2
     &                  - 12.76706d0 * (ALFAS/PI)**3
c alpha(M_Z), the relevant value for QED corrections to the Z width.
c CERN 95-03, p.10:
      ALFAZ  = 1.d0 / 128.87d0
      FACQED = 3*ALFAZ/(4*PI)
* Initialize masses and QCD correction factors. The order is important!
      call iniqcd (ALFAS, RMZ)
      CALL COUPLS (SIN2TH, RMT, RMZ)
      ALFAZ = ALFA / (1+PHADPI(Z)+PILEPT(Z))
      FACQED = 3*ALFAZ/(4*PI)
      call qcdcor ()
* Iterate to find the value for sin**2(theta_w) and M_W
* After this all couplings and renormalization constants are defined.
      NITER = 20
      DO 110 I = 1 , NITER
         RMWOLD = RMW
         CALL RNORM()
         CALL COUPLS (SIN2TH, RMT, RMZ)
         IF (ABS(RMWOLD-RMW)/RMW .LT. 1d-7) GOTO 130
  110 CONTINUE
      WRITE(*,120) NITER
  120 FORMAT(' The calculation of M_W does not converge in',I4,' steps',
     &     /,' We stop right here !')
      STOP
  130 CONTINUE

      if (dbgini) then
         write(iout,'(a)')' EWINIT: Initializing electroweak library...'
         WRITE(IOUT,150) (I,NAMES(I),RMASS(I),PWIDTH(I),I=0,NRMASS)
 150     FORMAT(/,' The properties of the fermions:',/,3X,
     +            ' label',7X,'name',4X,' mass (GeV)',
     +            '  partial width of the Z (GeV)',/,
     +            (' ',I6,1X,A13,1X,F12.7,8X,F12.7))
         WRITE(IOUT,'(1X,I6,1X,A13,21X,F12.7)')NRMASS+1,'      hadrons',
     +              PWIDTH(4)+PWIDTH(5)+PWIDTH(6)+PWIDTH(7)+PWIDTH(9)
      endif

      CALL FORMFS(Z,9)
      DO 160 I = 0 , NRMASS
         FZVMZ(I) = FZV(I)
         FZAMZ(I) = FZA(I)
         FGVMZ(I) = FGV(I)
         FGAMZ(I) = FGA(I)
  160 CONTINUE
*      ZWID = (IMSIGZ(Z)+IMZ2(Z))/RMZ/(1+BIGPIZ(Z)+bigpz2(Z))
      pizmz = BIGPIZ(Z) + bigpz2(Z)
      ZWID = IMZ(Z)/RMZ/(1+BIGPIZ(Z)+bigpz2(Z))
      if (dbgini) then
         WRITE(IOUT,170) RMZ,ZWID,RMW,SIN2TH,RMH
 170     FORMAT(/,' For the bosons we have (everything in GeV):',/,
     +            '  mass of the   Z   =',F10.4,
     +            '    total width of the Z = ',F10.7,/,
     +            '  mass of the   W   =',F10.4,
     +            '    <==> sin**2(theta-w) = ',F10.7,/,
     +            '  mass of the Higgs =',F10.4,/)

c Some effective parameters for the curious ones...
c Update: CERN 95-03, (175,179)
         cmix = dcmplx (SIGGZ(Z),IMSGGZ(Z))/dcmplx (Z+SIGG(Z),IMSIGG(Z))
         S2WEFF = (1 - ( VF(1) + dble (FZVMZ(1)+(QF(1)+FGVMZ(1))*cmix) )
     &                /( AF(1) + dble (FZAMZ(1)+       FGAMZ(1) *cmix) )
     &            ) / (4 * abs (QF(1)))
         S2WB   = (1 - ( VF(9) + dble (FZVMZ(9)+(QF(9)+FGVMZ(9))*cmix) )
     &                /( AF(9) + dble (FZAMZ(9)+       FGAMZ(9) *cmix) )
     &            ) / (4 * abs (QF(9)))
         ALR    = 2 * (1 - 4 * S2WEFF) / (1 + (1 - 4 * S2WEFF)**2)
         AFB    = 3.d0/4 * ALR**2
c (178), not used:
*         KAPPAE = S2WEFF / SIN2TH
*         RHOE = ( 1 + dble (FZAMZ(1)) / AF(1) )**2
*     &        * (1 - DELTAR()) / (1+BIGPIZ(Z)+bigpz2(Z))
         write(iout,190) S2WEFF, S2WB, ALR, AFB, 1/ALFAZ
 190     format(' Effective mixing angle: sin^2(theta^l_eff) = ',F9.5,/,
     &          ' effective mixing angle: sin^2(theta^b_eff) = ',F9.5,/,
     &          ' effective asymmetries : A_LR               = ',F9.5,/,
     &          '                         A^l_FB             = ',F9.5,/,
     &          '                         1/alpha(M_Z)       = ',F9.5,/)
      endif

* Set up "Density matrix" for initial state:
      dens( 1,-1) = (1+epol) * (1-ppol) / 4
      dens(-1, 1) = (1-epol) * (1+ppol) / 4
      dens( 1, 1) = (1+epol) * (1+ppol) / 4
      dens(-1,-1) = (1-epol) * (1-ppol) / 4
      END
CDECK  ID>, EEEEW.  
*CMZ :  2.01/08 29/11/95  16.08.19  by  Harald Anlauf
*CMZ :  2.00/01 30/09/94  15.01.21  by  Harald Anlauf
*CMZ :  1.99/03 26/05/93  13.10.29  by  Harald Anlauf & Helge Meinhard
*CMZ :  1.99/00 11/05/93  11.13.17  by  Harald Anlauf & Helge Meinhard
*-- Author :
      FUNCTION EEEEW (S, COSTH)
*     --------------
* The Born e+e- --> e+e- matrix element squared, including both gamma
* and Z in both s and t channel, and including WEAK corrections.
* W. Beenakker and S.C. van der Marck, April 1990.
* Heavy boxes (ZZ and WW) added: July 1990.
* Weighted summing/averaging over spins is performed using
* a density matrix for the initial state: H. Anlauf, September 1994.
      implicit none
      double precision eeeew, s, costh
      integer nrmass
      PARAMETER ( NRMASS = 9 )
      double precision RMASS, RMASS2, VF, AF, QF
      COMMON/ MASSES / RMASS(0:NRMASS), RMASS2(0:NRMASS),
     &          VF(0:NRMASS), AF(0:NRMASS), QF(0:NRMASS)
      double precision t3f, mq2mz
      common/ masses / t3f(0:NRMASS), mq2mz(0:NRMASS)
      COMPLEX*16 FZV,FZA,FGV,FGA
      COMMON/ FORMFA /FZV(0:NRMASS),FZA(0:NRMASS),
     +                FGV(0:NRMASS),FGA(0:NRMASS)
      double precision HBARC2, PI
      parameter (HBARC2 = 0.38937966d9, PI = 3.141592653589793238d0)
      double precision ALFA, ALFAS, FACQCD, FACQED, dens
      COMMON/ ADHOC  / ALFA, ALFAS, FACQCD, FACQED, dens(-1:1,-1:1)
      integer IWEAK, IFERM, ICHANN, IBOXES, IOUT
      COMMON/ OPTION / IWEAK, IFERM, ICHANN, IBOXES, IOUT
      double precision MATRIX(1:6), ZCOP(-1:1,-1:1)
      double complex GZS(-1:1,-1:1),GZT(-1:1,-1:1)
      double complex VZZS, AZZS, VWWS, AWWS, VZZT, AZZT, VWWT, AWWT
      double complex GS,GT,ZS,ZT,MIXS,MIXT, FFs, FFt
      double complex Vggs, Aggs, VgZs, AgZs, Vggt, Aggt, VgZt, AgZt
      double precision ppqp, ppqm, pppm, t, u, e2, sum, PHADPI
      double complex HADRQQ
      external HADRQQ, PHADPI
      integer i, l1, l2
      double complex null
      parameter (null = (0.d0, 0.d0))

      PPPM = 0.5d0  * s
      PPQP = 0.25d0 * s * (1 - costh)
      PPQM = 0.25d0 * s * (1 + costh)
      T    = - 2 * PPQP
      U    = - 2 * PPQM
* Define propagators, and include vertex form factors.
      E2   = 4*PI*ALFA
      I    = IFERM
      CALL GZPROP (S, GS, ZS, MIXS)
      if (iweak .eq. 0) then
         fzv(1) = null
         fza(1) = null
         fgv(1) = null
         fga(1) = null
         fzv(i) = null
         fza(i) = null
         fgv(i) = null
         fga(i) = null
      else
         CALL FORMFS (S, I)
      endif
      IF (ICHANN .EQ. 2) THEN
         GS = null
         ZS = null
         MIXS=null
      ENDIF
c The virtual+soft corrections:
      call ubffac (s, t, u, FFs, FFt,
     &             Vggs, Aggs, VgZs, AgZs, Vggt, Aggt, VgZt, AgZt)
      DO 20 L1 = - 1 , 1 , 2
         DO 10 L2 = - 1 , 1 , 2
c Update CERN 95-03: (171...175), use effective couplings, form factors:
            GZS(L1,L2) = E2 * FFs * (
     &           ( -QF(1)-FGV(1)            - L1*(      -FGA(1) ) )*
     &           ( -QF(I)-FGV(I)            - L2*(      -FGA(I) ) )*
     &           GS * (1 + Vggs + L1*L2*Aggs) +
     &           (        VF(1)+FZV(1)+(QF(1)+FGV(1))*MIXS
     &             - L1*( AF(1)+FZA(1)+       FGA(1) *MIXS) )*
     &           (        VF(I)+FZV(I)+(QF(I)+FGV(I))*MIXS
     &             - L2*( AF(I)+FZA(I)+       FGA(I) *MIXS) )*
     &           ZS * (1 + VgZs + L1*L2*AgZs) )
            ZCOP(L1,L2) = ((VF(1)-L1*AF(1))*(VF(IFERM)-L2*AF(IFERM)))**2
 10      CONTINUE
 20   CONTINUE
*     Heavy boxes !
      IF (ICHANN .EQ. 2 .or. iweak .eq. 0 .or. iboxes .eq. 0) THEN
         VZZS = null
         AZZS = null
         VWWS = null
         AWWS = null
      ELSE
         CALL HEAVYB (S,T,VZZS,AZZS,VWWS,AWWS)
      ENDIF
*     Now everything for the t channel
      CALL GZPROP (T, GT, ZT, MIXT)
      if (iweak .eq. 1) then
         CALL FORMFS (T, 1)
*     Incorporate the Burkhardt fit for the light quark loops.
*         GT = GT/( 1D0 - HADRQQ(T) - PHADPI(T) )
* otherwise form factors are still zero, no vacuum polarization
      endif
      IF (ICHANN .EQ. 1) THEN
         GT = null
         ZT = null
         MIXT=null
      ENDIF
      DO 40 L1 = - 1 , 1 , 2
         DO 30 L2 = - 1 , 1 , 2
c Update CERN 95-03: (171...175), use effective couplings, form factors:
            GZT(L1,L2) = E2 * FFt * (
     &           ( -QF(1)-FGV(1)            - L1*(      -FGA(1) ) )*
     &           ( -QF(1)-FGV(1)            - L2*(      -FGA(1) ) )*
     &           GT * (1 + Vggt + L1*L2*Aggt) +
     &           (        VF(1)+FZV(1)+(QF(1)+FGV(1))*MIXT
     &             - L1*( AF(1)+FZA(1)+       FGA(1) *MIXT) )*
     &           (        VF(1)+FZV(1)+(QF(1)+FGV(1))*MIXT
     &             - L2*( AF(1)+FZA(1)+       FGA(1) *MIXT) )*
     &           ZT * (1 + VgZt + L1*L2*AgZt) )
 30      CONTINUE
 40   CONTINUE
*     Heavy boxes !
      IF (ICHANN .EQ. 1 .or. iweak .eq. 0 .or. iboxes .eq. 0) THEN
         VZZT = null
         AZZT = null
         VWWT = null
         AWWT = null
      ELSE
         CALL HEAVYB (T,S,VZZT,AZZT,VWWT,AWWT)
      ENDIF
* There are 6 different helicity combinations.
      MATRIX(1) = 16*PPQM**2*(GZS( 1, 1)+GZT( 1, 1))*
     +                 DCONJG(GZS( 1, 1)+GZT( 1, 1))
      MATRIX(2) = 16*PPQM**2*(GZS(-1,-1)+GZT(-1,-1))*
     +                 DCONJG(GZS(-1,-1)+GZT(-1,-1))
      MATRIX(3) = 16*PPQP**2* GZS( 1,-1)*DCONJG(GZS( 1,-1))
      MATRIX(4) = 16*PPQP**2* GZS(-1, 1)*DCONJG(GZS(-1, 1))
      MATRIX(5) = 16*PPPM**2* GZT( 1,-1)*DCONJG(GZT( 1,-1))
      MATRIX(6) = 16*PPPM**2* GZT(-1, 1)*DCONJG(GZT(-1, 1))

      if (iboxes .eq. 1) then
*     Heavy boxes (factor 2 from 2*M0*M1)
      MATRIX(1) = MATRIX(1) + 32*PPQM**2*(GZS( 1, 1)+GZT( 1, 1))*
     +                        DCONJG((VZZS+AZZS+VZZT+AZZT)*ZCOP( 1, 1))
      MATRIX(2) = MATRIX(2) + 32*PPQM**2*(GZS(-1,-1)+GZT(-1,-1))*
     +  DCONJG((VZZS+AZZS+VZZT+AZZT)*ZCOP(-1,-1)+VWWS+AWWS+VWWT+AWWT)
      MATRIX(3) = MATRIX(3) + 32*PPQP**2* GZS( 1,-1)*
     +                        DCONJG(VZZS-AZZS)*ZCOP( 1,-1)
      MATRIX(4) = MATRIX(4) + 32*PPQP**2* GZS(-1, 1)*
     +                        DCONJG(VZZS-AZZS)*ZCOP(-1, 1)
      MATRIX(5) = MATRIX(5) + 32*PPPM**2* GZT( 1,-1)*
     +                        DCONJG(VZZT-AZZT)*ZCOP( 1,-1)
      MATRIX(6) = MATRIX(6) + 32*PPPM**2* GZT(-1, 1)*
     +                        DCONJG(VZZT-AZZT)*ZCOP(-1, 1)
      endif

      SUM =  dens( 1,-1) * (MATRIX(1) + MATRIX(3))
     &     + dens(-1, 1) * (MATRIX(2) + MATRIX(4))
     &     + dens( 1, 1) *  MATRIX(5)
     &     + dens(-1,-1) *  MATRIX(6)
      EEEEW = HBARC2 / (32*PI*S) * SUM
      END
CDECK  ID>, GZPROP. 
*CMZ :  2.01/08 29/11/95  15.38.47  by  Harald Anlauf
*CMZ :  1.99/03 26/05/93  17.22.30  by  Harald Anlauf & Helge Meinhard
*CMZ :  1.99/00 04/05/93  09.54.02  by  Harald Anlauf & Helge Meinhard
*-- Author :
      SUBROUTINE GZPROP (QSQR,PROPG,PROPZ,MIXING)
*     -----------------
* The gamma-Z propagators and their mixing, up to one loop corrections,
* but for the imaginary part of the Z propagator, which includes
* second order corrections.
* QSQR is input: the momentum transfer squared through the progagators.
* PROPG, PROPZ and MIXING are double complex output.
      implicit none
      double precision QSQR
      double complex   PROPG, PROPZ, MIXING
      double precision RMZ,RMW,RMH,RMT, ZWID, SIN2TH, sw, cw, cw2, pizmz
      COMMON/ BOSONS / RMZ,RMW,RMH,RMT, ZWID, SIN2TH, sw, cw, cw2, pizmz
      integer IWEAK, IFERM, ICHANN, IBOXES, IOUT
      COMMON/ OPTION / IWEAK, IFERM, ICHANN, IBOXES, IOUT
      double precision SIGG,SIGZ,SIGGZ, IMSIGG,IMSGGZ, IMSIGZ,IMZ2, IMZ
      external         SIGG,SIGZ,SIGGZ, IMSIGG,IMSGGZ, IMSIGZ,IMZ2, IMZ
      double complex   Z1, Z2, Z3, invdz
      IF (IWEAK .EQ. 1) THEN
         Z1 = DCMPLX( SIGG (QSQR) , IMSIGG(QSQR) )
*        Z2 = DCMPLX( SIGZ (QSQR) , IMSIGZ(QSQR) + IMZ2(QSQR) )
         Z2 = DCMPLX( SIGZ (QSQR) , IMZ(QSQR) )
         Z3 = DCMPLX( SIGGZ(QSQR) , IMSGGZ(QSQR) )
         propg = 1 / (QSQR + Z1)
         invdz = QSQR - RMZ**2 + Z2
c Update: CERN 95-03, (159,160):
         if (qsqr .gt. 0) then
c We haven't yet included a full 2-loop treatment of the self-energies,
c thus we linearize the imaginary part around the Z^0 pole.
            propz = 1 / dcmplx ( dble (invdz) - dble(Z3**2*propg),
     &                           (1+pizmz) * QSQR * ZWID/RMZ )
         else
            propz = 1 / (dble (invdz) - dble(Z3**2*propg))
         endif
c Note that the meaning of "mixing" has changed!  See write-up:
c mixing = \hat\Sigma^(\gamma Z)(s) / (s + \hat\Sigma^\gamma(s))
         mixing = Z3 * propg
      ELSE
         PROPG  = 1/QSQR
         IF (QSQR .GT. 0D0) THEN
            PROPZ = 1/DCMPLX( QSQR-RMZ**2 , RMZ*ZWID )
         ELSE
            PROPZ = DCMPLX ( 1/(QSQR-RMZ**2) , 0D0 )
         ENDIF
         MIXING = (0D0, 0D0)
      ENDIF
      END
CDECK  ID>, HEAVYB. 
*CMZ :  2.01/08 13/11/95  14.58.52  by  Harald Anlauf
*CMZ :  1.99/00 04/05/93  19.16.16  by  Harald Anlauf & Helge Meinhard
*-- Author :
      SUBROUTINE HEAVYB(S,T,VZZ,AZZ,VWW,AWW)
*     -----------------
* Subroutine giving the 'couplings' with which to contract the
* ZZ and WW boxes with the Born matrix element.
* S,T are input and VZZ,AZZ,VWW,AWW are 'double complex' output.
      IMPLICIT double precision (A-H,O-Z)
      double precision RMZ,RMW,RMH,RMT, ZWID, SIN2TH, sw, cw, cw2, pizmz
      COMMON/ BOSONS / RMZ,RMW,RMH,RMT, ZWID, SIN2TH, sw, cw, cw2, pizmz
      integer IWEAK, IFERM, ICHANN, IBOXES, IOUT
      COMMON/ OPTION / IWEAK, IFERM, ICHANN, IBOXES, IOUT
      double precision HBARC2, PI
      parameter (HBARC2 = 0.38937966d9, PI = 3.141592653589793238d0)
      double precision ALFA, ALFAS, FACQCD, FACQED, dens
      COMMON/ ADHOC  / ALFA, ALFAS, FACQCD, FACQED, dens(-1:1,-1:1)
      double complex VZZ,AZZ,VWW,AWW,RI5ST,RIST,RI5SU,RISU
      U = - S - T
      CALL HEAVYI(S,T,RMZ,RIST,RI5ST)
      CALL HEAVYI(S,U,RMZ,RISU,RI5SU)
      VZZ = ALFA/(2*PI)*( RIST  - RISU  )
      AZZ = ALFA/(2*PI)*( RI5ST + RI5SU )
*     WW boxes depend strongly on the isospin of the produced fermion
      IF(IFERM.EQ.0.OR.IFERM.EQ.4.OR.IFERM.EQ.6.OR.IFERM.EQ.8) THEN
*       isospin = + 1/2
        CALL HEAVYI(S,U,RMW,RISU,RI5SU)
        VWW = ALFA/(2*PI)*( - RISU  )
        AWW = ALFA/(2*PI)*( + RI5SU )
      ELSE
*       isospin = - 1/2
        CALL HEAVYI(S,T,RMW,RIST,RI5ST)
        VWW = ALFA/(2*PI)*( + RIST  )
        AWW = ALFA/(2*PI)*( + RI5ST )
      ENDIF
* To get the normalization right
      E2 = 4.d0 * PI * alfa
      VZZ = VZZ * E2/S
      AZZ = AZZ * E2/S
      VWW = VWW * E2/S/(4*SIN2TH**2)
      AWW = AWW * E2/S/(4*SIN2TH**2)
      END
CDECK  ID>, HEAVYI. 
*CMZ :  2.01/08 17/11/95  19.37.52  by  Harald Anlauf
*CMZ :  2.01/00 10/01/95  14.39.23  by  Harald Anlauf
*CMZ :  1.99/00 04/05/93  09.54.02  by  Harald Anlauf & Helge Meinhard
*-- Author :
      SUBROUTINE HEAVYI(S,T,RM,RI,RI5)
*     -----------------
* Function needed to calculate ZZ or WW boxes.
* S,T,RM are input, RI,RI5 are 'double complex' output.
      IMPLICIT double precision (A-H,O-Z)
      PARAMETER( EPS = 1D-10 )
      double precision HBARC2, PI
      parameter (HBARC2 = 0.38937966d9, PI = 3.141592653589793238d0)
      double precision ALFA, ALFAS, FACQCD, FACQED, dens
      COMMON/ ADHOC  / ALFA, ALFAS, FACQCD, FACQED, dens(-1:1,-1:1)
      double complex RI,RI5,SPENCE,ROOT1,ROOT2,X1,X2,Y1,Y2,FOURSP,RLOG12
      double complex SHELP,THELP,I,X1X2
      parameter (I = (0D0,1D0))
      IF ( S.GT.0D0 .AND. T.GT.0D0 ) THEN
        WRITE(*,'(A)')' HEAVYI: both S and T > 0.  This is not valid!'
        RI  = (0D0,0D0)
        RI5 = (0D0,0D0)
        RETURN
      ENDIF
      RM2 = RM**2
      IF( S .GT. 0D0 ) THEN
        SHELP = 4D0*RM2/DCMPLX(S,EPS)
      ELSE
        SHELP = 4D0*RM2/S
      ENDIF
      IF( T .GT. 0D0 ) THEN
        THELP = RM2/DCMPLX(T,EPS)
      ELSE
        THELP = RM2/T
      ENDIF
      ROOT1 = SQRT( (1D0,0D0)-SHELP )
      IF(S.LT.0D0.AND.T.LT.0D0.AND.4D0*RM2/S*(1D0+RM2/T).GT.1D0) THEN
        ROOT2 = I*SQRT( -( (1D0,0D0)-SHELP*( (1D0,0D0) + THELP ) ) )
      ELSE
        ROOT2 =   SQRT(    (1D0,0D0)-SHELP*( (1D0,0D0) + THELP )   )
      ENDIF
      Y1 = 0.5D0*( 1D0 + ROOT1 )
      Y2 = 0.5D0*( 1D0 - ROOT1 )
      X1 = 0.5D0*( 1D0 + ROOT2 )
      X2 = 0.5D0*( 1D0 - ROOT2 )
      X1X2 = ROOT2
      FOURSP = SPENCE(X1/(X1-Y1)) + SPENCE(X1/(X1-Y2)) -
     +         SPENCE(X2/(X2-Y2)) - SPENCE(X2/(X2-Y1))
      RLOG12 = LOG(-Y1/Y2)
      IF( ABS(X1X2) .LT. 10D0*EPS ) THEN
        X1X2 = (1D0,0D0)
        FOURSP = 1D0/(Y1-Y2)*( - 4D0*Y1*LOG(2D0*Y1/(Y1-Y2))
     +                         + 4D0*Y2*LOG(2D0*Y2/(Y2-Y1)) )
      ENDIF
      RI5 = (2D0*T+S+2D0*RM2)/(2D0*(S+T))*(
     +      SPENCE( 1D0+DCMPLX(T,EPS)/RM2 ) - PI*PI/6D0 - RLOG12**2 ) +
     +      .5D0*LOG(-DCMPLX(T,EPS)/RM2) + (Y2-Y1)/2D0*RLOG12 +
     +      ( S+2D0*T - 4D0*T*RM2/S + 2D0*RM2**2/T - 2D0*RM2**2/S )/
     +      ( 2D0*( S + T )*(-X1X2) ) * FOURSP
      RI5 = S/( S + T ) * RI5
      RI  = RI5 + 2D0*RLOG12**2 + 2D0/X1X2*FOURSP
      END
CDECK  ID>, COUPLS. 
*CMZ :  2.01/08 17/11/95  19.29.18  by  Harald Anlauf
*CMZ :  1.99/00 06/05/93  12.41.28  by  Harald Anlauf & Helge Meinhard
*-- Author :
      SUBROUTINE COUPLS (SIN2TH, RMT ,RMZ)
*     -----------------
* Define the fermion masses and their couplings to the bosons.
      implicit none
      double precision SIN2TH, RMT, RMZ
      integer nrmass
      PARAMETER ( NRMASS = 9 )
      double precision RMASS, RMASS2, VF, AF, QF
      COMMON/ MASSES / RMASS(0:NRMASS), RMASS2(0:NRMASS),
     &          VF(0:NRMASS), AF(0:NRMASS), QF(0:NRMASS)
      double precision t3f, mq2mz
      common/ masses / t3f(0:NRMASS), mq2mz(0:NRMASS)
      double precision SW, CW, Q, T3, runmas
      external runmas
      integer i

      RMASS(0) = 0D0
c Update: Charged lepton masses from Particle Data Group Publ. 1994.
      RMASS(1) = 0.51099906D-3
      RMASS(2) = 0.105658389D0
      RMASS(3) = 1.7771D0
c Update: The lighter quarks: (But avoid exact degeneracy for quarks!)
c (Note that they are tuned to reproduce the hadronic vacuum
c polarization with sufficient precision).
      RMASS(4) = 0.0449D0
      RMASS(5) = 0.0449D0 + 1.d-5
      RMASS(7) = 0.150D0
c Update: m_c, m_b according to CERN 95-03
      RMASS(6) = 1.55D0
      RMASS(8) = RMT
      RMASS(9) = 4.7D0
      SW = SQRT (    SIN2TH)
      CW = SQRT (1 - SIN2TH)
      DO 10 I = 0 , NRMASS
         RMASS2(I) = RMASS(I)**2
         IF(I .EQ. 0) THEN
            Q  =  0
            T3 =  0.5D0
         ELSEIF(I .LE. 3) THEN
            Q  = -1
            T3 = -0.5D0
         ELSEIF(I.EQ.4 .OR. I.EQ.6 .OR. I.EQ.8) THEN
            Q  =  2D0/3D0
            T3 =  0.5D0
         ELSE
            Q  = -1D0/3D0
            T3 = -0.5D0
         ENDIF
         VF(I)  = ( T3 - 2*Q*SIN2TH ) / (2*CW*SW)
         AF(I)  =   T3                / (2*CW*SW)
         QF(I)  =   Q
         t3f(i) =   T3
   10 CONTINUE

c For the QCD corrections to the Z decay width, we need the running
c quark masses at M_Z.  But this is only important for c and b.
c So we simply leave everything else as is.
      do i = 0, NRMASS
         if (i .eq. 6 .or. i .eq. 9) then
            mq2mz(i) = runmas (rmass(i), RMZ) ** 2
         else
            mq2mz(i) = rmass2(i)
         endif
      enddo

      END
CDECK  ID>, RNORM.  
*CMZ :  2.01/08 29/11/95  15.38.47  by  Harald Anlauf
*CMZ :  2.01/02 09/03/95  01.14.11  by  Harald Anlauf
*CMZ :  1.99/00 06/05/93  12.41.28  by  Harald Anlauf & Helge Meinhard
*-- Author :
      SUBROUTINE RNORM()
*     ----------------
* W.J.P. Beenakker, F.A.Berends and S.C. van der Marck, June 14th 1989
* Calculate all quantities that have to do with weak corrections on
* boson propagators.
      IMPLICIT double precision (A-H,O-Z)
      integer nrmass
      PARAMETER ( NRMASS = 9 )
      double precision RMASS, RMASS2, VF, AF, QF
      COMMON/ MASSES / RMASS(0:NRMASS), RMASS2(0:NRMASS),
     &          VF(0:NRMASS), AF(0:NRMASS), QF(0:NRMASS)
      double precision t3f, mq2mz
      common/ masses / t3f(0:NRMASS), mq2mz(0:NRMASS)
      double precision RMZ,RMW,RMH,RMT, ZWID, SIN2TH, sw, cw, cw2, pizmz
      COMMON/ BOSONS / RMZ,RMW,RMH,RMT, ZWID, SIN2TH, sw, cw, cw2, pizmz
      double precision PIGAM0,SIGGZ0,DELMZ,DELMW,DELZ2Z,DELZ2G,DELZ2W,
     &                 SUMQ1,SUMQ2
      COMMON/ RENORM / PIGAM0,SIGGZ0,DELMZ,DELMW,DELZ2Z,DELZ2G,DELZ2W,
     &                 SUMQ1,SUMQ2
      double precision HBARC2, PI
      parameter (HBARC2 = 0.38937966d9, PI = 3.141592653589793238d0)
      double precision ALFA, ALFAS, FACQCD, FACQED, dens
      COMMON/ ADHOC  / ALFA, ALFAS, FACQCD, FACQED, dens(-1:1,-1:1)

      double precision drhoHO, SIGG, IMSIGG, SIGGZ, IMSGGZ
      external drhoHO, SIGG, IMSIGG, SIGGZ, IMSGGZ

      sw  = sqrt (SIN2TH)
      cw2 = 1 - SIN2TH
      cw  = sqrt (cw2)
      Z   = RMZ**2
      W   = RMW**2
      SIGGZ0 = USIGGZ( 0D0 )
      PIGAM0 = DUSIGG( 0D0 )
*
* Renormalization constants eq. (3.16) and (3.17) of ref.2b
*      DELMZ  =   USIGZ( Z )
c If higher-order self-energies: Update: CERN 95-03, (154)
      DELMZ  =   USIGZ( Z ) -
     &     dble ( dcmplx (SIGGZ(Z),IMSGGZ(Z))**2
     &            / (Z+DCMPLX (SIGG(Z),IMSIGG(Z)) ) )
      DELMW  =   USIGW( W )
      DELZ2G = - PIGAM0
c Update: CERN 95-03, (156); higher order corrections to \Delta\rho:
      delrho = drhoHO ()
      DELZ2Z = - PIGAM0 - 2D0*(CW2-SIN2TH)/SW/CW*SIGGZ0/Z +
     +           (CW2-SIN2TH)/SIN2TH*( DELMZ/Z - DELMW/W + delrho)
      DELZ2W = - PIGAM0 - 2D0*CW/SW*SIGGZ0/Z +
     +                    CW2/SIN2TH*( DELMZ/Z - DELMW/W + delrho)
*
* Contributions from the DELTA-i terms
      SUMQ1 = ALFA/(4*PI * 2 * SIN2TH * W) * (
     +      + ( RMASS2(4)-RMASS2(5) )*LOG(RMASS(4)/RMASS(5))
     +      + ( RMASS2(6)-RMASS2(7) )*LOG(RMASS(6)/RMASS(7))
     +      + ( RMASS2(8)-RMASS2(9) )*LOG(RMASS(8)/RMASS(9)) )
      SUMQ2 = ALFA/(2*PI) * ( + LOG(RMASS(4)/RMASS(5))
     +                        + LOG(RMASS(6)/RMASS(7))
     +                        + LOG(RMASS(8)/RMASS(9)) )
*
* Calculate delta-r and update the values for sin(theta-w) and MW.
      DR = DELTAR()
*     BIGA0  = 37.2802D0**2
      GFERMI = 1.16639d-5
      BIGA0  = PI*ALFA/(sqrt(2.d0) * GFERMI)
      SIN2TH = 0.5D0*(  1 - SQRT( 1-4*BIGA0/Z/(1-DR) )  )
      RMW = SQRT( Z*( 1 - SIN2TH ) )
c Update some frequently used parameters:
      sw  = sqrt (SIN2TH)
      cw2 =  1 - SIN2TH
      cw  = sqrt (cw2)
      END
CDECK  ID>, FORMFS. 
*CMZ :  2.01/08 23/11/95  15.52.54  by  Harald Anlauf
*CMZ :  2.01/00 10/01/95  14.39.23  by  Harald Anlauf
*CMZ :  1.99/00 06/05/93  12.41.28  by  Harald Anlauf & Helge Meinhard
*-- Author :
      SUBROUTINE FORMFS(QSQR,IFERM)
*     -----------------
* W.J.P. Beenakker, F.A.Berends and S.C. van der Marck, June 14th 1989
* Calculate the vector and axial vector formfactors for the Z-ff and
* the gamma-ff couplings.
      IMPLICIT double precision (A-H,O-Z)
      integer nrmass
      PARAMETER ( NRMASS = 9 )
      double precision RMASS, RMASS2, VF, AF, QF
      COMMON/ MASSES / RMASS(0:NRMASS), RMASS2(0:NRMASS),
     &          VF(0:NRMASS), AF(0:NRMASS), QF(0:NRMASS)
      double precision t3f, mq2mz
      common/ masses / t3f(0:NRMASS), mq2mz(0:NRMASS)
      double precision RMZ,RMW,RMH,RMT, ZWID, SIN2TH, sw, cw, cw2, pizmz
      COMMON/ BOSONS / RMZ,RMW,RMH,RMT, ZWID, SIN2TH, sw, cw, cw2, pizmz
      double precision HBARC2, PI
      parameter (HBARC2 = 0.38937966d9, PI = 3.141592653589793238d0)
      double precision ALFA, ALFAS, FACQCD, FACQED, dens
      COMMON/ ADHOC  / ALFA, ALFAS, FACQCD, FACQED, dens(-1:1,-1:1)
      COMPLEX*16 FZV,FZA,FGV,FGA
      COMMON/ FORMFA /FZV(0:NRMASS),FZA(0:NRMASS),
     +                FGV(0:NRMASS),FGA(0:NRMASS)
      double complex FL0,FL1,FL2,FL3,FL4,FL5
      double complex RL2Z,RL2W,RL3W,RL3Z
*
      CALL LABDAS(QSQR,RMZ,RL2Z,RL3Z)
      CALL LABDAS(QSQR,RMW,RL2W,RL3W)
      ALF4PI = ALFA / (4*PI)
* eq. (C.4) ref 2b.
      FL1 = RL2W / (8*SW**3*CW) - 3*CW/(4*SW**3) * RL3W
      FL2 = -(1-2.d0/3*SIN2TH)/(8*SW**3*CW) * RL2W +
     +                            3*CW/(4*SW**3) * RL3W
      FL3 =  (1-4.d0/3*SIN2TH)/(8*SW**3*CW) * RL2W -
     +                            3*CW/(4*SW**3) * RL3W
      IF(IFERM .EQ. 9) THEN
         CALL FLBOT(QSQR,FL4,FL5)
      ELSE
         FL4 = FL3
         FL5 = 1/(6*SIN2TH) * RL2W - 3/(4*SIN2TH) * RL3W
      ENDIF
* eq. (C.2) of ref 2b.
      FZV(0) = ALF4PI/(4*SW*CW)*( RL2Z/(4*CW2*SIN2TH) +
     +           (1-1/(2*SIN2TH)) * RL2W + 3*CW2/SIN2TH * RL3W )
      FZA(0) = FZV(0)
* eq. (C.3) of ref 2b.
      FZV(1) = ALF4PI*( VF(1)*(VF(1)**2+3D0*AF(1)**2)*RL2Z + FL1 )
      FZA(1) = ALF4PI*( AF(1)*(3D0*VF(1)**2+AF(1)**2)*RL2Z + FL1 )
      FZV(4) = ALF4PI*( VF(4)*(VF(4)**2+3D0*AF(4)**2)*RL2Z + FL2 )
      FZA(4) = ALF4PI*( AF(4)*(3D0*VF(4)**2+AF(4)**2)*RL2Z + FL2 )
      FZV(5) = ALF4PI*( VF(5)*(VF(5)**2+3D0*AF(5)**2)*RL2Z + FL3 )
      FZA(5) = ALF4PI*( AF(5)*(3D0*VF(5)**2+AF(5)**2)*RL2Z + FL3 )
      FZV(9) = ALF4PI*( VF(9)*(VF(9)**2+3D0*AF(9)**2)*RL2Z + FL4 )
      FZA(9) = ALF4PI*( AF(9)*(3D0*VF(9)**2+AF(9)**2)*RL2Z + FL4 )
* eq. (C.12) ref 2b.
      FL1 =                        -3/(4*SIN2TH) * RL3W
      FL2 = -1/(12*SIN2TH) * RL2W + 3/(4*SIN2TH) * RL3W
      FL3 =  1/( 6*SIN2TH) * RL2W - 3/(4*SIN2TH) * RL3W
c Neutrino form factor: Thu Nov 23 15:43:33 1995 [ha]
c      FGV(0) = (0D0,0D0)
      FL0 = -1/( 4*SIN2TH) * RL2W + 3/(4*SIN2TH) * RL3W
      FGV(0) = ALF4PI*( FL0 )
      FGA(0) = FGV(0)
* eq. (C.11) ref 2b.
      FGV(1) = ALF4PI*( QF(1)*(VF(1)**2+AF(1)**2)*RL2Z + FL1 )
      FGA(1) = ALF4PI*( QF(1)*( 2D0*VF(1)*AF(1) )*RL2Z + FL1 )
      FGV(4) = ALF4PI*( QF(4)*(VF(4)**2+AF(4)**2)*RL2Z + FL2 )
      FGA(4) = ALF4PI*( QF(4)*( 2D0*VF(4)*AF(4) )*RL2Z + FL2 )
      FGV(5) = ALF4PI*( QF(5)*(VF(5)**2+AF(5)**2)*RL2Z + FL3 )
      FGA(5) = ALF4PI*( QF(5)*( 2D0*VF(5)*AF(5) )*RL2Z + FL3 )
      FGV(9) = ALF4PI*( QF(9)*(VF(9)**2+AF(9)**2)*RL2Z + FL5 )
      FGA(9) = ALF4PI*( QF(9)*( 2D0*VF(9)*AF(9) )*RL2Z + FL5 )
* all others are related to the previous ones.
      DO 10 I = 0 , NRMASS
         IF(I.EQ.2 .OR. I.EQ.3) THEN
            FZV(I) = FZV(1)
            FZA(I) = FZA(1)
            FGV(I) = FGV(1)
            FGA(I) = FGA(1)
         ELSEIF(I.EQ.6 .OR. I.EQ.8) THEN
            FZV(I) = FZV(4)
            FZA(I) = FZA(4)
            FGV(I) = FGV(4)
            FGA(I) = FGA(4)
         ELSEIF(I.EQ.7) THEN
            FZV(I) = FZV(5)
            FZA(I) = FZA(5)
            FGV(I) = FGV(5)
            FGA(I) = FGA(5)
         ENDIF
   10 CONTINUE
      END
CDECK  ID>, LABDAS. 
*CMZ :  2.01/08 23/11/95  14.25.42  by  Harald Anlauf
*CMZ :  2.01/02 31/01/95  01.05.14  by  Harald Anlauf
*CMZ :  2.01/00 10/01/95  14.18.22  by  Harald Anlauf
*CMZ :  1.99/07 02/06/93  19.25.55  by  Harald Anlauf & Helge Meinhard
*CMZ :  1.99/05 28/05/93  13.25.15  by  Harald Anlauf & Helge Meinhard
*CMZ :  1.99/00 04/05/93  09.54.02  by  Harald Anlauf & Helge Meinhard
*-- Author :
      SUBROUTINE LABDAS(QSQR,RM,LABDA2,LABDA3)
*     -----------------
      IMPLICIT double precision (A-H,O-Z)
      double complex LABDA2,LABDA3,W,ctmp,X,LOGX,SPENCE
      PARAMETER( EPS = 1D-10 )
      W = RM**2 / DCMPLX (QSQR, EPS)
      ctmp = sqrt (1-4*W)
      X = (ctmp - 1) / (ctmp + 1)
      LOGX = log (X)
*      LOGX = LOG( - BIGK(QSQR,RM) )
* This way of writing was not very stable for qsqr << rm**2. The
* second way is somewhat better, but still has to be cut off at some
* low qsqr value, in which case it should yield zero.
*      LABDA2 = -3.5D0 - 2D0*W - (2D0*W+3D0)*LOG(-W)+
*     +   2D0*(1D0+W)**2*( SPENCE(1D0+1D0/W) - PI**2/6D0 )
      LABDA2 = -3.5D0 - 2D0*W - (2D0*W+3D0)*LOG(-W)+
     +   2D0*(1D0+W)**2*( -SPENCE(-1D0/W)+LOG(-W)*LOG(1D0+1D0/W) )
      IF(DREAL(W).GT.1D6) LABDA2 = (0D0,0D0)
c There's a sign error in CERN 95-03, eq. (170).  This is correct:
      LABDA3 =  5D0/6D0 - 2D0/3D0*W - 1D0/3D0*(2D0*W+1)*
     +   SQRT(1D0-4D0*W)*LOGX + 2D0/3D0*W*(W+2D0)*LOGX**2
      END
CDECK  ID>, SIGG.   
*CMZ :  1.99/00 04/05/93  09.54.02  by  Harald Anlauf & Helge Meinhard
*-- Author :
      FUNCTION SIGG(QSQR)
*     -------------
* Real part of the renormalized weakly corrected photon propagator
      IMPLICIT double precision (A-H,O-Z)
      double precision PIGAM0,SIGGZ0,DELMZ,DELMW,DELZ2Z,DELZ2G,DELZ2W,
     &                 SUMQ1,SUMQ2
      COMMON/ RENORM / PIGAM0,SIGGZ0,DELMZ,DELMW,DELZ2Z,DELZ2G,DELZ2W,
     &                 SUMQ1,SUMQ2
* eq. (3.23) ref 2b.
      SIGG = USIGG(QSQR) - PIGAM0 * QSQR
      END
CDECK  ID>, SIGGZ.  
*CMZ :  2.01/08 14/11/95  20.36.37  by  Harald Anlauf
*CMZ :  2.01/02 31/01/95  01.00.43  by  Harald Anlauf
*CMZ :  1.99/00 04/05/93  09.54.03  by  Harald Anlauf & Helge Meinhard
*-- Author :
      FUNCTION SIGGZ(QSQR)
*     --------------
* Real part of the renormalized weakly corrected photon-Z mixing
      IMPLICIT double precision (A-H,O-Z)
      double precision PIGAM0,SIGGZ0,DELMZ,DELMW,DELZ2Z,DELZ2G,DELZ2W,
     &                 SUMQ1,SUMQ2
      COMMON/ RENORM / PIGAM0,SIGGZ0,DELMZ,DELMW,DELZ2Z,DELZ2G,DELZ2W,
     &                 SUMQ1,SUMQ2
      double precision RMZ,RMW,RMH,RMT, ZWID, SIN2TH, sw, cw, cw2, pizmz
      COMMON/ BOSONS / RMZ,RMW,RMH,RMT, ZWID, SIN2TH, sw, cw, cw2, pizmz

* eq. (3.23) ref 2b.
      SIGGZ = USIGGZ(QSQR) - SIGGZ0 - QSQR* CW*SW/(CW2-SIN2TH)*
     +        ( DELZ2Z - DELZ2G ) +
     +        QSQR*( - CW/SW*SUMQ1 - SUMQ2/(6*CW*SW) )
      END
CDECK  ID>, SIGZ.   
*CMZ :  2.01/08 14/11/95  17.24.05  by  Harald Anlauf
*CMZ :  1.99/00 04/05/93  09.54.03  by  Harald Anlauf & Helge Meinhard
*-- Author :
      FUNCTION SIGZ(QSQR)
*     -------------
* Real part of the renormalized weakly corrected Z propagator
      IMPLICIT double precision (A-H,O-Z)
      double precision PIGAM0,SIGGZ0,DELMZ,DELMW,DELZ2Z,DELZ2G,DELZ2W,
     &                 SUMQ1,SUMQ2
      COMMON/ RENORM / PIGAM0,SIGGZ0,DELMZ,DELMW,DELZ2Z,DELZ2G,DELZ2W,
     &                 SUMQ1,SUMQ2
      double precision RMZ,RMW,RMH,RMT, ZWID, SIN2TH, sw, cw, cw2, pizmz
      COMMON/ BOSONS / RMZ,RMW,RMH,RMT, ZWID, SIN2TH, sw, cw, cw2, pizmz

* eq. (3.23) ref 2b.
      SIGZ = USIGZ(QSQR) - DELMZ + DELZ2Z*( QSQR - RMZ**2 ) +
     +       (QSQR-RMZ**2)*((CW2-SIN2TH)/SIN2TH*SUMQ1+SUMQ2/3D0/SIN2TH)
      END
CDECK  ID>, SIGW.   
*CMZ :  2.01/08 14/11/95  17.24.59  by  Harald Anlauf
*CMZ :  1.99/00 04/05/93  09.54.03  by  Harald Anlauf & Helge Meinhard
*-- Author :
      FUNCTION SIGW(QSQR)
*     -------------
* Real part of the renormalized weakly corrected W propagator
      IMPLICIT double precision (A-H,O-Z)
      double precision PIGAM0,SIGGZ0,DELMZ,DELMW,DELZ2Z,DELZ2G,DELZ2W,
     &                 SUMQ1,SUMQ2
      COMMON/ RENORM / PIGAM0,SIGGZ0,DELMZ,DELMW,DELZ2Z,DELZ2G,DELZ2W,
     &                 SUMQ1,SUMQ2
      double precision RMZ,RMW,RMH,RMT, ZWID, SIN2TH, sw, cw, cw2, pizmz
      COMMON/ BOSONS / RMZ,RMW,RMH,RMT, ZWID, SIN2TH, sw, cw, cw2, pizmz

* eq. (3.23) ref 2b.
      SIGW = USIGW(QSQR) - DELMW + DELZ2W*( QSQR - RMW**2 ) +
     +       (QSQR-RMW**2)*( CW2/SIN2TH*SUMQ1 + SUMQ2/3D0/SIN2TH )
      END
CDECK  ID>, DELTAR. 
*CMZ :  1.99/00 04/05/93  09.54.03  by  Harald Anlauf & Helge Meinhard
*-- Author :
      FUNCTION DELTAR()
*     ---------------
* W.J.P. Beenakker, F.A.Berends and S.C. van der Marck, June 14th 1989
* The weak correction factor delta-r
      IMPLICIT double precision (A-H,O-Z)
      double precision RMZ,RMW,RMH,RMT, ZWID, SIN2TH, sw, cw, cw2, pizmz
      COMMON/ BOSONS / RMZ,RMW,RMH,RMT, ZWID, SIN2TH, sw, cw, cw2, pizmz
      double precision HBARC2, PI
      parameter (HBARC2 = 0.38937966d9, PI = 3.141592653589793238d0)
      double precision ALFA, ALFAS, FACQCD, FACQED, dens
      COMMON/ ADHOC  / ALFA, ALFAS, FACQCD, FACQED, dens(-1:1,-1:1)
* eq. (4.18) ref 2b.
      DELTAR = SIGW(0D0)/RMW**2 + ALFA/4D0/PI/SIN2TH*
     +        ( 6D0 + (7D0-4D0*SIN2TH)/2D0/SIN2TH*LOG(1D0-SIN2TH) )
      END
CDECK  ID>, USIGG.  
*CMZ :  2.01/08 14/11/95  20.50.04  by  Harald Anlauf
*CMZ :  2.01/04 20/03/95  15.11.34  by  Harald Anlauf
*CMZ :  2.01/02 31/01/95  00.51.52  by  Harald Anlauf
*CMZ :  1.99/00 06/05/93  12.41.28  by  Harald Anlauf & Helge Meinhard
*-- Author :
      FUNCTION USIGG(QSQR)
*     --------------
* W.J.P. Beenakker, F.A.Berends and S.C. van der Marck, June 14th 1989
* Real part of the unrenormalized weakly corrected photon prop
* eq. (B.2) ref 2b with errata, a minus sign and a bracket.
      IMPLICIT double precision (A-H,O-Z)
      integer nrmass
      PARAMETER ( NRMASS = 9 )
      double precision RMASS, RMASS2, VF, AF, QF
      COMMON/ MASSES / RMASS(0:NRMASS), RMASS2(0:NRMASS),
     &          VF(0:NRMASS), AF(0:NRMASS), QF(0:NRMASS)
      double precision t3f, mq2mz
      common/ masses / t3f(0:NRMASS), mq2mz(0:NRMASS)
      double precision RMZ,RMW,RMH,RMT, ZWID, SIN2TH, sw, cw, cw2, pizmz
      COMMON/ BOSONS / RMZ,RMW,RMH,RMT, ZWID, SIN2TH, sw, cw, cw2, pizmz
      double precision HBARC2, PI
      parameter (HBARC2 = 0.38937966d9, PI = 3.141592653589793238d0)
      double precision ALFA, ALFAS, FACQCD, FACQED, dens
      COMMON/ ADHOC  / ALFA, ALFAS, FACQCD, FACQED, dens(-1:1,-1:1)
      S   = QSQR
      TOT = 0D0
      FAC = 1D0
      DO 10 I = 1 , NRMASS
         IF(I .EQ. 4) FAC = 3D0
         RM = RMASS(I)
         TOT = TOT + 4D0/3D0*QF(I)**2*(
     +         ( S+2*RMASS2(I) )*freal1(S,RM) - S/3D0 )  *  FAC
   10 CONTINUE
      TOT = TOT - ( 3D0*S + 4D0*RMW**2 )*freal1(S,RMW)
      USIGG = ALFA/(4D0*PI) * TOT
      END
CDECK  ID>, DUSIGG. 
*CMZ :  2.01/08 15/11/95  13.09.41  by  Harald Anlauf
*CMZ :  2.01/04 20/03/95  15.11.34  by  Harald Anlauf
*CMZ :  1.99/00 06/05/93  12.41.28  by  Harald Anlauf & Helge Meinhard
*-- Author :
      FUNCTION DUSIGG(QSQR)
*     ---------------
* W.J.P. Beenakker, F.A.Berends and S.C. van der Marck, June 14th 1989
* Derivative of the real part of the unrenormalized
* weakly corrected photon propagator
      IMPLICIT double precision (A-H,O-Z)
      integer nrmass
      PARAMETER ( NRMASS = 9 )
      double precision RMASS, RMASS2, VF, AF, QF
      COMMON/ MASSES / RMASS(0:NRMASS), RMASS2(0:NRMASS),
     &          VF(0:NRMASS), AF(0:NRMASS), QF(0:NRMASS)
      double precision t3f, mq2mz
      common/ masses / t3f(0:NRMASS), mq2mz(0:NRMASS)
      double precision RMZ,RMW,RMH,RMT, ZWID, SIN2TH, sw, cw, cw2, pizmz
      COMMON/ BOSONS / RMZ,RMW,RMH,RMT, ZWID, SIN2TH, sw, cw, cw2, pizmz
      double precision HBARC2, PI
      parameter (HBARC2 = 0.38937966d9, PI = 3.141592653589793238d0)
      double precision ALFA, ALFAS, FACQCD, FACQED, dens
      COMMON/ ADHOC  / ALFA, ALFAS, FACQCD, FACQED, dens(-1:1,-1:1)
      S   = QSQR
      TOT = 0D0
      FAC = 1D0
      DO 10 I = 1 , NRMASS
         IF(I .EQ. 4) FAC = 3D0
         RM = RMASS(I)
         TOT = TOT + 4D0/3D0*QF(I)**2*( + freal1(S,RM) +
     +         ( S+2D0*RMASS2(I) )*DFREAL(S,RM,RM) - 1.d0/3 )  *  FAC
   10 CONTINUE
      TOT = TOT - 3*freal1(S,RMW) - (3*S + 4*RMW**2)*DFREAL(S,RMW,RMW)
      DUSIGG = ALFA/(4*PI) * TOT
      END
CDECK  ID>, USIGGZ. 
*CMZ :  2.01/08 14/11/95  20.32.10  by  Harald Anlauf
*CMZ :  2.01/04 20/03/95  15.11.34  by  Harald Anlauf
*CMZ :  2.01/02 31/01/95  00.36.18  by  Harald Anlauf
*CMZ :  1.99/00 06/05/93  12.41.28  by  Harald Anlauf & Helge Meinhard
*-- Author :
      FUNCTION USIGGZ(QSQR)
*     ---------------
* W.J.P. Beenakker, F.A.Berends and S.C. van der Marck, June 14th 1989
* Real part of the unrenormalized photon-Z mixing propagator
* eq. (B.3) ref 2b.
      IMPLICIT double precision (A-H,O-Z)
      integer nrmass
      PARAMETER ( NRMASS = 9 )
      double precision RMASS, RMASS2, VF, AF, QF
      COMMON/ MASSES / RMASS(0:NRMASS), RMASS2(0:NRMASS),
     &          VF(0:NRMASS), AF(0:NRMASS), QF(0:NRMASS)
      double precision t3f, mq2mz
      common/ masses / t3f(0:NRMASS), mq2mz(0:NRMASS)
      double precision RMZ,RMW,RMH,RMT, ZWID, SIN2TH, sw, cw, cw2, pizmz
      COMMON/ BOSONS / RMZ,RMW,RMH,RMT, ZWID, SIN2TH, sw, cw, cw2, pizmz
      double precision HBARC2, PI
      parameter (HBARC2 = 0.38937966d9, PI = 3.141592653589793238d0)
      double precision ALFA, ALFAS, FACQCD, FACQED, dens
      COMMON/ ADHOC  / ALFA, ALFAS, FACQCD, FACQED, dens(-1:1,-1:1)
      S   = QSQR
      TOT = 0
      FAC = 1
      DO 10 I = 1 , NRMASS
         IF(I .EQ. 4) FAC = 3D0
         RM = RMASS(I)
         TOT = TOT - 4D0/3D0*QF(I)*VF(I)*(
     +         ( S+2*RMASS2(I) )*freal1(S,RM) - S/3D0 )  *  FAC
   10 CONTINUE
      TOT = TOT + 1/(CW*SW)*( ( 3*CW2 + 1D0/6D0 )*S
     +                      + ( 4*CW2 + 4D0/3D0 )*RMW**2 )*
     +                          freal1(S,RMW)  +  S/(9*CW*SW)
      USIGGZ = ALFA/(4*PI) * TOT
      END
CDECK  ID>, DUSGGZ. 
*CMZ :  2.01/08 15/11/95  13.09.41  by  Harald Anlauf
*-- Author :    Harald Anlauf   14/11/95

      function DUSGGZ (S)
c     ---------------
c Real part of the derivative of the unrenormalized photon-Z mixing
c propagator.
      implicit none
      double precision DUSGGZ, S
      integer nrmass
      PARAMETER ( NRMASS = 9 )
      double precision RMASS, RMASS2, VF, AF, QF
      COMMON/ MASSES / RMASS(0:NRMASS), RMASS2(0:NRMASS),
     &          VF(0:NRMASS), AF(0:NRMASS), QF(0:NRMASS)
      double precision t3f, mq2mz
      common/ masses / t3f(0:NRMASS), mq2mz(0:NRMASS)
      double precision RMZ,RMW,RMH,RMT, ZWID, SIN2TH, sw, cw, cw2, pizmz
      COMMON/ BOSONS / RMZ,RMW,RMH,RMT, ZWID, SIN2TH, sw, cw, cw2, pizmz
      double precision HBARC2, PI
      parameter (HBARC2 = 0.38937966d9, PI = 3.141592653589793238d0)
      double precision ALFA, ALFAS, FACQCD, FACQED, dens
      COMMON/ ADHOC  / ALFA, ALFAS, FACQCD, FACQED, dens(-1:1,-1:1)
      double precision TOT, FAC, RM
      double precision DFREAL, FREAL1
      external DFREAL, FREAL1
      integer I

      TOT = 0
      FAC = 1
      DO 10 I = 1 , NRMASS
         IF(I .EQ. 4) FAC = 3
         RM = RMASS(I)
         TOT = TOT - 4D0/3D0*QF(I)*VF(I)*( + freal1(S,RM) +
     +         ( S+2*RMASS2(I) )*DFREAL(S,RM,RM) - 1.d0/3 )  *  FAC
   10 CONTINUE
      TOT = TOT + 1/(CW*SW)*(   ( 3*CW2 + 1D0/6D0 )*freal1(S,RMW)
     &                      + ( ( 3*CW2 + 1D0/6D0 )*S
     +                         +( 4*CW2 + 4D0/3D0 )*RMW**2 )*
     +                            DFREAL(S,RMW,RMW)
     &                      +  1.d0/9 )
      DUSGGZ = ALFA/(4*PI) * TOT

      end
CDECK  ID>, USIGZ.  
*CMZ :  2.01/08 14/11/95  17.24.59  by  Harald Anlauf
*CMZ :  2.01/04 20/03/95  15.11.34  by  Harald Anlauf
*CMZ :  2.01/02 31/01/95  01.11.32  by  Harald Anlauf
*CMZ :  1.99/00 06/05/93  12.41.28  by  Harald Anlauf & Helge Meinhard
*-- Author :
      FUNCTION USIGZ(QSQR)
*     --------------
* W.J.P. Beenakker, F.A.Berends and S.C. van der Marck, June 14th 1989
* Real part of the unrenormalized weakly corrected Z propagator,
* for QSQR > 0.
* eq. (B.4) ref 2b. 1 erratum in the pole part, not apparent here.
      IMPLICIT double precision (A-H,O-Z)
      integer nrmass
      PARAMETER ( NRMASS = 9 )
      double precision RMASS, RMASS2, VF, AF, QF
      COMMON/ MASSES / RMASS(0:NRMASS), RMASS2(0:NRMASS),
     &          VF(0:NRMASS), AF(0:NRMASS), QF(0:NRMASS)
      double precision t3f, mq2mz
      common/ masses / t3f(0:NRMASS), mq2mz(0:NRMASS)
      double precision RMZ,RMW,RMH,RMT, ZWID, SIN2TH, sw, cw, cw2, pizmz
      COMMON/ BOSONS / RMZ,RMW,RMH,RMT, ZWID, SIN2TH, sw, cw, cw2, pizmz
      double precision HBARC2, PI
      parameter (HBARC2 = 0.38937966d9, PI = 3.141592653589793238d0)
      double precision ALFA, ALFAS, FACQCD, FACQED, dens
      COMMON/ ADHOC  / ALFA, ALFAS, FACQCD, FACQED, dens(-1:1,-1:1)
      S   = QSQR
      TOT = 0D0
      FAC = 1D0
      DO 10 I = 1 , NRMASS
         IF(I .EQ. 4) FAC = 3D0
         IF(I .LE. 3) TOT = TOT + 4D0/3D0* 2D0*AF(I)**2*S*(
     +                    + 5D0/3D0 - LOG(ABS( S/RMASS2(I) )) ) * FAC
         RM = RMASS(I)
         F = freal1(S,RM)
         TOT = TOT + 4D0/3D0*(  ( VF(I)**2+AF(I)**2 )*(
     +                         + ( S+2D0*RMASS2(I) )*F - S/3D0 )
     +             - 3D0/8D0/(SIN2TH*CW2)*RMASS2(I)*F ) * FAC
   10 CONTINUE
      W = RMW**2
      Z = RMZ**2
      H = RMH**2
      TOT = TOT + ( ( -CW2**2*40D0*(S+2*W) + 12D0*W +
     1              (CW2-SIN2TH)**2*( 8D0*W+S ) )*freal1(S,RMW) +
     2            ( 10D0*Z - 2D0*H + S + (H-Z)**2/S )*FREAL(S,RMH,RMZ)-
     3            2D0*H*LOG(H/W) - 2D0*Z*LOG(Z/W) +
     4            ( 10D0*Z - 2D0*H + S )*( 1D0 - (H+Z)/(H-Z)*
     5              LOG(RMH/RMZ) - LOG(RMH*RMZ/W) ) +
     6            2D0/3D0*S*( 1D0 + (CW2-SIN2TH)**2 - 4D0*CW2**2 )
     7  )/(12*CW2*SIN2TH)
      USIGZ = ALFA/(4D0*PI) * TOT
      END
CDECK  ID>, USIGW.  
*CMZ :  2.01/08 14/11/95  17.25.45  by  Harald Anlauf
*CMZ :  1.99/00 06/05/93  12.41.28  by  Harald Anlauf & Helge Meinhard
*-- Author :
      FUNCTION USIGW(QSQR)
*     --------------
* W.J.P. Beenakker, F.A.Berends and S.C. van der Marck, June 14th 1989
* Real part of the unrenormalized weakly corrected W propagator,
* for QSQR >= 0.
* eq. (B.5) ref 2b with errata: a factor 3 for the last 7 lines,
*                               one factor s, one factor 1/s and a sign
      IMPLICIT double precision (A-H,O-Z)
      integer nrmass
      PARAMETER ( NRMASS = 9 )
      double precision RMASS, RMASS2, VF, AF, QF
      COMMON/ MASSES / RMASS(0:NRMASS), RMASS2(0:NRMASS),
     &          VF(0:NRMASS), AF(0:NRMASS), QF(0:NRMASS)
      double precision t3f, mq2mz
      common/ masses / t3f(0:NRMASS), mq2mz(0:NRMASS)
      double precision MP,MM
      double precision RMZ,RMW,RMH,RMT, ZWID, SIN2TH, sw, cw, cw2, pizmz
      COMMON/ BOSONS / RMZ,RMW,RMH,RMT, ZWID, SIN2TH, sw, cw, cw2, pizmz
      double precision HBARC2, PI
      parameter (HBARC2 = 0.38937966d9, PI = 3.141592653589793238d0)
      double precision ALFA, ALFAS, FACQCD, FACQED, dens
      COMMON/ ADHOC  / ALFA, ALFAS, FACQCD, FACQED, dens(-1:1,-1:1)
      S   = QSQR
      W   = RMW**2
      Z   = RMZ**2
      H   = RMH**2
      TOT = 0D0
      IF(ABS(S) .GT. 1D-10) THEN
         DO 10 I = 1 , NRMASS
            RM  = RMASS (I)
            RM2 = RMASS2(I)
            IF(I .LE. 3) THEN
              TOT = TOT + ( S-RM2/2D0-RM2**2/2D0/S )*FREAL(S,0D0,RM) +
     +                     2D0/3D0*S - RM2/2D0
            ELSE
               FAC = 3D0
               IF(MOD(I,2) .EQ. 0) THEN
                  MP  = RMASS(I)
                  MM  = RMASS(I+1)
                  SUM = RMASS2(I) + RMASS2(I+1)
                  DIF = RMASS2(I) - RMASS2(I+1)
                  TOT = TOT + (
     +               (S-SUM/2D0-DIF**2/2D0/S)*FREAL(S,MP,MM)+
     +               (S-SUM/2D0)*(1D0-SUM/DIF*LOG(MP/MM))-S/3D0) * FAC
               ENDIF
            ENDIF
   10    CONTINUE
         TOT = TOT + 3D0*(  ( SIN2TH**2*Z - CW2/3D0*( 7D0*Z + 7D0*W +
     1                  10D0*S - 2D0*(Z-W)**2/S ) - 1D0/6D0*(W+Z-S/2D0-
     2              (Z-W)**2/2D0/S ) )*FREAL(S,RMZ,RMW) +
     3       SIN2TH/3D0*( -4D0*W-10D0*S+2D0*W**2/S )*FREAL(S,0D0,RMW) +
     4       1D0/6D0*( 5D0*W-H+S/2D0+(H-W)**2/2D0/S )*FREAL(S,RMH,RMW)+
     5       ( CW2/3D0*( 7D0*Z+7D0*W+10D0*S-4D0*(Z-W) ) - SIN2TH**2*Z +
     6         1D0/6D0*( 2D0*W - S/2D0 ) ) * Z/(Z-W)*LOG(Z/W) -
     7       ( 2D0/3D0*W + S/12D0 ) * H/(H-W)*LOG(H/W) -
     8       CW2/3D0*( 7D0*Z + 7D0*W + 32D0/3D0*S ) + SIN2TH**2*Z +
     9       1D0/6D0*( 5D0/3D0*S + 4D0*W - Z - H ) -
     1       SIN2TH/3D0*( 4D0*W + 32D0/3D0*S )  )
      ELSE
         DO 20 I = 1 , NRMASS
            RM  = RMASS (I)
            RM2 = RMASS2(I)
            IF(I .LE. 3) THEN
              TOT = TOT - 3D0/4D0 * RM2
            ELSE
               FAC = 3D0
               IF(MOD(I,2) .EQ. 0) THEN
                  MP  = RMASS(I)
                  MM  = RMASS(I+1)
                  SUM = RMASS2(I) + RMASS2(I+1)
                  DIF = RMASS2(I) - RMASS2(I+1)
                  TOT = TOT -.5D0*(
     +                      3D0/2D0*SUM - LOG(MP/MM)/DIF*
     +                      (SUM**2+2D0*RMASS2(I)*RMASS2(I+1)) ) * FAC
               ENDIF
            ENDIF
   20    CONTINUE
         TOT = TOT +3D0*( (2D0/3D0*CW2+1D0/12D0)*(.5D0*(W+Z)-W*Z/(W-Z)*
     1               LOG(W/Z) ) + W/3D0*SIN2TH + 1D0/12D0*( .5D0*
     2               (H+W)-H*W/(W-H)*LOG(W/H) ) + ( CW2/3D0*( 3D0*Z+
     3               11D0*W ) - SIN2TH**2*Z + 1D0/3D0*W )*Z/(Z-W)*
     4               LOG(Z/W) - 2D0/3D0*W*H/(H-W)*LOG(H/W) -
     5               1D0/3D0*CW2*(7D0*Z+7D0*W) + SIN2TH**2*Z + 1D0/6D0*
     6              (4D0*W-Z-H) - SIN2TH/3D0*4D0*W  )
      ENDIF
      USIGW = ALFA/4D0/PI /3D0/SIN2TH  * TOT
      END
CDECK  ID>, BIGPIZ. 
*CMZ :  2.01/08 14/11/95  23.04.17  by  Harald Anlauf
*CMZ :  2.01/04 20/03/95  15.11.34  by  Harald Anlauf
*CMZ :  1.99/00 06/05/93  12.41.28  by  Harald Anlauf & Helge Meinhard
*-- Author :
      FUNCTION BIGPIZ(QSQR)
*     ---------------
* W.J.P. Beenakker, F.A.Berends and S.C. van der Marck, June 14th 1989
* Derivative of the real part of the renormalized
* weakly corrected Z propagator, for QSQR > 0.
      IMPLICIT double precision (A-H,O-Z)
      double precision PIGAM0,SIGGZ0,DELMZ,DELMW,DELZ2Z,DELZ2G,DELZ2W,
     &                 SUMQ1,SUMQ2
      COMMON/ RENORM / PIGAM0,SIGGZ0,DELMZ,DELMW,DELZ2Z,DELZ2G,DELZ2W,
     &                 SUMQ1,SUMQ2
      integer nrmass
      PARAMETER ( NRMASS = 9 )
      double precision RMASS, RMASS2, VF, AF, QF
      COMMON/ MASSES / RMASS(0:NRMASS), RMASS2(0:NRMASS),
     &          VF(0:NRMASS), AF(0:NRMASS), QF(0:NRMASS)
      double precision t3f, mq2mz
      common/ masses / t3f(0:NRMASS), mq2mz(0:NRMASS)
      double precision RMZ,RMW,RMH,RMT, ZWID, SIN2TH, sw, cw, cw2, pizmz
      COMMON/ BOSONS / RMZ,RMW,RMH,RMT, ZWID, SIN2TH, sw, cw, cw2, pizmz
      double precision HBARC2, PI
      parameter (HBARC2 = 0.38937966d9, PI = 3.141592653589793238d0)
      double precision ALFA, ALFAS, FACQCD, FACQED, dens
      COMMON/ ADHOC  / ALFA, ALFAS, FACQCD, FACQED, dens(-1:1,-1:1)
      S   = QSQR
      TOT = 0D0
      FAC = 1D0
      DO 10 I = 1 , NRMASS
         IF(I .EQ. 4) FAC = 3D0
         IF(I .LE. 3) TOT = TOT + 4D0/3D0* 2*AF(I)**2*(
     +                    + 2D0/3D0 - LOG(ABS( S/RMASS2(I) )) )
         RM = RMASS(I)
         F  = freal1(S,RM)
         DF = DFREAL(S,RM,RM)
         TOT = TOT + 4D0/3D0*(  ( VF(I)**2+AF(I)**2 )*(
     +                     F  + ( S+2*RMASS2(I) )*DF - 1D0/3D0 )
     +             - 3D0/8/(SIN2TH*CW2)*RMASS2(I)*DF ) * FAC
   10 CONTINUE
      W = RMW**2
      Z = RMZ**2
      H = RMH**2
      TOT = TOT + ( ( -CW2**2*(40D0*S+80D0*W) + 12D0*W +
     1                (CW2-SIN2TH)**2*( 8D0*W+S ) )*DFREAL(S,RMW,RMW) +
     2               (-40D0*CW2**2+(CW2-SIN2TH)**2 )*freal1(S,RMW) +
     3            ( 10D0*Z-2D0*H+S+(H-Z)**2/S )*DFREAL(S,RMH,RMZ) +
     4            (        1D0 - (H-Z)**2/S**2 )*FREAL(S,RMH,RMZ) +
     5            ( 1D0-(H+Z)/(H-Z)*LOG(RMH/RMZ)-LOG(RMH*RMZ/W) ) +
     6            2D0/3D0*( 1D0 + (CW2-SIN2TH)**2 - 4D0*CW2**2 )
     7  )/(12*CW2*SIN2TH)
      BIGPIZ = TOT * ALFA/(4*PI) + DELZ2Z +
     +                    (CW2-SIN2TH)/SIN2TH*SUMQ1 + SUMQ2/3D0/SIN2TH
      END
CDECK  ID>, BIGPZ2. 
*CMZ :  2.01/08 29/11/95  15.38.47  by  Harald Anlauf
*-- Author :    Harald Anlauf   14/11/95

      function bigpz2 (s)
c     ---------------
c Contribution to \Pi^Z from gamma-Z mixing propagator for s > 0
c See CERN 95-03, p.74, eq. (160)
      implicit none
      double precision bigpz2, s
      integer nrmass
      PARAMETER ( NRMASS = 9 )
      double precision RMZ,RMW,RMH,RMT, ZWID, SIN2TH, sw, cw, cw2, pizmz
      COMMON/ BOSONS / RMZ,RMW,RMH,RMT, ZWID, SIN2TH, sw, cw, cw2, pizmz
      double precision PIGAM0,SIGGZ0,DELMZ,DELMW,DELZ2Z,DELZ2G,DELZ2W,
     &                 SUMQ1,SUMQ2
      COMMON/ RENORM / PIGAM0,SIGGZ0,DELMZ,DELMW,DELZ2Z,DELZ2G,DELZ2W,
     &                 SUMQ1,SUMQ2

      double precision SIGG, IMSIGG, DUSIGG, SIGGZ, DUSGGZ, USIGGZ,
     &                 IMSGGZ
      external SIGG, IMSIGG, DUSIGG, SIGGZ, DUSGGZ, USIGGZ, IMSGGZ
      double complex   dsigg, dsiggz, rsigg, rsiggz

c Renormalized self-energies:
      rsigg  = dcmplx (SIGG(s),  IMSIGG(s))
      rsiggz = dcmplx (SIGGZ(s), IMSGGZ(s))

c Derivatives of the renormalized self-energies (see 153):
      dsigg  = dcmplx (DUSIGG (s) + delZ2g, IMSIGG (s)/s)
      dsiggz = dcmplx (DUSGGZ (s) - cw*sw/(cw2 - sin2th) *
     &                             (delZ2Z - delZ2g) +
     &                 ( - CW/SW*SUMQ1 - SUMQ2/(6*CW*SW) ),
     &                 IMSGGZ (s)/s)
c (Here we implicitly assumed that the derivative of the imaginary part
c  is constant because of massless fermions...):

c Calculate: - d/ds Re (rsiggz(s)^2/(s + rsigg(s)))
      bigpz2 = - dble ( (2 * dsiggz * (s+rsigg) -
     &                    (1 + dsigg) * rsiggz) * rsiggz /
     &                 (s+rsigg)**2 )

      end
CDECK  ID>, FREAL.  
*CMZ :  2.01/02 31/01/95  00.14.04  by  Harald Anlauf
*CMZ :  1.99/00 04/05/93  09.54.03  by  Harald Anlauf & Helge Meinhard
*-- Author :
      FUNCTION FREAL(S,RM1,RM2)
*     --------------
* W.J.P. Beenakker, F.A.Berends and S.C. van der Marck, June 14th 1989
* Real part of the function F(s,ma,mb), eq. (B.6) ref 2b.
      IMPLICIT double precision (A-H,O-Z)
      PARAMETER( EPS = 1D-10 )
      IF(RM1 .LT. RM2) THEN
        RMA = RM1
        RMB = RM2
      ELSE
        RMA = RM2
        RMB = RM1
      ENDIF
      RMA2 = RMA**2
      RMB2 = RMB**2
      IF(ABS(S) .LT. EPS) THEN
        F = 0D0
      ELSEIF(S .GT. 0D0) THEN
        IF(RMA2 .LT. EPS .AND. RMB2 .LT. EPS) THEN
          WRITE(*,*)'FREAL: ma = mb = 0 cannot be handled !'
          F = 0D0
        ELSEIF(RMA2 .LT. EPS) THEN
          F = - 1D0 + LOG( RMB2 )
        ELSEIF(ABS(RMA2-RMB2) .LT. EPS) THEN
          F =         LOG( RMB2 )
        ELSE
          F = - 1D0 + ( RMA2*LOG(RMA2)-RMB2*LOG(RMB2) )/(RMA2-RMB2)
        ENDIF
        F = - F
        IF(RMA2 .LT. EPS) THEN
          IF(ABS(S-RMB2) .LT. EPS) THEN
            F = F + LOG(S) - 2D0
          ELSE
            tmp = RMB2/S
            F = F + LOG(S) - 1D0 + tmp*(LOG(tmp) - 1) -
     +        ( (tmp-1D0)*(LOG(ABS(tmp-1D0)) - 1) )
          ENDIF
        ELSE
          S0   = - .5D0*( 1D0 + (RMA2 - RMB2)/S )
          S1   =   .5D0*( 1D0 - (RMA2 - RMB2)/S )
          DISCR= ( (S+RMA2-RMB2)**2 - 4D0*RMA2*S ) / (4*S**2)
          ROOTD= SQRT( ABS( DISCR ) )
          F = F + LOG(S) + S1*(LOG( RMB2/S ) - 2) -
     +                     S0*(LOG( RMA2/S ) - 2)
          IF(DISCR .GE. 0D0) THEN
            IF(S.LT.RMA2 .OR. S.LT.RMB2) THEN
              F = F + ROOTD*( LOG( (S1+ROOTD)**2*S/RMB2 )
     +                      - LOG( (S0+ROOTD)**2*S/RMA2 ) )
            ELSE
              F = F + ROOTD*( LOG( (S1+ROOTD)**2*S/RMB2 )
     +                      - LOG( RMA2/(S*(S0-ROOTD)**2) ) )
            ENDIF
          ELSE
            F = F + 2D0*ROOTD*( DATAN(S1/ROOTD) - DATAN(S0/ROOTD) )
          ENDIF
        ENDIF
      ELSE
        IF(RMA2 .LT. EPS .AND. RMB2 .LT. EPS) THEN
          WRITE(*,*)'FREAL: ma = mb = 0 cannot be handled !'
          F = 0D0
        ELSEIF(RMA2 .LT. EPS) THEN
          F = - 1D0 - ( 1D0-RMB2/S )*LOG( RMB2/(RMB2-S) )
        ELSE
          IF(ABS(RMA2-RMB2) .LT. EPS) THEN
            F = - 2D0
          ELSE
            F = - 1D0 - ( (RMA2-RMB2)/S - (RMA2+RMB2)/(RMA2-RMB2) )*
     +                  .5D0*LOG(RMB2/RMA2)
          ENDIF
          ROOTA = SQRT( (RMA+RMB)**2 - S )
          ROOTB = SQRT( (RMA-RMB)**2 - S )
          F = F - ROOTA*ROOTB/S*LOG( (ROOTA+ROOTB)**2/(4*RMA*RMB) )
        ENDIF
      ENDIF
      FREAL = - F
      END
CDECK  ID>, FREAL1. 
*CMZ :  2.01/04 20/03/95  14.57.16  by  Harald Anlauf
*-- Author :    Harald Anlauf   20/03/95

c FREAL1 - Real part of the function F(s,ma,mb);
c          much faster version for equal masses.

      function freal1 (s, m)
      implicit none
      double precision freal1, s, m

      double precision EPS, m2, x, absx, rootx, rootx1
      parameter (EPS = 1.d-10)

      m2 = m*m
      if (m2 .lt. EPS) then
         print *, 'FREAL1: m=0 not yet implemented'
         stop
      endif

      x = s / (4*m2)
      absx = abs (x)
      if (absx .lt. EPS) then
         freal1 = (2.d0/3.d0) * x
         return
      endif

      rootx  = sqrt (absx)
      rootx1 = sqrt (abs (x - 1))

      if (x .lt. 0) then
         freal1 = 2 - 2 * rootx1/rootx * log (rootx + rootx1)
      else if (x .lt. 1) then
         freal1 = 2 - 2 * rootx1/rootx * atan (rootx / rootx1)
      else
         freal1 = 2 - 2 * rootx1/rootx * log (rootx + rootx1)
      endif
      end
CDECK  ID>, FIMAG.  
*CMZ :  1.99/00 04/05/93  09.54.03  by  Harald Anlauf & Helge Meinhard
*-- Author :
      FUNCTION FIMAG(S,RMA,RMB)
*     --------
* Imaginary part of the function F(s,ma,mb)
      IMPLICIT double precision (A-H,O-Z)
      PARAMETER (PI=3.1415926535897932D0)
      FIMAG = 0D0
      IF(S.GT.(RMA+RMB)**2) FIMAG=PI*SQRT((S-(RMA+RMB)**2)*
     +                                     (S-(RMA-RMB)**2))/S
      END
CDECK  ID>, DFREAL. 
*CMZ :  1.99/00 04/05/93  09.54.03  by  Harald Anlauf & Helge Meinhard
*-- Author :
      FUNCTION DFREAL(S,RM1,RM2)
*     ---------------
* W.J.P. Beenakker, F.A.Berends and S.C. van der Marck, June 14th 1989
* Derivative of the real part of the function F(s,ma,mb).
      IMPLICIT double precision (A-H,O-Z)
      EPS = 1D-10
      IF(RM1 .LT. RM2) THEN
         RMA = RM1
         RMB = RM2
      ELSE
         RMA = RM2
         RMB = RM1
      ENDIF
      RMA2 = RMA**2
      RMB2 = RMB**2
      DIFF = RMB2 - RMA2
      IF(S.LT.EPS .AND. RMA2.LT.EPS .AND. RMB2.LT.EPS) THEN
         WRITE(*,*)'DFREAL: S = Ma = Mb = 0 cannot be handled !'
         F = 0D0
      ELSEIF(RMA2.LT.EPS .AND. RMB2.LT.EPS) THEN
         F = 1D0/S
      ELSEIF(ABS(S).LT.EPS .AND. ABS(RMA2-RMB2).LT.EPS) THEN
         F = - 1D0/6D0/RMA2
      ELSEIF(RMA2 .LT. EPS) THEN
         F = 1D0/S*( 1D0 - RMB2/S*LOG( RMB2/ABS(RMB2-S) ) )
      ELSEIF(ABS(S) .LT. EPS) THEN
         F = 1D0/DIFF*( .5D0 - RMB2/DIFF -
     +                  RMA2*RMB2/DIFF**2*LOG(RMB2/RMA2) )
      ELSEIF(S .LT. 0D0) THEN
         A = (RMA+RMB)**2 - S
         B = (RMA-RMB)**2 - S
         ROOTA = SQRT( A )
         ROOTB = SQRT( B )
         F = .5D0*(RMA2-RMB2)/S**2*LOG(RMB2/RMA2) + ROOTA*ROOTB/S*( (
     +       1D0/2D0/A + 1D0/2D0/B + 1D0/S )*
     +       LOG( (ROOTA+ROOTB)**2/4D0/RMA/RMB ) + 1D0/ROOTA/ROOTB )
      ELSE
         DISCR = - ( ( S + RMA2 - RMB2 )**2 - 4D0*RMA2*S )/4D0/S**2
         ROOTD = SQRT( ABS( DISCR ) )
         SP = (   S - RMA2 + RMB2 )/2D0/S
         SM = ( - S - RMA2 + RMB2 )/2D0/S
         IF(ROOTD .LT. EPS) THEN
            F = - ( 1D0/SP - 1D0/SM ) / S
         ELSEIF(DISCR .LT. 0D0) THEN
            IF(S.LT.RMA2 .OR. S.LT.RMB2) THEN
               F =-.5D0/S/ROOTD*LOG( (ROOTD+SP)**2/RMB2
     +                               /(ROOTD+SM)**2*RMA2 )
            ELSE
               F =-.5D0/S/ROOTD*( LOG( (ROOTD+SP)**2*S/RMB2 )
     +                          - LOG( RMA2/S/(SM-ROOTD)**2 ) )
            ENDIF
         ELSE
            F = 1D0/S/ROOTD*( DATAN(SP/ROOTD) - DATAN(SM/ROOTD) )
         ENDIF
         F = F * ( - S*(RMA2+RMB2) + DIFF**2 )/2D0/S**2 +
     +         1D0/S - DIFF/2D0/S**2*LOG(RMB2/RMA2)
      ENDIF
      DFREAL = - F
      END
CDECK  ID>, PWIDTH. 
*CMZ :  2.01/08 23/11/95  15.23.35  by  Harald Anlauf
*CMZ :  1.99/07 02/06/93  19.28.00  by  Harald Anlauf & Helge Meinhard
*CMZ :  1.99/00 06/05/93  12.41.28  by  Harald Anlauf & Helge Meinhard
*-- Author :
      FUNCTION PWIDTH(I)
*     ---------------
* W.J.P. Beenakker, F.A.Berends and S.C. van der Marck, June 14th 1989
* The partial width of the Z due to fermion i.
* Fermionic 2 loop effects have been taken into account using
* eq. (5.18) ref 2b and simple QED and QCD correction factors.
      IMPLICIT double precision (A-H,O-Z)
      double precision pwidth
      integer i
      integer nrmass
      PARAMETER ( NRMASS = 9 )
      double precision RMASS, RMASS2, VF, AF, QF
      COMMON/ MASSES / RMASS(0:NRMASS), RMASS2(0:NRMASS),
     &          VF(0:NRMASS), AF(0:NRMASS), QF(0:NRMASS)
      double precision t3f, mq2mz
      common/ masses / t3f(0:NRMASS), mq2mz(0:NRMASS)
      double precision RMZ,RMW,RMH,RMT, ZWID, SIN2TH, sw, cw, cw2, pizmz
      COMMON/ BOSONS / RMZ,RMW,RMH,RMT, ZWID, SIN2TH, sw, cw, cw2, pizmz
      COMPLEX*16 FZV,FZA,FGV,FGA
      COMMON/ FORMFA /FZV(0:NRMASS),FZA(0:NRMASS),
     +                FGV(0:NRMASS),FGA(0:NRMASS)
      double precision HBARC2, PI
      parameter (HBARC2 = 0.38937966d9, PI = 3.141592653589793238d0)
      double precision ALFA, ALFAS, FACQCD, FACQED, dens
      COMMON/ ADHOC  / ALFA, ALFAS, FACQCD, FACQED, dens(-1:1,-1:1)
      integer NC, NF
      parameter (NC = 3, NF = 5)
      double precision beta0, beta1, beta2, gamm0, gamm1, gamm2, lamqcd
      common /qcdcom/  beta0, beta1, beta2, gamm0, gamm1, gamm2, lamqcd

      double precision rvq0, rvqm, raq0, raqm, raqi, rmixed
      common /qcdcom/  rvq0, rvqm, raq0, raqm, raqi, rmixed
      double precision RV, RA, RQED, TOT, tmp, FAC, Z
      double precision SIGG, IMSIGG, SIGGZ, IMSGGZ, BIGPIZ, bigpz2
      double complex   GZMIX
      Z = RMZ**2
*      GZMIX = SIGGZ (Z) / Z
      GZMIX = dcmplx (SIGGZ (Z), IMSGGZ (Z)) /
     &        dcmplx (Z + SIGG (Z), IMSIGG (Z))
      CALL FORMFS (Z,I)
      TOT = 0
      if (i .le. 3) then
c Leptonic final state:
         IF (I .EQ. 0) THEN
            FAC = 1
         ELSE
            FAC = 1 * ( 1D0 + FACQED*QF(I)**2 )
         endif
         IF (Z .GT. 4*RMASS2(I)) THEN
            tmp = RMASS2(I)/Z
*            TOT = TOT + SQRT(1-4*tmp)*
*     +           ( ( VF(I)**2 + AF(I)**2 )*( 1 + 2*tmp ) -
*     +                        6*AF(I)**2 * tmp ) * FAC/3
*            TOT = TOT + FAC * 2D0/3D0*(
*     +                  VF(I)*( DBLE(FZV(I)) + QF(I)*GZMIX )
*     +                + AF(I)*  DBLE(FZA(I)) )
c Use effective couplings (`a la (174,175)):
            TOT = TOT + FAC/3 * SQRT (1-4*tmp) *
     &           (abs (VF(I)+FZV(I) + (QF(I)+FGV(I))*GZMIX)**2
     &            * (1+2*tmp)
     &           +abs (AF(I)+FZA(I) +        FGA(I) *GZMIX)**2
     &            * (1-4*tmp) )
         ENDIF
      else
c Hadronic final state:
c Update: CERN 95-03, (51ff)
         IF (Z .GT. 4*RMASS2(I)) THEN
            tmp  = mq2mz(I)/Z
            RQED = FACQED*QF(I)**2 * rmixed
            RV   = NC * (RQED + rvq0 + rvqm * tmp)
            RA   = NC * (RQED + raq0 + raqm * tmp + 2*t3f(i) * raqi)
*            FAC2 = NC * (rqed + rvq0)
*            TOT = TOT + ( VF(I)**2 * RV + AF(I)**2 * RA ) / 3
*            TOT = TOT + FAC2 * 2D0/3D0*(
*     +                  VF(I)*( DBLE(FZV(I)) + QF(I)*GZMIX )
*     +                + AF(I)*  DBLE(FZA(I)) )
            TOT = TOT +
     &           (abs (VF(I)+FZV(I) + (QF(I)+FGV(I))*GZMIX)**2 * RV
     &           +abs (AF(I)+FZA(I) +        FGA(I) *GZMIX)**2 * RA)/3
         ENDIF
      endif
      PWIDTH = TOT*ALFA*Z/( 1 + BIGPIZ(Z) + bigpz2(z) )/ RMZ
      END
CDECK  ID>, IMSIGZ. 
*CMZ :  2.01/08 14/11/95  22.38.58  by  Harald Anlauf
*CMZ :  1.99/00 06/05/93  12.41.28  by  Harald Anlauf & Helge Meinhard
*-- Author :
      FUNCTION IMSIGZ(S)
*     ---------------
* Imaginary part of the 1-loop Z self-energy
* Replaced pole mass by running mass @ M_Z.
      IMPLICIT double precision (A-H,O-Z)
      double precision IMSIGZ, S
      integer nrmass
      PARAMETER ( NRMASS = 9 )
      double precision RMASS, RMASS2, VF, AF, QF
      COMMON/ MASSES / RMASS(0:NRMASS), RMASS2(0:NRMASS),
     &          VF(0:NRMASS), AF(0:NRMASS), QF(0:NRMASS)
      double precision t3f, mq2mz
      common/ masses / t3f(0:NRMASS), mq2mz(0:NRMASS)
      double precision RMZ,RMW,RMH,RMT, ZWID, SIN2TH, sw, cw, cw2, pizmz
      COMMON/ BOSONS / RMZ,RMW,RMH,RMT, ZWID, SIN2TH, sw, cw, cw2, pizmz
      double precision HBARC2, PI
      parameter (HBARC2 = 0.38937966d9, PI = 3.141592653589793238d0)
      double precision ALFA, ALFAS, FACQCD, FACQED, dens
      COMMON/ ADHOC  / ALFA, ALFAS, FACQCD, FACQED, dens(-1:1,-1:1)
      TOT = 0D0
      DO 10 I = 0 , NRMASS
         IF (I .EQ. 0) FAC = 3D0
         IF (I .EQ. 1) FAC = 1D0
         IF (I .EQ. 4) FAC = 3D0
         IF (S .GT. 4.D0*RMASS2(I)) THEN
            TOT = TOT + SQRT(1-4*mq2mz(I)/S)*
     &         ( ( VF(I)**2 + AF(I)**2 )*( S + 2*mq2mz(I) ) -
     &             6*mq2mz(I)*AF(I)**2 ) * FAC/3
         ENDIF
   10 CONTINUE
      IF (S .GT. 4D0*RMW**2) TOT = TOT + SQRT(1D0-4D0*RMW**2/S)*
     +       ((-10D0*S-20D0*RMW**2)*CW2**2+(2D0*RMW**2+S/4D0)*
     +             (CW2-SIN2TH)**2+3D0*RMW**2)*4.D0*AF(1)**2/3D0
      IF (S .GT. (RMH+RMZ)**2) TOT = TOT +
     +        (10D0*RMZ**2-2D0*RMH**2+S+(RMH**2-RMZ**2)**2/S)*AF(1)**2*
     +        SQRT((1D0-(RMZ-RMH)**2/S)*(1D0-(RMZ+RMH)**2/S))/3D0
      IMSIGZ = TOT * ALFA
      END
CDECK  ID>, IMZ2.   
*CMZ :  2.01/08 15/11/95  00.25.16  by  Harald Anlauf
*CMZ :  2.01/02 31/01/95  01.15.11  by  Harald Anlauf
*CMZ :  1.99/07 02/06/93  19.28.54  by  Harald Anlauf & Helge Meinhard
*CMZ :  1.99/00 06/05/93  12.41.28  by  Harald Anlauf & Helge Meinhard
*-- Author :
      FUNCTION IMZ2(S)
*     -------------
* W.J.P. Beenakker, F.A.Berends and S.C. van der Marck, June 14th 1989
* Imaginary part of the fermionic 2-loop Z self-energy.
* eq. (5.18) ref 2b and simple QED and QCD correction factors.
* Replaced pole mass by running mass @ M_Z.
* Note that this part is linearized around the Z pole.
      IMPLICIT double precision (A-H,O-Z)
      double precision IMZ2, S
      integer nrmass
      PARAMETER ( NRMASS = 9 )
      double precision RMASS, RMASS2, VF, AF, QF
      COMMON/ MASSES / RMASS(0:NRMASS), RMASS2(0:NRMASS),
     &          VF(0:NRMASS), AF(0:NRMASS), QF(0:NRMASS)
      double precision t3f, mq2mz
      common/ masses / t3f(0:NRMASS), mq2mz(0:NRMASS)
      COMPLEX*16 FZVMZ,FZAMZ,FGVMZ,FGAMZ
      COMMON/ FORMMZ /FZVMZ(0:NRMASS),FZAMZ(0:NRMASS),
     +                FGVMZ(0:NRMASS),FGAMZ(0:NRMASS)
      double precision RMZ,RMW,RMH,RMT, ZWID, SIN2TH, sw, cw, cw2, pizmz
      COMMON/ BOSONS / RMZ,RMW,RMH,RMT, ZWID, SIN2TH, sw, cw, cw2, pizmz
      double precision HBARC2, PI
      parameter (HBARC2 = 0.38937966d9, PI = 3.141592653589793238d0)
      double precision ALFA, ALFAS, FACQCD, FACQED, dens
      COMMON/ ADHOC  / ALFA, ALFAS, FACQCD, FACQED, dens(-1:1,-1:1)
      integer NC, NF
      parameter (NC = 3, NF = 5)
      double precision beta0, beta1, beta2, gamm0, gamm1, gamm2, lamqcd
      common /qcdcom/  beta0, beta1, beta2, gamm0, gamm1, gamm2, lamqcd

      double precision rvq0, rvqm, raq0, raqm, raqi, rmixed
      common /qcdcom/  rvq0, rvqm, raq0, raqm, raqi, rmixed
      double precision RV, RA, tmp, TOT, Z
      Z = RMZ**2
      GZMIX = SIGGZ (Z) / Z
      TOT = 0D0
c Leptonic intermediate states:
      DO 10 I = 0 , 3
         IF (S .GT. 4*RMASS2(I)) THEN
            IF (I .EQ. 0) THEN
               FAC1 = 0
               FAC2 = 3
            ELSE
               FAC1 = 1 * FACQED*QF(I)**2
               FAC2 = 1 + FAC1
            ENDIF
            tmp = RMASS2(I)/Z
            TOT = TOT + SQRT(1-4*tmp)*
     +         ( ( VF(I)**2 + AF(I)**2 )*( 1D0 + 2*tmp ) -
     +                  6*tmp*AF(I)**2 ) * FAC1/3
            TOT = TOT + FAC2 * 2D0/3D0*(
     +                  VF(I)*( DBLE(FZVMZ(I)) + QF(I)*GZMIX )
     +                + AF(I)*  DBLE(FZAMZ(I)) )
         ENDIF
 10   CONTINUE
c Hadronic intermediate states:
      DO 20 I = 4 , NRMASS
         IF (S .GT. 4*RMASS2(I)) THEN
c Update: CERN 95-03, (51ff)
            tmp = mq2mz(I)/Z
            fac1 = FACQED*QF(I)**2 * rmixed
            RV   = NC * (fac1 + rvq0-1 +  rvqm   *tmp)
            RA   = NC * (fac1 + raq0-1 + (raqm+6)*tmp + 2*t3f(i) * raqi)
            FAC2 = NC * (fac1 + rvq0)
            TOT = TOT + ( VF(I)**2 * RV + AF(I)**2 * RA )/3.D0
            TOT = TOT + FAC2 * 2D0/3D0*(
     +                  VF(I)*( DBLE(FZVMZ(I)) + QF(I)*GZMIX )
     +                + AF(I)*  DBLE(FZAMZ(I)) )
         ENDIF
 20   CONTINUE
      IMZ2 = TOT * ALFA * S
      END
CDECK  ID>, IMZ.
*CMZ :  2.01/08 23/11/95  15.23.35  by  Harald Anlauf
*-- Author :    Harald Anlauf   15/11/95

      function imz(s)
*     ------------
* Imaginary part of the Z self-energy, including fermionic 2-loop
* contributions using simple QED and QCD correction factors.
* Note that the higher-order part is linearized around the Z pole.
      implicit none
      double precision imz, s
      integer nrmass
      PARAMETER ( NRMASS = 9 )
      double precision RMASS, RMASS2, VF, AF, QF
      COMMON/ MASSES / RMASS(0:NRMASS), RMASS2(0:NRMASS),
     &          VF(0:NRMASS), AF(0:NRMASS), QF(0:NRMASS)
      double precision t3f, mq2mz
      common/ masses / t3f(0:NRMASS), mq2mz(0:NRMASS)
      COMPLEX*16 FZVMZ,FZAMZ,FGVMZ,FGAMZ
      COMMON/ FORMMZ /FZVMZ(0:NRMASS),FZAMZ(0:NRMASS),
     +                FGVMZ(0:NRMASS),FGAMZ(0:NRMASS)
      double precision RMZ,RMW,RMH,RMT, ZWID, SIN2TH, sw, cw, cw2, pizmz
      COMMON/ BOSONS / RMZ,RMW,RMH,RMT, ZWID, SIN2TH, sw, cw, cw2, pizmz
      double precision HBARC2, PI
      parameter (HBARC2 = 0.38937966d9, PI = 3.141592653589793238d0)
      double precision ALFA, ALFAS, FACQCD, FACQED, dens
      COMMON/ ADHOC  / ALFA, ALFAS, FACQCD, FACQED, dens(-1:1,-1:1)
      integer NC, NF
      parameter (NC = 3, NF = 5)
      double precision beta0, beta1, beta2, gamm0, gamm1, gamm2, lamqcd
      common /qcdcom/  beta0, beta1, beta2, gamm0, gamm1, gamm2, lamqcd

      double precision rvq0, rvqm, raq0, raqm, raqi, rmixed
      common /qcdcom/  rvq0, rvqm, raq0, raqm, raqi, rmixed
      double precision RV, RA, RQED, TOT, tmp, FAC
      double precision SIGG, IMSIGG, SIGGZ, IMSGGZ
      double complex   GZMIX
      integer i

      if (s .le. 0) then
         imz = 0
         return
      endif

      GZMIX = dcmplx (SIGGZ (S), IMSGGZ (S)) /
     &        dcmplx (s + SIGG (s), IMSIGG (s))
      TOT = 0
c Leptonic intermediate states:
      DO 10 I = 0 , 3
         IF (S .GT. 4*RMASS2(I)) THEN
            IF (I .EQ. 0) THEN
               FAC = 3
            ELSE
               FAC = 1 * (1 + FACQED*QF(I)**2)
            ENDIF
            tmp = RMASS2(I)/S
c Use effective couplings (`a la (174,175)):
            TOT = TOT + FAC/3 * SQRT (1-4*tmp) *
     &           (abs (VF(I)+FZVMZ(I) + (QF(I)+FGVMZ(I))*GZMIX)**2
     &            * (1+2*tmp)
     &           +abs (AF(I)+FZAMZ(I) +        FGAMZ(I) *GZMIX)**2
     &            * (1-4*tmp) )
c (Note that this includes a contribution from gamma-Z mixing!)
         ENDIF
 10   CONTINUE
c Hadronic intermediate states:
      DO 20 I = 4 , NRMASS
         IF (S .GT. 4*RMASS2(I)) THEN
            tmp = mq2mz(I)/S
c Update: CERN 95-03, (51ff)
            RQED = FACQED*QF(I)**2 * rmixed
            RV   = NC * (RQED + rvq0 + rvqm * tmp)
            RA   = NC * (RQED + raq0 + raqm * tmp + 2*t3f(i) * raqi)
            TOT = TOT +
     &           (abs (VF(I)+FZVMZ(I) + (QF(I)+FGVMZ(I))*GZMIX)**2 * RV
     &           +abs (AF(I)+FZAMZ(I) +        FGAMZ(I) *GZMIX)**2 * RA
     &           )/3
         ENDIF
 20   CONTINUE
      TOT = TOT * S
c Now the bosonic contribution up to one loop:
      IF (S .GT. 4*RMW**2) TOT = TOT + SQRT(1D0-4D0*RMW**2/S)*
     +       ((-10D0*S-20D0*RMW**2)*CW2**2+(2D0*RMW**2+S/4D0)*
     +             (CW2-SIN2TH)**2+3D0*RMW**2)*4.D0*AF(1)**2/3D0
      IF (S .GT. (RMH+RMZ)**2) TOT = TOT +
     +        (10D0*RMZ**2-2D0*RMH**2+S+(RMH**2-RMZ**2)**2/S)*AF(1)**2*
     +        SQRT((1D0-(RMZ-RMH)**2/S)*(1D0-(RMZ+RMH)**2/S))/3D0
      IMZ = TOT * ALFA
      end
CDECK  ID>, IMSIGG. 
*CMZ :  2.01/08 15/11/95  01.00.18  by  Harald Anlauf
*CMZ :  1.99/00 06/05/93  12.41.28  by  Harald Anlauf & Helge Meinhard
*-- Author :
      FUNCTION IMSIGG(S)
*     ---------------
* Imaginary part of the 1-loop QED vacuumpolarization
* Replaced pole mass by running mass @ M_Z, rudimentary QCD corrections.
      IMPLICIT double precision (A-H,O-Z)
      double precision IMSIGG, S
      integer nrmass
      PARAMETER ( NRMASS = 9 )
      double precision RMASS, RMASS2, VF, AF, QF
      COMMON/ MASSES / RMASS(0:NRMASS), RMASS2(0:NRMASS),
     &          VF(0:NRMASS), AF(0:NRMASS), QF(0:NRMASS)
      double precision t3f, mq2mz
      common/ masses / t3f(0:NRMASS), mq2mz(0:NRMASS)
      double precision RMZ,RMW,RMH,RMT, ZWID, SIN2TH, sw, cw, cw2, pizmz
      COMMON/ BOSONS / RMZ,RMW,RMH,RMT, ZWID, SIN2TH, sw, cw, cw2, pizmz
      double precision HBARC2, PI
      parameter (HBARC2 = 0.38937966d9, PI = 3.141592653589793238d0)
      double precision ALFA, ALFAS, FACQCD, FACQED, dens
      COMMON/ ADHOC  / ALFA, ALFAS, FACQCD, FACQED, dens(-1:1,-1:1)
      double precision FAC, TOT, tmp

      if (s .le. 0) then
         IMSIGG = 0
         return
      endif

      TOT = 0
      FAC = 1
      DO 10 I = 1 , NRMASS
         IF (I .EQ. 4) FAC = 3D0 * (1 + FACQCD)
         IF (S .GT. 4*RMASS2(I)) then
            tmp = mq2mz(i)/S
            TOT = TOT + FAC * QF(I)**2 * SQRT(1-4*tmp)*(1+2*tmp)/3D0
         endif
   10 CONTINUE
      IF (S .GT. 4*RMW**2) TOT = TOT -
     +         SQRT(1-4*RMW**2/S)*(3D0/4D0+RMW**2/S)
      IMSIGG = TOT * ALFA * S
      END
CDECK  ID>, IMSGGZ. 
*CMZ :  2.01/08 15/11/95  01.05.48  by  Harald Anlauf
*CMZ :  1.99/00 06/05/93  12.41.28  by  Harald Anlauf & Helge Meinhard
*-- Author :
      FUNCTION IMSGGZ(S)
*     ---------------
* Imaginary part of the 1-loop Z-gamma mixing
* Replaced pole mass by running mass @ M_Z, rudimentary QCD corrections.
      IMPLICIT double precision (A-H,O-Z)
      double precision IMSGGZ, S
      integer nrmass
      PARAMETER ( NRMASS = 9 )
      double precision RMASS, RMASS2, VF, AF, QF
      COMMON/ MASSES / RMASS(0:NRMASS), RMASS2(0:NRMASS),
     &          VF(0:NRMASS), AF(0:NRMASS), QF(0:NRMASS)
      double precision t3f, mq2mz
      common/ masses / t3f(0:NRMASS), mq2mz(0:NRMASS)
      double precision RMZ,RMW,RMH,RMT, ZWID, SIN2TH, sw, cw, cw2, pizmz
      COMMON/ BOSONS / RMZ,RMW,RMH,RMT, ZWID, SIN2TH, sw, cw, cw2, pizmz
      double precision HBARC2, PI
      parameter (HBARC2 = 0.38937966d9, PI = 3.141592653589793238d0)
      double precision ALFA, ALFAS, FACQCD, FACQED, dens
      COMMON/ ADHOC  / ALFA, ALFAS, FACQCD, FACQED, dens(-1:1,-1:1)
      double precision FAC, TOT, tmp

      if (s .le. 0) then
         IMSGGZ = 0
         return
      endif

      TOT = 0
      FAC = 1
      DO 10 I = 1 , NRMASS
         IF (I .EQ. 4) FAC = 3D0 * (1 + FACQCD)
         IF (S .GT. 4*RMASS2(I)) then
            tmp = mq2mz(i)/S
            TOT = TOT - FAC * QF(I)*VF(I) * SQRT(1-4*tmp)*(1+2*tmp)/3D0
         endif
   10 CONTINUE
      IF (S .GT. 4*RMW**2) TOT = TOT-AF(1)*SQRT(1D0-4*RMW**2/S)*
     +                ( (3D0*CW2+1D0/6D0)+RMW**2/S*(4D0*CW2+4D0/3D0) )
      IMSGGZ = TOT * ALFA * S
      END
CDECK  ID>, FLBOT.  
*CMZ :  2.01/08 14/11/95  17.20.54  by  Harald Anlauf
*CMZ :  1.99/00 06/05/93  12.41.28  by  Harald Anlauf & Helge Meinhard
*-- Author :
      SUBROUTINE FLBOT(QSQR,FZL9,FGL9)
*     ----------------
* W.J.P. Beenakker, F.A.Berends and S.C. van der Marck, June 14th 1989
* The left handed Z-bb and gamma-bb form factors
* see eqs (C.8),(C.9), (C.14) ff. of ref 2b.
* Take into account FTJR higher order corrections (see also CERN 95-03).
      IMPLICIT double precision (A-H,O-Z)
      integer nrmass
      PARAMETER ( NRMASS = 9 )
      double precision RMASS, RMASS2, VF, AF, QF
      COMMON/ MASSES / RMASS(0:NRMASS), RMASS2(0:NRMASS),
     &          VF(0:NRMASS), AF(0:NRMASS), QF(0:NRMASS)
      double precision t3f, mq2mz
      common/ masses / t3f(0:NRMASS), mq2mz(0:NRMASS)
      double precision RMZ,RMW,RMH,RMT, ZWID, SIN2TH, sw, cw, cw2, pizmz
      COMMON/ BOSONS / RMZ,RMW,RMH,RMT, ZWID, SIN2TH, sw, cw, cw2, pizmz
      double complex FI(1:7),GI(1:7),ZLFIN,TOT1,TOT2,FZL9,FGL9,HELP
      double complex C1,C2,C3,C4,C5,C6,C7,C8,C9,C10
      double complex B1BAR,C0SCAL,C1PLUS,C2ZERO,C2MIN,C2PLUS
      double precision flbot2
      external flbot2

      RT = RMASS(8)
      RB = RMASS(9)
      C1 = C0SCAL(QSQR,RT,RMW,RB)
      C2 = C1PLUS(QSQR,RT,RMW,RB)
      C3 = C2MIN (QSQR,RT,RMW,RB)
      C4 = C2ZERO(QSQR,RT,RMW,RB)
      C5 = C2PLUS(QSQR,RT,RMW,RB)
      C6 = C0SCAL(QSQR,RMW,RT,RB)
      C7 = C1PLUS(QSQR,RMW,RT,RB)
      C8 = C2MIN (QSQR,RMW,RT,RB)
      C9 = C2ZERO(QSQR,RMW,RT,RB)
      C10= C2PLUS(QSQR,RMW,RT,RB)
* (C.7): (added missing term m_b^2 \bar{B_1}(...), [ha])
      ZLFIN = 1/(2*SIN2TH)*( 2 + (RT/RMW)**2 )*(
     &         B1BAR(RB**2,RT,RMW) -
     &         (RT**2-RMW**2)/(2*RB**2) * FREAL (RB**2,RT,RMW) +
     &         (RT**2-RMW**2+RB**2)/2   * DFREAL(RB**2,RT,RMW) )
* last terms in (C.6) and (C.13)
      FI(1) = (2.d0/3*SIN2TH-1)/(4*CW*SW) * ZLFIN
      GI(1) = -1.d0/6 * ZLFIN
* {..} in (C.8), F_b; (C.14), G_b:
      HELP  = -1.5D0 + 2D0*LOG(RMW/RT) + 4D0*C4 - 2D0*QSQR*(C5 - C3)
     +        + 4D0*QSQR*(C2-0.5D0*C1)
* F_b, G_b:
      FI(2) = (VF(8)+AF(8))/(4*SIN2TH)*( HELP )
     +      - (VF(8)-AF(8))/(4*SIN2TH)*2D0*RT**2*C1
      GI(2) =  1/(6*SIN2TH)*( HELP - 2D0*RT**2*C1 )
* F_c, G_c:
      HELP = -1.5D0 + 12D0*C9 - 2D0*QSQR*( C10 - C8 ) + 4D0*QSQR*C7
      FI(3) = -CW/(4*SIN2TH*SW) * HELP
      GI(3) = - 1/(4*SIN2TH)    * HELP
* F_d, G_d:
      HELP = (RT/RMW)**2*( -0.75D0 + LOG(RMW/RT) + 2D0*C4 -
     +       QSQR*( C5 - C3 ) )
      FI(4) = (VF(8)-AF(8))/(4*SIN2TH)*HELP -
     +        (VF(8)+AF(8))/(4*SIN2TH)*RT**4/RMW**2*C1
      GI(4) = 1/(6*SIN2TH)*( HELP - RT**4/RMW**2*C1 )
* F_e, G_e:
      HELP  = (RT/RMW)**2*( -0.25D0 + 2D0*C9 )
      FI(5) = (SIN2TH-CW2)/(8*SIN2TH*SW*CW)*HELP
      GI(5) = - 1/(4*SIN2TH)*HELP
* F_f, G_f, F_g, G_g:
      FI(6) = - RT**2/(4*SW*CW) *C6
      GI(6) =   RT**2/(4*SIN2TH)*C6
      FI(7) = FI(6)
      GI(7) = GI(6)
      TOT1 = 0D0
      TOT2 = 0D0
      DO 10 I = 1 , 7
         TOT1 = TOT1 + FI(I)
         TOT2 = TOT2 + GI(I)
   10 CONTINUE
c Don't forget the FTJR higher order corrections
      FZL9 = TOT1 + flbot2 () / (2*SW*CW)
      FGL9 = TOT2
      END
CDECK  ID>, B0BAR.  
*CMZ :  2.01/08 11/11/95  00.11.13  by  Harald Anlauf
*CMZ :  1.99/00 04/05/93  09.54.03  by  Harald Anlauf & Helge Meinhard
*-- Author :
      FUNCTION B0BAR(QSQR,RM1,RM2)
*     --------------
* eq. (C.15)
      IMPLICIT double precision (A-H,O-Z)
      double complex B0BAR, DCMPLX
      IF(ABS(RM1-RM2) .GT. 1D-10) THEN
         B0BAR = 1 - (RM1**2+RM2**2)/(RM1**2-RM2**2)*LOG(RM1/RM2) +
     +           DCMPLX( FREAL(QSQR,RM1,RM2) , FIMAG(QSQR,RM1,RM2) )
      ELSE
         B0BAR = DCMPLX( freal1(QSQR,RM1)    , FIMAG(QSQR,RM1,RM2) )
      ENDIF
      END
CDECK  ID>, B1BAR.  
*CMZ :  2.01/08 11/11/95  00.11.13  by  Harald Anlauf
*CMZ :  1.99/00 04/05/93  09.54.03  by  Harald Anlauf & Helge Meinhard
*-- Author :
      FUNCTION B1BAR(QSQR,RM1,RM2)
*     --------------
* eq. (C.16)
      IMPLICIT double precision (A-H,O-Z)
      double complex B1BAR,DCMPLX
      IF(ABS(RM1-RM2) .GT. 1D-10) THEN
         B1BAR = -0.25D0 + RM1**2/(RM1**2-RM2**2)*LOG(RM1/RM2) +
     +           ( RM2**2-RM1**2-QSQR )/(2*QSQR)*
     +            DCMPLX( FREAL(QSQR,RM1,RM2) , FIMAG(QSQR,RM1,RM2) )
      ELSE
         B1BAR = -0.25D0 + 0.5D0 - 0.5d0 *
     +            DCMPLX( freal1(QSQR,RM1)    , FIMAG(QSQR,RM1,RM2) )
      ENDIF
      END
CDECK  ID>, C0SCAL. 
*CMZ :  2.01/08 29/11/95  16.12.59  by  Harald Anlauf
*CMZ :  2.01/00 10/01/95  14.18.22  by  Harald Anlauf
*CMZ :  1.99/07 02/06/93  19.29.47  by  Harald Anlauf & Helge Meinhard
*CMZ :  1.99/00 04/05/93  09.54.03  by  Harald Anlauf & Helge Meinhard
*-- Author :
      FUNCTION C0SCAL(QSQR,RM1,RM2,RMF)
*     ---------------
* The scalar 3 point function with equal external masses.
* eq. (5.10), (C.17)
      IMPLICIT double precision (A-H,O-Z)
      double complex X(1:3),Y(1:3,1:2),HELP,AA,BB,CC,DD,FF,TOT,C0SCAL
      double complex SPENCE
      AA = DCMPLX(RMF**2,0D0)
      BB = DCMPLX(QSQR,0D0)
      CC = - BB
      DD = DCMPLX(RM1**2 - RM2**2 - RMF**2,0D0)
      FF = DCMPLX(RM2**2,-1D-15)
      ALPHA = 2D0*RMF**2/QSQR/( 1D0 + SQRT(1D0-4D0*RMF**2/QSQR) )
      X(1) = - ( DD + 2D0*AA + CC*ALPHA )/(CC+2D0*ALPHA*BB)
      X(2) = - DD/( (1D0-ALPHA)*(CC+2D0*ALPHA*BB) )
      X(3) = DD/( ALPHA*(CC+2D0*ALPHA*BB) )
      HELP = SQRT( CC**2 - 4D0*BB*( AA + DD + FF ) )
      IF(DREAL(CC) .GE. 0D0) THEN
         Y(1,1) = ( - CC - HELP )/(2*BB)
         Y(1,2) = 2*( AA + DD + FF )/(-CC-HELP)
      ELSE
         Y(1,1) = 2*( AA + DD + FF )/(-CC+HELP)
         Y(1,2) = ( - CC + HELP )/(2*BB)
      ENDIF
      HELP = SQRT( DD**2 - 4D0*FF*( AA + BB + CC ) )
      IF(DREAL(DD) .GE. 0D0) THEN
         Y(2,1) = ( - DD - HELP )/(2*AA)
         Y(2,2) = 4*FF*( AA + BB + CC )/(-DD-HELP)/(2*AA)
      ELSE
         Y(2,1) = 4*FF*( AA + BB + CC )/(-DD+HELP)/(2*AA)
         Y(2,2) = ( - DD + HELP )/(2*AA)
      ENDIF
      Y(3,1) = Y(2,1)
      Y(3,2) = Y(2,2)
      TOT = 0D0
      DO 20 J = 1 , 2
         DO 10 L = 1 , 3
            TOT = TOT + (-1D0)**L*(SPENCE(  X(L)   /(X(L)-Y(L,J)) )
     +                            -SPENCE( (X(L)-1)/(X(L)-Y(L,J)) ) )
   10    CONTINUE
   20 CONTINUE
      C0SCAL = TOT / ( CC + 2D0*ALPHA*BB )
      END
CDECK  ID>, C1PLUS. 
*CMZ :  2.01/08 11/11/95  00.02.03  by  Harald Anlauf
*CMZ :  1.99/00 04/05/93  09.54.03  by  Harald Anlauf & Helge Meinhard
*-- Author :
      FUNCTION C1PLUS(QSQR,RM1,RM2,RMF)
*     ---------------
* eq. (C.9)
      IMPLICIT double precision (A-H,O-Z)
      double complex C0SCAL,HELP,B0BAR,C1PLUS
      HELP = LOG(RM2/RM1) + B0BAR(QSQR,RM1,RM1) -
     +       B0BAR(RMF**2,RM1,RM2) + (RM2**2-RM1**2+RMF**2)*
     +       C0SCAL(QSQR,RM1,RM2,RMF)
      C1PLUS = HELP / ( 4D0*RMF**2 - QSQR )
      END
CDECK  ID>, C2ZERO. 
*CMZ :  2.01/08 11/11/95  00.02.03  by  Harald Anlauf
*CMZ :  1.99/00 04/05/93  09.54.03  by  Harald Anlauf & Helge Meinhard
*-- Author :
      FUNCTION C2ZERO(QSQR,RM1,RM2,RMF)
*     ---------------
* eq. (C.9)
      IMPLICIT double precision (A-H,O-Z)
      double complex B0BAR,C0SCAL,C1PLUS,C2ZERO
      C2ZERO = 0.25D0*( B0BAR(QSQR,RM1,RM1) + 1 ) +
     +         0.5D0 *( RM1**2 - RM2**2 - RMF**2 )*
     +         C1PLUS(QSQR,RM1,RM2,RMF) + 0.5D0*RM2**2*
     +         C0SCAL(QSQR,RM1,RM2,RMF)
      END
CDECK  ID>, C2PLUS. 
*CMZ :  2.01/08 11/11/95  00.05.09  by  Harald Anlauf
*CMZ :  1.99/00 04/05/93  09.54.03  by  Harald Anlauf & Helge Meinhard
*-- Author :
      FUNCTION C2PLUS(QSQR,RM1,RM2,RMF)
*     ---------------
* eq. (C.9)
      IMPLICIT double precision (A-H,O-Z)
      double complex B0BAR,B1BAR,C1PLUS,C2ZERO,HELP,C2PLUS
      HELP = 0.5D0*B0BAR(QSQR,RM1,RM1) + 0.5D0*( B1BAR(RMF**2,RM2,RM1)
     +       - 0.25D0 ) + ( RM2**2-RM1**2+RMF**2 )*
     +       C1PLUS(QSQR,RM1,RM2,RMF) - C2ZERO(QSQR,RM1,RM2,RMF)
      C2PLUS = HELP / ( 4D0*RMF**2 - QSQR )
      END
CDECK  ID>, C2MIN.  
*CMZ :  2.01/08 11/11/95  00.05.09  by  Harald Anlauf
*CMZ :  1.99/00 04/05/93  09.54.03  by  Harald Anlauf & Helge Meinhard
*-- Author :
      FUNCTION C2MIN(QSQR,RM1,RM2,RMF)
*     --------------
* eq. (C.9)
      IMPLICIT double precision (A-H,O-Z)
      double complex HELP,B1BAR,C2ZERO,C2MIN
      HELP = -0.5D0*( B1BAR(RMF**2,RM2,RM1) - 0.25D0 ) -
     +         C2ZERO(QSQR,RM1,RM2,RMF)
      C2MIN = HELP / QSQR
      END
CDECK  ID>, SPENCE. 
*CMZ :  2.01/08 29/11/95  16.13.00  by  Harald Anlauf
*CMZ :  2.01/00 10/01/95  14.16.49  by  Harald Anlauf
*CMZ :  1.99/00 04/05/93  09.54.03  by  Harald Anlauf & Helge Meinhard
*-- Author :
      FUNCTION SPENCE(X)
*     ---------------
* Hans Kuijf, 1988
* SPENCE(X) calculates the complex Spence-function, through mapping on
* the area where there is a quickly convergent series.
      double complex X, SPENC, SPENCE
      double precision PI
      parameter (PI=3.141592653589793238d0)
* Map the x onto the unit circle.
* But so that x is not in the neighbourhood of (1,0)
* ABS(Z)=-LOG(1-X) is always smaller than 1.10
* But (1.10)^19/(19!)*bernoulli(19)=2.7D-15
      IF (ABS(1-X) .LT. 1D-13) THEN
        SPENCE = PI*PI/6D0
      ELSE IF (ABS(1-X) .LT. 0.5D0) THEN
        SPENCE = PI*PI/6D0 - LOG(1-X)*LOG(X)  - SPENC(1-X)
      ELSE IF (ABS(X) .GT. 1D0) THEN
        SPENCE =-PI*PI/6D0 - 0.5D0*LOG(-X)**2 - SPENC(1/X)
      ELSE
        SPENCE = SPENC(X)
      END IF
      END
CDECK  ID>, SPENC.  
*CMZ :  2.01/08 21/11/95  17.51.43  by  Harald Anlauf
*CMZ :  2.01/00 10/01/95  14.17.13  by  Harald Anlauf
*CMZ :  1.99/00 04/05/93  09.54.03  by  Harald Anlauf & Helge Meinhard
*-- Author :
      FUNCTION SPENC(X)
*     --------------
      double complex X, SPENC, SUM, Z, Z2
      Z=-LOG(1-X)
      Z2=Z*Z
* Horner's rule for the powers z^3 through z^19
      SUM=43867D0/798D0
      SUM=SUM*Z2/342D0-3617D0/510D0
      SUM=SUM*Z2/272D0+7D0/6D0
      SUM=SUM*Z2/210D0-691D0/2730D0
      SUM=SUM*Z2/156D0+5D0/66D0
      SUM=SUM*Z2/110D0-1D0/30D0
      SUM=SUM*Z2/ 72D0+1D0/42D0
      SUM=SUM*Z2/ 42D0-1D0/30D0
      SUM=SUM*Z2/ 20D0+1D0/6D0
* The first three terms of the power series
      SUM=Z2*Z*SUM/6D0-0.25D0*Z2+Z
      SPENC=SUM
      END
CDECK  ID>, PILEPT. 
*CMZ :  2.01/08 15/11/95  01.10.33  by  Harald Anlauf
*-- Author :    Harald Anlauf   14/11/95

      function pilept (s)
c     --------------
c Real part of the purely leptonic part of sigma-g(qsqr) / qsqr
      implicit none
      double precision pilept, s
      integer nrmass
      PARAMETER ( NRMASS = 9 )
      double precision RMASS, RMASS2, VF, AF, QF
      COMMON/ MASSES / RMASS(0:NRMASS), RMASS2(0:NRMASS),
     &          VF(0:NRMASS), AF(0:NRMASS), QF(0:NRMASS)
      double precision t3f, mq2mz
      common/ masses / t3f(0:NRMASS), mq2mz(0:NRMASS)
      double precision HBARC2, PI
      parameter (HBARC2 = 0.38937966d9, PI = 3.141592653589793238d0)
      double precision ALFA, ALFAS, FACQCD, FACQED, dens
      COMMON/ ADHOC  / ALFA, ALFAS, FACQCD, FACQED, dens(-1:1,-1:1)
      double precision tot, FREAL1
      integer i
      tot = 0
      do 10 i = 1, 3
         tot = tot + 4D0/3D0*QF(I)**2*(
     +         ( S+2*RMASS2(I) )*freal1(S,RMASS(I)) - S/3D0 )
 10   continue
      pilept = ALFA/(4*PI*s) * TOT
      end
CDECK  ID>, PHADPI. 
*CMZ :  2.01/08 13/11/95  12.25.18  by  Harald Anlauf
*CMZ :  2.01/04 20/03/95  15.11.35  by  Harald Anlauf
*CMZ :  1.99/00 06/05/93  12.41.28  by  Harald Anlauf & Helge Meinhard
*-- Author :
      FUNCTION PHADPI(QSQR)
*     ---------------
* W.J.P. Beenakker, F.A.Berends and S.C. van der Marck, June 14th 1989
* Real part of the hadronic part of sigma-g(qsqr) / qsqr = pi-hadronic,
* calculated perturbatively.
      IMPLICIT double precision (A-H,O-Z)
      integer nrmass
      PARAMETER ( NRMASS = 9 )
      double precision RMASS, RMASS2, VF, AF, QF
      COMMON/ MASSES / RMASS(0:NRMASS), RMASS2(0:NRMASS),
     &          VF(0:NRMASS), AF(0:NRMASS), QF(0:NRMASS)
      double precision t3f, mq2mz
      common/ masses / t3f(0:NRMASS), mq2mz(0:NRMASS)
      double precision HBARC2, PI
      parameter (HBARC2 = 0.38937966d9, PI = 3.141592653589793238d0)
      double precision ALFA, ALFAS, FACQCD, FACQED, dens
      COMMON/ ADHOC  / ALFA, ALFAS, FACQCD, FACQED, dens(-1:1,-1:1)
      S   = QSQR
      TOT = 0D0
      FAC = 3D0
      DO 10 I = 4 , NRMASS
         RM = RMASS(I)
         IF(I.NE.8) TOT = TOT + 4D0/3D0*QF(I)**2*(
     +         ( S+2*RMASS2(I) )*freal1(S,RM) - S/3D0 ) * FAC
   10 CONTINUE
      PHADPI = ALFA/(4*PI*S) * TOT
      END
CDECK  ID>, HADRQQ. 
*CMZ :  2.01/08 13/11/95  12.25.18  by  Harald Anlauf
*CMZ :  2.00/01 29/09/94  16.38.29  by  Harald Anlauf
*CMZ :  1.99/00 04/05/93  10.06.11  by  Harald Anlauf & Helge Meinhard
*-- Author :
* --- From here on: general purpose routines from other authors.

      FUNCTION HADRQQ(S)
C  HADRONIC IRREDUCIBLE QQ SELF-ENERGY: TRANSVERSE
C     parametrize the real part of the photon self energy function
C     by  a + b ln(1+C*|S|) , as in my 1981 TASSO note but using
C     updated values, extended using RQCD up to 100 TeV
C     for details see:
C     H.Burkhardt, F.Jegerlehner, G.Penso and C.Verzegnassi
C     in CERN Yellow Report on "Polarization at LEP" 1988
C               H.BURKHARDT, CERN/ALEPH, AUGUST 1988
C
      IMPLICIT double precision (A-H,O-Z)
      double complex HADRQQ
      save ALFA, INIT, A1,B1,C1,A2,B2,C2,A3,B3,C3,A4,B4,C4
C
      DATA A1,B1,C1/   0.0   ,   0.00835,  1.0   /
      DATA A2,B2,C2/   0.0   ,   0.00238,  3.927 /
      DATA A3,B3,C3/ 0.00165 ,   0.00300,  1.0   /
      DATA A4,B4,C4/ 0.00221 ,   0.00293,  1.0   /
C
      DATA ALFAIN/137.0359895D0/,INIT/0/
C
      IF(INIT.EQ.0) THEN
        INIT=1
        ALFA=1/ALFAIN
      ENDIF
      T=ABS(S)
      IF(T.LT.0.3**2) THEN
        REPIAA=A1+B1*LOG(1+C1*T)
      ELSEIF(T.LT.3.**2) THEN
        REPIAA=A2+B2*LOG(1+C2*T)
      ELSEIF(T.LT.100.**2) THEN
        REPIAA=A3+B3*LOG(1+C3*T)
      ELSE
        REPIAA=A4+B4*LOG(1+C4*T)
      ENDIF
      if (s .gt. 0) then
C as imaginary part take -i alfa/3 Rexp   (But beware of the sign! [ha])
         HADRQQ = dcmplx (REPIAA, -ALFA/3*REXP(S))
      else
c No imaginary part for spacelike momentum transfer! [ha]
         HADRQQ = REPIAA
      endif
      END
CDECK  ID>, REXP.   
*CMZ :  2.01/08 29/11/95  15.00.15  by  Harald Anlauf
*CMZ :  2.01/00 10/01/95  14.21.36  by  Harald Anlauf
*CMZ :  1.99/00 04/05/93  09.54.03  by  Harald Anlauf & Helge Meinhard
*-- Author :
      FUNCTION REXP(S)
C  HADRONIC IRREDUCIBLE QQ SELF-ENERGY: IMAGINARY
      IMPLICIT double precision (A-H,O-Z)
C     continuum R = Ai+Bi W ,  this + resonances was used to calculate
C     the dispersion integral. Used in the imag part of HADRQQ
      PARAMETER (NDIM=18)
      DIMENSION WW(NDIM),RR(NDIM),AA(NDIM),BB(NDIM)
      DATA WW/1.,1.5,2.0,2.3,3.73,4.0,4.5,5.0,7.0,8.0,9.,10.55,
     .  12.,50.,100.,1000.,10 000.,100 000./
      DATA RR/0.,2.3,1.5,2.7,2.7,3.6,3.6,4.0,4.0,3.66,3.66,3.66,
     .   4.,3.87,3.84, 3.79, 3.76,    3.75/
      DATA INIT/0/
      IF(INIT.EQ.0) THEN
        INIT=1
C       calculate A,B from straight lines between R measurements
        BB(NDIM)=0
        DO 4 I=1,NDIM
          IF(I.LT.NDIM) BB(I)=(RR(I)-RR(I+1))/(WW(I)-WW(I+1))
          AA(I)=RR(I)-BB(I)*WW(I)
    4   CONTINUE
      ENDIF
      REXP=0.D0
      IF(S.GT.0.D0) THEN
        W=SQRT(S)
        IF(W.GT.WW(1)) THEN
          DO 2 I=1,NDIM
C           find out between which points of the RR array W is
            K=I
            IF(I.LT.NDIM) THEN
              IF(W.LT.WW(I+1)) GOTO 3
            ENDIF
    2     CONTINUE
    3     CONTINUE
          REXP=AA(K)+BB(K)*W
        ENDIF
      ENDIF
      END
CDECK  ID>, QCDCOR. 
*CMZ :  2.01/08 11/11/95  02.49.26  by  Harald Anlauf
*-- Author :    Harald Anlauf   09/11/95

      subroutine QCDCOR ()
c     -----------------
c Set up QCD correction factors for hadronic Z decays.
c Source: CERN 95-03
c
      implicit none

      double precision HBARC2, PI
      parameter (HBARC2 = 0.38937966d9, PI = 3.141592653589793238d0)
      double precision ALFA, ALFAS, FACQCD, FACQED, dens
      COMMON/ ADHOC  / ALFA, ALFAS, FACQCD, FACQED, dens(-1:1,-1:1)
      double precision RMZ,RMW,RMH,RMT, ZWID, SIN2TH, sw, cw, cw2, pizmz
      COMMON/ BOSONS / RMZ,RMW,RMH,RMT, ZWID, SIN2TH, sw, cw, cw2, pizmz
      integer NC, NF
      parameter (NC = 3, NF = 5)
      double precision beta0, beta1, beta2, gamm0, gamm1, gamm2, lamqcd
      common /qcdcom/  beta0, beta1, beta2, gamm0, gamm1, gamm2, lamqcd

      double precision rvq0, rvqm, raq0, raqm, raqi, rmixed
      common /qcdcom/  rvq0, rvqm, raq0, raqm, raqi, rmixed

      double precision delvu, delvm, del1am, del2am, as, x, l, i2, i3
      double precision ZETA3
      parameter (ZETA3 = 1.20205690316d0)

      as = alfas/PI
      x = (RMZ/RMT)**2
      l = log (x)

c Eq. (57):
      delvu  = (44.d0/675 - 2.d0/135 * l) * x
      delvm  = 1 + 8.736d0 * as +  45.15d0       * as**2
      del1am = 1 + 11.d0/3 * as + (11.286d0 + l) * as**2
      del2am = 8.d0/81 - l / 54

c Eq. (58):
      i2 =   -37.d0/ 12 + l + 7.d0/81 * x + 0.0132d0 * x**2
      i3 = -5075.d0/216 + 23.d0/36 * PI**2 + ZETA3
     &      + 67.d0/18 * l + 23.d0/12 * l**2

c Eq. (55), vector part
c Massless non-singlet correction (r_1) + massive universal (r_4)
      rvq0 = 1 + as + (1.40923d0 + delvu) * as**2 - 12.76706d0 * as**3
c Massive non-singlet correction (r_5) + massive universal (r_2)
      rvqm = 12 * as * delvm

c Eq. (56), axial vector part
c Massless non-singlet correction (r_1)
      raq0 = rvq0
c Massive non-singlet correction (r_7) + singlet (r_10)
      raqm = -6 * del1am - 10 * x * as**2 * del2am
c Isospin-dependent piece (r_9)
      raqi = - i2 * as**2 - i3 * as**3

c Coefficient of mixed QED-QCD corrections (r_12):
      rmixed = 1 - as/3

c Note: the singlet contribution to the vector part (r_11) will be
c neglected, since its size is of order -0.01 MeV for the Z width.

      end
CDECK  ID>, INIQCD. 
*CMZ :  2.01/08 17/11/95  19.03.05  by  Harald Anlauf
*-- Author :    Harald Anlauf   09/11/95

      subroutine iniqcd (alfamz, mz)
c     -----------------
c Initialize parameters relevant for calculation of QCD corrections.
c
      implicit none
      double precision alfamz, mz

      integer NC, NF
      parameter (NC = 3, NF = 5)
      double precision beta0, beta1, beta2, gamm0, gamm1, gamm2, lamqcd
      common /qcdcom/  beta0, beta1, beta2, gamm0, gamm1, gamm2, lamqcd

      double precision rvq0, rvqm, raq0, raqm, raqi, rmixed
      common /qcdcom/  rvq0, rvqm, raq0, raqm, raqi, rmixed

      double precision PI, ZETA3
      parameter (PI = 3.141592653589793238d0, ZETA3 = 1.20205690316d0)
      double precision dellam, tmp, alfqcd
      external alfqcd
      integer i

c The anomalous dimensions up to and including three loops:

      beta0 = ( 11 -  2.d0/3 * NF) / 4
      beta1 = (102 - 38.d0/3 * NF) /16
      beta2 = (2857.d0/2 - 5033.d0/18 * NF + 325.d0/54 * NF**2) / 64

      gamm0 = 1
      gamm1 = (202.d0/3 - 20.d0/9 * NF) / 16
      gamm2 = (1249 - (2216.d0/27 + 160.d0/3*ZETA3) * NF -
     &                140.d0/81 * NF**2) / 64

c Find the value of Lambda_QCD^(5) so that alpha_S(M_Z) = alfamz:
c Starting value:
      lamqcd = 0.250d0
      do i = 1, 20
c Simple Newton-type algorithm:
         tmp = alfqcd(mz)
         dellam = - (tmp - alfamz) /
     &        (2*PI/lamqcd *
     &         (beta0 * (tmp/PI)**2 + beta1 * (tmp/PI)**3))
         lamqcd = lamqcd + dellam
         if (abs (dellam) .lt. 1d-6)  goto 10
      enddo
      print *, 'INIQCD: Did not converge!'
      stop

 10   continue

      end
CDECK  ID>, ALFQCD. 
*CMZ :  2.01/08 17/11/95  19.03.05  by  Harald Anlauf
*-- Author :    Harald Anlauf   09/11/95

      function alfqcd (scale)
c     ---------------
c Calculate the running QCD coupling constant with 2-loop accuracy.
c It is assumed that there are five active flavors and no thresholds.
c
      implicit none
      double precision alfqcd, scale

      integer NC, NF
      parameter (NC = 3, NF = 5)
      double precision beta0, beta1, beta2, gamm0, gamm1, gamm2, lamqcd
      common /qcdcom/  beta0, beta1, beta2, gamm0, gamm1, gamm2, lamqcd

      double precision rvq0, rvqm, raq0, raqm, raqi, rmixed
      common /qcdcom/  rvq0, rvqm, raq0, raqm, raqi, rmixed

      double precision PI, L, logL, tmp
      parameter (PI = 3.141592653589793238d0)

      L = 2 * log (scale/lamqcd)
      tmp = beta0 * L
      logL = log (L)

c The 3-loop expression for alpha_S:

      alfqcd = PI/tmp * (1 - beta1 * logL/(tmp * beta0) +
     &                       ( (beta1/beta0)**2 * (logL**2 - logL - 1) +
     &                         beta2/beta0
     &                       ) / tmp**2 )

      end
CDECK  ID>, RUNMAS. 
*CMZ :  2.01/08 17/11/95  19.03.05  by  Harald Anlauf
*-- Author :    Harald Anlauf   09/11/95

      function runmas (polmas, scale)
c     ---------------
c Calculate the running quark mass at a given scale from the pole mass.
c
c We are a little bit sloppy here, since we are interested only in the
c case of the b-quark, and we have hard-coded the ratio M_c/M_b.
c
c For details see Chetyrkin et al., CERN 95-03.
      implicit none
      double precision runmas, polmas, scale

      integer NC, NF
      parameter (NC = 3, NF = 5)
      double precision beta0, beta1, beta2, gamm0, gamm1, gamm2, lamqcd
      common /qcdcom/  beta0, beta1, beta2, gamm0, gamm1, gamm2, lamqcd

      double precision rvq0, rvqm, raq0, raqm, raqi, rmixed
      common /qcdcom/  rvq0, rvqm, raq0, raqm, raqi, rmixed

      double precision PI
      parameter (PI = 3.141592653589793238d0)
      double precision alfqcd, mqmq, as0, Kq, r, Delta, as

c The relation between pole mass M_Q and m_q(M_Q):

      as0 = alfqcd (polmas) / PI
c The only relevant massive quark below the b is the c:
      r = 1.55d0 / 4.7d0
      Delta = r * (PI**2/8 - 0.597d0 * r + 0.230d0 * r**2)
      Kq = 17.1514d0 - 1.04137d0 * NF + 4.d0/3 * Delta
      mqmq = polmas * (1 - 4.d0 / 3 * as0 - as0**2 * (Kq - 16.d0/9))

c The running from M_Q to the given scale:

      as = alfqcd (scale) / PI
      runmas = mqmq * (as/as0)**(gamm0/beta0) *
     &     (1 + (gamm1/beta0 - beta1*gamm0/beta0**2)*(as-as0)
     &     + 0.5d0*((gamm1/beta0-beta1*gamm0/beta0**2)*(as-as0))**2
     &     + 0.5d0*(gamm2/beta0-beta1*gamm1/beta0**2-beta2*gamm0+
     &              beta1**2*gamm0/beta0**3)*(as**2-as0**2) )

      end
CDECK  ID>, DRHOHO. 
*CMZ :  2.01/08 29/11/95  15.29.55  by  Harald Anlauf
*-- Author :    Harald Anlauf   08/11/95

      function drhoHO ()
c     ---------------
c The higher order corrections to \Delta\rho, from
c CERN 95-03, p. 74, eq. (157)
      implicit none
      double precision drhoHO

      double precision ahpla
      common /ubpcom/  ahpla
      double precision alpha
      common /ubpcom/  alpha
      double precision mass1e
      common /ubpcom/  mass1e
      double precision mass2e
      common /ubpcom/  mass2e
      double precision mass1z
      common /ubpcom/  mass1z
      double precision mass2z
      common /ubpcom/  mass2z
      double precision mass1t
      common /ubpcom/  mass1t
      double precision mass1h
      common /ubpcom/  mass1h
      double precision mass1w
      common /ubpcom/  mass1w
      double precision gamm1z
      common /ubpcom/  gamm1z
      double precision gamm2z
      common /ubpcom/  gamm2z
      double precision sin2w
      common /ubpcom/  sin2w
      double precision alphas
      common /ubpcom/  alphas
      double precision ebeam
      common /ubpcom/  ebeam
      double precision epol
      common /ubpcom/  epol
      double precision ppol
      common /ubpcom/  ppol
      double precision ctsmin
      common /ubpcom/  ctsmin
      double precision ctsmax
      common /ubpcom/  ctsmax
      double precision ecut
      common /ubpcom/  ecut
      double precision acocut
      common /ubpcom/  acocut
      double precision evisct
      common /ubpcom/  evisct
      integer          nevent, pad022
      common /ubpcom/  nevent, pad022
      integer          rseed , pad023
      common /ubpcom/  rseed , pad023
      logical          tchann, pad024
      common /ubpcom/  tchann, pad024
      logical          qedvtx, pad025
      common /ubpcom/  qedvtx, pad025
      logical          qedbox, pad026
      common /ubpcom/  qedbox, pad026
      logical          weak  , pad027
      common /ubpcom/  weak  , pad027
      logical          boxes , pad028
      common /ubpcom/  boxes , pad028
      integer          isrtyp, pad029
      common /ubpcom/  isrtyp, pad029
      integer          fsrtyp, pad030
      common /ubpcom/  fsrtyp, pad030
      double precision epsiln
      common /ubpcom/  epsiln
      double precision taumin
      common /ubpcom/  taumin
      double precision taumax
      common /ubpcom/  taumax
      double precision emin
      common /ubpcom/  emin
      double precision eps2
      common /ubpcom/  eps2
      double precision qsq
      common /ubpcom/  qsq
      integer          bstyle, pad037
      common /ubpcom/  bstyle, pad037
      logical          nonlog, pad038
      common /ubpcom/  nonlog, pad038
      integer          stdin , pad039
      common /ubpcom/  stdin , pad039
      integer          stdout, pad040
      common /ubpcom/  stdout, pad040
      integer          stderr, pad041
      common /ubpcom/  stderr, pad041
      integer          errcnt, pad042
      common /ubpcom/  errcnt, pad042
      integer          errmax, pad043
      common /ubpcom/  errmax, pad043
      integer          verbos, pad044
      common /ubpcom/  verbos, pad044
      integer          status, pad045
      common /ubpcom/  status, pad045
      integer          hepdat, pad046
      common /ubpcom/  hepdat, pad046
      integer          heprev, pad047
      common /ubpcom/  heprev, pad047
      integer          runid , pad048
      common /ubpcom/  runid , pad048
      logical          dbgcol, pad049
      common /ubpcom/  dbgcol, pad049
      logical          dbghep, pad050
      common /ubpcom/  dbghep, pad050
      logical          dbgini, pad051
      common /ubpcom/  dbgini, pad051
      logical          dbgmas, pad052
      common /ubpcom/  dbgmas, pad052
      logical          dbgmup, pad053
      common /ubpcom/  dbgmup, pad053
      double precision HBARC2, PI
      parameter (HBARC2 = 0.38937966d9, PI = 3.141592653589793238d0)
      double precision ALFA, ALFAS, FACQCD, FACQED, dens
      COMMON/ ADHOC  / ALFA, ALFAS, FACQCD, FACQED, dens(-1:1,-1:1)
      double precision RMZ,RMW,RMH,RMT, ZWID, SIN2TH, sw, cw, cw2, pizmz
      COMMON/ BOSONS / RMZ,RMW,RMH,RMT, ZWID, SIN2TH, sw, cw, cw2, pizmz

      double precision GFERMI
      parameter (GFERMI = 1.16639d-5)
      double precision xt, xtbar, del3qcd, r, rho2, alfqcd
      external rho2, alfqcd
c      double precision alfsmt

      xtbar = ALFA / (16*PI*SIN2TH*CW2) * (RMT/RMZ)**2
      xt    = GFERMI * RMT**2 / (8*PI**2 * sqrt(2.d0))

c Update: CERN 95-03, (78ff)
c non-AFMT:
c      alfsmt = alfqcd (RMT)
c      del3qcd = - 2.d0/3 * (1 + PI**2/3) * alfsmt/PI
c     &          - PI**2 * (2.155165d0 - 6*0.180981d0) * (alfsmt/PI)**2
c (Regarding the choice N_f = 6 see the remarks on p.47)
c
c AFMT: (112) resp. (113) and remarks on p.48
c      del3qcd = - 2.d0/3 * (1 + PI**2/3) * alfqcd(0.444d0*RMT)/PI
c AFMT revised: as of "Note added in proof", p.162
      del3qcd = - 2.d0/3 * (1 + PI**2/3) * alfqcd(0.248d0*RMT)/PI
c AFMT revised: see contribution by Sirlin, p. 295, Note added in proof.
c      del3qcd = - 2.d0/3 * (1 + PI**2/3) * alfqcd(0.260d0*RMT)/PI

c Which one to choose?  Actually, the difference between those is part of
c the quoted "theoretical error".

      r = (RMT/RMH)**2

      drhoHO = 3 * xtbar * (xt * rho2 (r) + del3qcd)
c (Note that we have omitted the leading term which is supposed to be
c already contained in the 1-loop contribution).

      end
CDECK  ID>, RHO2.   
*CMZ :  2.01/08 09/11/95  00.03.54  by  Harald Anlauf
*-- Author :    Harald Anlauf   09/11/95

      function rho2 (r)
c     -------------
c \rho^{(2)} from the leading m_t^4 dependence, as calculated by:
c Barbieri et al., Nucl. Phys. B409 (1993) 105.
c
c Parameter: r = (m_t / m_H)**2
      implicit none
      double precision rho2, r

      double precision PI, PI2
      parameter (PI = 3.141592653589793238d0, PI2 = PI**2)

      double precision P1, P2, P3, P4, P5, P6, P7, P8, x, logr
      save P1, P2, P3, P4, P5, P6, P7, P8
      DATA P1/-0.74141D0/,P2/ -11.483D0  /,P3/  9.6577D0/,
     &     P4/ -6.7270D0/,P5/  3.0659D0  /,P6/-0.82053D0/,
     &     P7/ 0.11659D0/,P8/-0.67712D-02/

      x = 1 / sqrt (r)
      if (x .le. 4d0) then
c Interpolation formula taken from ZFITTER 4.8:
         rho2 = P1+P2*X+P3*X**2+P4*X**3+P5*X**4+P6*X**5+P7*X**6+P8*X**7
      else
         logr = log(r)
         rho2 = 49.d0/4 + PI2 + 27.d0/2 * logr + 1.5d0 * logr**2
     &        + r/3    * (   2 -  12*PI2 +   12 * logr -  27 * logr**2)
     &        + r*r/48 * (1613 - 240*PI2 - 1500 * logr - 720 * logr**2)
      endif

      end
CDECK  ID>, FLBOT2. 
*CMZ :  2.01/08 14/11/95  17.19.54  by  Harald Anlauf
*-- Author :    Harald Anlauf   11/11/95

      function flbot2 ()
c     ---------------
c The higher order (FJTR) corrections to the b-quark form-factor.
c CERN 95-03, eq. (176,177)
      implicit none
      double precision flbot2

      double precision HBARC2, PI
      parameter (HBARC2 = 0.38937966d9, PI = 3.141592653589793238d0)
      double precision ALFA, ALFAS, FACQCD, FACQED, dens
      COMMON/ ADHOC  / ALFA, ALFAS, FACQCD, FACQED, dens(-1:1,-1:1)
      double precision RMZ,RMW,RMH,RMT, ZWID, SIN2TH, sw, cw, cw2, pizmz
      COMMON/ BOSONS / RMZ,RMW,RMH,RMT, ZWID, SIN2TH, sw, cw, cw2, pizmz

      double precision GFERMI
      parameter (GFERMI = 1.16639d-5)
      double precision xt, xtbar, r, alfqcd, tau2
      external alfqcd, tau2

      xt    = GFERMI * RMT**2 / (8*PI**2 * sqrt(2.d0))
      xtbar = ALFA / (16*PI*SIN2TH*CW2) * (RMT/RMZ)**2

      r = (RMT/RMH)**2
      flbot2 = xt * (1 + xt * tau2 (r) - alfqcd (RMT) * PI/3) - xtbar

c Note that we have to correct for the additional prefactor in (166) in
c order to get consistency with the Barbieri et al. result:
      flbot2 = flbot2 * (4*PI)/alfa
      end
CDECK  ID>, TAU2.   
*CMZ :  2.01/08 24/11/95  11.39.31  by  Harald Anlauf
*-- Author :    Harald Anlauf   11/11/95

      function tau2 (r)
c     -------------
c \tau^{(2)} from the leading m_t^4 dependence, as calculated by:
c Barbieri et al., Nucl. Phys. B409 (1993) 105, P.L.B 312 (1993) 511 (E)
c
c Parameter: r = (m_t / m_H)**2
      implicit none

      double precision tau2, r

      double precision PI, PI2
      parameter (PI = 3.141592653589793238d0, PI2 = PI**2)

      double precision P1, P2, P3, P4, P5, P6, P7, P8, x, logr
      save P1, P2, P3, P4, P5, P6, P7, P8
      DATA P1/ 5.6807D0/,P2/ -11.015D0  /,P3/ 12.814D0/,
     &     P4/-9.2954D0/,P5/  4.3305D0  /,P6/-1.2125D0/,
     &     P7/0.18402D0/,P8/-0.11582D-01/

      x = 1 / sqrt (r)
      if (x .le. 4d0) then
c Interpolation formula taken from ZFITTER 4.8:
         tau2 = P1+P2*X+P3*X**2+P4*X**3+P5*X**4+P6*X**5+P7*X**6+P8*X**7
      else
         logr = log(r)
         tau2 =(   311 +   24*PI2 +   282 * logr +   90 * logr**2
     &         -(   40 +    6*PI2 +    15 * logr +   18 * logr**2) * 4*r
     &         +(24209 - 6000*PI2 - 45420 * logr -18000 * logr**2)
     &          * 0.03d0 * r**2 ) / 144
      endif

      end
