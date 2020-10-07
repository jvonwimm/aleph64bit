      SUBROUTINE UBINIT (IDATQQ, VERSQQ)
*KEEP,IMPLNONE.
c     implicit none
*KEND.
*KEEP,UBPCOM.
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
      double precision ebeam
      common /ubpcom/  ebeam
      double precision ctsmin
      common /ubpcom/  ctsmin
      double precision ctsmax
      common /ubpcom/  ctsmax
      double precision ecut
      common /ubpcom/  ecut
      double precision acocut
      common /ubpcom/  acocut
      integer          nevent, pad018
      common /ubpcom/  nevent, pad018
      integer          rseed , pad019
      common /ubpcom/  rseed , pad019
      logical          tchann, pad020
      common /ubpcom/  tchann, pad020
      logical          weak  , pad021
      common /ubpcom/  weak  , pad021
      logical          boxes , pad022
      common /ubpcom/  boxes , pad022
      double precision taumin
      common /ubpcom/  taumin
      double precision taumax
      common /ubpcom/  taumax
      double precision emin
      common /ubpcom/  emin
      double precision epsiln
      common /ubpcom/  epsiln
      double precision qsq
      common /ubpcom/  qsq
      integer          bstyle, pad028
      common /ubpcom/  bstyle, pad028
      integer          stdin , pad029
      common /ubpcom/  stdin , pad029
      integer          stdout, pad030
      common /ubpcom/  stdout, pad030
      integer          stderr, pad031
      common /ubpcom/  stderr, pad031
      integer          errcnt, pad032
      common /ubpcom/  errcnt, pad032
      integer          errmax, pad033
      common /ubpcom/  errmax, pad033
      integer          verbos, pad034
      common /ubpcom/  verbos, pad034
      integer          status, pad035
      common /ubpcom/  status, pad035
      integer          hepdat, pad036
      common /ubpcom/  hepdat, pad036
      integer          heprev, pad037
      common /ubpcom/  heprev, pad037
      integer          runid , pad038
      common /ubpcom/  runid , pad038
      logical          dbgcol, pad039
      common /ubpcom/  dbgcol, pad039
      logical          dbghep, pad040
      common /ubpcom/  dbghep, pad040
      logical          dbgini, pad041
      common /ubpcom/  dbgini, pad041
      logical          dbgmup, pad042
      common /ubpcom/  dbgmup, pad042
      logical          dbgsca, pad043
      common /ubpcom/  dbgsca, pad043
*KEEP,UBCSTA.
      double precision sumw, sumw2, maxws, maxwt
      common /ubcsta/  sumw, sumw2, maxws, maxwt
      integer isbad, iscall, isfail, isrej
      common /ubcsta/ isbad, iscall, isfail, isrej
      integer  itbad, itcall, itfail, callct, acccut
      common /ubcsta/  itbad, itcall, itfail, callct, acccut
*KEEP,UBCMSC.
      double precision cacomx, logeps
      common /ubcmsc/ cacomx, logeps
*KEEP,UBCNST.
      double precision PI
      parameter (PI = 3.141592653589793238d0)
*KEND.
      double precision ubrgen, dummy
      external ubrgen
      double precision MAXCTS
      parameter (MAXCTS = 0.985d0)
      integer date, time, IDATQQ, IVERSQ, mcid
      character*8  VERSQQ
      double precision IVERQQ
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
      call datime (date, time)
      time = time * 100
c Here we import the CMZ Version control strings.
c IDATQQ = yymmdd, IVERSQQ = vvrrll, VERSQQ = 'vv.rr/ll'.
      write (VERQQQ, 8000)
 8000 format (
*KEEP,QFTITLE,N=17.
     + 17HUNIBAB    1.99/08
*KEND.
     &       )
      read (VERQQQ, 9000) IVERQQ
 9000 format (9X,F5.2)
      IVERSQ = int (10000.0 * IVERQQ)
*KEEP,DATEQQ.
      IDATQQ = 931008
*KEEP,VERSQQ.
      VERSQQ = ' 1.99/08'
      IVERSQ =  19908
*KEND.
      hepdat = IDATQQ
      heprev = IVERSQ/100
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
      callct = 0
      acccut = 0
      sumw = 0.
      sumw2 = 0.
      maxws = 0.
      maxwt = 0.
c initialize the Monte Carlo Parameters
c Set branching scale
      qsq = 4. * ebeam**2
c a time saver
      logeps = log (epsiln)
c Transform given cuts into MC internal parameters:
      if (ctsmax .gt. MAXCTS) then
         write (msg,10) MAXCTS
 10      format ('ctsmax too large, reset to ',f6.3)
         call ubumsg ('ubinit', 1, msg)
         ctsmax = MAXCTS
      endif
c     angle cuts from cos(theta*)
      taumin = (1.d0 - ctsmax) / 2.d0
      taumax = (1.d0 - ctsmin) / 2.d0
      if (bstyle .ne. 0) then
         taumin = taumin * FUDGE1
         taumax = 1.d0 - (1.d0 - taumax) * FUDGE2
      endif
c     minimal invariant mass from energy and acollinearity cuts
      emin = ecut * cos (acocut * (PI / 360.d0))
      if (bstyle .ne. 0) then
         emin = emin * FUDGE3
      endif
      cacomx = cos (acocut * (PI / 180.d0))
c Set parameters of the Glashow-Salam-Weinberg model
      call ubigsw ()
c Special infos and warnings:
      if (dbgcol .or. dbgmup .or. dbgsca) then
         call ubumsg ('ubinit', 1,
     &        'You have touched the internal debugging options !')
         call ubumsg ('ubinit', 1,
     &        'You must *NOT* use the results in your publications !!!')
      endif
c Find maximal Born X-section
      call ubibmx ()
c We're ready:
      status = 1
      END
c UBIGSW - Initialize electroweak library
      SUBROUTINE UBIGSW ()
*KEEP,IMPLNONE.
c     implicit none
*KEND.
*KEEP,UBPCOM.
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
      double precision ebeam
      common /ubpcom/  ebeam
      double precision ctsmin
      common /ubpcom/  ctsmin
      double precision ctsmax
      common /ubpcom/  ctsmax
      double precision ecut
      common /ubpcom/  ecut
      double precision acocut
      common /ubpcom/  acocut
      integer          nevent, pad018
      common /ubpcom/  nevent, pad018
      integer          rseed , pad019
      common /ubpcom/  rseed , pad019
      logical          tchann, pad020
      common /ubpcom/  tchann, pad020
      logical          weak  , pad021
      common /ubpcom/  weak  , pad021
      logical          boxes , pad022
      common /ubpcom/  boxes , pad022
      double precision taumin
      common /ubpcom/  taumin
      double precision taumax
      common /ubpcom/  taumax
      double precision emin
      common /ubpcom/  emin
      double precision epsiln
      common /ubpcom/  epsiln
      double precision qsq
      common /ubpcom/  qsq
      integer          bstyle, pad028
      common /ubpcom/  bstyle, pad028
      integer          stdin , pad029
      common /ubpcom/  stdin , pad029
      integer          stdout, pad030
      common /ubpcom/  stdout, pad030
      integer          stderr, pad031
      common /ubpcom/  stderr, pad031
      integer          errcnt, pad032
      common /ubpcom/  errcnt, pad032
      integer          errmax, pad033
      common /ubpcom/  errmax, pad033
      integer          verbos, pad034
      common /ubpcom/  verbos, pad034
      integer          status, pad035
      common /ubpcom/  status, pad035
      integer          hepdat, pad036
      common /ubpcom/  hepdat, pad036
      integer          heprev, pad037
      common /ubpcom/  heprev, pad037
      integer          runid , pad038
      common /ubpcom/  runid , pad038
      logical          dbgcol, pad039
      common /ubpcom/  dbgcol, pad039
      logical          dbghep, pad040
      common /ubpcom/  dbghep, pad040
      logical          dbgini, pad041
      common /ubpcom/  dbgini, pad041
      logical          dbgmup, pad042
      common /ubpcom/  dbgmup, pad042
      logical          dbgsca, pad043
      common /ubpcom/  dbgsca, pad043
*KEEP,BOSONS.
      double precision RMZ,RMW,RMH,RMT,SIN2TH,ZWID
      COMMON/ BOSONS / RMZ,RMW,RMH,RMT,SIN2TH,ZWID
*KEEP,OPTION.
      integer IWEAK, IFERM, ICHANN, IBOXES, IOUT
      COMMON/ OPTION / IWEAK, IFERM, ICHANN, IBOXES, IOUT
*KEND.
      call ewinit (mass1z, mass1h, mass1t)
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
      END
      SUBROUTINE UBIBMX ()
*KEEP,IMPLNONE.
c     implicit none
*KEND.
*KEEP,UBPCOM.
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
      double precision ebeam
      common /ubpcom/  ebeam
      double precision ctsmin
      common /ubpcom/  ctsmin
      double precision ctsmax
      common /ubpcom/  ctsmax
      double precision ecut
      common /ubpcom/  ecut
      double precision acocut
      common /ubpcom/  acocut
      integer          nevent, pad018
      common /ubpcom/  nevent, pad018
      integer          rseed , pad019
      common /ubpcom/  rseed , pad019
      logical          tchann, pad020
      common /ubpcom/  tchann, pad020
      logical          weak  , pad021
      common /ubpcom/  weak  , pad021
      logical          boxes , pad022
      common /ubpcom/  boxes , pad022
      double precision taumin
      common /ubpcom/  taumin
      double precision taumax
      common /ubpcom/  taumax
      double precision emin
      common /ubpcom/  emin
      double precision epsiln
      common /ubpcom/  epsiln
      double precision qsq
      common /ubpcom/  qsq
      integer          bstyle, pad028
      common /ubpcom/  bstyle, pad028
      integer          stdin , pad029
      common /ubpcom/  stdin , pad029
      integer          stdout, pad030
      common /ubpcom/  stdout, pad030
      integer          stderr, pad031
      common /ubpcom/  stderr, pad031
      integer          errcnt, pad032
      common /ubpcom/  errcnt, pad032
      integer          errmax, pad033
      common /ubpcom/  errmax, pad033
      integer          verbos, pad034
      common /ubpcom/  verbos, pad034
      integer          status, pad035
      common /ubpcom/  status, pad035
      integer          hepdat, pad036
      common /ubpcom/  hepdat, pad036
      integer          heprev, pad037
      common /ubpcom/  heprev, pad037
      integer          runid , pad038
      common /ubpcom/  runid , pad038
      logical          dbgcol, pad039
      common /ubpcom/  dbgcol, pad039
      logical          dbghep, pad040
      common /ubpcom/  dbghep, pad040
      logical          dbgini, pad041
      common /ubpcom/  dbgini, pad041
      logical          dbgmup, pad042
      common /ubpcom/  dbgmup, pad042
      logical          dbgsca, pad043
      common /ubpcom/  dbgsca, pad043
*KEEP,UBCBRN.
      double precision brnmax
      common /ubcbrn/ brnmax
*KEND.
      double precision z, zp, zm, zdum, tmp
      character*55 msgbuf
      double precision EPS
      parameter (EPS = 1.d-5)
      double precision ubibn, ubxtot, ubumin
      external ubibn, ubxtot, ubumin
c find Z0 peak
      z  =  mass2z
      zp = (mass1z + gamm1z)**2
      zm = (mass1z - gamm1z)**2
      brnmax = - ubumin (zm, z, zp, ubibn, EPS, zdum)
      if (dbgini) then
         write (msgbuf,10) z, brnmax
 10      format ('Z0 peak @ s = ',e9.3,' GeV**2, brnmax = ',e9.3,' pb')
         call ubumsg ('ubibmx', 0, msgbuf)
      endif
c check lower end
      tmp = ubxtot (4.d0*emin*emin, taumin, taumax)
      brnmax = max (brnmax, tmp)
      if (dbgini) then
         write (msgbuf,20) 4.d0*emin*emin, tmp
 20      format ('At lower end: ',e9.3,' GeV**2: sigma  = ',e9.3,' pb')
         call ubumsg ('ubibmx', 0, msgbuf)
      endif
c account for numerical accuracy:
      brnmax = brnmax * (1.d0 + EPS)
      END
c UBIBN - returns minus the integrated trial cross section for
c         maximum search
      DOUBLE PRECISION FUNCTION UBIBN (S)
*KEEP,IMPLNONE.
c     implicit none
*KEND.
      double precision s
*KEEP,UBPCOM.
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
      double precision ebeam
      common /ubpcom/  ebeam
      double precision ctsmin
      common /ubpcom/  ctsmin
      double precision ctsmax
      common /ubpcom/  ctsmax
      double precision ecut
      common /ubpcom/  ecut
      double precision acocut
      common /ubpcom/  acocut
      integer          nevent, pad018
      common /ubpcom/  nevent, pad018
      integer          rseed , pad019
      common /ubpcom/  rseed , pad019
      logical          tchann, pad020
      common /ubpcom/  tchann, pad020
      logical          weak  , pad021
      common /ubpcom/  weak  , pad021
      logical          boxes , pad022
      common /ubpcom/  boxes , pad022
      double precision taumin
      common /ubpcom/  taumin
      double precision taumax
      common /ubpcom/  taumax
      double precision emin
      common /ubpcom/  emin
      double precision epsiln
      common /ubpcom/  epsiln
      double precision qsq
      common /ubpcom/  qsq
      integer          bstyle, pad028
      common /ubpcom/  bstyle, pad028
      integer          stdin , pad029
      common /ubpcom/  stdin , pad029
      integer          stdout, pad030
      common /ubpcom/  stdout, pad030
      integer          stderr, pad031
      common /ubpcom/  stderr, pad031
      integer          errcnt, pad032
      common /ubpcom/  errcnt, pad032
      integer          errmax, pad033
      common /ubpcom/  errmax, pad033
      integer          verbos, pad034
      common /ubpcom/  verbos, pad034
      integer          status, pad035
      common /ubpcom/  status, pad035
      integer          hepdat, pad036
      common /ubpcom/  hepdat, pad036
      integer          heprev, pad037
      common /ubpcom/  heprev, pad037
      integer          runid , pad038
      common /ubpcom/  runid , pad038
      logical          dbgcol, pad039
      common /ubpcom/  dbgcol, pad039
      logical          dbghep, pad040
      common /ubpcom/  dbghep, pad040
      logical          dbgini, pad041
      common /ubpcom/  dbgini, pad041
      logical          dbgmup, pad042
      common /ubpcom/  dbgmup, pad042
      logical          dbgsca, pad043
      common /ubpcom/  dbgsca, pad043
*KEND.
      double precision ubxtot
      external ubxtot
      ubibn =  - ubxtot (s, taumin, taumax)
      END
      SUBROUTINE UBGEN (N)
*KEEP,IMPLNONE.
c     implicit none
*KEND.
      integer n
*KEEP,HEPEVT.
      integer NMXHEP
      parameter (NMXHEP = 2000)
      integer nevhep, nhep, isthep(NMXHEP), idhep(NMXHEP),
     $     jmohep(2,NMXHEP), jdahep(2,NMXHEP)
      real phep(5,NMXHEP), vhep(4,NMXHEP), shep(4,NMXHEP)
      common /hepevt/ nevhep, nhep, isthep, idhep, jmohep, jdahep,
     $     phep, vhep
      common /hepspn/ shep
*KEEP,UBCEVT.
      integer NMXINT
      parameter (NMXINT = 100)
      double precision p(5,NMXINT)
      common /ubcevt/ p
*KEEP,PDGCODES.
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
*KEEP,UBCSTA.
      double precision sumw, sumw2, maxws, maxwt
      common /ubcsta/  sumw, sumw2, maxws, maxwt
      integer isbad, iscall, isfail, isrej
      common /ubcsta/ isbad, iscall, isfail, isrej
      integer  itbad, itcall, itfail, callct, acccut
      common /ubcsta/  itbad, itcall, itfail, callct, acccut
*KEEP,UBPCOM.
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
      double precision ebeam
      common /ubpcom/  ebeam
      double precision ctsmin
      common /ubpcom/  ctsmin
      double precision ctsmax
      common /ubpcom/  ctsmax
      double precision ecut
      common /ubpcom/  ecut
      double precision acocut
      common /ubpcom/  acocut
      integer          nevent, pad018
      common /ubpcom/  nevent, pad018
      integer          rseed , pad019
      common /ubpcom/  rseed , pad019
      logical          tchann, pad020
      common /ubpcom/  tchann, pad020
      logical          weak  , pad021
      common /ubpcom/  weak  , pad021
      logical          boxes , pad022
      common /ubpcom/  boxes , pad022
      double precision taumin
      common /ubpcom/  taumin
      double precision taumax
      common /ubpcom/  taumax
      double precision emin
      common /ubpcom/  emin
      double precision epsiln
      common /ubpcom/  epsiln
      double precision qsq
      common /ubpcom/  qsq
      integer          bstyle, pad028
      common /ubpcom/  bstyle, pad028
      integer          stdin , pad029
      common /ubpcom/  stdin , pad029
      integer          stdout, pad030
      common /ubpcom/  stdout, pad030
      integer          stderr, pad031
      common /ubpcom/  stderr, pad031
      integer          errcnt, pad032
      common /ubpcom/  errcnt, pad032
      integer          errmax, pad033
      common /ubpcom/  errmax, pad033
      integer          verbos, pad034
      common /ubpcom/  verbos, pad034
      integer          status, pad035
      common /ubpcom/  status, pad035
      integer          hepdat, pad036
      common /ubpcom/  hepdat, pad036
      integer          heprev, pad037
      common /ubpcom/  heprev, pad037
      integer          runid , pad038
      common /ubpcom/  runid , pad038
      logical          dbgcol, pad039
      common /ubpcom/  dbgcol, pad039
      logical          dbghep, pad040
      common /ubpcom/  dbghep, pad040
      logical          dbgini, pad041
      common /ubpcom/  dbgini, pad041
      logical          dbgmup, pad042
      common /ubpcom/  dbgmup, pad042
      logical          dbgsca, pad043
      common /ubpcom/  dbgsca, pad043
*KEEP,UBCBRN.
      double precision brnmax
      common /ubcbrn/ brnmax
*KEND.
      double precision s, tau, ws, wt, weight, ecms, ecms2
      double precision etmp1, etmp2, factor, ubr
      double precision beta(4), ipele(4), ippos(4), opele(4), oppos(4)
      integer i, j, savpt1, savpt2
      character*55 msgbuf
      double precision ubxtot, ubrgen
      logical ubgacc
      external ubxtot, ubrgen, ubgacc
      double precision MASS1M
      parameter (MASS1M = 0.105658389d0)
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
c Generate S and gammas
      if (bstyle .eq. 1 .or. bstyle .eq. 3) then
         call ubbini (4)
         call ubbini (5)
      endif
c Partonic CMS
      do 210 i = 1, 4
         p(i,6) = p(i,4) + p(i,5)
 210  continue
      s = p(4,6)**2 - p(1,6)**2 - p(2,6)**2 - p(3,6)**2
c Reject event if c.m.s. energy unphysical:
      if (s .le. 4.d0*emin**2) then
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
CHMH Reject event here if s weight is already smaller than random number,
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
c The total event weight is the product of weights from s and t generation
      weight = ws * wt
      sumw  = sumw  + weight
      sumw2 = sumw2 + weight**2
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
c Generate final state radiation
      if (bstyle .eq. 2 .or. bstyle .eq. 3) then
c Electron and Positron in c.m.s. before final state radiation
         do 320 i = 1, 4
            p(i,9)  = opele(i)
            p(i,10) = oppos(i)
 320     continue
c Save pointer to first photon emitted from first final state lepton.
         savpt1 = nhep + 1
         call ubbfin (9, s)
c Calculate total energy in branching of first lepton
         etmp1 = p(4,9)
         do 400 i = savpt1, nhep
            etmp1 = etmp1 + p(4,i)
 400     continue
c Take care of soft photon case when no explicit photon was generated!
         etmp1 = max (etmp1, ecms2)
         savpt2 = nhep + 1
         call ubbfin (10, s)
c Calculate total energy in branching of second lepton
         etmp2 = p(4,10)
         do 410 i = savpt2, nhep
            etmp2 = etmp2 + p(4,i)
 410     continue
         etmp2 = max (etmp2, ecms2)
c Now do the rescaling of the (massless) momenta of the particles
c generated in the final state branching and boost them back to the
c lab system.
         factor = ecms / (etmp1 + etmp2)
c Leptons ...
         do 420 i = 1, 4
            opele(i) = p(i,9)  * factor
            oppos(i) = p(i,10) * factor
 420     continue
         call ububoo (beta, opele, p(1,9))
         call ububoo (beta, oppos, p(1,10))
c ... and photons:
         do 430 j = savpt1, nhep
            do 440 i = 1, 4
               opele(i) = p(i,j) * factor
 440        continue
            call ububoo (beta, opele, p(1,j))
 430     continue
      else
c No final state radiation: just copy electron and positron momenta
      do 500 i = 1, 4
         p(i,9)  = p(i,7)
         p(i,10) = p(i,8)
 500  continue
      endif
c Check if event passes our given cuts:
      callct = callct + 1
      if (.not. ubgacc()) then
         goto 10
      endif
      acccut = acccut + 1
c Finally take care of special mode for muon pair production:
      if (dbgmup) then
         idhep(9) = PDGMU
         idhep(10) = -PDGMU
         p(5,9)  = MASS1M
         p(5,10) = MASS1M
c in principle one should rescale the momenta of the outgoing muons,
c since the electrons were on mass shell (with mass = 0) ...
c but this mode is supposed to not to be used by the normal user, anyway
      endif
c Copy particle momenta to /HEPEVT/ common block.
      do 600 i = 1, nhep
         do 610 j = 1, 5
            phep(j,i) = p(j,i)
 610     continue
 600  continue
      END
      SUBROUTINE UBGT (S, TAU, WEIGHT)
*KEEP,IMPLNONE.
c     implicit none
*KEND.
      double precision s, tau, weight
*KEEP,UBPCOM.
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
      double precision ebeam
      common /ubpcom/  ebeam
      double precision ctsmin
      common /ubpcom/  ctsmin
      double precision ctsmax
      common /ubpcom/  ctsmax
      double precision ecut
      common /ubpcom/  ecut
      double precision acocut
      common /ubpcom/  acocut
      integer          nevent, pad018
      common /ubpcom/  nevent, pad018
      integer          rseed , pad019
      common /ubpcom/  rseed , pad019
      logical          tchann, pad020
      common /ubpcom/  tchann, pad020
      logical          weak  , pad021
      common /ubpcom/  weak  , pad021
      logical          boxes , pad022
      common /ubpcom/  boxes , pad022
      double precision taumin
      common /ubpcom/  taumin
      double precision taumax
      common /ubpcom/  taumax
      double precision emin
      common /ubpcom/  emin
      double precision epsiln
      common /ubpcom/  epsiln
      double precision qsq
      common /ubpcom/  qsq
      integer          bstyle, pad028
      common /ubpcom/  bstyle, pad028
      integer          stdin , pad029
      common /ubpcom/  stdin , pad029
      integer          stdout, pad030
      common /ubpcom/  stdout, pad030
      integer          stderr, pad031
      common /ubpcom/  stderr, pad031
      integer          errcnt, pad032
      common /ubpcom/  errcnt, pad032
      integer          errmax, pad033
      common /ubpcom/  errmax, pad033
      integer          verbos, pad034
      common /ubpcom/  verbos, pad034
      integer          status, pad035
      common /ubpcom/  status, pad035
      integer          hepdat, pad036
      common /ubpcom/  hepdat, pad036
      integer          heprev, pad037
      common /ubpcom/  heprev, pad037
      integer          runid , pad038
      common /ubpcom/  runid , pad038
      logical          dbgcol, pad039
      common /ubpcom/  dbgcol, pad039
      logical          dbghep, pad040
      common /ubpcom/  dbghep, pad040
      logical          dbgini, pad041
      common /ubpcom/  dbgini, pad041
      logical          dbgmup, pad042
      common /ubpcom/  dbgmup, pad042
      logical          dbgsca, pad043
      common /ubpcom/  dbgsca, pad043
*KEND.
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
      END
      SUBROUTINE UBGPPR (E, TAU, IPELE, IPPOS, OPELE, OPPOS)
*KEEP,IMPLNONE.
c     implicit none
*KEND.
      double precision e, tau
      double precision ipele(4), ippos(4), opele(4), oppos(4)
*KEEP,UBCNST.
      double precision PI
      parameter (PI = 3.141592653589793238d0)
*KEND.
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
      END
      LOGICAL FUNCTION UBGACC ()
C UBGACC - logical function which check if event passed given cuts.
*KEEP,IMPLNONE.
c     implicit none
*KEND.
*KEEP,UBCEVT.
      integer NMXINT
      parameter (NMXINT = 100)
      double precision p(5,NMXINT)
      common /ubcevt/ p
*KEEP,UBPCOM.
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
      double precision ebeam
      common /ubpcom/  ebeam
      double precision ctsmin
      common /ubpcom/  ctsmin
      double precision ctsmax
      common /ubpcom/  ctsmax
      double precision ecut
      common /ubpcom/  ecut
      double precision acocut
      common /ubpcom/  acocut
      integer          nevent, pad018
      common /ubpcom/  nevent, pad018
      integer          rseed , pad019
      common /ubpcom/  rseed , pad019
      logical          tchann, pad020
      common /ubpcom/  tchann, pad020
      logical          weak  , pad021
      common /ubpcom/  weak  , pad021
      logical          boxes , pad022
      common /ubpcom/  boxes , pad022
      double precision taumin
      common /ubpcom/  taumin
      double precision taumax
      common /ubpcom/  taumax
      double precision emin
      common /ubpcom/  emin
      double precision epsiln
      common /ubpcom/  epsiln
      double precision qsq
      common /ubpcom/  qsq
      integer          bstyle, pad028
      common /ubpcom/  bstyle, pad028
      integer          stdin , pad029
      common /ubpcom/  stdin , pad029
      integer          stdout, pad030
      common /ubpcom/  stdout, pad030
      integer          stderr, pad031
      common /ubpcom/  stderr, pad031
      integer          errcnt, pad032
      common /ubpcom/  errcnt, pad032
      integer          errmax, pad033
      common /ubpcom/  errmax, pad033
      integer          verbos, pad034
      common /ubpcom/  verbos, pad034
      integer          status, pad035
      common /ubpcom/  status, pad035
      integer          hepdat, pad036
      common /ubpcom/  hepdat, pad036
      integer          heprev, pad037
      common /ubpcom/  heprev, pad037
      integer          runid , pad038
      common /ubpcom/  runid , pad038
      logical          dbgcol, pad039
      common /ubpcom/  dbgcol, pad039
      logical          dbghep, pad040
      common /ubpcom/  dbghep, pad040
      logical          dbgini, pad041
      common /ubpcom/  dbgini, pad041
      logical          dbgmup, pad042
      common /ubpcom/  dbgmup, pad042
      logical          dbgsca, pad043
      common /ubpcom/  dbgsca, pad043
*KEEP,UBCMSC.
      double precision cacomx, logeps
      common /ubcmsc/ cacomx, logeps
*KEND.
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
      END
      SUBROUTINE UBCLOS (N)
*KEEP,IMPLNONE.
c     implicit none
*KEND.
      integer n
*KEEP,UBCBRN.
      double precision brnmax
      common /ubcbrn/ brnmax
*KEEP,UBCSTA.
      double precision sumw, sumw2, maxws, maxwt
      common /ubcsta/  sumw, sumw2, maxws, maxwt
      integer isbad, iscall, isfail, isrej
      common /ubcsta/ isbad, iscall, isfail, isrej
      integer  itbad, itcall, itfail, callct, acccut
      common /ubcsta/  itbad, itcall, itfail, callct, acccut
*KEND.
      double precision xsct, errmc, CONV
c The conversion factor from pbarn to mbarn
      parameter (CONV = 1.e-9)
      if (iscall .eq. 0) then
         call ubumsg ('ubclos', 100, 'No events generated!')
      endif
c Calculate the total X-section
      xsct = brnmax * CONV * dble (acccut) / dble (iscall)
c Estimate the error of the Monte Carlo integration:
      errmc = brnmax * CONV
     &      * sqrt ((dble (iscall-acccut) * acccut) / dble (iscall)**3)
      call ubeens (n, xsct, errmc)
      END
      BLOCK DATA UBPINI
c Initialize the variables in the lookup table UBP
*KEEP,UBPCOM.
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
      double precision ebeam
      common /ubpcom/  ebeam
      double precision ctsmin
      common /ubpcom/  ctsmin
      double precision ctsmax
      common /ubpcom/  ctsmax
      double precision ecut
      common /ubpcom/  ecut
      double precision acocut
      common /ubpcom/  acocut
      integer          nevent, pad018
      common /ubpcom/  nevent, pad018
      integer          rseed , pad019
      common /ubpcom/  rseed , pad019
      logical          tchann, pad020
      common /ubpcom/  tchann, pad020
      logical          weak  , pad021
      common /ubpcom/  weak  , pad021
      logical          boxes , pad022
      common /ubpcom/  boxes , pad022
      double precision taumin
      common /ubpcom/  taumin
      double precision taumax
      common /ubpcom/  taumax
      double precision emin
      common /ubpcom/  emin
      double precision epsiln
      common /ubpcom/  epsiln
      double precision qsq
      common /ubpcom/  qsq
      integer          bstyle, pad028
      common /ubpcom/  bstyle, pad028
      integer          stdin , pad029
      common /ubpcom/  stdin , pad029
      integer          stdout, pad030
      common /ubpcom/  stdout, pad030
      integer          stderr, pad031
      common /ubpcom/  stderr, pad031
      integer          errcnt, pad032
      common /ubpcom/  errcnt, pad032
      integer          errmax, pad033
      common /ubpcom/  errmax, pad033
      integer          verbos, pad034
      common /ubpcom/  verbos, pad034
      integer          status, pad035
      common /ubpcom/  status, pad035
      integer          hepdat, pad036
      common /ubpcom/  hepdat, pad036
      integer          heprev, pad037
      common /ubpcom/  heprev, pad037
      integer          runid , pad038
      common /ubpcom/  runid , pad038
      logical          dbgcol, pad039
      common /ubpcom/  dbgcol, pad039
      logical          dbghep, pad040
      common /ubpcom/  dbghep, pad040
      logical          dbgini, pad041
      common /ubpcom/  dbgini, pad041
      logical          dbgmup, pad042
      common /ubpcom/  dbgmup, pad042
      logical          dbgsca, pad043
      common /ubpcom/  dbgsca, pad043
*KEND.
      data ahpla  /137.0359895         /
      data alpha  /0.0d0               /
      data mass1e /0.51099906d-3       /
      data mass2e /0.0d0               /
      data mass1z /91.187              /
      data mass2z /0.0d0               /
      data mass1t /150.0               /
      data mass1h /250.0               /
      data mass1w /0.0d0               /
      data gamm1z /0.0d0               /
      data gamm2z /0.0d0               /
      data sin2w  /0.0d0               /
      data ebeam  /46.0d0              /
      data ctsmin /-0.9                /
      data ctsmax /0.9                 /
      data ecut   /20.                 /
      data acocut /20.                 /
      data nevent /10000               /
      data rseed  /54217137            /
      data tchann /.true.              /
      data weak   /.true.              /
      data boxes  /.true.              /
      data taumin /0.0d0               /
      data taumax /0.0d0               /
      data emin   /0.0d0               /
      data epsiln /1.0d-5              /
      data qsq    /0.0d0               /
      data bstyle /3                   /
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
      data dbgmup /.false.             /
      data dbgsca /.false.             /
      END
c Perform a simple forward branching on the lepton stored as particle
c number PTR in /hepevt/.   Store the generated photons directly
c in /ubcevt/.
      SUBROUTINE UBBINI (PTR)
*KEEP,IMPLNONE.
c     implicit none
*KEND.
      integer ptr
*KEEP,UBCEVT.
      integer NMXINT
      parameter (NMXINT = 100)
      double precision p(5,NMXINT)
      common /ubcevt/ p
*KEEP,PDGCODES.
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
*KEEP,UBCNST.
      double precision PI
      parameter (PI = 3.141592653589793238d0)
*KEEP,UBPCOM.
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
      double precision ebeam
      common /ubpcom/  ebeam
      double precision ctsmin
      common /ubpcom/  ctsmin
      double precision ctsmax
      common /ubpcom/  ctsmax
      double precision ecut
      common /ubpcom/  ecut
      double precision acocut
      common /ubpcom/  acocut
      integer          nevent, pad018
      common /ubpcom/  nevent, pad018
      integer          rseed , pad019
      common /ubpcom/  rseed , pad019
      logical          tchann, pad020
      common /ubpcom/  tchann, pad020
      logical          weak  , pad021
      common /ubpcom/  weak  , pad021
      logical          boxes , pad022
      common /ubpcom/  boxes , pad022
      double precision taumin
      common /ubpcom/  taumin
      double precision taumax
      common /ubpcom/  taumax
      double precision emin
      common /ubpcom/  emin
      double precision epsiln
      common /ubpcom/  epsiln
      double precision qsq
      common /ubpcom/  qsq
      integer          bstyle, pad028
      common /ubpcom/  bstyle, pad028
      integer          stdin , pad029
      common /ubpcom/  stdin , pad029
      integer          stdout, pad030
      common /ubpcom/  stdout, pad030
      integer          stderr, pad031
      common /ubpcom/  stderr, pad031
      integer          errcnt, pad032
      common /ubpcom/  errcnt, pad032
      integer          errmax, pad033
      common /ubpcom/  errmax, pad033
      integer          verbos, pad034
      common /ubpcom/  verbos, pad034
      integer          status, pad035
      common /ubpcom/  status, pad035
      integer          hepdat, pad036
      common /ubpcom/  hepdat, pad036
      integer          heprev, pad037
      common /ubpcom/  heprev, pad037
      integer          runid , pad038
      common /ubpcom/  runid , pad038
      logical          dbgcol, pad039
      common /ubpcom/  dbgcol, pad039
      logical          dbghep, pad040
      common /ubpcom/  dbghep, pad040
      logical          dbgini, pad041
      common /ubpcom/  dbgini, pad041
      logical          dbgmup, pad042
      common /ubpcom/  dbgmup, pad042
      logical          dbgsca, pad043
      common /ubpcom/  dbgsca, pad043
*KEEP,UBCMSC.
      double precision cacomx, logeps
      common /ubcmsc/ cacomx, logeps
*KEND.
      integer MAXPHO
      parameter (MAXPHO=20)
      double precision ubrgen
      external ubrgen
      double precision pgam(5)
      double precision nbar, p0, t, r, wgt, kt, phi, beta, x,
     $                 ztmp, ttmp, zphot(MAXPHO), tphot(MAXPHO)
      integer n, nphot, i, j
c Calculate the probability for no emission
      if (dbgsca) then
c        for debugging of scale dependencies ... only log(s/m^2)
         beta = (alpha / PI) *  log (qsq/mass2e)
      else
c        full result: log(s/m^2) - 1
         beta = (alpha / PI) * (log (qsq/mass2e) - 1.d0)
      endif
      nbar = 0.5d0 * beta * (-2.0d0 * logeps - 0.5d0 * epsiln**2
     $     + 2.0d0 * epsiln - 1.5d0)
c Standard Poisson algorithm
      p0 = exp (-nbar)
      r = ubrgen (0)
      nphot = 0
 4    if (r.gt.p0 .and. nphot.lt.MAXPHO) then
         r = r * ubrgen (0)
         nphot = nphot +1
         goto 4
      endif
      if (nphot .eq. 0) then
c Smoothly continue with a soft photon (exponentiated) part.
         r = ubrgen (0)
c protect IBM/3090 against numerical underflow...
         if (r .ge. 10.d0**(-36.d0 * beta)) then
            x = 1.d0 - epsiln * r**(1.d0/beta)
         else
            x = 1.d0
         endif
         do 10 n = 1, 4
            p(n,ptr) = p(n,ptr) * x
 10      continue
         return
      endif
c Generate (1+(1-x)**2)/x for each photon
      do 30 n = 1, nphot
 40      continue
         x = exp (ubrgen (0) * logeps)
         wgt = 0.5d0 * (1.0d0 + (1.d0 - x)**2)
         if (ubrgen (0) .ge. wgt) goto 40
         zphot(n) = x
 30   continue
c Transversal momenta: First generate (1-cos(theta))/2
      t = mass2e/qsq
      do 35 n = 1, nphot
         tphot(n) = t ** ubrgen (0)
 35   continue
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
c Generating the photons
      pgam(5) = 0.0
      do 55 n = 1, nphot
         phi = 2.d0 * PI * ubrgen (0)
         pgam(4) = sqrt (p(1,ptr)**2 + p(2,ptr)**2 + p(3,ptr)**2)
     &           * zphot(n)
         if (dbgcol) then
c Strict collinear approximation (for debugging purposes only)
            tphot(n) = 0.d0
            kt = 0.d0
         else
            kt = pgam(4) * sqrt (4.0d0 * tphot(n) * (1.0d0 - tphot(n)))
         endif
         pgam(1) = cos(phi) * kt
         pgam(2) = sin(phi) * kt
         pgam(3) = (1.0d0 - 2.0d0 * tphot(n)) * sign(pgam(4), p(3,ptr))
c Store the photon
         call ubeent (1, PDGGAM, ptr, pgam)
c Update the electron, using strict four-momentum conservation
         p(1,ptr) = p(1,ptr) - pgam(1)
         p(2,ptr) = p(2,ptr) - pgam(2)
         p(3,ptr) = p(3,ptr) - pgam(3)
         p(4,ptr) = p(4,ptr) - pgam(4)
 55   continue
      END
c Generate final state photons, using a time-like parton shower.
c Store the generated photons directly in /ubcevt/.
      SUBROUTINE UBBFIN (PTR, SCALE)
*KEEP,IMPLNONE.
c     implicit none
*KEND.
      integer ptr
      double precision scale
*KEEP,UBCEVT.
      integer NMXINT
      parameter (NMXINT = 100)
      double precision p(5,NMXINT)
      common /ubcevt/ p
*KEEP,PDGCODES.
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
*KEEP,UBCNST.
      double precision PI
      parameter (PI = 3.141592653589793238d0)
*KEEP,UBPCOM.
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
      double precision ebeam
      common /ubpcom/  ebeam
      double precision ctsmin
      common /ubpcom/  ctsmin
      double precision ctsmax
      common /ubpcom/  ctsmax
      double precision ecut
      common /ubpcom/  ecut
      double precision acocut
      common /ubpcom/  acocut
      integer          nevent, pad018
      common /ubpcom/  nevent, pad018
      integer          rseed , pad019
      common /ubpcom/  rseed , pad019
      logical          tchann, pad020
      common /ubpcom/  tchann, pad020
      logical          weak  , pad021
      common /ubpcom/  weak  , pad021
      logical          boxes , pad022
      common /ubpcom/  boxes , pad022
      double precision taumin
      common /ubpcom/  taumin
      double precision taumax
      common /ubpcom/  taumax
      double precision emin
      common /ubpcom/  emin
      double precision epsiln
      common /ubpcom/  epsiln
      double precision qsq
      common /ubpcom/  qsq
      integer          bstyle, pad028
      common /ubpcom/  bstyle, pad028
      integer          stdin , pad029
      common /ubpcom/  stdin , pad029
      integer          stdout, pad030
      common /ubpcom/  stdout, pad030
      integer          stderr, pad031
      common /ubpcom/  stderr, pad031
      integer          errcnt, pad032
      common /ubpcom/  errcnt, pad032
      integer          errmax, pad033
      common /ubpcom/  errmax, pad033
      integer          verbos, pad034
      common /ubpcom/  verbos, pad034
      integer          status, pad035
      common /ubpcom/  status, pad035
      integer          hepdat, pad036
      common /ubpcom/  hepdat, pad036
      integer          heprev, pad037
      common /ubpcom/  heprev, pad037
      integer          runid , pad038
      common /ubpcom/  runid , pad038
      logical          dbgcol, pad039
      common /ubpcom/  dbgcol, pad039
      logical          dbghep, pad040
      common /ubpcom/  dbghep, pad040
      logical          dbgini, pad041
      common /ubpcom/  dbgini, pad041
      logical          dbgmup, pad042
      common /ubpcom/  dbgmup, pad042
      logical          dbgsca, pad043
      common /ubpcom/  dbgsca, pad043
*KEEP,UBCMSC.
      double precision cacomx, logeps
      common /ubcmsc/ cacomx, logeps
*KEND.
      integer MAXPHO
      parameter (MAXPHO=20)
      double precision ubrgen
      external ubrgen
      double precision pgam(5)
      double precision nbar, p0, t, wgt, phi, r, beta, x,
     $                 ztmp, ttmp, zphot(MAXPHO), tphot(MAXPHO)
      integer n, nphot, i, j
      double precision eo1(3), eo2(3), eo3(3), lept(4)
      double precision cosphi, sinphi, costh, sinth
      double precision tmp1, tmp2, big, MASS2M
      parameter (MASS2M = 0.105658389d0**2)
c Calculate the probability for no emission
      if (dbgmup) then
c Final state corrections for muon pair production: (debugging only)
         big = scale / MASS2M
      else
         big = scale / mass2e
      endif
      if (dbgsca) then
c        for debugging of scale dependencies ... only log(s'/m^2)
         beta = (alpha / PI) *  log (big)
      else
c        full result: log(s'/m^2) - 1
         beta = (alpha / PI) * (log (big) - 1.d0)
      endif
      nbar = 0.5d0 * beta * (-2.0d0 * logeps - 0.5d0 * epsiln**2
     $     + 2.0d0 * epsiln - 1.5d0)
c Standard Poisson algorithm
      p0 = exp (-nbar)
      r = ubrgen (0)
      nphot = 0
 4    if (r.gt.p0 .and. nphot.lt.MAXPHO) then
         r = r * ubrgen (0)
         nphot = nphot + 1
         goto 4
      endif
      if (nphot .eq. 0) then
c Smoothly continue with a soft photon (exponentiated) part.
         r = ubrgen(0)
c protect IBM/3090 against numerical underflow...
         if (r .ge. 10.d0**(-36.d0 * beta)) then
            x = 1.d0 - epsiln * r**(1.d0/beta)
         else
            x = 1.d0
         endif
         do 10 n = 1, 4
            p(n,ptr) = p(n,ptr) * x
 10      continue
         return
      endif
c Generate (1+(1-x)**2)/x for each photon
      do 30 n = 1, nphot
 40      continue
         x = exp (ubrgen (0) * logeps)
         wgt = 0.5d0 * (1.0d0 + (1.d0 - x)**2)
         if (ubrgen(0) .ge. wgt) goto 40
         zphot(n) = x
 30   continue
c Transversal momenta: First generate (1-cos(theta))/2
      t = 1.d0 / big
      do 35 n = 1, nphot
         tphot(n) = t ** ubrgen (0)
 35   continue
c Now we must sort virtualities in descending order.
c We do it by straight insertion, as the number of photons is small.
      do 45 j = 2, nphot
         ztmp = zphot(j)
         ttmp = tphot(j)
         do 46 i = j-1, 1,-1
            if (zphot(i)*tphot(i) .ge. ztmp*ttmp) goto 48
            zphot(i+1) = zphot(i)
            tphot(i+1) = tphot(i)
 46      continue
         i=0
 48      continue
         zphot(i+1) = ztmp
         tphot(i+1) = ttmp
 45   continue
c One by one, radiate some photons
      pgam(5) = 0.0
      do 200 n = 1, nphot
         phi = 2.d0 * PI * ubrgen (0)
         if (dbgcol) then
c Strict collinear approximation (for debugging purposes only)
            tphot(n) = 0.d0
         endif
         tmp1 = 1.d0 - 2.d0 * tphot(n)
         tmp2 = 1.d0 - tmp1**2
         costh = tmp1 * sqrt(1.d0 - zphot(n)**2 * tmp2)
     &           + zphot(n) * tmp2
         sinth = sqrt(1.d0 - costh**2)
         cosphi = cos(phi)
         sinphi = sin(phi)
         do 190 i = 1, 4
            lept(i) = p(i,ptr)
 190     continue
c Construct an orthonormal basis
         call ubuort (lept, eo1, eo2, eo3)
         pgam(4) = p(4,ptr) * zphot(n)
         do 165 i = 1, 3
            pgam(i) = (eo3(i) * costh + sinth * ( eo1(i)
     &                        * cosphi + eo2(i) * sinphi))
     &                        * pgam(4)
 165     continue
c Update the electron momentum
         p(1,ptr) = p(1,ptr) - pgam(1)
         p(2,ptr) = p(2,ptr) - pgam(2)
         p(3,ptr) = p(3,ptr) - pgam(3)
         p(4,ptr) = sqrt(p(1,ptr)**2 + p(2,ptr)**2 + p(3,ptr)**2)
c Store the photon
         call ubeent (1, PDGGAM, ptr, pgam)
 200  continue
      END
c UBXTOT - the integrated trial cross section
      FUNCTION UBXTOT (S, TAUMIN, TAUMAX)
*KEEP,IMPLNONE.
c     implicit none
*KEND.
      double precision ubxtot, s, taumin, taumax
      double precision a, b, prefac
c setup of the coefficients:
      call ubxcof (s, a, b, prefac)
      ubxtot = prefac * (a*(taumax-taumin) + b*(1./taumin-1./taumax))
      END
c UBXTRI - the differential trial cross section  d(sigma)/d(cos(theta))
c          which is a majorant of the full differential cross section.
      FUNCTION UBXTRI (S, TAU)
*KEEP,IMPLNONE.
c     implicit none
*KEND.
      double precision ubxtri, s, tau
      double precision a, b, prefac
c setup of the coefficients:
      call ubxcof (s, a, b, prefac)
c calculate the differential trial cross section
c Remember: d(sigma)/d(cos(theta)) = 0.5 * d(sigma)/d(tau)
      ubxtri = 0.5d0 * prefac * (a + b / tau**2)
      END
c UBXCOF - calculate the coefficients of the trial cross section,
c          where the differential trial cross section has the form
c          d(sigma)/d(tau) = prefac * ( A(s) + B(s)/tau**2 )
      SUBROUTINE UBXCOF (S, A, B, PREFAC)
*KEEP,IMPLNONE.
c     implicit none
*KEND.
      double precision s, a, b, prefac
*KEEP,UBPCOM.
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
      double precision ebeam
      common /ubpcom/  ebeam
      double precision ctsmin
      common /ubpcom/  ctsmin
      double precision ctsmax
      common /ubpcom/  ctsmax
      double precision ecut
      common /ubpcom/  ecut
      double precision acocut
      common /ubpcom/  acocut
      integer          nevent, pad018
      common /ubpcom/  nevent, pad018
      integer          rseed , pad019
      common /ubpcom/  rseed , pad019
      logical          tchann, pad020
      common /ubpcom/  tchann, pad020
      logical          weak  , pad021
      common /ubpcom/  weak  , pad021
      logical          boxes , pad022
      common /ubpcom/  boxes , pad022
      double precision taumin
      common /ubpcom/  taumin
      double precision taumax
      common /ubpcom/  taumax
      double precision emin
      common /ubpcom/  emin
      double precision epsiln
      common /ubpcom/  epsiln
      double precision qsq
      common /ubpcom/  qsq
      integer          bstyle, pad028
      common /ubpcom/  bstyle, pad028
      integer          stdin , pad029
      common /ubpcom/  stdin , pad029
      integer          stdout, pad030
      common /ubpcom/  stdout, pad030
      integer          stderr, pad031
      common /ubpcom/  stderr, pad031
      integer          errcnt, pad032
      common /ubpcom/  errcnt, pad032
      integer          errmax, pad033
      common /ubpcom/  errmax, pad033
      integer          verbos, pad034
      common /ubpcom/  verbos, pad034
      integer          status, pad035
      common /ubpcom/  status, pad035
      integer          hepdat, pad036
      common /ubpcom/  hepdat, pad036
      integer          heprev, pad037
      common /ubpcom/  heprev, pad037
      integer          runid , pad038
      common /ubpcom/  runid , pad038
      logical          dbgcol, pad039
      common /ubpcom/  dbgcol, pad039
      logical          dbghep, pad040
      common /ubpcom/  dbghep, pad040
      logical          dbgini, pad041
      common /ubpcom/  dbgini, pad041
      logical          dbgmup, pad042
      common /ubpcom/  dbgmup, pad042
      logical          dbgsca, pad043
      common /ubpcom/  dbgsca, pad043
*KEEP,PARAMS.
      integer nrmass
      PARAMETER ( NRMASS = 9 )
*KEEP,MASSES.
      double precision RMASS(0:NRMASS), RMASS2(0:NRMASS),
     &          VF(0:NRMASS), AF(0:NRMASS), QF(0:NRMASS)
      COMMON/ MASSES / RMASS, RMASS2, VF, AF, QF
*KEEP,OPTION.
      integer IWEAK, IFERM, ICHANN, IBOXES, IOUT
      COMMON/ OPTION / IWEAK, IFERM, ICHANN, IBOXES, IOUT
*KEEP,ADHOC.
      double precision ALFA, HBARC2, PI, FACQCB, FACQCD, FACQED
      COMMON/ ADHOC  / ALFA, HBARC2, PI, FACQCB, FACQCD, FACQED
*KEND.
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
      a = 0.5d0 * chiz2
     &  * ((vf(1)**2 - af(1)**2 + (s-mass2z)/s)**2 + mass2z*gamm2z/s**2)
c the coefficient of 1/tau**2:
      b = 1.d0 + FUDGE * chiz2 * (vf(1)**2+af(1)**2) * abs (s-mass2z)/s
      prefac = 4.d0 * HBARC2 * PI * alfeff**2 / s
      END
c UBXDIF - differential Bhabha cross section d(sigma)/d(cos(theta))
c          including weak corrections.
c          This function is intended as as interface to the
c          electroweak library.
      DOUBLE PRECISION FUNCTION UBXDIF (S, TAU)
*KEEP,IMPLNONE.
c     implicit none
*KEND.
      double precision s, tau
      double precision costh
      double precision eeeew
      external eeeew
c Call the (modified) function from ALIBABA
      costh = 1.d0 - 2.d0 * tau
      ubxdif = eeeew (s, costh)
      END
      DOUBLE PRECISION FUNCTION UBRGEN (ISEED)
c Random Number Generator:
c ------------------------
c We use the random number generator described in MZ87.
c The following seeds can be changed to any 0 <= IJ <= 31328,
c 0 <= KL <= 30081, but the given ones can be used for checking
c the executable.  The random numbers from 20001 to 20006 should read
c (after multiplication with 2**24): 6533892.0, 14220222.0, 7275067.0,
c 6172232.0, 8354498.0, and 10633180.0.
*KEEP,IMPLNONE.
c     implicit none
*KEND.
      integer iseed
      real    rndm
      ubrgen = dble(rndm(iseed))
      return
      END
      SUBROUTINE UBUBOO (V, XINIT, XFINAL)
*KEEP,IMPLNONE.
c     implicit none
*KEND.
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
      END
c Find the minimum of F in the interval (AX, CX), using a golden
c section search algorithm.
c Adapted from the routine 'GOLDEN' from 'NUMERICAL RECIPES'
      DOUBLE PRECISION FUNCTION UBUMIN (AX, BX, CX, F, TOL, XMIN)
*KEEP,IMPLNONE.
c     implicit none
*KEND.
      double precision ax, bx, cx, f, tol, xmin
      external f
      double precision R, C, FUDGE
      parameter (R = 0.61803399,
     $           C = 1.0 - r,
     $           FUDGE = 1.d-8)
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
      END
c Write the message MSG to the STDERR stream.
      SUBROUTINE UBUMSG (ORIGIN, LEVEL, MSG)
*KEEP,IMPLNONE.
c     implicit none
*KEND.
      character*(*) origin, msg
      integer level
*KEEP,UBPCOM.
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
      double precision ebeam
      common /ubpcom/  ebeam
      double precision ctsmin
      common /ubpcom/  ctsmin
      double precision ctsmax
      common /ubpcom/  ctsmax
      double precision ecut
      common /ubpcom/  ecut
      double precision acocut
      common /ubpcom/  acocut
      integer          nevent, pad018
      common /ubpcom/  nevent, pad018
      integer          rseed , pad019
      common /ubpcom/  rseed , pad019
      logical          tchann, pad020
      common /ubpcom/  tchann, pad020
      logical          weak  , pad021
      common /ubpcom/  weak  , pad021
      logical          boxes , pad022
      common /ubpcom/  boxes , pad022
      double precision taumin
      common /ubpcom/  taumin
      double precision taumax
      common /ubpcom/  taumax
      double precision emin
      common /ubpcom/  emin
      double precision epsiln
      common /ubpcom/  epsiln
      double precision qsq
      common /ubpcom/  qsq
      integer          bstyle, pad028
      common /ubpcom/  bstyle, pad028
      integer          stdin , pad029
      common /ubpcom/  stdin , pad029
      integer          stdout, pad030
      common /ubpcom/  stdout, pad030
      integer          stderr, pad031
      common /ubpcom/  stderr, pad031
      integer          errcnt, pad032
      common /ubpcom/  errcnt, pad032
      integer          errmax, pad033
      common /ubpcom/  errmax, pad033
      integer          verbos, pad034
      common /ubpcom/  verbos, pad034
      integer          status, pad035
      common /ubpcom/  status, pad035
      integer          hepdat, pad036
      common /ubpcom/  hepdat, pad036
      integer          heprev, pad037
      common /ubpcom/  heprev, pad037
      integer          runid , pad038
      common /ubpcom/  runid , pad038
      logical          dbgcol, pad039
      common /ubpcom/  dbgcol, pad039
      logical          dbghep, pad040
      common /ubpcom/  dbghep, pad040
      logical          dbgini, pad041
      common /ubpcom/  dbgini, pad041
      logical          dbgmup, pad042
      common /ubpcom/  dbgmup, pad042
      logical          dbgsca, pad043
      common /ubpcom/  dbgsca, pad043
*KEND.
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
      END
      SUBROUTINE UBUORT (VECTOR, E1, E2, E3)
*KEEP,IMPLNONE.
c     implicit none
*KEND.
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
      END
c Enter the identification of the Monte Carlo and the Run
      SUBROUTINE UBEENI (MCID, MCREV, MCDAT, RUNID, RUNDAT, RUNTIM)
*KEEP,IMPLNONE.
c     implicit none
*KEND.
      integer mcid, mcrev, mcdat, runid, rundat, runtim
*KEEP,HEPEVT.
      integer NMXHEP
      parameter (NMXHEP = 2000)
      integer nevhep, nhep, isthep(NMXHEP), idhep(NMXHEP),
     $     jmohep(2,NMXHEP), jdahep(2,NMXHEP)
      real phep(5,NMXHEP), vhep(4,NMXHEP), shep(4,NMXHEP)
      common /hepevt/ nevhep, nhep, isthep, idhep, jmohep, jdahep,
     $     phep, vhep
      common /hepspn/ shep
*KEND.
      nevhep = -1
      idhep(1) = mcid
      jdahep(1,1) = mcrev
      jdahep(2,1) = mcdat
      isthep(1) = runid
      jmohep(1,1) = rundat
      jmohep(2,1) = runtim
      END
      SUBROUTINE UBEENS (TOTEVT, XSECT, ERROR)
c Create a summary record.
c Write the estimated error to PHEP(1,2) as HERACLES does.
*KEEP,IMPLNONE.
c     implicit none
*KEND.
      integer totevt
      double precision xsect, error
*KEEP,HEPEVT.
      integer NMXHEP
      parameter (NMXHEP = 2000)
      integer nevhep, nhep, isthep(NMXHEP), idhep(NMXHEP),
     $     jmohep(2,NMXHEP), jdahep(2,NMXHEP)
      real phep(5,NMXHEP), vhep(4,NMXHEP), shep(4,NMXHEP)
      common /hepevt/ nevhep, nhep, isthep, idhep, jmohep, jdahep,
     $     phep, vhep
      common /hepspn/ shep
*KEND.
      nevhep = -2
      isthep(1) = totevt
      phep(1,1) = xsect
      phep(1,2) = error
      END
      SUBROUTINE UBEENT (STATUS, ID, MOTHER, PP)
*KEEP,IMPLNONE.
c     implicit none
*KEND.
      integer status, id, mother
      double precision pp(5)
*KEEP,HEPEVT.
      integer NMXHEP
      parameter (NMXHEP = 2000)
      integer nevhep, nhep, isthep(NMXHEP), idhep(NMXHEP),
     $     jmohep(2,NMXHEP), jdahep(2,NMXHEP)
      real phep(5,NMXHEP), vhep(4,NMXHEP), shep(4,NMXHEP)
      common /hepevt/ nevhep, nhep, isthep, idhep, jmohep, jdahep,
     $     phep, vhep
      common /hepspn/ shep
*KEEP,UBCEVT.
      integer NMXINT
      parameter (NMXINT = 100)
      double precision p(5,NMXINT)
      common /ubcevt/ p
*KEND.
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
      vhep(1,nhep) = 0.0
      vhep(2,nhep) = 0.0
      vhep(3,nhep) = 0.0
      vhep(4,nhep) = 0.0
      shep(1,nhep) = 0.0
      shep(2,nhep) = 0.0
      shep(3,nhep) = 0.0
      shep(4,nhep) = 1.0
      END
      SUBROUTINE UBENUL (IHEP)
*KEEP,IMPLNONE.
c     implicit none
*KEND.
      integer ihep
*KEEP,HEPEVT.
      integer NMXHEP
      parameter (NMXHEP = 2000)
      integer nevhep, nhep, isthep(NMXHEP), idhep(NMXHEP),
     $     jmohep(2,NMXHEP), jdahep(2,NMXHEP)
      real phep(5,NMXHEP), vhep(4,NMXHEP), shep(4,NMXHEP)
      common /hepevt/ nevhep, nhep, isthep, idhep, jmohep, jdahep,
     $     phep, vhep
      common /hepspn/ shep
*KEEP,UBCEVT.
      integer NMXINT
      parameter (NMXINT = 100)
      double precision p(5,NMXINT)
      common /ubcevt/ p
*KEND.
      isthep(ihep) = 0
      idhep(ihep) = 0
      jmohep(1,ihep) = 0
      jmohep(2,ihep) = 0
      jdahep(1,ihep) = 0
      jdahep(2,ihep) = 0
      p(1,ihep) = 0.0
      p(2,ihep) = 0.0
      p(3,ihep) = 0.0
      p(4,ihep) = 0.0
      p(5,ihep) = 0.0
      vhep(1,ihep) = 0.0
      vhep(2,ihep) = 0.0
      vhep(3,ihep) = 0.0
      vhep(4,ihep) = 0.0
      shep(1,ihep) = 0.0
      shep(2,ihep) = 0.0
      shep(3,ihep) = 0.0
      shep(4,ihep) = 1.0
      END
      SUBROUTINE UBENEW (N)
*KEEP,IMPLNONE.
c     implicit none
*KEND.
      integer n
*KEEP,HEPEVT.
      integer NMXHEP
      parameter (NMXHEP = 2000)
      integer nevhep, nhep, isthep(NMXHEP), idhep(NMXHEP),
     $     jmohep(2,NMXHEP), jdahep(2,NMXHEP)
      real phep(5,NMXHEP), vhep(4,NMXHEP), shep(4,NMXHEP)
      common /hepevt/ nevhep, nhep, isthep, idhep, jmohep, jdahep,
     $     phep, vhep
      common /hepspn/ shep
*KEND.
      nevhep = n
      nhep = 0
      END
      SUBROUTINE EWINIT (XMZ, XMH, XMT)
*     -----------------
* Initialization routine.
* The parameters are the masses of the Z, Higgs and top in GeV.
* All variables are input.
*KEEP,IMPLNONE.
c     implicit none
*KEND.
      double precision XMZ, XMH, XMT
*KEEP,UBPCOM.
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
      double precision ebeam
      common /ubpcom/  ebeam
      double precision ctsmin
      common /ubpcom/  ctsmin
      double precision ctsmax
      common /ubpcom/  ctsmax
      double precision ecut
      common /ubpcom/  ecut
      double precision acocut
      common /ubpcom/  acocut
      integer          nevent, pad018
      common /ubpcom/  nevent, pad018
      integer          rseed , pad019
      common /ubpcom/  rseed , pad019
      logical          tchann, pad020
      common /ubpcom/  tchann, pad020
      logical          weak  , pad021
      common /ubpcom/  weak  , pad021
      logical          boxes , pad022
      common /ubpcom/  boxes , pad022
      double precision taumin
      common /ubpcom/  taumin
      double precision taumax
      common /ubpcom/  taumax
      double precision emin
      common /ubpcom/  emin
      double precision epsiln
      common /ubpcom/  epsiln
      double precision qsq
      common /ubpcom/  qsq
      integer          bstyle, pad028
      common /ubpcom/  bstyle, pad028
      integer          stdin , pad029
      common /ubpcom/  stdin , pad029
      integer          stdout, pad030
      common /ubpcom/  stdout, pad030
      integer          stderr, pad031
      common /ubpcom/  stderr, pad031
      integer          errcnt, pad032
      common /ubpcom/  errcnt, pad032
      integer          errmax, pad033
      common /ubpcom/  errmax, pad033
      integer          verbos, pad034
      common /ubpcom/  verbos, pad034
      integer          status, pad035
      common /ubpcom/  status, pad035
      integer          hepdat, pad036
      common /ubpcom/  hepdat, pad036
      integer          heprev, pad037
      common /ubpcom/  heprev, pad037
      integer          runid , pad038
      common /ubpcom/  runid , pad038
      logical          dbgcol, pad039
      common /ubpcom/  dbgcol, pad039
      logical          dbghep, pad040
      common /ubpcom/  dbghep, pad040
      logical          dbgini, pad041
      common /ubpcom/  dbgini, pad041
      logical          dbgmup, pad042
      common /ubpcom/  dbgmup, pad042
      logical          dbgsca, pad043
      common /ubpcom/  dbgsca, pad043
*KEEP,PARAMS.
      integer nrmass
      PARAMETER ( NRMASS = 9 )
*KEEP,MASSES.
      double precision RMASS(0:NRMASS), RMASS2(0:NRMASS),
     &          VF(0:NRMASS), AF(0:NRMASS), QF(0:NRMASS)
      COMMON/ MASSES / RMASS, RMASS2, VF, AF, QF
*KEEP,FORMFA.
      COMPLEX*16 FZV,FZA,FGV,FGA
      COMMON/ FORMFA /FZV(0:NRMASS),FZA(0:NRMASS),
     +                FGV(0:NRMASS),FGA(0:NRMASS)
*KEEP,FORMMZ.
      COMPLEX*16 FZVMZ,FZAMZ,FGVMZ,FGAMZ
      COMMON/ FORMMZ /FZVMZ(0:NRMASS),FZAMZ(0:NRMASS),
     +                FGVMZ(0:NRMASS),FGAMZ(0:NRMASS)
*KEEP,BOSONS.
      double precision RMZ,RMW,RMH,RMT,SIN2TH,ZWID
      COMMON/ BOSONS / RMZ,RMW,RMH,RMT,SIN2TH,ZWID
*KEEP,ADHOC.
      double precision ALFA, HBARC2, PI, FACQCB, FACQCD, FACQED
      COMMON/ ADHOC  / ALFA, HBARC2, PI, FACQCB, FACQCD, FACQED
*KEEP,OPTION.
      integer IWEAK, IFERM, ICHANN, IBOXES, IOUT
      COMMON/ OPTION / IWEAK, IFERM, ICHANN, IBOXES, IOUT
*KEND.
      double precision IMSIGZ, IMZ2, alfas, z, bigpiz, rmwold
      integer i, niter
      CHARACTER*13 NAMES(0:NRMASS+1)
      DATA NAMES /'     neutrino','     electron','         muon',
     +            '          tau','     up quark','   down quark',
     +            '  charm quark','strange quark',
     +            '    top quark',' bottom quark','      hadrons'/
      RMZ    = XMZ
      RMH    = XMH
      RMT    = XMT
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
      ALFAS  = 0.12D0
* ALFA and HBARC2 from Particle Data Group Publ. 1990.
      ALFA   = 1D0 / 137.0359895D0
      HBARC2 = 0.38937966d9
      PI = 3.141592653589793238d0
* QCD and QED correction factors
      FACQCD = ALFAS/PI + (ALFAS/PI)**2*( 1.98D0 - 5D0*.115D0 )
      FACQCB = 0.045D0
      IF (ALFAS .LE. 0D0) THEN
        FACQCB = 0D0
      END IF
      FACQED = 3D0*ALFA/4D0/PI
* Starting value for sin**2(theta-w)
      SIN2TH = .2310D0
      RMW = RMZ*SQRT( 1D0 - SIN2TH )
* Iterate to find the value for sin**2(theta-w) and Mw
* After this all couplings and renormalization constants are defined.
      NITER = 20
      CALL COUPLS(SIN2TH,RMT)
      DO 110 I = 1 , NITER
        RMWOLD = RMW
        CALL RNORM()
        CALL COUPLS(SIN2TH,RMT)
        IF(ABS(RMWOLD-RMW)/RMW .LT. 1D-6) GOTO 130
  110 CONTINUE
      WRITE(*,120) NITER
  120 FORMAT(' The calculation of MW does not converge in',I4,' steps')
      STOP' We stop right here !'
  130 CONTINUE
C
C      WRITE(IOUT,150) (I,NAMES(I),RMASS(I),PWIDTH(I),I=0,NRMASS)
C  150 FORMAT(/,' The properties of the fermions:',/,3X,
C     +       ' label',7X,'name',4X,' mass (GeV)',
C     +       '  partial width of the Z (GeV)',/,
C     +       (' ',I6,1X,A13,1X,F12.7,8X,F12.7))
C      WRITE(IOUT,'(1X,I6,1X,A13,21X,F12.7)')NRMASS+1,'      hadrons',
C     +              PWIDTH(4)+PWIDTH(5)+PWIDTH(6)+PWIDTH(7)+PWIDTH(9)
      Z = RMZ**2
      CALL FORMFS(Z,9)
      DO 160 I = 0 , NRMASS
         FZVMZ(I) = FZV(I)
         FZAMZ(I) = FZA(I)
         FGVMZ(I) = FGV(I)
         FGAMZ(I) = FGA(I)
  160 CONTINUE
      ZWID = (IMSIGZ(Z)+IMZ2(Z))/RMZ/(1D0+BIGPIZ(Z))
      IF (DBGINI) THEN
         WRITE(IOUT,170) RMZ,ZWID,RMW,SIN2TH,RMH
 170     FORMAT(/,' For the bosons we have (everything in GeV):',/,
     +            '  mass of the   Z   =',F10.4,
     +            '    total width of the Z = ',F10.7,/,
     +            '  mass of the   W   =',F10.4,
     +            '    <==> sin**2(theta-w) = ',F10.7,/,
     +            '  mass of the Higgs =',F10.4,/)
C      WRITE(IOUT,180) 1D0/ALFA, 1D0+FACQED, ALFAS, 1D0+FACQCD
C  180 FORMAT(' Some coupling strengths:',/,
C     +       '                    1/alfa = ',F10.3,/,
C     +       ' the QED correction factor = ',F14.7,/,
C     +       '               alfa-strong = ',F10.3,/,
C     +       ' the QCD correction factor = ',F14.7,/)
      ENDIF
      END
      FUNCTION EEEEW (S, COSTH)
*     ---------------
* The Born e+e- --> e+e- matrix element squared, including both gamma
* and Z in both s and t channel, and including WEAK corrections.
* Summing/averaging over spins is performed.
* W. Beenakker and S.C. van der Marck, April 1990.
* Heavy boxes (ZZ and WW) added: July 1990.
*KEEP,IMPLNONE.
c     implicit none
*KEND.
      double precision eeeew, s, costh
*KEEP,PARAMS.
      integer nrmass
      PARAMETER ( NRMASS = 9 )
*KEEP,MASSES.
      double precision RMASS(0:NRMASS), RMASS2(0:NRMASS),
     &          VF(0:NRMASS), AF(0:NRMASS), QF(0:NRMASS)
      COMMON/ MASSES / RMASS, RMASS2, VF, AF, QF
*KEEP,FORMFA.
      COMPLEX*16 FZV,FZA,FGV,FGA
      COMMON/ FORMFA /FZV(0:NRMASS),FZA(0:NRMASS),
     +                FGV(0:NRMASS),FGA(0:NRMASS)
*KEEP,ADHOC.
      double precision ALFA, HBARC2, PI, FACQCB, FACQCD, FACQED
      COMMON/ ADHOC  / ALFA, HBARC2, PI, FACQCB, FACQCD, FACQED
*KEEP,OPTION.
      integer IWEAK, IFERM, ICHANN, IBOXES, IOUT
      COMMON/ OPTION / IWEAK, IFERM, ICHANN, IBOXES, IOUT
*KEND.
      double precision MATRIX(1:6),ZCOP(-1:1,-1:1)
      COMPLEX*16 GS,GT,ZS,ZT,MIXS,MIXT,GZS(-1:1,-1:1), GZT(-1:1,-1:1)
      COMPLEX*16 HADRQQ,VZZS,AZZS,VWWS,AWWS,VZZT,AZZT,VWWT,AWWT
      double precision ppqp, ppqm, pppm, t, e2, sum, phadpi
      external phadpi
      integer i, l1, l2
      complex*16 null
      parameter (null = (0.d0, 0.d0))
      PPQP = .25D0*S*( 1D0 - COSTH )
      PPQM = .25D0*S*( 1D0 + COSTH )
      PPPM =  .5D0*S
      T    = - 2D0*PPQP
* Define propagators, and include vertex form factors.
      E2   = 4D0*PI*ALFA
      CALL GZPROP (S, GS, ZS, MIXS)
      if (iweak .eq. 0) then
         fzv(1) = null
         fza(1) = null
         fgv(1) = null
         fga(1) = null
         fzv(iferm) = null
         fza(iferm) = null
         fgv(iferm) = null
         fga(iferm) = null
      else
         CALL FORMFS (S, IFERM)
      endif
      IF(ICHANN .EQ. 2) THEN
         GS = null
         ZS = null
         MIXS=null
      ENDIF
      I=IFERM
      DO 20 L1 = - 1 , 1 , 2
        DO 10 L2 = - 1 , 1 , 2
          GZS(L1,L2)=E2*( ( -QF(1)-FGV(1)-L1*(      -FGA(1) ) )*
     +                    ( -QF(I)-FGV(I)-L2*(      -FGA(I) ) )*GS +
     +                    (  VF(1)+FZV(1)-L1*( AF(1)+FZA(1) ) )*
     +                    (  VF(I)+FZV(I)-L2*( AF(I)+FZA(I) ) )*ZS -
     +      ( QF(1)*(VF(I)-L2*AF(I)) + QF(I)*(VF(1)-L1*AF(1)) )*MIXS )
          ZCOP(L1,L2) = ((VF(1)-L1*AF(1))*(VF(IFERM)-L2*AF(IFERM)))**2
   10   CONTINUE
   20 CONTINUE
*     Heavy boxes !
      IF(ICHANN .EQ. 2 .or. iweak .eq. 0 .or. iboxes .eq. 0) THEN
         VZZS = null
         AZZS = null
         VWWS = null
         AWWS = null
      ELSE
         CALL HEAVYB (S,T,VZZS,AZZS,VWWS,AWWS)
      ENDIF
*     Now everything for the t channel
      CALL GZPROP (T, GT ,ZT ,MIXT)
      if (iweak .eq. 1) then
         CALL FORMFS (T, 1)
*     Incorporate the Burkhardt fit for the light quark loops.
         GT = GT/( 1D0 - HADRQQ(T) - PHADPI(T) )
* otherwise form factors are still zero, no vacuum polarization
      endif
      IF(ICHANN .EQ. 1) THEN
         GT = null
         ZT = null
         MIXT=null
      ENDIF
      DO 40 L1 = - 1 , 1 , 2
        DO 30 L2 = - 1 , 1 , 2
          GZT(L1,L2)=E2*(
     +         ( -QF(1)-FGV(1)-L1*(      -FGA(1) ) )*
     +         ( -QF(1)-FGV(1)-L2*(      -FGA(1) ) )*GT +
     +         (  VF(1)+FZV(1)-L1*( AF(1)+FZA(1) ) )*
     +         (  VF(1)+FZV(1)-L2*( AF(1)+FZA(1) ) )*ZT -
     +       (QF(1)*(VF(1)-L2*AF(1))+QF(1)*(VF(1)-L1*AF(1)))*MIXT )
   30   CONTINUE
   40 CONTINUE
*     Heavy boxes !
      IF(ICHANN .EQ. 1 .or. iweak .eq. 0 .or. iboxes .eq. 0) THEN
         VZZT = null
         AZZT = null
         VWWT = null
         AWWT = null
      ELSE
         CALL HEAVYB (T,S,VZZT,AZZT,VWWT,AWWT)
      ENDIF
* There are 6 different helicity combinations.
      MATRIX(1) = 16D0*PPQM**2*(GZS( 1, 1)+GZT( 1, 1))*
     +                    CONJG(GZS( 1, 1)+GZT( 1, 1))
      MATRIX(2) = 16D0*PPQM**2*(GZS(-1,-1)+GZT(-1,-1))*
     +                    CONJG(GZS(-1,-1)+GZT(-1,-1))
      MATRIX(3) = 16D0*PPQP**2* GZS( 1,-1)*CONJG(GZS( 1,-1))
      MATRIX(4) = 16D0*PPQP**2* GZS(-1, 1)*CONJG(GZS(-1, 1))
      MATRIX(5) = 16D0*PPPM**2* GZT( 1,-1)*CONJG(GZT( 1,-1))
      MATRIX(6) = 16D0*PPPM**2* GZT(-1, 1)*CONJG(GZT(-1, 1))
      if (iboxes .eq. 1) then
*     Heavy boxes (factor 2 from 2*M0*M1)
      MATRIX(1) = MATRIX(1)+32D0*PPQM**2*(GZS( 1, 1)+GZT( 1, 1))*
     +                      CONJG((VZZS+AZZS+VZZT+AZZT)*ZCOP( 1, 1))
      MATRIX(2) = MATRIX(2)+32D0*PPQM**2*(GZS(-1,-1)+GZT(-1,-1))*
     +  CONJG((VZZS+AZZS+VZZT+AZZT)*ZCOP(-1,-1)+VWWS+AWWS+VWWT+AWWT)
      MATRIX(3) = MATRIX(3) +     32D0*PPQP**2*GZS( 1,-1)*
     +                         CONJG(VZZS-AZZS)*ZCOP( 1,-1)
      MATRIX(4) = MATRIX(4) +     32D0*PPQP**2*GZS(-1, 1)*
     +                         CONJG(VZZS-AZZS)*ZCOP(-1, 1)
      MATRIX(5) = MATRIX(5) +     32D0*PPPM**2*GZT( 1,-1)*
     +                         CONJG(VZZT-AZZT)*ZCOP( 1,-1)
      MATRIX(6) = MATRIX(6) +     32D0*PPPM**2*GZT(-1, 1)*
     +                         CONJG(VZZT-AZZT)*ZCOP(-1, 1)
      endif
      SUM = MATRIX(1) + MATRIX(2) + MATRIX(3) +
     +      MATRIX(4) + MATRIX(5) + MATRIX(6)
      EEEEW = HBARC2 / (128.d0*PI*S) * SUM
      END
      SUBROUTINE GZPROP(QSQR,PROPG,PROPZ,MIXING)
*     -----------------
* The gamma-Z propagators and their mixing, up to one loop corrections,
* but for the imaginary part of the Z propagator, which includes
* second order corrections.
* QSQR is input: the momentum transfer squared through the progagators.
* PROPG, PROPZ and MIXING are complex*16 output.
*KEEP,IMPLNONE.
c     implicit none
*KEND.
      double precision QSQR,SIGG,SIGZ,SIGGZ,IMSIGG,IMSGGZ,IMSIGZ,IMZ2
      COMPLEX*16 Z1,Z2,Z3, PROPG,PROPZ,MIXING, DCMPLX
*KEEP,BOSONS.
      double precision RMZ,RMW,RMH,RMT,SIN2TH,ZWID
      COMMON/ BOSONS / RMZ,RMW,RMH,RMT,SIN2TH,ZWID
*KEEP,OPTION.
      integer IWEAK, IFERM, ICHANN, IBOXES, IOUT
      COMMON/ OPTION / IWEAK, IFERM, ICHANN, IBOXES, IOUT
*KEND.
      IF (IWEAK .EQ. 1) THEN
        Z1 = DCMPLX( SIGG (QSQR) , IMSIGG(QSQR) )
        Z2 = DCMPLX( SIGZ (QSQR) , IMSIGZ(QSQR) + IMZ2(QSQR) )
        Z3 = DCMPLX( SIGGZ(QSQR) , IMSGGZ(QSQR) )
        PROPG = 1D0/( QSQR + Z1 )
        PROPZ = 1D0/( QSQR - RMZ**2 + Z2 )
        MIXING= - Z3/( QSQR*(QSQR-RMZ**2+Z2) )
      ELSE
        PROPG  = 1D0/QSQR
        IF (QSQR .GT. 0D0) THEN
          PROPZ = 1D0/DCMPLX( QSQR-RMZ**2 , RMZ*ZWID )
        ELSE
          PROPZ = DCMPLX ( 1D0/(QSQR-RMZ**2) , 0D0 )
        ENDIF
        MIXING = DCMPLX( 0D0 , 0D0 )
      ENDIF
      END
      SUBROUTINE HEAVYB(S,T,VZZ,AZZ,VWW,AWW)
*     -----------------
* Subroutine giving the 'couplings' with which to contract the
* ZZ and WW boxes with the Born matrix element.
* S,T are input and VZZ,AZZ,VWW,AWW are complex*16 output.
*KEEP,IMPLICIT.
      IMPLICIT double precision (A-H,O-Z)
*KEEP,BOSONS.
      double precision RMZ,RMW,RMH,RMT,SIN2TH,ZWID
      COMMON/ BOSONS / RMZ,RMW,RMH,RMT,SIN2TH,ZWID
*KEEP,OPTION.
      integer IWEAK, IFERM, ICHANN, IBOXES, IOUT
      COMMON/ OPTION / IWEAK, IFERM, ICHANN, IBOXES, IOUT
*KEEP,ADHOC.
      double precision ALFA, HBARC2, PI, FACQCB, FACQCD, FACQED
      COMMON/ ADHOC  / ALFA, HBARC2, PI, FACQCB, FACQCD, FACQED
*KEND.
      COMPLEX*16 VZZ,AZZ,VWW,AWW,RI5ST,RIST,RI5SU,RISU
      U = - S - T
      CALL HEAVYI(S,T,RMZ,RIST,RI5ST)
      CALL HEAVYI(S,U,RMZ,RISU,RI5SU)
      VZZ = ALFA/2D0/PI*( RIST  - RISU  )
      AZZ = ALFA/2D0/PI*( RI5ST + RI5SU )
*     WW boxes depend strongly on the isospin of the produced fermion
      IF(IFERM.EQ.0.OR.IFERM.EQ.4.OR.IFERM.EQ.6.OR.IFERM.EQ.8) THEN
*       isospin = + 1/2
        CALL HEAVYI(S,U,RMW,RISU,RI5SU)
        VWW = ALFA/2D0/PI*( - RISU  )
        AWW = ALFA/2D0/PI*( + RI5SU )
      ELSE
*       isospin = - 1/2
        CALL HEAVYI(S,T,RMW,RIST,RI5ST)
        VWW = ALFA/2D0/PI*( + RIST  )
        AWW = ALFA/2D0/PI*( + RI5ST )
      ENDIF
* To get the normalization right
      E2 = 4.d0 * PI * alfa
      VZZ = VZZ * E2/S
      AZZ = AZZ * E2/S
      VWW = VWW * E2/S/4D0/SIN2TH**2
      AWW = AWW * E2/S/4D0/SIN2TH**2
      END
      SUBROUTINE HEAVYI(S,T,RM,RI,RI5)
*     -----------------
* Function needed to calculate ZZ or WW boxes.
* S,T,RM are input, RI,RI5 are complex*16 output.
*KEEP,IMPLICIT.
      IMPLICIT double precision (A-H,O-Z)
*KEND.
      PARAMETER( EPS = 1D-10 )
*KEEP,ADHOC.
      double precision ALFA, HBARC2, PI, FACQCB, FACQCD, FACQED
      COMMON/ ADHOC  / ALFA, HBARC2, PI, FACQCB, FACQCD, FACQED
*KEND.
      COMPLEX*16 RI,RI5,SPENCE,ROOT1,ROOT2,X1,X2,Y1,Y2,FOURSP,RLOG12
      COMPLEX*16 SHELP,THELP,I,X1X2
      IF ( S.GT.0D0 .AND. T.GT.0D0 ) THEN
        WRITE(*,'(A)')' HEAVYI: both S and T > 0.  This is not valid!'
        RI  = (0D0,0D0)
        RI5 = (0D0,0D0)
        RETURN
      ENDIF
      RM2 = RM**2
      IF( S .GT. 0D0 ) THEN
        SHELP = 4D0*RM2/CMPLX(S,EPS)
      ELSE
        SHELP = 4D0*RM2/S
      ENDIF
      IF( T .GT. 0D0 ) THEN
        THELP = RM2/CMPLX(T,EPS)
      ELSE
        THELP = RM2/T
      ENDIF
      ROOT1 = SQRT( (1D0,0D0)-SHELP )
      IF(S.LT.0D0.AND.T.LT.0D0.AND.4D0*RM2/S*(1D0+RM2/T).GT.1D0) THEN
        I = (0D0,1D0)
        ROOT2 = I*SQRT( -( (1D0,0D0)-SHELP*( (1D0,0D0) + THELP ) ) )
      ELSE
        ROOT2 =   SQRT(    (1D0,0D0)-SHELP*( (1D0,0D0) + THELP )   )
      ENDIF
      Y1 = .5D0*( 1D0 + ROOT1 )
      Y2 = .5D0*( 1D0 - ROOT1 )
      X1 = .5D0*( 1D0 + ROOT2 )
      X2 = .5D0*( 1D0 - ROOT2 )
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
     +      SPENCE( 1D0+CMPLX(T,EPS)/RM2 ) - PI*PI/6D0 - RLOG12**2 ) +
     +      .5D0*LOG(-CMPLX(T,EPS)/RM2) + (Y2-Y1)/2D0*RLOG12 +
     +      ( S+2D0*T - 4D0*T*RM2/S + 2D0*RM2**2/T - 2D0*RM2**2/S )/
     +      ( 2D0*( S + T )*(-X1X2) ) * FOURSP
      RI5 = S/( S + T ) * RI5
      RI  = RI5 + 2D0*RLOG12**2 + 2D0/X1X2*FOURSP
      END
      SUBROUTINE COUPLS(SIN2TH,RMT)
*     -----------------
* Define the fermion masses and their couplings to the bosons.
*KEEP,IMPLICIT.
      IMPLICIT double precision (A-H,O-Z)
*KEEP,PARAMS.
      integer nrmass
      PARAMETER ( NRMASS = 9 )
*KEEP,MASSES.
      double precision RMASS(0:NRMASS), RMASS2(0:NRMASS),
     &          VF(0:NRMASS), AF(0:NRMASS), QF(0:NRMASS)
      COMMON/ MASSES / RMASS, RMASS2, VF, AF, QF
*KEND.
      RMASS(0) = 0D0
* Charged lepton masses from Particle Data Group Publ. 1990.
      RMASS(1) = .51099906D-3
      RMASS(2) = .105658387D0
      RMASS(3) = 1.7841D0
      RMASS(4) = .04145D0
      RMASS(5) = .04146D0
      RMASS(6) = 1.5D0
      RMASS(7) = .15D0
      RMASS(8) = RMT
      RMASS(9) = 4.5D0
      SW    = SQRT( SIN2TH )
      CW    = SQRT( 1D0 - SIN2TH )
      DO 10 I = 0 , NRMASS
         RMASS2(I) = RMASS(I)**2
         IF(I .EQ. 0) THEN
            Q  =  0D0
            T3 = .5D0
         ELSEIF(I .LE. 3) THEN
            Q  = -  1D0
            T3 = - .5D0
         ELSEIF(I.EQ.4 .OR. I.EQ.6 .OR. I.EQ.8) THEN
            Q  =   2D0/3D0
            T3 =  .5D0
         ELSE
            Q  = -  1D0/3D0
            T3 = - .5D0
         ENDIF
         VF(I) = ( T3 - 2D0*Q*SIN2TH ) /2D0/CW/SW
         AF(I) =   T3 /2D0/SW/CW
         QF(I) =   Q
   10 CONTINUE
      END
      SUBROUTINE RNORM()
*     ----------------
* W.J.P. Beenakker, F.A.Berends and S.C. van der Marck, June 14th 1989
* Calculate all quantities that have to do with weak corrections on
* boson propagators.
*KEEP,IMPLICIT.
      IMPLICIT double precision (A-H,O-Z)
*KEEP,PARAMS.
      integer nrmass
      PARAMETER ( NRMASS = 9 )
*KEEP,MASSES.
      double precision RMASS(0:NRMASS), RMASS2(0:NRMASS),
     &          VF(0:NRMASS), AF(0:NRMASS), QF(0:NRMASS)
      COMMON/ MASSES / RMASS, RMASS2, VF, AF, QF
*KEEP,BOSONS.
      double precision RMZ,RMW,RMH,RMT,SIN2TH,ZWID
      COMMON/ BOSONS / RMZ,RMW,RMH,RMT,SIN2TH,ZWID
*KEEP,RENORM.
      double precision PIGAM0,SIGGZ0,DELMZ,DELMW,DELZ2Z,DELZ2G,DELZ2W,
     &                 SUMQ1,SUMQ2
      COMMON/ RENORM / PIGAM0,SIGGZ0,DELMZ,DELMW,DELZ2Z,DELZ2G,DELZ2W,
     &                 SUMQ1,SUMQ2
*KEEP,ADHOC.
      double precision ALFA, HBARC2, PI, FACQCB, FACQCD, FACQED
      COMMON/ ADHOC  / ALFA, HBARC2, PI, FACQCB, FACQCD, FACQED
*KEND.
      SW  = SQRT( SIN2TH )
      CW2 =  1D0 - SIN2TH
      CW  = SQRT( CW2 )
      Z   = RMZ**2
      W   = RMW**2
      SIGGZ0 = USIGGZ( 0D0 )
      PIGAM0 = DUSIGG( 0D0 )
*
* Renormalization constants eq. (3.16) and (3.17) of ref.2b
      DELMZ  =   USIGZ( Z )
      DELMW  =   USIGW( W )
      DELZ2G = - PIGAM0
      DELZ2Z = - PIGAM0 - 2D0*(CW2-SIN2TH)/SW/CW*SIGGZ0/Z +
     +                    (CW2-SIN2TH)/SIN2TH*( DELMZ/Z - DELMW/W )
      DELZ2W = - PIGAM0 - 2D0*CW/SW*SIGGZ0/Z +
     +                    CW2/SIN2TH*( DELMZ/Z - DELMW/W )
*
* Contributions from the DELTA-i terms
      SUMQ1 = ALFA/4D0/PI/2D0/SIN2TH/W*(
     +      + ( RMASS2(4)-RMASS2(5) )*LOG(RMASS(4)/RMASS(5))
     +      + ( RMASS2(6)-RMASS2(7) )*LOG(RMASS(6)/RMASS(7))
     +      + ( RMASS2(8)-RMASS2(9) )*LOG(RMASS(8)/RMASS(9)) )
      SUMQ2 = ALFA/2D0/PI*( + LOG(RMASS(4)/RMASS(5))
     +                      + LOG(RMASS(6)/RMASS(7))
     +                      + LOG(RMASS(8)/RMASS(9)) )
*
* Calculate delta-r and update the values for sin(theta-w) and MW.
      DR = DELTAR()
      BIGA0  = 37.281D0**2
      SIN2TH = .5D0*(  1D0 - SQRT( 1D0-4D0*BIGA0/Z/(1D0-DR) )  )
      RMW = SQRT( Z*( 1D0 - SIN2TH ) )
      END
      SUBROUTINE FORMFS(QSQR,IFERM)
*     -----------------
* W.J.P. Beenakker, F.A.Berends and S.C. van der Marck, June 14th 1989
* Calculate the vector and axial vector formfactors for the Z-ff and
* the gamma-ff couplings.
*KEEP,IMPLICIT.
      IMPLICIT double precision (A-H,O-Z)
*KEEP,PARAMS.
      integer nrmass
      PARAMETER ( NRMASS = 9 )
*KEEP,MASSES.
      double precision RMASS(0:NRMASS), RMASS2(0:NRMASS),
     &          VF(0:NRMASS), AF(0:NRMASS), QF(0:NRMASS)
      COMMON/ MASSES / RMASS, RMASS2, VF, AF, QF
*KEEP,BOSONS.
      double precision RMZ,RMW,RMH,RMT,SIN2TH,ZWID
      COMMON/ BOSONS / RMZ,RMW,RMH,RMT,SIN2TH,ZWID
*KEEP,ADHOC.
      double precision ALFA, HBARC2, PI, FACQCB, FACQCD, FACQED
      COMMON/ ADHOC  / ALFA, HBARC2, PI, FACQCB, FACQCD, FACQED
*KEEP,FORMFA.
      COMPLEX*16 FZV,FZA,FGV,FGA
      COMMON/ FORMFA /FZV(0:NRMASS),FZA(0:NRMASS),
     +                FGV(0:NRMASS),FGA(0:NRMASS)
*KEND.
      COMPLEX*16 FL1,FL2,FL3,FL4,FL5
      COMPLEX*16 RL2Z,RL2W,RL3W,RL3Z
*
      CALL LABDAS(QSQR,RMZ,RL2Z,RL3Z)
      CALL LABDAS(QSQR,RMW,RL2W,RL3W)
      CW2  = 1D0 - SIN2TH
      SW   = SQRT( SIN2TH )
      CW   = SQRT( CW2 )
      ALF4PI = ALFA/4D0/PI
* eq. (C.4) ref 2b.
      FL1 = RL2W/8D0/SW**3/CW - 3D0*CW/4D0/SW**3*RL3W
      FL2 = -(1D0-2D0/3D0*SIN2TH)/8D0/SW**3/CW*RL2W +
     +         3D0*CW/4D0/SW**3*RL3W
      FL3 =  (1D0-4D0/3D0*SIN2TH)/8D0/SW**3/CW*RL2W -
     +         3D0*CW/4D0/SW**3*RL3W
      IF(IFERM .EQ. 9) THEN
         CALL FLBOT(QSQR,FL4,FL5)
      ELSE
         FL4 = FL3
         FL5 = 1D0/6D0/SIN2TH*RL2W - 3D0/4D0/SIN2TH*RL3W
      ENDIF
* eq. (C.3) of ref 2b.
      FZV(0) = ALFA/4D0/PI/4D0/SW/CW*( RL2Z/4D0/CW2/SIN2TH +
     +           (1D0-1D0/2D0/SIN2TH)*RL2W + 3D0*CW2/SIN2TH*RL3W )
      FZA(0) = FZV(0)
      FZV(1) = ALF4PI*( VF(1)*(VF(1)**2+3D0*AF(1)**2)*RL2Z + FL1 )
      FZA(1) = ALF4PI*( AF(1)*(3D0*VF(1)**2+AF(1)**2)*RL2Z + FL1 )
      FZV(4) = ALF4PI*( VF(4)*(VF(4)**2+3D0*AF(4)**2)*RL2Z + FL2 )
      FZA(4) = ALF4PI*( AF(4)*(3D0*VF(4)**2+AF(4)**2)*RL2Z + FL2 )
      FZV(5) = ALF4PI*( VF(5)*(VF(5)**2+3D0*AF(5)**2)*RL2Z + FL3 )
      FZA(5) = ALF4PI*( AF(5)*(3D0*VF(5)**2+AF(5)**2)*RL2Z + FL3 )
      FZV(9) = ALF4PI*( VF(9)*(VF(9)**2+3D0*AF(9)**2)*RL2Z + FL4 )
      FZA(9) = ALF4PI*( AF(9)*(3D0*VF(9)**2+AF(9)**2)*RL2Z + FL4 )
* eq. (C.12) ref 2b.
      FL1 = -3D0/ 4D0/SIN2TH*RL3W
      FL2 = -1D0/12D0/SIN2TH*RL2W + 3D0/4D0/SIN2TH*RL3W
      FL3 =  1D0/ 6D0/SIN2TH*RL2W - 3D0/4D0/SIN2TH*RL3W
      FGV(0) = CMPLX(0D0,0D0)
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
      SUBROUTINE LABDAS(QSQR,RM,LABDA2,LABDA3)
*     -----------------
*KEEP,IMPLICIT.
      IMPLICIT double precision (A-H,O-Z)
*KEND.
      COMPLEX*16 LABDA2,LABDA3,W,X,BIGK,SPENCE, DCMPLX
      PARAMETER( EPS = 1D-10 )
      W = RM**2/DCMPLX( QSQR , EPS )
      X = LOG( - BIGK(QSQR,RM) )
* This way of writing was not very stable for qsqr << rm**2. The
* second way is somewhat better, but still has to be cut off at some
* low qsqr value, in which case it should yield zero.
*      LABDA2 = -3.5D0 - 2D0*W - (2D0*W+3D0)*LOG(-W)+
*     +   2D0*(1D0+W)**2*( SPENCE(1D0+1D0/W) - PI**2/6D0 )
      LABDA2 = -3.5D0 - 2D0*W - (2D0*W+3D0)*LOG(-W)+
     +   2D0*(1D0+W)**2*( -SPENCE(-1D0/W)+LOG(-W)*LOG(1D0+1D0/W) )
      IF(DREAL(W).GT.1D6) LABDA2 = (0D0,0D0)
      LABDA3 =  5D0/6D0 - 2D0*W/3D0 - 1D0/3D0*(2D0*W+1)*
     +   CDSQRT(1D0-4D0*W)*X + 2D0/3D0*W*(W+2D0)*X**2
      END
      FUNCTION BIGK(QSQR,RM)
*     -------------
*KEEP,IMPLICIT.
      IMPLICIT double precision (A-H,O-Z)
*KEND.
      COMPLEX*16 BIGK,W, DCMPLX
      W = RM**2/DCMPLX( QSQR , 1D-10 )
      BIGK = - ( CDSQRT(1D0-4D0*W) - 1D0 )/( CDSQRT(1D0-4D0*W) + 1D0 )
      END
      FUNCTION SIGG(QSQR)
*     -------------
* Real part of the renormalized weakly corrected photon propagator
*KEEP,IMPLICIT.
      IMPLICIT double precision (A-H,O-Z)
*KEEP,RENORM.
      double precision PIGAM0,SIGGZ0,DELMZ,DELMW,DELZ2Z,DELZ2G,DELZ2W,
     &                 SUMQ1,SUMQ2
      COMMON/ RENORM / PIGAM0,SIGGZ0,DELMZ,DELMW,DELZ2Z,DELZ2G,DELZ2W,
     &                 SUMQ1,SUMQ2
*KEND.
* eq. (3.23) ref 2b.
      SIGG = USIGG(QSQR) - PIGAM0 * QSQR
      END
      FUNCTION SIGGZ(QSQR)
*     --------------
* Real part of the renormalized weakly corrected photon-Z mixing
*KEEP,IMPLICIT.
      IMPLICIT double precision (A-H,O-Z)
*KEEP,RENORM.
      double precision PIGAM0,SIGGZ0,DELMZ,DELMW,DELZ2Z,DELZ2G,DELZ2W,
     &                 SUMQ1,SUMQ2
      COMMON/ RENORM / PIGAM0,SIGGZ0,DELMZ,DELMW,DELZ2Z,DELZ2G,DELZ2W,
     &                 SUMQ1,SUMQ2
*KEEP,BOSONS.
      double precision RMZ,RMW,RMH,RMT,SIN2TH,ZWID
      COMMON/ BOSONS / RMZ,RMW,RMH,RMT,SIN2TH,ZWID
*KEND.
      CW2 = 1D0 - SIN2TH
      CW  = SQRT( CW2 )
      SW  = SQRT( SIN2TH )
* eq. (3.23) ref 2b.
      SIGGZ = USIGGZ(QSQR) - SIGGZ0 - QSQR* CW*SW/(CW2-SIN2TH)*
     +        ( DELZ2Z - DELZ2G ) +
     +        QSQR*( - CW/SW*SUMQ1 - SUMQ2/6D0/CW/SW )
      END
      FUNCTION SIGZ(QSQR)
*     -------------
* Real part of the renormalized weakly corrected Z propagator
*KEEP,IMPLICIT.
      IMPLICIT double precision (A-H,O-Z)
*KEEP,RENORM.
      double precision PIGAM0,SIGGZ0,DELMZ,DELMW,DELZ2Z,DELZ2G,DELZ2W,
     &                 SUMQ1,SUMQ2
      COMMON/ RENORM / PIGAM0,SIGGZ0,DELMZ,DELMW,DELZ2Z,DELZ2G,DELZ2W,
     &                 SUMQ1,SUMQ2
*KEEP,BOSONS.
      double precision RMZ,RMW,RMH,RMT,SIN2TH,ZWID
      COMMON/ BOSONS / RMZ,RMW,RMH,RMT,SIN2TH,ZWID
*KEND.
      CW2 = 1D0 - SIN2TH
* eq. (3.23) ref 2b.
      SIGZ = USIGZ(QSQR) - DELMZ + DELZ2Z*( QSQR - RMZ**2 ) +
     +       (QSQR-RMZ**2)*((CW2-SIN2TH)/SIN2TH*SUMQ1+SUMQ2/3D0/SIN2TH)
      END
      FUNCTION SIGW(QSQR)
*     -------------
* Real part of the renormalized weakly corrected W propagator
*KEEP,IMPLICIT.
      IMPLICIT double precision (A-H,O-Z)
*KEEP,RENORM.
      double precision PIGAM0,SIGGZ0,DELMZ,DELMW,DELZ2Z,DELZ2G,DELZ2W,
     &                 SUMQ1,SUMQ2
      COMMON/ RENORM / PIGAM0,SIGGZ0,DELMZ,DELMW,DELZ2Z,DELZ2G,DELZ2W,
     &                 SUMQ1,SUMQ2
*KEEP,BOSONS.
      double precision RMZ,RMW,RMH,RMT,SIN2TH,ZWID
      COMMON/ BOSONS / RMZ,RMW,RMH,RMT,SIN2TH,ZWID
*KEND.
      CW2 = 1D0 - SIN2TH
* eq. (3.23) ref 2b.
      SIGW = USIGW(QSQR) - DELMW + DELZ2W*( QSQR - RMW**2 ) +
     +       (QSQR-RMW**2)*( CW2/SIN2TH*SUMQ1 + SUMQ2/3D0/SIN2TH )
      END
      FUNCTION DELTAR()
*     ---------------
* W.J.P. Beenakker, F.A.Berends and S.C. van der Marck, June 14th 1989
* The weak correction factor delta-r
*KEEP,IMPLICIT.
      IMPLICIT double precision (A-H,O-Z)
*KEEP,BOSONS.
      double precision RMZ,RMW,RMH,RMT,SIN2TH,ZWID
      COMMON/ BOSONS / RMZ,RMW,RMH,RMT,SIN2TH,ZWID
*KEEP,ADHOC.
      double precision ALFA, HBARC2, PI, FACQCB, FACQCD, FACQED
      COMMON/ ADHOC  / ALFA, HBARC2, PI, FACQCB, FACQCD, FACQED
*KEND.
* eq. (4.18) ref 2b.
      DELTAR = SIGW(0D0)/RMW**2 + ALFA/4D0/PI/SIN2TH*
     +        ( 6D0 + (7D0-4D0*SIN2TH)/2D0/SIN2TH*LOG(1D0-SIN2TH) )
      END
      FUNCTION USIGG(QSQR)
*     --------------
* W.J.P. Beenakker, F.A.Berends and S.C. van der Marck, June 14th 1989
* Real part of the unrenormalized weakly corrected photon prop
* eq. (B.2) ref 2b with errata, a minus sign and a bracket.
*KEEP,IMPLICIT.
      IMPLICIT double precision (A-H,O-Z)
*KEEP,PARAMS.
      integer nrmass
      PARAMETER ( NRMASS = 9 )
*KEEP,MASSES.
      double precision RMASS(0:NRMASS), RMASS2(0:NRMASS),
     &          VF(0:NRMASS), AF(0:NRMASS), QF(0:NRMASS)
      COMMON/ MASSES / RMASS, RMASS2, VF, AF, QF
*KEEP,BOSONS.
      double precision RMZ,RMW,RMH,RMT,SIN2TH,ZWID
      COMMON/ BOSONS / RMZ,RMW,RMH,RMT,SIN2TH,ZWID
*KEEP,ADHOC.
      double precision ALFA, HBARC2, PI, FACQCB, FACQCD, FACQED
      COMMON/ ADHOC  / ALFA, HBARC2, PI, FACQCB, FACQCD, FACQED
*KEND.
      S   = QSQR
      TOT = 0D0
      FAC = 1D0
      DO 10 I = 1 , NRMASS
         IF(I .EQ. 4) FAC = 3D0
         RM = RMASS(I)
         TOT = TOT + 4D0/3D0*QF(I)**2*(
     +         ( S+2D0*RMASS2(I) )*FREAL(S,RM,RM) - S/3D0 )  *  FAC
   10 CONTINUE
      TOT = TOT - ( 3D0*S + 4D0*RMW**2 )*FREAL(S,RMW,RMW)
      USIGG = ALFA/4D0/PI * TOT
      END
      FUNCTION DUSIGG(QSQR)
*     ---------------
* W.J.P. Beenakker, F.A.Berends and S.C. van der Marck, June 14th 1989
* Derivative of the real part of the unrenormalized
* weakly corrected photon propagator
*KEEP,IMPLICIT.
      IMPLICIT double precision (A-H,O-Z)
*KEEP,PARAMS.
      integer nrmass
      PARAMETER ( NRMASS = 9 )
*KEEP,MASSES.
      double precision RMASS(0:NRMASS), RMASS2(0:NRMASS),
     &          VF(0:NRMASS), AF(0:NRMASS), QF(0:NRMASS)
      COMMON/ MASSES / RMASS, RMASS2, VF, AF, QF
*KEEP,BOSONS.
      double precision RMZ,RMW,RMH,RMT,SIN2TH,ZWID
      COMMON/ BOSONS / RMZ,RMW,RMH,RMT,SIN2TH,ZWID
*KEEP,ADHOC.
      double precision ALFA, HBARC2, PI, FACQCB, FACQCD, FACQED
      COMMON/ ADHOC  / ALFA, HBARC2, PI, FACQCB, FACQCD, FACQED
*KEND.
      S   = QSQR
      TOT = 0D0
      FAC = 1D0
      DO 10 I = 1 , NRMASS
         IF(I .EQ. 4) FAC = 3D0
         RM = RMASS(I)
         TOT = TOT + 4D0/3D0*QF(I)**2*( + FREAL(S,RM,RM) +
     +         ( S+2D0*RMASS2(I) )*DFREAL(S,RM,RM) - 1D0/3D0 )  *  FAC
   10 CONTINUE
      TOT = TOT - 3D0*FREAL(S,RMW,RMW) -
     +            ( 3D0*S + 4D0*RMW**2 )*DFREAL(S,RMW,RMW)
      DUSIGG = ALFA/4D0/PI * TOT
      END
      FUNCTION USIGGZ(QSQR)
*     ---------------
* W.J.P. Beenakker, F.A.Berends and S.C. van der Marck, June 14th 1989
* Real part of the unrenormalized photon-Z mixing propagator
* eq. (B.3) ref 2b.
*KEEP,IMPLICIT.
      IMPLICIT double precision (A-H,O-Z)
*KEEP,PARAMS.
      integer nrmass
      PARAMETER ( NRMASS = 9 )
*KEEP,MASSES.
      double precision RMASS(0:NRMASS), RMASS2(0:NRMASS),
     &          VF(0:NRMASS), AF(0:NRMASS), QF(0:NRMASS)
      COMMON/ MASSES / RMASS, RMASS2, VF, AF, QF
*KEEP,BOSONS.
      double precision RMZ,RMW,RMH,RMT,SIN2TH,ZWID
      COMMON/ BOSONS / RMZ,RMW,RMH,RMT,SIN2TH,ZWID
*KEEP,ADHOC.
      double precision ALFA, HBARC2, PI, FACQCB, FACQCD, FACQED
      COMMON/ ADHOC  / ALFA, HBARC2, PI, FACQCB, FACQCD, FACQED
*KEND.
      S   = QSQR
      SW  = SQRT(       SIN2TH )
      CW  = SQRT( 1D0 - SIN2TH )
      TOT = 0D0
      FAC = 1D0
      DO 10 I = 1 , NRMASS
         IF(I .EQ. 4) FAC = 3D0
         RM = RMASS(I)
         TOT = TOT - 4D0/3D0*QF(I)*VF(I)*(
     +         ( S+2D0*RMASS2(I) )*FREAL(S,RM,RM) - S/3D0 )  *  FAC
   10 CONTINUE
      TOT = TOT + 1D0/CW/SW*( ( 3D0*CW**2 + 1D0/6D0 )*S
     +                      + ( 4D0*CW**2 + 4D0/3D0 )*RMW**2 )*
     +                       FREAL(S,RMW,RMW)  +  S/9D0/CW/SW
      USIGGZ = ALFA/4D0/PI * TOT
      END
      FUNCTION USIGZ(QSQR)
*     --------------
* W.J.P. Beenakker, F.A.Berends and S.C. van der Marck, June 14th 1989
* Real part of the unrenormalized weakly corrected Z propagator,
* for QSQR > 0.
* eq. (B.4) ref 2b. 1 erratum in the pole part, not apparent here.
*KEEP,IMPLICIT.
      IMPLICIT double precision (A-H,O-Z)
*KEEP,PARAMS.
      integer nrmass
      PARAMETER ( NRMASS = 9 )
*KEEP,MASSES.
      double precision RMASS(0:NRMASS), RMASS2(0:NRMASS),
     &          VF(0:NRMASS), AF(0:NRMASS), QF(0:NRMASS)
      COMMON/ MASSES / RMASS, RMASS2, VF, AF, QF
*KEEP,BOSONS.
      double precision RMZ,RMW,RMH,RMT,SIN2TH,ZWID
      COMMON/ BOSONS / RMZ,RMW,RMH,RMT,SIN2TH,ZWID
*KEEP,ADHOC.
      double precision ALFA, HBARC2, PI, FACQCB, FACQCD, FACQED
      COMMON/ ADHOC  / ALFA, HBARC2, PI, FACQCB, FACQCD, FACQED
*KEND.
      S   = QSQR
      CW2 =  1D0 - SIN2TH
      TOT = 0D0
      FAC = 1D0
      DO 10 I = 1 , NRMASS
         IF(I .EQ. 4) FAC = 3D0
         IF(I .LE. 3) TOT = TOT + 4D0/3D0* 2D0*AF(I)**2*S*(
     +                    + 5D0/3D0 - LOG(ABS( S/RMASS2(I) )) ) * FAC
         RM = RMASS(I)
         F = FREAL(S,RM,RM)
         TOT = TOT + 4D0/3D0*(  ( VF(I)**2+AF(I)**2 )*(
     +                         + ( S+2D0*RMASS2(I) )*F - S/3D0 )
     +             - 3D0/8D0/SIN2TH/CW2*RMASS2(I)*F ) * FAC
   10 CONTINUE
      W = RMW**2
      Z = RMZ**2
      H = RMH**2
      TOT = TOT + ( ( -CW2**2*(40D0*S+80D0*W) + 12D0*W +
     1              (CW2-SIN2TH)**2*( 8D0*W+S ) )*FREAL(S,RMW,RMW) +
     2            ( 10D0*Z - 2D0*H + S + (H-Z)**2/S )*FREAL(S,RMH,RMZ)-
     3            2D0*H*LOG(H/W) - 2D0*Z*LOG(Z/W) +
     4            ( 10D0*Z - 2D0*H + S )*( 1D0 - (H+Z)/(H-Z)*
     5              LOG(RMH/RMZ) - LOG(RMH*RMZ/W) ) +
     6            2D0/3D0*S*( 1D0 + (CW2-SIN2TH)**2 - 4D0*CW2**2 )
     7  )/12D0/CW2/SIN2TH
      USIGZ = ALFA/4D0/PI * TOT
      END
      FUNCTION USIGW(QSQR)
*     --------------
* W.J.P. Beenakker, F.A.Berends and S.C. van der Marck, June 14th 1989
* Real part of the unrenormalized weakly corrected W propagator,
* for QSQR >= 0.
* eq. (B.5) ref 2b with errata: a factor 3 for the last 7 lines,
*                               one factor s, one factor 1/s and a sign
*KEEP,IMPLICIT.
      IMPLICIT double precision (A-H,O-Z)
*KEEP,PARAMS.
      integer nrmass
      PARAMETER ( NRMASS = 9 )
*KEEP,MASSES.
      double precision RMASS(0:NRMASS), RMASS2(0:NRMASS),
     &          VF(0:NRMASS), AF(0:NRMASS), QF(0:NRMASS)
      COMMON/ MASSES / RMASS, RMASS2, VF, AF, QF
*KEND.
      double precision MP,MM
*KEEP,BOSONS.
      double precision RMZ,RMW,RMH,RMT,SIN2TH,ZWID
      COMMON/ BOSONS / RMZ,RMW,RMH,RMT,SIN2TH,ZWID
*KEEP,ADHOC.
      double precision ALFA, HBARC2, PI, FACQCB, FACQCD, FACQED
      COMMON/ ADHOC  / ALFA, HBARC2, PI, FACQCB, FACQCD, FACQED
*KEND.
      S   = QSQR
      CW2 =  1D0 - SIN2TH
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
      FUNCTION BIGPIZ(QSQR)
*     ---------------
* W.J.P. Beenakker, F.A.Berends and S.C. van der Marck, June 14th 1989
* Derivative of the real part of the renormalized
* weakly corrected Z propagator, for QSQR > 0.
*KEEP,IMPLICIT.
      IMPLICIT double precision (A-H,O-Z)
*KEEP,RENORM.
      double precision PIGAM0,SIGGZ0,DELMZ,DELMW,DELZ2Z,DELZ2G,DELZ2W,
     &                 SUMQ1,SUMQ2
      COMMON/ RENORM / PIGAM0,SIGGZ0,DELMZ,DELMW,DELZ2Z,DELZ2G,DELZ2W,
     &                 SUMQ1,SUMQ2
*KEEP,PARAMS.
      integer nrmass
      PARAMETER ( NRMASS = 9 )
*KEEP,MASSES.
      double precision RMASS(0:NRMASS), RMASS2(0:NRMASS),
     &          VF(0:NRMASS), AF(0:NRMASS), QF(0:NRMASS)
      COMMON/ MASSES / RMASS, RMASS2, VF, AF, QF
*KEEP,BOSONS.
      double precision RMZ,RMW,RMH,RMT,SIN2TH,ZWID
      COMMON/ BOSONS / RMZ,RMW,RMH,RMT,SIN2TH,ZWID
*KEEP,ADHOC.
      double precision ALFA, HBARC2, PI, FACQCB, FACQCD, FACQED
      COMMON/ ADHOC  / ALFA, HBARC2, PI, FACQCB, FACQCD, FACQED
*KEND.
      S   = QSQR
      CW2 = 1D0 - SIN2TH
      TOT = 0D0
      FAC = 1D0
      DO 10 I = 1 , NRMASS
         IF(I .EQ. 4) FAC = 3D0
         IF(I .LE. 3) TOT = TOT + 4D0/3D0* 2D0*AF(I)**2*(
     +                    + 2D0/3D0 - LOG(ABS( S/RMASS2(I) )) ) * FAC
         RM = RMASS(I)
         F  =  FREAL(S,RM,RM)
         DF = DFREAL(S,RM,RM)
         TOT = TOT + 4D0/3D0*(  ( VF(I)**2+AF(I)**2 )*(
     +                   F  + ( S+2D0*RMASS2(I) )*DF - 1D0/3D0 )
     +             - 3D0/8D0/SIN2TH/CW2*RMASS2(I)*DF ) * FAC
   10 CONTINUE
      W = RMW**2
      Z = RMZ**2
      H = RMH**2
      TOT = TOT + ( ( -CW2**2*(40D0*S+80D0*W) + 12D0*W +
     1                (CW2-SIN2TH)**2*( 8D0*W+S ) )*DFREAL(S,RMW,RMW) +
     2               (-40D0*CW2**2+(CW2-SIN2TH)**2 )*FREAL(S,RMW,RMW) +
     3            ( 10D0*Z-2D0*H+S+(H-Z)**2/S )*DFREAL(S,RMH,RMZ) +
     4            (        1D0 - (H-Z)**2/S**2 )*FREAL(S,RMH,RMZ) +
     5            ( 1D0-(H+Z)/(H-Z)*LOG(RMH/RMZ)-LOG(RMH*RMZ/W) ) +
     6            2D0/3D0*( 1D0 + (CW2-SIN2TH)**2 - 4D0*CW2**2 )
     7  )/12D0/CW2/SIN2TH
      BIGPIZ = TOT * ALFA/4D0/PI + DELZ2Z +
     +                    (CW2-SIN2TH)/SIN2TH*SUMQ1 + SUMQ2/3D0/SIN2TH
      END
      FUNCTION FREAL(S,RM1,RM2)
*     --------------
* W.J.P. Beenakker, F.A.Berends and S.C. van der Marck, June 14th 1989
* Real part of the function F(s,ma,mb), eq. (B.6) ref 2b.
*KEEP,IMPLICIT.
      IMPLICIT double precision (A-H,O-Z)
*KEND.
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
            F = F + LOG(S) - 1D0 + RMB2/S*LOG(RMB2/S) - RMB2/S -
     +        ( (RMB2/S-1D0)*LOG(ABS(RMB2/S-1D0)) - (RMB2/S-1D0) )
          ENDIF
        ELSE
          S0   = - .5D0*( 1D0 + RMA2/S - RMB2/S )
          S1   =   .5D0*( 1D0 - RMA2/S + RMB2/S )
          DISCR= ( (S+RMA2-RMB2)**2 - 4D0*RMA2*S ) /4D0/S**2
          ROOTD= SQRT( ABS( DISCR ) )
          F = F + LOG(S) + S1*LOG( RMB2/S ) - 2D0*S1 -
     +                      S0*LOG( RMA2/S ) + 2D0*S0
          IF(DISCR .GE. 0D0) THEN
            IF(S.LT.RMA2 .OR. S.LT.RMB2) THEN
              F = F + ROOTD*( LOG( (S1+ROOTD)**2*S/RMB2 )
     +                      - LOG( (S0+ROOTD)**2*S/RMA2 ) )
            ELSE
              F = F + ROOTD*( LOG( (S1+ROOTD)**2*S/RMB2 )
     +                      - LOG( RMA2/S/(S0-ROOTD)**2 ) )
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
          F = F - ROOTA*ROOTB/S*LOG( (ROOTA+ROOTB)**2/4D0/RMA/RMB )
        ENDIF
      ENDIF
      FREAL = - F
      END
      FUNCTION FIMAG(S,RMA,RMB)
*     --------
* Imaginary part of the function F(s,ma,mb)
*KEEP,IMPLICIT.
      IMPLICIT double precision (A-H,O-Z)
*KEND.
      PARAMETER (PI=3.1415926535897932D0)
      FIMAG = 0D0
      IF(S.GT.(RMA+RMB)**2) FIMAG=PI*SQRT((S-(RMA+RMB)**2)*
     +                                     (S-(RMA-RMB)**2))/S
      END
      FUNCTION DFREAL(S,RM1,RM2)
*     ---------------
* W.J.P. Beenakker, F.A.Berends and S.C. van der Marck, June 14th 1989
* Derivative of the real part of the function F(s,ma,mb).
*KEEP,IMPLICIT.
      IMPLICIT double precision (A-H,O-Z)
*KEND.
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
      FUNCTION IMSIGZ(S)
*     ---------------
* Imaginary part of the 1-loop Z self-energy
*KEEP,IMPLICIT.
      IMPLICIT double precision (A-H,O-Z)
*KEND.
      double precision IMSIGZ
*KEEP,PARAMS.
      integer nrmass
      PARAMETER ( NRMASS = 9 )
*KEEP,MASSES.
      double precision RMASS(0:NRMASS), RMASS2(0:NRMASS),
     &          VF(0:NRMASS), AF(0:NRMASS), QF(0:NRMASS)
      COMMON/ MASSES / RMASS, RMASS2, VF, AF, QF
*KEEP,BOSONS.
      double precision RMZ,RMW,RMH,RMT,SIN2TH,ZWID
      COMMON/ BOSONS / RMZ,RMW,RMH,RMT,SIN2TH,ZWID
*KEEP,ADHOC.
      double precision ALFA, HBARC2, PI, FACQCB, FACQCD, FACQED
      COMMON/ ADHOC  / ALFA, HBARC2, PI, FACQCB, FACQCD, FACQED
*KEND.
      CW2 = 1D0 - SIN2TH
      TOT = 0D0
      DO 10 I = 0 , NRMASS
         IF(I .EQ. 0) FAC = 3D0
         IF(I .EQ. 1) FAC = 1D0
         IF(I .EQ. 4) FAC = 3D0
         IF(S .GT. 4.D0*RMASS2(I)) THEN
            TOT = TOT + SQRT(1D0-4D0*RMASS2(I)/S)*
     +         ( ( VF(I)**2 + AF(I)**2 )*( S + 2D0*RMASS2(I) ) -
     +          6D0*RMASS2(I)*AF(I)**2 )/3.D0 * FAC
         ENDIF
   10 CONTINUE
      IF(S .GT. 4D0*RMW**2) TOT = TOT + SQRT(1D0-4D0*RMW**2/S)*
     +       ((-10D0*S-20D0*RMW**2)*CW2**2+(2D0*RMW**2+S/4D0)*
     +             (CW2-SIN2TH)**2+3D0*RMW**2)*4.D0*AF(1)**2/3D0
      IF(S .GT. (RMH+RMZ)**2) TOT = TOT +
     +        (10D0*RMZ**2-2D0*RMH**2+S+(RMH**2-RMZ**2)**2/S)*AF(1)**2*
     +        SQRT((1D0-(RMZ-RMH)**2/S)*(1D0-(RMZ+RMH)**2/S))/3D0
      IMSIGZ = TOT * ALFA
      END
      FUNCTION IMZ2(S)
*     -------------
* W.J.P. Beenakker, F.A.Berends and S.C. van der Marck, June 14th 1989
* Imaginary part of the fermionic 2-loop Z self-energy.
* eq. (5.18) ref 2b and simple QED and QCD correction factors.
*KEEP,IMPLICIT.
      IMPLICIT double precision (A-H,O-Z)
*KEND.
      double precision IMZ2
*KEEP,PARAMS.
      integer nrmass
      PARAMETER ( NRMASS = 9 )
*KEEP,MASSES.
      double precision RMASS(0:NRMASS), RMASS2(0:NRMASS),
     &          VF(0:NRMASS), AF(0:NRMASS), QF(0:NRMASS)
      COMMON/ MASSES / RMASS, RMASS2, VF, AF, QF
*KEEP,FORMMZ.
      COMPLEX*16 FZVMZ,FZAMZ,FGVMZ,FGAMZ
      COMMON/ FORMMZ /FZVMZ(0:NRMASS),FZAMZ(0:NRMASS),
     +                FGVMZ(0:NRMASS),FGAMZ(0:NRMASS)
*KEEP,BOSONS.
      double precision RMZ,RMW,RMH,RMT,SIN2TH,ZWID
      COMMON/ BOSONS / RMZ,RMW,RMH,RMT,SIN2TH,ZWID
*KEEP,ADHOC.
      double precision ALFA, HBARC2, PI, FACQCB, FACQCD, FACQED
      COMMON/ ADHOC  / ALFA, HBARC2, PI, FACQCB, FACQCD, FACQED
*KEND.
      Z = RMZ**2
      GZMIX  = SIGGZ(Z) / Z
      TOT = 0D0
      DO 10 I = 0 , NRMASS
         IF(S .GT. 4D0*RMASS2(I)) THEN
            IF(I .EQ. 0) THEN
               FAC1 = 3D0*FACQED*QF(I)**2
               FAC2 = 3D0 + FAC1
            ELSEIF(I .LE. 3) THEN
               FAC1 = 1D0*FACQED*QF(I)**2
               FAC2 = 1D0 + FAC1
            ELSEIF(I .EQ. 9) THEN
               FAC1 = 3D0*( FACQED*QF(I)**2*(1D0+FACQCB) + FACQCB )
               FAC2 = 3D0 + FAC1
            ELSE
               FAC1 = 3D0*( FACQED*QF(I)**2*(1D0+FACQCD) + FACQCD )
               FAC2 = 3D0 + FAC1
            ENDIF
            TOT = TOT +
     +           RMZ**2*ALFA*SQRT(1D0-4D0*RMASS2(I)/Z)*
     +         ( ( VF(I)**2 + AF(I)**2 )*( 1D0 + 2D0*RMASS2(I)/Z ) -
     +          6D0*RMASS2(I)/Z*AF(I)**2 )/3.D0 * FAC1
            TOT = TOT + FAC2 * 2D0/3D0*ALFA*RMZ**2*(
     +               VF(I)*( DREAL(FZVMZ(I)) + QF(I)*GZMIX )
     +             + AF(I)*  DREAL(FZAMZ(I)) )
         ENDIF
   10 CONTINUE
      TOT  = TOT * S/RMZ**2
      IMZ2 = TOT
      END
      FUNCTION IMSIGG(S)
*     ---------------
* Imaginary part of the 1-loop QED vacuumpolarization
*KEEP,IMPLICIT.
      IMPLICIT double precision (A-H,O-Z)
*KEND.
      double precision IMSIGG
*KEEP,PARAMS.
      integer nrmass
      PARAMETER ( NRMASS = 9 )
*KEEP,MASSES.
      double precision RMASS(0:NRMASS), RMASS2(0:NRMASS),
     &          VF(0:NRMASS), AF(0:NRMASS), QF(0:NRMASS)
      COMMON/ MASSES / RMASS, RMASS2, VF, AF, QF
*KEEP,BOSONS.
      double precision RMZ,RMW,RMH,RMT,SIN2TH,ZWID
      COMMON/ BOSONS / RMZ,RMW,RMH,RMT,SIN2TH,ZWID
*KEEP,ADHOC.
      double precision ALFA, HBARC2, PI, FACQCB, FACQCD, FACQED
      COMMON/ ADHOC  / ALFA, HBARC2, PI, FACQCB, FACQCD, FACQED
*KEND.
      TOT = 0D0
      FAC = 1D0
      DO 10 I = 1 , NRMASS
         IF(I .EQ. 4) FAC = 3D0
         IF(S .GT. 4D0*RMASS2(I)) TOT = TOT + FAC * QF(I)**2*
     +        SQRT(1D0-4D0*RMASS2(I)/S)*(1D0+2D0*RMASS2(I)/S)/3D0
   10 CONTINUE
      IF(S .GT. 4D0*RMW**2) TOT = TOT -
     +         SQRT(1D0-4D0*RMW**2/S)*(3D0/4D0+RMW**2/S)
      IMSIGG = TOT * ALFA * S
      END
      FUNCTION IMSGGZ(S)
*     ---------------
* Imaginary part of the 1-loop Z-gamma mixing
*KEEP,IMPLICIT.
      IMPLICIT double precision (A-H,O-Z)
*KEND.
      double precision IMSGGZ
*KEEP,PARAMS.
      integer nrmass
      PARAMETER ( NRMASS = 9 )
*KEEP,MASSES.
      double precision RMASS(0:NRMASS), RMASS2(0:NRMASS),
     &          VF(0:NRMASS), AF(0:NRMASS), QF(0:NRMASS)
      COMMON/ MASSES / RMASS, RMASS2, VF, AF, QF
*KEEP,BOSONS.
      double precision RMZ,RMW,RMH,RMT,SIN2TH,ZWID
      COMMON/ BOSONS / RMZ,RMW,RMH,RMT,SIN2TH,ZWID
*KEEP,ADHOC.
      double precision ALFA, HBARC2, PI, FACQCB, FACQCD, FACQED
      COMMON/ ADHOC  / ALFA, HBARC2, PI, FACQCB, FACQCD, FACQED
*KEND.
      CW2 = 1D0 - SIN2TH
      TOT = 0D0
      FAC = 1D0
      DO 10 I = 1 , NRMASS
         IF(I .EQ. 4) FAC = 3D0
         IF(S .GT. 4D0*RMASS2(I)) TOT = TOT - S*QF(I)*VF(I)/3D0* FAC*
     +             SQRT(1D0-4D0*RMASS2(I)/S)*(1D0+2D0*RMASS2(I)/S)
   10 CONTINUE
      IF(S .GT. 4D0*RMW**2) TOT = TOT-S*AF(1)*SQRT(1D0-4D0*RMW**2/S)*
     +                ( (3D0*CW2+1D0/6D0)+RMW**2/S*(4D0*CW2+4D0/3D0) )
      IMSGGZ = TOT * ALFA
      END
      SUBROUTINE FLBOT(QSQR,FZL9,FGL9)
*     ----------------
* W.J.P. Beenakker, F.A.Berends and S.C. van der Marck, June 14th 1989
* The left handed Z-bb and gamma-bb form factors
* see eqs (C.8),(C.9), (C.14) ff. of ref 2b.
*KEEP,IMPLICIT.
      IMPLICIT double precision (A-H,O-Z)
*KEEP,PARAMS.
      integer nrmass
      PARAMETER ( NRMASS = 9 )
*KEEP,MASSES.
      double precision RMASS(0:NRMASS), RMASS2(0:NRMASS),
     &          VF(0:NRMASS), AF(0:NRMASS), QF(0:NRMASS)
      COMMON/ MASSES / RMASS, RMASS2, VF, AF, QF
*KEEP,BOSONS.
      double precision RMZ,RMW,RMH,RMT,SIN2TH,ZWID
      COMMON/ BOSONS / RMZ,RMW,RMH,RMT,SIN2TH,ZWID
*KEND.
      COMPLEX*16 FI(1:7),GI(1:7),ZLFIN,TOT1,TOT2,FZL9,FGL9,HELP
      COMPLEX*16 C1,C2,C3,C4,C5,C6,C7,C8,C9,C10
      COMPLEX*16 B1BAR,C0SCAL,C1PLUS,C2ZERO,C2MIN,C2PLUS
      SW = SQRT( SIN2TH )
      CW2 = 1D0 - SIN2TH
      CW  = SQRT( CW2 )
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
      ZLFIN = 1D0/2D0/SIN2TH*( 2D0 + RT**2/RMW**2 )*(
     +         B1BAR(RB**2,RT,RMW) )
      FI(1) = (2D0/3D0*SIN2TH-1D0)/4D0/CW/SW * ZLFIN
      GI(1) = -1D0/6D0*ZLFIN
      HELP  = -1.5D0 + 2D0*LOG(RMW/RT) + 4D0*C4 - 2D0*QSQR*(C5 - C3)
     +        + 4D0*QSQR*(C2-.5D0*C1)
      FI(2) = (VF(8)+AF(8))/4D0/SIN2TH*( HELP )
     +      - (VF(8)-AF(8))/4D0/SIN2TH*2D0*RT**2*C1
      GI(2) =  1D0/6D0/SIN2TH*( HELP - 2D0*RT**2*C1 )
      HELP = -1.5D0 + 12D0*C9 - 2D0*QSQR*( C10 - C8 ) + 4D0*QSQR*C7
      FI(3) = -  CW/4D0/SIN2TH/SW * HELP
      GI(3) = - 1D0/4D0/SIN2TH    * HELP
      HELP = RT**2/RMW**2*( -.75D0 + LOG(RMW/RT) + 2D0*C4 -
     +       QSQR*( C5 - C3 ) )
      FI(4) = (VF(8)-AF(8))/4D0/SIN2TH*HELP -
     +        (VF(8)+AF(8))/4D0/SIN2TH*RT**4/RMW**2*C1
      GI(4) = 1D0/6D0/SIN2TH*( HELP - RT**4/RMW**2*C1 )
      HELP  = RT**2/RMW**2*( - .25D0 + 2D0*C9 )
      FI(5) = (SIN2TH-CW2)/8D0/SIN2TH/SW/CW*HELP
      GI(5) = - 1D0/4D0/SIN2TH*HELP
      FI(6) = - RT**2/4D0/SW/CW *C6
      GI(6) =   RT**2/4D0/SIN2TH*C6
      FI(7) = FI(6)
      GI(7) = GI(6)
      TOT1 = 0D0
      TOT2 = 0D0
      DO 10 I = 1 , 7
         TOT1 = TOT1 + FI(I)
         TOT2 = TOT2 + GI(I)
   10 CONTINUE
      FZL9 = TOT1
      FGL9 = TOT2
      END
      FUNCTION B0BAR(QSQR,RM1,RM2)
*     --------------
* eq. (C.15)
*KEEP,IMPLICIT.
      IMPLICIT double precision (A-H,O-Z)
*KEND.
      COMPLEX*16 DCMPLX,B0BAR
      IF(ABS(RM1-RM2) .GT. 1D-10) THEN
         B0BAR = 1D0 - (RM1**2+RM2**2)/(RM1**2-RM2**2)*LOG(RM1/RM2) +
     +           DCMPLX( FREAL(QSQR,RM1,RM2) , FIMAG(QSQR,RM1,RM2) )
      ELSE
         B0BAR = DCMPLX( FREAL(QSQR,RM1,RM2) , FIMAG(QSQR,RM1,RM2) )
      ENDIF
      END
      FUNCTION B1BAR(QSQR,RM1,RM2)
*     --------------
* eq. (C.16)
*KEEP,IMPLICIT.
      IMPLICIT double precision (A-H,O-Z)
*KEND.
      COMPLEX*16 DCMPLX,B1BAR
      IF(ABS(RM1-RM2) .GT. 1D-10) THEN
         B1BAR = -.25D0 + RM1**2/(RM1**2-RM2**2)*LOG(RM1/RM2) +
     +           ( RM2**2-RM1**2-QSQR )/2D0/QSQR*
     +            DCMPLX( FREAL(QSQR,RM1,RM2) , FIMAG(QSQR,RM1,RM2) )
      ELSE
         B1BAR = -.25D0 + .5D0 +
     +           ( RM2**2-RM1**2-QSQR )/2D0/QSQR*
     +            DCMPLX( FREAL(QSQR,RM1,RM2) , FIMAG(QSQR,RM1,RM2) )
      ENDIF
      END
      FUNCTION C0SCAL(QSQR,RM1,RM2,RMF)
*     ---------------
* The scalar 3 point function with equal external masses.
* eq. (5.10), (C.17)
*KEEP,IMPLICIT.
      IMPLICIT double precision (A-H,O-Z)
*KEND.
      COMPLEX*16 X(1:3),Y(1:3,1:2),HELP,AA,BB,CC,DD,FF,TOT,C0SCAL
      COMPLEX*16 SPENCE,DCMPLX
      AA = DCMPLX(RMF**2,0D0)
      BB = DCMPLX(QSQR,0D0)
      CC = - BB
      DD = DCMPLX(RM1**2 - RM2**2 - RMF**2,0D0)
      FF = DCMPLX(RM2**2,-1D-15)
      ALPHA = 2D0*RMF**2/QSQR/( 1D0 + SQRT(1D0-4D0*RMF**2/QSQR) )
      X(1) = - ( DD + 2D0*AA + CC*ALPHA )/(CC+2D0*ALPHA*BB)
      X(2) = - DD/( (1D0-ALPHA)*(CC+2D0*ALPHA*BB) )
      X(3) = DD/ALPHA/(CC+2D0*ALPHA*BB)
      HELP = CDSQRT( CC**2 - 4D0*BB*( AA + DD + FF ) )
      IF(DREAL(CC) .GE. 0D0) THEN
         Y(1,1) = ( - CC - HELP )/2D0/BB
         Y(1,2) = 4D0*BB*( AA + DD + FF )/(-CC-HELP)/2D0/BB
      ELSE
         Y(1,1) = 4D0*BB*( AA + DD + FF )/(-CC+HELP)/2D0/BB
         Y(1,2) = ( - CC + HELP )/2D0/BB
      ENDIF
      HELP = CDSQRT( DD**2 - 4D0*FF*( AA + BB + CC ) )
      IF(DREAL(DD) .GE. 0D0) THEN
         Y(2,1) = ( - DD - HELP )/2D0/AA
         Y(2,2) = 4D0*FF*( AA + BB + CC )/(-DD-HELP)/2D0/AA
      ELSE
         Y(2,1) = 4D0*FF*( AA + BB + CC )/(-DD+HELP)/2D0/AA
         Y(2,2) = ( - DD + HELP )/2D0/AA
      ENDIF
      Y(3,1) = Y(2,1)
      Y(3,2) = Y(2,2)
      TOT = 0D0
      DO 20 J = 1 , 2
         DO 10 L = 1 , 3
            TOT = TOT + (-1D0)**L*(SPENCE(  X(L)     /(X(L)-Y(L,J)) )
     +                            -SPENCE( (X(L)-1D0)/(X(L)-Y(L,J)) ) )
   10    CONTINUE
   20 CONTINUE
      C0SCAL = TOT / ( CC + 2D0*ALPHA*BB )
      END
      FUNCTION C1PLUS(QSQR,RM1,RM2,RMF)
*     ---------------
* eq. (C.9)
*KEEP,IMPLICIT.
      IMPLICIT double precision (A-H,O-Z)
*KEND.
      COMPLEX*16 C0SCAL,HELP,B0BAR,C1PLUS
      HELP = LOG(RM2/RM1) + B0BAR(QSQR,RM1,RM1) -
     +       B0BAR(RMF**2,RM1,RM2) + (RM2**2-RM1**2+RMF**2)*
     +       C0SCAL(QSQR,RM1,RM2,RMF)
      C1PLUS = HELP / ( 4D0*RMF**2 - QSQR )
      END
      FUNCTION C2ZERO(QSQR,RM1,RM2,RMF)
*     ---------------
* eq. (C.9)
*KEEP,IMPLICIT.
      IMPLICIT double precision (A-H,O-Z)
*KEND.
      COMPLEX*16 B0BAR,C0SCAL,C1PLUS,C2ZERO
      C2ZERO = .25D0*( B0BAR(QSQR,RM1,RM1) + 1D0 ) +
     +         .5D0*( RM1**2 - RM2**2 - RMF**2 )*
     +         C1PLUS(QSQR,RM1,RM2,RMF) + .5D0*RM2**2*
     +         C0SCAL(QSQR,RM1,RM2,RMF)
      END
      FUNCTION C2PLUS(QSQR,RM1,RM2,RMF)
*     ---------------
* eq. (C.9)
*KEEP,IMPLICIT.
      IMPLICIT double precision (A-H,O-Z)
*KEND.
      COMPLEX*16 B0BAR,B1BAR,C1PLUS,C2ZERO,HELP,C2PLUS
      HELP = .5D0*B0BAR(QSQR,RM1,RM1) + .5D0*( B1BAR(RMF**2,RM2,RM1)
     +       - .25D0 ) + ( RM2**2-RM1**2+RMF**2 )*
     +       C1PLUS(QSQR,RM1,RM2,RMF) - C2ZERO(QSQR,RM1,RM2,RMF)
      C2PLUS = HELP / ( 4D0*RMF**2 - QSQR )
      END
      FUNCTION C2MIN(QSQR,RM1,RM2,RMF)
*     --------------
* eq. (C.9)
*KEEP,IMPLICIT.
      IMPLICIT double precision (A-H,O-Z)
*KEND.
      COMPLEX*16 HELP,B1BAR,C2ZERO,C2MIN
      HELP = -.5D0*( B1BAR(RMF**2,RM2,RM1) - .25D0 ) -
     +         C2ZERO(QSQR,RM1,RM2,RMF)
      C2MIN = HELP / QSQR
      END
      FUNCTION SPENCE(X)
*     ---------------
* Hans Kuijf, 1988
* SPENCE(X) calcs the complex spence-function, through mapping on
* the area where there is a quickly convergent series.
      COMPLEX*16 X, SPENC, SPENCE
      double precision PI
      parameter (PI=3.141592653589793238d0)
* Map the x on the unit circle.
* But so that x is not in the neighbourhood of (1,0)
* ABS(Z)=-CDLOG(1D0-X) is always smaller than 1.10
* But (1.10)^19/(19!)*bernoulli(19)=2.7D-15
      IF (CDABS(1D0-X).LT.1D-13) THEN
        SPENCE=PI*PI/6D0
      ELSE IF (CDABS(1D0-X).LT.0.5D0) THEN
        SPENCE=PI*PI/6D0-CDLOG(1D0-X)*CDLOG(X)-SPENC(1D0-X)
      ELSE IF (CDABS(X).GT.1D0) THEN
        SPENCE=-PI*PI/6D0-0.5D0*CDLOG(-X)*CDLOG(-X)-SPENC(1D0/X)
      ELSE
        SPENCE = SPENC(X)
      END IF
      END
      FUNCTION SPENC(X)
      COMPLEX*16 X,SUM,Z,Z2,SPENC
      Z=-CDLOG(1D0-X)
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
      FUNCTION PHADPI(QSQR)
*     ---------------
* W.J.P. Beenakker, F.A.Berends and S.C. van der Marck, June 14th 1989
* Real part of the hadronic part of sigma-g(qsqr) / qsqr = pi-hadronic,
* calculated perturbatively.
*KEEP,IMPLICIT.
      IMPLICIT double precision (A-H,O-Z)
*KEEP,PARAMS.
      integer nrmass
      PARAMETER ( NRMASS = 9 )
*KEEP,MASSES.
      double precision RMASS(0:NRMASS), RMASS2(0:NRMASS),
     &          VF(0:NRMASS), AF(0:NRMASS), QF(0:NRMASS)
      COMMON/ MASSES / RMASS, RMASS2, VF, AF, QF
*KEEP,ADHOC.
      double precision ALFA, HBARC2, PI, FACQCB, FACQCD, FACQED
      COMMON/ ADHOC  / ALFA, HBARC2, PI, FACQCB, FACQCD, FACQED
*KEND.
      S   = QSQR
      TOT = 0D0
      FAC = 3D0
      DO 10 I = 4 , NRMASS
         RM = RMASS(I)
         IF(I.NE.8) TOT = TOT + 4D0/3D0*QF(I)**2*(
     +         ( S+2D0*RMASS2(I) )*FREAL(S,RM,RM) - S/3D0 ) * FAC/S
   10 CONTINUE
      PHADPI = ALFA/4D0/PI * TOT
      END
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
*KEEP,IMPLICIT.
      IMPLICIT double precision (A-H,O-Z)
*KEND.
      COMPLEX*16 HADRQQ
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
        ALFA=1./ALFAIN
      ENDIF
      T=ABS(S)
      IF(T.LT.0.3**2) THEN
        REPIAA=A1+B1*LOG(1.+C1*T)
      ELSEIF(T.LT.3.**2) THEN
        REPIAA=A2+B2*LOG(1.+C2*T)
      ELSEIF(T.LT.100.**2) THEN
        REPIAA=A3+B3*LOG(1.+C3*T)
      ELSE
        REPIAA=A4+B4*LOG(1.+C4*T)
      ENDIF
C     as imaginary part take -i alfa/3 Rexp
      HADRQQ=REPIAA-(0.,1.)*ALFA/3.*REXP(S)
      END
      FUNCTION REXP(S)
C  HADRONIC IRREDUCIBLE QQ SELF-ENERGY: IMAGINARY
*KEEP,IMPLICIT.
      IMPLICIT double precision (A-H,O-Z)
*KEND.
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
        BB(NDIM)=0.
        DO 4 I=1,NDIM
          IF(I.LT.NDIM) BB(I)=(RR(I)-RR(I+1))/(WW(I)-WW(I+1))
          AA(I)=RR(I)-BB(I)*WW(I)
    4   CONTINUE
      ENDIF
      REXP=0.D0
      IF(S.GT.0.D0) THEN
        W=REAL(SQRT(S))
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
