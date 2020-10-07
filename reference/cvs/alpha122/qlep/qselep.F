      SUBROUTINE QSELEP(SLOUT,XBANK,ICLEP,IERR)
CKEY QSELEP/INTERNAL
C-------------------------------------------------------------------------------
C! Lepton selection for Heavy Flavour (QSELEP algorithm)
C     QSELEP from LEPTAG written by Mark Parsons , 1st version in June 1992
C                extracted from LEPTAG by   I.Ten Have            July 1994
C                 &             HEVLEP from P.Perret
C     Adapted to ALPHA by M-N Minard                            10-Nov-1994
C
C  Modified by Gary Taylor 23/10/96 --> put in ALPHA 122.22 26/02/97
C  - to take into account muon selection as specified in LSMC cards
C  - to allow to modify the dedx cut in lepton id if available as
C    specified in LSEC card
C
C This subroutine implements the lepton selection definition given in
C Aleph-note ALEPH 92-101 "Lepton and jet definitions for the lepton paper"
C (D. Abbaneo et al). It finds all high momentum e+,e-,mu+, and mu- particles
C in an event and calculates their inclusive and exclusive Pt. It creates
C bank PDLT for Lepton id and PLJT for recording the jet used in that
C analysis.
C
C Monte carlo truth available is calculated and stored in PMLT bank
C
C Inputs:
C -------
C           INTEGER SLOUT    = Unit number on which debug and QSELEP error
C                              information is to be written.
C           LOGICAL XBANK    = Controls whether the PDLT bank will be
C                              produced and filled
C Outputs:
C --------
C           INTEGER ICLEP    = The number of leptons found in the event
C
C           INTEGER IERR     = Error return code from the lepton finder
C
C     IERR = 0     ! No error occurred
C     IERR < 0     ! An error occurred while processing the event.
C
C     IERR = -1    ! Too few good tracks.
C     IERR = -2    ! LEP energy too low or too high.
C     IERR = -3    ! Less than 10% of LEP energy visible.
C     IERR = -4    ! While trying to find the charge of a lepton
C                  ! ALPHA returned the "incorrect" charge.
C     IERR = -5    ! The BOS space booked for leptons and the number
C                  ! of leptons written out differ.
C     IERR = -6    ! BOS was unable to book the space requested.
C     IERR = -7    ! There were less than 2 jets in this event.
C     IERR = -8    ! Lepton to jet search failed.
C     IERR = -9    ! Lepton Pt calculation failed.
C     IERR = -10   ! Event has bad HV or DAQ status.
C     IERR = -11   ! Event has either no tracks or no energy flow objects.
C     IERR = -12   ! Unable to calculate the year corresponding to this run.
C     IERR = -13   ! QTRUTH returned an undocumented value.
C
C-------------------------------------------------------------------------------
#ifndef DOC
      IMPLICIT NONE
#include "qdecl.h"
#include "qcde.h"
C  HAC parameter
      INTEGER NLENG , IGARB, KPDLT, KPMLT, JPDLT , JPMLT ,ICLEP
#include "pljtjj.h"
C
C Subroutine parameters and function prototypes
C
C--
C-- Cut variable prototypes
C--
      COMMON/HADSEL/MINGDT,MINVIS,MINHIT,D0CUT,Z0CUT,MCOSTH
      INTEGER MINGDT,MINHIT
      REAL MINVIS,D0CUT,Z0CUT,MCOSTH
C--
      COMMON/EPTSEL/TPCEP,D0MXEP,Z0MXEP,CSTHEP,PEPMIN,PTEPMN
      INTEGER TPCEP
      REAL D0MXEP,Z0MXEP,CSTHEP,PEPMIN,PTEPMN
C--
      COMMON/EPSEL/R2CUT,R3CUT,MINWIR,R5CUT
      INTEGER MINWIR
      REAL R2CUT(2),R3CUT(2),R5CUT(2)
C--
      COMMON/MUTSEL/TPCMU,D0MXMU,Z0MXMU,CSTHMU,PMUMIN,PTMUMN
      INTEGER TPCMU
      REAL D0MXMU,Z0MXMU,CSTHMU,PMUMIN,PTMUMN
C--
      COMMON/MUSEL/NQMUID,QMACC
      INTEGER NQMUID,QMACC(20)
C--
      COMMON/CONDAL/DXYCUT,DZCUT,MXINV,RWCONV
      REAL DXYCUT,DZCUT,MXINV
      INTEGER RWCONV
C--
      COMMON/JETSEL/NTRJET,MINJET,ENTRJT,SYCUT,RWPTS
      REAL ENTRJT , SYCUT
      INTEGER NTRJET,MINJET,IERJET,RWPTS
C--
      REAL YCUT
      INTEGER NJETS
      CHARACTER*8 JNAM
C--
      COMMON /LTPRIV/ LASTRN,NLPTEV,LTFRST,YEARID,LTFWCD
      INTEGER LASTRN,NLPTEV,YEARID,LTFWCD
      LOGICAL LTFRST
C--
      INTEGER NAMIND,NBANK,NLINK,NATR,NDROP
      REAL DUMMY
      LOGICAL BADTRK,CHTSIM,XBANK
      INTEGER SLOUT,NLEP,IERR
C
C Variables
C
      INTEGER IDPMLT,NWPMLT,IDX,ITK,NGOOD,NLEPT,NPOSI,NELEC,NMUPL,
     @        NMUMI,NCONV,NBOTH,IEREL,NS,IERDEX,NC1,NC2,
     @        IERQPA,IRUN,IBE,IBT,IM1,IM2,NEXP,NFIR,N10,N03,ISHAD,
     @        IDF,IMCF,IERMU,SIZE,
     @        IDJET,LSJET,JETID,II,NJET,ETK,NTRACK,I,
     @        CPYEAR,CPIDL,SIDX,
     @        IMCFL,IFLAV,IMCT,JMBEST,IPAR,
     @        IPOP,ICHAIN,ISPLIT,LEPID,IBMES,IQUAL
      REAL    ETOT,R2,R3,RI,TL,RIEXP,SIGMA,R5,DXY,DZ0,DZ2,DTH,RMA,
     @        ZMA,XMA,P(3),XMULT,RAPP,ANG,SUDNT,EXPFIR,
     @        PLEPT(3),PJET(4),VINP,COSTHE,SINTHE,PTINC,PTEXCL,
     @        DIN1,DIN2,EJET,WEIGHT
      logical gmuon
C
C     QSELEP counters
C
      COMMON /LPTPUB/ LEPCNT
      INTEGER NCNT
      PARAMETER(NCNT = 25)
      INTEGER LEPCNT(NCNT)
      INTEGER KPLJT,JPLJT,IJET
      INTEGER LTNCHR,LTQLPR,LTECHR,LTACHR,LTBOSC,LTBOSS,LTNJET,LTJTAS,
     @        LTPTFL,LTHVDQ,LTHAD ,LTNLEP,LTYLEP,LTCONV,LTELMU,LTNEMI,
     @        LTNEPL,LTNMMI,LTNMPL,LTNCNV,LTCALL,LTNEFO,LTGTYF,LTCPDF,
     @        LTBRFL
C
      PARAMETER(LTNCHR= 1,LTQLPR= 2,LTECHR= 3,LTACHR= 4,LTBOSC= 5,
     @          LTBOSS= 6,LTNJET= 7,LTJTAS= 8,LTPTFL= 9,LTHVDQ=10,
     @          LTHAD =11,LTNLEP=12,LTYLEP=13,LTCONV=14,LTELMU=15,
     @          LTNEMI=16,LTNEPL=17,LTNMMI=18,LTNMPL=19,LTNCNV=20,
     @          LTCALL=21,LTNEFO=22,LTGTYF=23,LTCPDF=24,LTBRFL=25)
      REAL QLPEAK
      PARAMETER ( QLPEAK = 91.2 )
C
#include "qmacro.h"
C-------------------------------------------------------------------------------
C If this is the first call to QSELEP then initialize the bank format and
C write out the welcome message. Also work what type and year of data this
C is.
C
      IF (IW(NAMIND('QLID')).EQ.0) GO TO 9999
      IF (LTFRST) THEN
        LASTRN = KRUN
        CALL GTYEAR
        WRITE(SLOUT,1000)
        IF (LTFWCD.EQ.0) WRITE(SLOUT,1007)
        IF (LTFWCD.EQ.1) WRITE(SLOUT,1008)
        IF (LTFWCD.EQ.2) WRITE(SLOUT,1009)
        IF (XMCEV) THEN
          WRITE(SLOUT,1010) YEARID
        ELSE
          WRITE(SLOUT,1011) YEARID
        ENDIF
        WRITE(SLOUT,1000)
        CALL BKFMT('PDLT','2I,(2I,2F,2I)')
        CALL BKFMT('PMLT','2I,(8I)')
        CALL BKFMT('PLJT','2I,(4F,I)')
        LTFRST = .FALSE.
      ENDIF
      LEPCNT(LTCALL) = LEPCNT(LTCALL)+1
C
C Check to see that the run number hasn't changed. If it has then write
C out a warning if the last run contained no good events. Also calculate
C the year we are running on.
C
      IF (LASTRN.NE.KRUN) THEN
        IF (NLPTEV.EQ.0) THEN
          WRITE(SLOUT,*) 'WARNING-> QSELEP found no good leptons in ',
     @                   'the previous run (',LASTRN,')'
        ENDIF
        NLPTEV = 0
        LASTRN = KRUN
        CALL GTYEAR
      ENDIF
      IF (YEARID.EQ.0) GOTO 912
C
C Occasionally BOS will not drop the PMLT bank created during the previous
C event. This is checked for here and if a spurious PMLT bank is found it
C is dropped before proceeding.
C
      KPDLT  = NDROP('PDLT',0)
      KPMLT  = NDROP('PMLT',0)
      KPLJT  = NDROP('PLJT',0)
C
C Check to see we have at least the minimum number of charged tracks and
C more then zero energy flow objects to study. This is basically a crash
C protection against strange input.
C
      ICLEP = 0
      IERR = 0
      IF (KNCHT.LE.0.OR.KNEFT.LE.0) GOTO 911
C
C Zero all Alpha locks and user flags.
C
      CALL QLZER(KRECO)
      DO 100 ITK = KFCHT,KLCHT
        CALL QLUTRK(ITK)
        DO 100 IDX = 1,18
          CALL QSTFLI(ITK,IDX,0)
  100 CONTINUE
      DO 105 ITK = KFEFT,KLEFT
        CALL QLUTRK(ITK)
        DO 105 IDX = 1,18
          CALL QSTFLI(ITK,IDX,0)
  105 CONTINUE
C
C The following Alpha user flags are used:
C
C        Flag        Value        Comment
C        ----        -----        -------
C          8            n         CHT -> EFT track pointer (CHT only)
C         10            1         Genuine electron
C         10            2         Genuine positron
C         10            3         Conversion / pair e+ or e-
C         10           -1         Fake electron
C         10           -2         Fake positron
C         12            n         EFT -> JET track pointer (EFT only)
C         15            1         Genuine mu+
C         15            2         Genuine mu-
C         15           -1         Fake mu+
C         15           -2         Fake mu-
C         16          13,14       IDF code from QMUIDO if mu+ or mu-
C
C Check that the HCAL, ECAL, ITC, DEDX high voltage bits are set and that
C this event does not contain a DAQ error. Have to do extra test to see if
C this is Monte Carlo as the HV info is not available for MC data.
C
      IF (.NOT.XMCEV) THEN
        IF (.NOT.(XVHCAL.AND.XVECAL.AND.XVTPCD.AND.XVITC)) GOTO 910
      ENDIF
      IF (KEVEES.NE.1) GOTO 910
C
C Test each charged track to see if it passes basic track cuts.
C If it does then increment the number of good tracks counter and add it's
C energy to the ETOT variable. Then cut on the final values. The bad tracks
C are not locked at this point. They are locked seperately for the
C electron/positron and muon routines as the cuts to define what is/is not
C a good track are different.
C
      NGOOD = 0
      ETOT = 0.0
      DO 110 ITK=KFCHT,KLCHT
        IF (.NOT.BADTRK(ITK,MCOSTH,MINHIT,D0CUT,Z0CUT)) THEN
          NGOOD = NGOOD+1
          ETOT = ETOT+QE(ITK)
        ENDIF
  110 CONTINUE
C
C Enough good tracks and energy to continue?
C
      IF (NGOOD.LT.MINGDT) GOTO 901
      IF (QELEP.LT.80.0.OR.QELEP.GT.300.0) GOTO 902
      IF (ETOT.LT.MINVIS*QELEP) GOTO 903
      LEPCNT(LTHAD) = LEPCNT(LTHAD)+1
C-------------------------------------------------------------------------------
C--   Do jetfinding
C--
      CALL QJOPTR('EF',' ')
      JNAM = 'ENJETS  '
      YCUT =  SYCUT*(QLPEAK**2/QELEP**2)
      CALL QJMMCL(NJETS,JNAM,KRECO,YCUT,QELEP)
      IF (NJETS.LT.2) GO TO 907
C-------------------------------------------------------------------------------
C Loop over all charged tracks locking any track which fails to satisfy
C the basic track cuts for consideration as an electron or positron.
C
      NLEPT = 0
      NPOSI = 0
      NELEC = 0
      NMUPL = 0
      NMUMI = 0
      NCONV = 0
      NBOTH = 0
      NGOOD = 0
      DO 120 ITK = KFCHT,KLCHT
        IF (BADTRK(ITK,CSTHEP,TPCEP,D0MXEP,Z0MXEP)) THEN
          CALL QLTRK(ITK)
        ELSE
          NGOOD = NGOOD+1
        ENDIF
  120 CONTINUE
C
C-------------------------Start of electron/positron loop----------------------
C
      DO 200 ITK=KFCHT,KLCHT
        IF (XLOCK(ITK)) GOTO 200
C
C Cut on the minimum momentum of the track.
C
        IF (QP(ITK).LT.PEPMIN) GOTO 200
C
C Calculate the R2, R3 estimators.
C
        IF (XEID(ITK)) THEN
          R2 = QEIDRI(ITK,2)
          R3 = QEIDRI(ITK,3)
        ELSE
          GOTO 200
        ENDIF
C
C The R5 dE/dx estimator is a bit more complicated.
C Call DEDXM if old Monte-Carlo
C
        NS = 0
C Gary Taylor 23/10/96
C change default value of R5 to allow a cut to be made
C on electron dedx when available
C
        R5 = +998.
        IF(.NOT.CHTSIM(DUMMY)) THEN
        CALL QDEDXM(ITK,1,QPMASS('e+'),QCH(ITK),
     @              RI,NS,TL,RIEXP,SIGMA,IERDEX)
        ELSE
        CALL QDEDX(ITK,1,QPMASS('e+'),QCH(ITK),
     @              RI,NS,TL,RIEXP,SIGMA,IERDEX)
        ENDIF
        IF (IERDEX.NE.0) GO TO 199
        R5 = (RI - RIEXP)/SIGMA
 199    CONTINUE
C
C Apply the R3 cut.
C
        IF (R3.LE.R3CUT(1).OR.R3.GE.R3CUT(2)) GOTO 200
C
C Apply the R2 cut.
C
        IF (R2.LE.R2CUT(1).OR.R2.GE.R2CUT(2)) GOTO 200
C
C Apply the R5 cut.
C
        IF (NS.LT.MINWIR.OR.R5.LE.R5CUT(1).OR.R5.GE.R5CUT(2)) GOTO 200
C
C Electron/positron candidate has passed R2, R3, and R5 cuts so flag it in
C user flag 16.
C
        IF (KCH(ITK).EQ.-1) THEN
          CALL QSTFLI(ITK,10,1)
          NLEPT = NLEPT+1
          NELEC = NELEC+1
        ELSE IF (KCH(ITK).EQ.1) THEN
          CALL QSTFLI(ITK,10,2)
          NLEPT = NLEPT+1
          NPOSI = NPOSI+1
        ELSE
          GOTO 904
        ENDIF
C
C At this point we want to remove any pairs and Dalitz decays.
C We search for pairs using QPAIRF. Each track is compared with the current
C track to see if they are really a pair from a photon conversion.
C Don't want to compare the track with itself or compare like sign
C tracks as these can obviously never come from a photon conversion.
C
        DO 240 IDX = KFCHT,KLCHT
          IF (ITK.EQ.IDX.OR.(QCH(IDX)*QCH(ITK).GE.0.)) GOTO 240
          CALL QPAIRF(ITK,IDX,DXY,DZ0,DZ2,DTH,RMA,ZMA,XMA,
     @                NC1,DIN1,NC2,DIN2,P,IERQPA)
          IF (IERQPA.NE.0) GOTO 240
          IF (ABS(DXY).GE.DXYCUT) GOTO 240
          IF (ABS(DZ2).GE.DZCUT) GOTO 240
          IF (XMA.GE.MXINV) GOTO 240
C
C If we get to here then we've found a conversion. Flag it and adjust counters.
C
          IF (KRDFL(ITK,10).EQ.1) NELEC = NELEC-1
          IF (KRDFL(ITK,10).EQ.2) NPOSI = NPOSI-1
          NCONV = NCONV+1
          LEPCNT(LTNCNV) = LEPCNT(LTNCNV)+1
          IF(KRDFL(ITK,10).EQ.1) CALL QSTFLI(ITK,10,3)
          IF(KRDFL(ITK,10).EQ.2) CALL QSTFLI(ITK,10,4)
          IF( RWCONV.GT.0) GO TO 240
          NLEPT = NLEPT-1
          GOTO 200
  240   CONTINUE
C
C Succesfully processed that track - now the next.
C
  200 CONTINUE
C-------------------------End of electron/positron loop------------------------
C
C Reset all the track locks here and then call BADTRK with the muon track
C cuts. Visible energy and number of good track cuts are not repeated as they
C are assumed to have been passed the first time BADTRK was called for the
C electrons and positrons.
C
      NGOOD = 0
      DO 300 ITK=KFCHT,KLCHT
        CALL QLUTRK(ITK)
  300 CONTINUE
      DO 310 ITK=KFCHT,KLCHT
        IF (BADTRK(ITK,MCOSTH,MINHIT,D0CUT,Z0CUT)) THEN
           CALL QLTRK(ITK)
        ELSE
          IF (BADTRK(ITK,CSTHMU,TPCMU,D0MXMU,Z0MXMU)) THEN
            CALL QLTRK(ITK)
           ELSE
             NGOOD = NGOOD+1
           ENDIF
         ENDIF
  310 CONTINUE
C
C-------------------------Start of mu+/mu- loop--------------------------------
C
      DO 400 ITK=KFCHT,KLCHT
        IF (XLOCK(ITK)) GOTO 400
C
C First do a cut on the minimum track momentum for consideration as a muon.
C
        IF (QP(ITK).LT.PMUMIN) GOTO 400
C
C Selecting muons is easy with QMUIDO 8.0. Just call it and store muon
C if IDF=13 or 14. Corresponding to 1 or 2 muon chamber hits + high purity
C heavy flavour cuts.
C
        CALL QMUIDO(ITK,IRUN,IBE,IBT,IM1,IM2,NEXP,NFIR,N10,N03,
     @              XMULT,RAPP,ANG,ISHAD,SUDNT,IDF,IMCF,IERMU)
        IF (IERMU.NE.0) GOTO 400
C
C To enable booking of histograms if NEXP=0 then we set EXPFIR to 999.0.
C This has NO effect on the muons we actually select.
C
        IF (NEXP.NE.0) THEN
          EXPFIR = FLOAT(NFIR)/FLOAT(NEXP)
        ELSE
          EXPFIR = 999.0
        ENDIF
C
C Gary Taylor 23/10/96
C  Pay attention to LSMC card for muon id
C
         gmuon=.false.
         do i=1,nqmuid
          if(idf.eq.QMACC(i)) gmuon=.true.
         enddo
         IF (gmuon) THEN
C
C Flag what we've found as being either a mu+ or mu- in the user flags.
C
          IF (KCH(ITK).EQ.-1) THEN
            CALL QSTFLI(ITK,15,2)
            CALL QSTFLI(ITK,16,IDF)
            NLEPT = NLEPT+1
            NMUMI = NMUMI+1
          ELSE IF (KCH(ITK).EQ.1) THEN
            CALL QSTFLI(ITK,15,1)
            CALL QSTFLI(ITK,16,IDF)
            NLEPT = NLEPT+1
            NMUPL = NMUPL+1
          ELSE
            GOTO 904
          ENDIF
C
C Test here to see if the muon we have just selected was already
C flagged as a genuine electron or conversion. If it was we clear the
C electron flag as it is much more likely to actually be a muon. We
C modify the counters and increment NBOTH to reflect this strange occurence.
C
          IF (KRDFL(ITK,10).EQ.1) THEN
            NELEC = NELEC-1
            NLEPT = NLEPT-1
            CALL QSTFLI(ITK,10,0)
            NBOTH = NBOTH+1
            LEPCNT(LTELMU) = LEPCNT(LTELMU)+1
          ENDIF
          IF (KRDFL(ITK,10).EQ.2) THEN
            NPOSI = NPOSI-1
            NLEPT = NLEPT-1
            CALL QSTFLI(ITK,10,0)
            NBOTH = NBOTH+1
            LEPCNT(LTELMU) = LEPCNT(LTELMU)+1
          ENDIF
          IF (KRDFL(ITK,10).EQ.3) THEN
            NCONV = NCONV-1
            CALL QSTFLI(ITK,10,0)
            NBOTH = NBOTH+1
            LEPCNT(LTCONV) = LEPCNT(LTCONV)+1
          ENDIF
        ENDIF
C
C Succesfully processed that track - now the next.
C
  400 CONTINUE
C
C-------------------------End of mu+/mu- loop----------------------------------
C
C If we get to here then everything has worked ok. We either have or do not
C have leptons. If we don't then just exit at this point with IERR set to zero.
C If we have found (a) useful lepton(s) then create the PDLT bank and add it
C to the 'E' list ready for writing out. If no writing out takes places this
C will have no adverse effect.
C
      IF (NLEPT.EQ.0) THEN
        LEPCNT(LTNLEP) = LEPCNT(LTNLEP)+1
        IERR = 0
        GOTO 900
      ENDIF
C
C Check that we have at least 2 jets. If not then exit with an error.
C
      NJET = 0
      IDJET = KPDIR(JNAM,KRECO)
      LSJET = IDJET
  520 IF (IDJET.NE.0) THEN
        NJET = NJET+1
        IDJET = KFOLLO(IDJET)
        GOTO 520
      ENDIF
      IF ( XBANK ) THEN
      NLENG = LPLJTA*NJET +LMHLEN
      KPLJT = NDROP('PLJT',0)
      CALL AUBOS('PLJT',0,NLENG,KPLJT,IGARB)
      IF ( IGARB.EQ.2) THEN
        WRITE(SLOUT,*) 'Not enough space to load PLJT bank ',KRUN,KEVT
        GO TO 906
      ENDIF
      IW(KPLJT+LMHROW) = NJET
      IW(KPLJT+LMHCOL) = LPLJTA
      IDJET = LSJET
      IJET = 0
  525 IF (IDJET.NE.0) THEN
        IJET = IJET+1
        JPLJT = KROW(KPLJT,IJET)
        RW(JPLJT+JPLJPX) = QX(IDJET)
        RW(JPLJT+JPLJPY) = QY(IDJET)
        RW(JPLJT+JPLJPZ) = QZ(IDJET)
        RW(JPLJT+JPLJPE) = QE(IDJET)
        IW(JPLJT+JPLJNO) = 0
        IDJET = KFOLLO(IDJET)
        GOTO 525
      ENDIF
      ENDIF
      IERJET =0
      IF (NJET.LT.MINJET) THEN
        IERJET = 40
      ENDIF
C
C Fill user flag 12 of each energy flow object with the jet that the
C object belongs to. Any object not matched is flagged zero.
C
      DO 570 ITK = KFEFT,KLEFT
        IDJET = KPDIR(JNAM,KRECO)
  580   IF (IDJET.NE.0) THEN
          IF (XSAME(ITK,IDJET)) THEN
            JPLJT = KROW(KPLJT,1-IDJET+LSJET)
            IW(JPLJT+JPLJNO) = IW(JPLJT+JPLJNO)+1
            CALL QSTFLI(ITK,12,IDJET)
            GOTO 570
          ENDIF
          IDJET = KFOLLO(IDJET)
          GOTO 580
        ENDIF
  570 CONTINUE
C
C Fill user flag 8 of each charged track with the id number of its
C corresponding energy flow object. Any object not matched will be flagged
C zero.
C
      DO 585 ITK = KFCHT,KLCHT
        DO 586 IDX = KFEFT,KLEFT
          IF (XSAME(ITK,IDX)) THEN
            IF (KRDFL(ITK,8).NE.0) THEN
              CALL QSTFLI(ITK,8,-IDX)
            ELSE
              CALL QSTFLI(ITK,8,IDX)
            ENDIF
          ENDIF
  586   CONTINUE
  585 CONTINUE
C
C For each good lepton check to see that the jet it belongs to passes two jet
C cuts: Firstly that there are enough objects in the jet and secondly that
C the lepton doesn't contribute more than a certain percentage of the
C total energy of the jet. If the lepton fails these cuts then it is deleted
C from the list of good leptons.
C
C If the lepton to energy flow object to jet association is found to have
C failed QSELEP returns with an error.
C
      DO 590 ITK = KFCHT,KLCHT
        IF (KRDFL(ITK,10).EQ.1.OR.KRDFL(ITK,10).EQ.2.OR.
     @    ((KRDFL(ITK,10).EQ.3.OR.KRDFL(ITK,10).EQ.4).AND.RWCONV.GT.0).
     @    OR.KRDFL(ITK,15).EQ.1.OR.KRDFL(ITK,15).EQ.2) THEN
              IF (KRDFL(ITK,8).LE.0) THEN
                CALL QSTFLI(ITK,8,-1000)
                GO TO 590
              ENDIF
          IDJET = KRDFL(KRDFL(ITK,8),12)
          IF (IDJET.EQ.0) THEN
             CALL QSTFLI(ITK,8,-2000)
             GO TO 590
          ENDIF
          NTRACK = 0
          DO 610 IDX = KFEFT,KLEFT
            IF (KRDFL(IDX,12).EQ.IDJET) NTRACK = NTRACK+1
  610     CONTINUE
          EJET = QP(ITK) / QE(IDJET)
          IF (NTRACK.GE.NTRJET.AND.EJET.LT.ENTRJT) GO TO 590
          CALL  QSTFLI(ITK,8,-3000)
C         GO TO 590
        ENDIF
  590 CONTINUE
C
C Check again to see if we have any good leptons left. Have to
C do this because the jet cuts above may have removed all of our good
C leptons.
C
      IF (NLEPT.EQ.0) THEN
        LEPCNT(LTNLEP) = LEPCNT(LTNLEP)+1
        IERR = 0
        GOTO 900
      ENDIF
C-------------------   Filling of bank PDLT  ----------------------------------
C Reserve enough room for the bank. This is the bank size * the number of
C leptons + 2 for the bank size + number of rows information. After this
C point if an error occurs the PDLT bank is always dropped before leaving
C QSELEP.
C
      IF ( XBANK ) THEN
      NLENG = NLEPT*LPDLTA + LMHLEN
      CALL AUBOS('PDLT',0,NLENG,KPDLT,IGARB)
      IF ( IGARB.EQ.2) THEN
        WRITE(SLOUT,*) 'Not enough space to load PDLT bank ',KRUN,KEVT
        GO TO 906
      ENDIF
C
C Fill the first 2 words of PDLT so that the PDLT bank obeys the aleph
C bank standard. The first word corresponds to the number of words
C per lepton. The second corresponds to the number of leptons.
C
      IW(KPDLT+LMHCOL) = LPDLTA
      IW(KPDLT+LMHROW) = NLEPT
      IF( XMCEV ) THEN
      NLENG = NLEPT*LPMLTA + LMHLEN
      CALL AUBOS('PMLT',0,NLENG,KPMLT,IGARB)
      IF ( IGARB.EQ.2) THEN
        WRITE(SLOUT,*) 'Not enough space to load PDLT bank ',KRUN,KEVT
        GO TO 906
      ENDIF
      IW(KPMLT+LMHCOL) = LPMLTA
      IW(KPMLT+LMHROW) = NLEPT
      ENDIF
      ENDIF
C
C Now loop through all the charged tracks reading the user flags and
C creating lepton banks for those that have flags set
C
C Initialisation
C
      IDX = 0
      CALL VZERO(PJET,4)
      CALL VZERO(PLEPT,3)
C
C Start the main writeout loop
C
      DO 500 ITK=KFCHT,KLCHT
        PTINC = 0
        PTEXCL = 0
C
C Check to see if this track is flagged as a good lepton.
C or a conversion to be kept
C
        IF (KRDFL(ITK,10).EQ.1.OR.KRDFL(ITK,10).EQ.2.OR.
     @  ((KRDFL(ITK,10).EQ.3.OR.KRDFL(ITK,10).EQ.4).AND.RWCONV.GT.0).
     @    OR.KRDFL(ITK,15).EQ.1.OR.KRDFL(ITK,15).EQ.2) THEN
C
C Find the energy flow track which corresponds to our lepton.
C
          ETK = KRDFL(ITK,8)
C
C Generate an error if we can't associate our lepton with an energy flow
C track.
C
          PTINC = 0
          PTEXCL = 0
          IF (ETK.LE.0) THEN
            IERJET = 10
            GO TO 501
          ENDIF
C
C Find which jet this lepton belongs to.
C
          JETID = KRDFL(ETK,12)
C
C Check that we've associated this correctly if not generate an error.
C
          IF (JETID.EQ.0) THEN
            IERJET =20
            GOTO 501
          ENDIF
C
C Get the details about the lepton candidate
C
          PLEPT(1) = QX(ETK)
          PLEPT(2) = QY(ETK)
          PLEPT(3) = QZ(ETK)
C
C Go to the jet and pick up the jet four-vector.
C
          PJET(1) = QX(JETID)
          PJET(2) = QY(JETID)
          PJET(3) = QZ(JETID)
          PJET(4) = QE(JETID)
C
C Calculate the inclusive Pt (easy ain't it?). Note that the charged
C track and not the enflw object is used in this calculation.
C
          PTINC = QPPER(ITK,JETID)
C
C Calculate the exclusive Pt by subtracting the lepton momentum
C from the jet.
C
C The recipe is as follows:
C
C        o First subtract the lepton energy flow object from the jet.
C        o Next calculate theta using the jet and the lepton track.
C        o Then calculate the exclusive Pt using the lepton track momentum.
C
          DO II = 1,3
            PJET(II) = PJET(II) - PLEPT(II)
          ENDDO
C
C Calculate the pt using the charged track exiting if an error occurs.
C
          PLEPT(1) = QX(ITK)
          PLEPT(2) = QY(ITK)
          PLEPT(3) = QZ(ITK)
C
          PJET(4) = SQRT(PJET(1)**2+PJET(2)**2+PJET(3)**2)
          VINP=PLEPT(1)*PJET(1)+PLEPT(2)*PJET(2)+PLEPT(3)*PJET(3)
          IF (PJET(4).EQ.0..OR.QP(ITK).EQ.0.) THEN
            IERJET = 20
            GO TO 501
          ENDIF
          COSTHE = VINP/(PJET(4)*QP(ITK))
          SINTHE = 1 - COSTHE**2
          IF (SINTHE.GE.0) THEN
            SINTHE = SQRT(SINTHE)
          ELSE
            IERJET = 30
            GO TO 501
          ENDIF
          PTEXCL = QP(ITK)*SINTHE
          IF((KRDFL(ITK,10).EQ.1.OR.KRDFL(ITK,10).EQ.2.OR.
     @  ((KRDFL(ITK,10).EQ.3.OR.KRDFL(ITK,10).EQ.4).AND.RWCONV.GT.0))
     @   .AND.PTEXCL.LT.PTEPMN) THEN
            IERJET = 50
          ENDIF
          IF((KRDFL(ITK,15).EQ.1.OR.KRDFL(ITK,15).EQ.2).AND.PTEXCL.LT
     @    .PTMUMN) THEN
            IERJET = 50
          ENDIF
C
C-   Now fill PDLT bank
C
  501     CONTINUE
          IF ( RWPTS.GT.0.OR.IERJET.EQ.0) GO TO 502
  600     IF (KRDFL(ITK,10).EQ.1) THEN
            NELEC = NELEC-1
            NLEPT = NLEPT-1
            CALL QSTFLI(ITK,10,0)
          ENDIF
          IF (KRDFL(ITK,10).EQ.2) THEN
            NPOSI = NPOSI -1
            NLEPT = NLEPT-1
            CALL QSTFLI(ITK,10,0)
          ENDIF
          IF (KRDFL(ITK,10).EQ.3) THEN
            NLEPT = NLEPT-1
            CALL QSTFLI(ITK,10,0)
          ENDIF
          IF (KRDFL(ITK,10).EQ.4) THEN
            NLEPT = NLEPT-1
            CALL QSTFLI(ITK,10,0)
          ENDIF
          IF (KRDFL(ITK,15).EQ.1) THEN
            NMUPL = NMUPL-1
            NLEPT = NLEPT-1
            CALL QSTFLI(ITK,15,0)
            CALL QSTFLI(ITK,16,0)
          ENDIF
          IF (KRDFL(ITK,15).EQ.2) THEN
            NMUMI = NMUMI-1
            NLEPT = NLEPT-1
            CALL QSTFLI(ITK,15,0)
            CALL QSTFLI(ITK,16,0)
          ENDIF
          GO TO 500
 502      CONTINUE
          IF ( XBANK ) THEN
          ICLEP = ICLEP + 1
C
C Word 5.
          JPDLT = KROW(KPDLT,ICLEP)
          IW(JPDLT+JPDLFR)  = KTN(ITK) ! Julia track number
C
C Word 1.
C
C For the electrons/positrons the particle code is multiplied by 10* the
C track's quality flag returned by KEIDIF.
C KEIDIF = 0 - good
C        = 1 - estimation done in crack region
C        = 2 - estimation done in overlap region
C To remove this information just do a mod 10 on the stored word....
C
          IEREL = 0
          IQUAL = 0
          IF (KRDFL(ITK,10).EQ.1) THEN
            IF (XEID(ITK)) IEREL = ABS(KEIDIF(ITK))
            IQUAL  = INT(3+10*IEREL)! Electron
            LEPCNT(LTNEMI) = LEPCNT(LTNEMI)+1
          ENDIF
          IF (KRDFL(ITK,10).EQ.2) THEN
            IF (XEID(ITK)) IEREL = ABS(KEIDIF(ITK))
            IQUAL  = INT(2+10*IEREL)! POSITR ON
            LEPCNT(LTNEPL) = LEPCNT(LTNEPL)+1
          ENDIF
          IF (KRDFL(ITK,10).EQ.3) THEN
            IF (XEID(ITK)) IEREL = ABS(KEIDIF(ITK))
            IQUAL  = INT(103+10*IEREL)! Electron
            LEPCNT(LTNEMI) = LEPCNT(LTNEMI)+1
          ENDIF
          IF (KRDFL(ITK,10).EQ.4) THEN
            IF (XEID(ITK)) IEREL = ABS(KEIDIF(ITK))
            IQUAL  = INT(102+10*IEREL)! Positron
            LEPCNT(LTNEPL) = LEPCNT(LTNEPL)+1
          ENDIF
          IF (KRDFL(ITK,15).EQ.1) THEN
            IF (KRDFL(ITK,16).EQ.14)
     @        IQUAL = 5         ! Mu+
            IF (KRDFL(ITK,16).EQ.13)
     @        IQUAL = 15        ! Mu+
            IF (KRDFL(ITK,16).EQ.11)
     @        IQUAL = 25        ! Mu+
            IF (KRDFL(ITK,16).EQ.15)
     @        IQUAL = 35        ! Mu+
            IF (KRDFL(ITK,16).EQ.12.or.KRDFL(ITK,16).LT.11)
     @        IQUAL = 45        ! Mu+
            LEPCNT(LTNMPL) = LEPCNT(LTNMPL)+1
          ENDIF
          IF (KRDFL(ITK,15).EQ.2) THEN
            IF (KRDFL(ITK,16).EQ.14)
     @        IQUAL = 6         ! Mu-
            IF (KRDFL(ITK,16).EQ.13)
     @        IQUAL = 16        ! Mu-
            IF (KRDFL(ITK,16).EQ.11)
     @        IQUAL = 26        ! Mu-
            IF (KRDFL(ITK,16).EQ.15)
     @        IQUAL = 36        ! Mu-
            IF (KRDFL(ITK,16).EQ.12.or.KRDFL(ITK,16).LT.11)
     @        IQUAL = 46        ! Mu-
            LEPCNT(LTNMMI) = LEPCNT(LTNMMI)+1
          ENDIF
          IW(JPDLT+JPDLPA) = IQUAL
C
C Word 2.
C
          IF ( JETID.EQ.0) THEN
            IW(JPDLT+JPDLJT) = 0
          ELSE
            IW(JPDLT+JPDLJT)  =  LSJET - JETID + 1      ! Jet number
          ENDIF
          IF ( IERJET.NE.0 ) THEN
            IW(JPDLT+JPDLVP) = IERJET
          ELSE
            IW(JPDLT+JPDLVP) = 0
          ENDIF
C
C Word 3.
C
          RW(JPDLT+JPDLPI)  =     PTINC               ! PT INCLUSIVE
C
C Word 4.
C
          RW(JPDLT+JPDLPE)  =     PTEXCL            ! PT EXCLUSIVE
C---------------  Call QTRUTH and fill bank PMLT  ------------------------------
C If Monte Carlo truth exists then call QTRUTH to evaluate the Monte Carlo
C history information of the lepton, fill PMLT bank
C
C Input = track number. Output = parent quark and decay type.
C
          IF (.NOT.XMCEV) GO TO 500
          JPMLT = KROW(KPMLT,ICLEP)
          IMCFL = 0
          IFLAV = 0
          IPAR = 0
          IF (XMCEV) THEN
            CALL QTRUTH(ITK,IFLAV,IPOP,ICHAIN,ISPLIT,LEPID,IBMES)
            IF(IFLAV.GT.5.OR.IFLAV.LT.0) THEN
              IERR = -13
              GO TO 913
            ENDIF
C
C Input = lepton track number. Output = # of the associated MC track
C
            IF (IFLAV.GT.0) THEN
              IMCT = JMBEST(ITK)
C  Find the parent of the lepton
  555         IF (IMCT.GT.0) THEN
                IPAR = KMOTH(IMCT,1)
                IF (KTPCOD(IMCT).EQ.KTPCOD(IPAR))THEN
                  IMCT = IPAR
                  GOTO 555
                ENDIF
              ENDIF
            ENDIF
          ENDIF
C
C Fill the PMLT bank word by word.
C
C
C Word 8.
          IW(JPMLT+JPMLFR) = KTN(ITK) ! JULIA TRACK NUMBER
C
C Word 1.
C
          IW(JPMLT+JPMLFL) = IFLAV ! PRIMARY QUARK  FLAG
C
C Word 2.
C
          IW(JPMLT+JPMLPO)  = IPOP ! IPOP
C
C Word 3.
C
          IW(JPMLT+JPMLCH)  = ICHAIN ! ICHAIN
C
C Word 4.
C
          IW(JPMLT+JPMLSP)  = ISPLIT  ! ISPLIT
C
C Word 5.
C
          IW(JPMLT+JPMLLE) = LEPID !  LEPID
C
C Word 6.
C
          IW(JPMLT+JPMLME) = IBMES ! IBMES
C
C Word 7
C
          IW(JPMLT+JPMLKT) = IMCT - KFMCT + 1
        ENDIF
      ENDIF
  500 CONTINUE
 
C
C   Fill number of leptons
C
      IF ( XBANK) THEN
      IW(KPDLT+LMHROW) = ICLEP
      IF(XMCEV) THEN
      IW(KPMLT+LMHROW) = ICLEP
      ENDIF
      CALL AUBPRS('PDLTPMLT')
      ENDIF
C
C Zero all locks and user  flags as per the calling convention.
C
      CALL QLZER(KRECO)
      DO 510 ITK = KFCHT,KLCHT
        CALL QLUTRK(ITK)
        DO 510 IDX = 1,18
          CALL QSTFLI(ITK,IDX,0)
  510 CONTINUE
      DO 530 ITK = KFEFT,KLEFT
        CALL QLUTRK(ITK)
        DO 530 IDX = 1,18
          CALL QSTFLI(ITK,IDX,0)
  530 CONTINUE
C
C Add the  bank to the E list so it is written out then return.
C
      LEPCNT(LTYLEP) = LEPCNT(LTYLEP)+1
      NLPTEV = NLPTEV+1
      IERR = NLEPT
      CALL BLIST(IW,'E+','PMLT')
      CALL BLIST(IW,'E+','PDLT')
      CALL BLIST(IW,'E+','PLJT')
      GO TO 9999
C---------------  Error handling -----------------------------------------------
C Here - grouped together - are all the error returns. Individual
C comments describe what they actually mean. All negative values of
C IERR are actual errors. Zero return means no problem but no leptons
C found. Positive values indicate no problems and N leptons found.
C
  901 CONTINUE            !      'ERROR - too few good tracks'
      LEPCNT(LTNCHR) = LEPCNT(LTNCHR)+1
      IERR = -1           ! Too few good tracks.
      GOTO 900            !
  902 CONTINUE            !      'ERROR - LEP energy wrong'
      LEPCNT(LTQLPR) = LEPCNT(LTQLPR)+1
      IERR = -2           ! LEP energy too low or too high.
      GOTO 900            !
  903 CONTINUE            !      'ERROR - Less than 10% Ecm available'
      LEPCNT(LTECHR) = LEPCNT(LTECHR)+1
      IERR = -3           ! Less than 10% of LEP energy visible.
      GOTO 900            !
  904 CONTINUE            !      'ERROR - Alpha charge error'
      LEPCNT(LTACHR) = LEPCNT(LTACHR)+1
      IERR = -4           ! While trying to find the charge of a lepton
      GOTO 900            ! Alpha has returned a value other than -1 or +1.
C 905 CONTINUE            !      'ERROR - BOS space corruption'
C     LEPCNT(LTBOSC) = LEPCNT(LTBOSC)+1
C     IERR = -5           ! The BOS space booked for leptons and the number
C     GOTO 900            ! stored do not match - this should NEVER happen.
  906 CONTINUE            !      'ERROR - BOS unable to reserve space'
      LEPCNT(LTBOSS) = LEPCNT(LTBOSS)+1
      IERR = -6           ! BOS was unable to book the space requested.
      GOTO 900            !
  907 CONTINUE            !      'ERROR - Less than 2 jets found'
      LEPCNT(LTNJET) = LEPCNT(LTNJET)+1
      IERR = -7           ! There were less than 2 jets in this event.
      GOTO 900            !
C 908 CONTINUE            !      'ERROR - Lepton to jet assoc. failed'
C     LEPCNT(LTJTAS) = LEPCNT(LTJTAS)+1
C     IERR = -8           ! Couldn't find the jet to which the lepton belongs.
C     GOTO 900            !
C 909 CONTINUE            !      'ERROR - Lepton Pt calc failed'
C     LEPCNT(LTPTFL) = LEPCNT(LTPTFL)+1
C     IERR = -9           ! While trying to calculate the lepton Pt a
C     GOTO 900            ! floating point exception would have occured.
  910 CONTINUE            !      'ERROR - Bad HV or DAQ status'
      LEPCNT(LTHVDQ) = LEPCNT(LTHVDQ)+1
      IERR = -10          ! This event either had bad HV or bad DAQ status.
      GOTO 900            !
  911 CONTINUE            !      'ERROR - No energy flow objects'
      LEPCNT(LTNEFO) = LEPCNT(LTNEFO)+1
      IERR = -11          ! The event was found to have zero tracks or
      GOTO 900            ! zero energy flow objects.
  912 CONTINUE            !      'ERROR - Unable to calculate the year'
      LEPCNT(LTGTYF) = LEPCNT(LTGTYF)+1
      IERR = -12          ! GTYEAR has been unable to associate a year
      GOTO 900            ! with this run.
  913 CONTINUE            !      'ERROR - Bad return code from QTRUTH'
      LEPCNT(LTBRFL) = LEPCNT(LTBRFL)+1
      IERR = -13          ! QTRUTH returned a value outside its documented
      GOTO 900            ! return value ranges.
C
C Zero all locks and user flags as per the calling convention...
C
  900 CONTINUE                !  'Zeroing all locks and user flags'
      IF(KNCHT.GT.0.OR.KNEFT.GT.0) THEN
      CALL QLZER(KRECO)
      IF(KNCHT.GT.0) THEN
      DO 540 ITK = KFCHT,KLCHT
        CALL QLUTRK(ITK)
        DO 540 IDX = 1,18
          CALL QSTFLI(ITK,IDX,0)
  540 CONTINUE
      ENDIF
      IF(KNEFT.GT.0) THEN
      DO 550 ITK = KFEFT,KLEFT
        CALL QLUTRK(ITK)
        DO 550 IDX = 1,18
          CALL QSTFLI(ITK,IDX,0)
  550 CONTINUE
      ENDIF
      ENDIF
C
C ...and return.
C
 9999 CONTINUE
      RETURN
C-------------------------------------------------------------------------------
C Format statements.
C
 1000 FORMAT(1X,'+--------------------------------------------+')
 1001 FORMAT(1X,'| Welcome to QSELEP Version 1.0              |')
 1002 FORMAT(1X,'| -----------------------------              |')
 1003 FORMAT(1X,'|                                            |')
 1004 FORMAT(1X,'|                                            |')
 1005 FORMAT(1X,'|                                            |')
 1006 FORMAT(1X,'|                                            |')
 1007 FORMAT(1X,'| Weights will not be filled.                |')
 1008 FORMAT(1X,'| Weight filling selected (Altarelli model). |')
 1009 FORMAT(1X,'| Weight filling selected (ISGW model).      |')
 1010 FORMAT(1X,'| You are using ',I4,' monte carlo data.       |')
 1011 FORMAT(1X,'| You are using ',I4,' real data.              |')
C
      END
#endif
