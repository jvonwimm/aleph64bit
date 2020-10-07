      SUBROUTINE LPRLUM
C--------------------------------------------------------------------
C! Calculate and print luminosity for this run
C!
C!  Author       :- P.H.Hansen 89/01/01
C!  Modified     :- J. Wear and P. H. Hansen   89/11/19
C!                - J. Wear 90/02/09
C!                - P.H.Hansen 92/02/19
C!  Modified     : P. Comas      940502
C!           Use of X1TT instead of XTOP dropped in the readout upgrade.
C!  Modified     : P.H.Hansen    960501
C!           Print only Method 10. Background estimate is #events with
C!           dphi<20deg - corrected for the possible anisotropy of the
C!           the single arm combinations recorded in LBAK. If not
C!           enough method 10 events in LBAK use method 4.
C!  Modified : P. H. Hansen  16-Jun-1997 Use BHZ0 bank for Z0 runs from 1996
C!  Modified : P. H. Hansen  11-Aug-1997 Compensate for dead module in early
C!                                       1997 running
C!  Modified : P. H. Hansen  15-March-2000 Fill new row (11th) in LUMI
C!                                         with Lumi including miniramp 
C!
C!  Input:       'LUMI' 'BHAB'  'LBAK' 'LONL'
C!
C!  Description:
C!  ============
C!  Do final background update for this run
C!  Print energy and vertex summary
C!  Print summary of lumi triggers for this run
C!  For each method
C!     Print selection criteria
C!     Print numbers of events surviving successive cuts
C!     Print corresponding numbers of fake events
C!     Calculate background contribution
C!     Calculate luminosity and its statistical error
C--------------------------------------------------------------------
#ifndef DOC
#include "bcs.h"
#include "rcurnt.h"
#include "rconds.h"
#include "rparac.h"
#include "rflags.h"
#include "lcnamc.h"
#include "bhabjj.h"
#include "lumijj.h"
#include "lonljj.h"
#include "lbakjj.h"
#include "lphist.h"
#include "lehijj.h"
#include "xtopjj.h"
#include "x1ttjj.h"
#include "rlunit.h"
#include "rstatr.h"
      EXTERNAL NDROP
      PARAMETER (LLEN=50)
      CHARACTER*(LLEN) XLUMI(2)
      CHARACTER*(LLEN) TLUMI(9)
      CHARACTER*(LLEN) TCALC(4)
      CHARACTER*(LLEN) TMETH(11)
      CHARACTER*(LLEN) TQUAL(2)
      CHARACTER*(LLEN) TCUTS(16)
      CHARACTER*(LLEN) TRIGS(8)
      DATA XLUMI /
     $ '  Events inside fiducial area with dphi=180.......',
     $ '  Events inside fiducial area with dphi=0.........'/
      DATA TLUMI /
     $ '  Events with E1,E2 > threshold...................',
     $ '  Events with E1+E2 > threshold...................',
     $ '  Events inside fiducial with dphi=180 (signal)...',
     $ '  Events inside fiducial with dphi=0   (backgr)...',
     $ '  Accepted events with LCAL and ECAL on...........',
     $ '  Accepted events with LCAL ECAL ITC on...........',
     $ '  Accepted exclusive Low-Low triggers.............',
     $ '  Accepted exclusive Very High Singles............',
     $ '  Accepted without Very High Single bit...........'/
      DATA TCALC /
     $ '  Corrected Background contribution...............',
     $ '  Luminosity (HV on) for this run (nb-1)..........',
     $ '  Statistical error...............................',
     $ '  Systematic error................................'/
      DATA TMETH /
     $ 'Method 1: Use tracks to get accepted fraction     ',
     $ 'Method 2: Require track on small side             ',
     $ 'Method 3: Require tracks on both sides            ',
     $ 'Method 4: Use only cluster centroids              ',
     $ 'Method 5: Use asymmetry across loose  boundaries  ',
     $ 'Method 6: Use asymmetry across strict boundaries  ',
     $ 'Method 7: Require central tower inside boundaries ',
     $ 'Method 8: Improved Method 5                       ',
     $ 'Method 9: Improved Method 6                       ',
     $ 'Method10: Improved Method 6 with Sical in front   ',
     $ 'Method11: Method 10 including miniramp            '/
      DATA TQUAL /
     $ '  Track quality : track                           ',
     $ '  Track quality : track or patch                  '/
      DATA TCUTS /
     $ '  high energy threshold...........................',
     $ '  low energy threshold............................',
     $ '  acoplanarity cut................................',
     $ '  minimum distance to X edge (small area).........',
     $ '  minimum distance to X edge (large area).........',
     $ '  minimum distance to Y edge (small area).........',
     $ '  minimum distance to Y edge (large area).........',
     $ '  minimum abs(cos(phi)) (small area)..............',
     $ '  minimum abs(cos(phi)) (large area)..............',
     $ '  maximum abs(cos(phi)) (small area)..............',
     $ '  maximum abs(cos(phi)) (large area)..............',
     $ '  minimum theta (small area)......................',
     $ '  minimum theta (large area)......................',
     $ '  maximum theta (small area)......................',
     $ '  maximum theta (large area)......................',
     $ '  cross-section ..................................'/
      DATA TRIGS /
     $ '  GBX counts divided by 1024......................',
     $ '  Other triggers..................................',
     $ '  Exclusive single low triggers...................',
     $ '  Exclusive single high triggers..................',
     $ '  Exclusive single very high triggers.............',
     $ '  Exclusive low-low triggers......................',
     $ '  High-Low triggers...............................',
     $ '  Background collections..........................'/
C
C Fraction of Bhabhas with delta_phi=0
      DATA BHSS/0.00025/
#include "bmacro.h"
C--------------------------------------------------------------------
C
C Do background combinations on last events of this run
C
      CALL LBLDBA
      KLSIN=IW(NALSIN)
      IF(KLSIN.GT.0) KLSIN=NDROP('LSIN',0)
C
      KBHAB = IW(NABHAB)
C
C For LEPII calibration runs use BHZ0:
      IF( IRUNRC.GE.40000 .AND. ENERRC.LT.96. ) THEN
        KBHAB = IW(NABHZ0)
      ENDIF
C
      KLUMI = IW(NALUMI)
      KLONL = IW(NALONL)
      IF(KBHAB.LE.0.OR.KLUMI.LE.0.OR.KLONL.LE.0)            GOTO 999
      NMETH = LROWS(KBHAB)
      WRITE(LOUTRL,500)
C
C Print energy calibration
      CALL LPRCAL
C
C Trigger summary
      IF(JPRERF(5).GT.1) THEN
        WRITE(LOUTRL,'(/,1X,A)') 'Trigger summary:'
        DO 81 I=1,8
          WRITE(LOUTRL,'(1X,A,E12.4)') TRIGS(I),RTABL(KLONL,1,I)
   81   CONTINUE
        WRITE(LOUTRL,'(1X,A)') 'Downscale factors:'
        KXTOP = IW(NAMIND('XTOP'))
        IF(KXTOP.GT.0) THEN
          JOFF = KXTOP+LMHLEN+JXTODS-1
          WRITE(LOUTRL,'(1X,8I7)') (IW(JOFF+J),J=1,8)
        ELSE
          KX1TT = IW(NAMIND('X1TT'))
          IF(KX1TT.GT.0) THEN
             JOFF = KX1TT+LMHLEN+JX1TDS-1
             WRITE(LOUTRL,'(1X,8I7)') (IW(JOFF+J),J=1,8)
          ENDIF
        ENDIF
      ENDIF
C
C Determine correction factor - sqrt(s) dependence -
C for Bhabha xsect from reference MC
      CALL LBHCOR(FACT)
C
C Loop over requested methods
      IMMIN=10
      IMMAX=11
      IF(JPRERF(5).GT.1) IMMIN=8
      JBHAB = 0
      DO 100 IM=IMMIN,IMMAX
        IF(RTABL(KLUMI,IM,JLUMCP).LE.0.) GOTO 100
        JBHAB=1
        MET = ITABL(KBHAB,IM,JBHAME)
        IQUA = ITABL(KBHAB,IM,JBHAQF)
        IGEN = ITABL(KBHAB,IM,JBHAGE)
C
C Header for this method
        WRITE(LOUTRL,'(/,1X,A)') TMETH(MET)
C
C Signal is #events with delta_phi=180
        WRITE(LOUTRL,'(/,1X,A)') 'Real coincidences:'
        DO 92 I=1,2
          WRITE(LOUTRL,'(1X,A,F10.1)') TLUMI(I),RTABL(KLUMI,IM,I)
   92   CONTINUE
        DO 93 I=3,9
          WRITE(LOUTRL,'(1X,A,F10.1)') TLUMI(I),RTABL(KLUMI,IM,I+8)
   93   CONTINUE
        IF(MET.EQ.1.AND.RTABL(KLUMI,IM,JLUMTR).GT.0.) THEN
          FACC = RTABL(KLUMI,IM,JLUMF2)/RTABL(KLUMI,IM,JLUMTR)
          ABHA = RTABL(KLUMI,IM,JLUMSA)*FACC +
     &             RTABL(KLUMI,IM,JLUMF4)
          DABHA = SQRT(RTABL(KLUMI,IM,JLUMBH))
          ABHV = RTABL(KLUMI,IM,JLUMB2)
          DABHV = SQRT(ABHV)
        ELSE
          ABHA = RTABL(KLUMI,IM,JLUMBH)
          DABHA = SQRT(ABHA)
          ABHV = RTABL(KLUMI,IM,JLUMB2)
          DABHV = SQRT(ABHV)
        ENDIF
C
C Background is #events with delta_phi=0
C - real coincidences corrected for real Bhabhas
        AFORW = MAX(RTABL(KLUMI,IM,JLUMNP)-ABHV*BHSS,0.)
C - fake coincidences (require at least 10 for statistics)
        RLR=1.
        KLBAK = IW(NALBAK)
        IF(KLBAK.GT.0.) THEN
          IMB=IM
          BBNP=RTABL(KLBAK,IMB,JLBANP)
          BBBH=RTABL(KLBAK,IMB,JLBABH)
          IF(BBNP.LT.10.) THEN
            IMB=4
            BBBH=RTABL(KLBAK,IMB,JLBABH)
            BBNP=RTABL(KLBAK,IMB,JLBANP)
          ENDIF
          WRITE(LOUTRL,'(/,1X,A,I4,A)')
     &    'Fake coincidences:  Method',IMB,'  (low and VH)'
          WRITE(LOUTRL,'(1X,A,2F10.2)')
     &      XLUMI(1),BBBH,RTABL(KLBAK,IMB,JLBABH+1)
          WRITE(LOUTRL,'(1X,A,2F10.2)')
     &      XLUMI(2),BBNP,RTABL(KLBAK,IMB,JLBANP+1)
          IF(BBNP.GE.10.) RLR=BBBH/BBNP
        ENDIF
C
C Scale background by ratio of fake coincidences
C with delta_phi=180 to those with delpta_phi=0
        BBHV = 0.
        DBBHV = 0.
        IF (AFORW.GT.0.) THEN
          BBHV = AFORW*RLR
          DBBHV = SQRT(AFORW)*RLR
        ENDIF
C
C Now get luminosity
        SIGMA = RTABL(KBHAB,IM,JBHACS)*FACT
        IF(SIGMA.NE.0.)THEN
           ALUM  = (ABHV-BBHV)/SIGMA
           DALUM = SQRT(DABHV**2+DBBHV**2)/SIGMA
           SYST = RTABL(KBHAB,IM,JBHAER)/SIGMA
        ELSE
           ALUM=0.
           DALUM=0.
           SYST=0.
        ENDIF
        IF(IRUNRC.GE.43484.AND.IRUNRC.LE.43542) THEN
           ALUM = 2.*ALUM
           DALUM = 2.*DALUM
           SYST = 2.*SYST
        ENDIF
        WRITE(LOUTRL,'(//,1X,A,F10.2)') TCALC(2),ALUM
        WRITE(LOUTRL,'(1X,A,F10.2)') TCALC(3),DALUM
        WRITE(LOUTRL,'(1X,A,35X,F10.2)') '  Cross-section',SIGMA
        KLUM = KROW(KLUMI,IM)
        RW(KLUM+JLUMLU) = ALUM
        RW(KLUM+JLUMSE) = DALUM
        RW(KLUM+JLUMSY) = SYST
  100 CONTINUE
      IF(JBHAB.EQ.1) THEN
        WRITE(LOUTRL,510)
      ELSE
        WRITE(LOUTRL,'(1X,A)') 'No Bhabha events'
      ENDIF
C
  999 CONTINUE
C
  500 FORMAT(1X,80('!'),/,1X,
     &'Luminosity summary :',
     &//)
  510 FORMAT(1X,'--------------------------------------------',/)
      END
#endif
