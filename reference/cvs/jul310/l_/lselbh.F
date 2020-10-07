      SUBROUTINE LSELBH(ITYPE)
C-----------------------------------------------------------------------
C! Bhabha selection algorithm
C!
C!   Author   : P. H. Hansen  890701
C!   Modified :- E. Lancon             10-APR-1991
C!                    Use ALTRIG to Access Trigger bits
C!   Modified : P. H. Hansen           01-MAY-1996
C!                    Improved debug print. Fix reference energy.
C!   Modified : P. H. Hansen           24-JUN-1997
C!                    Fix SFRAC for methods < 9
C!                    
C!
C!     Input  :  ITYPE           Trigger type:
C!                               2: HI SINGLE Trigger
C!                               3: VH SINGLE Trigger
C!                               4: LOW-LOW Trigger
C!                               5: HIGH-LOW Trigger
C!              'BHAB'           Bhabha selection criteria
C!     Output : 'LUMI'           Selected Bhabha event statistics
C!   Description :
C!   ===========
C?      Decide which side has the smaller fiducial area.
C?      For each requested measurement method require succesively:
C?         At least one cluster over high threshold.
C?         Both clusters over low threshold.
C?         Coplanarity.
C?         Both clusters be contained in their proper fiducial areas.
C?      Update a counter in LUMI after each passed cut.
C?      In SATR region count events with tracks
C?         and events with tracks in the fiducial area.
C?
C-----------------------------------------------------------------------
C
#ifndef DOC
#include "rconds.h"
#include "bcs.h"
#include "rflags.h"
#include "lcnamc.h"
#include "lidtjj.h"
#include "lifljj.h"
#include "lumijj.h"
#include "bhabjj.h"
#include "alcons.h"
#include "rcurnt.h"
      INTEGER ALTRIG
      EXTERNAL ALTRIG
      LOGICAL FSATR,FTRAC,BTEST,XVH
      PARAMETER (NLBIT=5)
      DIMENSION LVHBT(NLBIT),LVHTW(NLBIT)
      CHARACTER*8 VHNAM
      DATA VHNAM /'VH_SING'/
      DATA LRUN  /-9999/
#include "bmacro.h"
C---------------------------------------------------------------------
C     IF(ITYPE.LE.2.OR.ITYPE.GE.6)                  GOTO 999
      KBHAB = IW(NABHAB)
      IF(KBHAB.LE.0)                                GOTO 999
      NMETH = LROWS(KBHAB)
      IF(NMETH.LE.0)                                GOTO 999
      KLUMI = IW(NALUMI)
      IF(KLUMI.LE.0)                                GOTO 999
C
C Select largest electron candidate clusters on the A and B side
      KLIDT = IW(NALIDT)
      IF(KLIDT.LE.0)                                GOTO 999
      NIDT = LROWS(KLIDT)
      IF(NIDT.LE.0)                                 GOTO 999
      KLIFL = IW(NALIFL)
      EA = 0.
      EB = 0.
      IA=0
      IB=0
      DO 30 I=1,NIDT
        EC = RTABL(KLIDT,I,JLIDEC)
        TH = RTABL(KLIDT,I,JLIDTC)
        IF(TH.LT.PIBY2.AND.EC.GT.EA) THEN
          EA = EC
          IA = I
        ENDIF
        IF(TH.GT.PIBY2.AND.EC.GT.EB) THEN
          EB = EC
          IB = I
        ENDIF
   30 CONTINUE
C
      IF(IA.EQ.0.OR.IB.EQ.0)                        GO TO 999
C
C Take beam energy
      EBEAM = ENERRC/2.
C
C Event weight
      W = 1.
      KKEVH = IW(NAMIND('KEVH'))
      IF(KKEVH.GT.0) W = RTABL(KKEVH,1,5)
C
C Acoplanarity
      PHA = RTABL(KLIDT,IA,JLIDPC)
      PHB = RTABL(KLIDT,IB,JLIDPC)
      DPHI = ABS(PHA-PHB)
      DPHI = AMIN1(DPHI,TWOPI-DPHI)
C
C Define small and large fiducial areas
      K1 = ITABL(KLIFL,IA,JLIFK1)
      K2 = ITABL(KLIFL,IB,JLIFK2)
      KE1 = ITABL(KLIFL,IA,JLIFKH)
      KE2 = ITABL(KLIFL,IB,JLIFKL)
      IF1 = ITABL(KLIDT,IA,JLIDIF)
      IF2 = ITABL(KLIDT,IB,JLIDIF)
      IF(MOD(IEVTRC,2).NE.0) THEN
        K1 = ITABL(KLIFL,IB,JLIFK1)
        K2 = ITABL(KLIFL,IA,JLIFK2)
        KE1 = ITABL(KLIFL,IB,JLIFKH)
        KE2 = ITABL(KLIFL,IA,JLIFKL)
        IF1 = ITABL(KLIDT,IB,JLIDIF)
        IF2 = ITABL(KLIDT,IA,JLIDIF)
      ENDIF
C
C SATR flag
      FSATR = .FALSE.
      IF(BTEST(IF1,2).AND.BTEST(IF2,2)) FSATR = .TRUE.
C
C HI Voltage flag
      CALL LHVSTA(IHV)
C
C Loop over methods
      DO 100 IM=1,NMETH
        KLUM = KROW(KLUMI,IM)
        IB = IM-1
C
C Method type
        MET = ITABL(KBHAB,IM,JBHAME)
C
C Required track quality
        IQA = ITABL(KBHAB,IM,JBHAQF)
C
C Track flag
        FTRAC = .FALSE.
        IF(BTEST(IF1,IQA).AND.BTEST(IF2,IQA)) FTRAC = .TRUE.
        IF(MET.EQ.2.AND.BTEST(IF1,IQA))       FTRAC = .TRUE.
C
C Threshold for energy sum (protect against old DB)
        IF(LCOLS(KBHAB).GE.25) THEN
          SFRAC = 0.5*RTABL(KBHAB,IM,JBHAES)/RTABL(KBHAB,IM,JBHAEG)
          IF(IRUNRC.GE.40000.AND.IM.GE.9) SFRAC=SFRAC*2.
        ELSE
          SFRAC = 0.6
        ENDIF
C
C Apply energy thresholds
        IF(.NOT.BTEST(KE1,IB))                        GOTO 100
        IF(.NOT.BTEST(KE2,IB))                        GOTO 100
        RW(KLUM+JLUMHI) = RW(KLUM+JLUMHI) + W
        IF((EA+EB)/(2.*EBEAM).LT.SFRAC)               GOTO 100
        RW(KLUM+JLUMLO) = RW(KLUM+JLUMLO) + W
C
C coplanarity
        IF(DPHI.GT.RTABL(KBHAB,IM,JBHADP))
     &    RW(KLUM+JLUMCP) = RW(KLUM+JLUMCP) + W
C
C Aplly acoplanatity as last cut
        IF(MET.GE.4.AND.BTEST(K1,IB).AND.BTEST(K2,IB)) THEN
          IF(DPHI.LT.PI-RTABL(KBHAB,IM,JBHADP)) THEN
            RW(KLUM+JLUMNP) = RW(KLUM+JLUMNP) + W
            IF(FDEBRF.AND.JDBDRF(5).GE.2.AND.MET.EQ.10) THEN
              WRITE(IW(6),'(2X,A,2F6.1,F8.3)')
     &        '+++LSELBH+++ Background coincidence (EA,EB,Dphi)',
     &        EA,EB,DPHI
            ENDIF
            GOTO 100
          ELSEIF(DPHI.GT.RTABL(KBHAB,IM,JBHADP)) THEN
            RW(KLUM+JLUMBH) = RW(KLUM+JLUMBH) + W
            IF(FDEBRF.AND.JDBDRF(5).GE.2.AND.MET.EQ.10) THEN
              WRITE(IW(6),'(2X,A,2F6.1,F8.3)')
     &        '+++LSELBH+++ Accepted coincidence     (EA,EB,Dphi)',
     &        EA,EB,DPHI
            ENDIF
            GOTO 90
          ENDIF
        ELSEIF(MET.GE.4) THEN
          IF(FDEBRF.AND.JDBDRF(5).GE.2.AND.MET.EQ.10) THEN
            WRITE(IW(6),'(2X,A,2F6.1,F8.3)')
     &      '+++LSELBH+++ Rejected coincidence     (EA,EB,Dphi)',
     &      EA,EB,DPHI
          ENDIF
          GOTO 100
        ENDIF
        IF(MET.EQ.1.AND.BTEST(K1,IB).AND.BTEST(K2,IB)) THEN
          IF(DPHI.LT.PI-RTABL(KBHAB,IM,JBHADP)) THEN
            RW(KLUM+JLUMNP) = RW(KLUM+JLUMNP) + W
            GOTO 100
          ELSEIF(DPHI.GT.RTABL(KBHAB,IM,JBHADP)) THEN
            RW(KLUM+JLUMBH) = RW(KLUM+JLUMBH) + W
          ENDIF
        ENDIF
C
C Count accepted events in SATR and no SATR regions seperately
        IF(FSATR) THEN
          RW(KLUM+JLUMSA) = RW(KLUM+JLUMSA) + W
          IF(FTRAC) THEN
            RW(KLUM+JLUMTR) = RW(KLUM+JLUMTR) + W
            IF(.NOT.BTEST(K1,IB))                   GOTO 100
            RW(KLUM+JLUMF1) = RW(KLUM+JLUMF1) + W
            IF(MET.EQ.2) THEN
              IF(DPHI.LT.PI-RTABL(KBHAB,IM,JBHADP)) THEN
                RW(KLUM+JLUMNP) = RW(KLUM+JLUMNP) + W
                GOTO 100
              ENDIF
              IF(DPHI.GT.RTABL(KBHAB,IM,JBHADP)) THEN
                RW(KLUM+JLUMBH) = RW(KLUM+JLUMBH) + W
                GOTO 90
              ENDIF
            ENDIF
            IF(.NOT.BTEST(K2,IB))                   GOTO 100
            RW(KLUM+JLUMF2) = RW(KLUM+JLUMF2) + W
            IF(MET.EQ.3) THEN
              IF(DPHI.LT.PI-RTABL(KBHAB,IM,JBHADP)) THEN
                RW(KLUM+JLUMNP) = RW(KLUM+JLUMNP) + W
                GOTO 100
              ENDIF
              IF(DPHI.GT.RTABL(KBHAB,IM,JBHADP)) THEN
                RW(KLUM+JLUMBH) = RW(KLUM+JLUMBH) + W
              ENDIF
            ENDIF
          ELSE
             IF(MET.NE.1)                            GOTO 100
             IF(.NOT.BTEST(K1,IB))                   GOTO 100
             IF(.NOT.BTEST(K2,IB))                   GOTO 100
          ENDIF
        ELSE
          IF(MET.NE.1)                               GOTO 100
          RW(KLUM+JLUMNS) = RW(KLUM+JLUMNS) + W
          IF(.NOT.BTEST(K1,IB))                      GOTO 100
          RW(KLUM+JLUMF3) = RW(KLUM+JLUMF3) + W
          IF(.NOT.BTEST(K2,IB))                      GOTO 100
          RW(KLUM+JLUMF4) = RW(KLUM+JLUMF4) + W
        ENDIF
   90   CONTINUE
C
C Count accepted events with correct HV bits
        IF(IHV.EQ.1) RW(KLUM+JLUMB1) = RW(KLUM+JLUMB1) + W
        IF(IHV.EQ.2) THEN
          RW(KLUM+JLUMB1) = RW(KLUM+JLUMB1) + W
          RW(KLUM+JLUMB2) = RW(KLUM+JLUMB2) + W
        ENDIF
C
C Count VH efficiency, exclusive VHs and exclusive LO-LO's
        IF ( ALTRIG(IT1,IT2,IL2).GT.0  ) THEN
          ITB = IT1
        ELSE
          ITB = 0
        ENDIF
C Get VH trigger bits
        IF (IRUNRC.NE.LRUN) THEN
          CALL LTGBIT(IRUNRC,VHNAM,LVHBT,LVHTW,NVHBT,IFVH)
          LRUN = IRUNRC
        ENDIF
C
C Check if any VHI triggers have fired.
        XVH = .FALSE.
        DO 22 IVHBT = 1,NVHBT
 22         XVH = XVH.OR.BTEST(ITB,LVHBT(IVHBT))
        IF(.NOT.XVH) RW(KLUM+JLUMNV) = RW(KLUM+JLUMNV) + W
        IF(ITYPE.EQ.3) RW(KLUM+JLUMVH) = RW(KLUM+JLUMVH) + W
        IF(ITYPE.EQ.4) RW(KLUM+JLUMLL) = RW(KLUM+JLUMLL) + W
  100 CONTINUE
C
  999 CONTINUE
      END
#endif
