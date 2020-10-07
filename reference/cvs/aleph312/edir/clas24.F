      SUBROUTINE CLAS24(IGOOD)
C-----------------------------------------------------------------------
CKEY EDIR TAU CLASS24
C! Steering routine for events of class 24.
C! Lepton pair selection by hadron, 2-gamma and cosmic rejection.
C-
C   Input  : None
C   Output : IGOOD  = Class  logical flag
C-
C   Input banks : PFRF,PFRT,EGPC
C-
C     from ALEPHLIB routine LEPTO
C
C                  Lepto :     Author : J.C.Brient        Date : 7/7/89
C                              Updated: C.Geweniger       Date : 7/2/90
C                  Tauclass :           Tau group         Date : 2/12/92
C                              Last modification by
C                                       S. Orteu                 2/2/93
C                              The changes can be obtained by looking
C                              at the commentaries CL15
C                  Updated:             M.Cattaneo        Date : 21/7/96
C                              Reject tracks with bad TRPFRF return code
C                              Protect against missing banks
C                                       G.Ganis           Date : 17/2/96
C                              Add PGAC in the searched banks list for 
C                              GAMPE(C,X,K) photons 
C-----------------------------------------------------------------------
#ifndef DOC
      SAVE
#include "bcs.h"
#include "pfrfjj.h"
#include "pfrtjj.h"
#include "egpcjj.h"
#include "pgpcjj.h"
#include "pgacjj.h"
C --
      DIMENSION PP1(4),PP2(4),VEPRO(3),ZVEC(3),CUTIS(20)
      PARAMETER (ITRMAX = 500)
      DIMENSION ITRQUA(ITRMAX),IJET(ITRMAX),AXTR(3)
      DIMENSION PJ1(4),PJ2(4)
      EXTERNAL NAMIND, VDOTN, VMOD, VADD
      LOGICAL IGOOD,FIRST
      DATA FIRST/.TRUE./
      DATA ZVEC/0.,0.,1./
#include "bmacro.h"
C --
      IGOOD = .FALSE.
C --
      IF(FIRST) THEN
        FIRST = .FALSE.
C --
C   all values of the cuts used in the selection
C   ----------------------------------------------
C                       Abs(Z0)
        CUTIS(1) = 10.0
C                       Abs(D0)
        CUTIS(2) =  2.0
C                       Max |cos| for ch.tr (not used)
        CUTIS(3) =  1.0
C                       Impulsion min. (gev/c)
        CUTIS(4) =  0.10
C                       Min. number of points in ITC (not used)
        CUTIS(5) =  0.0
C                       Min. number of points in TPC
        CUTIS(6) =  4.0
C                       Acol cut (not used)
        CUTIS(7) =  1.0
C                       At least one good tr. with p > 3.0 GeV
C--CL15 CUTIS(8) =   3.0
        CUTIS(8) =   2.0
C                       Cut on the smallest d0 (cosmic) at 0.5 cm
C                       (not used)
        CUTIS(9)=   0.5
C                       Max. opening angle
        CUTIS(10)=  0.85
C --
      ENDIF

      NEGPC = 0
      JEGPC = IW(NAMIND('EGPC'))
      IF(JEGPC.GT.0) NEGPC = LROWS(JEGPC)

      NPGPC = 0
      JPGPC = IW(NAMIND('PGPC'))
      IF(JPGPC.GT.0) NPGPC = LROWS(JPGPC)

      NPGAC = 0
      JPGAC = IW(NAMIND('PGAC'))
      IF(JPGAC.GT.0) NPGAC = LROWS(JPGAC)

      NPFRF = 0
      JPFRF = IW(NAMIND('PFRF'))
      IF(JPFRF.GT.0) NPFRF = LROWS(JPFRF)
C --
C  want at least 2 charged tracks
C -------------------------------
      IF(NPFRF.LT.2) GO TO 990
      IF(NPFRF.GT.ITRMAX) GO TO 990
C --
C  first select the good tracks
C ------------------------------
      KNCHS  = 0
      KNCHW  = 0
      ECHFRF = 0.
      PMA    = 0.
      SD0    = 10.
      JPFRT  = IW(NAMIND('PFRT'))
      IF( JPFRT .LE. 0 ) GOTO 11
      NPITC  = NINT(CUTIS(5))
      NPTPC  = NINT(CUTIS(6))
      DO 10 I = 1 , NPFRF
        ITRQUA(I) = 0
        Z0     =   RTABL(JPFRF,I,JPFRZ0)
        D0     =   RTABL(JPFRF,I,JPFRD0)
        CALL TRPFRF(I,PP1,IRF)
        IF( IRF .NE. 0 ) GOTO 10
        PP     =   ABS(PP1(4))
        CTX    =   ABS(PP1(3))/PP
        NPPT   =   ITABL(JPFRT,I,JPFRNT)
        NPPI   =   ITABL(JPFRT,I,JPFRNI)
        IF(NPPT     .LT.     NPTPC)     GO TO 10
C---    IF(NPPI     .LT.     NPITC)     GO TO 10
        IF(ABS(Z0)  .GT.  CUTIS(1))     GO TO 10
C---    IF(ABS(CTX) .GT.  CUTIS(3))     GO TO 10
        IF(PP       .LT.  CUTIS(4))     GO TO 10
        IF(ABS(D0)  .GT.    5.0   )     GO TO 10
          ITRQUA(I)     = 2
          KNCHW  = KNCHW + 1
        IF(ABS(D0)  .GT.  CUTIS(2))     GO TO 10
          ITRQUA(I)     = 1
          KNCHS  = KNCHS + 1
          IF(ABS(D0).LT.SD0) SD0 = ABS(D0)
          IF(PP.GT.PMA) PMA = PP
 10   CONTINUE
 11   CONTINUE
C --
C ADD GAMPEC PHOTON WITH E>CUTIS(8) GeV IN THE SEARCH FOR PMA
C
      PMAGAM = 0.
      IF (NEGPC.GT.0) THEN
        DO I = 1 , NEGPC
          EGAMMA =  SQRT ( RTABL(JEGPC,I,JEGPPX)**2  +
     &                     RTABL(JEGPC,I,JEGPPY)**2  +
     &                     RTABL(JEGPC,I,JEGPPZ)**2    )
          IF ( EGAMMA .GT. MAX(PMAGAM,CUTIS(8)) )   PMAGAM = EGAMMA
        ENDDO
      ELSE IF (NPGPC.GT.0) THEN
        DO I=1, NPGPC
          EGAMMA = RTABL(JPGPC,I,JPGPEC)
          IF ( EGAMMA .GT. MAX(PMAGAM,CUTIS(8)) )   PMAGAM = EGAMMA
        ENDDO
      ELSE IF (NPGAC.GT.0) THEN
        DO I=1, NPGAC
          EGAMMA = RTABL(JPGAC,I,JPGAEC)
          IF ( EGAMMA .GT. MAX(PMAGAM,CUTIS(8)) )   PMAGAM = EGAMMA
        ENDDO
      ENDIF
C --
C   KNCHW is the number of tracks with a wide d0 cut of 5 cm
C   SD0 is the smallest d0 for the "good" tracks
C   PMA is the highest momentum in the event
C --
C  first cut on charged multiplicity
C  ----------------------------------
C  use wide d0 cut for 2-track events, otherwise use narrow d0 cut
C  ---------------------------------------------------------------
      IF(KNCHW.NE.2) THEN
        IF(KNCHS.LT.2.OR.KNCHS.GT.8)  GO TO 990
      ENDIF
C --
C  reset track quality flag
C  ------------------------
      IF(KNCHW.EQ.2) KNCHS = 0
      DO 20 I = 1 , NPFRF
        IF(KNCHW.EQ.2) THEN
           IF(ITRQUA(I).EQ.2) ITRQUA(I) = 1
           IF(ITRQUA(I).EQ.1) KNCHS = KNCHS + 1
        ELSE
           IF(ITRQUA(I).EQ.2) ITRQUA(I) = 0
        ENDIF
 20   CONTINUE
C --
C  compute the thrust axis and value
C  ---------------------------------
      CALL VZERO(AXTR,3)
C --
      CALL TRUSLU(ITRQUA,KNCHS,THRUST,AXTR)
C  To avoid the STOP in truslu
      IF (THRUST.LT.-1.9) RETURN
C --
C   define the 2 "jets"
C  --------------------
      MC1 = 0
      MC2 = 0
      DO 200 I = 1 , NPFRF
        IJET(I) = 0
        IF(ITRQUA(I).NE.1)     GO TO 200
        CALL TRPFRF(I,PP1,IRF)
        IF( IRF .NE. 0 ) GOTO 200
C199    CONTINUE
        CS1 = VDOTN(PP1,AXTR,3)
        IF(CS1.GT.0.) THEN
          MC1 = MC1 + 1
          IJET(I) = 1
        ELSE
          MC2 = MC2 + 1
          IJET(I) = 2
        ENDIF
 200  CONTINUE
C --
C   at least 1 track per hemisphere
C  ----------------------------------
CL15  IF(MC1.LT.1.OR.MC2.LT.1)      GO TO 990
C --
C   store the 2 "jets"
C  --------------------
      CALL VZERO(PJ1,4)
      CALL VZERO(PJ2,4)
C --
      DO 100 I = 1 , NPFRF
          IF(IJET(I).EQ.1) THEN
              CALL TRPFRF(I,PP1,IRF)
              IF( IRF .NE. 0 ) GOTO 100
              CALL VADD(PJ1,PP1,PJ1,3)
          ENDIF
          IF(IJET(I).EQ.2) THEN
              CALL TRPFRF(I,PP1,IRF)
              IF( IRF .NE. 0 ) GOTO 100
              CALL VADD(PJ2,PP1,PJ2,3)
          ENDIF
 100  CONTINUE
C --
C --
C   Gamma gamma rejection
C  ----------------------
C---  IF(ACOL.GT.CUTIS(7))          GO TO 990
C--CL15    IF(PMA.LT.CUTIS(8))          GO TO 990
      IF(PMA.LT.CUTIS(8) .AND. PMAGAM.LT.CUTIS(8) )          GO TO 990
C --
C  reject COSMICS (not applied)
C  --------------
C---  IF(KNCHS.EQ.2) THEN
C---     IF(SD0.GT.CUTIS(9)) GO TO 990
C---  ENDIF

C --
C   compute the max opening angle in each hemisphere
C  ------------------------------------------------------
      DO 250 J = 1 , 2
        CMA =  2.
        DO 240 I = 1 , NPFRF
          IF(ITRQUA(I).NE.1)     GO TO 240
          CALL TRPFRF(I,PP1,IRF)
          IF( IRF .NE. 0 ) GOTO 240
          IF(IJET(I).EQ.J) THEN
            IF(J.EQ.1)  CS1 = VDOTN(PP1,PJ1,3)
            IF(J.EQ.2)  CS1 = VDOTN(PP1,PJ2,3)
            IF(CS1.LT.CMA) CMA = CS1
          ENDIF
 240    CONTINUE
        IF(J.EQ.1) CM1 = CMA
        IF(J.EQ.2) CM2 = CMA
 250  CONTINUE
C --
      CMA1 = CM1
      CMA2 = CM2
      CUC  = CUTIS(10)
C --
C   cut on the max of the opening angle in each hemisphere
C  -------------------------------------------------------
      IF(KNCHS.GT.4) THEN
        IF(CM1.LT.CUC.OR.CM2.LT.CUC) GO TO 990
      ENDIF
C --
C  here are the "good" lepton pairs
C  --------------------------------
      IGOOD = .TRUE.
C --
 990  RETURN
      END
#endif
