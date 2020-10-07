      SUBROUTINE FKKPAR(NPTIT,NPTTP,LISTI,LISTT,PROCH,PROPA,IFAIL)
C-----************************************************************-----
C!    Compare the helix parameters of the two halfs of an ITC-TPC track
C!
C!    AUTHOR        :- GEORG STIMPFL 88/8/08
C!
C!    =  =  =  =  =  =  =  =  =  =  =  =  =  =  =  =  =  =  =  =  =  =
C!
C!    INPUT  : NPTIT = number of ITC coordinates
C!             NPTTP = number of TPC coordinates
C!             LISTI = list of ITC coordinates
C!             LISTT = list of TPC coordinates
C!
C!    OUTPUT : PROCH = probability to have no kink using chi**2 test
C!             PROPA = probability to have no kink using parameter test
C!             IFAIL = error flag (=0 if everything was okay)
C!
C?
C!======================================================================
#ifndef DOC
#include "bcs.h"
C***********************************************************************
      PARAMETER (NPAR=6, NPARC=5, NMAT=21, NMATC=15)
      INTEGER     LISTI(*),LISTT(*)
      REAL DVV(NPARC),PROD(NPARC)
      REAL EEM1(NMAT),COV1(NMAT),VVM1(NPAR)
      REAL EEM2(NMAT),COV2(NMAT),VVM2(NPAR)
      PARAMETER (RMS=29.6, XMS=0.027)
C--
      IFAIL = 0
      IF (NPTTP+NPTIT.LT.4) THEN
        IFAIL = 11
        RETURN
      ENDIF
C--
C--   Get chisq
      IOPT1 = 2
      IF (NPTIT.GT.4 .AND. NPTTP.GT.4) IOPT1 = 4
      CALL UFITMS(NPTTP,NPTIT,0,LISTT,LISTI,LIST3,IOPT1,0,RMS,XMS,
     1            VVM1,EEM1,COV1,CHISQ,IERR)
      IF (IERR.GT.0) THEN
        IFAIL = 12
        RETURN
      ENDIF
      PROCH = PROB(CHISQ,2*(NPTTP+NPTIT)-5)
      NPTOT = NPTTP + NPTIT
      IF (NPTOT.LT.10) THEN
        IFAIL = 2
        RETURN
      ENDIF
C--
C--   Cut track into two parts
      NP1 = (NPTOT+1)/2
      IF (NPTIT.LE.NP1) THEN
        NPI1 = NPTIT
      ELSE
        NPI1 = NP1
      ENDIF
      NPT1 = NP1 - NPI1
      IF (NPT1.LT.5 .AND. NPTTP.GT.10) NPT1 = 5
C--
C--   Fit first part
      IOPT1 = 4
      IF (NPT1.LT.5 .OR. NPI1.LT.5) IOPT1 = 2
      CALL UFITMS(NPT1,NPI1,0,LISTT,LISTI,LIST3,IOPT1,0,RMS,XMS,
     1            VVM1,EEM1,COV1,CHI1,IERR)
      IF (IERR.GT.0) THEN
        IFAIL = 11
        RETURN
      ENDIF
      CALL UNEWDP(VVM1,D0P,PHI0P)
      VVM1(3) = PHI0P
      VVM1(4) = D0P
C--
C--   Fit second part
      NPT2 = NPTTP - NPT1
      NPI2 = NPTIT - NPI1
      IOPT2 = 4
      IF (NPT2.LT.5 .OR. NPI2.LT.5) IOPT2 = 2
      CALL UFITMS(NPT2,NPI2,0,LISTT(NPT1+1),LISTI(NPI1+1),LIST3,2,IOPT2,
     1            RMS,XMS,VVM2,EEM2,COV2,CHI2,IERR)
      IF (IERR.GT.0) THEN
        IFAIL = 11
        RETURN
      ENDIF
C--
C--   Compare the two parts
      DO 110 IPAR = 1,NPARC
  110 DVV(IPAR) = VVM1(IPAR)-VVM2(IPAR)
      DO 120 I = 1,NMATC
  120 EEM1(I) = COV1(I)+COV2(I)
      CALL SMINV(EEM1,DUMMY,NPARC,0,NRANK)
      CALL SMAV(PROD,EEM1,DVV,NPARC)
      CHIP = 0.
      DO 130 IPAR = 1,NPARC
  130 CHIP = CHIP+DVV(IPAR)*PROD(IPAR)
      PROPA = PROB(CHIP,NPARC)
      END
#endif
