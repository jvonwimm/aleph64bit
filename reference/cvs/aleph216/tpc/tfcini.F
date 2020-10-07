      SUBROUTINE TFCINI(LOUT,LCONS,IRUN,IER)
C
C-----------------------------------------------------------------------
C! Initialize TPC field corrections constants in TFCORR
C!
C!    Author:   R. Johnson   22-09-88
C!              Adapted from code by M. Schmelling in order to
C!              interface to the ALEPH database.
C!
C!    Input:    LOUT        /I      Output unit for error messages
C!              LCONS       /I      Input unit for the data base
C!              IRUN        /I      Current run number
C!    Output:   IER         /I      Error code:
C!                                     0  successful initialization
C!                                     1  data-base banks not found
C!                                     2  NSPACE too small in TFCORR
C!                                     3  NGMAX  too small in TFCORR
C!    Called by TINIRU
C!
C-----------------------------------------------------------------------
#ifndef DOC
      SAVE
C
#include "tjjpar.h"
#include "bcs.h"
#include "tfcorr.h"
C
      INTEGER ALGTDB
      LOGICAL FIRST
      DATA FIRST/.TRUE./
C
#include "bmacro.h"
C
      IRET= ALGTDB(LCONS,'T1FCT2FC',IRUN)
C
C++   Return immediately if the banks already are loaded and
C++   initialization has been done.
C
      IF (IRET.GT.0 .AND. .NOT.FIRST) THEN
        IER=0
        RETURN
      ENDIF
C
C++   Return with an error code if the banks cannot be found
C
      KT1FC=IW(NAMIND('T1FC'))
      KT2FC=IW(NAMIND('T2FC'))
      IF (KT1FC.EQ.0 .OR. KT2FC.EQ.0) THEN
        IER=1
        WRITE(LOUT,1) IRUN
    1   FORMAT(/' TFCINI:  TPC field correction banks T1FC, T2FC ',
     &          ' cannot be found for run ',I5/)
        RETURN
      ENDIF
      FIRST=.FALSE.
C
      RLOWFC= RTABL(KT1FC,1,JT1FLL)
      RHIGFC= RTABL(KT1FC,1,JT1FUL)
      DRFCOR= RTABL(KT1FC,1,JT1FSS)
      NRFCOR= ITABL(KT1FC,1,JT1FNP)
      PLOWFC= RTABL(KT1FC,2,JT1FLL)
      PHIGFC= RTABL(KT1FC,2,JT1FUL)
      DPFCOR= RTABL(KT1FC,2,JT1FSS)
      NPFCOR= ITABL(KT1FC,2,JT1FNP)
      ZLOWFC= RTABL(KT1FC,3,JT1FLL)
      ZHIGFC= RTABL(KT1FC,3,JT1FUL)
      DZFCOR= RTABL(KT1FC,3,JT1FSS)
      NZFCOR= ITABL(KT1FC,3,JT1FNP)
C
      NWORDS = NRFCOR*NPFCOR*NZFCOR
      INDCOR(1) = 1
      INDCOR(2) = INDCOR(1) + NWORDS
      INDCOR(3) = INDCOR(2) + NWORDS
      INDCOR(4) = INDCOR(3) + NWORDS
      NTOT= NWORDS*NFCVAL
      IF (NTOT.GT.NSPACE) THEN
        WRITE(LOUT,6903) NSPACE,NTOT
 6903   FORMAT(/1X,'TFCINI - buffer too small for displacement table'/
     &         1X,'         Increase NSPACE in common/TFCORR/'/
     &         1X,'         Current setting:  NSPACE = ',I10/
     &         1X,'         Needed:           NSPACE = ',I10/)
        IER=2
        RETURN
      ENDIF
C
      DO 11 I=1,NWORDS
        FSPACE(INDCOR(1)+I-1)= RTABL(KT2FC,1,JT2FR1+I-1)
        FSPACE(INDCOR(2)+I-1)= RTABL(KT2FC,1,JT2FR2+I-1)
        FSPACE(INDCOR(3)+I-1)= RTABL(KT2FC,1,JT2FP1+I-1)
        FSPACE(INDCOR(4)+I-1)= RTABL(KT2FC,1,JT2FP2+I-1)
   11 CONTINUE
C
      NLEN=NRFCOR+NPFCOR+NZFCOR
      IF (NLEN.GT.NGMAX) THEN
        WRITE(LOUT,6904) NGMAX,NLEN
 6904   FORMAT(/1X,'TFCINI - buffer for grid description too small'/
     &         1X,'         Increase NGMAX in common/TFCORR/'/
     &         1X,'         Current setting: NGMAX = ',I10/
     &         1X,'         Needed:          NGMAX = ',I10/)
        IER=3
        RETURN
      ENDIF
C
      NAFCOR(1) = NRFCOR
      NAFCOR(2) = NPFCOR
      NAFCOR(3) = NZFCOR
C
      IDOT    = 0
      DO 21 I=1,NRFCOR
        IDOT    = IDOT + 1
        AFCORR(IDOT) = RLOWFC + FLOAT(I-1)*DRFCOR
 21   CONTINUE
      DO 22 I=1,NPFCOR
        IDOT    = IDOT + 1
        AFCORR(IDOT) = PLOWFC + FLOAT(I-1)*DPFCOR
 22   CONTINUE
      DO 23 I=1,NZFCOR
        IDOT    = IDOT + 1
        AFCORR(IDOT) = ZLOWFC + FLOAT(I-1)*DZFCOR
 23   CONTINUE
C
      WRITE(LOUT,6901) IRUN
      WRITE(LOUT,6902) 'rlow,rhig,dr,nr: ',RLOWFC,RHIGFC,DRFCOR,NRFCOR
      WRITE(LOUT,6902) 'plow,phig,dp,np: ',PLOWFC,PHIGFC,DPFCOR,NPFCOR
      WRITE(LOUT,6902) 'zlow,zhig,dz,nz: ',ZLOWFC,ZHIGFC,DZFCOR,NZFCOR
      WRITE(LOUT,6905)
 6901 FORMAT(/1X,'TCOREC - TPC drift field correction constants',
     &           ' successfully read for run ',I6,'.'/
     &           ' Grid for coordinate correction:')
 6902 FORMAT(1X,'TCOREC - ',(A),3F12.4,I6)
 6905 FORMAT(1X,'---------------------------------------------------'/)
C
      IER=0
      RETURN
      END
#endif
