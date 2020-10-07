      SUBROUTINE LBUNCA(SCORR,WCORR)
C-----------------------------------------------------------------------
C!   Lcal bunch-to-bunch calibration
C!
C!   Author   : P. H. Hansen  950401
C!
C!     Input   : SCORR(4) Storey energy correction factor
C!               WCORR(4) Wire energy correction factor
C!     Output  : SCORR(4) Modified factor
C!               WCORR(4) Modified factor
C!
C!     Description :
C!     ===========
C?     Changes the correction factor for each Lcal module
C?     depending on wagon number and wagon separation.
C?
C!======================================================================
#ifndef DOC
      DIMENSION SCORR(4),WCORR(4),BSCOR(4,4),BWCOR(4,4)
C
C BSCOR(I,J) corrects module I wagon J storey data
      DATA BSCOR /16*1./
C
C BWCOR(I,J) corrects module I wagon J wire data
      DATA BWCOR /16*1./
C
C-----------------------------------------------------------------------
      CALL RQBUNC(IBUN,INBU,NWAG,IQUA)
      IF(IBUN.LE.0.OR.IBUN.GE.5.OR.INBU.LE.0)     GOTO 999
      DO 10 I=1,4
        SCORR(I)=SCORR(I)*BSCOR(I,IBUN)
        WCORR(I)=WCORR(I)*BWCOR(I,IBUN)
   10 CONTINUE
  999 CONTINUE
      END
#endif
