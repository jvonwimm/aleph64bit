      SUBROUTINE QVSAVX(PVTX,DJET,SVTX,ESVTX,AVTX,CAVTX)
CKEY  QVSRCH / INTERNAL
C ------------------------------------------------------------------------------
C! Converts secondary vertex and errors from internal coordinates
C! to ALEPH coordinates
C  Called from QVSRCH
C  Author : T. MATTISON  U.A.BARCELONA/SLAC  1 DECEMBER 1992
C
C ------------------------------------------------------------------------------
#ifndef DOC
      DIMENSION PVTX(3),DJET(3),SVTX(3),ESVTX(3)
      DIMENSION AVTX(3),CAVTX(3,3)
C ------------------------------------------------------------------------------
C TRANSFORM VERTEX TO ALEPH COORDINATES
      CALL YVSTRI(PVTX,DJET,SVTX,AVTX)
C
C ZERO COVARIANCE MATRIX
      CALL UZERO(CAVTX,1,9)
C
C LOAD DIAGONAL ERRORS
      CAVTX(1,1)=ESVTX(1)**2
      CAVTX(2,2)=ESVTX(2)**2
      CAVTX(3,3)=ESVTX(3)**2
C
C ROTATE COVARIANCE MATRIX FROM JET COORDINATES TO ALEPH COORDINATES
      CALL YVSRMI(DJET,CAVTX,CAVTX)
C
      RETURN
      END
#endif
