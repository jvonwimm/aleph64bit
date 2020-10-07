      SUBROUTINE YVSPVE(EVTX,DJET,EX,EZ,ET)
CKEY  QVSRCH / INTERNAL
C ----------------------------------------------------------------------
C! Finds projected primary vertex errors in rotated system
C ----------------------------------------------------------------------
      DIMENSION EVTX(3),DJET(3)
      DIMENSION CV(3,3)
C ----------------------------------------------------------------------
C
C CLEAR COVARIANCE
      CALL UZERO(CV,1,9)
C LOAD DIAGONAL ELEMENTS
      CV(1,1)=EVTX(1)**2
      CV(2,2)=EVTX(2)**2
      CV(3,3)=EVTX(3)**2
C ROTATE INTO JET SYSTEM
      CALL YVSRMJ(DJET,CV,CV)
C UNLOAD X AND Z VERTEX ERRORS
      EX=SQRT(ABS(CV(1,1)))
      EZ=SQRT(ABS(CV(2,2)))
      ET=SQRT(ABS(CV(3,3)))
      RETURN
      END
