      SUBROUTINE QCABSA(RL,PMOM,ISIDE,IDEL,RLN)
CKEY  FILL / INTERNAL
C ----------------------------------------------------------------------
C! Recalculates RL for electron - Auxiliary to FIXRTRL
C  Called from FIXRTRL
C                                 Author: M.N Minard 10/11/93
C
C     Input : RL   Estimator stored in preceeding processing
C             PMOM Track momentum
C             ISIDE = 1 Barrel , 2 Endcap , 3 Overlap
C             IDEL  Correction level (1 Monte_carlo 92)
C     Output : New estimator
C ----------------------------------------------------------------------
      PARAMETER (NVER=2)
      DIMENSION PARMEC(3,NVER),PARMBR(3,NVER)
      DIMENSION PARSEC(3,NVER),PARSBR(3,NVER)
      DIMENSION PECRED(NVER),PBRRED(NVER)
      DATA PARMEC/0.17657,0.020881,0.22670E-2
     1           ,0.17370,0.010945,-0.13903E-2/
      DATA PARSEC/0.53330E-1,0.20732E-1,0.26332E-2
     1           ,13.905,18.305,0./
      DATA PARMBR/0.18826,0.030015,0.31633E-2
     1           ,0.18901,0.025051,0.11433E-2/
      DATA PARSBR/0.61381E-1,0.27413E-1,0.40436E-2
     1           ,13.207,17.319,0./
      DATA PECRED /0.865,0.9018/
      DATA PBRRED /0.888,0.923/
C
      DIMENSION BMEAN(3),BSIG(3)
C ----------------------------------------------------------------------
      ILAS  = NVER
      IF ( ISIDE.GT.2) THEN
        RLN = RL
        GO TO  999
      ENDIF
C Endcap :
      IF (ISIDE.EQ.2) THEN
        DO I=1,3
          BMEAN(I)=PARMEC(I,IDEL)
          BSIG(I) =PARSEC(I,IDEL)
          RSFACT  =PECRED(IDEL)
        ENDDO
C Barrel :
      ELSEIF (ISIDE.EQ.1) THEN
        DO I=1,3
          BMEAN(I)=PARMBR(I,IDEL)
          BSIG(I) =PARSBR(I,IDEL)
          RSFACT = PBRRED(IDEL)
        ENDDO
      ENDIF
      SIGBSA0=BSIG(1)+BSIG(2)*ALOG(1./PMOM)+BSIG(3)*(ALOG(PMOM))**2
      BSA0=BMEAN(1)+BMEAN(2)*ALOG(1./PMOM)+BMEAN(3)*(ALOG(PMOM))**2
      SIGBSA0=SIGBSA0*RSFACT
      BSA=RL*SIGBSA0+BSA0
C
C-    CALCULATE NEW  RL
C
C Endcap :
      IF (ISIDE.EQ.2) THEN
        DO I=1,3
          BMEAN(I)=PARMEC(I,ILAS)
          BSIG(I) =PARSEC(I,ILAS)
          RSFACT  =PECRED(ILAS)
        ENDDO
C Barrel :
      ELSEIF (ISIDE.EQ.1) THEN
        DO I=1,3
          BMEAN(I)=PARMBR(I,ILAS)
          BSIG(I) =PARSBR(I,ILAS)
          RSFACT = PBRRED(ILAS)
        ENDDO
      ENDIF
      SIGBSA0=1./(BSIG(1)+BSIG(2)*ALOG(PMOM))
      BSA0=BMEAN(1)+BMEAN(2)*ALOG(1./PMOM)+BMEAN(3)*(ALOG(PMOM))**2
      SIGBSA0=SIGBSA0*RSFACT
      RLN = (BSA-BSA0)/SIGBSA0
*
 999  RETURN
      END
