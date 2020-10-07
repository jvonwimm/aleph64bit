        SUBROUTINE GETX(FLAM,ALPHA,RSQ,A,X1)
CKEY PHYSICS MCARLO /INTERNAL
C. *************************************************************************
C. *  Author : Jane Ratcliffe   6/6/88                                     *
C. *  Called by SUBROUTINE PIDECY                                          *
C! *  Using the Technique of Importance Sampling to return an X value (X1) *
C. *************************************************************************
#ifndef DOC
      SAVE
C
        DATA CON1,CON2,CON3/.865,5.108,4.416/
        DATA IFIRST/0/
C
        IF(IFIRST.EQ.0)THEN
        GAMMA=0.
        X=RSQ
 100    CONTINUE
        X=X+RSQ
        IF(X.GT.1.0) GOTO 200
        IF(X.GE.RSQ.AND.X.LE.0.1)THEN
          HX=1./(FLAM*X)
        ELSE
          HX=(10./FLAM)*EXP(-ALPHA*(X-0.1))
        ENDIF
        BETA=SQRT(1.-RSQ/X)
        G1=(1.+A*X)**2
        G2=(2.*(1.-X)**3)/(3.*X)
        G3=BETA*(1.+RSQ/(2.*X))
        GX=G1*G2*G3
        GXDHX=GX/HX
        IF(GXDHX.GT.GAMMA)GAMMA=GXDHX
        GOTO 100
 200    CONTINUE
        IFIRST=1
        ENDIF
C------------------------------------------------
 10       R1=RNDM(0)
          IF(R1.LT.CON1)THEN
            X1=EXP(CON2*R1*FLAM+ALOG(RSQ))
          ELSE IF(R1.GE.CON1)THEN
            X1A=(FLAM*ALPHA/10.)*(CON2*R1-CON3)
            X1B=(1./ALPHA)*ALOG(1.-X1A)
            X1=0.1-X1B
          ENDIF
C
        IF(X1.GE.RSQ.AND.X1.LE.0.1)THEN
          HX1=1./(FLAM*X1)
        ELSE IF(X1.GT.0.1.AND.X1.LE.1.0)THEN
          HX1=(10./FLAM)*EXP(-ALPHA*(X1-0.1))
        ELSE
          GOTO 10
        ENDIF
        BETA=SQRT(1.-RSQ/X1)
        G1=(1.+A*X1)**2
        G2=(2.*(1.-X1)**3)/(3.*X1)
        G3=BETA*(1.+RSQ/(2.*X1))
        GX1=G1*G2*G3
        DELTA=GX1/HX1
        REJ=DELTA/GAMMA
        R2=RNDM(0)
        IF(R2.GT.REJ)THEN
          GOTO 10
        ENDIF
C
        RETURN
        END
#endif
