      SUBROUTINE ULTHRU(MXWORK,N,PP,IFLAG,THR,THRAX,OBL)
C-----------------------------------------------------------------------
CKEY EDIR THRUST JETSET
C! JETSET routine
C-
C   Could be improved!!!
C   modified for I/O via arguments
C-
C   Input  : MXWORK number of working dimensions (usually=42)
C            N  # of particles
C            P(j,i)= px,py,pz,E of ith particle (i=1,N)
C            IFLAG(i) =0 if particle i enters calculus
C   Output : THR   = thrust
C            THRAX =   "    axes
C            OBL   = oblateness
C-
C   Called by   : TRUSLU
C   Calls  : VUROBO
C   Input banks : None
C-
C                                                      Author: Jetset!
C-----------------------------------------------------------------------
#ifndef DOC
      SAVE
C --
         EXTERNAL ULANGL
         DIMENSION PP(4,N+MXWORK) , IFLAG(N+MXWORK)
         DIMENSION TDI(3),TPR(3)
         DIMENSION THRAX(3)
         DATA CONVRG /0.0001/
         CALL VZERO(IFLAG(N+1),MXWORK)
         NP=0
         PS=0.
         DO 280 LD=1,2
            IF(LD.EQ.2) THEN
C --
C   Thrust axis along z direction for major axis search
C --
         NWORK = N + 1
         PHI=ULANGL(PP(1,N+1),PP(2,N+1))
         CALL VUROBO(NWORK,PP,IFLAG,0.,-PHI,0.,0.,0.)
         THE=ULANGL(PP(3,N+1),PP(1,N+1))
         CALL VUROBO(NWORK,PP,IFLAG,-THE,0.,0.,0.,0.)
            ENDIF
C --
C   Find and order particles with highest P (Pt fOR MAJOR)
C --
      DO 100 LF=N+4,N+MXWORK/10+4
  100 PP(4,LF)=0.
         DO 140 I=1,N
         IF(IFLAG(I).NE.0) GOTO 140
            IF(LD.EQ.1) THEN
            NP=NP+1
            PA=SQRT(PP(1,I)**2+PP(2,I)**2+PP(3,I)**2)
            PS = PS + PA
            ELSE
             PA=SQRT(PP(1,I)**2+PP(2,I)**2)
            ENDIF
        DO 110 LF=N+MXWORK/10+3,N+4,-1
        IF(PA.LE.PP(4,LF)) GOTO 120
        DO 110 J=1,4
  110   PP(J,LF+1)=PP(J,LF)
        LF=N+3
  120 DO 130 J=1,3
  130 PP(J,LF+1)=PP(J,I)
          PP(4,LF+1)=PA
  140     CONTINUE
      IF(NP.LE.1) THEN
C --
C   Very low multiplicities (0 or 1) not considered
C --
          THR=-1.
          OBL=-1.
          RETURN
       ENDIF
C --
C   Find and order initial axes with highest thrust
C --
      DO 150 LG=N+MXWORK/10+5,N+MXWORK/10+15
  150 PP(4,LG)=0.
      NC=2**(MIN(MXWORK/10,NP)-1)
      DO 210 LC=1,NC
      DO 160 J=1,3
  160 TDI(J)=0.
      DO 170 LF=1,MIN(MXWORK/10,NP)
      SGN = 1.
      IF(2**LF*((LC+2**(LF-1)-1)/2**LF).GE.LC) SGN=-SGN
      DO 170 J=1,4-LD
  170 TDI(J)=TDI(J)+SGN*PP(J,N+LF+3)
      TDS=TDI(1)**2+TDI(2)**2+TDI(3)**2
      DO 180 LG=N+MXWORK/10+MIN(LC,10)+4,N+MXWORK/10+5,-1
      IF(TDS.LE.PP(4,LG)) GOTO 190
      DO 180 J=1,4
  180 PP(J,LG+1)=PP(J,LG)
      LG=N+MXWORK/10+4
  190 DO 200 J=1,3
  200 PP(J,LG+1)=TDI(J)
      PP(4,LG+1)=TDS
  210 CONTINUE
C --
C  Iterate direction of axis until stable maximum
C --
      PP(4,N+LD)=0.
      LG=0
  220 LG=LG+1
      THP=0.
  230 THPS=THP
      DO 240 J=1,3
        IF(THP.LE.1E-10) TDI(J)=PP(J,N+MXWORK/10+4+LG)
        IF(THP.GT.1E-10) TDI(J)=TPR(J)
        TPR(J) = 0.
  240 CONTINUE
       DO 260 I=1,N
       IF(IFLAG(I).NE.0) GOTO 260
       SGN=SIGN(1.,TDI(1)*PP(1,I)+TDI(2)*PP(2,I)+TDI(3)*PP(3,I))
       DO 250 J=1,4-LD
  250 TPR(J)=TPR(J)+SGN*PP(J,I)
  260 CONTINUE
       THP=SQRT(TPR(1)**2+TPR(2)**2+TPR(3)**2)/PS
       IF(THP.GE.THPS+CONVRG) GOTO 230
C --
C   Save good axis, try new initial axis until a number of TRIES AGREE
C --
       IF(THP.LT.PP(4,N+LD)-CONVRG.AND.LG.LT.MIN(10,NC))
     $  GOTO 220
        IF(THP.GT.PP(4,N+LD)+CONVRG) THEN
        LAGR=0
        SGN=-(-1.)**INT(RNDM(0)+0.5)
        DO 270 J=1,3
  270   PP(J,N+LD)=SGN*TPR(J)/(PS*THP)
        PP(4,N+LD)=THP
        ENDIF
        LAGR=LAGR+1
  280 IF(LAGR.LT.MOD(MXWORK,10).AND.LG.LT.MIN(10,NC)) GOTO 220
C --
C   Find minor axis and value by orthogonality
C --
       SGN=-(-1.)**INT(RNDM(0)+0.5)
       PP(1,N+3)=-SGN*PP(2,N+2)
       PP(2,N+3)=SGN*PP(1,N+2)
       PP(3,N+3)=0.
       THP=0.
       DO 290 I=1,N
       IF(IFLAG(I).NE.0) GOTO 290
       THP=THP + ABS(PP(1,N+3)*PP(1,I)+PP(2,N+3)*PP(2,I))
  290 CONTINUE
       PP(4,N+3)=THP/PS
C --
C   Reset unused components, rotate back to original coordinate system
C --
       CALL VUROBO(N+3,PP,IFLAG,THE,PHI,0.,0.,0.)
        THR=PP(4,N+1)
        THRAX(1) = PP(1,N+1)
        THRAX(2) = PP(2,N+1)
        THRAX(3) = PP(3,N+1)
        OBL=PP(4,N+2)-PP(4,N+3)
        RETURN
        END
#endif
