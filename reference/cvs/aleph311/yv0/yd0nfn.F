      SUBROUTINE YD0NFN(VPRR,RADD,D00,FI00,CFF,XDNN,SIGND)
C-----------------------------------------------
C!Minimize the dist. in x-y  between track and vprr point
C
C   AUTHOR: M.A.CIOCCI 2/4/90
CKEY YV0 MIN DISTANCE /USER
C
C     INPUT
C            VPR/R  POINT ASSUMED AS BEAM BEAM CROSSING
C            RAD/R   RADIUS CURVATURE
C            D0/R    -CHARGE*D0 (BANK-FRFT)
C            FI0/R   FI0 (BANK FRFT)
C            CF/R    CHARGE
C            SIGND /R  SIGN OF NEW D0
C     OUTPUT
C            XDN(3)/R  COORDINATES OF POINT
C
C     CALLED:
C             UPSIVE
C     CALLS :
C             NONE
C
C             DESCRIPTION
C             ===========
C
C Finds the point coordinates minimizing the distance
C between helix and vpr point
C
C--------------------------------------------------
#ifndef DOC
      SAVE
        DOUBLE PRECISION VPR(3),XDN(3),RAD,D0,FI0,CF,SIGND0
        DOUBLE PRECISION XCEN,YCEN,COST0,COST1,COST2,XX1,YY1
        DOUBLE PRECISION XX2,YY2,DD,DIST1,DIST2
        REAL VPRR(*),XDNN(*),RADD,D00,FI00,CFF,SIGND
         DO 1 I=1,3
         VPR(I)=VPRR(I)
  1      CONTINUE
         RAD=RADD
         D0=D00
         FI0=FI00
         CF=CFF

       XCEN=CF*(RAD-D0)*DSIN(FI0)
       YCEN=-CF*(RAD-D0)*DCOS(FI0)
       COST0=(VPR(1)-XCEN)/(VPR(2)-YCEN)
       COST1=COST0**2+1.D0
       COST2=DSQRT(RAD**2/COST1)
       YY1=YCEN+COST2
       YY2=YCEN-COST2
       XX1=(YY1-YCEN)*COST0+XCEN
       XX2=(YY2-YCEN)*COST0+XCEN
       DIST1=DSQRT((VPR(1)-XX1)**2+(VPR(2)-YY1)**2)
       DIST2=DSQRT((VPR(1)-XX2)**2+(VPR(2)-YY2)**2)
          DD=DSQRT((XCEN-VPR(1))**2+(YCEN-VPR(2))**2)
          IF(DD.LT.RAD)SIGND0=1.
          IF(DD.GE.RAD)SIGND0=-1.
         IF(DIST1.LE.DIST2)THEN
       XDN(1)=XX1
       XDN(2)=YY1
         ELSE
       XDN(1)=XX2
       XDN(2)=YY2
         ENDIF
         DO 2 I=1,3
         XDNN(I)=XDN(I)
   2     CONTINUE
C        WRITE(6,*)SQRT(XDNN(1)**2+XDNN(2)**2)
         SIGND=SIGND0
         RETURN
           END
#endif
