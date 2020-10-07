      SUBROUTINE YD0NFI(VPR,RAD,D0,FI0,CF,XDN)
C-----------------------------------------------
C!Minimize the dist. in x-y  between track and vpr point
C
C   AUTHOR: M.A.CIOCCI 20/1/90
CKEY YV0 MIN DISTANCE / USER
C
C     INPUT
C            VPR/R  POINT ASSUMED AS BEAM BEAM CROSSING
C            RAD/R   RADIUS CURVATURE
C            D0/R    -CHARGE*D0 (BANK-FRFT)
C            FI0/R   FI0 (BANK FRFT)
C            CF/R    CHARGE
C     OUTPUT
C            XDN(2)/R  COORDINATES OF POINT
C
C     CALLED:
C             YPSIVE
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
       REAL VPR(3),XDN(2),RAD,D0,FI0,CF
       XCEN=CF*(RAD-D0)*SIN(FI0)
       YCEN=-CF*(RAD-D0)*COS(FI0)
       COST0=(VPR(1)-XCEN)/(VPR(2)-YCEN)
       COST1=COST0**2+1.
       COST2=SQRT(RAD**2/COST1)
       YY1=YCEN+COST2
       YY2=YCEN-COST2
       XX1=(YY1-YCEN)*COST0+XCEN
       XX2=(YY2-YCEN)*COST0+XCEN
       DIST1=(VPR(1)-XX1)**2+(VPR(2)-YY1)**2
       DIST2=(VPR(1)-XX2)**2+(VPR(2)-YY2)**2
         IF(DIST1.LT.DIST2)THEN
       XDN(1)=XX1
       XDN(2)=YY1
         ELSE
       XDN(1)=XX2
       XDN(2)=YY2
         ENDIF
         RETURN
           END
#endif
