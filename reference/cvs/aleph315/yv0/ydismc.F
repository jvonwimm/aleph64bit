       SUBROUTINE YDISMC(XK1,D01,FI01,Z01,XLAM1,C1,
     $                   XK2,D02,FI02,Z02,XLAM2,C2,
     $                   XM1,YM1,XM2,YM2,XM3,YM3,XM4,YM4,IND)
C---------------------------------------------------------------
C! Finds the points at minimum distance on the two tracks
CKEY YV0 DISTANCE TRACK
C
C      AUTHOR     : M.A.CIOCCI, L.ROLANDI 25/2/87
C      MODIFIED   : M.A. CIOCCI 2/2/90
C
C      Modified : P. Rensing 14/2/95
C            Fix calculation of symmetry point when one center
C            lies inside the other circle. This happens for
C            like-signed tracks.
C
C       CONVENCTION ON RADIUS OF CURVATURE : IS NOT SIGNED
C       CONVENCTION ON D0:  D0=-CHARGE*(D0-BANK-FRTF)
C
C      INPUT:
C
C---------------------------------------------------------------
C            XK1/R     -CHARGE/(RADIUS OF CURVATURE-D0)        F
C                                                              O
C                      WHERE D0 IS DEFINED AS THE              R
C                      CLOSEST DISTANCE OF APPROACH
C                      TO LINE X=Y=0 IN THE X-Y PLANE          T
C                      SIGNED + IF POINT X=Y=0 IS INSIDE       H
C                      TRACK CIRCLE, SIGNED - IF POINT         E
C                      X=Y=0   IS OUTSIDE TRACK CIRCLE.
C                                                              F
C                                                              I
C             D01/R    D0                                      R
C             FI01/R   PHI AT CLOSEST POINT OF APPROACH        S
C                      TO LINE X=Y=0                           T
C             Z01/R    ZETA COORDINATE AT D0
C                                                              T
C             XLAM1/R   TANGENT OF DIP ANGLE                   R
C                                                              A
C             C1/R     CHARGE                                  C
C                                                              K
C
C---------------------------------------------------------------
C            XK2/R     -CHARGE/(RADIUS OF CURVATURE-D0)        F
C                                                              O
C                      WHERE D0 IS DEFINED AS THE              R
C                      CLOSEST DISTANCE OF APPROACH
C                      TO LINE X=Y=0 IN THE X-Y PLANE          T
C                      SIGNED + IF POINT X=Y=0 IS INSIDE       H
C                      TRACK CIRCLE, SIGNED - IF POINT         E
C                      X=Y=0   IS OUTSIDE TRACK CIRCLE.
C                                                              S
C             D02/R    D0                                      E
C                                                              C
C             FI02/R   PHI AT CLOSEST POINT OF APPROACH        O
C                      TO LINE X=Y=0                           N
C                                                              D
C             Z02/R    ZETA COORDINATE AT D0
C                                                              T
C                                                              R
C             XLAM2/R  TANGENT OF DIP ANGLE                    A
C                                                              C
C             C2/R     CHARGE                                  K
C
C---------------------------------------------------------------
C
C
C      OUTPUT:
C
C               XM1/REAL       L
C                              O
C               YM1/REAL       O
C                              K
C               XM2/REAL
C                              D
C               YM2/REAL       E
C               XM3/REAL       S
C                              C
C               YM3/REAL       R
C                              I
C               XM4/REAL       P
C                              T
C               YM4/REAL       I
C                              O
C                              N
C
C               IND/REAL    IND=1 THE TWO CIRCLES DO INTERSECT
C                           AT TWO POINT
C                           IND=0 THE TWO CIRCLES DO INTERSECT
C                           AT ONE POINT
C                           IND LESS THAN 0 THE TWO CIRCLES
C                           DO NOT INTERSECT
C
C      CALLED:  YFPSIN
C
C                    DESCRIPTION
C                   ============
C        Following the method of UA1-TN 84/31 by V.Karimaki
C        searchs the points at minimum distance on the two
C        circles in the X-Y plane.
C        If the two circles do intersect, IND=1 and (XM1,YM1)
C        (XM2,YM2) are the coordinates of the two intersection
C        points, on the X-Y plane,
C        but in this case the real solution can be also
C        (XM3,XM4), (XM5,XM6) intersection between a straigth line
C        and the two circles joining the two centers
C        If IND=0 the two circle are intersecting at one point
C       that is stored in both the adresses (XM1,YM1),(XM2,YM2)
C        If IND=-1 the two circles do not intersect
C        and are one outside the other  and (XM1,YM1)
C        is the coordinate of the first track and (XM2,YM2) is
C        the coordinate of the second track at point of minimum
C        distance on the X-Y plane.
C        If IND=-2 the two circles do not intersect
C        and are one inside the other  and (XM1,YM1)
C        is the coordinate of the first track and (XM2,YM2) is
C        the coordinate of the second track at point of minimum
C        distance on the X-Y plane.
C        If Ind=6 the two circles  are one inside the other
C        with the same center (the number of solution will be
C        infinite)
C        See also ALEPH NOTE .....
C
C------------------------------------------------------------
#ifndef DOC
      IMPLICIT NONE
C
        REAL XK1,D01,FI01,Z01,XLAM1,C1
        REAL XK2,D02,FI02,Z02,XLAM2,C2
        REAL XM1,YM1,XM2,YM2,XM3,YM3,XM4,YM4
        INTEGER IND
C
        REAL XC1, XC2, YC1, YC2
        REAL A, A2, C0, S0, RO1, RO2, XSI, YSI, T
        REAL S1, S2, HSQ, H, CS1, CS2
C
        XM3=9999.
        YM3=9999.
        XM4=9999.
        YM4=9999.
        RO1=-C1/XK1+D01
        RO2=-C2/XK2+D02
C
C++  COMPUTE THE COORDINATES OF THE SYMMETRY POINT
C
        XC1=-SIN(FI01)/XK1
        YC1= COS(FI01)/XK1
        XC2=-SIN(FI02)/XK2
        YC2= COS(FI02)/XK2
        A2 =1./XK1/XK1+1./XK2/XK2-2.*COS(FI01-FI02)/XK1/XK2
            IF(A2.LT.0.000001)THEN
        IND=6
        GO TO 600
            ENDIF
        A  =SQRT(A2)
        C0=(YC1-YC2)/A
        S0=(XC2-XC1)/A
        IF(A.GT.RO1+RO2) GO TO 300
        IF(A.LE.ABS(RO1-RO2)) GO TO 400
C
C+     HERE THE CIRCLES DO INTERSECT
C
        XSI=0.5*(XC1+XC2-(RO1-RO2)*(RO1+RO2)*(XC1-XC2)/A2)
        YSI=0.5*(YC1+YC2-(RO1-RO2)*(RO1+RO2)*(YC1-YC2)/A2)
C
        T=RO1+RO2-A
        S1=T*(RO2-T/2.)/A
        S2=T*(RO1-T/2.)/A
C
C+  T IS THE DISTANCE BETWEEN THE TWO CIRCLES AT THE SYMMETRY
C+  POINT
           IF(ABS(T).LT.0.001)THEN
        IND=0
        XM1=XSI
        YM1=YSI
        XM2=XSI
        YM2=YSI
        GO TO 600
           ELSE
        IND=1
        HSQ=0.5*(S1*(2.*RO1-S1)+S2*(2.*RO2-S2))
        IF(HSQ.LE..0) THEN
           H=0.
        ELSE
           H=SQRT(HSQ)
        ENDIF
        XM1=XSI+H*C0
        YM1=YSI+H*S0
        XM2=XSI-H*C0
        YM2=YSI-H*S0
        IF (A .LT. MAX(RO1,RO2)) THEN
C One center is inside the other
           IF (RO1 .GT. RO2) THEN
              CS1 = 1.0
              CS2 = -1.0
           ELSE
              CS1 = -1.0
              CS2 = 1.0
           END IF
        ELSE
           CS1=1.0
           CS2=1.0
        END IF
        XM3=XC1+CS1*RO1*S0
        XM4=XC2-CS2*RO2*S0
        YM3=YC1-CS1*RO1*C0
        YM4=YC2+CS2*RO2*C0
        GO TO 600
           ENDIF
 300    CONTINUE
C
C+ HERE THE CIRCLES ARE ONE OUTSIDE THE OTHER
C
        IND=-1
        CS1=1.
        CS2=1.
        GO TO 550
 400    CONTINUE
C
C+ HERE THE CIRCLES ARE ONE INSIDE THE OTHER
C
        IND=-2
        IF(RO1.GT.RO2) THEN
        CS1=1.
        CS2=-1.
        ELSE
        CS1=-1.
        CS2=1.
        ENDIF
C
C+  HERE THE CIRCLES DO NOT INTERSECT
C
 550    XM1=XC1+CS1*RO1*S0
        XM2=XC2-CS2*RO2*S0
        YM1=YC1-CS1*RO1*C0
        YM2=YC2+CS2*RO2*C0
 600    CONTINUE
        RETURN
        END
#endif
