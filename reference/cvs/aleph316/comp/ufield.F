      SUBROUTINE UFIELD(POS,IRGN,B)
C-------------------------------------------------------------------
C!Compute magnetic field at position POS =(x,y,z) in region IRGN
C!The field components are returned in B(3)
C
C -  Author : D. DECAMP ,  may 87
C       modified may 1988 to introduce better parametrisation
C
C -  Structure : SUBROUTINE
C                User Entry Name : UFIELD
C                External References: none
C                Comdecks referenced : none
C
C -  Usage   : CALL UFIELD(POS,IRGN,B)
C
C -  Input   : POS(1-3)=x , y , z of spatial position
C              IRGN   = region # ( in the range 1 to 11) as defined
C                       by Jack Steinberger ( see aleph 87-65 )
C -  Output  : B(1-3) = Bx,By,Bz in KGauss at this position
C
C         ------------------------------------------------------
#ifndef DOC
      SAVE
      PARAMETER(IDR=4,IDZ=5,NRGN=6)
      DIMENSION POS(3),B(3)
      DIMENSION AR(IDR+1,NRGN),AZ(IDZ+1,NRGN),XEN(NRGN)
      DATA AR/   815.08,-1151.479,577.61,-118.122,7.82026,
     1        -416.304,629.662,-351.403,85.537,-7.6246,
     2        -508.9548,748.1448,-408.0278,97.78187,-8.67898,
     3        -20.6924,105.234,-159.841,99.961,-22.0848,
     4       4.1492,-13.33,22.826,-8.5565,.91609,
     5       .59013,-1.2344,10.899,-5.1884,.63847/
      DATA AZ/ 2929.631,-5064.1845,3265.6056,-927.66317,97.7948,0.,
     1   -8.7135,-6.21285,11.373,-8.337,2.55726,-.2668,
     2    -6.4635,.305387,-.3155,-.0867391,.172192,-.04,
     3    77.038,-268.4640,450.8673,-364.112,141.505,-21.18347,
     4    25.5492,-54.82541,71.876,-40.642,9.72089,-.8298918,
     5    6.8294,-16.7394,23.2646,-14.00396,3.604451,-.332438/
      DATA XEN/315.,300.,384.2,315.,362.,420.8/
      B(1)=0.
      B(2)=0.
      B(3)=0.
C
C  Protect Ufield against STUPID region number
C
      IF (IRGN.LT.1 .OR. IRGN.GT.11) GO TO 999
C
C In regions 2 and 9 field is zero.  Return also if region = 0
C
      IF (IRGN.EQ.2 .OR. IRGN.EQ.9) GO TO 999
C
C Region of uniform solenoidal field (TPC + ECAL).
C
      IF(IRGN.EQ.1.OR.IRGN.EQ.10.OR.IRGN.EQ.11) THEN
         B(3)=15.
         GO TO 999
      ENDIF
C
C Barrel regions: Parametrisation in terms of Z-component.
C Endcap regions: Parametrisation in terms of R
C
      R = SQRT(POS(1)*POS(1) + POS(2)*POS(2))
      ABSZ = ABS(POS(3))
C
C Get R or Z at the entrance of the region
C
      IF(IRGN.EQ.4.OR.IRGN.EQ.5) THEN
         X=(ABSZ/100.)*(XEN(IRGN-2)/R)
      ELSE
         X=(R/100.)*(XEN(IRGN-2)/ABSZ)
      ENDIF
C
C Compute longitudinal and radial field components using polynomial
C parametrisation.
C
      BR=AR(1,IRGN-2)
      B(3)=AZ(1,IRGN-2)
      W=1.
      IMAX=MAX(IDR,IDZ)
      DO 1 I=1,IMAX
         W=W*X
         IF(IRGN.EQ.4.AND.X.LT.2.1) THEN
            BR=0.
         ELSEIF(IRGN.EQ.5.AND.X.LT.2.9) THEN
            BR=0.
         ELSE
            IF(I.LE.IDR) BR=BR+AR(I+1,IRGN-2)*W
         ENDIF
         IF(I.LE.IDZ) B(3)=B(3)+AZ(I+1,IRGN-2)*W
    1 CONTINUE
C
C Get x,y components
C
      B(1)=SIGN(BR,POS(3))*POS(1)/R
      B(2)=SIGN(BR,POS(3))*POS(2)/R
C
  999 RETURN
      END
#endif
