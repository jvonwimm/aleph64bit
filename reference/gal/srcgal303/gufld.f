      SUBROUTINE GUFLD(POS,B)
C--------------------------------------------------------------
C! Return field components for position xyz
C
C    B. Bloch-Devaux                 30 april 1987
C   this routine is the interface between Geant tracking and
C   the field map in case one has to use such a map
C       modified may 1988 to introduce better parametrisation
C
C  units are cm for POS and KGauss for Field components  B
C-----------------------------------------------------------------
      SAVE
      PARAMETER (LNRG=11)
      COMMON / FMGEOM / FMZMIN(LNRG),FMZMAX(LNRG),FMRMIN(LNRG)
     1                  ,FMRMAX(LNRG)
      COMMON/ALFGEO/ALRMAX,ALZMAX,ALFIEL,ALECMS
C
      PARAMETER (IDR=4,IDZ=5,NRGN=6)
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
C
      RXY = SQRT(POS(1)*POS(1) + POS(2)*POS(2))
      ABSZ=ABS(POS(3))
      DO 10 I=3,8
      IF (RXY.LE.FMRMAX(I) .AND. RXY.GE.FMRMIN(I)
     &  .AND. ABSZ.GE.FMZMIN(I) .AND. ABSZ.LE.FMZMAX(I)) GO TO 11
  10  CONTINUE
      I=0
  11  CONTINUE
      IRGN=I
C
C In regions 2 and 9 field is zero.  Return also if region = 0
C
      IF(IRGN*(IRGN-2)*(IRGN-9).EQ.0)  GO TO 90
C
C Region of uniform solenoidal field (TPC + ECAL).
C
      IF (IRGN.EQ.1 .OR. IRGN.GE.10)    GO TO 95
C
C Barrel regions: Parametrisation in terms of Z-component.
C Endcap regions: Parametrisation in terms of R
C
      IF (IRGN.EQ.4 .OR. IRGN.EQ.5) THEN
C
C Get R or Z at the entrance of the region
C
         X= (ABSZ/100.)*(XEN(IRGN-2)/RXY)
      ELSE
         X= (RXY/100.)*(XEN(IRGN-2)/ABSZ)
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
      B(1)=SIGN(BR,POS(3))*POS(1)/RXY
      B(2)=SIGN(BR,POS(3))*POS(2)/RXY
C
C
      GO TO 96
 90   CONTINUE
      B(1)=0.
      B(2)=0.
      B(3)=0.
      GO TO 96
  95  CONTINUE
         B(3)=15.
         B(2)=0.
         B(1)=0.
C
C       allow for a non nominal field
C
  96    CONTINUE
      FUDGE=ALFIEL/15.
      DO 30 I=1,3
        B(I)=B(I)*FUDGE
  30  CONTINUE
      RETURN
      END
