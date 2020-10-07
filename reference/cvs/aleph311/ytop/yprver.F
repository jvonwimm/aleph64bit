      SUBROUTINE YPRVER(XVER,KBIT,LDMP)
C----------------------------------------------------------*
C!    Project vertex into VDET/ITC/TPC bit pattern
CKEY YTOP
C!    Author :     W. Manner 25/01/93
C!
C!    Description
C!    ===========
C!     Given a vertex with coordinates XVER(3) define a bit in
C!     INTEGER array KBIT(31) depending on the vertex position
C      bit 1: before VDET layer 1
C      bit 2: between VDET layer 1 and 2
C      bit 3: between VDET layer 2 and ITC or closest to ITC  layer 1
C      bit 2+I, I=2,8: closest to ITC layer I
C      bit 10+I, I=1,21: closest to TPC layer I
C----------------------------------------------------------*
#ifndef DOC
      SAVE
      REAL XVER(3),ZARR(31)
      INTEGER IBIT(31)
      LOGICAL LDMP
      DATA ZARR/ 6.,11.5,
     +   16.10, 17.20, 18.70, 19.80,
     +   21.67, 23.00, 24.68, 26.00,
     +   39.87, 46.27, 52.67, 59.00, 65.47,
     +   71.87, 78.27, 84.67, 91.07,100.22,
     +   106.62,113.02,119.40,125.82,132.22,
     +   138.62,145.02,151.42,157.82,164.22,170.62 /
C -----------------------------------------------------------
      CALL UZERO(IBIT,1,31)
      KBIT=0
      RAD=SQRT(XVER(1)**2+XVER(2)**2)
      DIS=10000.
C For VDET the logic is different
      IF(RAD.LT.ZARR(2)) KBIT=2
      IF(RAD.LT.ZARR(1)) KBIT=1
      IF(KBIT.NE.0) GO TO 100
      DO I=3,31
        IF(ABS(RAD-ZARR(I)).LT.DIS) THEN
          DIS=ABS(RAD-ZARR(I))
          KBIT=I
        ENDIF
        IF(ABS(RAD-ZARR(I)).GT.DIS.AND.KBIT.GT.0) GO TO 100
      ENDDO
  100 CONTINUE
      IBIT(KBIT)=1
      IF(LDMP)
     +  WRITE(6,'('' VERTX'',''   '',I2,8X,I2,8X,4X,8I2,4X,21I2)') IBIT
      END
#endif
