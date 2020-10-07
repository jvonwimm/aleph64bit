      SUBROUTINE YVSCM2(F1,F2,NU1,NV,NU2,MU1,MV,MU2)
CKEY  QVSRCH / INTERNAL
C ----------------------------------------------------------------------
C! Finds consistent peak in 2 sampled 2-d likelihood functions
C     STAYS AWAY FROM EDGES IN ALL VARIABLES
C  Author : T. MATTISON  U.A.BARCELONA/SLAC  1 DECEMBER 1992
C
C  Input Arguments :
C  *  F1 IS FIRST FUNCTION, STORED AS IF DIMENSIONED F1(NU1,NV)
C  *  F2 IS SECOND, STORED AS F2(NU2,NV)
C  *  NU1,NV,NU2 ARE DIMENSIONS OF ARRAYS
C  Output Arguments :
C  *  MU1,MV,MU2 DESCRIBE LOCATION OF MAXIMUM OF SUM OF 2 FUNCTIONS:
C        F1(MU1,MV)+F2(MU2,MV)
C
C ----------------------------------------------------------------------
      DIMENSION F1(*),F2(*)
C ----------------------------------------------------------------------
      FSUM=-999999.
C LOOP OVER COMMON VARIABLE V
      DO 350 IV=2,NV-1
C FIND LARGEST ELEMENT IN F1 AT THIS V
        IOF=(IV-1)*NU1
        F1M=-999999.
        DO 150 IU1=2,NU1-1
          IBIN=IOF+IU1
          IF (F1(IBIN) .GT. F1M) THEN
            JU1=IU1
            F1M=F1(IBIN)
          ENDIF
  150   CONTINUE
C FIND LARGEST ELEMENT IN F2 AT THIS V
        IOF=(IV-1)*NU2
        F2M=-999999.
        DO 250 IU2=2,NU2-1
          IBIN=IOF+IU2
          IF (F2(IBIN) .GT. F2M) THEN
            JU2=IU2
            F2M=F2(IBIN)
          ENDIF
  250   CONTINUE
C SAVE IF LARGEST YET
        IF (F1M+F2M .GT. FSUM) THEN
          MU1=JU1
          MU2=JU2
          MV=IV
          FSUM=F1M+F2M
        ENDIF
  350 CONTINUE
      RETURN
      END
