        SUBROUTINE EBCDRG(TETA , PHI , ITRW , JFCL , KODE , NREG , IER)
C-----------------------------------------------------
C   AUTHOR   : J.Badier    17/04/89
C! Region codes for the direction theta , phi.
CKEY PHOTONS GAMMA REGION CODE / USER
C   The position of the tower by respect to the cracks , the overlaps
C   and the limits of region is coded in the KODE array.
C   Crack : Hole between two modules of a same subdetector.
C   Overlap : Junction between barrel (or luminometer) and endcap.
C
C   Input  :  TETA   Theta angle.
C             PHI    Phi angle.
C
C   Output :  ITRW   Raw    | of the tower containing
C             JFCL   Column | the theta , phi direction.
C             KODE
C                 KODE(1) (LRTM) 1 or 2 if limit of region.
C                 KODE(2) (KSTM) 1 if endcap crack in the overlap.
C                 KODE(3) (KRTM) 1 , 2 , 3 or 4 if near a crack.
C                 KODE(4) (NVTM) 1 to 6 if barrel/endcap overlap.
C                                7 if endcap/luminometer.
C             NREG(3)
C                 NREG(1)   Subcomponent number( 1 to 3 ).
C                 NREG(2)   Module number( 1 to 12 ).
C                 NREG(3)   Region number( 1 to 4 ).
C             IER    1 if tower row out of range.
C
C   BANKS :
C     INPUT   : NONE
C     OUTPUT  : NONE
C     CREATED : NONE
C
C   Calls EFNDTW  EBRGCD
C ----------------------------------------------------
#ifndef DOC
      SAVE
      DIMENSION KODE(*) , NREG(*) , X(3)
      CHARACTER*6 FAUX
C   X point inside barrel cylinder.
      X(3) = COS( TETA )
      R = SIN( TETA )
      X(1) = R * COS( PHI )
      X(2) = R * SIN( PHI )
      CALL EFNDTW(X,'ALEPH',JFCL,ITRW,IST,ISC,MD,IPL,FAUX)
      IF( ITRW .LT. 51 .OR. ITRW .GT. 178 ) THEN
C   Direction out of barrel. (X is a direction, the * by 400. is there
C   to get a point in the endcap region)
        DO 1 I = 1 , 3
          X(I) = 400. * X(I)
    1   CONTINUE
        CALL EFNDTW(X,'ALEPH',JFCL,ITRW,IST,ISC,MD,IPL,FAUX)
C   Is it also out of endcap ?
        IF( FAUX .EQ. 'REGION' ) GO TO 101
      ENDIF
C   Evaluate region code.
      IER = 0
      CALL EBRGCD( ITRW , JFCL , KODE , NREG , IER )
      IF( IER .NE. 0 ) GO TO 101
      RETURN
C ============================ error =============================
C   Direction out of EMCAL.
  101 CONTINUE
      IER = 1
      DO 2 I = 1 , 3
        KODE(I) = 0
        NREG(I) = 0
    2 CONTINUE
      KODE(4) = 0
      END
#endif
