        SUBROUTINE EBRGCD( IROW , JCOL , KODE , NREG , IER )
C-----------------------------------------------------
C   AUTHOR   : J.Badier    17/04/89
C! Region codes for the tower IROW,JCOL ( SOFTWR 88-15 ).
CKEY PHOTONS GAMMA REGION CODE / INTERNAL
C   The position of the tower by respect to the cracks , the overlaps
C   and the limits of region is coded in the KODE array.
C   Crack : Hole between two modules of a same subdetector.
C   Overlap : Junction between barrel (or luminometer) and endcap.
C
C   Input  :  IROW   Tower number along theta.
C             JCOL   Tower number along phi.
C
C   Output :  KODE(4)
C                 KODE(1) (LRTM) 1 or 2 if limit of region.
C                 KODE(2) (KSTM) 1 if endcap crack in the overlap.
C                 KODE(3) (KRTM) 1 , 2 , 3 or 4 if near a crack.
C                 KODE(4) (NVTM) 1 to 6 if barrel/endcap overlap.
C                                7 if endcap/luminometer.
C             NREG(3)
C                 NREG(1)   Subcomponent number( 1 to 3 ).
C                 NREG(2)   Module number( 1 to 12 ).
C                 NREG(3)   Region number( 1 to 5 ).
C             IER    1 if tower row out of range.
C                    2 if tower column out of range.
C                    0 OK
C
C   BANKS :
C     INPUT   : NONE
C     OUTPUT  : NONE
C     CREATED : NONE
C
C   Calls EMDTOW
C   Called by EBCDRG
C ----------------------------------------------------
#ifndef DOC
      SAVE
C   NBKT is the theta range. LIM# are the limits of regions.
      PARAMETER( NBJT = 228 )
      PARAMETER( LIM1 = 8 , LIM2 = 24 , LIM3 = 40 , LIM4 = 50 )
C
      DIMENSION KODE(*) , NREG(*)
C -----------------------------------------------------
C   Theta range test.
      IF( IROW .LE. 0 .OR. IROW .GT. NBJT ) GO TO 101
C   Search detector and module.
      CALL EMDTOW( IROW , JCOL , NREG(1) , NREG(2) , NREG(3) )
C   Bad module number = Phi out of range.
      IF( NREG(2) .LT. 0 .OR. NREG(2) .GT. 12 ) GO TO 102
C   Set output to zero.
      KODE(1) = 0
      KODE(4) = 0
      KODE(3) = 0
      KODE(2) = 0
      IER = 0
C   All calculations in the positive hemisphere.
      ITTA = MIN( IROW , NBJT + 1 - IROW )
      IF ( NREG(1) .EQ. 2 ) THEN
C   Barrel.
        IF ( ITTA .LE. LIM4 + 10 ) THEN
C   Near the endcap.
          IF( ITTA .GT. LIM4 + 4 ) THEN
C   No endcap but a truncated barrel.
            KODE(4) = 6
          ELSE
C   Overlap with endcap.
            IF( ITTA .LE. LIM4 + 2 ) THEN
C   More than 10 cm of barrel.
              KODE(4) = 4
            ELSE
C   Less than 10 cm of barrel.
              KODE(4) = 5
            ENDIF
          ENDIF
        ENDIF
      ELSE
C   Endcap.
        IF( NREG(3) .EQ. 4 ) THEN
C ----- Region 4 ------------------------
          IF ( ITTA .EQ. LIM3 + 1 ) THEN
            KODE(1) = 1
          ELSE
C ---------------------------------------
C   Near barrel overlap
            IF(ITTA .LT. LIM3 + 6) THEN
C   No barrel but a truncated endcap.
              IF(ITTA .GT. LIM3 + 2) KODE(4) = 1
            ELSE
              KODE(4) = 2
              IF(ITTA .GE. LIM3 + 8) KODE(4) = 3
            ENDIF
          ENDIF
        ELSE
          IF( NREG(3) .EQ. 1 ) THEN
C ----- Region 1 ------------------------
C   Near the luminometer.
            IF ( ITTA .LE. 4 ) KODE(4) = 7
C   Near region 2.
            IF ( ITTA .EQ. LIM1 ) KODE(1) = 2
          ELSE
            IF( NREG(3) .EQ. 2 ) THEN
C ----- Region 2 -------------------------
C   Near region 1.
              IF (ITTA .EQ. LIM1 + 1 ) KODE(1) = 1
C   Near region 3.
              IF ( ITTA .EQ. LIM2 )  KODE(1) = 2
            ELSE
C ----- Region 3 ------------------------
C   Near region 2.
              IF ( ITTA .EQ. LIM2 + 1) KODE(1) = 1
C   Near region 4.
              IF ( ITTA .EQ. LIM3 )  KODE(1) = 2
            ENDIF
          ENDIF
        ENDIF
      ENDIF
C ----- Crack.
      IF(NREG(1) .EQ. 2) THEN
        KRA = MOD( JCOL + 1 , 32 )
      ELSE
        KRA = MOD( JCOL + 4 * NREG(3) + 1 , 8 * NREG(3) )
      ENDIF
C   Crack code : ...., 0 , 0 , 3 , 4 // 1 , 2 , 0 , 0 ,.....
      IF ( KRA .LE. 3 ) THEN
        IF ( KRA .GT. 1 ) KODE(3) = KRA - 1
        IF ( KRA .LE. 1 ) KODE(3) = KRA + 3
      ENDIF
      IF( KODE(4) .NE. 0 .AND. KODE(4) .NE. 7 ) THEN
C   Overlap and crack in the detector which do not contains the
C   IROW , JCOL tower ===> KODE(2) = 1
        IF( NREG(1) .EQ. 2 ) THEN
          KRA = MOD( JCOL + 17 , 32 )
        ELSE
          KRA = MOD( JCOL + 1 , 32 )
        ENDIF
        IF( KRA .LE. 3 ) THEN
          KODE(2) = 1
          KODE(3) = KRA - 1
          IF( KRA .LE. 1 ) KODE(3) = KRA + 3
        ENDIF
      ENDIF
      IER = 0
      RETURN
C ====================== error =============================
C   Tower out of range.
  101 IER = 1
      GO TO 98
  102 IER = 2
 98   CONTINUE
      END
#endif
