      SUBROUTINE MUTRSF(XLOC,DCLOC)
C
C***********************************************************************
C
C T.Wang -851214
C
C! tranformate the track element to the local system
C
C       Input  : common block /TRKCOM/
C       Output : XLOC(*)  -- coordinates in local system
C                DCLOC(*) -- Direction cosines in local system
C
C       Called by MUSTRM
C       Calls     GTRNSF                       in GEANT3
C
C***********************************************************************
C
      SAVE
      PARAMETER (LETRK=27, LITRK=11, LNTRK=10)
      COMMON /TRKCOM/   ITRKEL(LITRK), TRKELE(LETRK), TRKNXT(LNTRK)
     &                , FTRHAD
      LOGICAL FTRHAD
      COMMON /TRKKAR/   TRKVOL
      CHARACTER*4 TRKVOL
C
      REAL XLOC(3),DCLOC(3)
C
C       Transform coordimates of the track element to local system
C
      IF(TRKELE(27).EQ.0.)THEN
         DO 10 I=1,3
            XLOC(I) = TRKELE(I) - TRKELE(14+I)
   10    CONTINUE
      ELSE
         CALL GTRNSF(TRKELE(1),TRKELE(15),TRKELE(18),XLOC(1))
      ENDIF
C
C       Transform derection cosine to local system
C
      DO 20 I=1,3
         J=3*I
         DCLOC(I) = TRKELE(4) * TRKELE(J+15) + TRKELE(5) * TRKELE(J+16)
     +   + TRKELE(6) * TRKELE(J+17)

   20 CONTINUE
      RETURN
      END
