        SUBROUTINE EBGRID( NUST,INDX,ESTO,ITCR,JFCR,GRID)
C ----------------------------------------------------
C   AUTHOR   : J.Badier   01/09/89
C! Contruct an array of storeys surrounding a central tower.
CKEY PHOTONS ARRAY CLUSTER STOREYS / INTERNAL
C
C   Input  :  NUST    Number of storeys of the cluster.
C             INDX(1,IST) Theta index of the storey IST.
C             INDX(2,IST) Phi index of the storey IST.
C             INDX(3,IST) Stack number of the storey IST.
C               ESTO(IST)   Raw content of the storey IST.
C                           IST = 1 , NUST
C             ITCR      Central tower theta index
C             JFCR      Central tower phi index
C
C   Output :  GRID(IT,JF,KS)
C                       Content of storeys surounding the central tower.
C                       IT = Theta index (1 to 3)
C                       JF = Phi index   (1 to 3)
C                       KS = Stack       (1 to 3)
C
C   BANKS :
C     INPUT   : NONE
C     OUTPUT  : NONE
C     CREATED : NONE
C
C ----------------------------------------------------
#ifndef DOC
      SAVE
      DIMENSION INDX(3,*) , ESTO(*) , GRID(3,3,*) , KODE(4) , NREG(3)
      PARAMETER( LAP1 = 51 , LAP3 = 178 )
C   Initialisations
      DO 1 I = 1 , 3
      DO 1 J = 1 , 3
      DO 1 K = 1 , 3
    1 GRID(I,J,K) = 0.
C   Region code of the central tower.
      CALL EBRGCD( ITCR , JFCR , KODE , NREG , IER )
      IF( IER .NE. 0 ) GO TO 98
C   KLAP = 0 : The overlap has not to be considered.
      KLAP = 0
C   KLAP = 1 : Overlap
      IF( KODE(4) .NE. 0 ) KLAP = 1
C   KLAP = 2 : Near the luminometer.
      IF( KODE(4) .EQ. 7 ) KLAP = 2
C   Proximity of module 1 / 12 limit.
      KDIS = 0
      IF( NREG(2) .EQ. 1 .OR. NREG(2) .EQ. 12 ) THEN
        JMAX = 96 * NREG(3)
        IF( JFCR .LE. 8 ) KDIS = 1
        IF( JFCR .GE. JMAX - 8 ) KDIS = -1
      ENDIF
C   Prepare the limit of region treatment.
      IF( KODE(1) .NE. 0 ) THEN
        JCCR = ( 24 * JFCR - 12 ) / NREG(3)
        JCR1 = 24 / NREG(3)
        JCR2 = 12 / NREG(3) - 1
      ENDIF
C
C   Loop over the storeys
      DO 2 ISTO = 1 , NUST
C
        ITFI = INDX(1,ISTO)
        JFFI = INDX(2,ISTO)
        KMFI = INDX(3,ISTO)
C
        IF( KLAP .NE. 1 ) GO TO 3
C
C   One is in the overlap.
C   Change theta numerotation of the overlap towers.
        IF( ITFI .LT. LAP1 ) THEN
          IF( NREG(1) .EQ. 2 ) ITFI = ITFI + 5
        ELSE
          IF( ITFI .GT. LAP3 ) THEN
            IF( NREG(1) .EQ. 2 ) ITFI = ITFI - 5
          ELSE
            IF( NREG(1) .EQ. 1 ) ITFI = ITFI - 5
            IF( NREG(1) .EQ. 3 ) ITFI = ITFI + 5
          ENDIF
        ENDIF
C   Change stack numerotation of the overlap towers as defined in EBSLIM
        KMIN = KMFI
        IF( INDX(1,ISTO) .NE. ITFI ) THEN
          IF( KODE(4) .EQ. 0 ) THEN
            KMFI = 3
            IF( NREG(1) .NE. 2 ) KMFI = 1
          ELSE
            IF( KODE(4) .EQ. 1 ) KMFI = 1
            IF( KODE(4) .EQ. 2 ) KMFI = 1
            IF( KODE(4) .EQ. 3 .AND. KMIN .EQ. 3 ) KMFI = 2
            IF( KODE(4) .EQ. 4 ) KMFI = MIN( KMIN + 1 , 3 )
            IF( KODE(4) .EQ. 5 ) KMFI = 3
            IF( KODE(4) .EQ. 6 ) KMFI = 3
          ENDIF
        ELSE
          IF( KODE(4) .EQ. 2 ) KMFI = MIN( KMIN + 1 , 3 )
          IF( KODE(4) .EQ. 3 ) KMFI = 3
          IF( KODE(4) .EQ. 4 ) KMFI = 1
          IF( KODE(4) .EQ. 5 .AND. KMIN .EQ. 3 ) KMFI = 2
        ENDIF
C   End of the overlap treatment.
C
    3   CONTINUE
        IF( KDIS .NE. 0 ) THEN
C
C   Module 1/12 limit.
          IF( KDIS .EQ. -1 .AND. JFFI .LE.      8 ) JFFI = JFFI + JMAX
          IF( KDIS .EQ.  1 .AND. JFFI .GE. JMAX-8 ) JFFI = JFFI - JMAX
        ENDIF
C
        IF( KODE(1) .NE. 0 ) THEN
C   Near a region limit.
          CALL EMDTOW( INDX(1,ISTO),INDX(2,ISTO),ISCI,MODI,IRGI )
C         IF( IRGI .NE. NREG(1) ) THEN
          IF( IRGI .NE. NREG(3) ) THEN
C   Not in the central tower region. One searches the nearest tower
C   of the central tower region.
            NDIS = ( 24 * JFFI - 12 ) / IRGI - JCCR
            NCOR = ( ABS( NDIS ) + JCR2 ) / JCR1
            JFFI = JFCR + SIGN( NCOR , NDIS )
          ENDIF
        ENDIF
C
C   Table of the storeys surrounding the central tower.
        IT = ITFI - ITCR
        JF = JFFI - JFCR
        IF( ABS(IT) .LE. 1 .AND. ABS(JF) .LE. 1 ) GRID( IT+2,JF+2,KMFI )
     +                     = ESTO( ISTO ) + GRID( IT+2,JF+2,KMFI )
    2 CONTINUE
      GO TO 98
   98 CONTINUE
      RETURN
      END
#endif
