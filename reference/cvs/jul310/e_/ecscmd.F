      SUBROUTINE ECSCMD(I,J,IMOD,ISC)
C***********************************************************************
C! DETERMINE ECAL MODULE NUMBER FOR A GIVE ROW AND COLUMN
C             I = PHI NUMBER
C             J = TETA NUMBER
C!    IMOD  1-12 MODULES
C     ISC = 1 ENDCAP 1
C           2 BARREL
C           3 ENCAP 2
C!
C?
C!======================================================================
#ifndef DOC
      IF (J.GE. 51.AND. J.LE.178) THEN
         ISC = 2
         MODRC = (I-1)/ 32
         MODRC = MODRC +1
      ELSE
         JJ = J
         IF (JJ .GE. 179) JJ =  229- J
         IF (JJ .LE. 8) THEN
            MODRC = (I+3)/ 8+ 1
         ELSEIF (JJ .LE. 24) THEN
            MODRC = (I+7)/ 16+ 1
         ELSEIF (JJ .LE. 40) THEN
            MODRC = (I+11)/ 24+ 1
         ELSE
            MODRC = (I+15)/ 32+ 1
         ENDIF
         ISC = 1
         IF (J .GE. 179) ISC = 3
         IF (MODRC .EQ. 13) MODRC = 1
         IF (MODRC .EQ. 37) MODRC = 36
      ENDIF
      IMOD = MODRC
      RETURN
      END
#endif
