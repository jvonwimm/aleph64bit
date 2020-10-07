      SUBROUTINE ECMOD(I,J,IMOD)
C***********************************************************************
C! DETERMINE ECAL MODULE NUMBER FOR A GIVE ROW AND COLUMN
C!          1-12 MODULES FROM ENDCAP1
C!         13-24 MODULES FROM BARREL
C!         25-36 MODULES FROM ENCAP2
C!
C***********************************************************************
#ifndef DOC
      IF (J.GE. 51.AND. J.LE.178) THEN
         MODRC = (I-1)/ 32+ 13
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
         IF (J .GE. 179) MODRC = MODRC + 24
         IF (MODRC .EQ. 13) MODRC = 1
         IF (MODRC .EQ. 37) MODRC = 36
      ENDIF
      IMOD = MODRC
      RETURN
      END
#endif
