      SUBROUTINE EALIGN
C.----------------------------------------------------------------------
CKEY ECALDES ALIGNMENT MATRIX / INTERNAL
C     H.Videau      creation le 15/12/85  modification le 04/03/91
C! Fill alignment matrix
C  This routine enters in EALIAM the displacement matrices from the
C  EPHY bank. If EPHY NR =1 theoretical alignment, if NR = 2001
C the alignment is taken from the february 91 analysis by HV.
C  There is one matrix (4x4 ) per module in its slot: 36
C   Input :
C           none
C   Output:
C           Alignment matrix EALIAM
C   Called by EDEFMD
C.----------------------------------------------------------------------
#ifndef DOC
      IMPLICIT NONE
      SAVE
#include "ecccom.h"
C
      INTEGER MD,SC,I,J,SL
C
      DO 10 SC=1,ECALSC
         DO 20 MD=1,ECALMD
           SL=(SC-1)*ECALMD+MD
C  Definition d'une matrice diagonale
            DO 21 J=1,ECALLS
              DO 21 I=1,ECALLS
                IF(I.EQ.J) THEN
                  EALIAM(I,J,MD,SC) = 1.
                ELSE
                  EALIAM(I,J,MD,SC) = 0.
                END IF
 21         CONTINUE
            EALIID(MD,SC)= SL
C  petites translations
              EALIAM(1,4,MD,SC)=EALIAM(1,4,MD,SC)-EPHYLE(1,SL)
              EALIAM(2,4,MD,SC)=EALIAM(2,4,MD,SC)-EPHYLE(2,SL)
              EALIAM(3,4,MD,SC)=EALIAM(3,4,MD,SC)-EPHYLE(3,SL)
C  petites rotations
              EALIAM(1,2,MD,SC)=EALIAM(1,2,MD,SC)-EPHYAN(3,SL)
              EALIAM(2,3,MD,SC)=EALIAM(2,3,MD,SC)-EPHYAN(1,SL)
              EALIAM(1,3,MD,SC)=EALIAM(1,3,MD,SC)+EPHYAN(2,SL)
              EALIAM(2,1,MD,SC)=EALIAM(2,1,MD,SC)+EPHYAN(3,SL)
              EALIAM(3,2,MD,SC)=EALIAM(3,2,MD,SC)+EPHYAN(1,SL)
              EALIAM(3,1,MD,SC)=EALIAM(3,1,MD,SC)-EPHYAN(2,SL)
   20    CONTINUE
   10 CONTINUE
      END
#endif
