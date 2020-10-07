      FUNCTION MDROP(JW,NAME,NR)
#include "boscom.h"
#include "param.h"
      INTEGER JW(*)
#include "namcha.h"
      IF(JW(ICK).NE.12345) CALL BBNOT(JW,'MDROP')
      JW1=JW(1)
      JW(1)=0
      JW(2)=0
#include "namez.h"
      IF(NAMI.EQ.0) GOTO 100
#include "index.h"
      IF(INDI.NE.0) THEN
C
C        DROP BANK
C
         JW(LCH)=JW(LCH)+1
         JW(INDJ-1)=JW(INDI-1)
         NDIF=JW(INDI)+4
         IF(JW1.NE.1.AND.JW(IGP).EQ.INDI+NDIF-3) THEN
C
C           LAST BANK
C
            JW(IGP)=JW(IGP)-NDIF
         ELSE
C
C           NOT LAST BANK OR JW(1)=1 (FORCE DROP)
C
            JW(INDI)=-NDIF
            JW(NDN)=JW(NDN)+NDIF
            JW(IGN)=MIN(JW(IGN),INDI)
         END IF
C
C        RESET LINK BANKS
C
         JND=JW(IDL)+NAMI-NSYST
         IF(JW(JND).NE.0) JW(JW(JND))=0
      END IF
  100 MDROP=0
      RETURN
      END
