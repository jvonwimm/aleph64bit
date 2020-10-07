      SUBROUTINE BDROP(JW,LIST)
C
C     DROP SET OF BANKS, SPECIFIED BY LIST
C
      INTEGER JW(*)
#include "boscom.h"
#include "param.h"
      CHARACTER*(*) LIST
      CHARACTER*4 NAME
C     ...
      IF(JW(ICK).NE.12345) CALL BBNOT(JW,'BDROP')
C
C     HISTOGRAM ON USED SPACE
C
      NUS=JW(NJW)-JW(IWK)+JW(IGP)-JW(NDW)
      IH=10.0*FLOAT(NUS)/FLOAT(JW(NJW))+1.0
      IH=MIN(10,MAX(1,IH))
      JW(NFH+IH)=JW(NFH+IH)+1
      JW(LCH)=JW(LCH)+1
#include "listnn.h"
      IF(NAMI.EQ.0) GOTO 20
C
C     RESET LINK BANKS
C
      JND=JW(IDL)+NAMI-NSYST
      IF(JW(JND).NE.0) JW(JW(JND))=0
      IND=NAMI+1
   10 IND=JW(IND-1)
      IF(IND.NE.0) THEN
         NDIF=JW(IND)+4
         JW(NDN)=JW(NDN)+NDIF
         JW(IGN)=MIN(JW(IGN),IND)
         JW(IND)=-NDIF
         GOTO 10
      END IF
      JW(NAMI)=0
      GOTO 3
C
C     TRY TO REGAIN SPACE
C
   20 IF(JW(IGN)+JW(NDN)-3.EQ.JW(IGP)) THEN
C
C        CONTIGOUS SPACE
C
         JW(IGP)=JW(IGN)-3
         JW(NDN)=0
         JW(IGN)=JW(NJW)
      ELSE
C
C        NOT CONTIGUOUS SPACE
C
         INDROP=0
         IND=JW(IGN)
   30    IF(IND.LT.JW(IGP)) THEN
            IF(JW(IND).LT.0) THEN
               IF(INDROP.EQ.0) INDROP=IND
               IND=IND-JW(IND)
            ELSE
               IND=IND+JW(IND)+4
               INDROP=0
            END IF
            GOTO 30
         ELSE
            IF(INDROP.NE.0) THEN
               JW(NDN)=JW(NDN)+INDROP-3-JW(IGP)
               JW(IGP)=INDROP-3
            END IF
         END IF
      END IF
      JW(1)=0
      JW(2)=0
  100 RETURN
      END
