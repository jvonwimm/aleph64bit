      SUBROUTINE TPRHIT(NAME)
C  -------------------------------------------------
C! Print out TPC hit bank contents
C - M.Mermikides - 860500       F.Ranjard - 870507
C - input  : NAME  = bank name TPHT or TTHT
#ifndef DOC
      SAVE
#include "bcs.h"
      CHARACTER*4 NAME,NAM2
C --------------------------------------------------
C
      LOUT = IW(6)
      IF (LOUT.EQ.0) GOTO 999
C
      NAHT = NAMIND(NAME)
      KHT = IW(NAHT)
      IF (KHT.EQ.0) GO TO 999
      KHT1 = KHT + 2
      NWPHT = IW(KHT + 1)
      NHITS = IW(KHT + 2)
C  Get track element ref bank (xxHE)
      KHE = 0
      NAM2 = NAME(1:2)//'HE'
      NAHE = NAMIND(NAM2)
      IF (NAHE.NE.0) KHE = IW(NAHE)
C Get track element bank
      KTE = 0
      NAM2 = NAME(1:2)//'TE'
      NATE = NAMIND(NAM2)
      IF(NATE.NE.0) KTE = IW(NATE)
C
      WRITE(LOUT,800) NAME,NHITS
 800  FORMAT(//,1X,A4,'   No of hits = ',I5,/
     *  8X, 'Chan    Phi      z    dphi/dr   dz/dr   Ref TE/Kin',/)
      IT = 0
      DO 20 IH = 1,NHITS
         IK = IW(KHT1 + 1)
         IF(KHE.NE.0)  THEN
            IT = IW(KHE + 2 + IH)
            IF(KTE.NE.0) IK = IW(KTE+2+(IT-1)*IW(KTE+1) + 1)
         ENDIF
         WRITE(LOUT,801) IH,IW(KHT1 + 2),(RW(KHT1 + M),M= 3,6),IT,IK
 801     FORMAT(1X,I4,')',I8,4F8.3,2X,2I4)
         KHT1 = KHT1 + NWPHT
 20   CONTINUE
C
 999  RETURN
      END
#endif
