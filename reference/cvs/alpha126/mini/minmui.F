      SUBROUTINE MINMUI
C
CKEY MDST /USER
C-----------------------------------------------------------------------
C! Modify and save muon banks for Mini-DST.
C
C     Author: Stephen Haywood      28-Jul-1992
C     Mod     J. Boucrot           09-Jan-2001
C
C     Input  : HMAD, MCAD, MUID
C     Output : HMAD, MCAD, MUID
C 
C     Called by MINDST.
C-----------------------------------------------------------------------
#ifndef DOC
C
      PARAMETER ( MINNV = 205)
#include "bcs.h"
#include "hmadjj.h"
#include "bmacro.h"
C-----------------------------------------------------------------------      
C
C++   Only keep HMAD entries which have N10 > 0 or Muon Chamber hits
C++   in MINIS version < 205;
C++   Keep all HMAD entries for MINI version >= 205
C
      IDMAA=1
      IF (MINVSN(DUM).GE.MINNV) IDMAA=2
C
      KHMAD = NLINK('HMAD',0)
      IF (KHMAD.GT.0) THEN
         NHMAD = LROWS(KHMAD)
      ELSE
         NHMAD = 0
      ENDIF
C
      KOUNT = 0
      DO 100 I=1,NHMAD
         ITRAK = ITABL(KHMAD,I,JHMATN)
         CALL AMUID(ITRAK, IRUN,IBE,IBT,IM1,IM2,NEXP,NFIR,N10,N03,XMULT,
     &     RAPP,ANG,ISHAD,SUDNT,IDF,IMCF,IER)
         IF (IDMAA.EQ.1.AND.N10.LE.0.AND.IM1.LE.0.AND.IM2.LE.0) GOTO 100
         KOUNT = KOUNT + 1
         IF (I.GT.KOUNT) CALL UCOPY(IW(KROW(KHMAD,I)+1),
     &     IW(KROW(KHMAD,KOUNT)+1),LHMADA)
  100 CONTINUE
C
C++   Modify size of HMAD if necessary and add to Mini list.
C
      IF (KOUNT.GT.0) THEN
         IF (KOUNT.NE.NHMAD) THEN
            LEN = LMHLEN + LHMADA * KOUNT
            CALL AUBOS('HMAD',0,LEN, KHMAD,IGARB)
            IW(KHMAD+LMHROW) = KOUNT
         ENDIF
         CALL MINLIS('HMAD')
      ENDIF
C
C++   Add MCAD to the Mini list provided it is not empty.
C
      KMCAD = NLINK('MCAD',0)
      IF (KMCAD.GT.0) THEN
         NMCAD = LROWS(KMCAD)
      ELSE
         NMCAD = 0
      ENDIF
      IF (NMCAD.GT.0) CALL MINLIS('MCAD')
C
C++   Add MUID to the Mini list provided it is not empty.
C
      KMUID = NLINK('MUID',0)
      IF (KMUID.GT.0) THEN
         NMUID = LROWS(KMUID)
      ELSE
         NMUID = 0
      ENDIF
      IF (NMUID.GT.0) CALL MINLIS('MUID')
C++   Add D4CD to the MINI list
      CALL MINLIS('D4CD')
C
      RETURN
      END
#endif
