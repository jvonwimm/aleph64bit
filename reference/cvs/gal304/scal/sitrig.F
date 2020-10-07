*DK sitrig
      SUBROUTINE SITRIG
C.----------------------------------------------------------------
C  B.Bloch-Devaux  October 91 -January 93
C! SCAL : Form Trigger signals , apply zero suppression when creating
C!        SIFO analog sums
C     - Called by SIDIGI
C     - Calls     BDROP,BLIST                    from BOS lib
C     - Calls     ALBOS                          from ALEPHlib
C.----------------------------------------------------------------
#ifndef DOC
      SAVE
#include "bcs.h"
#include "sinamc.h"
#include "siconst.h"
#include "sifojj.h"
#include "sihtjj.h"
C   MASK for address without R bin
      PARAMETER ( MSKRSI = 32707)
C     PARAMETER ( MSKRSI = 'FC3'X)
#include "bmacro.h"
C-----------------------------------------------------------
C - Drop existing banks
      CALL BDROP (IW,'SIFO')
C - Create new bank SIFO from SIHT info
      JSIHT = IW(NASIHT)
      IF ( JSIHT.LE.0) GO TO 999
      LSROW = LROWS(JSIHT)
      IF ( LSROW.LE.0) GO TO 999
      LEN = LSROW *LSIFOA+LMHLEN
      CALL ALBOS('SIFO',0,LEN,JSIFO,IGARB)
      IW(JSIFO+LMHCOL) = LSIFOA
      IW(JSIFO+LMHROW) = 0
      IF ( IGARB.GT.0) JSIHT = IW(NASIHT)
C - Add to the 'E' list
      CALL BLIST(IW,'E+','SIFO')
C Make summation over Amplex ( all R bins)
      DO 10 I = 1,LSROW
         IAD = IAND(ITABL(JSIHT,I,JSIHAD),MSKRSI)
         NSIFO = LROWS(JSIFO)
C No row filled yet , fill first address
         IF ( NSIFO.EQ.0 ) THEN
            IW(KNEXT(JSIFO)+JSIFAD) = IAD
            IW(JSIFO+LMHROW) = IW(JSIFO+LMHROW)+1
            NSIFO = LROWS(JSIFO)
         ENDIF
C  Look if this address already exists in SIFO
         IROW = 0
         DO 20 J= 1,NSIFO
            IF  (IAD.EQ.ITABL(JSIFO,J,JSIFAD)) IROW = J
 20      CONTINUE
C  This is a new row
         IF ( IROW.EQ.0) THEN
            IROW = LROWS(JSIFO)+1
            IW(KNEXT(JSIFO)+JSIFAD) = IAD
            IW(JSIFO+LMHROW) = IW(JSIFO+LMHROW)+1
         ENDIF
C  Update content
         DO 30 K = 1,3
            IW(KROW(JSIFO,IROW)+JSIFAD+K)=IW(KROW(JSIFO,IROW)+JSIFAD+K)
     $                      + IW(KROW(JSIHT,I)+JSIHAD+K)
 30      CONTINUE
 10   CONTINUE
      NSIFO = LROWS(JSIFO)
      KFIN = 0
      DO 50 I = 1,NSIFO
C Now transform into ADC counts ( SIMPCT Mev per ADC count ) and apply
C Zero suppression : at least one value in each triplet is above ISIFCT
         KSIF = KROW(JSIFO,I)
         DO 40 K = 1,3
            IW(KSIF+JSIFAD+K)=IW(KSIF+JSIFAD+K)/SIMPCT
 40      CONTINUE
         IF( (IW(KSIF+JSIFA1).GT.ISIFCT).OR.(IW(KSIF+JSIFA2).GT.ISIFCT)
     $   .OR.     (IW(KSIF+JSIFA3).GT.ISIFCT)) THEN
            KFIN = KFIN + 1
            IF ( KFIN.NE.I) THEN
               KSFO = KROW(JSIFO,KFIN)
               IW(KSFO+JSIFAD) = IW(KSIF+JSIFAD)
               IW(KSFO+JSIFA1) = IW(KSIF+JSIFA1)
               IW(KSFO+JSIFA2) = IW(KSIF+JSIFA2)
               IW(KSFO+JSIFA3) = IW(KSIF+JSIFA3)
            ENDIF
         ENDIF
  50  CONTINUE
      IW(JSIFO+LMHROW) = KFIN
C
 999  RETURN
      END
#endif
