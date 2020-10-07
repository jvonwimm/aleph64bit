*DK lcadc
      SUBROUTINE LCADC
C--------------------------------------------------------------
C! Fills banks LTDI and LWDI
C. - J.Dines Hansen & P.Hansen - 860417
C.                               modified by F.Ranjard - 890317
C.                               modified by P.H.Hansen -950202
C. - Stores digitizations for LCal towers and wireplanes in MeV
C. - Called by  LCDIGI                           from this .HLB
C -----------------------------------------------
#ifndef DOC
      SAVE
      EXTERNAL RNDM
#include "iocom.h"
#include "jobcom.h"
#include "bcs.h"
#include "lcparm.h"
#include "ljjpar.h"
#include "lcnamc.h"
#include "lccomc.h"
#include "bmacro.h"
#include "lcmacr.h"
C -------------------------------------------------------------
C
C -  Find length of the wire hit bank
      KWH = IW(NALWHT)
      IF(KWH .LE. 0)                                  GOTO 999
      NROW = LROWS(KWH)
      IF(NROW .LE. 0)                                 GOTO 999
C
C - Geometric cell-to-cell variations in percent
      KLCCA = IW(NALCCA)
C
C - Book wire digitalisation bank
      LEN = NROW*(LCNWPL+1) + LMHLEN
      CALL ALBOS ('LWDI',0,LEN,KWD,IGARB)
      IW(KWD+1) = LCNWPL+1
      IW(KWD+2) = NROW
      CALL BLIST(IW,'E+','LWDI')
      IF (IGARB.EQ.1) KWH = IW(NALWHT)
      KWS  = IW(NALCWS)
C
C - Loop over modules, storeyes and wire planes
        DO 130 MODU = 1,NROW
          IF(MODU.GE.3) THEN
            IFB=2
          ELSE
            IFB=1
          ENDIF
          KRWH = KROW(KWH,MODU)
          KRWD = KROW(KWD,MODU)
          IW(KRWD+1) = IW(KRWH+1)
          LAY = 0
          DO 120 IST = 1,3
            LAYMX = LCNLAY(IST)
            DO 110 L = 1,LAYMX
              LAY = LAY + 1
C
C - Convert plane hits to MeV
              ADCC = 1000.*ECRTLC*FLOAT(IW(KRWH+1+LAY))/16.
C
C - Apply gain variations
              ADCC = ADCC*(1.+GVARLC*(RNDM(DUMMY)-0.5))
C
C - Check saturation
              ADCC = AMIN1(ADCC,6250.)
C
C - Convert to integer MeV
              FACT = 1.
              IF(IST.EQ.3) FACT = SSTPLC(3)/SSTPLC(2)
              IW(KRWD+1+LAY) = NINT(ADCC*FACT)
C
C - Update summary bank
              IF(KWS.GT.0) THEN
                IW(KWS+LMHLEN+8+IFB) = IW(KWS+LMHLEN+8+IFB)+
     &                                 IW(KRWD+1+LAY)
              ENDIF
  110       CONTINUE
  120     CONTINUE
  130   CONTINUE
C
C -  Find length of the tower hit bank
      KTH = IW(NALTHT)
      IF(KTH .LE. 0)                                  GOTO 999
      NROW = LROWS(KTH)
      IF(NROW .LE. 0)                                 GOTO 999
C
C - Book tower digitalisation bank
      LEN = NROW*LTDIC + LMHLEN
      CALL ALBOS ('LTDI',0,LEN,KDI,IGARB)
      IW(KDI+1) = LTDIC
      IW(KDI+2) = NROW
      CALL BLIST(IW,'E+','LTDI')
      IF (IGARB.EQ.1) KTH = IW(NALTHT)
      KWS  = IW(NALCWS)
C
C - Loop over tower hits
      DO 230 ITOW = 1,NROW
        KRTH = KROW(KTH,ITOW)
        KRDI = KROW(KDI,ITOW)
C - Store address
        IADDR = IW(KRTH+1)
        IW(KRDI+1) = IADDR
C - Decode cell address
        IROW = LCROW(IADDR)
        ICOL = LCCOL(IADDR)
        MODU = LCMOD(IADDR)
        IF(IROW.GE.16) IROW = 31-IROW
C - Find geometrical gain variation
        GCORR = 1.
C        IF(KLCCA.GT.0) THEN
C          ICELL = JLCCDI - 1 + ICOL
C          GCORR = 1.+FLOAT(ITABL(KLCCA,IROW,ICELL))/100.
C        ENDIF
C
        DO 220 IST  = 1,3
C
C - Convert storey hits to MeV
          ADCC = 1000.*ECRTLC*FLOAT(IW(KRTH+1+IST))/16.
C
C - Apply gain variations and smear Ecrit quantization
          ADCC = ADCC*(1.+GVARLC*(RNDM(DUMMY)-0.5))*GCORR
C
C - Check saturation
          ADCC = AMIN1(ADCC,100000.)
C
C - Convert to integer MeV
          FACT = 1.
          IF(IST.EQ.3) FACT = SSTPLC(3)/SSTPLC(2)
          IW(KRDI+1+IST) = NINT(ADCC*FACT)
C
C - Update summary bank
          IF(KWS.GT.0) THEN
             IW(KWS+LMHLEN+6+IFB) = IW(KWS+LMHLEN+6+IFB)+
     &                              IW(KRDI+1+IST)
          ENDIF
  220   CONTINUE
  230 CONTINUE
C
C - debug
      IF (FDEBJO .AND. IPRIJO(5).NE.0) THEN
         WRITE(LOUTIO,*) ' +++LCADC+++ debug banks LTDI and LWDI '
         CALL PRTABL ('LTDI',0)
         CALL PRTABL ('LWDI',0)
      ENDIF
C
  999 RETURN
      END
#endif
