      INTEGER FUNCTION ALRLEP (IELEP,BLEP,IPERI,IFILL,IPOL)
C -------------------------------------------------------------------
CKEY ALEF LEP RLEP
C - B.Bloch-Devaux 891106
C! Build data run header RLEP
C - Input  :  IELEP   = LEP energy in Mev
C             BLEP    = Beam type (character*4)
C             IPERI   = Lep operation period number
C             IFILL   = Lep fill number
C             IPOL    = Polarisation code
C - Output :  ALRLEP  = RLEP bank index
C                       0 means not enough space to book the bank
C  ---------------------------------------------------
#ifndef DOC
#include "bcs.h"
#include "rlepjj.h"
      CHARACTER*4   BLEP
      EXTERNAL INTCHA
#include "bmacro.h"
C --------------------------------------------------------------------
C - Book the bank 'RLEP'
      CALL AUBOS ('RLEP',0,LMHLEN+LRLEPA,JRLEP,IGARB)
      IF (JRLEP.EQ.0) GOTO 999
      IW(JRLEP+LMHCOL) = LRLEPA
      IW(JRLEP+LMHROW) = 1
      CALL BKFMT ('RLEP','3I,A,3I')
C
C - fill the  row
      KRLEP = JRLEP + LMHLEN
      IW(KRLEP+JRLELE) = IELEP
      IW(KRLEP+JRLELB) = INTCHA(BLEP)
      IW(KRLEP+JRLELD) = IPERI
      IW(KRLEP+JRLELF) = IFILL
      IW(KRLEP+JRLELP) = IPOL
C
 999  CONTINUE
      ALRLEP = JRLEP
      END
#endif
