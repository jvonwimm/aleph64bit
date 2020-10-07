      SUBROUTINE X1REDO
C ----------------------------------------------------------------------
C
C.
C. - Author   : A. Putzer  - 95/01/10  FOR ALEPHLIB 204
C.
C! - Redo trigger analysis with different thresholds
C.
C.
C.   Bank     : X1IP                         is read
C -----------------------------------------------------------------------
#ifndef DOC
      SAVE
C
#include "bcs.h"
#include "x1tsto.h"
#include "x1namc.h"
C.
C ----------------------------------------------------------------------
C.
      PARAMETER (I2P16=65536,ISMAX=I2P16-1)
C.
C ----------------------------------------------------------------------
C.
C.
#include "bmacro.h"
C ----------------------------------------------------------------------
C.
C.
C-   Get name index for bank ('SIX2')
C.
      NASIX2 = NAMIND('SIX2')
C.
C-   Check if trigger bank ('X1RG') available
C.
      NAX1RG = NAMIND('X1RG')
      KX1RG = IW(NAX1RG)
      IF (KX1RG.EQ.0) GOTO 999
C-   Check if threshold bank ('X1TV') available
C.
      NAX1TV = NAMIND('X1TV')
      KX1MT = IW(NAX1TV)
      IF (KX1MT.EQ.0) GOTO 999
C.
C-   Check if trigger input bank ('X1IP') available
C.
      NAX1IP = NAMIND('X1IP')
      KX1IP = IW(NAX1IP)
      IF (KX1IP.EQ.0) GOTO 999
      KX1IP = KX1IP + LMHLEN
      NRO   = IW(KX1IP)
      NCO   = IW(KX1IP-1)
      DO 100 I = 1, NRO
        LX1IP = KX1IP+1
        IF (IW(KX1IP+1).EQ.INTCHA('HCW ')) THEN
          DO 201 J = 1, NSEGM, 2
            LX1IP = LX1IP + 1
            IHWSUM(J)   = IW(LX1IP)/I2P16
            IHWSUM(J+1) = MOD(IW(LX1IP),I2P16)
  201     CONTINUE
        ELSEIF (IW(KX1IP+1).EQ.INTCHA('ECW ')) THEN
          DO 202 J = 1, NSEGM, 2
            LX1IP = LX1IP + 1
            IEWSUM(J)   = IW(LX1IP)/I2P16 - 200
C           IEWSUM(J+1) = MOD(IW(LX1IP),I2P16)
            IEWSUM(J+1) = IW(LX1IP) - (IEWSUM(J)+200)*I2P16 - 200
  202     CONTINUE
        ELSEIF (IW(KX1IP+1).EQ.INTCHA('MISC')) THEN
          DO 203 J = 1, NSEGL
            LX1IP = LX1IP + 1
            ILWSUM(J)   = IW(LX1IP)
  203     CONTINUE
          DO 204 J = 1, NTEEW
            LX1IP = LX1IP + 1
            IECWTE(J)   = IW(LX1IP)
  204     CONTINUE
          DO 205 J = 1, NSEGI
            LX1IP = LX1IP + 1
            IITSUM(J)   = IW(LX1IP)
  205     CONTINUE
        ENDIF
        KX1IP = KX1IP + NCO
  100 CONTINUE
C
      CALL X1DISN
C
      CALL X1APTN
C
  999 CONTINUE
      RETURN
      END
#endif
