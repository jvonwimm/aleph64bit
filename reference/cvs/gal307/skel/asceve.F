*DK asceve
      SUBROUTINE ASCEVE
C ----------------------------------------------------------------------
C. - F.RANJARD - 850328                 modified - 861009
C! Close the current event
C.   do analysis
C.   save event on tape
C.   drop BOS banks and release space
C. - called by    ASPRUN                      from this .HLB
C. - calls        USCEVE, ASEVST, ASWRTP      from this .HLB
C.                RDMOUT                      from KERNLIB
C---------------------------------------------------------------
#ifndef DOC
      SAVE
#include "jqcom.h"
#include "iocom.h"
#include "jobcom.h"
#include "afidjj.h"
#include "evehjj.h"
      CHARACTER*4 NAME,NLIST
      DATA IFI /0/
#include "bmacro.h"
C ----------------------------------------------------------------------
C
C - Call USER routine
      CALL USCEVE
C
C - Build event statistics bank 'ASEV'
      CALL ASEVST
C
C - Update AFID beam energy with EVEH beam energy at 1st entry
      IF (IFI .EQ. 0) THEN
        IF(IW(NAMIND('RLEP')) .EQ. 0) THEN
          JAFID = IW(NAMIND('AFID'))
          IF (JAFID .GT. 0) THEN
            JEVEH = IW(NAEVEH)
            IF (JEVEH .GT. 0) THEN
              RW(JAFID+LMHLEN+JAFIBE) = REAL(IW(JEVEH+JEVETE))*1.E-6
            ENDIF
          ENDIF
        ENDIF
        IFI = 1
      ENDIF
C
C Produce friendly Fxxx DST if asked for.
      IF (FXXXJO) THEN
        CALL ASFXXX
        IF (FDEBJO .AND. IPRIJO(16).EQ.1) CALL PRFKIN
      ENDIF
C
C - Save event onto unit # LSAVIO if required
C
      IF (MSAVJO.NE.0) CALL ASWRTP ('E')
C
      IF (FDEBJO .AND. IPRIJO(14).EQ.1) CALL AUBLIS ('E')
C
#if defined(DRAW)
C
C   Draw standard plots if diplay required
C
      IF (FDISJO)  CALL ADVIEW
#endif
C
C - Save last random number which will be the root of the next evt.
      CALL RDMOUT (NRNDJO(1))
      CALL VZERO (IRNDJO,LPRO*LRND)
C
C - Debug
C
      IF (FDEBJO .OR. KERRJO.NE.0) THEN
        CALL TIMED (TIMEJO(6))
        TIMEJO(4) = TIMEJO(4) + TIMEJO(6)
        WRITE (LOUTIO,801) NEVTJO,TIMEJO(4),TIMEJO(6)
  801   FORMAT (/1X,'+++ASCEVE+++ event# ',I5,'  total time spent = '
     &             ,F9.3,' sec , during digitization + trigger = '
     &             ,F9.3,' sec')
        TIMEJO(4) = 0.
      ENDIF
C
      RETURN
      END
#endif
