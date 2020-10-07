      SUBROUTINE THPCOR(NHALF,IPDMN,ISLOT,IROW,RPHI,RFUDG,IERY)
C
C--------------------------------------------------------------------
C! Fudge the coordinate location in case two half pads are used
C
C  R. Johnson     13-3-90
C  D. Casper      06-08-97 create THPCOR from THPFDG, to handle
C                          corrections for 1 half-pad coords as well
C
C  Input:  NHALF     /I      Number of half-pads in coordinate
C          IPDMN     /I      1st pad number in the cluster
C          ISLOT     /I      TPC sector slot number
C          IROW      /I      Row number in the sector
C          RPHI      /R      Uncorrected RPHI in pad units
C  Output: RFUDG     /R      Corrected RPHI
C          IERY      /I      Error code.  Chuck this coordinate if
C                            not zero.
C--------------------------------------------------------------------
#ifndef DOC
C
#include "tpgpar.h"
#include "tpgeom.h"
#include "tpgeop.h"
C
      IERY=0
      ISTYP=ITPTYP(ISLOT)
      NTPAD=NTPDPR(IROW,ISTYP)+2
C
C++   If the cluster has half pads in it then
C++   apply an empirical correction
C
      
      IF (IPDMN.LE.2) THEN
        DEL = (RPHI - 2.0)*TPDSEP(ISTYP)
        IF (NHALF.EQ.1) THEN
          CALL TCSHF1(DEL,DELO,IER)
        ELSE
          CALL TCSHFT(DEL,DELO,IER)
        ENDIF
        RFUDG = RPHI + (DELO - DEL)/TPDSEP(ISTYP)
        IF (IER.NE.0) THEN
           IERY = 800
           GOTO 999
        ENDIF
      ELSE
        DEL = (FLOAT(NTPAD) - RPHI -1.0)*TPDSEP(ISTYP)
        IF (NHALF.EQ.1) THEN
          CALL TCSHF1(DEL,DELO,IER)
        ELSE
          CALL TCSHFT(DEL,DELO,IER)
        ENDIF
        RFUDG = RPHI + (DEL - DELO)/TPDSEP(ISTYP)
        IF (IER.NE.0) THEN
          IERY = 800
          GOTO 999
        ENDIF
      ENDIF
C
  999 CONTINUE
      RETURN
      END
#endif
