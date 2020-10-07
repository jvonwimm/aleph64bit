      SUBROUTINE TSCOOR(ISLOT,IROW,RPHIP,ZB,RS,RPHIS,ZS)
C
C-----------------------------------------------------------------------
C! Determine coordinate in sector frame
C!
C!    Author:    R. Johnson  03-08-87
C!    Modified:  R. Johnson  19-01-88
C!
C!    Input:
C!       - ISLOT     /I      Sector slot number
C!       - IROW      /I      Padrow number in the sector
C!       - RPHIP     /R      r*phi coordinate in units of pad spacing
C!       - ZB        /R      Z coordinate in units of bucket length
C!    Output:
C!       - RS        /R      radius in units of cm in sector frame
C!       - RPHIS     /R      r*phi coordinate in units of cm
C!       - ZS        /R      z coordinate in units of cm
C!
C!    Called by TCOOR
C!
C!----------------------------------------------------------------------
#ifndef DOC
C
#include "tparac.h"
#include "tpgpar.h"
#include "tpgeom.h"
#include "tpgeop.h"
#include "talign.h"
#include "tpccon.h"
#include "tflags.h"
C
      ISTYP=ITPTYP(ISLOT)
C
C++   Convert buckets to cm using the drift velocity
C
      ZS= (ZB-TVOFFS(ISLOT))*TVDRFT(IENDTP(ISLOT))
C
C++   Convert pad number to cm for the r*phi coordinate
C
      NTPAD=NTPDPR(IROW,ISTYP)+2
      RPHIS = TPDSEP(ISTYP) * (0.5*FLOAT(NTPAD+1)-RPHIP)
C
C++   Convert the sector row number to a radius in cm
C
      IF (ISTYP.EQ.1) THEN
        IROWG=IROW
      ELSE
        IROWG=IROW+NTPDRW(1)
      ENDIF
      RS=TPPROW(IROWG)
C
C++   Correct for any misalignment of pads within the sector.
C++   So far, this is expected to be a problem only for TPC90.
C
      IF (FTPC90) THEN
        RPHIS = RPHIS + TAPRPH(IROW,ISTYP)*RS
      ENDIF
C
      RETURN
      END
#endif
