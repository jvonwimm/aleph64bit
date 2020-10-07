      SUBROUTINE RINRUN
C----------------------------------------------------------------------
C!  - Initialise the run
C!
C!
C!  Author       J. Bunn
C!
C!  Modified     S. Orteu      25-FEB-1987
C!   Modified :- E. Lancon             17-MAY-1993
C!        Add call to RFORMA
C!               F. Ranjard            25-APR-1994
C!        remove call to XTRSIN
C!               P. Comas               2-MAY-1995
C!        Add call to RWJUBW (bunch number used for reconstruction)
C?
C!======================================================================
#ifndef DOC
      INTEGER GTSTUP, ALGTDB
#include "ajobjj.h"
#include "rparac.h"
#include "bosext.h"
#include "bcs.h"
#include "rflags.h"
#include "rlunit.h"
#include "rstatr.h"
#include "rcurnt.h"
#include "rtimes.h"
#include "rconds.h"
#include "bsumco.h"
#include "rpass0.h"
#include "bmacro.h"
C--------------------------------------------------------------------
C
C?   Get Start Of Run Time Counter
C
      CALL ALTIME(ARNSRT)
C
      RENLEP = ALEFIL (IRUNRC,JLFIL,IROW)
      IF (FDEBRF) WRITE(LOUTRL,'(A,I6,A,F7.2,A)')
     *    '0RINRUN- Initialise run number ', IRUNRC,
     *    ' LEP c.m. Energy ', RENLEP, ' GeV'
C
      FMCRUN = IW(NAMIND('AJOB')).NE.0
C
C++   Define formats
C
      CALL RFORMA
C ================== '92 MC prod ========================
C the EZTH bank format has been forgotten in GALEPH.
C so the content of EZTH bank is badly translated from
C EPIO to host machine.
C To overcome the problem, drop EZTH,NR=3 and get it again
C from ADBSCONS.
      JAJOB = IW(NAMIND('AJOB'))
      IF (JAJOB.GT.0) THEN
         IGVRS = ITABL(JAJOB,1,JAJOGV)
         IGCOR = ITABL(JAJOB,1,JAJOCV)
         IF (IGVRS.LE.255 .AND. IGCOR.LE.200) THEN
            CALL BDROP (IW,'EZTH')
            IEC = GTSTUP ('EC',IRUNRC)
            IRET = ALGTDB (LRGEOM,'EZTH',-IEC)
         ENDIF
      ENDIF
      CALL BPRNT (IW,'EZTH')
C =======================================================
C
C?   Initialize PASS0
C
      IF (FPASS0) THEN
        WRITE (LOUTRL,7000) 'PASS0'
        CALL RIPAS0
        WRITE (LOUTRL,7001) 'PASS0'
      ENDIF
C
C
C?   In case of POT-POT repro save JSUM into UMJS
C?
C?   JCON is swapped because in case of reprocessing one needs
C?   the JCON bank previously used to redo TPC z coordinates unpacking
C?   One fill JCON with new values, write it to output in RWRUNH
C?   and by a swap refill JCON with old values
C
      IF ( FREPRO ) THEN
        CALL BSWAP (IW, 'JSUM', 'UMJS')
        CALL BSWAP (IW, 'JCON', 'ONJC')
      ENDIF
C
C?   zero the statistics arrays for this run
C
      CALL RZERUN
      CALL BDROP (IW,'JEDS')
C
C?   Initialise conditions for all ALEPH. This migth be changed
C
      CALL RINCND
      CALL FYIRUN(FIELRC,.TRUE.,.TRUE.,0.)
C
C?   For bunch trains, write bunch number for reconstruction in EVEH
C     Added here in case the bunch number is needed at run initiali-
C     zation by any subdetector
C
      CALL RWJUBW
C
C?   Initialise run for each detector
C
      IF (FDETRF(JULVD)) THEN
        WRITE (LOUTRL,7000) TNAMRD(JULVD)
        CALL VINIRU
        WRITE (LOUTRL,7001) TNAMRD(JULVD)
      ENDIF
      IF (FDETRF(JULIT)) THEN
        WRITE (LOUTRL,7000) TNAMRD(JULIT)
        CALL IINIRU
        WRITE (LOUTRL,7001) TNAMRD(JULIT)
      ENDIF
      IF (FDETRF(JULTP)) THEN
        WRITE (LOUTRL,7000) TNAMRD(JULTP)
        CALL TINIRU
        CALL FINIRU
        WRITE (LOUTRL,7001) TNAMRD(JULTP)
      ENDIF
      IF (FDETRF(JULEC)) THEN
        WRITE (LOUTRL,7000) TNAMRD(JULEC)
        CALL EINIRU
        WRITE (LOUTRL,7001) TNAMRD(JULEC)
      ENDIF
      IF (FDETRF(JULLC)) THEN
        WRITE (LOUTRL,7000) TNAMRD(JULLC)
        CALL LINIRU
        WRITE (LOUTRL,7001) TNAMRD(JULLC)
      ENDIF
      IF (FDETRF(JULSA)) THEN
        WRITE (LOUTRL,7000) TNAMRD(JULSA)
        CALL SINIRU
        WRITE (LOUTRL,7001) TNAMRD(JULSA)
      ENDIF
      IF (FDETRF(JULSI)) THEN
        WRITE (LOUTRL,7000) TNAMRD(JULSI)
        CALL SIINIRU
        WRITE (LOUTRL,7001) TNAMRD(JULSI)
      ENDIF
      IF ( FDETRF(JULHC) .OR. FDETRF(JULMU) ) THEN
        WRITE (LOUTRL,7000) TNAMRD(JULHC)
        CALL HINIRU
        WRITE (LOUTRL,7001) TNAMRD(JULHC)
      ENDIF
      IF (FDETRF(JULMU)) THEN
        WRITE (LOUTRL,7000) TNAMRD(JULMU)
        CALL MINIRU
        WRITE (LOUTRL,7001) TNAMRD(JULMU)
      ENDIF
      IF (FDETRF(JULBC)) THEN
        WRITE (LOUTRL,7000) TNAMRD(JULBC)
        CALL BINIRU
        WRITE (LOUTRL,7001) TNAMRD(JULBC)
      ENDIF
      IF (FDETRF(JULBO)) THEN
        WRITE (LOUTRL,7000) TNAMRD(JULBO)
        CALL OINIRU
        WRITE (LOUTRL,7001) TNAMRD(JULBO)
      ENDIF
      IF (FDETRF(JULYR).OR.FDETRF(JULYT)) THEN
        WRITE (LOUTRL,7000) TNAMRD(JULYR)
        CALL YINIRU
        WRITE (LOUTRL,7001) TNAMRD(JULYT)
      ENDIF
      IF (FDETRF(JULFA)) THEN
        WRITE (LOUTRL,7000) TNAMRD(JULFA)
        CALL RFALIR
        WRITE (LOUTRL,7001) TNAMRD(JULFA)
      ENDIF
C
C?   Initialize Beam Position finding:
C
      CALL VBSINR
C
C?   User initialises his/her variables.
C
      CALL USIRUN
C
C?   Fill JCON Bank
C
      CALL RFJCON
C
C?   Set to zero BSUM array (used in BMSUM and RCJSUM)
C
      CALL VZERO (BSUM,20)
C
C?   Write Run header to the POT
C
      IF (ANYORL) CALL RWRUNH
C
C?   In case of Re-processing swap again JCON
C
      IF ( FREPRO ) CALL BSWAP (IW, 'JCON', 'ONJC')
C--------------------------------------------------------------------
  999 CONTINUE
 7000 FORMAT(/,1X,'  +------------ ',A,
     &  ' -- Run Initialisation ---------------- Started  ----------+')
 7001 FORMAT(1X,'  +------------ ',A,
     &  ' -- Run Initialisation ---------------- Finished ----------+')
      END
#endif
