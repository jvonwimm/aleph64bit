      SUBROUTINE VDNOIS
C----------------------------------------------------------------------
C!  - Detects noisy strips to be switched off during subsequent
C!    reprocessing
C!
C!  Author: C.Vannini  -  6/1/90
C!
C!  Modified 16-1-94 by Dave Brown for the VDET upgrade
C!  Modified March 1995 A. Bonissent, M. Thulasidas
C!                 reorganise and debug
C?
C!======================================================================
#ifndef DOC
#include "vprtnc.h"
#include "vrecon.h"
#include "vdflgs.h"
C
C  Functions calls
C
      INTEGER NLINK, NAMIND
C
C  Local variables
C
      INTEGER NAVFPH
      INTEGER NS,ISTRT,IS
      INTEGER KVFPH,KVFHL,KVFLG,KVOCC
      INTEGER IADDR,IROMD,ILAY,IWFF,IFAC,IVIEW
      INTEGER ISTRP,NVFPH,IVFPH,IVOCC,FSTRP
      INTEGER ICLU,NVFHL
      INTEGER IBNUM
      INTEGER IFLAG,IPULS
      INTEGER NCHAN,MSTRP
      REAL PITCH
      CHARACTER*6 MNAME
C
C  MSKCM is the superposition of all the masks of a bad channel
C
      INTEGER MSKCM
      PARAMETER (MSKCM = VBUNBD+VBUNUS+VBOVER+VBSUPP+VBNOIS)
#include "bcs.h"
C
C  Inline functions
C
      INTEGER IFLG
      LOGICAL SGOOD
      LOGICAL FIRST
      SAVE FIRST, NAVFPH
      DATA FIRST /.TRUE./
C
C
#include "bmacro.h"
      SGOOD(IFLG) = IAND(IFLG,MSKCM).EQ.0
C-----------------------------------------------------------------
      IF(FIRST)THEN
         FIRST=.FALSE.
         NAVFPH=NAMIND('VFPH')
      ENDIF
C
C  Look for the banks
C
      KVFPH = IW(NAVFPH)
      NHOTEV = NHOTEV + 1
C
C  Loop over all existing VPLH banks
C
      DO WHILE(KVFPH .GT. 0)
        IBNUM = IW(KVFPH-2)
C
C  Link corresponding banks
C
        KVFHL = NLINK('VFHL',IBNUM)
        KVFLG = NLINK('VFLG',IBNUM)
        IF(KVFHL .LE. 0 .OR. KVFLG .LE. 0)THEN
          CALL RERROR('VDNOIS',2,'VFHL, VFLG banks missing')
          GOTO 999
        END IF
C
C  Loop over all clusters on this wafer
C
        NVFHL  = LROWS(KVFHL)
        NVFPH = LROWS(KVFPH)
        ISTRT = 0
        DO ICLU =1,NVFHL
C
C  Unpack the address
C
          IADDR = ITABL(KVFHL,ICLU,1)
          CALL VUNADD(IADDR,NS,ILAY,IWFF,IFAC,IVIEW,ISTRP)
          CALL VAENWA(IROMD,ILAY,IWFF,IFAC,IVIEW)
C
C  Find VOCC bank
C
          KVOCC = NLINK('VOCC',IROMD)
          IF(KVOCC.LE.0) THEN
            CALL RERROR('VDNOIS',1,'VOCC bank missing')
            GOTO 999
          ENDIF
C
C  Increment occupancy for good strips above threshold
C
          DO IS=1,NS
            IVFPH = IS + ISTRT
            IPULS = ITABL(KVFPH,IVFPH,1)
            IFLAG = ITABL(KVFLG,IVFPH,1)
            IF (SGOOD(IFLAG).AND.IPULS.GE.MNPULS)THEN
              IVOCC = ISTRP+IS-1
              IW(KVOCC+IVOCC) = IW(KVOCC+IVOCC)+1
            END IF
          END DO
          ISTRT = ISTRT + NS
        END DO
C
C  Update VPLH pointer
C
        KVFPH = IW(KVFPH-1)
      END DO
C
 999  CONTINUE
      RETURN
      END
#endif
