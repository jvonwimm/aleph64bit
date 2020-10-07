      INTEGER FUNCTION vbitgd()
CKEY VDET / USER
C----------------------------------------------------------------------
C!   Check readout state of VDET.
C!
C!   Author   : HCJ Seywerd           13-Nov-1997 
C!              New routine for handling periods when
C!              VDET HV bit was unreliable. Gets this information
C!              from the database. Replaces different routines
C!              previously in ALPHA and JULIA
C!   Modified : M.Cattaneo             2-Dec-1997
C!              Optimise access to banks, remove local arrays
C!
C!   Returns:  0 if in a period when readout was GOOD; otherwise
C!            +1 if HV appears to be ON; or
C!            -1 if HV appears to be OFF.
C!
C!            If the readout status is GOOD, Vdet information is read
C!            from ALEPH when the HV is ON and is not read out when OFF.
C!            If the readout status is BAD, the Vdet information may be
C!            read. If the Vdet is ON, this is fine; however if it is
C!            OFF, spurious noise hits will be produced.
C!            For MC always in GOOD state.
C!
C!            Periods when the status is BAD and the Vdet is OFF have
C!            been identified from the Vdet noise (number of hits in
C!            VFHL bank).
C!            In these cases, VBITGD is -1 and FRFT/2 tracks are dubious
C!            - this means the tracks stored on the Mini are dubious.
C!======================================================================
#ifndef DOC
      IMPLICIT NONE

#include "bcs.h"
      INTEGER IRUN, IEVT

      INTEGER kvhpv, kvhiv, ludbas
      SAVE ludbas

      INTEGER  mdard, NAMIND, junidb
      EXTERNAL mdard, NAMIND, junidb
C
      INTEGER iper, ioff
C
      INTEGER NAVHPV /0/, NAVHIV /0/
      SAVE NAVHPV, NAVHIV
      INTEGER lrun / 0 /, lret
      SAVE lrun, lret
C
#include "vhpvjj.h"
#include "vhivjj.h"
#include "bmacrod.h"
#include "bmacro.h"
C----------------------------------------------------------------------
      VBITGD = 0
      CALL ABRUEV(IRUN,IEVT)
      IF (IRUN.LT.2000) RETURN

C++ Get the bank vhpv from the database
      IF( NAVHPV .EQ. 0 ) THEN
        ludbas = junidb(0)
        kvhpv  = mdard(iw,ludbas,'VHPV',0)
        NAVHPV = NAMIND('VHPV')
      ELSE
        kvhpv = IW(NAVHPV)
      ENDIF
      IF (kvhpv.LE.0) THEN
C         Something went wrong...
        WRITE (IW(6),'(1X,2(A,I5))')
     >       '+++VBITGD+++  Error reading VHPV DAF on unit', ludbas,
     >       ': iret =', kvhpv
        GOTO 999
      ENDIF

C++   See if we are in a period of readout problems, once per run.
C     If not, return.

      IF( irun .NE. lrun ) THEN
        DO iper = 1, lrows(kvhpv)
          IF (irun.GE. ITABL(kvhpv,iper,jvhpfr) .AND. 
     &        irun.LE. ITABL(kvhpv,iper,jvhplr)) vbitgd = +1
        ENDDO
        lrun = irun
        lret = vbitgd
      ELSE
        vbitgd = lret
      ENDIF
      IF (vbitgd.EQ.0) GOTO 999

C++ We are in a period of readout problems 

C++ Get the bank vhpv from the database
      IF( NAVHIV .EQ. 0 ) THEN
        kvhiv  = mdard(iw,ludbas,'VHIV',0)
        NAVHIV = NAMIND('VHIV')
      ELSE
        kvhiv = IW(NAVHIV)
      ENDIF
      IF (kvhiv.LE.0) THEN
C         Something went wrong...
        WRITE (IW(6),'(1X,3(A,I5))')
     >       '+++VBITGD+++  Error reading VHIV DAF on unit', ludbas,
     >       ': iret =', kvhiv
        GOTO 999
      ENDIF
C
C++ See if HV is off.
      DO IOFF=1,lrows(kvhiv)
        IF (irun.LT.ITABL(kvhiv,ioff,jvhirn)) GOTO 999
        IF (irun.EQ.ITABL(kvhiv,ioff,jvhirn)) THEN
          IF (ievt.GE.ITABL(kvhiv,ioff,jvhife) .AND. 
     &        ievt.LE.ITABL(kvhiv,ioff,jvhile)) THEN
            vbitgd = -1
            GOTO 999
          ENDIF
        ENDIF
      ENDDO
C
  999 CONTINUE
      RETURN
      END
#endif
