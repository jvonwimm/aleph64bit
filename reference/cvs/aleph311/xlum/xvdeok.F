      LOGICAL FUNCTION XVDEOK(dummy)
C----------------------------------------------------------------------
C!   Determine HV state of VDET.
C!   This is a routine to replace xvdeok and vdetok
C!   as they are now used in alpha and julia respectively
C!   and is intended for alephlib.
C!   Check if VDET HV is on or not
C!   Make additional checks on periods when HV bit had problems.
CKEY VDET /USER
C!
C!   Author   : HCJ Seywerd            13-NOV-1997
C!   Modified : M.Cattaneo              2-Dec-1997
C!              Optimise access to banks, remove local arrays
C!
C!   Inputs:
C!           LUDBAS -- unit for dbase 
C!   Outputs: .TRUE. if the HV state of the VDET is ON.
C!            For MC always .TRUE.
C!======================================================================
#ifndef DOC
      IMPLICIT NONE
#include "bcs.h"

      INTEGER  vbitgd, NAMIND, mdard, junidb, xhvbit
      EXTERNAL vbitgd, NAMIND, mdard, junidb, xhvbit

      INTEGER kvhbv,ludbas
      INTEGER navhbv/0/, lrun /0/
      SAVE navhbv, ibvd, ludbas

      INTEGER kgood
      INTEGER irun,ievt

      INTEGER jhvst, ibvd
      INTEGER irow, iret
      REAL    dummy

* HV bits
#include "vhbvjj.h"
#include "bmacrod.h"
#include "bmacro.h"
C----------------------------------------------------------------------
      XVDEOK = .FALSE.
C
C++   For MC, always .TRUE.
C
      CALL ABRUEV(IRUN,IEVT)
      IF (IRUN.LT.2000) THEN
        XVDEOK = .TRUE.
        GOTO 999
      ENDIF

C
C++   Is this in a bad period when readout was blocked ?
C++   If so, check noise to determine HV state.
C
      KGOOD = VBITGD()

      IF (KGOOD.NE.0) THEN
         IF (KGOOD.EQ.+1) XVDEOK = .TRUE.
         GOTO 999
      ENDIF
C
C++ Get the VHBV bank from the dbase
      IF( navhbv .LE. 0 ) THEN
        ludbas = junidb(0)
        kvhbv  = mdard(iw,ludbas,'VHBV',0)
        navhbv = NAMIND('VHBV')
      ELSE
        kvhbv = IW(navhbv)
      ENDIF
      IF (kvhbv.LE.0) THEN
C         Something went wrong...
        WRITE (IW(6),'(1X,2(A,I5))')
     >       '+++XVDEOK+++  Error reading DAF on unit', ludbas,
     >       ': iret =', kvhbv
        GOTO 999
      ENDIF

C++ Determine once per run which bit is to be used for this run period
      IF( irun .NE. lrun ) THEN
        DO irow = 1, lrows(kvhbv)
          IF ( irun .GE. ITABL(kvhbv,irow,jvhbfr) .AND.
     &         irun .LT. ITABL(kvhbv,irow,jvhblr)) THEN
            ibvd = ITABL(kvhbv,irow,jvhbbn)
          ENDIF
        ENDDO
        lrun = irun
      ENDIF

C++ Check the bit!!
      iret = XHVBIT(JHVST)
      IF( JHVST .NE. 0 ) XVDEOK = BTEST(JHVST,IBVD)
C
  999 CONTINUE
      RETURN
      END
#endif
