      INTEGER FUNCTION VDAFRD (LUNDAF,IGET)
C ----------------------------------------------------------------------
CKEY VDETDES READ DBASE / INTERNAL
C!  Read VDET geometry banks from DAF
C - Steve Wasserbaech, January 1994
C   (Based on VRDDAF, G. Triggiani, 17/02/87.)
C   Modified 31 July 1995, S. Wasserbaech: add NWFBIT to VRDOCO
C
C  Called by: VRDDAF
C  Calls:     ALGTDB
C
C - Input:
C   LUNDAF / I  Logical unit number of DAF file
C   IGET   / I  VDET setup code to be read
C
C - Output:
C   VDAFRD / I  = VDOK if successful
C               = VDERR if an error occurred
C ----------------------------------------------------------------------
#ifndef DOC
C     IMPLICIT NONE
#include "vglobl.h"
#include "vsltco.h"
#include "vdlaco.h"
#include "vzpwco.h"
#include "vwgeco.h"
#include "vrdoco.h"
#include "vgindx.h"
#include "vdetge.h"
#include "vsltjj.h"
#include "vdlajj.h"
#include "vzpwjj.h"
#include "vwgejj.h"
#include "vrdojj.h"
#include "bcs.h"
C
C     Arguments:
      INTEGER LUNDAF, IGET
C
C     Parameters:
C     List of VDET geometry banks to read:
      CHARACTER*20 LIST1
      PARAMETER (LIST1 = 'VSLTVDLAVZPWVWGEVRDO')
C
C     Local variables
      INTEGER LOUT, IRET, I
      INTEGER KVSLT, KVDLA, KVZPW, KVWGE, KVRDO
      INTEGER ILAY, IFAC, IMOD
#include "bmacrod.h"
      LOGICAL MISS
C
C     External functions:
      INTEGER ALGTDB, NAMIND
#include "bmacro.h"
C
C ----------------------------------------------------------------------
C
      VDAFRD = VDERR
      LOUT = IW(6)
C
C     Initialize all VDET geometry commons:
C
C     VSLTCO:
      NSLOTS = 0
      CALL VZERO(JJLAYF,NVFMAX)
      CALL VZERO(PHIOFF,NVFMAX)
      CALL VZERO(ISSFLG,NVFMAX)
C
C     VDLACO:
      CALL VZERO(RWVDLA,NVLAYR)
      CALL VZERO(WATILT,NVLAYR)
      CALL VZERO(IORIEN,NVLAYR)
C
C     VZPWCO:
      NWAFEF = 0
      CALL VZERO(WAFERZ,NVWFMX)
C
C     VWGECO:
      WSIZEA = 0.
      WSIZEB = 0.
      NZSTRP = 0
      NPSTRP = 0
      STPITZ = 0.
      STPITP = 0.
      STLENZ = 0.
      STLENP = 0.
      AMNSRZ = 0.
      AMNSRP = 0.
      BMNSRZ = 0.
      BMNSRP = 0.
      WTHICK = 0.
C
C     VRDOCO:
      NRDSTZ = 0
      NRDSTP = 0
      NREFRZ = 0
      NREFRP = 0
      NOFRDZ = 0
      NOFRDP = 0
      NZRSSC = 0
      IECORZ = 0
      IECORP = 0
      NZEROM = 0
      NPEROM = 0
      NWFBIT = 0
C
C     VGINDX:
      IVSTUP = 0
      NMODUL = 0
      NWAFER = 0
      CALL VZERO(NFACEL,NVLAYR)
      NWAFEM = 0
      CALL VZERO(JJFACM,NVMMAX)
      CALL VZERO(JJMODW,NVWMAX)
      CALL VZERO(JIFACF,NVFMAX)
      CALL VZERO(JIMODM,NVMMAX)
      CALL VZERO(JIWAFW,NVWMAX)
      CALL VZERO(IJFACE,NVLAYR*NVFLMX)
      CALL VZERO(IJMODU,NVLAYR*NVFLMX*NVMODF)
      CALL VZERO(IJWAFR,NVLAYR*NVFLMX*NVMODF*NVWMMX)
      CALL VZERO(JIWFFW,NVWMAX)
      CALL VZERO(IJWFFR,NVLAYR*NVFLMX*NVWFMX)
      DO IMOD=1,NVMODF
        DO IFAC=1,NVFMAX
          DO ILAY=1,NVLAYR
            TXMODU(ILAY,IFAC,IMOD) = '    '
          ENDDO
        ENDDO
      ENDDO
C
C     VDETGE:
      RVDMIN = 0.
      RVDMAX = 0.
      ZVDMAX = 0.
      CALL VZERO(WAXCEN,NVWMAX)
      CALL VZERO(WAYCEN,NVWMAX)
      CALL VZERO(WAZCEN,NVWMAX)
      CALL VZERO(WARHOC,NVFMAX)
      CALL VZERO(WAPHIC,NVFMAX)
      CALL VZERO(CPHIOF,NVFMAX)
      CALL VZERO(SPHIOF,NVFMAX)
      CALL VZERO(TNWTLT,NVLAYR)
      AMXSRZ = 0.
      AMXSRP = 0.
      BMXSRZ = 0.
      BMXSRP = 0.
      NZSROM = 0
      NPSROM = 0
      NZSMOD = 0
      NPSMOD = 0
      NPRSSC = 0
      NZROMM = 0
      LZMULT = .FALSE.
      MSVWAF = 0
      MSVNST = 0
      ISSLAY = 0
      ISSNST = 0
C
C ----------------------------------------------------------------------
C
C     Read the banks from the DAF:
      IRET = ALGTDB(LUNDAF,LIST1,-IGET)
C
      IF (IRET .EQ. 0) THEN
C       Something went wrong...
        WRITE (LOUT,'(1X,3(A,I5))')
     >        '+++VDAFRD+++  Error reading DAF on unit', LUNDAF,
     >        ', setup', IGET, ': iret =', IRET
        GO TO 1000
      ENDIF
C
C ----------------------------------------------------------------------
C
C     Get the indices to banks just read in:
C
      KVSLT = IW(NAMIND('VSLT'))
      KVDLA = IW(NAMIND('VDLA'))
      KVZPW = IW(NAMIND('VZPW'))
      KVWGE = IW(NAMIND('VWGE'))
      KVRDO = IW(NAMIND('VRDO'))
C
      MISS = (KVSLT .LE. 0) .OR. (KVDLA .LE. 0) .OR.
     >       (KVZPW .LE. 0) .OR. (KVWGE .LE. 0) .OR.
     >       (KVRDO .LE. 0)
C
      IF (MISS) THEN
        WRITE (LOUT,'(1X,A)') '+++VDAFRD+++  Missing bank(s)!'
        WRITE (LOUT,'(1X,A,I10)') '+++VDAFRD+++  KVSLT =', KVSLT
        WRITE (LOUT,'(1X,A,I10)') '+++VDAFRD+++  KVDLA =', KVDLA
        WRITE (LOUT,'(1X,A,I10)') '+++VDAFRD+++  KVZPW =', KVZPW
        WRITE (LOUT,'(1X,A,I10)') '+++VDAFRD+++  KVWGE =', KVWGE
        WRITE (LOUT,'(1X,A,I10)') '+++VDAFRD+++  KVRDO =', KVRDO
        GO TO 1000
      ENDIF
C
C ----------------------------------------------------------------------
C
C     Fill the commons which correspond to database banks:
C
C     VSLTCO (VDET slots/faces):
C
      NSLOTS = LROWS(KVSLT)
      DO I=1,NSLOTS
        JJLAYF(I) = ITABL(KVSLT,I,JVSLLA)
        PHIOFF(I) = RTABL(KVSLT,I,JVSLPH)
        ISSFLG(I) = ITABL(KVSLT,I,JVSLSS)
      ENDDO
C
C     VDLACO (VDET layers):
C
      IF (LROWS(KVDLA) .NE. NVLAYR) THEN
        WRITE (LOUT,'(1X,A,I10)')
     >       '+++VDAFRD+++  Invalid number of rows in VDLA:',
     >       LROWS(KVDLA)
        GO TO 1000
      ENDIF
      DO I=1,NVLAYR
        RWVDLA(I) = RTABL(KVDLA,I,JVDLRA)
        WATILT(I) = RTABL(KVDLA,I,JVDLTA)
        IORIEN(I) = ITABL(KVDLA,I,JVDLOR)
      ENDDO
C
C     Fill VZPWCO:
C
      NWAFEF = LROWS(KVZPW)
      DO I=1,NWAFEF
        WAFERZ(I) = RTABL(KVZPW,I,JVZPZC)
      ENDDO
C
C     Fill VWGECO:
C
      WSIZEA = RTABL(KVWGE,1,JVWGSA)
      WSIZEB = RTABL(KVWGE,1,JVWGSB)
      NZSTRP = ITABL(KVWGE,1,JVWGNZ)
      NPSTRP = ITABL(KVWGE,1,JVWGNP)
      STPITZ = RTABL(KVWGE,1,JVWGPZ)
      STPITP = RTABL(KVWGE,1,JVWGPP)
      STLENZ = RTABL(KVWGE,1,JVWGLZ)
      STLENP = RTABL(KVWGE,1,JVWGLP)
      AMNSRZ = RTABL(KVWGE,1,JVWGAZ)
      AMNSRP = RTABL(KVWGE,1,JVWGAP)
      BMNSRZ = RTABL(KVWGE,1,JVWGBZ)
      BMNSRP = RTABL(KVWGE,1,JVWGBP)
      WTHICK = RTABL(KVWGE,1,JVWGWT)
C
C     Fill VRDOCO:
C
      IF (LCOLS(KVRDO) .LT. JVRDWB) THEN
        WRITE (LOUT,'(/1X,70A1)') ('=', I=1,70)
        WRITE (LOUT,'(2A)')  ' +++VDAFRD+++ Error reading VRDO--',
     >                       'please use DB version > 199'
        WRITE (LOUT,'(1X,70A1)') ('=', I=1,70)
        WRITE (LOUT,'(1X)')
        GO TO 1000
      ENDIF
      NRDSTZ = ITABL(KVRDO,1,JVRDRZ)
      NRDSTP = ITABL(KVRDO,1,JVRDRP)
      NREFRZ = ITABL(KVRDO,1,JVRDFZ)
      NREFRP = ITABL(KVRDO,1,JVRDFP)
      NOFRDZ = ITABL(KVRDO,1,JVRDOZ)
      NOFRDP = ITABL(KVRDO,1,JVRDOP)
      NZRSSC = ITABL(KVRDO,1,JVRDSZ)
      IECORZ = ITABL(KVRDO,1,JVRDDZ)
      IECORP = ITABL(KVRDO,1,JVRDDP)
      NZEROM = ITABL(KVRDO,1,JVRDEZ)
      NPEROM = ITABL(KVRDO,1,JVRDEP)
      NWFBIT = ITABL(KVRDO,1,JVRDWB)
C
C ----------------------------------------------------------------------
C
C     Success!
C
      VDAFRD = VDOK
C
 1000 CONTINUE
      RETURN
      END
#endif
