      SUBROUTINE VGNEVT(IFRFT)
C ---------------------------------------------------------------------
C! Create the new banks that will be filled by the VDet pattern recognition
CKEY VGLOB VDET TRACK
C
C      Paul Rensing   - 4-5-95
C
C Create the new banks (FRFT, VDCO, FVCL) that are filled by the VDet
C     pattern recognition. Also, clear the number of assignments in the
C     VDXY and VDZT banks. This should be called only once per event at
C     the start of the VDet pattern recognition.
C
C  INPUT:
C      INTEGER IFRFT  - the bank number for the new frft bank
C OUTPUT:
C      FRFT number IFRFT, VDCO, FVCL are created.
C      VDXY, VDZT are modified
C ------------------------------------------------------------------
#ifndef DOC
      IMPLICIT NONE
C
C Function parameters
C
      INTEGER IFRFT
C
C  Global includes
C
#include "bcs.h"
C
#include "vggwjj.h"
#include "vdmrjj.h"
#include "vdgcjj.h"
#include "vdcojj.h"
#include "frftjj.h"
#include "frtljj.h"
#include "vdxyjj.h"
#include "vdztjj.h"
#include "fvcljj.h"
C
C  Function calls
C
      INTEGER NLINK, NBANK, NAMIND, VDYEAR
      EXTERNAL NLINK, NBANK, NAMIND, VDYEAR
C
C  Local variables
C
      INTEGER INDEX, indVDMR, year
      integer ncl
      INTEGER OLDFRFT, NEWFRFT
      INTEGER I, J
      INTEGER NAMFRFT, NAMVDCO, NAMFRTL, NAMVDXY, NAMVDZT, NAMVDGC
C
      SAVE NAMFRFT, NAMVDCO, NAMFRTL, NAMVDXY, NAMVDZT, NAMVDGC
C
#include "bmacrod.h"
C
C Data statements
C
      DATA NAMFRFT/0/
C
#include "bmacro.h"
C
      IF (NAMFRFT .LE. 0) THEN
         NAMFRFT = NAMIND('FRFT')
         NAMVDCO = NAMIND('VDCO')
         NAMFRTL = NAMIND('FRTL')
         NAMVDXY = NAMIND('VDXY')
         NAMVDZT = NAMIND('VDZT')
         NamVDGC = NAMIND('VDGC')
      END IF
C
C Delete any banks to be recreated
C
      CALL BDROP(IW, 'VCPLVDCOFVCL')
      CALL BLIST(IW, 'E+', 'VGHCVGXC')
      CALL BLIST(IW, 'E+', 'VM5C')
C
      year = VDYEAR()
C
C Copy the FRFT bank to the new one
C
      OLDFRFT = IW(NAMFRFT)
      NEWFRFT = NBANK('FRFT', IFRFT, IW(OLDFRFT))
      IF (NEWFRFT .LE. 0) THEN
         CALL RERROR('VGNEVT', -1, ' Out of memory')
         call bosbk(iw)
         goto 999
      end if
      CALL UCOPY(IW(OLDFRFT+1),IW(NEWFRFT+1),IW(OLDFRFT))
C
C Create the VDCO bank
C
      INDEX = NBANK('VDCO',0,LMHLEN+LVDCOA*4*LROWS(OLDFRFT))
      IF (INDEX .LE. 0) THEN
         CALL RERROR('VGNEVT', -1, ' Out of memory')
         goto 999
      end if
      IW(INDEX+LMHCOL) = LVDCOA
      IW(INDEX+LMHROW) = 0
C
C Create FVCL bank.
C
      INDEX = NBANK('FVCL',0,LMHLEN+LFVCLA*4*LROWS(OLDFRFT))
      IF (INDEX .LE. 0) THEN
         CALL RERROR('VGNEVT', -1, ' Out of memory')
         goto 999
      end if
      IW(INDEX+LMHCOL) = LFVCLA
      IW(INDEX+LMHROW) = 0
C
C-- clear VDET info in FRTL
C
      INDEX = IW(NAMFRTL)
      DO I = 1, LROWS(INDEX)
         IW(KROW(INDEX,I)+JFRTIV) = 0
         IW(KROW(INDEX,I)+JFRTNV) = 0
      END DO
C
C-- clear number of associate track in VDXY and VDZT
C   If not VDet 95, create the VDMR banks anyway. This gives the hit
C     number as the unique hit ID.
C
      ncl = 0
      INDEX = IW(NAMVDXY)
      DO WHILE (INDEX .GT. 0)
         if (year .ne. 95) then
            indVDMR = NBANK('VDMR', IW(index-2)+1,
     $           LMHLEN+LROWS(index)*LVDMRA)
            If (indVDMR .le. 0) then
               call RERROR('VGNEVT', -1, ' Out of memory')
               goto 999
            end if
            IW(indVDMR + LMHCOL) = LVDMRA
            IW(indVDMR + LMHROW) = LROWS(index)
         end if
         DO I = 1, LROWS(INDEX)
            IW(KROW(INDEX,I)+JVDXNA) = 0
            if (year .ne. 95) then
               ncl = ncl + 1
               IW(KROW(indVDMR,I) + JVDMVD) = ncl
            end if
         END DO
         INDEX = IW(INDEX - 1)
      END DO
C
      INDEX = IW(NAMVDZT)
      DO WHILE (INDEX .GT. 0)
         if (year .ne. 95) then
            indVDMR = NBANK('VDMR', IW(index-2),
     $           LMHLEN+LROWS(index)*LVDMRA)
            If (indVDMR .le. 0) then
               call RERROR('VGNEVT', -1, ' Out of memory')
               goto 999
            end if
            IW(indVDMR + LMHCOL) = LVDMRA
            IW(indVDMR + LMHROW) = LROWS(index)
         end if
         DO I = 1, LROWS(INDEX)
            IW(KROW(INDEX,I)+JVDZNA) = 0
            if (year .ne. 95) then
               ncl = ncl + 1
               IW(KROW(indVDMR,I) + JVDMVD) = ncl
            end if
         END DO
         INDEX = IW(INDEX - 1)
      END DO
C
C for VDET 95, clear the NAssign field in VDGC
C
      if (year .eq. 95) then
         index = IW(NamVDGC)
         ncl = LROWS(index)
         do i = 1, ncl
            IW(KROW(index,i) + JVDGNA) = 0
         end do
      end if
C
C Set up ITC
C
CC      call ISETUP(-1)
C
C Create a (named) temporary bank for solving the combinatorics.
C
      index = NBANK('VGGW', 0, LMHLEN + ncl*LVGGWA)
      If (index .le. 0) then
         call RERROR('VGNEVT', -1, ' Out of memory')
         goto 999
      end if
      IW(index + LMHCOL) = LVGGWA
      IW(index + LMHROW) = ncl
C
 999  CONTINUE
      RETURN
      END
#endif
