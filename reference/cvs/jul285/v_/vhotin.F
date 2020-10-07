      SUBROUTINE VHOTIN
C
C----------------------------------------------------------------------
C! Initialize the hot channels list (VHOT bank) for current run.
C
C!  If in a special run for hot channels list creation, initialization
C!  is done by VHINIT.  Else the VHOT bank corresponding to run IRUNRC
C!  is checked, and eventually reduced, according to current MXp/zOCC
C!  values.
C!  Author : C. Vannini  13/7/90
C!
C!  Modified 16-1-94 by Dave Brown for the VDET upgrade
C!  Modified March 1995 A. Bonissent, M. Thulasidas
C!                 reorganise and debug
C!======================================================================
#ifndef DOC
#include "vhotjj.h"
#include "vpesjj.h"
C
C  Global includes
C
#include "bcs.h"
#include "vrecon.h"
#include "vdflgs.h"
C
C  Functions
C
      INTEGER NBANK,NLINK,NAMIND
      INTEGER IRET,IGARB
C
C  Local variables
C
      INTEGER NAVHOT,KVHOT
      INTEGER IROMD,IRONX,NEWMD
      INTEGER IADDR,NSTRP,ILAY,IWAF,IPHI,IVIEW,ISTRP
      INTEGER NVHOT,IVHOT
      INTEGER KVPES,NVPES
      REAL OCCUP,OSCAL
      LOGICAL FIRST
      DATA FIRST /.TRUE./
C
C  Scale factor for computing occupancy from integer value = 1/256
C
      DATA OSCAL/.003922/
C
C  Inline functions
C
#include "bmacro.h"
C--------------------------------------------------------------------
C
      IF(FIRST)THEN
         FIRST=.FALSE.
         NAVHOT=NAMIND('VHOT')
      ENDIF
C
C Drop possible old banks
C
      CALL BDROP(IW,'VPES')
      IF (MKVHOT) THEN
         CALL VHINIT
         GOTO 999
      ENDIF
      IRET=0
C
C If data card VNHT is present, ignore the VHOT bank (which comes
C with data)
C
      IF(IW(NAMIND('VNHT')).NE.0)GO TO 999
C
      KVHOT = IW(NAVHOT)
      IF (KVHOT.LE.0) THEN
        CALL RERROR('VHOTIN',1, 'Cant find VHOT bank')
        GOTO 999
      ENDIF
      NVHOT = LROWS(KVHOT)
      IF(NVHOT .LE. 0) THEN
        CALL RERROR('VHOTIN',2,'Bank VHOT is empty')
        GOTO 999
      ENDIF
C
C Say that we start a new module
C
      NEWMD=1
      IF(NVHOT.NE.0)THEN
C
C Unpack the first address
C here # of strips is the occupancy (0-255 = 0-100%)
C
        IADDR = ITABL(KVHOT,1,JVHOHA)
        CALL VUNADD(IADDR,NSTRP,ILAY,IWAF,IPHI,IVIEW,ISTRP)
C
C Make the module address
C
        CALL VAENWA(IROMD,ILAY,IWAF,IPHI,IVIEW)
      ENDIF
C
C  Loop over the hot channels
C
      DO IVHOT = 1,NVHOT
C
C  See if the occupancy is above threshold
C
        OCCUP = NSTRP*OSCAL
        IF(OCCUP.GE.MXOCUP(IVIEW))THEN
C
C  If this is a new module, build the bank
C  (with maximum possible size)
C
          IF(NEWMD.EQ.1)THEN
            NEWMD=0
            CALL AUBOS('VPES',IROMD,LMHLEN+NVHOT*LVPESA,KVPES,IGARB)
            IF(IGARB.EQ.2)THEN
              CALL RERROR('VHOTIN',3, 'No space for VPES bank')
              IRET=1
              GOTO 999
            END IF
            IW(KVPES+LMHCOL) = LVPESA
            IW(KVPES+LMHROW) = 0
          ENDIF
C
C  Add the strip
C
          IW(KNEXT(KVPES)+JVPEHA) = ISTRP
          IW(KNEXT(KVPES)+JVPEHF) = VBHOTC
          IW(KVPES+LMHROW) = LROWS(KVPES) + 1
        END IF
        IF(IVHOT.LT.NVHOT)THEN
C
C Unpack the next address
C here # of strips is the occupancy (0-255 = 0-100%)
C
          IADDR = ITABL(KVHOT,IVHOT+1,JVHOHA)
          CALL VUNADD(IADDR,NSTRP,ILAY,IWAF,IPHI,IVIEW,ISTRP)
C
C Make the module address
C
          CALL VAENWA(IRONX,ILAY,IWAF,IPHI,IVIEW)
        ENDIF
C
C If a VPES is currently opened and
C    (end of VHOT, or we change module),
C remake the previous VPES to size, if it exists
C
        IF(NEWMD.EQ.0.AND.(IVHOT.EQ.NVHOT.OR.IRONX.NE.IROMD))THEN
          NVPES = LROWS(KVPES)
          CALL AUBOS('VPES',IROMD,LMHLEN+NVPES*LVPESA,KVPES,IGARB)
          IROMD=IRONX
          NEWMD=1
        ENDIF
      END DO
 999  CONTINUE
C
C  Put VHOT on the C list, so it isn't dropped
C
      CALL BLIST(IW,'C+','VHOT')
      RETURN
      END
#endif
