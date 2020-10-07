      INTEGER FUNCTION VINIVG()
C ----------------------------------------------------------------------
CKEY VDETDES INDEX / USER
C! initialize VDET geography package, unconnected channels, face/module
C
C  VZGEOG.FOR
C       extensions to VDET geometry package        21 Sept. 1995
C                                                   J. Rothberg
C                               modified  6 Oct. 1995  for VEXCH
C ---------------------------------------------------------------------
C - Joe Rothberg, August 20 1995
C ---------------------------------------------------------------------
#ifndef DOC
C     IMPLICIT NONE
C ---------------------------------------------------------------------
#include "vduefc.h"
#include "vglobl.h"
#include "vsltco.h"
#include "bcs.h"
C ---------------------------------------------------------------------
      INTEGER NAMI, NAMIND, GTSTUP
      INTEGER i, nrows, irow
C
      INTEGER KVUEC, KVXCH, KVUBE, KVFMC
      INTEGER version
      CHARACTER*4 CHAINT
#include "bmacrod.h"
C --------------------------------------------------------------
#include "bmacro.h"
C ---------------------------------------------------------------------
       VINIVG = VDERR
C
C Bank VUEC, disconnected channels
       nami = NAMIND('VUEC')
       KVUEC = IW(nami)
       IF(KVUEC .EQ. 0)THEN
         GOTO 999
       ENDIF
C
       vuech(1)  = ITABL(kvuec,1,1)
       vuech(2)  = ITABL(kvuec,1,2)
C
C Bank VXCH, extra readout channels
       nami = NAMIND('VXCH')
       KVXCH = IW(nami)
       IF(KVXCH .EQ. 0)THEN
         GOTO 999
       ENDIF
C
       vexch(1)  = ITABL(kvxch,1,1)
       vexch(2)  = ITABL(kvxch,1,2)
C --------------------------------------
C Flag to ignore bonding maps
C    VIGBM=1 ignore bonding map
       VIGBM = 0
       nami = NAMIND('VUBE')
       KVUBE = IW(nami)
C
       IF(KVUBE .EQ. 0)THEN
       ELSE
          VIGBM  = ITABL(KVUBE,1,1)
       ENDIF
C notify that bond map is not being used
       IF (VIGBM.EQ.1 .AND. IW(6).GT.0) THEN
         WRITE(IW(6),*)' VINIVG: bond map ignored'
       ENDIF
C
C ----------------------------------------------------------
C Bank VFMC, face module content
       nami = NAMIND('VFMC')
       KVFMC = IW(nami)
       IF(KVFMC .EQ. 0)THEN
         GOTO 999
       ENDIF
C
       nrows = LROWS(kvfmc)
       DO i = 1, nrows
        facec(i)  = CHAINT(ITABL(kvfmc,i,1))
        facen(i)  = ITABL(kvfmc,i,2)
        modneg(i) = ITABL(kvfmc,i,3)
        modpos(i) = ITABL(kvfmc,i,4)
      ENDDO
C
      VINIVG = VDOK

C error return
 999  CONTINUE
      RETURN
      END
#endif
