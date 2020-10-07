      INTEGER FUNCTION VRECOR (IVIEW,ILAY,IFAC,IWFF,RSTRP,DSPLC)
C ----------------------------------------------------------------------
CKEY VDETDES TRANSFORM STRIP / USER
C!  Additional coordinate displacement;given view,layer,face,wafer,strip
C - Joe Rothberg, 20 July 1998
C
C     Returns displacement of local wafer coordinate (cm)
C        fault types 301, 302    
C
C - Input:
C   IVIEW  / I  View number (=1 for z, =2 for r-phi)
C   ILAY   / I  layer number
C   IFAC   / I  face number (in layer)
C   IWFF   / I  wafer number (wafer in face)
C   RSTRP  / R  readout strip number (floating number)
C
C - Output:
C   DSPLC  / R  displacement (cm)  in local wafer coordinates
C ----------------------------------------------------------------------
#ifndef DOC
      IMPLICIT NONE
C ----------------------------------------------------
#include "vglobl.h"
C!    Parameters for VDET geometry package
C ----------------------------------------------------------------------
#include "vbnder.h"
C --------------------------------------------------------------
C
C     Arguments:
      REAL  RSTRP, DSPLC
      INTEGER ILAY, IFAC, IVIEW, IWFF
C
C     Local variables
      INTEGER JMOD, IMOD, IWAF
      INTEGER  ivfault, ivbond, ivdisp, ie, isign
      INTEGER iros, irom, isch, iech
      INTEGER ich1, ich2
C centimeters per micron
      REAL cmmic/0.0001/
C
      INTEGER STATUS/1/
C
C  functions
      INTEGER  VFWAFF, VJMODI, VDRSSC, VDSCEC
      EXTERNAL VFWAFF, VJMODI, VDRSSC, VDSCEC
C
C ---------------------------------------------------------------
      DSPLC = 0.
C   convert to JMOD, IWAF
      status = VFWAFF(IWFF,imod,iwaf)
      IF(status .NE. VDOK) GOTO 900
      status = VJMODI(ILAY,IFAC,IMOD,jmod) 
      IF(status .NE. VDOK) GOTO 900
C   convert readout strip to electronics channel
      IROS = rstrp
      status = VDRSSC(IMOD,IWAF,IVIEW,IROS,irom,isch)
      IF(status .NE. VDOK) GOTO 900
      status = VDSCEC(IVIEW,ISCH,iech)
      IF(status .NE. VDOK) GOTO 900
C ---------------------------------------------------------------
C   search error list
      DO ie = 1, numerr(JMOD,iview)
         ivfault= IBNERR(JMOD,iview,ie,4)
         IF(ivfault .EQ. 301 .OR. ivfault .EQ. 302) THEN
C             sign of displacement
           isign =  2*(ivfault-301)-1
           ich1  = IBNERR(JMOD,iview,ie,1)
           ich2  = IBNERR(JMOD,iview,ie,2)
           ivbond = IBNERR(JMOD,iview,ie,3)
C             displacement in microns
           ivdisp = IBNERR(JMOD,iview,ie,5)
C ---------------------------------------------------
C  debug printout
C           WRITE(6,'(1X,8I6 )')JMOD,iview,
C     >   ivfault,isign,ich1,ich2,ivbond,ivdisp
C ---------------------------------------------------
C   rphi view
           IF(iview .EQ. 2) THEN
C             strip range
             IF(iech .GE. ich1 .AND. iech .LE. ich2) THEN
C                  bond 1 is at hybrid (IWAF=3)
                IF(IWAF .LE. 4-ivbond) THEN

                   DSPLC =  isign*ivdisp*cmmic
C -------------------------------------------------
C                 bond number
                ENDIF
C ------------------------------------------
C              strip range
             ENDIF
C -----------------------------------------------------------
           ELSEIF (iview .EQ. 1) THEN
C                z view
           ENDIF
C -----------------------------------------------------------
C          fault number
         ENDIF
C ----------------------------------------------------------------------
C       error list
      ENDDO
C ----------------------------------------------------------------------
 900  CONTINUE
      VRECOR =  STATUS
      RETURN
      END

C-----------------------------------------------------------------------
#endif
