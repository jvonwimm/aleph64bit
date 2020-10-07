      FUNCTION VDMSRL(U,W,IWAFID)
C!----------------------------------------------------------------
C!  Get the radiation lenght for a given point on a vdet face
CKEY VDET TRACK
C!
C!  Author         G.Taylor    29/9/92
C!
C!  Input : U       VDET local coordinaate in FACE reference frame
C!          W       VDET local coordinaate in FACE reference frame
C!          IWAFID  wafer identifier of the VDET module ( as VDCO)
C!
C!  Output : VDMSRL fraction of radiation lenght at that point
C!
C!----------------------------------------------------------------
#ifndef DOC
#include "bcs.h"
#include "vdrljj.h"
      INTEGER GTSTUP, ALGTDB
      DATA NVDRL,IROLD,IVDOLD/0,0,-9/
#include "bmacro.h"
C
C get the vdet material description from the database
C

      IF (NVDRL .EQ.0) NVDRL = NAMIND('VDRL')
      CALL ABRUEV (IRUN,IEVT)
      IF (IRUN.NE.IROLD) THEN
        IROLD = IRUN
        IVDSTP = GTSTUP ('VD',IRUN)
        IF (IVDSTP.NE.IVDOLD) THEN
           IVDOLD = IVDSTP
           IRET= ALGTDB(JUNIDB(0),'VDRL',-IVDSTP)
        ENDIF
      ENDIF
C
C - next entry
C
      JVDRL = IW(NVDRL)
      IF(JVDRL.LE.0) THEN
         CALL ALTELL('VDMSRL :  Needs a VDRL bank ',0,'RETURN')
         VDMSRL = 0.
         RETURN
      ENDIF
      CALL VADEWA(IWAFID,LAYER,IZ,IPHI,IVX)
      UFACE=U
      WFACE=W
      IF(LAYER.EQ.1) UFACE=-UFACE
      IF(WFACE.GT.0.) THEN
        WMODUL=WFACE
        UMODUL=UFACE
      ELSE
        WMODUL=-WFACE
        UMODUL=-UFACE
      ENDIF
      X=0.
      DO 10 I=1,LROWS(JVDRL)
        IF( ITABL(JVDRL,I,JVDRIG).EQ.1.OR.
     &     (ITABL(JVDRL,I,JVDRIG).EQ.3.AND.LAYER.EQ.1).OR.
     &     (ITABL(JVDRL,I,JVDRIG).EQ.4.AND.LAYER.EQ.2)) THEN
C a region of a face
          IF(UFACE.GE.RTABL(JVDRL,I,JVDRUL).AND.
     &      UFACE.LE.RTABL(JVDRL,I,JVDRUH).AND.
     &      WFACE.GE.RTABL(JVDRL,I,JVDRWL).AND.
     &      WFACE.LE.RTABL(JVDRL,I,JVDRWH)) X=X+RTABL(JVDRL,I,JVDRRL)
        ELSE IF( ITABL(JVDRL,I,JVDRIG).EQ.2.OR.
     &     (ITABL(JVDRL,I,JVDRIG).EQ.5.AND.LAYER.EQ.1).OR.
     &     (ITABL(JVDRL,I,JVDRIG).EQ.6.AND.LAYER.EQ.2)) THEN
C a region of a module
          IF(UMODUL.GE.RTABL(JVDRL,I,JVDRUL).AND.
     &      UMODUL.LE.RTABL(JVDRL,I,JVDRUH).AND.
     &      WMODUL.GE.RTABL(JVDRL,I,JVDRWL).AND.
     &      WMODUL.LE.RTABL(JVDRL,I,JVDRWH)) X=X+RTABL(JVDRL,I,JVDRRL)
        ENDIF
   10 CONTINUE
      VDMSRL=X
      RETURN
      END
#endif
