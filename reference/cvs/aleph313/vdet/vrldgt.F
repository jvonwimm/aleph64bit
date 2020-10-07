      SUBROUTINE VRLDGT( IER)
C!-------------------------------------------------------------------
C! fill /VRLDCOM/ from VRLD data base bank
C! Extract the parameters dealing with the effects of multiple
C! scattering from the data base bank VRLD
CKEY VDET TRACK
C!
C!    AUTHOR: G. Taylor   22/6/95
C!
C!    OUTPUT: See description of VRLD bank for details
C!            IER= 0  ok
C!            IER=-1 database bank not found
C!-------------------------------------------------------------------
#ifndef DOC
#include "bcs.h"
C --
#include "vrldjj.h"
#include "vrldcom.h"
C --
      INTEGER GTSTUP, ALGTDB
      INTEGER NVDRL,IROLD,IVDOLD
      DATA NVDRL,IROLD,IVDOLD/0,0,-9/
#include "bmacro.h"
C
C ----------------------------------------------------------------
C-
C get the multiple scattering  material description from the database
C-
      IER=0
      IF (NVRLD .EQ.0) NVRLD = NAMIND('VRLD')
      CALL ABRUEV (IRUN,IEVT)
C
      IF (IRUN.NE.IROLD) THEN
       IROLD = IRUN
       IVDSTP = GTSTUP ('VD',IRUN)
C
       IF (IVDSTP.NE.IVDOLD) THEN
        IVDOLD = IVDSTP
        IRET= ALGTDB(JUNIDB(0),'VRLD',-IVDSTP)
        JVRLD = IW(NVRLD)
C
        IF(JVRLD.LE.0) THEN
         CALL ALTELL('VRLDGT:  Needs a VRLD bank - JOB SHOULD STOP',0,
     &               'RETURN')
         IER=-1
        ELSE
C
C beam pipe
C
          UKRVAC     = RTABL(JVRLD,1,JVRLBR)
          UKSVAC     = RTABL(JVRLD,1,JVRLBL)
C
C ITC/TPC part
C
          UKRITC     = RTABL(JVRLD,1,JVRLIR)
          UKSITC     = RTABL(JVRLD,1,JVRLIL)
          UKSPITC    = RTABL(JVRLD,1,JVRLIG)
          UKRTPC     = RTABL(JVRLD,1,JVRLIT)
          UKSTPC     = RTABL(JVRLD,1,JVRLTL)
          UKSPTPC    = RTABL(JVRLD,1,JVRLTG)
C
C vdet
C
          UKZICA     = RTABL(JVRLD,1,JVRLZI)
          UKZOCA     = RTABL(JVRLD,1,JVRLZO)
          UKRIICA    = RTABL(JVRLD,1,JVRLII)
          UKRIOCA    = RTABL(JVRLD,1,JVRLIO)
          UKROICA    = RTABL(JVRLD,1,JVRLOI)
          UKROOCA    = RTABL(JVRLD,1,JVRLOO)
          UKSICA     = RTABL(JVRLD,1,JVRLSI)
          UKSOCA     = RTABL(JVRLD,1,JVRLSO)
        ENDIF
       ENDIF
      ENDIF
C
      END
#endif
