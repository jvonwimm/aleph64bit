      SUBROUTINE TINIRU
C
C-------------------------------------------------------------------
C! Initialize run for the TPC
C!
C!    Author:   R. Johnson   15-04-87
C!    Modified: R. Johnson    8-04-88
C!
C!    Description
C!    -----------
C!    At each run beginning, this routine fills common blocks with
C!    constants need for TPC reconstruction.
C!
C!------------------------------------------------------------------
#ifndef DOC
C
#include "rparac.h"
#include "rflags.h"
#include "tflags.h"
#include "rlunit.h"
#include "rcurnt.h"
#include "bcs.h"
C
      CHARACTER*4 TRGTY,CHAINT
      CHARACTER*110 TEXT
      PARAMETER (JRLTTY=7, JRLRTY=9)
C
C++   Determine whether this run is from TPC90 or the full TPC
C
      FTPC90=.FALSE.
      KRUNH=IW(NAMIND('RUNH'))
      IF (KRUNH.NE.0) THEN
        IF (IW(KRUNH).GT.JRLTTY) THEN
          TRGTY= CHAINT(IBITS(IW(KRUNH+JRLRTY),0,32))
          IF (TRGTY.EQ.'TPC9') FTPC90=.TRUE.
        ENDIF
      ENDIF
C
C++   Get run constants from direct access files or the input tape
C
      CALL TRNCON
C
C++   Initialize pad-response function
C
      CALL TPRFIN
C
C++   Get geometry of TPC or TPC90 for this run
C
      IF (FTPC90) THEN
        CALL TRDT90
      ELSE
C
C++     Fill the geometry common blocks
C
        CALL TRDDAF(LRCONS,IRUNRC,IRET)
        IF (IRET.EQ.0) THEN
C
C++       At least one bank is missing: cannot continue the run
C
          CALL REPORT('TINIRU','Cannot find TPC geometry constants',1)
          RETURN
        ELSE
C
C++       Geometry printout if required
C
          IF (FDEBRF) THEN
            IF (JDBDRF(JULTP).GE.4)  CALL TGEDMP(LDEBRL)
            IF (JDBDRF(JULTP).GE.21) CALL TGEPBK
          ENDIF
        ENDIF
      ENDIF
C
C++   Get constants for field corrections
C
      IF (FFCORR) THEN
        CALL TFCINI(LOUTRL,LRCONS,-IRUNRC,IRET)
        IF (IRET.NE.0) THEN
          FFCORR=.FALSE.
          WRITE(TEXT,34) IRET
   34     FORMAT('Return code ',I2,' from TFCINI---field correction',
     &           ' CONSTANTS WERE NOT READ&AND COORDINATES WILL NOT',
     &           ' be corrected')
          CALL REPORT('TINIRU',TEXT(1:107),1)
        ENDIF
      ENDIF
C
C++   Build a data structure for a list of bad TPC pads
C
      CALL TPDTHR
C
C++   Make bank with packing constants
C
      CALL TFILTJ(IRET)
      IF (IRET.NE.0) THEN
         CALL RERROR ('TINIRU',-1,'Cannot create PTUN bank for packing')
      ENDIF
C
      RETURN
      END
#endif
