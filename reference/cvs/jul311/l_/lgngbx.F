      SUBROUTINE LGNGBX(NGBX)
C-----------------------------------------------------------------------
C! Extracts the number of GBX's since the last ALEPH readout.
C  Author :  J.Wear  89/11/02
C  Modified :  P.H.Hansen  89/11/20
C  Modified :  P.H.Hansen  960501 Use X1RG bank
C  Input : none
C  Output: NGBX
C           = number of gated bunch crossings since the last readout
C             divided by 1024
C           = 100 if X1SC bank doesn't exist AND
C             4534 < run < 5314 (XTCN wrong)
C
C?
C!======================================================================
#ifndef DOC
#include "bcs.h"
#include "rcurnt.h"
      CHARACTER*4 CHAINT
      EXTERNAL NAMIND
#include "bmacro.h"
C-----------------------------------------------------------------------
      NGBX  = 100
      IF ( FMCRUN ) GOTO 999
C
      KX1SC = IW( NAMIND('X1SC') )
      KXTCN = IW( NAMIND('XTCN') )
      KX1RG = IW( NAMIND('X1RG') )
C
C Count number of GBX's since the last aleph readout (scaled by 1024)
      IF(KX1RG.NE.0) THEN
        DO 2 IROW=1,LROWS(KX1RG)
          IF(CHAINT(ITABL(KX1RG,IROW,1)).EQ.'TSR ') THEN
             NGBX = ITABL(KX1RG,IROW,2)/1024
             GOTO 3
          ENDIF
    2   CONTINUE
    3   CONTINUE
      ELSEIF(IRUNRC.GE.4535.AND.IRUNRC.LT.5314) THEN
        IF( KX1SC .NE. 0 )
     &    NGBX = ITABL(KX1SC,27,3)/65536
      ELSEIF(KXTCN.NE.0.AND.(IRUNRC.LE.4534.OR.IRUNRC.GE.5314)) THEN
        NGBX = IW(KXTCN+LMHLEN+3)/1024
      ELSE
        CALL RERROR ('LGNGBX',1,'GBX count not available')
      ENDIF
C
  999 RETURN
      END
#endif
