      SUBROUTINE SILHCR(IER)
C----------------------------------------------------------------------
C!  - Create and fill the SILH banks
C!
C!    Author:     B. Bloch-Devaux   10-Feb-1993
C!     Adapt to new trigger banks - April 29 , 1994
C      Allow reprocessing POT to POT    October 1994   B. Bloch
C!              Modified for Bunch train mode October 1994
C!
C!    Input:      - banks 'SIX2'                     from Sical
C!                        'LOLE'                     from Lcal
C!                        'XTBP','XTCN','XTOP','X1SC'from Trigger
C!                        'XTBP','XTCN','X1TT' 'X1HI' from 1994 onwards
C!                  from data base:
C!    Output:     - IER       /I    error flag:
C!                                  0 = no error
C!                                  1 = no space for new bank
C!                - bank  'SILH'
C?
C!======================================================================
#ifndef DOC
#include "alcons.h"
#include "bcs.h"
#include "silhjj.h"
#include "schujj.h"
#include "lolejj.h"
#include "rcurnt.h"
#include "rflags.h"
#include "rlunit.h"
#include "x1namc.h"
#include "xtcnjj.h"
#include "xtopjj.h"
#include "x1scjj.h"
#include "sinamc.h"
#include "x1ttjj.h"
#include "x1hijj.h"
      PARAMETER(JXTBTT=1,JXTBCL=3,JXTBT1=4,JXTBT2=5,JXTBGC=6,JXTBLL=7,
     +          JXTBHT=8,JXTBHW=17,JXTBET=26,JXTBEW=35,JXTBLT=44,
     +          JXTBTE=48,JXTBIT=50,JXTBTP=53,JXTBTR=56,LXTBPA=58)

      INTEGER ALTRIG
      EXTERNAL ALTRIG
      PARAMETER ( MSKHVB = 14811135)
      PARAMETER ( MSKTEN =-2004933636)
      CHARACTER*4 RNAME,CHAINT
      INTEGER ISCAL(3)
      DATA ICALL /0/
#include "bmacro.h"
C----------------------------------------------------------------------
C initialization
      IF (ICALL .EQ. 0) THEN
        NLOLE = NAMIND('LOLE')
        NXTOP = NAMIND('XTOP')
        NX1TT = NAMIND('X1TT')
        NX1HI = NAMIND('X1HI')
        IF (NAXTBP .LE.0 ) NAXTBP = NAMIND('XTBP')
        IF (NAXTCN .LE.0 ) NAXTCN = NAMIND('XTCN')
        IF (NAX1SC .LE.0 ) NAX1SC = NAMIND('X1SC')
        ICALL = 1
C  init integrated scalers counters
        CALL VZERO(ISCAL,3)
      ENDIF
C reset the error flag
      IER = 0
C   If reprocessing from POT to POT , SILH is fine just reset the
C   accepted method word
      IF ( FREPRO) THEN
         JSILH = IW(NASILH)
         IF (JSILH .GT.0) THEN
            KSILH = KROW(JSILH,1)
            IW ( KSILH+JSILAM ) = 0
         ENDIF
      ELSE
      IND = NDROP('SILH',0)
C create SILH bank
      CALL AUBOS('SILH',0,LMHLEN+LSILHA,JSILH,IGARB)
      IF (JSILH .EQ. 0 .OR. IGARB .EQ. 2)                   GOTO 902
      IW(JSILH+LMHCOL) = LSILHA
      IW(JSILH+LMHROW) = 1
C link to banks needed
      JLOLE = IW(NLOLE)
      JSIX2 = IW(NASIX2)
      JXTBP = IW(NAXTBP)
      JXTCN = IW(NAXTCN)
      JXTOP = IW(NXTOP)
      JX1SC = IW(NAX1SC)
      JX1TT = IW(NX1TT)
      JX1HI = IW(NX1HI)
C pointer to  SILH row
      KSILH = KROW(JSILH,1)
C GBX counter
      CALL LGNGBX(NGBX)
      IW(KSILH+JSILGB) = NGBX
C HV bit pattern
      IF (JXTCN .NE. 0) IW(KSILH+JSILHV) = ITABL(JXTCN,1,JXTCTR)
      IF (JXTCN .LE. 0) IW(KSILH+JSILHV) = MSKHVB
C trigger level 1 bit pattern
      IALTR = ALTRIG(IT1,IT2,IL2)
      JTRL1 = 0
      IF ( IALTR.GT.0 ) THEN
        JTRL1 = IT1
      ELSE IF (JXTBP .NE. 0) THEN
        JTRL1 = ITABL(JXTBP,1,JXTBT1)
      ENDIF
      IW(KSILH+JSILT1) = JTRL1
C trigger level 2 bit pattern
      IF ( IALTR.GT.0 ) IW(KSILH+JSILT2) = IL2
C trigger enable bit pattern
      JTREN = 0
      IF (JXTCN .NE. 0) JTREN = ITABL(JXTCN,1,JXTCTR+1)
      IF (JXTCN .LE. 0) JTREN = MSKTEN
      IW(KSILH+JSILTE) = JTREN
C LOLE information word
      IF (JLOLE .NE. 0) THEN
        IF (ITABL(JLOLE,1,JLOLER) .NE. 0) THEN
          DO 200 IL=1,11
            IF(ITABL(JLOLE,1,IL).NE.0) THEN
              IW(KSILH+JSILLO) = 1000*IL
              IF(IL.LE.5.OR.IL.GE.11) THEN
                IW(KSILH+JSILLO) = IW(KSILH+JSILLO)+ITABL(JLOLE,1,IL)
              ENDIF
            ENDIF
  200     CONTINUE
        ENDIF
      ELSE
        IW(KSILH+JSILLO) = -1
      ENDIF
C SIX2 information word
      IF (JSIX2 .NE. 0) THEN
        IW(KSILH+JSILS2) = ITABL(JSIX2,1,1)
      ENDIF
C downscale factors  for Sical triggers
      IF ( JXTOP.GT.0 ) THEN
        DO 300 I = 3,5
          IW(KSILH+JSILDW+I-3) =  ITABL(JXTOP,1,JXTODS+I)
  300   CONTINUE
      ELSEIF ( JX1TT.GT.0 ) THEN
        DO 301 I = 3,5
          IW(KSILH+JSILDW+I-3) =  ITABL(JX1TT,1,JX1TDS+I)
  301   CONTINUE
      ENDIF
C Scalers contents  for Sical triggers
      IF (JX1SC .NE. 0) THEN
        DO 400 J = 1,LROWS(JX1SC)
          RNAME = CHAINT(ITABL(JX1SC,J,JX1SIT))
C -- Bug fix
          IF(RNAME.EQ.'TRG ') RNAME = 'TRB '
          IF(RNAME.NE.'TRB ') GO TO 400
C SICAL scalers are 4th,5th,6th scalers,but packed in long words 2 and 3
          ITEST = ITABL(JX1SC,J,JX1SSV+1)
          ISCAL1 = ISHFT(ITEST,-16)
          ITEST = ITABL(JX1SC,J,JX1SSV+2)
          ISCAL2 = IAND(ITEST,65535)
          ISCAL3 = ISHFT(ITEST,-16)
          ISCAL(1) = ISCAL(1)+ISCAL1
          ISCAL(2) = ISCAL(2)+ISCAL2
          ISCAL(3) = ISCAL(3)+ISCAL3
 400    CONTINUE
      ELSEIF (JX1HI .NE. 0) THEN
        DO 401 J = 1,LROWS(JX1HI)
          RNAME = CHAINT(ITABL(JX1HI,J,JX1HNA))
          IF(RNAME.NE.'TPR ') GO TO 401
C SICAL scalers are 4th,5th,6th scalers,but packed in long words 2 and 3
          ITEST = ITABL(JX1HI,J,JX1HCO+1)
          ISCAL1 = ISHFT(ITEST,-16)
          ITEST = ITABL(JX1HI,J,JX1HCO+2)
          ISCAL2 = IAND(ITEST,65535)
          ISCAL3 = ISHFT(ITEST,-16)
          ISCAL(1) = ISCAL(1)+ISCAL1
          ISCAL(2) = ISCAL(2)+ISCAL2
          ISCAL(3) = ISCAL(3)+ISCAL3
 401    CONTINUE
      ENDIF
      IW(KSILH+JSILSC)   = ISCAL(1)
      IW(KSILH+JSILSC+1) = ISCAL(2)
      IW(KSILH+JSILSC+2) = ISCAL(3)
C Now append the wagon number ( or rather raw ADC info ) as 15th word
      IF ( LSILHA.GT.14) THEN
         KSCHU = IW(NASCHU)
         IWAG = 685
         IF ( KSCHU.GT.0) IWAG = ITABL(KSCHU,1,JSCHWI)
         IW(KSILH+JSILWI) = IWAG
      ENDIF
C bank filled
      ENDIF
      GOTO 999
C----------------------------------------------------------------------
  902 WRITE (IW(6),'(A)') ' SILHCR --> NO SPACE FOR NEW BANK SILH'
      IER = 1
  999 RETURN
      END
#endif
