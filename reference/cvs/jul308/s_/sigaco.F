      REAL FUNCTION SIGACO(IE,IM,IS,IP,IR)
C-------------------------------------------------------------
C!  - Return PAD ENERGY corrected for calibration
C!
C!    Author B.Bloch-Devaux JUNE 92
C!    Input:  IE  pad energy in Mev ( integer)
C!            IM  Module ( 1 or 2)
C!            IS  Layer z number ( 1 to 12)
C!            IP  Phi bin number ( 1 to 32)
C!            IR  Rad bin number ( 1 to 16)
C!    Output: RE  pad energy in Mev after calibration correction( real)
C!    BANKS:
C!       INPUT  : SCAX , SCGA Gain calibration constants
C!======================================================================
#ifndef DOC
      CHARACTER*4 KEY,CHAINT
      DIMENSION COE(3)
#include "bcs.h"
#include "scaxjj.h"
#include "scgajj.h"
      DATA IFIR/0/
#include "bmacro.h"
      IF (IFIR.EQ.0) THEN
        IFIR = 1
        NASCGA = NAMIND ('SCGA')
        NASCAX = NAMIND ('SCAX')
      NASRCO = NAMIND ('SRCO')
      JSRCO = IW ( NASRCO)
      IF ( JSRCO.GT.0 ) THEN
        DO  10 I= 1,LROWS(JSRCO)
          KEY = CHAINT(ITABL(JSRCO,I,1))
          IF (KEY.EQ.'CALI') SICALI = ITABL(JSRCO,I,2)
   10 CONTINUE
      ELSE
        SICALI = 5.
      ENDIF
      ENDIF
      KSCGA = IW(NASCGA)
      KSCAX = IW(NASCAX)
      X1 = 0.
C   protect against negativ or 0 energies ==> 0.
      IF ( IE.LE.0) GO TO 999
      IF ( KSCGA.LE.0) GO TO 999
      IF ( KSCAX.LE.0) GO TO 999
      CORR = RTABL(KSCAX,1,IR)
      IROW = (IM-1)* 384 +(IP-1)*12 +IS
      COE(1) = RTABL(KSCGA,IROW,JSCGFA+2)
      COE(2) = RTABL(KSCGA,IROW,JSCGFA+1)
      IF ( COE(2).EQ.0. .AND. COE(1).EQ.0.) THEN
        X1 = 0.
      ELSE
      C1 = RTABL(KSCGA,IROW,JSCGFA)
      IF(C1.GT.0.) C1=0.
C   There is SICALI Mev per ADC count in SIDI
      COE(3) = C1              -IE/(SICALI*(1.+CORR)*
     $   (RTABL(KSCGA,IROW,JSCGFG)+RTABL(KSCGA,IROW,JSCGFG+1)*(IR-1)))
        CALL SISOL2(COE,X1,X2,IOK)
        IF (IOK . EQ .2 ) X1 = 0.
      ENDIF
      X1 = X1*SICALI
  999 SIGACO = X1
      RETURN
      END
#endif
