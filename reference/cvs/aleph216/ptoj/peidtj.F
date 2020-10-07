      SUBROUTINE PEIDTJ (LIST,IER)
C
C----------------------------------------------------------------------
C! Unpack  PEID bank into EIDT bank
C!
C!    Author:  D. PALLIN - 881129
C
C     Input :   LIST      /C    BOS event list
C                               if LIST(2:2).eq.'-' drop POT banks
C!
C!    Output:   IER       /I    Error return=0 if operation successful
C                                           -1 if OK but garbage coll.
C                                            2 if not enough space
C                                            1 POT bank missing
C!
C!    Input bank  : PEID
C!    Output bank : EIDT
C!
#ifndef DOC
      SAVE
#include "bcs.h"
#include "eidtjj.h"
#include "peidjj.h"
      CHARACTER*(*) LIST, PLIST*4, JLIST*4
      PARAMETER(RAT=100./65534.)
      DATA NEIDT,NPEID/0,0/
#include "bmacro.h"
C?
C?
      IF(NEIDT.EQ.0)THEN
        NEIDT=NAMIND('EIDT')
        NPEID=NAMIND('PEID')
        CALL BKFMT('EIDT','2I,(I,8F,I,3F,2I)')
      ENDIF
C?
C - bank PEID exist ?
C?
      JPEID=IW(NPEID)
      IER = 1
      IF (JPEID.EQ.0) GOTO 999
      IF (LROWS(JPEID).EQ.0) GOTO 999
C
C  create EIDT bank
C?
      LEN=LROWS(JPEID)*LEIDTA+LMHLEN
      CALL AUBOS('EIDT',0,LEN,JEIDT,IER)
      IF (IER.EQ.2) GOTO 999
      JLIST = 'EIDT'
      JPEID=IW(NPEID)
C?
C  fill  EIDT bank
C?
      IW(JEIDT+LMHCOL)=LEIDTA
      IW(JEIDT+LMHROW)=LROWS(JPEID)
      DO 1 NN=1,LROWS(JPEID)
         KPEID = KROW(JPEID,NN)
         KEIDT = KROW(JEIDT,NN)
C
      IFLIPP=IW(KPEID+JPEIIF)
      IF(IFLIPP.GE.10)THEN
         IPP=1
      ELSE
         IPP=0
      ENDIF
      IW(KEIDT+JEIDIP)=IPP
      IW(KEIDT+JEIDIF)=IFLIPP-IPP*10
C
      IBN=IW(KPEID+JPEIR1)
      RR1=0
      IF(IBN.EQ.128)RR1=1000.
      IF(ABS(IBN).EQ.127)RR1=999.*SIGN(1.,FLOAT(IBN))
      IF(ABS(RR1).LT.998 )RR1=FLOAT(IBN)/10.
      RW(KEIDT+JEIDR1)=RR1
C
      IBN=IW(KPEID+JPEIR2)
      RR2=0
      IF(IBN.EQ.128)RR2=1000.
      IF(ABS(IBN).EQ.127)RR2=999.*SIGN(1.,FLOAT(IBN))
      IF(ABS(RR2).LT.998 )RR2=FLOAT(IBN)/10.
      RW(KEIDT+JEIDR2)=RR2
C
      IBN=IW(KPEID+JPEIR3)
      RR3=0
      IF(IBN.EQ.128)RR3=1000.
      IF(ABS(IBN).EQ.127)RR3=999.*SIGN(1.,FLOAT(IBN))
      IF(ABS(RR3).LT.998 )RR3=FLOAT(IBN)/10.
      RW(KEIDT+JEIDR3)=RR3
C
      IBN=IW(KPEID+JPEIR4)
      RR4=0
      IF(IBN.EQ.128)RR4=1000.
      IF(ABS(IBN).EQ.127)RR4=999.*SIGN(1.,FLOAT(IBN))
      IF(ABS(RR4).LT.998 )RR4=FLOAT(IBN)/10.
      RW(KEIDT+JEIDR4)=RR4
C
      IBN=IW(KPEID+JPEIR6)
      RR6=0
      IF(IBN.EQ.128)RR6=1000.
      IF(ABS(IBN).EQ.127)RR6=999.*SIGN(1.,FLOAT(IBN))
      IF(ABS(RR6).LT.998 )RR6=FLOAT(IBN)/10.
      RW(KEIDT+JEIDR6)=RR6
C
      IBN=IW(KPEID+JPEIR7)
      RR7=0
      IF(IBN.EQ.128)RR7=1000.
      IF(ABS(IBN).EQ.127)RR7=999.*SIGN(1.,FLOAT(IBN))
      IF(ABS(RR7).LT.998 )RR7=FLOAT(IBN)/10.
      RW(KEIDT+JEIDR7)=RR7
C
      IBN=IW(KPEID+JPEIEC)
      IF(IBN.EQ.65535)ECC=1000.
      ECC=RAT*IBN
      RW(KEIDT+JEIDEC)=ECC
C
      IBN=IW(KPEID+JPEIET)
      IF(IBN.EQ.65535)ETT=1000.
      ETT=RAT*IBN
      IBN=IW(KPEID+JPEIP1)
      EP1=FLOAT(IBN)/255.
      IBN=IW(KPEID+JPEIP2)
      EP2=FLOAT(IBN)/255.
      RW(KEIDT+JEIDE1)=EP1*ETT
      RW(KEIDT+JEIDE2)=EP2*ETT
      RW(KEIDT+JEIDE3)=(1.-(EP1+EP2))*ETT
C
      IW(KEIDT+JEIDFR)=IW(KPEID+JPEIPF)
C
 1    CONTINUE
C?
 998  CONTINUE
C - get the drop flag if any, then drop POT banks if required,
C   add JUL banks to S-list
C   POT banks are on PLIST, JUL banks on JLIST
      PLIST = 'PEID'
#include "jplist.h"
C
      IF (IER .EQ. 1) IER = -1
C?
  999 CONTINUE
      RETURN
      END
#endif
