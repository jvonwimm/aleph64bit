      SUBROUTINE BOSDP(JW)
      INTEGER JW(*)
      CHARACTER*4 CHAINT
#include "boscom.h"
#include "param.h"
      INTEGER LIM(5),ICOM(20)
      CHARACTER*24 TEXT(5)
      CHARACTER*5 TSYS(13)
      CHARACTER*5 TARR(43)
      EQUIVALENCE (ICOM(1),NSYST)

      DATA TEXT/
     1     'SYSTEM PARAMETER        ',
     2     'INDICES FOR NAMES       ',
     3     'NAMED BANKS             ',
     4     'GAP                     ',
     5     'WORK BANKS              '/
      DATA TSYS/'NSYST','NAMES','NPRIM','IDNAM','IDPTR',
     1          'IDFMT','     ','NRESR','NLPLM',' NARR',
     2          'ARRAY','ADDR.','.....'/
      DATA TARR/'ADARG','RETCD','     ','     ','CUNIT',
     1          'PUNIT','PRETC','PBNKS','ARNM1','ARNM2',
     2          'ARNUM','ARLEN','ARADD','INAMA','IGAPA',
     3          'IWRKA','NDELN','NDELW','IDELN','IDELW',
     4          'I+LNK','ILSTC','ILSTE','ILSTR','ILSTS',
     5          'ILSTT','ILSTU','     ','IIOST','CHECK',
     6          'SRET1','SRET2','SRET3','SRET4','SRET5',
     7          'SRET6','SRET7','SRET8','NGACN','NGACW',
     8          'HISTO','.....','.....'/
C     ...
      IF(JW(ICK).NE.12345) CALL BBNOT(JW,'BOSDP')
      IF(JW(LUP).EQ.0) GOTO 100
      WRITE(JW(LUP),1001)  JW(NJA),CHAINT(JW(IAR+1)),CHAINT(JW(IAR+2))
C
C     COMMON/SYSBOS/
C
      WRITE(JW(LUP),1002)  TSYS
      CALL BBUFM(NSYST,1,20,JW(LUP))
C
C     AREAS OF ARRAY JW
C
      LIM(1)=NSYST
      LIM(2)=JW(INM)-1
      LIM(3)=JW(IGP)-1
      LIM(4)=JW(IWK)-1
      LIM(5)=JW(NJW)
      NB=0
      DO 10 I=1,5
         IF(I.EQ.2) THEN
            WRITE(JW(LUP),1003)
            CALL BBUFM(IW(IDNAM+1),1,IW(IDNAM),JW(LUP))
         END IF
         NA=NB+1
         NB=LIM(I)
         WRITE(JW(LUP),1004) NA,NB,TEXT(I)
         IF(I.EQ.1) THEN
            WRITE(JW(LUP),1002) TARR
         END IF
         IF(I.NE.4) THEN
            CALL BBUFM(JW,NA,NB,JW(LUP))
         END IF
   10 CONTINUE
C
  100 RETURN
 1001 FORMAT('0BOS++BOSDP+  ARRAY NR',I3,5X,2A4/
     1       '0-----------  COMMON/SYSCOM/'/)
 1002 FORMAT(1(11X,10(7X,A5),2X))
 1003 FORMAT('0-----------  LIST OF NAMES'/)
 1004 FORMAT('0-----------  AREA',I7,' -',I10,10X,A24/)
      END
