      SUBROUTINE BOSIO

*     PRINT STATUS OF IO

#include "boscom.h"
      REAL RW(1000)
      EQUIVALENCE (IW(1),RW(1))
      INTEGER IPA(6)
#include "param.h"
*
*     BANK +BUF LUN
*                                   'FORT'      'EPIO'       'TEXT'
*      1   packed information  see    0+          10+          20+
*                                   + 0 (NEUTRAL) + 1 (READ) + 2 (WRITE)
*      2   blocksize                 WORDS      16BIT WORDS   BYTES
*      3   NR OF IO ERRORS
*      4   nr of blocks (read/write blocks)/ last record
*      5   nr of segments within record
*      6   nr of BOS records
*      7   MAX NR OF WORDS/EVENT
*      8   MBYTES
*      9   IEFMT-FLAG record pointer
*     10   pointer within record (if zero, no record present)

      CHARACTER*24 TMODE
      CHARACTER*9  TBUFF
*     ...
      IF(IW(LUP).EQ.0) GOTO 100
      IBF=NAMIND('+BUF')+1
      IF(IW(IBF-1).EQ.0) GOTO 100
*     loop on all non-DAF files
      IPFL=0
   10 IBF=IW(IBF-1)
      IF(IBF.EQ.0) GOTO 20
#include "unpackio.h"
      IF(IOMD.EQ.7) GOTO 10
      IF(IPFL.EQ.0) WRITE(IW(LUP),1001)
      IPFL=1
      TMODE=' '
      IF(IOMD.EQ.0) THEN
*        fortran
         TMODE='FORT'
         TBUFF='WORDS'
         IF(IOST.EQ.1) THEN
            TMODE(10:14)='read '
            IF(IW(IBF+11).EQ.0) TMODE(16:24)='endfile'
         END IF
         IF(IOST.EQ.2) THEN
            TMODE(10:14)='write'
            IF(IW(IBF+11).EQ.0) TMODE(16:24)='buff out'
         END IF
      END IF
      IF(IOMD.EQ.1) THEN
*        epio
         TMODE='EPIO'
         TBUFF='16bit wds'
      END IF
      IF(IOMD.EQ.2) THEN
*        text
         TMODE='TEXT'
         TBUFF='char/line'
      END IF
      IF(IOST.EQ.1) TMODE(10:14)='read '
      IF(IOST.EQ.2) TMODE(10:14)='write'
      IF(IOST.EQ.3) TMODE(16:24)='rewind'

      WRITE(IW(LUP),1002) IW(IBF-2),TMODE,IW(IBF+2),TBUFF,
     1   (IW(IBF+I),I=3,7),RW(IBF+8)
      GOTO 10
   20 WRITE(IW(LUP),1002)
*     DAF files
      IPFL=0
      IBF=NAMIND('+BUF')+1
   30 IBF=IW(IBF-1)
      IF(IBF.EQ.0) GOTO 40
#include "unpackio.h"
      IF(IOMD.NE.7) GOTO 30
      IF(IPFL.EQ.0) WRITE(IW(LUP),1003)
      IPFL=1
      IPA(1)=IW(IBF+11)
      IPA(2)=IW(IBF+12)
      IPA(3)=3
      IPA(4)=IW(IBF+12)-IW(IBF+14)+1
      IPA(5)=IW(IBF+13)-3
      IPA(6)=IW(IBF+14)-IW(IBF+13)-1
      WRITE(IW(LUP),1004) IW(IBF-2),IPA,IW(IBF+16),IW(IBF+17),
     + (IW(IBF+I),I=3,6)
      GOTO 30
   40 WRITE(IW(LUP),1004)

  100 RETURN
 1001 FORMAT('0BOS++BOSIO+  Status of input/output'/
     1   '0unit',3X,'          ',18X,'buffer-size',7X,
     1   '  errors    blocks  segments  BOS-recs ',
     2   ' max wrds    Mbytes')
 1002 FORMAT(1X,I4,3X,A24,I10,1X,A9,5I10,F10.3)
 1003 FORMAT('0unit',3X,'direct access',13X,'number of records',14X,
     1   'date',14X,'IO-opeartions'/
     2   26X,' total   sys   dir  data  free',4X,'instal   modif',
     3   6X,'read',11X,'write')
 1004 FORMAT(1X,I4,3X,'recl=',I5,' words',2X,5I6,2X,2I8,2X,
     + I8,' +',I6,I8,' +',I6)
      END
