      SUBROUTINE BBOSF(JW,LUN,IBF,LIST)

*     write set of format banks using FORTRAN write
C     FLR - 960219
C     if the data card COMP 'NOFM' is present
C     do not write but RETURN

#include "boscom.h"
#include "param.h"
#include "mparam.h"
      CHARACTER*(*) LIST, NAME*4
      INTEGER JW(*)
      REAL RW(1000)
      EQUIVALENCE (IW(1),RW(1))
C
C - FLR corrections - 960219 =========================
      CHARACTER*4 CHAINT
      INTEGER NACOMP /0/
      LOGICAL NOFMFL /.FALSE./
      IF (NOFMFL) RETURN
      IF (NACOMP.EQ.0) THEN
        NACOMP = NAMIND('COMP')
        JCOMP = IW(NACOMP)
        IF (JCOMP.GT.0) THEN
          DO I=1,IW(JCOMP)
            IF (CHAINT(IW(JCOMP+I)).EQ.'NOFM')THEN
              NOFMFL=.TRUE.
              GOTO 100
            ENDIF
          ENDDO
        ENDIF
      ENDIF
C - End of FLR corrections - 960219 ==================
C
      IC=0
      ISTART=0
      ICOUNT=0
      IBH=0
#include "unpackio.h"
*
*     loop on all format (work) banks of this list
*
#include "listnn.h"
      IF(NAMI.EQ.0) GOTO 80
      ID=IW(IDFMT+NAMI-NSYST)
      IF(ID.EQ.0) GOTO 3
      ISTART=ID-3
      ICOUNT=IW(ID)+4

*     add bank to buffer

      NTOT=IW(IBF+11)
   20 NDIF=IW(IBF+2)-1-NTOT
      NBH=5
      IF(IBH.NE.0) NBH=0

      IF(NDIF.LE.NBH) THEN
*        write buffer intermediately
         IF(IC.LE.1) IC=IC+1
         IW(IBH+ 2)=IC
         IW(IBF+11)=NTOT
         CALL BBFWRS(LUN,IOAC,IW(IBF+4)+1,IW(IBF+11),IW(IBF+2))
*        statistic
         IW(IBF+ 4)=IW(IBF+4)+1
         RW(IBF+ 8)=RW(IBF+8)+1.0E-6*FLOAT(NBYTWD*(IW(IBF+11)+1))
         NTOT=0
         IBH=0
         GOTO 20
      END IF

      NW=MIN(NDIF-NBH,ICOUNT)

*     add HIDD bank, if not yet present

      IF(IBH.EQ.0) THEN
*        counter within buffer
         IW(IBF+5)=IW(IBF+5)+1
         IW(IBF+NTOT+12)=INTCHA('HIDD')
         IW(IBF+NTOT+13)=1
         IW(IBF+NTOT+14)=0
         IW(IBF+NTOT+15)=1
         IBH=IBF+NTOT+14
         NTOT=NTOT+5
*        store record number
         IF(IW(IBF+9).EQ.0) IW(IBF+9)=1000*(IW(IBF+4)+1)+IW(IBF+5)
      END IF

*     add work bank to buffer

      IW(IBH+1)=IW(IBH+1)+NW
      IW(IBH+2)=0
      CALL UCOPY(IW(ISTART),IW(IBF+NTOT+12),NW)
      NTOT=NTOT+NW
      ISTART=ISTART+NW
      ICOUNT=ICOUNT-NW
      IF(ICOUNT.GT.0) GOTO 20
      IW(IBF+11)=NTOT
      GOTO 3
   80 CONTINUE
      IF(IBH.EQ.0) GOTO 100
*     IC
      IF(IC.NE.0) IC=3
      IW(IBH+2)=IC
  100 RETURN
      END
