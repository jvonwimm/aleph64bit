       SUBROUTINE TPRLST(ISLOT,KTPAD,KTPDI,KTPRL,JGARB,IER)
C
C----------------------------------------------------------------------
C! Establish TPC rowlist
C!
C!  Author:    D. Schlatter
C!  Modified:  R. Johnson    10-05-88
C!
C!  INPUT:
C!       - ISLOT  /I     Sector number
C!       - KTPAD  /I     Offset for pad-hit bank
C!       - KTPDI  /I     Offset to pad digitization bank
C!  OUTPUT:
C!       - KTPAD  /I     (this may change if garbage collection is done)
C!       - KTPDI  /I     (this may change if garbage collection is done)
C!       - KTPRL  /I     Offset for rowlist bank
C!       - JGARB  /I     =1 if garbage collection done; =0 otherwise
C!       - IER    /I     Set to zero for good return,
C!                       and set to 1 if space is not available.
C!                       >9 means the input data format is wrong.
C!  Called from TPADS
C!
C!  Description
C!  ===========
C!     Define offsets into TPC raw data banks TPAD+TPDI
C!     for each padrow in a sector.
C!     Offsets and number of hits and samples per row are stored
C!     in the TPC-row-list bank TPRL.
C!     Several checks are made to verify that the input data are
C!     in the proper format.
C!--------------------------------------------------------------------
#ifndef DOC
C
#include "rparac.h"
#include "ptstjj.h"
#include "tparac.h"
#include "tpgpar.h"
#include "tprljj.h"
#include "bcs.h"
#include "tpgeom.h"
#include "tpgeop.h"
#include "tmonit.h"
C
      LOGICAL FIRST
      CHARACTER BUF*100
      DATA FIRST/.TRUE./
C
C---------------------- Function definitions ---------------------------
C
#include "bmacro.h"
C
C-----------------------------------------------------------------------
C
      IF (FIRST) THEN
        NPTST=NAMIND('PTST')
        FIRST=.FALSE.
      ENDIF
C
C++   Create the TPC Row List bank TPRL for this sector
C
      JGARB=0
      ISTYP=ITPTYP(ISLOT)
      LEN=LTPRLA*NTPDRW(ISTYP)+LMHLEN
      IW(1)=1
      CALL AUBOS('TPRL',ISLOT,LEN,KTPRL,IGARB)
      IF (IGARB.EQ.2) GO TO 998
      IER=0
      IF (IGARB.NE.0) THEN
        JGARB=1
        KTPAD=NLINK('TPAD',ISLOT)
        KTPDI=NLINK('TPDI',ISLOT)
      ENDIF
      CALL BLIST(IW,'T+','TPRL')
C
      CALL VZERO(IW(KTPRL+1),LEN)
      IW(KTPRL+LMHCOL)=LTPRLA
      IW(KTPRL+LMHROW)=NTPDRW(ISTYP)
C
C++   Upper limit on number of digitizations in the sector
C
      NDIG=4*IW(KTPDI)
C
C++   IOFRB points to the row list in TPAD of current interest.
C++   NSTOT accumulates the total number of samples.
C
      IOFRB=2
      NSTOT=0
      IRLST=0
      NRTOT=0
      NHTOT=0
C
C++   Begin loop over the rows included in TPAD
C
      LENTP=IW(KTPAD)
  100 IF (IOFRB.GT.LENTP) GO TO 120
        NHITR=IW(KTPAD+IOFRB)
        IROW=IBITS(IW(KTPAD+IOFRB-1),0,4)
        NRTOT=NRTOT+1
        IF (NHITR.LT.1 .OR. NHITR.GT.(LENTP-IOFRB)) THEN
          WRITE(BUF,1001) IROW,ISLOT,NHITR
 1001     FORMAT('Input data format error in row ',I2,' of sector',
     &           I2,':  number of hits=',I9,' is out of range.')
          CALL RERROR('TPRLST',20,BUF(1:89))
          NHORTM(ISLOT)=NHORTM(ISLOT)+1
C
C++       If the number of hits looks like rubbish, scan ahead in the
C++       bank and try to recognize the start of the next rowlist
C
          JPT=IOFRB
   45     IF (JPT.GT.LENTP-2) GO TO 55
            JPT=JPT+1
            JROW=IW(KTPAD+JPT)
            IF (JROW.LE.NTPDRW(ISTYP) .AND. JROW.GT.IROW) THEN
              MHT=IW(KTPAD+JPT+1)
              IF (MHT.GT.0 .AND. MHT.LE.IW(KTPAD)-JPT-1) THEN
                NHITR=JPT-IOFRB-1
                GO TO 56
              ENDIF
            ENDIF
            GO TO 45
   55     CONTINUE
          NHITR=IW(KTPAD)-IOFRB
   56     CONTINUE
C
          WRITE(BUF,1073) IROW,ISLOT,NHITR
 1073     FORMAT('Guessing that number of hits in row ',I2,
     &           ' of sector ',I2,' is ',I5,'. . .')
          CALL RERROR('TPRLST',21,BUF(1:65))
          IW(KTPAD+IOFRB)=NHITR
C
        ENDIF
        IF (IROW.GT.NTPDRW(ISTYP) .OR. IROW.LT.0) THEN
C
C++       Make a guess at the row number if it is out of range
C
          JROW=IRLST+1
          WRITE(BUF,1002) ISLOT,IROW,JROW
 1002     FORMAT('Input data format error in sector ',I2,
     &           ':  THE ROW NUMBER=',I3,' IS OUT OF RANGE.&',
     &           ' Set to IROW=',I2)
          CALL RERROR('TPRLST',30,BUF(1:90))
          IROW=JROW
          IW(KTPAD+IOFRB-1)=IROW
          NROWTM(ISLOT)=NROWTM(ISLOT)+1
        ENDIF
        IF (IROW.LE.IRLST) THEN
          IER=40
          WRITE(BUF,1003) ISLOT,IRLST
 1003     FORMAT('Input data format error in sector ',I2,
     &           ':  PADROWS ARE NOT IN ASCENDING ORDER.&',
     &           ' Truncate data at row ',I2)
          CALL RERROR('TPRLST',IER,BUF(1:99))
          NROWTM(ISLOT)=NROWTM(ISLOT)+1
C
C++       Truncate the TPAD bank at end of previous padrow
C
          LENTP=IOFRB-2
          NRTOT=NRTOT-1
          LOSS=IW(KTPAD)-LENTP
          WRITE(BUF,1086) LOSS,ISLOT
          CALL RERROR('TPRLST',72,BUF(1:75))
          CALL AUBOS('TPAD',ISLOT,LENTP,KTPAD,IGARB)
          IF (IGARB.EQ.2) GO TO 997
          IER=0
          IF (IGARB.EQ.1) THEN
            JGARB=IGARB
            KTPDI=NLINK('TPDI',ISLOT)
            KTPRL=NLINK('TPRL',ISLOT)
          ENDIF
          GO TO 120
        ENDIF
        IRLST=IROW
        IW(KROW(KTPRL,IROW)+JTPROS)=NSTOT
C
C++     Loop over pad hits in the row.  Check order of pad numbers
C
        MSAMP=0
        IORDR=0
        IPDLS=IBITS(IW(KTPAD+IOFRB+1),24,8)
        DO 110 IP=1,NHITR
C
C++       Look for repeating hit words (from bad TPD for example)
C
          IF (IP.NE.1) THEN
            IF (IW(KTPAD+IOFRB+IP).EQ.IW((KTPAD+IOFRB+IP)-1)) THEN
              IER=80
              WRITE(BUF,1094) IROW,ISLOT,IW(KTPAD+IOFRB+IP)
 1094         FORMAT('In row ',I2,' of sector ',I2,' hit word ',Z8,
     &               ' appears twice.')
              CALL RERROR('TPRLST',IER,BUF(1:55))
              NBDHTM(ISLOT)=NBDHTM(ISLOT)+1
              GO TO 1236
            ENDIF
          ENDIF
C
C++       Look for rediculous time information
C
          IT0=IBITS(IW(KTPAD+IOFRB+IP),0,9)
          NBCK=IBITS(IW(KTPAD+IOFRB+IP),16,8)
          IPDN=IBITS(IW(KTPAD+IOFRB+IP),24,8)
          IF (IT0+NBCK.GT.511) THEN
            IER=90
            WRITE(BUF,1397) IPDN,IROW,ISLOT
 1397       FORMAT('For pad ',I3,' of row ',I2,' of sector ',I2,
     &             ' the time is out of range.')
            NBDHTM(ISLOT)=NBDHTM(ISLOT)+1
            CALL RERROR('TPRLST',IER,BUF(1:60))
            GO TO 1236
          ENDIF
C
C++       Look for rediculous pad numbers
C
          IF (IPDLS.NE.IPDN) THEN
            IF (IORDR.EQ.0) THEN
              IORDR=IPDN-IPDLS
            ELSE
              JORDR=IPDN-IPDLS
              IF (IORDR*JORDR.LT.0) THEN
                IER=60
                WRITE(BUF,1009) IROW,ISLOT,IPDN,IP
 1009           FORMAT('Data format error in row ',I2,' of sector ',I2,
     &                 ':  pad number ',I4,', pulse ',I4,
     &                 ', is out of order.')
                NBDHTM(ISLOT)=NBDHTM(ISLOT)+1
                CALL RERROR('TPRLST',IER,BUF(1:88))
              ENDIF
            ENDIF
          ENDIF
          IF (IPDN.LE.0 .OR. IPDN.GT.(NTPDPR(IROW,ISTYP)+2)) THEN
            IER=50
            WRITE(BUF,1004) IROW,ISLOT,IPDN
 1004       FORMAT('Data format error in row ',I2,' of sector ',I2,
     &             ':  pad number=',I4,' is out of range.')
            CALL RERROR('TPRLST',IER,BUF(1:77))
            NBDHTM(ISLOT)=NBDHTM(ISLOT)+1
          ENDIF
C
C++       Try to recover the pad number by looking at the TPD
C++       channel number of this and neighboring pulses.
C
          IF (IER.EQ.50 .OR. IER.EQ.60) THEN
            ITPD2=IBITS(IW(KTPAD+IOFRB+IP),9,6)
            IF (ITPD2.NE.0 .AND. IORDR.NE.0) THEN
              IF (IP.NE.NHITR) THEN
                ITPD3=IBITS(IW(KTPAD+IOFRB+IP+1),9,6)
                IF (ITPD3.EQ.ITPD2+1) THEN
                  IPDNX=IBITS(IW(KTPAD+IOFRB+IP+1),24,8)
                  IF (IORDR.GT.0) THEN
                    IPDN=IPDNX-1
                  ELSE
                    IPDN=IPDNX+1
                  ENDIF
                  IF (IPDN.GT.0.AND.IPDN.LE.(NTPDPR(IROW,ISTYP)+2)) THEN
                    IER=0
                  ENDIF
                ENDIF
              ENDIF
              IF (IER.NE.0 .AND. IP.NE.1) THEN
                ITPD1=IBITS(IW(KTPAD+IOFRB+IP-1),9,6)
                IF (ITPD1.EQ.ITPD2-1) THEN
                  IF (IORDR.GT.0) THEN
                    IPDN=IPDLS+1
                  ELSE
                    IPDN=IPDLS-1
                  ENDIF
                  IF (IPDN.GT.0
     &                   .AND.IPDN.LE.(NTPDPR(IROW,ISTYP)+2)) THEN
                    IER=0
                  ENDIF
                ENDIF
              ENDIF
              IF (IER.EQ.0) THEN
                WRITE(BUF,9371) IROW,ISLOT,IPDN,ITPD2
 9371           FORMAT('Row ',I2,' of sector ',I2,': pad number ',
     &                 'set to ',I3,' for TPD channel ',I2)
                CALL RERROR('TPRLST',52,BUF(1:61))
                CALL MVBITS(IPDN,0,8,IW(KTPAD+IOFRB+IP),24)
              ENDIF
            ENDIF
          ENDIF
          IPDLS=IPDN
C
          MSAMP=MSAMP+IBITS(IW(KTPAD+IOFRB+IP),16,8)
          IF (NSTOT+MSAMP.GT.NDIG) THEN
            WRITE(BUF,1005) ISLOT
 1005       FORMAT('Data format error in sector ',I2,
     &             ':  TPAD specifies more samples than are in TPDI.')
            CALL RERROR('TPRLST',70,BUF(1:78))
            IER=70
            NTPDTM(ISLOT)=NTPDTM(ISLOT)+1
          ENDIF
C
C++       Truncate the pad bank to the end of the last pulse in case
C++       this pulse cannot be fixed.
C
 1236     CONTINUE
          IF (IER.NE.0) THEN
            IF (IP.EQ.1) THEN
              LENTP=IOFRB-2
              NRTOT=NRTOT-1
            ELSE
              LENTP=IOFRB+IP-1
              IW(KTPAD+IOFRB)=IP-1
            ENDIF
            LOSS=IW(KTPAD)-LENTP
            WRITE(BUF,1086) LOSS,ISLOT
 1086       FORMAT('Truncating ',I5,' words from bank TPAD for ',
     &             'slot ',I2,' due to data format error.')
            CALL RERROR('TPRLST',71,BUF(1:75))
            CALL AUBOS('TPAD',ISLOT,LENTP,KTPAD,IGARB)
            IF (IGARB.EQ.2) GO TO 997
            IER=0
            IF (IGARB.EQ.1) THEN
              JGARB=IGARB
              KTPDI=NLINK('TPDI',ISLOT)
              KTPRL=NLINK('TPRL',ISLOT)
            ENDIF
            NCPDTM(ISLOT)=NCPDTM(ISLOT)+1
            MSAMP=MSAMP-IBITS(IW(KTPAD+IOFRB+IP),16,8)
            IW(KROW(KTPRL,IROW)+JTPRNS)=MSAMP
            IW(KROW(KTPRL,IROW)+JTPRNP)=IP-1
            IW(KROW(KTPRL,IROW)+JTPROP)=IOFRB
            KPTST=IW(NPTST)
            IF (KPTST.NE.0) THEN
              IW(KROW(KPTST,ISLOT)+JPTSTP)=0
              ILSS=IW(KTPDI)*4 - (ITABL(KTPRL,IROW,JTPROS)+MSAMP)
              IW(KROW(KPTST,ISLOT)+JPTSNT)=LOSS*4+ILSS
            ENDIF
            GO TO 120
          ENDIF
          NHTOT=NHTOT+1
  110   CONTINUE
        NSTOT=NSTOT+MSAMP
        IW(KROW(KTPRL,IROW)+JTPRNS)=MSAMP
        IW(KROW(KTPRL,IROW)+JTPRNP)=NHITR
        IW(KROW(KTPRL,IROW)+JTPROP)=IOFRB
        IOFRB=IOFRB+NHITR+2
        GO TO 100
  120 CONTINUE
C
C++   Check length of TPAD bank.  Print message if it is too long.
C
      IF ((NRTOT*2 + NHTOT).NE.IW(KTPAD)) THEN
        WRITE(BUF,1006) ISLOT
 1006   FORMAT('WARNING:  length of TPAD bank for sector ',I2,
     &         ' is not correct.')
        CALL RERROR('TPRLST',80,BUF(1:59))
      ENDIF
      GO TO 999
C
  997 CONTINUE
        CALL BDROP(IW,'TPRL')
        GO TO 999
  998 CONTINUE
        IER=1
        JGARB=2
        CALL RERROR('TPRLST',IER,'No space to add named bank TPRL')
  999 CONTINUE
      RETURN
      END
#endif
