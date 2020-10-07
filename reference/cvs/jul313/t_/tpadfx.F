      SUBROUTINE TPADFX
C----------------------------------------------------------------------
C!  - Purpose: synchronize TPAD and TPDI bank when
C!             one hit is missing in the TPAD bank but not
C!             in the TPDI bank. This could happen when
C!             a hit was removed by the TPP in the TPAD bank
C!             but the corresponding digitizations were not
C!             removed.
C!
C!   Author   :- Werner Wiedenmann     19-May-1993
C!
C!======================================================================
#ifndef DOC
C
#include "tpgpar.h"
#include "tpgeom.h"
#include "tpgeop.h"
#include "tclbjj.h"
#include "tsimjj.h"
#include "tmonit.h"
#include "rparac.h"
#include "rflags.h"
#include "rlunit.h"
#include "rcurnt.h"
#include "bcs.h"
C
      LOGICAL FIRST,FBAD(LTSECT),QDATA,QMARK
      CHARACTER TEXT*80
      DIMENSION IOFS(LTSECT,6),IPHV(512)
      DATA FIRST/.TRUE./
C
#include "bmacro.h"
C
C++   Initialisation
C
      IF (FIRST) THEN
        FIRST=.FALSE.
        NTPAD=NAMIND('TPAD')
        NTPDI=NAMIND('TPDI')
        NTCLB=NAMIND('TCLB')
        NTSIM=NAMIND('TSIM')
      ENDIF
C
C++   Reset flag and offset for bad sectors
C
      DO 50 ISLOT=1,LTSECT
        FBAD(ISLOT)=.FALSE.
        DO IL=1,6
           IOFS(ISLOT,IL)=0
        ENDDO
   50 CONTINUE
C
C++   Check for the TCLB bank, to get the threshold parameters
C++   If bank is missing: return
C++   For Monte Carlo use the TSIM bank.
C
C.... Preset Thresholds (default values for pads)
      QDATA  = .TRUE.
      ITHRSH = 8
      NPRE   = 2
      NPOS   = 2
      LIMW   = 2
C.... For Monte Carlo
      KTSIM = IW(NTSIM)
      IF (KTSIM.NE.0) THEN
         QDATA  = .FALSE.
         ITHRSH = ITABL(KTSIM,1,JTSILH)
         NPRE   = ITABL(KTSIM,1,JTSINE)
         NPOS   = ITABL(KTSIM,1,JTSINT)
         LIMW   = ITABL(KTSIM,1,JTSIMN)
      ELSE
C.... For real data there should be a TCLB bank
         KTCLB  = IW(NTCLB)
         IF (KTCLB.EQ.0) CALL RERROR('TPADFX',1,
     +   'Warning, no TCLB bank, default values used for thresholds.')
         QDATA  = .TRUE.
      ENDIF
C
C++   Count the the number of samples in the TPAD bank and
C++   calculate the exepected length of the corresponding
C++   TPDI bank
C++   ----------------------------------------------------
C
C     +-------------------+
C++   | Loop over sectors |
C     +-------------------+
      KTPAD=IW(NTPAD)
  100 IF (KTPAD.EQ.0) GO TO 501
        ISLOT=IW(KTPAD-2)
        IF (ISLOT.LT.1 .OR. ISLOT.GT.LTSECT) THEN
          KTPAD=IW(KTPAD-1)
          GO TO 100
        ENDIF
        KTPDI=NLINK('TPDI',ISLOT)
        IF (KTPDI.EQ.0) THEN
          KTPAD=IW(KTPAD-1)
          GO TO 100
        ENDIF
C
C++     setup pad thresholds and pulse limits for this sector (data)
C
        IF (QDATA.AND.(KTCLB.NE.0)) THEN
           ITHRSH = ITABL(KTCLB,ISLOT,JTCLPT)
           IPADLM = ITABL(KTCLB,ISLOT,JTCLPL)
           NPRE   = IBITS(IPADLM,8,4)
           NPOS   = IBITS(IPADLM,12,4)-NPRE
           LIMW   = IBITS(IPADLM,0,8)-IBITS(IPADLM,12,4)+1
        ENDIF
C
        ISTYP=ITPTYP(ISLOT)
C       +-------------------+
C++     | Loop over padrows |
C       +-------------------+
        QMARK=.FALSE.
        IOFSET=0
        IPNT=0
        NS=0
  120   IF (IPNT.GE.IW(KTPAD)-2) GO TO 151
          IPNT=IPNT+1
          IROW=IBITS(IW(KTPAD+IPNT),0,4)
C
C++       Skip this sector if a row number is bad or if
C++       the number of hits in a row is garbage
C
          IF (IROW.LT.1 .OR. IROW.GT.NTPDRW(ISTYP)) THEN
            FBAD(ISLOT)=.FALSE.
            KTPAD=IW(KTPAD-1)
            GO TO 100
          ENDIF
          IPNT=IPNT+1
          NHIT=IW(KTPAD+IPNT)
          IF (NHIT.LT.1 .OR. NHIT.GT.(IW(KTPAD)-IPNT)) THEN
            FBAD(ISLOT)=.FALSE.
            KTPAD=IW(KTPAD-1)
            GO TO 100
          ENDIF
C         +-------------------------------+
C++       | Loop over hits in this padrow |
C         +-------------------------------+
          DO 150 IH=1,NHIT
            IPNT=IPNT+1
            IPAD =IBITS(IW(KTPAD+IPNT),24,8)
            IT0  =IBITS(IW(KTPAD+IPNT),0,9)
            NSAMP=IBITS(IW(KTPAD+IPNT),16,8)
            NS   =IBITS(IW(KTPAD+IPNT),16,8)+NS
C             +-------------------------------------------------------+
C+++          | Loop over samples in TPDI, pick up the pulse heights. |
C             +-------------------------------------------------------+
              IADDR = KTPDI + 1 + IOFSET/4
              IBIT0 = 24 - 8*MOD(IOFSET,4)
C
              DO 40 ISAMP=1,NSAMP
                 JWORD = IW(IADDR)
                 IPH = IBITS(JWORD,IBIT0,8)
                 IPHV(ISAMP) = IPH
                 IBIT0 = IBIT0-8
                 IF (IBIT0.LT.0) THEN
                    IADDR = IADDR + 1
                    IBIT0 = 24
                 ENDIF
  40          CONTINUE
              IOFSET = IOFSET + NSAMP
C             +-----------------------------------------+
C+++          | Check samples in TPDI, spot bad pulses. |
C             +-----------------------------------------+
              IF (.NOT.QMARK) THEN
                 IF (IPHV(2).GT.ITHRSH) QMARK=.TRUE.
                 DO 41 ISAMP=3,(NSAMP-2)
                    IF (IPHV(ISAMP).LE.ITHRSH) QMARK=.TRUE.
  41             CONTINUE
                 IF (QMARK) THEN
                    IOFS(ISLOT,2)=IROW
                    IOFS(ISLOT,3)=IPAD
                    IOFS(ISLOT,4)=IT0
                    IOFS(ISLOT,5)=NSAMP
                    IOFS(ISLOT,6)=IH
                 ENDIF
              ENDIF
  150     CONTINUE
          GO TO 120
  151   CONTINUE
C
C++     Number of possible samples in TPDI
C
        N4TPDI = 4 * IW(KTPDI)
C
C++     examine last word of TPDI/subtract 0 pulses to get correct
C++     number of samples
C
        IF (N4TPDI.GT.0) THEN
           LDWORD = IW(KTPDI+IW(KTPDI))
           IPH1   = IBITS(LDWORD,0,8)
           IPH2   = IBITS(LDWORD,8,8)
           IPH3   = IBITS(LDWORD,16,8)
           IF (IPH1.EQ.0) N4TPDI = N4TPDI-1
           IF (IPH2.EQ.0) N4TPDI = N4TPDI-1
           IF (IPH3.EQ.0) N4TPDI = N4TPDI-1
        ENDIF
C
C++     Mark this sector as candidate if the digitization bank is longer
C
        IF ((N4TPDI.GT.NS).AND.QMARK) THEN
           FBAD(ISLOT)=.TRUE.
           IOFS(ISLOT,1)=N4TPDI-NS
        ENDIF
C
        KTPAD=IW(KTPAD-1)
        GO TO 100
  501 CONTINUE
C
C++   Debug output
C
      IF (JDBDRF(JULTP).GE.4) THEN
        WRITE(LDEBRL,320) IRUNRC,IEVTRC
 320    FORMAT(' TPADFX dump of bad sectors for Run:',I10,' Event:',I10)
        WRITE(LDEBRL,321)
 321    FORMAT(' Column 1 : sector number '/
     +         ' Column 2 : =T digitizations will be removed from TPDI'/
     +         ' Column 3 : offset in TPDI for bad digitizations'/
     +         ' Column 4 : row# for first bad hit'/
     +         ' Column 5 : pad# for first bad hit'/
     +         ' Column 6 : t0   for first bad hit'/
     +         ' Column 7 : #samples for first bad hit'/
     +         ' Column 8 : hit# in the pad row for first bad hit'/
     +         ' ------------------------------------------------')
        DO ISS=1,LTSECT
           WRITE(LDEBRL,322) ISS,FBAD(ISS),(IOFS(ISS,ILL),ILL=1,6)
 322       FORMAT(I5,L3,6I8)
        ENDDO
      ENDIF
C
C++   Remake TPDI banks for the bad sectors
C++   -------------------------------------
C
      DO 801 ISLOT=1,LTSECT
        IF (.NOT.FBAD(ISLOT)) GO TO 801
C
        WRITE(TEXT,325) ISLOT
  325   FORMAT('Warning, hits being removed from TPDI ',
     &         'for sector ',I2)
        CALL RERROR('TPADFX',2,TEXT(1:51))
        NTPDTM(ISLOT)=NTPDTM(ISLOT)+1
C
        KTPAD=NLINK('TPAD',ISLOT)
        KTPDI=NLINK('TPDI',ISLOT)
C       +-------------------+
C++     | Loop over padrows |
C       +-------------------+
        IPNT=0
        IWRD=KTPDI+1
        IBT=24
        IWRD1=KTPDI+1
        IBT1=24
  620   IF (IPNT.GE.IW(KTPAD)-1) GO TO 621
          IPNT=IPNT+1
          IROW=IBITS(IW(KTPAD+IPNT),0,4)
          IPNT=IPNT+1
          NHIT=IW(KTPAD+IPNT)
C
C++       Throw out digitizations, which do not correspond to a hit
C++       in the TPAD bank
C
          DO 750 IH=1,NHIT
            IPNT=IPNT+1
            IHTWD=IW(KTPAD+IPNT)
            IT0=IBITS(IHTWD,0,9)
            NBK=IBITS(IHTWD,16,8)
C           +-------------------------------------------------+
C++         | If the pad pulse is reached, jump by the offset |
C++         | computed before in the TPDI bank                |
C           +-------------------------------------------------+
            IF ((IOFS(ISLOT,2).EQ.IROW).AND.(IOFS(ISLOT,6).EQ.IH)) THEN
              DO 736 II=1,IOFS(ISLOT,1)
                IBT=IBT-8
                IF (IBT.LT.0) THEN
                  IBT=24
                  IWRD=IWRD+1
                ENDIF
  736         CONTINUE
            ENDIF
C           +---------------+
C++         | Copy the rest |
C           +---------------+
            DO 735 II=1,NBK
              IRRR = IW(IWRD)
              CALL MVBITS(IRRR,IBT,8,IW(IWRD1),IBT1)
              IBT=IBT-8
              IF (IBT.LT.0) THEN
                IBT=24
                IWRD=IWRD+1
              ENDIF
              IBT1=IBT1-8
              IF (IBT1.LT.0) THEN
                IBT1=24
                IWRD1=IWRD1+1
              ENDIF
  735       CONTINUE
  750     CONTINUE
C
          GO TO 620
  621   CONTINUE
C
C++     Set the length of TPDI
C
        IF (IBT1.EQ.24) THEN
          LEN=IWRD1-KTPDI-1
        ELSE
          LEN=IWRD1-KTPDI
C
          IEND=(IBT1+8)/8
          DO 835 II=1,IEND
            IRRR = 0
            CALL MVBITS(IRRR,IBT,8,IW(IWRD1),IBT1)
            IBT=IBT-8
            IF (IBT.LT.0) THEN
              IBT=24
            ENDIF
            IBT1=IBT1-8
            IF (IBT1.LT.0) THEN
              GOTO 836
            ENDIF
  835     CONTINUE
  836     CONTINUE
        ENDIF
        CALL AUBOS('TPDI',ISLOT,LEN,KTPDI,IGARB)
C
  801 CONTINUE
C
  999 RETURN
      END
#endif
