      SUBROUTINE PTBCOJ(LIST,IER)
C
C-----------------------------------------------------------------------
C! Transfer the PTBC bank to the TBCO bank
C!
C!    Author:   R. Johnson  16-06-88
C!    Input :    LIST      BOS event list
C                          if LIST(2:2).eq.'-' then drop POT banks
C     Output:    IER       = 0  successful
C                          = 1  input bank does not exist or is empty
C                          = 2  not enough space
C                          =-1  OK but garbage collection
C!
C!    Called by TPTOJ
C!
C-----------------------------------------------------------------------
#ifndef DOC
      SAVE
C
#include "bcs.h"
#include "tbcojj.h"
#include "ptbcjj.h"
#include "ptunjj.h"
#include "tpgpar.h"
#include "tpgeom.h"
#include "tpgeop.h"
C
      CHARACTER*(*) LIST, PLIST*4, JLIST*4
      LOGICAL FIRST
      DATA FIRST/.TRUE./
C
#include "bmacro.h"
C
      IF (FIRST) THEN
        FIRST=.FALSE.
        NPTBC=NAMIND('PTBC')
        NPTUN=NAMIND('PTUN')
        NTBCO=NAMIND('TBCO')
        CALL BKFMT('TBCO','2I,(I,6F,I)')
      ENDIF
C
      KPTBC=IW(NPTBC)
      KPTUN=IW(NPTUN)
      IER = 1
      IF (KPTBC.EQ.0 .OR. KPTUN.EQ.0) GOTO 999
      IF (LROWS(KPTBC).EQ.0 .OR. LROWS(KPTUN).EQ.0) GOTO 999
C
      LEN=LTBCOA*LROWS(KPTBC)+LMHLEN
      IW(1)=1
      CALL AUBOS('TBCO',0,LEN,KTBCO,IER)
      IF (IER.EQ.2) GOTO 999
      JLIST = 'TBCO'
      IF (IER.EQ.1) THEN
        KPTUN=IW(NPTUN)
        KPTBC=IW(NPTBC)
      ENDIF
      IW(KTBCO+LMHCOL)=LTBCOA
      IW(KTBCO+LMHROW)=LROWS(KPTBC)
C
      IPTUN=KPTUN+LMHLEN
      DO 500 IC=1,LROWS(KPTBC)
        ISLOT=ITABL(KPTBC,IC,JPTBSL)
        IROW=ITABL(KPTBC,IC,JPTBSR)
        ISTYP=ITPTYP(ISLOT)
        IF (ISTYP.NE.1) THEN
          IROWG=IROW+NTPDRW(1)
        ELSE
          IROWG=IROW
        ENDIF
        PHI=FLOAT(ITABL(KPTBC,IC,JPTBPS))*RW(IPTUN+JPTUPB)
        Z=FLOAT(ITABL(KPTBC,IC,JPTBZV))*RW(IPTUN+JPTUZB)
        R= TPDRBG(ISTYP)+(IROW-1)*TPDRST(ISTYP)
        DPHI=PHI+TPDPHW(IROW,ISTYP)
        PAD= (R*DPHI)/TPDSEP(ISTYP)
        IPAD=MIN(NTPDPR(IROW,ISTYP)+2,INT(PAD+1.5))
        RPHI=R*PHI
        CALL TCTGLB(ISLOT,R,RPHI,Z,RG,RPHIG,ZG)
        PHIG=RPHIG/RG
        IW(KROW(KTBCO,IC)+JTBCIN)=100000*IROW+1000*ISLOT+IPAD
        RW(KROW(KTBCO,IC)+JTBCRV)=RG
        RW(KROW(KTBCO,IC)+JTBCPH)=PHIG
        RW(KROW(KTBCO,IC)+JTBCZV)=ZG
        IW(KROW(KTBCO,IC)+JTBCRW)=ITABL(KPTBC,IC,JPTBNP)
        IW(KROW(KTBCO,IC)+JTBCZW)=ITABL(KPTBC,IC,JPTBNB)
        IW(KROW(KTBCO,IC)+JTBCSC)=0
  500 CONTINUE
C
  998 CONTINUE
C - get the drop flag if any, then drop POT banks if required,
C   add JUL banks to S-list
C   POT banks are on PLIST, JUL banks on JLIST
      PLIST = 'PTBC'
#include "jplist.h"
C
      IF (IER.EQ.1) IER = -1
C
  999 CONTINUE
      RETURN
      END
#endif
