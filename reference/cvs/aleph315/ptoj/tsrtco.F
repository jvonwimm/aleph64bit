      SUBROUTINE TSRTCO(LIST,IER)
C
C----------------------------------------------------------------------
C! Sort the TPCO bank and produce the rowlist (TCRL) bank
C!
C!    Author:  R. Johnson   28-06-88
C!    Modified: I. Tomalin  12-05-92 to correct bug in reordering of
C!                                   FTCL bank.
C!    Input :    LIST      BOS event list
C                          if LIST(2:2).eq.'-' drop POT banks
C     Output:    IER       = 0  successful
C                          = 1  input bank does not exist or is empty
C                          = 2  not enough space
C                          =-1  OK but garbage collection
C!
C!    Called by TPTOJ
C!
C!    Comments
C!    ========
C!    This is meant to be used when unpacking the POT.
C!    If the FRTL and FTCL banks exist, then the pointers in FTCL
C!    are updated to point properly into the new TPCO bank.  Thus
C!    the track bank should be unpacked before calling this routine.
C!
C----------------------------------------------------------------------
#ifndef DOC
      SAVE
      CHARACTER*(*) LIST, PLIST*1, JLIST*4
#include "bcs.h"
#include "tpgpar.h"
#include "tpgeom.h"
#include "tpcojj.h"
#include "tcrljj.h"
#include "frtljj.h"
C
      COMMON/TSRTWK/ JSORTW
      LOGICAL FIRST
      DATA FIRST/.TRUE./
C
#include "bmacro.h"
C
      IF (FIRST) THEN
        FIRST=.FALSE.
        JSORTW=0
        NTPCO=NAMIND('TPCO')
        NTCRL=NAMIND('TCRL')
        NTTTT=NAMIND('TTTT')
        NFRTL=NAMIND('FRTL')
        NFTCL=NAMIND('FTCL')
      ENDIF
C
      IER = 1
      KTPCO=IW(NTPCO)
      IF (KTPCO.EQ.0) GOTO 999
      IF (LROWS(KTPCO).EQ.0) GOTO 999
C
C++   Book some workspace for sorting
C
      IER = 2
      NCOOR=LROWS(KTPCO)
      LEN=3*NCOOR
      IW(1)=1
      CALL WBANK(IW,JSORTW,LEN,*999)
C
C++   Book space for the rowlist bank
C
      IF (IW(NTCRL).NE.0) CALL BDROP(IW,'TCRL')
C
      LEN= LTPDRO*LTCRLA+LMHLEN
      CALL AUBOS('TCRL',0,LEN,KTCRL,IER)
      IF (IER.EQ.2) GOTO 999
      JLIST = 'TCRL'
      IER2 = IER
      IW(KTCRL+LMHCOL)=LTCRLA
      IW(KTCRL+LMHROW)=LTPDRO
      KTPCO = IW(NTPCO)
C
C++   Temporarily use the name TTTT for the new TPCO bank
C
      IW(1)=1
      CALL AUBOS('TTTT',0,IW(KTPCO),KTTTT,IER)
      IF (IER.EQ.2) GOTO 998
      IER1 = IER
      KTPCO=IW(NTPCO)
      IW(KTTTT+LMHCOL)=LCOLS(KTPCO)
      IW(KTTTT+LMHROW)=LROWS(KTPCO)
C
C++   Make sure the coordinates are pointing to the FRFT track
C
      KFRTL=IW(NFRTL)
      KFTCL=IW(NFTCL)
      IF (KFRTL.NE.0 .AND. KFTCL.NE.0) THEN
        DO 20 IC=1,NCOOR
          IW(KROW(KTPCO,IC)+JTPCTN)=0
   20   CONTINUE
        DO 50 ITK=1,LROWS(KFRTL)
          DO 40 II=1,ITABL(KFRTL,ITK,JFRTNT)
            IC=IW(KFTCL+LMHLEN+ITABL(KFRTL,ITK,JFRTIT)+II)
            IW(KROW(KTPCO,IC)+JTPCTN)=ITK
   40     CONTINUE
          IOFF=ITABL(KFRTL,ITK,JFRTIT)+ITABL(KFRTL,ITK,JFRTNT)
          DO 45 II=1,ITABL(KFRTL,ITK,JFRTNR)
            IC=IW(KFTCL+LMHLEN+IOFF+II)
            IW(KROW(KTPCO,IC)+JTPCTN)=-ITK
   45     CONTINUE
   50   CONTINUE
      ENDIF
C
C++   Make a list of the coordinate indices for sorting
C
      DO 100 IC=1,NCOOR
        IW(JSORTW+IC)=ITABL(KTPCO,IC,JTPCIN)
  100 CONTINUE
C
C++   Sort the coordinates
C
      CALL SORTZV(IW(JSORTW+1),IW(JSORTW+NCOOR+1),NCOOR,-1,0,0)
C
C++   Fill array of where coordinates went to
C
      DO 150 JJ=1,NCOOR
        IC=IW(JSORTW+NCOOR+JJ)
        IW(JSORTW+2*NCOOR+IC)=JJ
  150 CONTINUE
C
C++   Copy the coordinates to the new bank
C
      DO 200 II=1,NCOOR
        IC=IW(JSORTW+NCOOR+II)
        CALL UCOPY(IW(KROW(KTPCO,IC)+1),
     &                   IW(KROW(KTTTT,II)+1),LCOLS(KTPCO))
  200 CONTINUE
C
C++   Update FTCL bank
C
      IF (KFRTL.NE.0 .AND. KFTCL.NE.0) THEN
        DO 250 JJ=1,LROWS(KFTCL)
          ICO=IW(KFTCL+LMHLEN+JJ)
          IW(KFTCL+LMHLEN+JJ)=IW(JSORTW+2*NCOOR+ICO)
  250   CONTINUE
      END IF
C
C++   Swap the name TPCO to the new bank and drop the old one
C
      CALL BSWAP(IW,'TPCO','TTTT')
      CALL BDROP(IW,'TTTT')
      KTPCO=IW(NTPCO)
C
C++   Fill the rowlist bank
C
      IRLST=-1
      DO 300 IC=1,NCOOR
        ID=ITABL(KTPCO,IC,JTPCIN)
        IROW=ID/100000
        ISLOT=(ID-IROW*100000)/1000
        IF (IROW.NE.IRLST) THEN
          IRLST=IROW
          IW(KROW(KTCRL,IROW)+JTCROC)=IC-1
        ENDIF
        IW(KROW(KTCRL,IROW)+JTCRNC)=ITABL(KTCRL,IROW,JTCRNC)+1
        IF (IENDTP(ISLOT).EQ.2) THEN
          IW(KROW(KTCRL,IROW)+JTCRN1)=ITABL(KTCRL,IROW,JTCRN1)+1
        ENDIF
  300 CONTINUE
C
  998 CONTINUE
C - get the drop flag if any, then drop POT banks if required,
C   add JUL banks to S-list
C   POT banks are on PLIST, JUL banks on JLIST
      PLIST = ' '
#include "jplist.h"
C
      IF (IER1+IER2 .GT. 0) IER = -1
C
  999 CONTINUE
      CALL WDROP(IW,JSORTW)
      RETURN
      END
#endif
