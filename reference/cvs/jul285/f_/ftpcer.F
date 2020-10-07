      SUBROUTINE FTPCER(IRET)
C
C----------------------------------------------------------------------
C! Recalculate the TPC coordinate errors for all coordinates
C
C            R. Johnson    19-OCT-1990
C  Modified: D. Casper     12-Jun-1995  Get wire correction flag
C
C  Input:     Bank TPCO with NR=0   Old coordinate bank (gets dropped)
C             Bank PCOI with info on coordinates
C             Bank FRFT with track helix parameters
C             Banks FRTL and FTCL with pointers from track to coordinates
C
C  Output:    IRET      /I        Return code.  0=all OK
C             Bank TPCO with NR=1   Coordinates with new errors
C
C----------------------------------------------------------------------
#ifndef DOC
C
#include "bcs.h"
#include "frftjj.h"
#include "frtljj.h"
#include "tpcojj.h"
#include "pcoijj.h"
C
      LOGICAL FIRST
      DATA FIRST/.TRUE./
C
C------------------ Statement Function Definitions ---------------------
C
#include "bmacro.h"
C
C-----------------------------------------------------------------------
C
      IF (FIRST) THEN
        FIRST=.FALSE.
        NFRFT=NAMIND('FRFT')
        NFRTL=NAMIND('FRTL')
        NFTCL=NAMIND('FTCL')
        NPCOI=NAMIND('PCOI')
        NTCRL=NAMIND('TCRL')
      ENDIF
C
      ITPCO=NLINK('TPCO',0)
      KFRFT=IW(NFRFT)
      KFRTL=IW(NFRTL)
      KFTCL=IW(NFTCL)
      IF (ITPCO.EQ.0 .OR. KFRFT.EQ.0 .OR. KFRTL.EQ.0
     &                                  .OR. KFTCL.EQ.0) THEN
        IRET=1
        CALL RERROR('FTPCER',1,'Input banks not found.')
        RETURN
      ENDIF
C
C++   Drop the output bank if it already exists
C
      KTPCO=NLINK('TPCO',1)
      IF (KTPCO.NE.0) KTPCO=NDROP('TPCO',1)
C
C++   Rename the TPCO bank to have NR=1
C
      KTPCO=MSWAP(IW,'TPCO',0,'TPCO',1)
C
C++   Loop over all coordinates on the first half loop of tracks
C++   and redo their error estimates.
C
      KPCOI=IW(NPCOI)
      KTCRL=IW(NTCRL)
      DO 100 ITK=1,LROWS(KFRTL)
        NCO=ITABL(KFRTL,ITK,JFRTNT)
        IOF=ITABL(KFRTL,ITK,JFRTIT)+LMHLEN+KFTCL
        DO 70 II=1,NCO
          IC=IW(IOF+II)
C
C++       Make sure that the pointer in TPCO is correct
C
          IW(KROW(KTPCO,IC)+JTPCTN)=ITK
C
C++       Get the track parameters at the padrow in question
C
          CALL TFILPA(KROW(KTPCO,IC),KROW(KFRFT,ITK),PADCR,WIRCR)
C
C++       Are there half pads in the cluster?  We can find out from
C++       the PCOI bank as long as TPCO has not been resorted (i.e.
C++       as long as TCRL does not exist).
C
          IF (KTCRL.EQ.0 .AND. KPCOI.NE.0) THEN
            NHALF=ITABL(KPCOI,IC,JPCONH)
            ICOR = ITABL(KPCOI,IC,JPCOTF)/10
          ELSE
            NHALF=0
            ICOR = 0
            IF (KTCRL.NE.0)
     &        CALL RERROR('FTPCER',1,
     &        'TCRL exists, Landau correction errors will be wrong')
          ENDIF
C
C++       Get the parameterized error
C
          Z=RTABL(KTPCO,IC,JTPCZV)
          TL=RTABL(KFRFT,ITK,JFRFTL)
          CALL TERPAR(PADCR,Z,TL,NHALF,ICOR,SRPHI,SIGZ)
C
C++       Save it in TPCO
C
          RW(KROW(KTPCO,IC)+JTPCSR)=SRPHI**2
          RW(KROW(KTPCO,IC)+JTPCSZ)=SIGZ**2
C
   70   CONTINUE
  100 CONTINUE
C
      IRET=0
      END
#endif
