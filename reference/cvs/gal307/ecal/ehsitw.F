*DK ehsitw
      SUBROUTINE EHSITW(ISCMP,MODUL,TYGEO)
C.----------------------------------------------------------------
C Y.Karyotakis M.Rumpf Dec 85
C! ECAL signals -->addresses
C    Process ECAL Signals for one Geant3 Track Element
C    Find Towers and wire planes addresses
C   - Called by EHTRKE,EHSHOW
C   _ Calls     EHSMTW,EHSMWI,EHTTWD
C.----------------------------------------------------------------
#ifndef DOC
      SAVE
#include "bcs.h"
#include "ecsize.h"
#include "ecstat.h"
#include "ecnamc.h"
#include "trkcom.h"
C
      EXTERNAL EFNDCL,EFNDLG
      INTEGER  EFNDCL,EFNDLG
      DIMENSION IPLST(LWPLA)
      CHARACTER * 5 TYGEO
C
#include "bmacro.h"
      DATA NCALL/0/
C
      IF (NCALL.EQ.0)      THEN
        NCALL = 1
        CALL ECPLST(IPLST)
      ENDIF
C
C -------------------------------------------------------------
      IF (IDPSIG.EQ.0) RETURN
C
      NECSG = LROWS(IDPSIG)
C
      IF (NECSG .EQ. 0)                  RETURN
C
C     Loop over all generated Signals
C
      IPX = IDPSIG + LMHLEN
      DO 1 L = 1 , NECSG
C
C     Tranform HIT coordinates to ICL,ILG,STACK for PADS,and WIRE Plane
C
         ICL = EFNDCL(ISCMP,MODUL,RW(IPX+1),TYGEO)
         IF (ICL.LE.0)      THEN
             NECONT(6) = NECONT(6) + 1
             GO TO 1
         ENDIF
         ILG = EFNDLG(ISCMP,MODUL,RW(IPX+1),TYGEO)
         IF (ILG.LE.0)      THEN
            NECONT(7) = NECONT(7) + 1
            GO TO 1
         ENDIF
         CALL EVERIF (ILG,ICL,MODUL,IOKK)
         IF (IOKK.GT.0) ILG = IOKK
C
C probably the previous correction makes this redundant
C
         IF( IW(IPX+6).GT.LWPLA ) IW(IPX+6) = LWPLA
         IST = IPLST( IW(IPX+6) )
         ECCONT(IST) = ECCONT(IST) + IW(IPX+4)
C
C -         Compute tower address and fill 'ESHI'
C
         IADTW = 0
         CALL MVBITS(ICL,0,9,IADTW,2)
         CALL MVBITS(ILG,0,9,IADTW,16)
         CALL CAHIST(NAESHI,IADTW,IST,IW(IPX+4))
C
C-          Compute wire address and fill 'EWHI'
C
         IADWI = (ISCMP-1)*LMODU + MODUL
         CALL CAHIST(NAEWHI,IADWI,IW(IPX+6),IW(IPX+5))
C
    1 IPX = IPX + LCOLS(IDPSIG)
C
      RETURN
      END
#endif
