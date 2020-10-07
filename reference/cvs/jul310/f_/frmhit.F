      SUBROUTINE FRMHIT(DET,ITRK,NDEL,LIST)
C!
C!  Remove NDEL DET hits contained in LIST from track ITRK
C!  Uses DET banks FRTL and F*CL
C!
C!      Created: 23-OCT-1995    D.Casper
C!
C -------------------------------------------------------------------
#ifndef DOC
      IMPLICIT NONE
C
      CHARACTER*(*) DET
      INTEGER LIST(*), ITRK, NDEL
C
#include "frtljj.h"
#include "ftcljj.h"
#include "fvcljj.h"
#include "ficljj.h"
#include "tgcljj.h"
#include "tgtljj.h"
#include "bcs.h"
#include "bosext.h"
#include "bmacrod.h"
C
      LOGICAL KILL
C
      INTEGER KTKPL,KTKCL, IND, IOTKCL, NCO, JTKPOF, LTKCL, I
      INTEGER NKILLED, J, ICOORD
      INTEGER NFRTL, NFTCL, NFVCL, NFICL, NTGCL, NTGTL
      DATA    NFRTL, NFTCL, NFVCL, NFICL, NTGCL, NTGTL /6*0/

#include "bmacro.h"
C ----------------------------------------------------------------
C - 1st entry
      IF(NFRTL.EQ.0)THEN
        NFRTL = NAMIND('FRTL')
        NFTCL = NAMIND('FTCL')
        NFVCL = NAMIND('FVCL')
        NFICL = NAMIND('FICL')
        NTGCL = NAMIND('TGCL')
        NTGTL = NAMIND('TGTL')
      ENDIF
C
C - select the detector
C     TKPL is the TrackPointList (FRTL or TGTL)
C     TKCL is the TrackCoordinateList (FVCL, FICL, FTCL or TGCL)
C     JTKPOF is the TKPL HAC parameter of the offset to TKCL
C
      IF (DET(1:2).EQ.'TG') THEN
        KTKPL  = IW(NTGTL)
        KTKCL  = IW(NTGCL)
        JTKPOF = JTGTIO
      ELSEIF (DET(1:1).EQ.'T') THEN
        KTKPL  = IW(NFRTL)
        KTKCL  = IW(NFTCL)
        JTKPOF = JFRTIT
      ELSEIF (DET(1:1).EQ.'V') THEN
        KTKPL  = IW(NFRTL)
        KTKCL  = IW(NFVCL)
        JTKPOF = JFRTIV
      ELSEIF (DET(1:1).EQ.'I') THEN
        KTKPL  = IW(NFRTL)
        KTKCL  = IW(NFICL)
        JTKPOF = JFRTII
      ENDIF
C
C - check banks
      IF (KTKPL.LE.0 .OR. KTKCL.LE.0)THEN
        CALL RERROR('FRMHIT',1,'Missing track bank(s)')
        RETURN
      ENDIF
C
      LTKCL = IW(KTKCL)
      IOTKCL = ITABL(KTKPL,ITRK,JTKPOF)

      IND = KTKCL + LMHLEN + IOTKCL + 1
      NKILLED = 0
      NCO = ITABL(KTKPL,ITRK,JTKPOF+1)
C
C++   Check coordinates for track ITRK against list to be removed
C
      DO I = 1, NCO
        ICOORD = ABS(ITABL(KTKCL,IOTKCL+I,1))
        KILL =.FALSE.
        DO J = 1, NDEL
          IF (LIST(J).EQ.ICOORD)KILL=.TRUE.
        ENDDO 
        IF(.NOT.KILL)THEN
          IW(IND) = ICOORD
          IND = IND + 1
        ELSE
          NKILLED = NKILLED+1
        ENDIF
      ENDDO
      IF (NKILLED.NE.NDEL)
     &      CALL RERROR('FRMHIT',2,'Hit to be removed is missing.')
C
C++   Copy the remainder of TKCL
C
      CALL UCOPY(IW(KTKCL+LMHLEN+IOTKCL+NCO+1),IW(IND),
     &  LTKCL-(LMHLEN+IOTKCL+NCO))
C
C++   Decrement the total length of new TKCL
C
      IW(KTKCL+LMHROW) = IW(KTKCL+LMHROW) - NKILLED
C
C++   Update TKPL
C
C++   Number of coords on ITRK is NKILLED less
C
      IW(KROW(KTKPL,ITRK)+JTKPOF+1) = NCO - NKILLED
C
C++   All offsets starting after IOTKCL are reduced by NKILLED
C
      DO I = 1, LROWS(KTKPL)
        IF (ITABL(KTKPL,I,JTKPOF).GT.IOTKCL)
     &    IW(KROW(KTKPL,I)+JTKPOF) = IW(KROW(KTKPL,I)+JTKPOF) - NKILLED
      ENDDO

      RETURN
      END
#endif
