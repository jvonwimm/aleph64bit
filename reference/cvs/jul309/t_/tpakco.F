      SUBROUTINE TPAKCO(IER)
C
C----------------------------------------------------------------------
C! Produce the PTNC bank from TPCO
C!
C!    Author:   R. Johnson    880615
C!    Modified: M.Cattaneo    971017
C!              Produce PTNC even if FRTL and FTCL are missing
C!
C!    Output:  IER        /I    Error code=0 if operation is successful
C!
C!    Called by TJTOP
C!
C----------------------------------------------------------------------
#ifndef DOC
C
#include "bcs.h"
#include "tpcojj.h"
#include "ptncjj.h"
#include "frtljj.h"
C
      LOGICAL FIRST
      DATA FIRST/.TRUE./
C
#include "bmacro.h"
C
      IF (FIRST) THEN
        NTPCO=NAMIND('TPCO')
        NFRTL=NAMIND('FRTL')
        NFTCL=NAMIND('FTCL')
        NPTUN=NAMIND('PTUN')
        NPTNC=NAMIND('PTNC')
      ENDIF
C
      KTPCO=IW(NTPCO)
      IF (KTPCO.EQ.0) THEN
        IER=1
        GO TO 999
      ENDIF
      KPTUN=IW(NPTUN)
      IF (KPTUN.EQ.0) THEN
        IER=4
        GO TO 999
      ENDIF
C
      IF (IW(NPTNC).NE.0) CALL BDROP(IW,'PTNC')
      LEN=LROWS(KTPCO)*LPTNCA+LMHLEN
      IW(1)=1
      CALL AUBOS('PTNC',0,LEN,KPTNC,IGARB)
      IF (IGARB.EQ.2) THEN
        IER=4
        GO TO 999
      ELSEIF (IGARB.EQ.1) THEN
        KTPCO=IW(NTPCO)
      ENDIF
C
      IW(KPTNC+LMHCOL)=LPTNCA
      IW(KPTNC+LMHROW)=LROWS(KTPCO)
C
C++   Zero the track pointers in TPCO
C
      DO 20 IC=1,LROWS(KTPCO)
        IW(KROW(KTPCO,IC)+JTPCTN)=0
   20 CONTINUE
      NCO=0
C
C++   Loop over coordinates by order of the TPC tracks
C
      KFRTL=IW(NFRTL)
      IF (KFRTL.NE.0) THEN

        KFTCL=IW(NFTCL)
        NTK = LROWS(KFRTL)
        IF( NTK .GT. 0 .AND. KFTCL .GT. 0 ) THEN

          DO 100 ITK=1,NTK
            NC=ITABL(KFRTL,ITK,JFRTNT)+ITABL(KFRTL,ITK,JFRTNR)
            IF (NC.GT.0) THEN
              DO 50 II=1,NC
                IC=IW(KFTCL+LMHLEN+ITABL(KFRTL,ITK,JFRTIT)+II)
C
C++       Fill the track pointer in TPCO
C
                IW(KROW(KTPCO,IC)+JTPCTN)=ITK
C
C++       Fill an entry into PTNC
C
                NCO=NCO+1
                CALL TPK1CO(IC,IW(KROW(KPTNC,NCO)+JPTNSL))
   50         CONTINUE
            ENDIF
  100     CONTINUE
        ENDIF
      ENDIF
C
C++   Now take care of the unassociated coordinates (those which were
C++   not marked with a track number above).
C
      DO 200 IC=1,LROWS(KTPCO)
        IF (ITABL(KTPCO,IC,JTPCTN).EQ.0) THEN
          NCO=NCO+1
          CALL TPK1CO(IC,IW(KROW(KPTNC,NCO)+JPTNSL))
        ENDIF
  200 CONTINUE
      IER=0
C
  999 CONTINUE
      RETURN
      END
#endif
