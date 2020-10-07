      SUBROUTINE TRKMCA
C
C--------------------------------------------------------------------
C! Associate TPC tracks with MC tracks
C!
C!    Author:   R. Johnson   11-10-87
C!    Modified: R. Johnson   19-01-88
C!
C!    Called by TPHIST
C!
C!    Description
C!    ===========
C!    First, all coordinates must be associated with Monte Carlo hits
C!    by a call to the routine TREFCH, which will fill the bank
C!    TPCH.  Then for each fitted track in bank TGFT, all Monte Carlo
C!    tracks are found which contain at least one hit matching with
C!    a coordinate used on the fitted track.  These results, including
C!    a chi-squared value giving a quality of each match, are stored
C!    in the banks TGMA and TMTL.
C!
C!-------------------------------------------------------------------
#ifndef DOC
C
#include "bcs.h"
#include "tpgpar.h"
#include "tgftjj.h"
#include "tgtljj.h"
#include "tgmajj.h"
#include "tmtljj.h"
#include "tpchjj.h"
#include "tphtjj.h"
#include "tpccon.h"
#include "rconds.h"
#include "partjj.h"
#include "fkinjj.h"
#include "fverjj.h"
#include "fpoijj.h"
C
      DIMENSION A(5,5),HELIX(5),P(3),X(3),XC(2)
      LOGICAL FIRST
      DATA FIRST/.TRUE./
C
C----------------------- Statement Functions -----------------------
C
#include "bmacro.h"
C
C-------------------------------------------------------------------
C
      IF (FIRST) THEN
        FIRST=.FALSE.
        NPART=NAMIND('PART')
        NTPHT=NAMIND('TPHT')
        NTPCO=NAMIND('TPCO')
        NTGFT=NAMIND('TGFT')
        NTGTL=NAMIND('TGTL')
        NTGCL=NAMIND('TGCL')
        NTPCH=NAMIND('TPCH')
        NTGMA=NAMIND('TGMA')
        NFKIN=NAMIND('FKIN')
        NFVER=NAMIND('FVER')
        NFPOI=NAMIND('FPOI')
        NTMTL=NAMIND('TMTL')
      ENDIF
C
      IF (IW(NTGMA).NE.0) CALL BDROP(IW,'TGMA')
      IF (IW(NTMTL).NE.0) CALL BDROP(IW,'TMTL')
C
      KFKIN=IW(NFKIN)
      IF (KFKIN.EQ.0) GO TO 999
      KFPOI=IW(NFPOI)
      IF (KFPOI.EQ.0) GO TO 999
      KFVER=IW(NFVER)
      IF (KFVER.EQ.0) GO TO 999
      KPART=MDARD(IW,JUNIDB(0),'PART',0)
      IF (KPART.EQ.0) GO TO 999
      KTPHT=IW(NTPHT)
      IF (KTPHT.EQ.0) GO TO 999
      KTPCO=IW(NTPCO)
      IF (KTPCO.EQ.0) GO TO 999
      KTGFT=IW(NTGFT)
      IF (KTGFT.EQ.0) GO TO 999
      KTGTL=IW(NTGTL)
      IF (KTGTL.EQ.0) GO TO 999
      KTGCL=IW(NTGCL)
      IF (KTGCL.EQ.0) GO TO 999
      KTPCH=IW(NTPCH)
      IF (KTPCH.EQ.0) GO TO 999
C
C++   Create association banks
C
      IW(1)=1
      LEN= LTGMAA*LROWS(KTGFT) + LMHLEN
      CALL AUBOS('TGMA',0,LEN,KTGMA,IGARB)
      IF (IGARB.EQ.2) GO TO 999
      CALL BLIST(IW,'T+','TGMA')
      IW(KTGMA+LMHCOL)=LTGMAA
      IW(KTGMA+LMHROW)=LROWS(KTGFT)
C
      IW(1)=1
      NMMX=2*LROWS(KTGFT)
      LEN= LTMTLA*NMMX+LMHLEN
      CALL AUBOS('TMTL',0,LEN,KTMTL,IGARB)
      IF (IGARB.EQ.2) GO TO 999
      CALL BLIST(IW,'T+','TMTL')
      IW(KTMTL+LMHCOL)=LTMTLA
      IW(KTMTL+LMHROW)=0
C
C++   Make one entry in TGMA per track in TGFT
C
      KTPCO=IW(NTPCO)
      KTPHT=IW(NTPHT)
      KTGMA=IW(NTGMA)
      KTMTL=IW(NTMTL)
      KTGFT=IW(NTGFT)
      KTGTL=IW(NTGTL)
      KTGCL=IW(NTGCL)
      KPART=IW(NPART)
      KFPOI=IW(NFPOI)
      DO 200 ITK=1,IW(KTGFT+LMHROW)
        IW(KROW(KTGMA,ITK)+JTGMNM)=0
        IW(KROW(KTGMA,ITK)+JTGMOM)=IW(KTMTL+LMHROW)
C
C++     Consider only those coordinates used in the track fit.
C
        IW(KROW(KTGMA,ITK)+JTGMNC)=ITABL(KTGTL,ITK,JTGTN1)
        IW(KROW(KTGMA,ITK)+JTGMNA)=0
C
C++     Loop over all coordinates on the fitted track and look for
C++     an associated MC hit and track
C
        DO 170 II=1,ITABL(KTGMA,ITK,JTGMNC)
          IC= IW(KTGCL + LMHLEN + ITABL(KTGTL,ITK,JTGTIO) + II)
          ICMC= ITABL(KTPCH,IC,JTPCIH)
          IF (ICMC.EQ.0) GO TO 170
          ITMC= ITABL(KTPHT,ICMC,JTPHKT)
C
C++       This MC track number from TPHT points to the old KINE bank.
C++       Use the FPOI bank to translate to the FKIN bank.
C
          IF (ITMC.GT.LROWS(KFPOI)) GO TO 170
          ITMC= ITABL(KFPOI,ITMC,JFPOIP)
          IF (ITMC.EQ.0) GO TO 170
C
          IW(KROW(KTGMA,ITK)+JTGMNA)=ITABL(KTGMA,ITK,JTGMNA)+1
C
C++       Fill entry into TMTL for each new MC track associated
C
          DO 160 J=1,ITABL(KTGMA,ITK,JTGMNM)
            IMT= ITABL(KTGMA,ITK,JTGMOM) + J
            IF (ITMC.EQ.ITABL(KTMTL,IMT,JTMTMT)) THEN
              IW(KROW(KTMTL,IMT)+JTMTNH)=ITABL(KTMTL,IMT,JTMTNH)+1
              GO TO 170
            ENDIF
  160     CONTINUE
C
          IW(KROW(KTGMA,ITK)+JTGMNM)=ITABL(KTGMA,ITK,JTGMNM)+1
          IW(KTMTL+LMHROW)=IW(KTMTL+LMHROW)+1
          MTMTL=IW(KTMTL+LMHROW)
          IF (MTMTL.GT.NMMX) THEN
            NMMX=NMMX+IW(KTGFT+LMHROW)
            LEN= LTMTLA*NMMX+LMHLEN
            IW(1)=1
            CALL AUBOS('TMTL',0,LEN,KTMTL,IGARB)
            IF (IGARB.EQ.2) GO TO 999
            IF (IGARB.EQ.1) THEN
              KTGMA=IW(NTGMA)
              KTGFT=IW(NTGFT)
              KTGTL=IW(NTGTL)
              KTGCL=IW(NTGCL)
              KTPCO=IW(NTPCO)
              KTPCH=IW(NTPCH)
              KTPHT=IW(NTPHT)
              KPART=IW(NPART)
              KFPOI=IW(NFPOI)
            ENDIF
          ENDIF
          IW(KROW(KTMTL,MTMTL)+JTMTMT)=ITMC
          IW(KROW(KTMTL,MTMTL)+JTMTNH)=1
  170   CONTINUE
C
C++     Get the errors on the fitted track parameters
C
        IDX=-1
        DO 180 I=1,5
          DO 178 J=1,I
            IDX=IDX+1
            A(I,J)= RTABL(KTGFT,ITK,JTGFEM+IDX)
  178     CONTINUE
  180   CONTINUE
C
C++     Compare the track parameters with all associated MC tracks
C
        DO 190 J=1,ITABL(KTGMA,ITK,JTGMNM)
          IMT= ITABL(KTGMA,ITK,JTGMOM) + J
          ITMC=ITABL(KTMTL,IMT,JTMTMT)
          ITYP=ITABL(KFKIN,ITMC,JFKIPA)
          Q=RTABL(KPART,ITYP,JPARCH)
          IF (Q.EQ.0.) THEN
            CHISQ=9999.
          ELSE
            P(1)=RTABL(KFKIN,ITMC,JFKIPX)
            P(2)=RTABL(KFKIN,ITMC,JFKIPY)
            P(3)=RTABL(KFKIN,ITMC,JFKIPZ)
            IF (P(1).NE.0. .OR. P(2).NE.0.) THEN
              IVT=ITABL(KFKIN,ITMC,JFKIOV)
              X(1)=RTABL(KFVER,IVT,JFVEVX)
              X(2)=RTABL(KFVER,IVT,JFVEVY)
              X(3)=RTABL(KFVER,IVT,JFVEVZ)
              CALL TNRHPA(P,X,Q,BFIELD,HELIX,XC,S)
              CHISQ=0.
              DO 185 K=1,5
                IF (A(K,K).EQ.0.) THEN
                  CHISQ=9999.
                  GO TO 186
                ENDIF
                CHISQ=CHISQ + (HELIX(K)
     &                        - RTABL(KTGFT,ITK,JTGFIR-1+K))**2
     &                        / A(K,K)
  185         CONTINUE
  186         CONTINUE
            ELSE
              CHISQ=9999.
            ENDIF
          ENDIF
          RW(KROW(KTMTL,IMT)+JTMTCS)=CHISQ
  190   CONTINUE
  200 CONTINUE
C
C++   Set the length of the TMTL bank
C
      CALL AUBPRS('TMTL')
C
  999 CONTINUE
      RETURN
      END
#endif
