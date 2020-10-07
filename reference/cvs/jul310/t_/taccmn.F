      SUBROUTINE TACCMN
C----------------------------------------------------------------------
C!  - Accumulate TPC monitoring information
C!
C!  Author:  R. Johnson  11-10-89
C!   Modified :- E. Lancon             21-JUL-1993
C!   Modified :- Z. FENG               06-MAY-1994
C!               fill DEDX histograms per sector and global
C!               DEDX per sector is not corrected with sector gain map
C!               DEDX global is corrected with sector gain map
C!   Modified :- D. Casper             02-NOV-1995
C!               include pad DEDX
C?
C--------------------------------------------------------------------
#ifndef DOC
C
#include "bcs.h"
#include "alcons.h"
#include "tpgpar.h"
#include "tpgeom.h"
#include "tpgeow.h"
#include "tpgeop.h"
#include "tc1xjj.h"
#include "tmonit.h"
#include "tgtljj.h"
#include "tgftjj.h"
#include "texsjj.h"
#include "tpxsjj.h"
#include "tpcojj.h"
#include "tbcojj.h"
#include "telsjj.h"
#include "tplsjj.h"
#include "rconds.h"
#include "rparac.h"
#include "tdxcjj.h"
C
      DIMENSION ISLCT(LTSECT)
      LOGICAL FIRST
      DATA PIMAS /.13957/
      DATA FIRST/.TRUE./
      DATA IRUNO / 0 /
C
#include "bmacro.h"
C
      IF (FIRST) THEN
        FIRST=.FALSE.
        NTELS=NAMIND('TELS')
        NTGFT=NAMIND('TGFT')
        NTGCL=NAMIND('TGCL')
        NTGTL=NAMIND('TGTL')
        NTPCO=NAMIND('TPCO')
        NTBCO=NAMIND('TBCO')
        NTEXS=NAMIND('TEXS')
        NTELS=NAMIND('TELS')
        NTC1X=NAMIND('TC1X')
        NTDXC=NAMIND('TDXC')
        NTPXS=NAMIND('TPXS')
        NTPLS=NAMIND('TPLS')
        KTDXC = IW(NTDXC)
        IF (KTDXC.NE.0) THEN
          WRMP = FLOAT(ITABL(KTDXC,1,JTDXFW))/100.
          CTMX = RTABL(KTDXC,1,JTDXCT)
          MNCO = ITABL(KTDXC,1,JTDXTC)
          PRMN = RTABL(KTDXC,1,JTDXFP)
          PMIN = RTABL(KTDXC,1,JTDXMI)
          PMAX = RTABL(KTDXC,1,JTDXMX)
          MNTK = ITABL(KTDXC,1,JTDXGT)
          RPMI = RTABL(KTDXC,1,JTDXLI)
          RPMX = RTABL(KTDXC,1,JTDXUI)
          D0MX = RTABL(KTDXC,1,JTDXD0)
          Z0MX = RTABL(KTDXC,1,JTDXZ0)
          MNWR = ITABL(KTDXC,1,JTDXWT)
        ENDIF
        MNTK=MAX(1,MNTK)
      ENDIF
C
C++   Count number of tracks in each sector
C
      NEVTTM=NEVTTM+1
      KTGTL=IW(NTGTL)
      KTGCL=IW(NTGCL)
      KTPCO=IW(NTPCO)
      IF (KTGTL.NE.0 .AND. KTGCL.NE.0 .AND. KTPCO.NE.0) THEN
        DO 100 ITK=1,LROWS(KTGTL)
          NSL=0
          DO 50 II=1,ITABL(KTGTL,ITK,JTGTN1)
            IC=IW((KTGCL+LMHLEN+ITABL(KTGTL,ITK,JTGTIO))+II)
            ISLOT=MOD(ITABL(KTPCO,IC,JTPCIN),100000)/1000
            DO 30 J=1,NSL
              IF (ISLCT(J).EQ.ISLOT) GO TO 50
   30       CONTINUE
            NSL=NSL+1
            ISLCT(NSL)=ISLOT
   50     CONTINUE
          DO 60 J=1,NSL
            NTKSTM(ISLCT(J))=NTKSTM(ISLCT(J))+1
   60     CONTINUE
  100   CONTINUE
      ENDIF
C
C++   Count number of used & unused good coordinates in each sector
C
      IF (KTPCO.NE.0) THEN
        DO 200 IC=1,LROWS(KTPCO)
          ITK=ITABL(KTPCO,IC,JTPCTN)
          ISLOT=MOD(ITABL(KTPCO,IC,JTPCIN),100000)/1000
          IF (ITK.EQ.0) THEN
            NUUCTM(ISLOT)=NUUCTM(ISLOT)+1
          ELSE
            NUSCTM(ISLOT)=NUSCTM(ISLOT)+1
          ENDIF
  200   CONTINUE
      ENDIF
C
C++   Count number of bad coordinates in each sector
C
      KTBCO=IW(NTBCO)
      IF (KTBCO.NE.0) THEN
        DO 300 IC=1,LROWS(KTBCO)
          ISLOT=MOD(ITABL(KTBCO,IC,JTBCIN),100000)/1000
          NBADTM(ISLOT)=NBADTM(ISLOT)+1
  300   CONTINUE
      ENDIF
C
C++   Find the average dE/dx per sector
C
      IOFF = JULTP*1000 + 100
      KTGFT = IW(NTGFT)
      KTEXS = IW(NTEXS)
      KTELS = IW(NTELS)
      KTC1X = IW(NTC1X)
      KTDXC = IW(NTDXC)
      KTPXS = IW(NTPXS)
      KTPLS = IW(NTPLS)
      IF (KTGFT.GT.0 .AND. KTGTL.GT.0
     &    .AND. KTEXS.GT.0 .AND. KTELS.GT.0 .AND. KTC1X.GT.0 .AND.
     &    KTDXC.GT.0 ) THEN
        RTRNK = 100./FLOAT(ITABL(KTC1X,1,JTC1TP))
C
C++       Select tracks of good quality
C
        DO 500 ITK=1,LROWS(KTGFT)
C
C?   TPC coordinates
C
          NCO=ITABL(KTGTL,ITK,JTGTN1)
          IF (NCO.LT.MNCO) GO TO 500
C
C?   Momentum cut and cos(theta) cut
C
          RC=1./RTABL(KTGFT,ITK,JTGFIR)
          PT=RC*CLGHT*FIELRC/100000.
          TL=RTABL(KTGFT,ITK,JTGFTL)
          P=ABS(PT)*SQRT(1.+TL**2)
          IF (P.LT.PMIN) GO TO 500
          IF (P.GT.PMAX) GO TO 500
          CT = SQRT(TL**2/(1.+TL**2))
          IF (CT.GT.CTMX) GO TO 500
C
C?   Vertex cut
C
          D0V=RTABL(KTGFT,ITK,JTGFD0)
          IF (ABS(D0V).GT.D0MX) GO TO 500
          Z0V=RTABL(KTGFT,ITK,JTGFZ0)
          IF (ABS(Z0V).GT.Z0MX) GO TO 500
C
C?   Cut on fit probability
C
          CHI2=RTABL(KTGFT,ITK,JTGFCD)
          NFRE=ITABL(KTGFT,ITK,JTGFDF)
          FPRO=PROB(CHI2,NFRE)
          IF (FPRO.LT.PRMN) GO TO 500
C
C++       Get RAW dE/dx information
C
          DO 405 II=1,LROWS(KTELS)
            IF (ITK.EQ.ITABL(KTELS,II,JTELTN)) THEN
              TRMN = 0.0
              NS = 0
              DO 400 JJ=1,ITABL(KTELS,II,JTELNG)
                ITX=ITABL(KTELS,II,JTELSO)+JJ
                NWIR=ITABL(KTEXS,ITX,JTEXNS)
                ISLOT=ITABL(KTEXS,ITX,JTEXSI)
                ITYP=ITPTYP(ISLOT)
                DEDX=RTABL(KTEXS,ITX,JTEXTM)
                RATW=FLOAT(NWIR)*RTRNK/FLOAT(NTWIRE(ITYP))
                IF (RATW.GE.WRMP) THEN
C
C?  Fill sector to sector gain calibration histograms which later will be
C?  put into the JHDX banks ( a run header offline dE/dX calibration bank ).
C
                  CALL HFILL (IOFF+ISLOT,DEDX,0.,1.)
C
C?   Get Sector dE/dX information and store it in TMONIT, this is only used for
C?   Julia informative output , set dE/dX range 0.5 - 1.5 to avoid  e- tails.
C
                  IF ( DEDX.GE..5 .AND. DEDX.LE.1.5 ) THEN
                    NDEXTM(ISLOT)=NDEXTM(ISLOT)+1
                    DEDXTM(ISLOT)=DEDXTM(ISLOT)+DEDX
                    DEX2TM(ISLOT)=DEX2TM(ISLOT)+DEDX**2
                  ENDIF
                ENDIF
C
C?  Get this track's dE/dX by taking weighted NWIR mean of each track's segment
C?  Caution: track's trancated mean is corrected by previous runs gain map !
C
                TRMN = TRMN + FLOAT(NWIR)*DEDX*GAINTM(ISLOT)
                NS = NS + NWIR
  400         CONTINUE
C
              IF (NS.LT.MNWR) THEN
                GO TO 405
              ELSE
                RI=TRMN/FLOAT(NS)
              ENDIF
C
C? Fill in the global gain calibration histogram which later will be put in
C? to JHDX bank.( used for monitoring the pressure variation )
C
              CALL HFILL (IOFF+LTSECT+1,RI,0.,1.)
C
C?  Store this track's dE/dX value into the monitoring common blocks (TMONIT)
C?  these valuse will only be used for Julia informative output.
C
              IF ( RI.GE.0.5 .AND. RI.LE.1.5 ) THEN
                NTDXTM=NTDXTM+1
                TDEXTM=TDEXTM+RI
                TDX2TM=TDX2TM+RI**2
              ENDIF
              GO TO 406
            ENDIF
  405     CONTINUE
  406     CONTINUE
C
C++       Get RAW pad dE/dx information
C
          IF(KTPLS.GT.0 .AND. KTPXS.GT.0)THEN
            DO 407 II=1,LROWS(KTPLS)
                IF (ITK.EQ.ITABL(KTPLS,II,JTPLTN)) THEN
                    TRMN = 0.0
                    XS = 0.0
                    DO 1400 JJ=1,ITABL(KTPLS,II,JTPLNG)
                        ITX=ITABL(KTPLS,II,JTPLSO)+JJ
                        XPAD=RTABL(KTPXS,ITX,JTPXNS)
                        ISLOT=ITABL(KTPXS,ITX,JTPXSI)
                        DEDX=RTABL(KTPXS,ITX,JTPXTM)
C
C?  Fill sector to sector gain monitoring histograms
C
                        CALL HFILL (IOFF+100+ISLOT,DEDX,0.,1.)
C
C?   Get Sector dE/dX information and store it in TMONIT, this is only
C?   used for Julia informative output , set dE/dX range 0.5 - 1.5 to
C?   avoid  e- tails.
C
                        IF ( DEDX.GE..5 .AND. DEDX.LE.1.5 ) THEN
                            NDPXTM(ISLOT)=NDPXTM(ISLOT)+1
                            DPDXTM(ISLOT)=DPDXTM(ISLOT)+DEDX
                            DPX2TM(ISLOT)=DPX2TM(ISLOT)+DEDX**2
                        ENDIF
C
C?  Get this track's dE/dX by taking weighted NWIR mean of each track's
C?  segment
C?  Caution: track's trancated mean is corrected by previous runs gain
C?  map !
C
                        TRMN = TRMN + XPAD*DEDX
                        XS = XS + XPAD
 1400               CONTINUE
C
                    IF(XS.EQ.0)THEN
                        GO TO 407
                    ELSE
                        RI=TRMN/XS
                    ENDIF
C
C? Fill in the global gain monitoring histogram
C
                    CALL HFILL (IOFF+100+LTSECT+1,RI,0.,1.)
C
C?  Store this track's dE/dX value into the monitoring common blocks
C?  (TMONIT) these values will only be used for Julia informative
C?  output.
C
                    IF ( RI.GE.0.5 .AND. RI.LE.1.5 ) THEN
                        NTPXTM=NTPXTM+1
                        TDPXTM=TDPXTM+RI
                        TPX2TM=TPX2TM+RI**2
                    ENDIF
                    GO TO 408
                ENDIF
  407       CONTINUE
  408       CONTINUE
          ENDIF
  500   CONTINUE
      ENDIF
C
      RETURN
      END
#endif
