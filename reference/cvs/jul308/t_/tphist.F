      SUBROUTINE TPHIST
C
C----------------------------------------------------------------
C! TPC histograms
C! for analysis of coordinate- and track-finding efficiency.
C!
C!    Author:   M. Mermikides
C!    Modified: R. Johnson      8/04/88
C!    Modified: I. Tomalin     23/06/88
C!
C----------------------------------------------------------------
#ifndef DOC
C
#include "rparac.h"
#include "bcs.h"
#include "tparac.h"
#include "tpgpar.h"
#include "tpchjj.h"
#include "tphtjj.h"
#include "tpcojj.h"
#include "tgftjj.h"
#include "tgmajj.h"
#include "tmtljj.h"
#include "tgtljj.h"
#include "trcljj.h"
C
      COMMON/TPHIWB/ JIDWBK,JINDXW
C
      PARAMETER (JOFF=JULTP*1000, MXNCT=200)
      DIMENSION NCT(MXNCT),NKI(MXNCT),NSH(MXNCT)
      LOGICAL FIRST
      DATA FIRST/.TRUE./
C
C-----------------Statement function-------------------------------
C
#include "bmacro.h"
C
C------------------------------------------------------------------
C
      IF (FIRST) THEN
        FIRST=.FALSE.
        JIDWBK=0
        JINDXW=0
        NTPHT=NAMIND('TPHT')
        NTPCO=NAMIND('TPCO')
        NTPCH=NAMIND('TPCH')
        NTGFT=NAMIND('TGFT')
        NTGMA=NAMIND('TGMA')
        NTMTL=NAMIND('TMTL')
        NTSCL=NAMIND('TSCL')
        NTGCL=NAMIND('TGCL')
        NTGTL=NAMIND('TGTL')
        NTRCL=NAMIND('TRCL')
      ENDIF
C
C++   Do first those histograms not requiring MC truth information
C
C++   Histogram number of TPC coordinates
C
      KTPCO = IW(NTPCO)
      IF (KTPCO.EQ.0) THEN
        NCO=0
      ELSE
        NCO = LROWS(KTPCO)
      ENDIF
      CALL HFILL(JOFF+2,FLOAT(NCO),0.,1.)
C
C++   Fraction of coordinates not on a track
C
      NMISS=0.
      DO 237 I=1,NCO
        IF (ITABL(KTPCO,I,JTPCTN).EQ.0) NMISS=NMISS+1
  237 CONTINUE
      IF (NCO.GT.0) THEN
         FRAC = FLOAT(NMISS)/FLOAT(NCO)
         CALL HFILL(JOFF+6,FRAC,0.,1.)
      ENDIF
C
C++   Number of found tracks
C
      KTGFT = IW(NTGFT)
      IF (KTGFT.EQ.0) THEN
        NTK=0
      ELSE
        NTK=LROWS(KTGFT)
      ENDIF
      CALL HFILL(JOFF+13,FLOAT(NTK),0.,1.)
C
C++   Chisq/ndof, Proj track length, Number of coords on found tracks
C
      KTGFT=IW(NTGFT)
      KTGCL=IW(NTGCL)
      KTGTL=IW(NTGTL)
      KTPCO=IW(NTPCO)
      IF (KTGFT.NE.0 .AND. KTGCL.NE.0 .AND. KTGTL.NE.0
     &                                       .AND. KTPCO.NE.0) THEN
        DO 300 IT=1,LROWS(KTGFT)
          NHT1 = ITABL(KTGTL,IT,JTGTN1)
          NHT = ITABL(KTGTL,IT,JTGTNR) + NHT1
          JH = ITABL(KTGTL,IT,JTGTIO)
          IC1 = IW(KTGCL+LMHLEN+JH+1)
          IC2 = IW(KTGCL+LMHLEN+JH+NHT)
          DZ = RTABL(KTPCO,IC1,JTPCZV)-RTABL(KTPCO,IC2,JTPCZV)
          NDF = ITABL(KTGFT,IT,JTGFDF)
          CHI = RTABL(KTGFT,IT,JTGFCD)
          PRB = PROB(CHI,NDF)
          TANL = RTABL(KTGFT,IT,JTGFTL)
          IF (TANL.NE.0.) THEN
            SLEN = ABS(DZ/TANL)
          ELSE
            SLEN = 999.
          ENDIF
          CALL HFILL(JOFF+15,PRB,0.,1.)
          CALL HFILL(JOFF+16,SLEN,0.,1.)
          CALL HFILL(JOFF+20,FLOAT(NHT1),0.,1.)
          CALL HFILL(JOFF+17,FLOAT(NHT),0.,1.)
  300   CONTINUE
      ENDIF
C
C++   Now fill those histograms requiring MC truth information
C
      KTPHT = IW(NTPHT)
      IF (KTPHT.EQ.0) GO TO 999
      KTPCO=IW(NTPCO)
      IF (KTPCO.EQ.0) GO TO 999
C
      NHITS = IW(KTPHT+LMHROW)
      CALL HFILL(JOFF+1,FLOAT(NHITS),0.,1.)
C
      IF (NHITS.GT.0) THEN
        EFFIC = FLOAT(LROWS(KTPCO))/FLOAT(NHITS)
        CALL HFILL(JOFF+3,EFFIC,0.,1.)
      ENDIF
C
C++   Resolution obtained by comparing reconstructed coordinates to
C++   hits.   Relation between the two is given by the bank 'TPCH'.
C
      KTPCH = IW(NTPCH)
      IF (KTPCH.NE.0) THEN
C
C++     Make sure the TPCH bank matches the TPCO bank
C
        IF (LROWS(KTPCH).NE.LROWS(KTPCO)) THEN
          KTPCH=NDROP('TPCH',0)
        ENDIF
      ENDIF
      IF (KTPCH.EQ.0) GO TO 999
C
C++   Number of MC tracks in each cluster
C
      KTRCL=IW(NTRCL)
  425 IF (KTRCL.EQ.0) GO TO 428
        DO 426 ICL=1,LROWS(KTRCL)
          NHTCL=ITABL(KTRCL,ICL,JTRCNH)
          CALL HFILL(JOFF+10,FLOAT(NHTCL),0.,1.)
  426   CONTINUE
        KTRCL=IW(KTRCL-1)
        GO TO 425
  428 CONTINUE
C
C++   Histogram coordinate errors with respect to MC points
C
      KTPCO=IW(NTPCO)
      KTPHT=IW(NTPHT)
      DO 20 I = 1,LROWS(KTPCO)
        IFL = ITABL(KTPCO,I,JTPCTN)
        IREF = ITABL(KTPCH,I,JTPCIH)
        IF (IREF.EQ.0) GO TO 20
        RAD=  RTABL(KTPCO,I,JTPCRV)
        DPHI= RTABL(KTPCO,I,JTPCPH) - RTABL(KTPHT,IREF,JTPHPH)
        DZ=   RTABL(KTPCO,I,JTPCZV) - RTABL(KTPHT,IREF,JTPHZV)
        DW=   RAD*DPHI
        CALL HFILL(JOFF+4,DW,0.,1.)
        CALL HFILL(JOFF+5,DZ,0.,1.)
        PULL = DW/SQRT(RTABL(KTPCO,I,JTPCSR))
        CALL HFILL(JOFF+8,PULL,0.,1.)
        PULL = DZ/SQRT(RTABL(KTPCO,I,JTPCSZ))
        CALL HFILL(JOFF+9,PULL,0.,1.)
   20 CONTINUE
C
C++   Find no of distinct MC tracks (NTRK) giving hits and number of
C++   coordinates NCT(i) on each.
C
      CALL WBANK(IW,JIDWBK,LROWS(KTPCO),*999)
      IW(JIDWBK-3)=INTCHA('WIDW')
      CALL WBANK(IW,JINDXW,LROWS(KTPCO),*999)
      IW(JINDXW-3)=INTCHA('WNDX')
      NN = 0
      DO 25 I = 1,LROWS(KTPCO)
        IREF = ITABL(KTPCH,I,JTPCIH)
        IF (IREF.EQ.0) GO TO 25
        NN = NN + 1
        IW(JIDWBK+NN) = ITABL(KTPHT,IREF,JTPHKT)
   25 CONTINUE
C
C++   Sort hits according to kin. track number
C
      NTRK = 0
      IF (NN.GE.1) THEN
        CALL SORTZV(IW(JIDWBK+1),IW(JINDXW+1),NN, -1, 0, 0 ,0 ,0)
        JT = IW(JINDXW+1)
        IDLST = IW(JIDWBK+JT)
        NTRK = 1
        NCT(NTRK) = 1
        DO 28 I = 2,NN
          JT = IW(JINDXW+I)
          IF (IW(JIDWBK+JT).EQ.IDLST) THEN
            NCT(NTRK) = NCT(NTRK) + 1
          ELSE
            NKI(NTRK)=IDLST
            CALL HFILL(JOFF+7,FLOAT(NCT(NTRK)),0.,1.)
            IDLST = IW(JIDWBK+JT)
            IF (NTRK.EQ.MXNCT) GO TO 29
            NTRK = NTRK + 1
            NCT(NTRK) = 1
          ENDIF
   28   CONTINUE
   29   CONTINUE
        NKI(NTRK)=IDLST
        CALL HFILL(JOFF+7,FLOAT(NCT(NTRK)),0.,1.)
      ENDIF
      CALL WDROP(IW,JIDWBK)
      CALL WDROP(IW,JINDXW)
C
C++   No of MC tracks entering, and no. with > 3 hits
C
      CALL HFILL(JOFF+11,FLOAT(NTRK),0.,1.)
      NGOOD = 0
      DO 35 I=1,NTRK
        IF(NCT(I).GT.3) NGOOD = NGOOD + 1
   35 CONTINUE
      CALL HFILL(JOFF+12,FLOAT(NGOOD),0.,1.)
  650 CONTINUE
C
      KTGFT = IW(NTGFT)
      IF (KTGFT.EQ.0) GO TO 999
C
C++   Track-finding efficiency
C
      MTGFT = LROWS(KTGFT)
      IF (NGOOD.GT.0) THEN
        EFFT = FLOAT(MTGFT)/FLOAT(NGOOD)
        CALL HFILL(JOFF+14,EFFT,0.,1.)
      ENDIF
C
      DO 410 I=1,MIN(NTRK,MXNCT)
        NSH(I)=0
  410 CONTINUE
C
      KTGMA=IW(NTGMA)
      KTMTL=IW(NTMTL)
      IF (KTGMA.GT.0 .AND. KTMTL.GT.0) THEN
        DO 421 ITK=1,IW(KTGMA+LMHROW)
C Get the number of MC tracks associated with JULIA track ITK.
          NMC=ITABL(KTGMA,ITK,JTGMNM)
C Find the total number of hits on JULIA track ITK.
          NHITK=ITABL(KTGMA,ITK,JTGMNC)
          CALL HFILL(JOFF+18,FLOAT(NMC),0.,1.)
          NSHAR=0
C Loop over the true MC tracks sharing hits with JULIA track ITK.
          DO 419 J=1,NMC
            IMC= J + ITABL(KTGMA,ITK,JTGMOM)
C Note the KINE number of this true MC track.
            IKINE=ITABL(KTMTL,IMC,JTMTMT)
C Note the number of hits which it shares with JULIA track ITK.
            ISHAR=ITABL(KTMTL,IMC,JTMTNH)
            IF (ISHAR.GE.6)
     +        CALL HFILL(JOFF+19,RTABL(KTMTL,IMC,JTMTCS),0.,1.)
C Note the true MC track which shares the most hits with JULIA track ITK
            IF (ISHAR.GT.NSHAR) THEN
              JBEST=IKINE
              NSHAR=ISHAR
            END IF
C If true MC track IKINE shares more of its hits with JULIA track ITK
C than with any other JULIA track, note the number of hits shared.
            IOFF=1
  413       IF (NKI(IOFF).EQ.IKINE.OR.IOFF.GT.MIN(NTRK,MXNCT)) GOTO 415
            IOFF=IOFF+1
            GOTO 413
  415       IF (ISHAR.GT.NSH(IOFF)) NSH(IOFF)=ISHAR
  419     CONTINUE
C  Plot the fraction of the hits on JULIA track ITK which were shared
C  with its closest true MC track.
          FRAC=FLOAT(NSHAR)/FLOAT(NHITK)
          CALL HFILL(JOFF+21,FRAC,0.0,1.0)
  421   CONTINUE
C  Plot the fraction of the hits on each true MC track which were
C  shared with its closest JULIA track.
        DO 430 L=1,NTRK
          FRAC=FLOAT(NSH(L))/FLOAT(NCT(L))
          CALL HFILL(JOFF+22,FRAC,0.0,1.0)
  430   CONTINUE
      ENDIF
  999 RETURN
      END
#endif
