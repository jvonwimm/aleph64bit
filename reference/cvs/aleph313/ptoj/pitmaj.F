      SUBROUTINE PITMAJ(LIST,FIELD,IER)
C
C-----------------------------------------------------------------------
CKEY PTOJ ITC
C! Unpack the POT banks PITM and PASL, to the ITMA and IASL banks.
C
C     Author:   R. Johnson   10-1-89 (original name : PTGMAJ)
C     Modified: I. Tomalin   15-3-89 to unpack combined ITC-TPC banks.
C     Modified: I. Tomalin   01-6-89 to include scattering.
C
C     Input :    LIST      BOS event list
C                          if LIST(2:2).eq.'-' drop POT banks
C                FIELD     Signed Magnetic Field in KG.
C     Output:    IER       = 0  successful
C                          = 1  input bank does not exist or is empty
C                          = 2  not enough space
C                          =-1  OK but garbage collection
C
C    Calls     : SMINV,TRNHPA,UINCOV
C
C    Libraries required : BOS, ALEPHLIB
C
C? Create ITMA and IASL banks.
C? Loop over the FRFT tracks.
C?   Load information from PITM and PASL banks into the new banks.
C? End loop.
C? Find the no. of ITC + TPC hits on each track and put into ITMA bank.
C? Find CHI**2 of comparison between helix params. of FRFT and FKIN
C? tracks. Put this into the IASL bank.
C? Sort out the bank lists.
C
C-----------------------------------------------------------------------
#ifndef DOC
      SAVE
C
#include "bcs.h"
#include "pitmjj.h"
#include "pasljj.h"
#include "itmajj.h"
#include "iasljj.h"
#include "frtljj.h"
#include "frftjj.h"
#include "fxjjpar.h"
#include "partjj.h"
C
      CHARACTER LIST*(*),PLIST*4,JLIST*4
      DIMENSION COV(15),CORR(15),VV0(5),DIFF(5),HELIX(5),P(3),X(3),XC(2)
      LOGICAL FIRST
      DATA FIRST/.TRUE./
C
#include "bmacro.h"
C
      IF (FIRST) THEN
        FIRST = .FALSE.
        NPITM = NAMIND('PITM')
        NPASL = NAMIND('PASL')
        NITMA = NAMIND('ITMA')
        NIASL = NAMIND('IASL')
        NFRTL = NAMIND('FRTL')
        NFRFT = NAMIND('FRFT')
        NFKIN = NAMIND('FKIN')
        NFVER = NAMIND('FVER')
        NPART = NAMIND('PART')
        CALL BKFMT('ITMA','I')
        CALL BKFMT('IASL','2I,(2I,F)')
      ENDIF
C
      IER = 1
      KPITM = IW(NPITM)
      IF (KPITM.EQ.0) GO TO 999
      KPASL = IW(NPASL)
      IF (KPASL.EQ.0) GO TO 999
C
      IER = 0
      IF (IW(NITMA).NE.0) CALL BDROP(IW,'ITMA')
      IF (IW(NIASL).NE.0) CALL BDROP(IW,'IASL')
C
      LEN=LROWS(KPITM)*LITMAA+LMHLEN
      CALL AUBOS('ITMA',0,LEN,KITMA,IGARB)
      IF (IGARB.EQ.2) GO TO 998
      IF (IGARB.EQ.1) THEN
        KPASL = IW(NPASL)
        KPITM = IW(NPITM)
      END IF
      IW(KITMA+LMHCOL) = LITMAA
      IW(KITMA+LMHROW) = LROWS(KPITM)
C
      LEN = LROWS(KPASL)*LIASLA + LMHLEN
      CALL AUBOS('IASL',0,LEN,KIASL,IGARB)
      IF (IGARB.EQ.2) GO TO 998
      IF (IGARB.EQ.1) THEN
        KPASL = IW(NPASL)
        KPITM = IW(NPITM)
        KITMA = IW(NITMA)
      END IF
      IW(KIASL+LMHCOL) = LIASLA
      IW(KIASL+LMHROW) = LROWS(KPASL)
C
C++   Copy information from the POT banks
C
      IOFF = 0
C Loop over the FRFT tracks.
      DO 500 ITK = 1,LROWS(KITMA)
C Note number of FKIN tracks associated with FRFT track "ITK".
        IW(KROW(KITMA,ITK)+JITMNA) = ITABL(KPITM,ITK,JPITNA)
C Note row offset to these FKIN tracks in the IASL bank.
        IW(KROW(KITMA,ITK)+JITMOF) = IOFF
C Note the number of ITC + TPC hits on track (updated later).
        IW(KROW(KITMA,ITK)+JITMNC) = 0
C
C Loop over the FKIN tracks associated with them.
        DO 300 II = 1,ITABL(KPITM,ITK,JPITNA)
          IOFF = IOFF + 1
C Note the FKIN track number ...
          IW(KROW(KIASL,IOFF)+JIASFK) = ITABL(KPASL,IOFF,JPASFK)
C ... and the number of hits associated with it.
          IW(KROW(KIASL,IOFF)+JIASNH) = ITABL(KPASL,IOFF,JPASNH)
C Note the CHI**2 of the FKIN to JULIA fit (updated later).
          RW(KROW(KIASL,IOFF)+JIASCS) = 999999.
  300   CONTINUE
  500 CONTINUE
C
C++   Calculate the remaining information
C
      KFRTL = IW(NFRTL)
C First get the number of ITC + TPC hits on the FRFT track.
      IF (KFRTL.GT.0) THEN
        DO 600 ITK = 1,LROWS(KITMA)
          IW(KROW(KITMA,ITK)+JITMNC) = ITABL(KFRTL,ITK,JFRTNT) +
     +                                    ITABL(KFRTL,ITK,JFRTNI)
  600   CONTINUE
      ENDIF
C
      KFRFT = IW(NFRFT)
      KFKIN = IW(NFKIN)
      KFVER = IW(NFVER)
      KPART = IW(NPART)
C Then find out how well the track parameters of the FRFT and FKIN track
C match (at their point of closest approach to the origin).
      IF (KFRTL.NE.0.AND.KFRFT.NE.0.AND.KFKIN.NE.0.AND.KPART.NE.0.AND.
     +KFVER.NE.0) THEN
C
C Loop over the FRFT tracks.
        DO 800 ITK = 1,LROWS(KITMA)
C
          DO 610 J=1,5
            VV0(J) = RTABL(KFRFT,ITK,J)
 610      CONTINUE
          DO 625 J=1,15
            COV(J) = RTABL(KFRFT,ITK,JFRFEM-1+J)
 625      CONTINUE
C
C Transform to include effects of scattering.
          IOPT = MOD(ITABL(KFRFT,ITK,JFRFNO),100)/10
          NCTPC = ITABL(KFRTL,ITK,JFRTNT)
          NCITC = ITABL(KFRTL,ITK,JFRTNI)
          NCMVD = ITABL(KFRTL,ITK,JFRTNV)
          IR = 0
          IF (ABS(VV0(4)).GT.9.5) IR = 1
          IF (ABS(VV0(4)).GT.29.6) IR = 2
          CALL UINCOV(IR,IOPT,NCTPC,NCITC,NCMVD,FIELD,VV0,COV,CORR,JER)
C Check that the covariance matrix is positive definate.
          IF (CORR(1).LE.0.0) GOTO 800
          IF (CORR(3).LE.0.0) GOTO 800
          IF (CORR(6).LE.0.0) GOTO 800
          IF (CORR(10).LE.0.0) GOTO 800
          IF (CORR(15).LE.0.0) GOTO 800
C Invert to give the correlation matrix.
          CALL SMINV(CORR,DUM,5,0,NRANK)
C Give up on this track if the matrix inversion failed.
          IF (NRANK.NE.5) GOTO 800
C
C Loop over the tracks associated FKIN tracks.
          IOFF = ITABL(KITMA,ITK,JITMOF)
          DO 700 II = 1,ITABL(KITMA,ITK,JITMNA)
            IMT = IOFF + II
C Get the FKIN number of the track.
            IFKI = ITABL(KIASL,IMT,JIASFK)
C Get its particle type.
            ITYP = ITABL(KFKIN,IFKI,JFKIPA)
            Q = RTABL(KPART,ITYP,JPARCH)
            IF (Q.NE.0.0) THEN
              P(1) = RTABL(KFKIN,IFKI,JFKIPX)
              P(2) = RTABL(KFKIN,IFKI,JFKIPY)
              P(3) = RTABL(KFKIN,IFKI,JFKIPZ)
              IF (P(1).NE.0.0.OR.P(2).NE.0.0) THEN
                IVT = ITABL(KFKIN,IFKI,JFKIOV)
                X(1) = RTABL(KFVER,IVT,JFVEVX)
                X(2) = RTABL(KFVER,IVT,JFVEVY)
                X(3) = RTABL(KFVER,IVT,JFVEVZ)
                CALL TNRHPA(P,X,Q,FIELD,HELIX,XC,S)
C
                CHISQ = 0.0
                IDX = 0
                DO 675 I = 1,5
                  DIFF(I) = HELIX(I) - RTABL(KFRFT,ITK,JFRFIR-1+I)
                  DO 670 J = 1,I
                    IDX = IDX + 1
                    ADD = DIFF(I)*CORR(IDX)*DIFF(J)
                    IF (I.NE.J) ADD = 2.0*ADD
                    CHISQ = CHISQ + ADD
  670             CONTINUE
  675           CONTINUE
              ENDIF
            ENDIF
            RW(KROW(KIASL,IMT)+JIASCS) = CHISQ
  700     CONTINUE
  800   CONTINUE
      ENDIF
C
C++   Get the drop flag if any, then drop POT banks if required;
C     add JUL banks to S-list
C     POT banks are on PLIST, JUL banks on JLIST
C
      PLIST = 'PITMPASL'
      JLIST = 'ITMAIASL'
#include "jplist.h"
C
      GOTO 999
C
  998 CONTINUE
C Error return.
      CALL BDROP(IW,'ITMAIASL')
  999 CONTINUE
      END
#endif
