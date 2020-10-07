
      SUBROUTINE QFMTCH
CKEY FILL MC MATCH/INTERNAL
C-----------------------------------------------------------------------
C   Author   : D. CINABRO, H. ALBRECHT   5-OCT-1988
C
C   Description
C   ===========
C!Fill match infomation into QVEC
C
C 05/05/88 Rewritten 14/10/88 to add ITC Altered 26/04/89 to use
C ITMA and IASL
C
C Uses the TGMA,TMTL,IPJT, and IRJT; or the ITMA and IASL banks to relat
C FRFT track to FKIN tracks.
C
C TGMA and IPJT1 are parallel with the first tracks in FRFT.  First use
C TGMA and TMTL to get the MC track number and the number of associated
C hits in the TPC.  Then use IPJT1 and IRJT1 to add extra hits and possi
C tracks from the ITC.  Store the hits and tracks in IHIT and IPART.  No
C that FPOI is necessary as IPJT points to KINE tracks, so the tranlatio
C to FKIN is done via FPOI.  Store in QVEC and point in QMTL/QMTS.  For
C IPJT2 it is the same but simpler as extra tracks from another detector
C do not have to be considered.
C
C The use of ITMA and IASL is exactly as ITJT2 as the information from
C the ITC and TPC are combined in these banks.  FPOI is not necessary
C in this case as the banks point to FKIN.  These banks are used
C preferntially and the others are used if ITMA and IASL are not
C present.
C
C If more than 20 MC tracks are associated to one MC particle then
C there will be an overflow.  An error message is printed and we go
C to the next track.
C
C  Called from QFILMC
C
C-----------------------------------------------------------------------
#ifndef DOC
      SAVE IERR,IERCT
#include "qcdesh.h"
C
#include "tgmajj.h"
#include "tmtljj.h"
#include "ipjtjj.h"
#include "irjtjj.h"
#include "iasljj.h"
#include "itmajj.h"
C
      PARAMETER (MXMC=20)
      DATA IERR,IERCT /0,0/
C
C The common used to sort the pointers in FTTM to the MC tracks by the
C number of hits associated with the MC tracks
      COMMON/SORT/IHIT(MXMC),IPART(MXMC)
#include "bmacro.h"
C-----------------------------------------------------------------------
C
C Other statement functions
C
C       address of JQVENM for track I
      INM(I) = KOQVEC + I * KCQVEC + JQVENM
C       address of JQVEML for track I
      IML(I) = KOQVEC + I * KCQVEC + JQVEML
C-----------------------------------------------------------------------
C
C If there are no charged tracks forget the whole thing
C
      IF (KNCHT .EQ. 0)  GO TO 999
C
C Are ITMA and IASL present
C
      IFLAG = 0
      JITMA = IW(NAITMA)
      JIASL = IW(NAIASL)
      IF (JITMA.LE.0.AND.JIASL.LE.0) THEN
C
C OK. See if the other guys are here
C
        JFPOI = IW(NAFPOI)
        JTGMA = IW(NATGMA)
        JIPJ1 = NLINK('IPJT',1)
        IF (JTGMA.LE.0.OR.JIPJ1.LE.0..OR.JFPOI.LE.0) THEN
          IERCT=IERCT+1
          IF(IERCT.LE.5)
     &     CALL QWMESE('_QFMTCH_ Unable to fill matching information')
          IF(IERCT.EQ.1)
     &      CALL QWMESS('_QFMTCH_ Warning message printed only 5 times')
          GO TO 999
        ELSE
          IFLAG = 1
        ENDIF
        JIPJ2 = NLINK('IPJT',2)
        NTTRKS = LROWS(JTGMA)
        NITRKS = LROWS(JIPJ2)
        JTMTL = IW(NATMTL)
        IF (JTMTL.LE.0) GOTO 999
        NPOIT = LROWS(JTMTL)
        JIRJ1 = NLINK('IRJT',1)
        NPOI1 = LROWS(JIRJ1)
        JITJ2 = NLINK('IRJT',2)
        NPOI2 = LROWS(JITJ2)
        NPOI = NPOIT+NPOI1+NPOI2
      ELSE
        NTRKS=LROWS(JITMA)
      ENDIF
C
C Fill the banks.  First branch around to us ITMA and IASL
C
      LQVEC = KOQVEC + KFCHT * KCQVEC
      IF (IFLAG.EQ.0) GOTO 410
C
C Using the old stuff
C
      IF (NTTRKS.LE.0) GOTO 301
      DO 300 I=1,NTTRKS
        NPOT = ITABL(JTGMA,I,JTGMNM)
        NPOI = ITABL(JIPJ1,I,JIPJNS)
        IF ((NPOT+NPOI).EQ.0) GO TO 300
C
C Here if too many MC tracks associated with a charged track print
C an error message and get out.
C
        IF ((NPOT+NPOI).GT.MXMC) THEN
          IERR = IERR + 1
          IF (IERR .LE. 20)  CALL QWMESE
     &         ('_QFMTCH_ Unable to fill matching information')
          GO TO 300
        ENDIF
C
C First get MC tracks and associated number of hits to the reconstructed
C track, for the TPC
C
        IPOT = ITABL(JTGMA,I,JTGMOM)
        DO 100 J=1,NPOT
          IHIT(J) = ITABL(JTMTL,IPOT+J,JTMTNH)
          IPART(J) = ITABL(JTMTL,IPOT+J,JTMTMT)
  100   CONTINUE
C
C Now for the ITC, if same MC particle as all ready associated in TPC
C simply add hits, if not add a new particle and number of hits
C
        NIOF = 0
        IPOI = ITABL(JIPJ1,I,JIPJRN) - 1
        DO 101 J=1,NPOI
          IIPAR = ITABL(JFPOI,IABS(ITABL(JIRJ1,IPOI+J,JIRJGN)),1)
          DO 102 K=1,NPOT
            IF (IPART(K).EQ.IIPAR) THEN
              IHIT(K) = IHIT(K) + ITABL(JIRJ1,IPOI+J,JIRJSH)
              GOTO 101
            ENDIF
  102     CONTINUE
          NIOF = NIOF + 1
          IHIT(NPOT+NIOF) = ITABL(JIRJ1,IPOI+J,JIRJSH)
          IPART(NPOT+NIOF) = IIPAR
  101   CONTINUE
C
C Fill in QVEC
C
        IW(LQVEC+JQVENM) = NPOT+NIOF
        IW(LQVEC+JQVEML) = KNQMTX
C
C Check length of QMTX
C
        IF (KNQMTX + NPOT + NIOF .GE. IW(KOQMTL))
     &     CALL QSBANK ('QMTX', KNQMTX+200)
C
C Now fill in QMTX and QMTL
C
        DO 200 J=1,NPOT+NIOF
          KNQMTX = KNQMTX + 1
          IKIN = IPART(J) + KFMCT - 1
          IW(INM(IKIN)) = IW(INM(IKIN)) + 1
          IW(KOQMTL+KNQMTX) = IKIN
          IW(KOQMTS+KNQMTX) = IHIT(J)
  200   CONTINUE
C
  300 LQVEC = LQVEC + KCQVEC
C
C Now tracks that are only in the ITC
C
  301 JIPJ2=NLINK('IPJT',2)
      JITJ2=NLINK('IRJT',2)
      IF (NITRKS.LE.0) GOTO 499
C
      DO 400 I = 1,NITRKS
        NPOI = ITABL(JIPJ2,I,JIPJNS)
C
        IF (NPOI.EQ.0) GO TO 400
C
C Here if too many MC tracks associated with a charged track print
C an error message and get out.
C
        IF (NPOI.GT.MXMC) THEN
          IERR = IERR + 1
          IF (IERR .LE. 20)  CALL QWMESE
     &        ('_QFMTCH_ Unable to fill matching information')
          GO TO 400
        ENDIF
        IW(LQVEC+JQVENM) = NPOI
        IW(LQVEC+JQVEML) = KNQMTX
C
        IPOI = ITABL(JIPJ2,I,JIPJRN) - 1
        DO 401 J=1,NPOI
          IHIT(J) = ITABL(JITJ2,IPOI+J,JIRJSH)
  401   CONTINUE
        IF (KNQMTX + NPOI .GE. IW(KOQMTL))
     &       CALL QSBANK ('QMTX', KNQMTX+200)
        DO 402 J=1,NPOI
          KNQMTX = KNQMTX + 1
          IKIN=ITABL(JFPOI,IABS(ITABL(JITJ2,IPOI+J,JIRJGN)),1)+KFMCT-1
          IW(INM(IKIN)) = IW(INM(IKIN)) + 1
          IW(KOQMTL+KNQMTX) = IKIN
          IW(KOQMTS+KNQMTX) = IHIT(J)
  402   CONTINUE
C
  400 LQVEC = LQVEC + KCQVEC
      GOTO 499
C
C Here if using ITMA and IASL.  Much simpler than above.
C
  410 IF (NTRKS.LE.0) GOTO 999
C
      DO 450 I = 1,NTRKS
        NPOI = ITABL(JITMA,I,JITMNA)
C
        IF (NPOI.EQ.0) GO TO 450
C
C Here if too many MC tracks associated with a charged track print
C an error message and get out.
C
        IF (NPOI.GT.MXMC) THEN
          IERR = IERR + 1
          IF (IERR .LE. 20)  CALL QWMESE
     &        ('_QFMTCH_ Unable to fill matching information')
          GO TO 450
        ENDIF
        IW(LQVEC+JQVENM) = NPOI
        IW(LQVEC+JQVEML) = KNQMTX
C
        IPOI = ITABL(JITMA,I,JITMOF)
        DO 451 J=1,NPOI
          IHIT(J) = ITABL(JIASL,IPOI+J,JIASNH)
  451   CONTINUE
        IF (KNQMTX + NPOI .GE. IW(KOQMTL))
     &       CALL QSBANK ('QMTX', KNQMTX+200)
        DO 452 J=1,NPOI
          KNQMTX = KNQMTX + 1
          IKIN=ITABL(JIASL,IPOI+J,JIASFK)+KFMCT-1
          IW(INM(IKIN)) = IW(INM(IKIN)) + 1
          IW(KOQMTL+KNQMTX) = IKIN
          IW(KOQMTS+KNQMTX) = IHIT(J)
  452   CONTINUE
C
  450 LQVEC = LQVEC + KCQVEC
C
C Now fill the inverse relation
C
  499 LQVEC = KOQVEC + KFMCT * KCQVEC
      DO 500 I=KFMCT,KLMCT
        IW(LQVEC+JQVEML) = KNQMTX - IW(LQVEC+JQVENM)
        KNQMTX = KNQMTX + IW(LQVEC+JQVENM)
  500 LQVEC = LQVEC + KCQVEC
C
      IF (KNQMTX .GE. IW(KOQMTL))  CALL QSBANK ('QMTX',KNQMTX+200)
C
      LQVEC = KOQVEC + KFCHT * KCQVEC
      DO 600 I=KFCHT,KLCHT
        DO 510 J=1,IW(LQVEC+JQVENM)
          IMC = IW(KOQMTL+IW(LQVEC+JQVEML)+J)
          IW(IML(IMC)) = IW(IML(IMC)) + 1
          IW(KOQMTL+IW(IML(IMC))+IW(INM(IMC))) = I
          IW(KOQMTS+IW(IML(IMC))+IW(INM(IMC))) =
     &      IW(KOQMTS+IW(LQVEC+JQVEML)+J)
  510   CONTINUE
  600 LQVEC = LQVEC + KCQVEC
C
  999 CONTINUE
      END
#endif
