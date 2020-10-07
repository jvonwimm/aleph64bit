      SUBROUTINE FAWIAN
C------------------------------------------------------------------------
C! dE/dx analysis of FASGAL output
C D.Cinabro - 881215
C
C Inputs: TFXS,TGMA,TMTL
C Output: TEXS
C
C The program considers all TGFT (TGMA is parallel) tracks, gets which
C Monte Carlo tracks they are related to in TGMA and TMTL.  Monte Carlo
C tracks related to only one track will be considered further.  Then loop
C on TFXS and find if the Monte Carlo tracks relate to an entry in
C TFXS.  If so choose the track with the most number of shared hits.
C Make an entry in TEXS if there are enough wire samples on the track.
C Note that the sector number and drift distance in TEXS will be meaningless
C with the sector always being one and drift distance always being 0.
C?
C!======================================================================
#ifndef DOC
#include "tgmajj.h"
#include "tmtljj.h"
#include "texsjj.h"
#include "twirct.h"
#include "bcs.h"
C
      LOGICAL FIRST,SECOND
      DIMENSION NHIT(50,2)
      DATA FIRST,SECOND/.TRUE.,.TRUE./
C
#include "bmacro.h"
C
C Are the TGMA, TMTL, and TFXS banks there?
C
      IF (FIRST) THEN
         FIRST = .FALSE.
         NTGMA = NAMIND('TGMA')
         NTMTL = NAMIND('TMTL')
         NTFXS = NAMIND('TFXS')
      ENDIF
      KTMTL = IW(NTMTL)
      IF (KTMTL.LE.0) GOTO 999
      KTGMA = IW(NTGMA)
      IF (KTGMA.LE.0) GOTO 999
      KTFXS = IW(NTFXS)
      IF (KTFXS.LE.0) GOTO 999
C
C If so book TEXS
C
      LEN = LTEXSA*LROWS(KTFXS)+LMHLEN
      CALL ALBOS('TEXS',0,LEN,KTEXS,IGARB)
      IW(KTEXS+LMHCOL) = LTEXSA
      IW(KTEXS+LMHROW) = 0
      NROW = 0
C
C Loop on TGMA
C
      KTGMA=IW(NTGMA)
      KTMTL=IW(NTMTL)
      KTFXS=IW(NTFXS)
      DO 10 I = 1,LROWS(KTGMA)
C
C How many MC tracks is it related to.
C
         NUMT=ITABL(KTGMA,I,JTGMNM)
         IF (NUMT.LE.0) GOTO 10
C
C Which ones
C
         IPOI = ITABL(KTGMA,I,JTGMOM)
         DO 3 J = 1,NUMT
            NHIT(J,1) = 0
            NHIT(J,2) = 0
            IMCE = ITABL(KTMTL,IPOI+J,JTMTMT)
C
C This MC track must not relate to any other TPC track
C
            DO 4 K = 1,LROWS(KTMTL)
               IF (K.EQ.(IPOI+J)) GOTO 4
               IMC = ITABL(KTMTL,K,JTMTMT)
               IF (IMC.EQ.IMCE) GOTO 3
    4       CONTINUE
C
C Loop on TFXS
C
            DO 5 K = 1,LROWS(KTFXS)
               IMCF = ITABL(KTFXS,K,JTEXTN)
               IF (IMCF.NE.IMCE) GOTO 5
C
C How many hits are related?
C
               NHIT(J,1) = ITABL(KTMTL,IPOI+J,JTMTNH)
               NHIT(J,2) = K
   5        CONTINUE
C
   3     CONTINUE
C
C Choose the one with the most hits. If they are all zeros forget it.
C If two have the same number of hits we are choosing the first.
C
         NMAX = 0
         IMAX = 0
         DO 6 J = 1,NUMT
            IF (NHIT(J,1).GT.NMAX) THEN
               NMAX = NHIT(J,1)
               IMAX = NHIT(J,2)
            ENDIF
    6    CONTINUE
         IF (NMAX.NE.0) THEN
C
C Are there enough hits on the track to make it worthwhile
C
            IROW = IMAX
            ISAM = INT(FLOAT(ITABL(KTFXS,IROW,JTEXNS))*.6)
            IF (ISAM.LT.MNSPTW) GOTO 10
C
C Fill in TEXS if number of wire
C
            IW(KTEXS+LMHLEN+NROW*LTEXSA+JTEXSI) =
     &                 ITABL(KTFXS,IROW,JTEXSI)
            RW(KTEXS+LMHLEN+NROW*LTEXSA+JTEXTM) =
     &                 RTABL(KTFXS,IROW,JTEXTM)
            RW(KTEXS+LMHLEN+NROW*LTEXSA+JTEXTL) =
     &                 RTABL(KTFXS,IROW,JTEXTL)
            IW(KTEXS+LMHLEN+NROW*LTEXSA+JTEXNS) = ISAM
            RW(KTEXS+LMHLEN+NROW*LTEXSA+JTEXAD) =
     &                 RTABL(KTFXS,IROW,JTEXAD)
            IW(KTEXS+LMHLEN+NROW*LTEXSA+JTEXTN) = I
            NROW = NROW + 1
         ENDIF
C
  10  CONTINUE
C
C Compress TEXS
C
      IW(KTEXS+LMHROW) = NROW
      CALL AUBPRS('TEXS')
C
  999 RETURN
      END
#endif
