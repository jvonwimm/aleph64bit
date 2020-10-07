      SUBROUTINE IATTMA(IERR)
C-----------------------------------------------------------------------
C! This program relates JULIA to true MC tracks in the ITC and TPC.
C!
C!    Author     :- I. Tomalin    89/03/14
C!
C!    Input:
C!      commons:       /BCS/
C!      parameters:    IPJTJJ,IRJTJJ,TGMAJJ,TMTLJJ,FPOIJJ
C!                     PITMJJ,PASLJJ
C!
C!    Output:
C!         I/   IERR = 0 if new banks produced.
C!                   = 1 if the input banks did not make sense.
C!                   = 2 if there was insufficient BOS space.
C!
C!    called by : ITCREC
C!    calls     : none
C!
C!    Libraries required: BOS
C!
C? Link to all the required banks.
C? Create the PITM and PASL banks.
C? Loop over the ITC-TPC tracks.
C?   First find those FKIN tracks associated in the TPC and start filling
C?   the PITM and PASL banks.
C?   Then find those associated in the ITC and continue filling the new
C?   banks. (Exactly how depending on whether or not the FKIN tracks was
C?   also found in the TPC).
C? End Loop
C? Loop over the ITC stand-alone tracks.
C?   Finish filling the PITM and PASL banks.
C? End Loop
C? Compress the PASL bank.
C?
C!======================================================================
#ifndef DOC
C I/O commons and parameters
#include "bcs.h"
#include "bosext.h"
#include "ipjtjj.h"
#include "irjtjj.h"
#include "tmtljj.h"
#include "tgmajj.h"
#include "fpoijj.h"
#include "pitmjj.h"
#include "pasljj.h"
C-----------------------------------------------------------------------
      LOGICAL FIRST,INTPC
      DATA FIRST/.TRUE./
C-----------------------------------------------------------------------
#include "bmacro.h"
C-----------------------------------------------------------------------
C
      IERR = 0
C
      KTGMA = NLINK('TGMA',0)
      KTMTL = NLINK('TMTL',0)
      KIPJT1 = NLINK('IPJT',1)
      KIRJT1 = NLINK('IRJT',1)
      KIPJT2 = NLINK('IPJT',2)
      KIRJT2 = NLINK('IRJT',2)
      KFPOI = NLINK('FPOI',0)
C
      NTPC = 0
      MAXLT = 0
      IF (KTGMA.GT.0.AND.KTMTL.GT.0) THEN
        NTPC = LROWS(KTGMA)
        MAXLT = LROWS(KTMTL)
      END IF
C
      NITC1 = 0
      MAXLI1 = 0
      IF (KFPOI.GT.0.AND.KIPJT1.GT.0.AND.KIRJT1.GT.0) THEN
        NITC1 = LROWS(KIPJT1)
        MAXLI1 = LROWS(KIRJT1)
      END IF
C
      NITC2 = 0
      MAXLI2 = 0
      IF (KFPOI.GT.0.AND.KIPJT2.GT.0.AND.KIRJT2.GT.0) THEN
        NITC2 = LROWS(KIPJT2)
        MAXLI2 = LROWS(KIRJT2)
      END IF
      IF (NITC1.GT.0.AND.NITC1.NE.NTPC) THEN
        IERR = 1
        GOTO 999
      END IF
C Create banks relating FRFT to FKIN tracks.
C (The PASL bank will probably need compressing later).
      CALL AUBOS('PITM',0,LMHLEN+NTPC+NITC2,KPITM,IGAR1)
      IF (IGAR1.EQ.2) THEN
        IERR = 2
        GOTO 999
      END IF
      CALL AUBOS('PASL',0,LMHLEN+LPASLA*(MAXLT+MAXLI1+MAXLI2),KPASL,
     +IGAR2)
      IF (IGAR2.EQ.2) THEN
        IERR = 2
        CALL BDROP(IW,'PITM')
        GOTO 999
      END IF
C
      IF (IGAR1.EQ.1.OR.IGAR2.EQ.1) THEN
        KTGMA = NLINK('TGMA',1)
        KTMTL = NLINK('TMTL',1)
        KIPJT1 = NLINK('IPJT',1)
        KIRJT1 = NLINK('IRJT',1)
        KIPJT2 = NLINK('IPJT',2)
        KIRJT2 = NLINK('IRJT',2)
        KFPOI = NLINK('FPOI',0)
      END IF
      IF (IGAR2.EQ.1) KPITM = NLINK('PITM',0)
C
      IW(KPITM+LMHCOL) = LPITMA
      IW(KPITM+LMHROW) = NTPC + NITC2
      IW(KPASL+LMHCOL) = LPASLA
      IW(KPASL+LMHROW) = 0
C
C Loop over the ITC-TPC tracks.
      NOFF = 0
      DO 50 I = 1,NTPC
C First look at the TPC part of the tracks.
        ITGMA = KROW(KTGMA,I)
        IPITM = KROW(KPITM,I)
        IOFF = IW(ITGMA+JTGMOM)
C Fill the PITM bank.
        IW(IPITM+JPITNA) = IW(ITGMA+JTGMNM)
C Note the row number in the PASL bank of the first FKIN track associated
C with this TPC track.
        NFIRS = NOFF + 1
C Loop over the associated true Monte Carlo tracks.
        DO 20 NA = 1,IW(ITGMA+JTGMNM)
          IFKN = ITABL(KTMTL,IOFF+NA,JTMTMT)
          NSH = ITABL(KTMTL,IOFF+NA,JTMTNH)
          NOFF = NOFF + 1
C Fill the PASL bank.
          IW(KROW(KPASL,NOFF)+JPASFK) = IFKN
          IW(KROW(KPASL,NOFF)+JPASNH) = NSH
  20    CONTINUE
C Note the row number of the last FKIN track for this TPC track.
        NLAST = NOFF
C
C Then look at the ITC part of the tracks.
        IF (NITC1.EQ.0) GOTO 50
        IIPJT1 = KROW(KIPJT1,I)
        IOFF = IW(IIPJT1+JIPJRN) - 1
C Loop over the associated true Monte Carlo tracks.
        DO 45 NA = 1,IW(IIPJT1+JIPJNS)
          IKIN = ITABL(KIRJT1,IOFF+NA,JIRJGN)
C Get the FKIN track number corresponding to KINE track number "IKIN".
          IF (IKIN.GT.LROWS(KFPOI)) GOTO 45
          ISAVE = ITABL(KFPOI,IKIN,JFPOIS)
          IF (ISAVE.NE.0) GOTO 45
          IFKN = ITABL(KFPOI,IKIN,JFPOIP)
          NSH = ITABL(KIRJT1,IOFF+NA,JIRJSH)
C See if this FKIN track was associated with the TPC part of the track.
          INTPC = .FALSE.
          DO 30 K = NFIRS,NLAST
            IF (IFKN.EQ.ITABL(KPASL,K,JPASFK)) THEN
              INTPC = .TRUE.
              IW(KROW(KPASL,K)+JPASNH) = ITABL(KPASL,K,JPASNH) + NSH
            END IF
  30      CONTINUE
          IF (.NOT.INTPC) THEN
            IW(IPITM+JPITNA) = IW(IPITM+JPITNA) + 1
            NOFF = NOFF + 1
            IW(KROW(KPASL,NOFF)+JPASFK) = IFKN
            IW(KROW(KPASL,NOFF)+JPASNH) = NSH
          END IF
  45    CONTINUE
  50  CONTINUE
C
C Loop over the ITC stand alone tracks.
      DO 150 I = 1,NITC2
        IIPJT2 = KROW(KIPJT2,I)
        IPITM = KROW(KPITM,NTPC+I)
        IOFF = IW(IIPJT2+JIPJRN) - 1
C Initialize variable for storing number of associated FKIN tracks.
C (Can't be taken directly from IPJT bank because some KINE tracks
C do not correspond to FKIN tracks).
        NATR = 0
C Loop over the associated true Monte Carlo tracks.
        DO 145 NA = 1,IW(IIPJT2+JIPJNS)
          IKIN = ITABL(KIRJT2,IOFF+NA,JIRJGN)
C Get the FKIN track number corresponding to KINE track number "IKIN".
          IF (IKIN.GT.LROWS(KFPOI)) GOTO 145
          ISAVE = ITABL(KFPOI,IKIN,JFPOIS)
          IF (ISAVE.NE.0) GOTO 145
          IFKN = ITABL(KFPOI,IKIN,JFPOIP)
          NSH = ITABL(KIRJT2,IOFF+NA,JIRJSH)
          NATR = NATR + 1
          NOFF = NOFF + 1
          IW(KROW(KPASL,NOFF)+JPASFK) = IFKN
          IW(KROW(KPASL,NOFF)+JPASNH) = NSH
 145    CONTINUE
C Fill the PITM bank.
        IW(IPITM+JPITNA) = NATR
 150  CONTINUE
C Finalise and compress PASL bank.
      IW(KPASL+LMHROW) = NOFF
      CALL AUBOS('PASL',0,LMHLEN+LPASLA*NOFF,KPASL,IGARB)
 999  CONTINUE
      IF (IERR.EQ.2) CALL RERROR('IATTMA',1,'Insufficient BOS space.')
      CALL BLIST(IW,'E+','PITMPASL')
      END
#endif
