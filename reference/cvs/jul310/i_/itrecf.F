      SUBROUTINE ITRECF
C-----------------------------------------------------------------------
C! Finalise ITC-TPC tracking
C!
C!    Author      :- J. Sedgbeer 87/02/09
C!    Modified    :- J. Sedgbeer 88/06/22
C!    Modified    :- I. Tomalin  89/09/01
C!
C!   Input:
C!     need common:/BCS/
C!                 /ITNAMC/
C!                 /TPNAMC/
C!                 /IWBNKT/
C!                 /ITRKCC/
C!          params: ITCOJJ,IGTLJJ,TGTLJJ,ITFTJJ,TGFTJJ
C!
C!   Output:
C!      finalised banks IGCL#1, IGTL#1, ITFT#1
C!
C!   called by : ITCREC
C!   calls     : UFITMS
C!
C!   Libraries required: BOS,CERNLIB
C!======================================================================
#ifndef DOC
#include "rparac.h"
#include "rflags.h"
#include "rlunit.h"
C-----------------------------------------------------------------------
C I/O commons
#include "bcs.h"
#include "rcurnt.h"
#include "bosext.h"
#include "itnamc.h"
#include "tpnamc.h"
#include "iwbnkt.h"
#include "itrkcc.h"
C-----------------------------------------------------------------------
#include "itcojj.h"
#include "igtljj.h"
#include "tgtljj.h"
#include "itftjj.h"
#include "tgftjj.h"
C-----------------------------------------------------------------------
C Dummy common block to save work bank indices.
      COMMON/IDUMYE/IGTLIW,IGCLIW
      LOGICAL SETZ
      REAL VV0(6),EE0(21),COV(21)
      DATA SETZ/.TRUE./
C-----------------------------------------------------------------------
#include "bmacro.h"
C-----------------------------------------------------------------------
      IF (SETZ) THEN
        IGTLIW = 0
        IGCLIW = 0
        SETZ = .FALSE.
      END IF
C
C Compress IGCL bank
C
      KIGCL = NLINK('IGCL',1)
      NWGC  = LCOLS(KIGCL)
      NGC   = LROWS(KIGCL)
      LIGCL = LMHLEN+LROWS(KIGCL)*LCOLS(KIGCL)
C
      CALL AUBOS('IGCL',1,LIGCL,KIGCL,IGARB)
      IF (IGARB.EQ.2) KIGCL = NLINK('IGCL',1)
C
      KIGTL = NLINK('IGTL',1)
C
C Reorder the IGCL bank so that the entries in it are ordered according
C to increasing track number.
      IF (IGTLIW.GT.0) THEN
        CALL WDROP(IW,IGTLIW)
        CALL RERROR('ITRECF',1,'IGTLIW work bank still exists '//
     &    ' from earlier. It should have been dropped.')
      END IF
      CALL BKTOW(IW,'IGTL',1,IW,IGTLIW,*5)
      IF (IGCLIW.GT.0) THEN
        CALL WDROP(IW,IGCLIW)
        CALL RERROR('ITRECF',2,'IGCLIW work bank still exists'//
     &   ' from  earlier. It should have been dropped.')
      END IF
      CALL BKTOW(IW,'IGCL',1,IW,IGCLIW,*5)
      GOTO 10
C
    5 CONTINUE
C Insufficient BOS space. Scrap ITC-TPC tracking and give error message.
      CALL VZERO(IW(KIGTL+LMHLEN+1),LROWS(KIGTL)*LCOLS(KIGTL))
      IW(KIGCL+LMHROW) = 0
      CALL AUBOS('IGCL',1,LMHLEN,KIGCL,IGARB)
      IF (IGARB.GE.1) KIGTL = NLINK('IGTL',1)
      IF (IGARB.EQ.2) KIGCL = NLINK('IGCL',1)
      CALL RERROR('ITRECF',3,'Not enough space for IGCLIW and'//
     +' IGTLIW work banks.')
      GOTO 25
C
   10 CONTINUE
      NOFF = 0
      DO 20 I = 1,LROWS(IGTLIW)
        IIGTL = KROW(IGTLIW,I)
        MCO = IW(IIGTL+JIGTN1) + IW(IIGTL+JIGTNR)
C Fill the reordered IGTL bank.
        IW(KROW(KIGTL,I)+JIGTIO) = NOFF
        IOFF = IW(IIGTL+JIGTIO)
        DO 15 J = 1,MCO
          NOFF = NOFF + 1
C Fill the reordered IGCL bank.
          IW(KIGCL+LMHLEN+NOFF) = IW(IGCLIW+LMHLEN+IOFF+J)
   15   CONTINUE
   20 CONTINUE
C
      IF(FDEBRF.AND.JDBDRF(JULIT).GE.4) THEN
        LL = IW(KIGTL)
        IF (LL.GT.LMHLEN) WRITE(LDEBRL,1006) (IW(KIGTL+K),K=1,LL)
 1006   FORMAT('0ITRECF:  IGTL#1:',2I5,/(10(2X,2I4)))
        LL = IW(KIGCL)
        IF (LL.GT.LMHLEN) WRITE(LDEBRL,1007) (IW(KIGCL+K),K=1,LL)
 1007   FORMAT('          IGCL#1:',2I5,/(20I5))
      ENDIF
C
   25 CONTINUE
C Ensure that unused coords. are at their original values.
C (Some were changed during ITCTPC, even though they were not used).
      KITC0 = NLINK('ITCO',0)
      KITC1 = NLINK('ITCO',1)
      IF (KITC0.GT.0.AND.KITC1.GT.0.AND.IUCOIW.GT.0) THEN
        NCO = LROWS(KITC1)
        DO 30 I = 1,NCO
          MUSE = IW(IUCOIW+LMHLEN+I)
          IF (MUSE.EQ.1) GOTO 30
C This coordinate was not used.
          LITC0 = KROW(KITC0,I)
          LITC1 = KROW(KITC1,I)
          RW(LITC0+JITCP1) = RW(LITC1+JITCP1)
          RW(LITC0+JITCP2) = RW(LITC1+JITCP2)
          RW(LITC0+JITCSR) = RW(LITC1+JITCSR)
   30   CONTINUE
      END IF
C
      KITFT = IW(NAITFT)
      KTGFT = IW(NATGFT)
      KTGTL = IW(NATGTL)
      KTGCL = IW(NATGCL)
C
      JER = 0
      IF(KTGTL.LE.0.OR.KTGCL.LE.0) THEN
        CALL RERROR('ITRECF',4,' TGTL or TGCL banks not found.')
C Note this problem by setting JER = 1.
        JER=1
      ENDIF
C
C Do combined fit of TPC and ITC hits and fill ITFT.
C
      NTGF  = LROWS(KTGFT)
      NMULT = 1
      IOPT  = 4
      DO 100 I=1,NTGF
        IIGTL = KROW(KIGTL,I)
        ITGFT = KROW(KTGFT,I)
        IITFT = KROW(KITFT,I)
   35   CONTINUE
        NHI = IW(IIGTL+JIGTN1)
        IF (JER.EQ.0.AND.NHI.GT.0) THEN
C If there were ITC hits on this track, then refit it.
          IOI = IW(IIGTL+JIGTIO)
          ILI = KIGCL + LMHLEN + IOI + 1
C
          ITGTL = KROW(KTGTL,I)
          NHT = IW(ITGTL+JTGTN1)
          NHTR = IW(ITGTL+JTGTNR)
          IOT = IW(ITGTL+JTGTIO)
          ILT = KTGCL+LMHLEN+IOT+1
C
          CALL UFITMS(NHT,NHI,0,IW(ILT),IW(ILI),IDUM,IOPT,NMULT,
     +              RSCAIT,0.027,VV0,EE0,COV,CHI2,IERR)
C
C Note the number of ITC coords with z-info.
          NHIZ = 0
          DO 37 J = 1,NHI
            ICO = ABS(IW(ILI-1+J))
            IF (RTABL(KITC0,ICO,JITCSZ).LT.10000.) NHIZ = NHIZ + 1
   37     CONTINUE
C Check the increase in CHI**2 on adding the ITC coordinates.
          DELCH = CHI2 - RW(ITGFT+JTGFCD)
          IF (DELCH.LT.0.0) DELCH = 0.0
          IF (PROB(DELCH,NHI+NHIZ).LT.1.0E-4) THEN
C Don't extend this TPC track after all.
C Update used coordinate list
            DO 40 JJ = 1,NHI
              ICO = ABS(IW(ILI-1+JJ))
              IW(IUCOIW+LMHLEN+ICO) = 0
   40       CONTINUE
C Update IGTL bank.
            IW(IIGTL+JIGTN1) = 0
            DO 45 JJ = I+1,NTGF
              IIGTP = KROW(KIGTL,JJ)
              IW(IIGTP+JIGTIO) = IW(IIGTP+JIGTIO) - NHI
   45       CONTINUE
C Update IGCL bank
            DO 50 JJ = IOI+NHI+1,LROWS(KIGCL)
              IW(KIGCL-NHI+LMHLEN+JJ) = IW(KIGCL+LMHLEN+JJ)
   50       CONTINUE
            IW(KIGCL+LMHROW) = IW(KIGCL+LMHROW) - NHI
            GOTO 35
          END IF
C Fill the ITFT bank.
          DO 55 J=1,6
            RW(IITFT+J) = VV0(J)
   55     CONTINUE
          DO 60 J=1,21
            RW(IITFT+JITFCM-1+J) = COV(J)
   60     CONTINUE
          RW(IITFT+JITFCD) = CHI2
          IW(IITFT+JITFND) = 2*NHT + (NHI + NHIZ) - 5
          IW(IITFT+JITFFT) = NMULT*100 + IOPT*10 + IERR
        ELSE
C If there were no ITC hits on the track, fill ITFT bank from TGFT one.
          DO 65 J=1,5
            RW(IITFT+J) = RW(ITGFT+J)
   65     CONTINUE
          RW(IITFT+JITFSA) = 0.0
          DO 70 J=1,15
            RW(IITFT+JITFCM-1+J) = RW(ITGFT+JTGFEM-1+J)
   70     CONTINUE
          DO 80 J=16,21
            RW(IITFT+JITFCM-1+J) = 0.0
   80     CONTINUE
          RW(IITFT+JITFCD) = RW(ITGFT+JTGFCD)
          IW(IITFT+JITFND) = IW(ITGFT+JTGFDF)
          IW(IITFT+JITFFT) = 1*100 + 2*10
C*********************************************************************
C ***** The following transformation has been commented out, as *****
C ***** it was decided to leave the error matrix uncorrected.   *****
C Now, transform the covariance matrix so as to include the errors due
C to scattering between the ITC and the TPC.
C (Extension of ALEPH Note 88-6 Eqn's 6.4 and 6.5. Also see UFITMS).
C          R0   = 1.0/RW(ITGFT+JTGFIR)
C          TLAM = RW(ITGFT+JTGFTL)
C          D0   = RW(ITGFT+JTGFD0)
C          IF (ABS(D0).GT.RSCAIT) GOTO 90
C First calculate the angle of the track at the scattering radius.
C (Compare with subroutine INSECT).
C          SINB = (0.5*(RSCAIT**2-D0**2)+R0*D0)/(RSCAIT*R0)
C          IF(ABS(SINB).GT.1.0) GOTO 90
C          TPENA = ASIN(SINB)
C Calculate the r.m.s. scattering angle of the track.
C          PT = FRHOIT*ABS(R0)
C          THSCT = FSCAIT/(PT*SQRT(COS(TPENA)*SQRT(1.0+TLAM**2)))
C Project it into the r-phi plane.
C          THSCT = THSCT*SQRT(1.0+TLAM**2)
C Calculate the angle between the Origin and the intersection of the
C track with the scattering radius, as seen from the centre of the
C circular flight path.
C          CPSI = 1.0 - (RSCAIT**2-D0**2)/(2.0*R0*(R0-D0))
C          IF(ABS(CPSI).GT.1.0) GOTO 90
C          PSI = ACOS(CPSI)
C Get the derivatives of the new D0 and PHI0 w.r.t. the scattering angle
C          DD0DS = -ABS(R0*SIN(PSI))
C          DP0DS = -R0*CPSI/(R0-D0)
C Transform the error matrix.
C          RW(IITFT+(JITFCM-1)+6) = RW(IITFT+(JITFCM-1)+6) +
C     +                                 (DP0DS*THSCT)**2
C          RW(IITFT+(JITFCM-1)+9) = RW(IITFT+(JITFCM-1)+9) +
C     +                              DP0DS*DD0DS*THSCT**2
C          RW(IITFT+(JITFCM-1)+10) = RW(IITFT+(JITFCM-1)+10) +
C     +                                 (DD0DS*THSCT)**2
C          RW(IITFT+(JITFCM-1)+18) = DP0DS*THSCT**2
C          RW(IITFT+(JITFCM-1)+19) = DD0DS*THSCT**2
C          RW(IITFT+(JITFCM-1)+21) = THSCT**2
C   90     CONTINUE
C************************************************************************
        ENDIF
C
        IF(FDEBRF.AND.JDBDRF(JULIT).GE.4) THEN
          IF(NHI.GT.0) THEN
            WRITE(LDEBRL,1008) I,NHI,IOI,(IW(ILI+J),J=0,NHI-1)
 1008       FORMAT('0ITRECF: Track',I4,' #ITC hits',I4,' Offset',I4,
     +             /5X,'ITC coords',8I5)
            WRITE(LDEBRL,1009) NHT,NHTR,IOT,(IW(ILT+J),J=0,NHT-1)
 1009       FORMAT(5X,'#TPC hits(total)',I3,'(',I3,')',' Offset',I4,
     +            /5X,'TPC coords',10I4,/(15X,10I4))
            WRITE(LDEBRL,1010) (VV0(J),J=1,6),CHI2
 1010       FORMAT(3X,'Fit params:',F7.4,F7.3,F7.4,F7.3,F7.1,F8.4,
     +                ' CHISQ',F7.1)
          ENDIF
          WRITE(LDEBRL,1011) (RW(ITGFT+J),J=1,5),RW(ITGFT+21),
     +    IW(ITGFT+22)
 1011     FORMAT(3X,'TGFT      :',F7.4,F7.3,F7.4,F7.3,F7.1,9X,
     +              'CHISQ',F7.1,' NDF',I4)
        ENDIF
C
  100 CONTINUE
C Compress IGCL bank (in case any extensions were rejected by CHI**2).
C
      KIGCL = NLINK('IGCL',1)
      NWGC  = LCOLS(KIGCL)
      NGC   = LROWS(KIGCL)
      LIGCL = LMHLEN+LROWS(KIGCL)*LCOLS(KIGCL)
C
      CALL AUBOS('IGCL',1,LIGCL,KIGCL,IGARB)
C Drop work banks
      IF (IGCLIW.NE.0) CALL WDROP(IW,IGCLIW)
      IF (IGTLIW.NE.0) CALL WDROP(IW,IGTLIW)
      IF (IWTPCP.GT.0) CALL WDROP(IW,IWTPCP)
      IF (ILNKIW.NE.0) CALL WDROP(IW,ILNKIW)
      IF (INNXIW.NE.0) CALL WDROP(IW,INNXIW)
      IF (IOFNIW.NE.0) CALL WDROP(IW,IOFNIW)
      IF (INXLIW.NE.0) CALL WDROP(IW,INXLIW)
      END
#endif
