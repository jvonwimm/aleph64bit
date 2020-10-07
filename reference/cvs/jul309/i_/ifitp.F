      SUBROUTINE IFITP(ITRY,MODE,SPIRAL,IDTPC,IER)
C-----------------------------------------------------------------------
C! Fit found paths.
C!
C!    Author      :- J. Sedgbeer 88/04/25
C!    Modified    :- J. Sedgbeer 88/05/09
C!    Modified    :- I. Tomalin  89/09/01
C!    Modified    :- J.Sedgbeer  91/09/10 If MODE=1 and 1st arc then fit
C!                               with TPC points.
C!
C!   Input:
C!      ITRY   /I : Type of search flag
C!      MODE   /I : = 1 if in TPC track extension phase.
C!                  = 2 if in ITC stand alone phase.
C!      SPIRAL /L : =FALSE if one is studying the 1st arc of a track.
C!                  =TRUE otherwise (N.B. spiralling tracks can have
C!                     many arcs/orbits).
C!      IDTPC  /I : If MODE=1, then IDTPC is the no. in the TGFT bank
C!                   of the TPC track being extended.
C!                  If MODE=2, then it is a dummy variable.
C!      commons:     /BCS/
C!                   /IPATHC/
C!                   /ITRKPA/
C!                   /IWBNKT/
C!                   /ITRKCC/
C!                   /UFITCB/
C!      params.:     ITFTJJ,IGTLJJ,ITCOJJ
C!
C!   Output:
C!      IER    /I : Error flag   = 0 if one or more tracks were found
C!                               = 1 if no tracks were found
C!                               = 2 if there were problems with banks
C!
C!   called by : IGICHA and IFIND2
C!   calls     : IENANG, IPHCOR, UFITMS
C!
C!   Libraries required: BOS
C!======================================================================
#ifndef DOC
#include "rparac.h"
#include "rflags.h"
#include "rlunit.h"
C-----------------------------------------------------------------------
C I/O Commons etc.
#include "bcs.h"
#include "bosext.h"
#include "itrkpa.h"
#include "ipathc.h"
#include "iwbnkt.h"
#include "itrkcc.h"
#include "ufitcb.h"
C
#include "itftjj.h"
#include "igtljj.h"
#include "itcojj.h"
#include "tgtljj.h"
C-----------------------------------------------------------------------
      REAL IPHCOR
      EXTERNAL IPHCOR
C
      LOGICAL SPIRAL
      LOGICAL FIRST
      INTEGER ILIST(8)
      REAL VV0(6),EE0(21),COV(21)
      REAL PHCOR(8),SRCOR(8)
      DATA FIRST/.TRUE./
C-----------------------------------------------------------------------
#include "bmacro.h"
C-----------------------------------------------------------------------
      IER   = 1
      IF(NPTHIP.LE.0) GOTO 999
C
      CLMN = 0.0
      IF(FIRST) THEN
        FIRST = .FALSE.
        NTGTL = NAMIND('TGTL')
        NTGCL = NAMIND('TGCL')
      ENDIF
      KIGTL = NLINK('IGTL',MODE)
      KIGCL = NLINK('IGCL',MODE)
      KITFT = NLINK('ITFT',MODE)
      IF(KIGTL.LE.0) THEN
        IER = 2
        CALL RERROR('IFITP',1,'IGTL bank missing')
        GOTO 999
      ENDIF
      IF(KIGCL.LE.0) THEN
        IER = 2
        CALL RERROR('IFITP',2,'IGCL bank missing')
        GOTO 999
      ENDIF
      IF(KITFT.LE.0) THEN
        IER = 2
        CALL RERROR('IFITP',3,'ITFT bank missing')
        GOTO 999
      ENDIF
      IF (MODE.EQ.2) THEN
C Check enough room in banks for new track - if not then extend banks
        LFROW = LFRROW(KIGTL)
        IF(LFROW.LE.0) THEN
          LIGTL = IW(KIGTL) + LIGTLA*5
          LITFT = IW(KITFT) + LITFTA*5
          CALL AUBOS('IGTL',MODE,LIGTL,KIGTL,IGARB)
          IF(IGARB.EQ.2) THEN
            IER = 2
            CALL RERROR('IFITP',4,'Insufficient BOS space')
            GOTO 999
          ENDIF
          CALL AUBOS('ITFT',MODE,LITFT,KITFT,IGARB)
          IF(IGARB.EQ.2) THEN
            IER = 2
            CALL RERROR('IFITP',4,'Insufficient BOS space')
            GOTO 999
          ENDIF
          KIGTL = NLINK('IGTL',MODE)
          KIGCL = NLINK('IGCL',MODE)
        ENDIF
      END IF
C Find the offsets in the banks to be filled.
      IF (MODE.EQ.1) THEN
        IIGTL = KROW(KIGTL,IDTPC)
        IITFT = KROW(KITFT,IDTPC)
      ELSE
        IIGTL = KNEXT(KIGTL)
        IITFT = KNEXT(KITFT)
      END IF
      NTRK  = LROWS(KIGTL)
      NCL   = LROWS(KIGCL)
      KIO   = NCL*LCOLS(KIGCL)
      NHITR = IW(IIGTL+JIGTNR)
      IIGCL = KNEXT(KIGCL)
      KITC0 = NLINK('ITCO',0)
      KITC1 = NLINK('ITCO',1)
C
      NHIT  = 0
C IEL gives the number of coordinates deleted from each path.
C For MODE=2, it is always zero.
      IEL = 0
C
C "Loop" over number of coordinates deleted from each path (if MODE=1).
   5  CONTINUE
C
      NLNK = LPTHIP - IEL
      N2 = NLNK + 1
C Reject if too few links.
      IF(NLNK.LT.MNLINK(MODE)) GOTO 999
C
C Loop over paths in /IPATHC/.
C
      DO 50 I=1,NPTHIP
C Initialize the worst coordinate to zero, for the next loop over this
C path (in case it happens).
        IF (MODE.EQ.1) IWORST(IEL+1,I) = 0
C If a coordinate needs to be deleted, but none is particularly bad,
C give up on this path.
        IF (IEL.GT.0.AND.IWORST(IEL,I).EQ.0) GOTO 50
C
C Get 1st hit on path (unless it was deleted in previous loops).
C
        JA = 0
        LINK = LLNKIP(1,I)
        ICO = ITABL(ILNKIW,LINK,1)
        DO 6 K=1,IEL
          IF (IWORST(K,I).EQ.ICO) GOTO 7
    6   CONTINUE
        JA = JA + 1
        ILIST(JA) = ICO
    7   CONTINUE
C
C Loop over links on path: get hit at end of each link
C
        DO 10 J=1,NLNK+IEL
          LINK = LLNKIP(J,I)
          ICO = ITABL(ILNKIW,LINK,2)
          DO 8 K=1,IEL
            IF (IWORST(K,I).EQ.ICO) GOTO 9
    8     CONTINUE
          JA = JA + 1
          ILIST(JA) = ICO
    9     CONTINUE
   10   CONTINUE
C
        IF(FDEBRF.AND.JDBDRF(JULIT).GE.5) THEN
          WRITE(LDEBRL,15) I,(ILIST(J),J=1,N2)
   15     FORMAT(' IFITP: Path',I3,' coords:',8I5)
        ENDIF
C
C If we are dealing with a stand-alone track, then correct phi-coords.
C for the track's entrance angle through each ITC drift cell.
        IF (MODE.EQ.2) THEN
C   First do this very approximately;-
          DO 20 K=1,N2,2
            N = K
            IF (K.EQ.N2) N = K-1
            IC0 = ABS(ILIST(N))
            IC1 = ABS(ILIST(N+1))
            IA0 = -1
            IA1 = -1
            IF (ILIST(N).LT.0) IA0 = 0
            IF (ILIST(N+1).LT.0) IA1 = 0
            CALL IENANG(IC1,IA1,IC0,IA0,EN1,EN0)
            RW(KROW(KITC0,IC0)+JITCP2+IA0) = IPHCOR(IC0,IA0,EN0,0.0)
            RW(KROW(KITC0,IC1)+JITCP2+IA1) = IPHCOR(IC1,IA1,EN1,0.0)
            RW(KROW(KITC0,IC0)+JITCSR) = RTABL(KITC1,IC0,JITCSR)/
     +                                    MAX(ABS(COS(EN0)),0.1)**2
            RW(KROW(KITC0,IC1)+JITCSR) = RTABL(KITC1,IC1,JITCSR)/
     +                                    MAX(ABS(COS(EN1)),0.1)**2
   20     CONTINUE
C Then do it somewhat more accurately, by making a circle + line fit to
C the hits.
          IOPT  = 1
          NMULT = 0
          CALL UFITMS(0,N2,0,IDUM,ILIST,IDUM,IOPT,NMULT,RSCAIT,0.027,
     +             VV0,EE0,COV,CHI2,IERR)
          DO 25 K=1,N2
            IC = ABS(ILIST(K))
            IA = -1
            IF (ILIST(K).LT.0) IA = 0
            RLAY = RTABL(KITC0,IC,JITCRA)
            R0   = 1.0/VV0(1)
            D0   = VV0(4)
C Calculate the angle of the track through the cell at the cell radius.
            SINB = (0.5*(RLAY**2-D0**2)+R0*D0)/(RLAY*R0)
C Occasionally, the helix does not pass through layers containing its
C coordinates.
            IF (SINB.LT.-0.999) SINB = -0.999
            IF (SINB.GT.0.999) SINB = 0.999
            EN = ASIN(SINB)
            RW(KROW(KITC0,IC)+JITCP2+IA) = IPHCOR(IC,IA,EN,0.0)
            RW(KROW(KITC0,IC)+JITCSR) = RTABL(KITC1,IC,JITCSR)/
     +                                    MAX(ABS(COS(EN)),0.1)**2
   25     CONTINUE
        END IF
C
C Note the number of degrees of freedom in the fit.
        NCOZ = 0
        DO 27 J=1,N2
          ICO = ABS(ILIST(J))
          IITC0 = KROW(KITC0,ICO)
C Note the number of coords on the track with z-info.
          IF (RW(IITC0+JITCSZ).LT.10000.) NCOZ = NCOZ + 1
   27   CONTINUE
C
C Fit
C First fit just ITC points
C
        IOPT  = MODE
        NMULT = 0
        CALL UFITMS(0,N2,0,IDUM,ILIST,IDUM,IOPT,NMULT,RSCAIT,0.027,
     +             VV0,EE0,COV,CHI2,IERR)
C
C Find the worst coordinate (if any are really bad) on this path.
        IF (MODE.EQ.1) THEN
          CMAX = CHIELI
          DO 30 II = 1,N2
            IF ( CHIACU(II) .GT. CMAX ) THEN
              CMAX = CHIACU(II)
              IWORST(IEL+1,I) = ILIST(II)
            END IF
   30     CONTINUE
        END IF
C
C Calculate confidence level for fit and test
        NDF = N2 - 3 + MAX(NCOZ-2,0)
        CL = 0.0
        IF (CHI2.GT.0.0) CL = PROB(CHI2,NDF)
C
        IF(CL.GT.CLMINI(MODE)) THEN
C
C If mode=1 and 1st arc then fit to TPC + ITC
          IF(.NOT.SPIRAL.AND.MODE.EQ.1) THEN
            NMULT = 1
            IOPT  = 2
            KTGTL = IW(NTGTL)
            ITGTL = KROW(KTGTL,IDTPC)
            NHT = IW(ITGTL+JTGTN1)
            IOT = IW(ITGTL+JTGTIO)
            ILT = IW(NTGCL)+LMHLEN+IOT+1
            CALL UFITMS(NHT,N2,0,IW(ILT),ILIST,IDUM,IOPT,NMULT,
     +              RSCAIT,0.027,VV0,EE0,COV,CHI2,IERR)
C
C Use this Chisq to find best fit to store
            NDF  = 2*NHT + N2 + NCOZ - 5
            CL = 0.0
            IF (CHI2.GT.0.0) CL = PROB(CHI2,NDF)
          ENDIF
C
C Store best fit
          IF(CL.GT.CLMN) THEN
            IER = 0
            CLMN = CL
            NHIT  = N2
            IF (.NOT.SPIRAL) THEN
              IW(IIGTL+JIGTIO) = KIO
              IW(IIGTL+JIGTN1) = N2
              IF (MODE.EQ.2) IW(KIGTL+LMHROW) = NTRK + 1
            ELSE
              IF (IW(IIGTL+JIGTN1)+NHITR.EQ.0) IW(IIGTL+JIGTIO) = KIO
              IW(IIGTL+JIGTNR) = NHITR + N2
            END IF
C
            DO 35 J=1,N2
              IW(IIGCL+J) = ILIST(J)
C If in stand-alone phase then store corrected coords in case they
C get overwritten by another path.
              IF (MODE.EQ.2) THEN
                ICO = ABS(ILIST(J))
                IAM = -1
                IF (ILIST(J).LT.0) IAM = 0
                IITC0 = KROW(KITC0,ICO)
                PHCOR(J) = RW(IITC0+JITCP2+IAM)
                SRCOR(J) = RW(IITC0+JITCSR)
              END IF
   35       CONTINUE
            IW(KIGCL+LMHROW) = NCL + N2
C
            IF (.NOT.SPIRAL) THEN
              DO 40 J=1,6
                RW(IITFT+J)=VV0(J)
   40         CONTINUE
              DO 45 J=1,21
                RW(IITFT+JITFCM-1+J) = COV(J)
   45         CONTINUE
              RW(IITFT+JITFCD) = CHI2
              IW(IITFT+JITFND) = NDF
              IW(IITFT+JITFFT) = NMULT*100 + IOPT*10 + IERR
              IF (MODE.EQ.2) IW(KITFT+LMHROW) = NTRK + 1
            END IF
          ENDIF
        ENDIF
C If in stand-alone phase, reset corrected coordinates in case this will
C not be the best track. (For ITC-TPC phase, this is done in ITRECF).
        IF (MODE.EQ.2) THEN
          DO 47 K=1,N2
            ICO = ABS(ILIST(K))
            IAM = -1
            IF (ILIST(K).LT.0) IAM = 0
            IITC0 = KROW(KITC0,ICO)
            IITC1 = KROW(KITC1,ICO)
            RW(IITC0+JITCP2+IAM) = RW(IITC1+JITCP2+IAM)
            RW(IITC0+JITCSR) = RW(IITC1+JITCSR)
   47     CONTINUE
        END IF
C
   50 CONTINUE
C See if a track was stored.
      IF (NHIT.EQ.0) THEN
        IF (MODE.EQ.1) THEN
C If not and in ITC-TPC tracking phase, then try deleting the worst
C coordinate.
          IEL = IEL + 1
          GOTO 5
        END IF
C Else give up.
        GOTO 999
      END IF
      IF(FDEBRF.AND.JDBDRF(JULIT).GE.2) THEN
        IF (MODE.EQ.1) THEN
          WRITE(LDEBRL,70) IDTPC,I,CLMN,SPIRAL,
     +    (IW(IIGCL+II),II=1,NHIT)
   70     FORMAT(' TPC extension : ',2I5,' Confidence Level=',E13.3,
     +    ' SPIRAL=',L4,' coords.=',8(I5,:))
        ELSE
          WRITE(LDEBRL,75) (NTRK+1),I,CLMN,SPIRAL,
     +    (IW(IIGCL+J),II=1,NHIT)
   75     FORMAT(' ITC stand alone : ',2I5,' Confidence Level=',E13.3,
     +    ' SPIRAL=',L4,' coords.=',8(I5,:))
        END IF
      END IF
C
        IF(FDEBRF.AND.JDBDRF(JULIT).GE.3.AND..NOT.SPIRAL) THEN
          WRITE(LDEBRL,80) (RW(IITFT+J),J=1,6)
   80     FORMAT(' IFITP Fit:',4F10.4,F8.1,F10.5)
          WRITE(LDEBRL,85) (RW(IITFT+JITFCM-1+J),J=1,15)
   85     FORMAT(' IFITP Cov. Matrix: ',7E12.3,/,8E12.3)
        ENDIF
C
C Update used coordinate list
C
      DO 90 I=1,NHIT
        ICO = ABS(IW(IIGCL+I))
        IW(IUCOIW+LMHLEN+ICO) = 1
   90 CONTINUE
C
      IF (MODE.EQ.1) GOTO 999
C
C If in stand-alone phase, correct coordinates for entrance angle.
C (If in ITC-TPC phase, this has already been done).
      DO 92 J=1,NHIT
        ICO = ABS(IW(IIGCL+J))
        IAM = -1
        IF (ILIST(J).LT.0) IAM = 0
        IITC0 = KROW(KITC0,ICO)
        RW(IITC0+JITCP2+IAM) = PHCOR(J)
        RW(IITC0+JITCSR) = SRCOR(J)
   92 CONTINUE
C
C Update the link list.
C
      DO 100 ILNK=1,LROWS(ILNKIW)
        IPNT = KROW(ILNKIW,ILNK)
C If link ILNK has a coord. which has just been used, then set IGAP=0
        DO 95 J=1,NHIT
          ICO = ABS(IW(IIGCL+J))
          IF (ABS(IW(IPNT+1)).EQ.ICO.OR.ABS(IW(IPNT+2)).EQ.ICO)
     +          IW(IPNT+3) = 0
   95   CONTINUE
  100 CONTINUE
C
C Update the link pointer lists (but don't bother deleting pointers from
C links with IGAP=0).
C
      DO 250 M=1,LROWS(ILNKIW)
        IGAPM = ITABL(ILNKIW,M,3)
        IF (IGAPM.EQ.0) GOTO 250
C
        NOFF=0
        IPNT = INXLIW+LMHLEN+IW(IOFNIW+LMHLEN+M)
        DO 150 N=1,IW(INNXIW+LMHLEN+M)
          NNXTL = IW(IPNT+N)
          IGAPN = ITABL(ILNKIW,NNXTL,3)
          IF (IGAPN.EQ.0) GOTO 150
          NOFF = NOFF + 1
          IW(IPNT+NOFF) = NNXTL
  150   CONTINUE
        IW(INNXIW+LMHLEN+M) = NOFF
  250 CONTINUE
C
  999 CONTINUE
      END
#endif
