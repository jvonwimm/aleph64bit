      SUBROUTINE TFITTK(IER)
C-----**********************----------------------------------------
C! Fit TPC track candidates and produce TPC tracks
C!
C!    AUTHOR        :- MARTIN POPPE 87/9/25
C!    Modified      :  I.Tomalin    12/3/94
C!                     Protection put in requiring chains to have at
C!                     least 3 coords. (Necessary because TOVRLP
C!                     sometimes leaves chains with less than this).
C!                  :  D.Casper     09/5/97
C!                     Protect against crazy fits by calling sanity check
C!
C!    INPUT  BANKS  : 'TCAL' , 'TTCC' , 'TPCO'
C!    OUTPUT BANKS  : 'T1FT' , 'T1TL' , 'T1CL'
C!
C?    DESCRIPTION
C?    ===========
C?
C?    THE HELIX PARAMETERS OF ALL TRACK CANDIDATES ARE FITTED.
C?    TRACKS WITH A BAD CHI**2 PROBABILITY ARE MASSAGED:
C?
C?    FIRST, IT IS TRIED TO IMPROVE THE FIT BY EXCLUDING
C?    THE POINT WITH THE LARGEST CONTRIBUTION TO CHI**2.
C?
C?    IF THIS METHOD DOES NOT LEAD TO AN ACCEPTABLE TRACK,
C?    A KINK SEARCH "A LA STIMPFL" IS MADE: THE CRITERIUM FOR
C?    A KINK IS THAT THE DIFFERENCE IN CHI**2 BETWEEN THE "BEST"
C?    KINK POINT AND THE "WORST" KINK POINT HYPOTHESIS IS
C?    LARGER THAN A CUT VALUE (CALLED DCM IN THIS PROGRAM).
C?    IF A KINK HAS BEEN FOUND, THE TRACK IS SPLIT INTO TWO PIECES.
C?    IF THE FIT PROBABILITY OF ONE OF THESE PIECES IS TOO LOW,
C?    THE POINT(S) WITH THE LARGEST CONTRIBUTION TO CHI**2
C?    ARE REMOVED.
C?
C?    FOR THE REMAINING TRACK CANDIDATES, POINTS
C?    OF A TRACK CANDIDATE ARE THROWN AWAY "ONE BY ONE" TO OBTAIN
C?    A BEST FIT.
C?
C?    IF NEITHER MASSAGE HELPS, THE TRACK CANDIDATE (OR A PIECE
C?    THEREOF IN CASE OF A KINK) IS THROWN AWAY.
C?
C-------------------------------------------------------------------
#ifndef DOC
C
#include "bcs.h"
#include "t1ftjj.h"
#include "t1tljj.h"
#include "tcaljj.h"
#include "tpcojj.h"
#include "tpgpar.h"
C
      COMMON/TFTKWB/ IWORST
C
      LOGICAL FIRST
      REAL    DCHI2(LTPDRO)
      INTEGER IBUFF(LTPDRO)
      DATA FIRST/.TRUE./
C
C---> Steering constants follow:
C
      DATA MINPT/4/,PMIN/0.00030/,LOMAX/2/,MKINK/10/,DCM/150.0/
C          |        |            |         |         D(CHI**2)/KINK
C          |        |            |         +- MIN. POINTS FOR KINK
C          |        |            +- MAX. POINTS TO THROW AWAY
C          |        +- MIN. FIT PROBABILITY FOR TRACK
C          +- MINIMAL NUMBER OF POINTS ON A TRACK
C
C---------------------- statement function definitions ---------------
C
#include "bmacro.h"
C
C-----> OBTAIN LIST AND NUMBER OF POINTS FOR TRACK CANDIDATE 'I'
      KKTTCC(II) = KTTCC+LMHLEN+ITABL(KTCAL,II,JTCAIO)
      N1CORD(II) = ITABL(KTCAL,II,JTCANF)
C-----> OBTAIN LIST AND NUMBER OF POINTS FOR TRACK CANDIDATE 'I'
      KKT1CL(II) = KT1CL+LMHLEN+ITABL(KT1TL,II,JT1TIO)
C-----> POINTER BEFORE COORDINATE LIST IN 'T1CL'
C
C---------------------- end of statement functions ------------------
C
      IF(FIRST) THEN
        IWORST=0
        NT1FT = NAMIND('T1FT')
        NT1TL = NAMIND('T1TL')
        NT1CL = NAMIND('T1CL')
        NTTRC = NAMIND('TTRC')
        NTCAL = NAMIND('TCAL')
        NTTCC = NAMIND('TTCC')
        NTPCO = NAMIND('TPCO')
        FIRST = .FALSE.
      END IF
      IER = 0
C
C---> Check for necessary input banks
C
      KTTRC = IW(NTTRC)
      KTCAL = IW(NTCAL)
      KTTCC = IW(NTTCC)
      KTPCO = IW(NTPCO)
      IF(KTTRC.LE.0)                              GO TO 998
      IF(KTCAL.LE.0)                              GO TO 998
      IF(KTTCC.LE.0)                              GO TO 998
      IF(KTPCO.LE.0)                              GO TO 998
C
C---> Make sure that output banks are not already present
C
      IF (IW(NT1FT).NE.0) CALL BDROP(IW,'T1FT')
      IF (IW(NT1TL).NE.0) CALL BDROP(IW,'T1TL')
      IF (IW(NT1CL).NE.0) CALL BDROP(IW,'T1CL')
C
C---> Create workbank for array IWORST
C--->   IWORST = 0       -> Track candidate has been accepted
C       IWORST = IPT > 0 -> Ipt is the worst point on the track
C                             (track not yet accepted)
      NCAND = LROWS(KTCAL)
      IF(NCAND.LE.0)                                GO TO 1000
      IW(1)=1
      CALL WBANK(IW,IWORST,NCAND,*995)
C
C---> Creation of output banks
C---> Create banks of size such that all particles are kinks
C       (Twice as large as no kink, -> always large enough)
C
      LT1FT = LT1FTA*2*NCAND + LMHLEN
      LT1TL = LT1TLA*2*NCAND + LMHLEN
      LT1CL = IW(KTTCC)
      CALL AUBOS('T1FT',0,LT1FT,KT1FT,IGARB)
      IF (IGARB.EQ.2) GO TO 999
      IF (IGARB.EQ.1) THEN
        KTTRC = IW(NTTRC)
        KTCAL = IW(NTCAL)
        KTTCC = IW(NTTCC)
        KTPCO = IW(NTPCO)
      END IF
      CALL AUBOS('T1TL',0,LT1TL,KT1TL,IGARB)
      IF (IGARB.EQ.2) GO TO 999
      IF (IGARB.EQ.1) THEN
        KT1FT = IW(NT1FT)
        KTTRC = IW(NTTRC)
        KTCAL = IW(NTCAL)
        KTTCC = IW(NTTCC)
        KTPCO = IW(NTPCO)
      END IF
      CALL AUBOS('T1CL',1,LT1CL,KT1CL,IGARB)
      IF (IGARB.EQ.2) GO TO 999
      IF (IGARB.EQ.1) THEN
         KT1FT = IW(NT1FT)
         KT1TL = IW(NT1TL)
         KTTRC = IW(NTTRC)
         KTCAL = IW(NTCAL)
         KTTCC = IW(NTTCC)
         KTPCO = IW(NTPCO)
      END IF
C
C---> Define number of columns
C
      IW(KT1FT+LMHCOL) = LT1FTA
      IW(KT1TL+LMHCOL) = LT1TLA
      IW(KT1CL+LMHCOL) = 1
C
C*******************************************************************C
C                                                                   C
C       SIMPLE TRACKS = TRACKS WITH DECENT CHI**2 BEFORE FUDGEING   C
C                                                                   C
C*******************************************************************C
C
      NTKOK = 0
      NPTOK = 0
      DO 100 ICAND = 1,NCAND
        JT1FT = KROW(KT1FT,NTKOK+1)
        NPT   = N1CORD(ICAND)
C Protection against chains with less than three coords.
        IF (NPT.LT.3) THEN
          IW(IWORST+ICAND)=0
          GOTO 100
        END IF
        KSTCC = KKTTCC(ICAND)
        CALL TMSHEL('TPCO',NPT,IW(KSTCC+1),2,RW(JT1FT+JT1FIR),
     &                           RW(JT1FT+JT1FEM),RW(JT1FT+JT1FCD))
        IW(JT1FT+JT1FDF) = 2*NPT-5
        PPP = PROB(RW(JT1FT+JT1FCD),(2*NPT-5))
C---->  Protect against hopeless fits
        CALL TFCHEK(RW(JT1FT+JT1FIR),.TRUE.,IERR)
        IF (PPP.GT.PMIN .AND. IERR.EQ.0) THEN
          NTKOK = NTKOK+1
          IW(IWORST+ICAND)=0
          IW(JT1FT+JT1FTC)=ICAND
          IW(JT1FT+JT1FTG)=0
C----->   Candidate -> 'T1TL'
          KSCAL = KROW(KTCAL,ICAND)
          KSGTL = KROW(KT1TL,NTKOK)
          IW(KSGTL+JT1TIO) = NPTOK
          IW(KSGTL+JT1TN1) = IW(KSCAL+JTCANF)
          IW(KSGTL+JT1TNR) = IW(KSCAL+JTCANR)
C----->   Candidate -> 'T1CL'
          KSGCL = KKT1CL(NTKOK)
          NLONG = IW(KSGTL+JT1TN1)+IW(KSGTL+JT1TNR)
          DO 80 I=1,NLONG
            IW(KSGCL+I) = IW(KSTCC+I)
   80     CONTINUE
          NPTOK = NPTOK+NLONG
        ELSE
C-----> Else, determine which was the worst point
          CHI2  = 0.0
          CALL TMSHEC(DCHI2)
          DO 90 IPT =1,NPT
          IF(DCHI2(IPT).GT.CHI2) THEN
            CHI2 = DCHI2(IPT)
            IW(IWORST+ICAND)=IPT
          END IF
   90     CONTINUE
        END IF
  100 CONTINUE
C
C*******************************************************************C
C                                                                   C
C       IMPROVE TRACKS BY REMOVING POINTS OF LARGE CHI**2           C
C                                                                   C
C*******************************************************************C
C
      DO 200 ICAND =1,NCAND
        IF(IW(IWORST+ICAND).EQ.0) GO TO 200
        NPT   = N1CORD(ICAND)
        IF(NPT.LE.MINPT)       GO TO 200
        KSTCC = KKTTCC(ICAND)
        JT1FT = KROW(KT1FT,NTKOK+1)
C---->  Fill buffer with points
        DO 110 IPT=1,NPT
          IBUFF(IPT) = IW(KSTCC+IPT)
  110   CONTINUE
        LOST = 0
C
  111   CONTINUE
C
C---->  Remove worst point
        IF (IW(IWORST+ICAND).LT.NPT) THEN
          DO 112 IPT=IW(IWORST+ICAND)+1,NPT
            IBUFF(IPT-1)=IBUFF(IPT)
  112     CONTINUE
        END IF
        NPT = NPT-1
        LOST= LOST+1
C---->  Now fit again
        CALL TMSHEL('TPCO',NPT,IBUFF,2,RW(JT1FT+JT1FIR),
     &                     RW(JT1FT+JT1FEM),RW(JT1FT+JT1FCD))
        IW(JT1FT+JT1FDF) = 2*NPT-5
        PPP = PROB(RW(JT1FT+JT1FCD),(2*NPT-5))
C---->  Take out the garbage
        CALL TFCHEK(RW(JT1FT+JT1FIR),.TRUE.,IERR)
        IF (PPP.GT.PMIN .AND. IERR.EQ.0) THEN
          NTKOK = NTKOK+1
          IW(IWORST+ICAND)=0
          IW(JT1FT+JT1FTC)=ICAND
          IW(JT1FT+JT1FTG)=0
C----->   Candidate -> 'T1TL'
          KSCAL = KROW(KTCAL,ICAND)
          KSGTL = KROW(KT1TL,NTKOK)
          IW(KSGTL+JT1TIO) = NPTOK
          IW(KSGTL+JT1TN1) = NPT
          IW(KSGTL+JT1TNR) = IW(KSCAL+JTCANR)
C----->   Candidate -> 'T1CL'
          KSGCL = KKT1CL(NTKOK)
          DO 130 IPT=1,NPT
            IW(KSGCL+IPT)=IBUFF(IPT)
  130     CONTINUE
          NGET  = IW(KSGTL+JT1TNR)
          IF (NGET.GT.0) THEN
            IGET  = KSTCC+IW(KSCAL+JTCANF)
            IPUT  = KSGCL+NPT
            DO 131 IPT=1,NGET
              IW(IPUT+IPT)=IW(IGET+IPT)
  131       CONTINUE
          END IF
          NPTOK = NPTOK+NPT+NGET
        ELSE
C-----> Else, determine again, which was the worst point
          CHI2  = 0.0
          CALL TMSHEC(DCHI2)
          DO 140 IPT =1,NPT
          IF(DCHI2(IPT).GT.CHI2) THEN
            CHI2 = DCHI2(IPT)
            IW(IWORST+ICAND)=IPT
          END IF
  140     CONTINUE
        END IF
C
        IF(IW(IWORST+ICAND).NE.0
     1                 .AND.LOST.LT.LOMAX
     2                        .AND.NPT.GT.MINPT)       GO TO 111
C          Jump back to 111 if another point is to be removed
C-----------------------------------------------------------------+
  200 CONTINUE
C
C*******************************************************************C
C                                                                   C
C              SEARCH FOR KINKS                                     C
C                                                                   C
C*******************************************************************C
C
      DO 300 ICAND =1,NCAND
        IF (IW(IWORST+ICAND).EQ.0)                 GO TO 300
        NPT = N1CORD(ICAND)
        IF (NPT.LE.MKINK)                          GO TO 300
        KSTCC = KKTTCC(ICAND)
        JT1FT = KROW(KT1FT,NTKOK+1)
        CALL TKINK(DCF,KINK,IW(KSTCC+1),NPT)
        IF (DCF.LT.DCM)                            GO TO 300
        IACC1 = 0
        IACC2 = 0
        NPT1 = KINK-1
        NPT2 = NPT-NPT1
C
C-----> Kink is accepted, process 1st half of candidate
C
        DO 210 IPT=1,NPT1
          IBUFF(IPT) = IW(KSTCC+IPT)
  210   CONTINUE
        LOST = 0
C
        CALL TMSHEL('TPCO',NPT1,IBUFF,2,RW(JT1FT+JT1FIR),
     &                    RW(JT1FT+JT1FEM),RW(JT1FT+JT1FCD))
C-----> See if track is garbage
        CALL TFCHEK(RW(JT1FT+JT1FIR),.TRUE.,IERR)
        IF (IERR.NE.0) THEN
            IW(ICAND+IWORST) = 0
            GOTO 240
        ENDIF
        IW(JT1FT+JT1FDF) = 2*NPT1-5
        PPP = PROB(RW(JT1FT+JT1FCD),(2*NPT1-5))
        IACC1 = 1
        NTKOK = NTKOK+1
        IW(IWORST+ICAND)=0
        IW(JT1FT+JT1FTC)=ICAND
        IW(JT1FT+JT1FTG)=0
C--->   Candidate -> 'T1TL'
        KSCAL = KROW(KTCAL,ICAND)
        KSGTL = KROW(KT1TL,NTKOK)
        IW(KSGTL+JT1TIO) = NPTOK
        IW(KSGTL+JT1TN1) = NPT1
        IW(KSGTL+JT1TNR) = 0
C--->   Candidate -> 'T1CL'
        KSGCL = KKT1CL(NTKOK)
        DO 230 IPT=1,NPT1
          IW(KSGCL+IPT)=IBUFF(IPT)
  230   CONTINUE
        NPTOK = NPTOK+NPT1
C
C-----> Now process 2nd half of candidate
C
  240   CONTINUE
        JT1FT = KROW(KT1FT,NTKOK+1)
        DO 250 IPT=1,NPT2
          IBUFF(IPT) = IW(KSTCC+IPT+KINK-1)
  250   CONTINUE
        LOST = 0
        CALL TMSHEL('TPCO',NPT2,IBUFF,2,RW(JT1FT+JT1FIR),
     &                     RW(JT1FT+JT1FEM),RW(JT1FT+JT1FCD))
C-----> Check for garbage
        CALL TFCHEK(RW(JT1FT+JT1FIR),.TRUE.,IERR)
        IF (IERR.NE.0) THEN
            IW(IWORST+ICAND) = 0
            GOTO 300
        ENDIF
        IW(JT1FT+JT1FDF) = 2*NPT2-5
        IACC2 = 1
        NTKOK = NTKOK+1
        IW(IWORST+ICAND)=0
        IW(JT1FT+JT1FTC)=ICAND
        IW(JT1FT+JT1FTG)=0
C-----> Candidate -> 'T1TL'
        KSCAL = KROW(KTCAL,ICAND)
        KSGTL = KROW(KT1TL,NTKOK)
        IW(KSGTL+JT1TIO) = NPTOK
        IW(KSGTL+JT1TN1) = NPT2
        IW(KSGTL+JT1TNR) = IW(KSCAL+JTCANR)
C--->   Candidate -> 'T1CL'
        KSGCL = KKT1CL(NTKOK)
        DO 260 IPT=1,NPT2
          IW(KSGCL+IPT)=IBUFF(IPT)
  260   CONTINUE
C
        NGET  = IW(KSGTL+JT1TNR)
        IF (NGET.GT.0) THEN
          IGET  = KSTCC+IW(KSCAL+JTCANF)
          IPUT  = KSGCL+NPT2
          DO 261 IPT=1,NGET
            IW(IPUT+IPT)=IW(IGET+IPT)
  261     CONTINUE
        END IF
        NPTOK = NPTOK+NPT2+NGET
        IF ((IACC1+IACC2).GT.0) IW(IWORST+ICAND)=0
  300 CONTINUE
C
C*******************************************************************C
C                                                                   C
C       IMPROVE TRACKS BY REMOVING POINTS OF LARGE CHI**2           C
C              (2-ND STRATEGY = FIND BEST CHI**2 )                  C
C                                                                   C
C*******************************************************************C
C
      DO 400 ICAND =1,NCAND
        IF(IW(IWORST+ICAND).EQ.0) GO TO 400
        NPT   = N1CORD(ICAND)
        IF(NPT.LE.MINPT)       GO TO 400
        KSTCC = KKTTCC(ICAND)
C---->  Fill buffer with points
        DO 310 IPT=1,NPT
          IBUFF(IPT) = IW(KSTCC+IPT)
  310   CONTINUE
        LOST = 0
C
C------ Jump back to 311 if another point is to be removed
C
  311   CONTINUE
C
C---->  Remove worst point
        CALL TREMPT(PBEST,IBUFF,IBUFF,NPT)
        NPT = NPT-1
        LOST= LOST+1
        IF (PBEST.GE.PMIN) THEN
C          NTKOK = NTKOK+1
          JT1FT = KROW(KT1FT,NTKOK+1)
          CALL TMSHEL('TPCO',NPT,IBUFF,2,RW(JT1FT+JT1FIR),
     &                     RW(JT1FT+JT1FEM),RW(JT1FT+JT1FCD))
C----->   Check for garbage
          CALL TFCHEK(RW(JT1FT+JT1FIR),.TRUE.,IERR)
          IF (IERR.NE.0) THEN
            IW(IWORST+ICAND) = 0
            GOTO 400
          ENDIF
          NTKOK = NTKOK+1
          IW(JT1FT+JT1FDF) = 2*NPT-5
          IW(IWORST+ICAND)=0
          IW(JT1FT+JT1FTC)=ICAND
          IW(JT1FT+JT1FTG)=0
C----->   Candidate -> 'T1TL'
          KSCAL = KROW(KTCAL,ICAND)
          KSGTL = KROW(KT1TL,NTKOK)
          IW(KSGTL+JT1TIO) = NPTOK
          IW(KSGTL+JT1TN1) = NPT
          IW(KSGTL+JT1TNR) = IW(KSCAL+JTCANR)
C----->   Candidate -> 'T1CL'
          KSGCL = KKT1CL(NTKOK)
          DO 330 IPT=1,NPT
            IW(KSGCL+IPT)=IBUFF(IPT)
  330     CONTINUE
          NGET  = IW(KSGTL+JT1TNR)
          IF (NGET.GT.0) THEN
            IGET  = KSTCC+IW(KSCAL+JTCANF)
            IPUT  = KSGCL+NPT
            DO 331 IPT=1,NGET
              IW(IPUT+IPT)=IW(IGET+IPT)
  331       CONTINUE
          END IF
          NPTOK = NPTOK+NPT+NGET
        END IF
        IF (IW(IWORST+ICAND).NE.0
     1                .AND.LOST.LT.LOMAX
     2                      .AND.NPT.GT.MINPT)       GO TO 311
C----------- Jump back to 111 if another point is to be removed
  400 CONTINUE
C
C*******************************************************************C
C                                                                   C
C               KEEP REMAINING TRACKS AS THEY ARE                   C
C                                                                   C
C*******************************************************************C
C
      DO 500 ICAND =1,NCAND
        IF (IW(IWORST+ICAND).EQ.0) GO TO 500
        JT1FT = KROW(KT1FT,NTKOK+1)
        NPT   = N1CORD(ICAND)
        KSTCC = KKTTCC(ICAND)
        CALL TMSHEL('TPCO',NPT,IW(KSTCC+1),2,RW(JT1FT+JT1FIR),
     &                         RW(JT1FT+JT1FEM),RW(JT1FT+JT1FCD))
C--->   Garbage check
        CALL TFCHEK(RW(JT1FT+JT1FIR),.TRUE.,IERR)
        IF (IERR.NE.0) THEN
            IW(IWORST+ICAND) = 0
            GOTO 500
        ENDIF
        IW(JT1FT+JT1FDF) = 2*NPT-5
C
C--->   IWORST = 0       -> Track candidate has been accepted
C                           (track not yet accepted)
        NTKOK = NTKOK+1
        IW(IWORST+ICAND)=0
        IW(JT1FT+JT1FTC)=ICAND
        IW(JT1FT+JT1FTG)=0
C---->  Candidate -> 'T1TL'
        KSCAL = KROW(KTCAL,ICAND)
        KSGTL = KROW(KT1TL,NTKOK)
        IW(KSGTL+JT1TIO) = NPTOK
        IW(KSGTL+JT1TN1) = IW(KSCAL+JTCANF)
        IW(KSGTL+JT1TNR) = IW(KSCAL+JTCANR)
C-----> Candidate -> 'T1CL'
        KSGCL = KKT1CL(NTKOK)
        NLONG = IW(KSGTL+JT1TN1)+IW(KSGTL+JT1TNR)
        DO 480 I=1,NLONG
          IW(KSGCL+I) = IW(KSTCC+I)
  480   CONTINUE
        NPTOK = NPTOK+NLONG
  500 CONTINUE
C
C---> Fill remaining words in banks and set the lengths
C
      IW(KT1FT+LMHROW)=NTKOK
      IW(KT1TL+LMHROW)=NTKOK
      IW(KT1CL+LMHROW)=NPTOK
      CALL AUBPRS('T1FTT1TLT1CL')
      CALL BLIST(IW,'T+','T1FTT1TLT1CL')
C
C*******************************************************************C
C                                                                   C
C              RELABEL THE 'TPCO' BANK                              C
C                                                                   C
C*******************************************************************C
C
      NCOR = LROWS(KTPCO)
      DO 800 I=1,NCOR
        IW(KROW(KTPCO,I)+JTPCTN)=0
  800 CONTINUE
      NTK = LROWS(KT1FT)
      DO 820 ITK = 1,NTK
        JT1TL = KROW(KT1TL,ITK)
        I1 = IW(JT1TL+JT1TIO)
        I2 = IW(JT1TL+JT1TN1)
        I3 = IW(JT1TL+JT1TNR)
        DO 810 IPT =1,I2
          ICO = IW((KT1CL+LMHLEN+I1)+IPT)
          IW(KROW(KTPCO,ICO)+JTPCTN) = ITK
  810   CONTINUE
        DO 813 IPT=1,I3
          ICO = IW((KT1CL+LMHLEN+I1+I2)+IPT)
          IW(KROW(KTPCO,ICO)+JTPCTN) = -ITK
  813   CONTINUE
  820 CONTINUE
C
C     Call the subroutine that converts the parameter definition
C     from TASSO convention to standard convention.
C     This is done by actually refitting the track yet again and
C     overwriting the T1FT bank which was filled above.
C
      CALL UCTGFT
C
      GO TO 1000
  995 CONTINUE
      IER=1
      CALL RERROR('TFITTK',IER,'No room in BOS for workbank')
      GO TO 1000
  998 CONTINUE
      IER = 2
      CALL RERROR('TFITTK',IER,'Input banks not found')
      GO TO 1000
  999 CONTINUE
      IER = 3
      CALL RERROR('TFITTK',IER,'Not enough space in IW')
      CALL BDROP(IW,'T1FTT1TLT1CL')
C
 1000 CONTINUE
      CALL WDROP(IW,IWORST)
      RETURN
      END
#endif
