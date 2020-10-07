      SUBROUTINE TFCAND(IER)
C-----*****************---------------------------------------------
C! Assemble tpc chains to track candidates
C!
C!    AUTHOR        :- MARTIN POPPE 87/5/25
C!    Modified      :- Robert Johnson 88/9/14
C!    Modified      :- Marco Cattaneo 99/12/09
C!      Kill hits on a chain if there are more than 21 pad rows in the chain, 
C!      due to pattern recognition error that should never happen.....
C!
C!    OUTPUT: IER  = ERROR CODE: =0 IF OK, =1 IF INCOMPLETE
C!
C!    INPUT  BANKS  : 'TCHA' , 'TCTC' , 'TLNK'
C!    OUTPUT BANKS  : 'TCAL' , 'TTCC' , 'TARC', 'TATC'
C!
C!    DESCRIPTION
C!    ===========
C!
C!    According to the table of links between chains provided by
C!    the bank 'tlnk', sets of chains are assembled to form the
C!    so called track candidates. Track candidates are sets of
C!    tpc pad coordinates which are assumed to originate in the
C!    same particle. the coordinates are ordered according to
C!    their path of flight. For non spiraling tracks, this path
C!    is assumed to point away from the beam line. For spiraling
C!    tracks, the order is deduced from the assumption that
C!    the particles loose energy as they traverse the chamber.
C!    All pad coordinates that belong to a chain will also
C!    belong to a track candidate. Every chain will be part
C!    of exactly one track candidate.
C!    Also, the TARC and TATC banks are filled with the chains
C!    which are not in the first arc of a track candidate.
C!    This is done in order to make available the helix parameters
C!    of the extra loops of track candidates.
C!
C-------------------------------------------------------------------
#ifndef DOC
C
#include "bcs.h"
#include "tarcjj.h"
#include "tchajj.h"
#include "tlnkjj.h"
#include "tcaljj.h"
#include "tpgpar.h"
C
      COMMON/TCANWB/ITCALW
      PARAMETER (MXFIT=20)
      DIMENSION IBUFF(LTPDRO),XFT(MXFIT),YFT(MXFIT),EFT(MXFIT)
      LOGICAL FIRST,TAKEN,FSWAP
      INTEGER CDUMY,CTYPE
      DATA FIRST/.TRUE./
C
C---> The following parameters should eventually go on data base:
C
      DATA SCUT/60.0/,D0CUT/100./,Z0CUT/2./
C
C     SCUT= significance cut on the rate of change of curvature of
C           a track.  If the measured rate of change divided by
C           the estimated uncertainty is less in absolute value than
C           SCUT, then this is assumed not to be a significant
C           measure of which direction the particle is going.
C           If the change of curvature is not significant, then
C           take the end of the helix closest to the origin in
C           z if the two ends are further apart in z than Z0CUT
C           and at least one end comes within D0CUT of the beamline.
C           If those conditions aren't satified, use the curvature to
C           decide, even though it is not significant.
C
C
C     Description of workbank ITCALW:
C
C     Words 1..nchains        Flag for used chains
C     nchains+1..2*nchains    list of chains belonging to candidate
C     2*nchains+1..3*nchains  link type to next chain
C
C---------------------- Statement Functions -------------------------
C
#include "bmacro.h"
C
C---> Offset in TCTC for first coordinate of track II
      KKTCTC(II) = KTCTC+LMHLEN+ITABL(KTCHA,II,JTCHOF)
C---> Functions to reference the workbank ITCALW
      TAKEN(II) = IW(ITCALW+II).EQ.1
C---> IPLNK = Array of chains belonging to the track candidate
      IPLNK(II) = IW(JPLNKW+II)
C---> CTYPE = Link types to next chain for each chain of candidate
      CTYPE(II) = IW(JTYPEW+II)
C
C--------------------------------------------------------------------
C
      IF (FIRST) THEN
        ITCALW=0
        NTCHA = NAMIND('TCHA')
        NTCTC = NAMIND('TCTC')
        NTLNK = NAMIND('TLNK')
        NTCAL = NAMIND('TCAL')
        NTTCC = NAMIND('TTCC')
        NTARC = NAMIND('TARC')
        NTATC = NAMIND('TATC')
        FIRST = .FALSE.
      END IF
      IER=0
      KTCHA = IW(NTCHA)
      IF (KTCHA.LE.0)                               GO TO 998
      KTCTC = IW(NTCTC)
      IF (KTCTC.LE.0)                               GO TO 998
      KTLNK = IW(NTLNK)
      IF (KTLNK.LE.0)                               GO TO 998
      NCHAN = LROWS(KTCHA)
C
C-----> create banks of max possible size / drop old ones if necessary
C
      IF (IW(NTTCC).NE.0) CALL BDROP(IW,'TTCC')
      IF (IW(NTCAL).NE.0) CALL BDROP(IW,'TCAL')
      IF (IW(NTARC).NE.0) CALL BDROP(IW,'TARC')
      IF (IW(NTATC).NE.0) CALL BDROP(IW,'TATC')
C
      CALL BLIST(IW,'T+','TARCTTCCTCALTATC')
      NCOOD = LROWS(KTCTC)
      LTTCC = LMHLEN+NCOOD
      IW(1)=1
      CALL AUBOS('TTCC',1,LTTCC,KTTCC,IGARB)
      IF (IGARB.EQ.2) GO TO 999
      IW(KTTCC+LMHCOL) = 1
      LTCAL = LMHLEN + LTCALA*NCHAN
      IW(1)=1
      CALL AUBOS('TCAL',1,LTCAL,KTCAL,IGARB)
      IF (IGARB.EQ.2) GO TO 999
      IW(KTCAL+LMHCOL) = LTCALA
C
      LTARC= NCHAN*LTARCA + LMHLEN
      IW(1)=1
      CALL AUBOS('TARC',0,LTARC,KTARC,IGARB)
      IF (IGARB.EQ.2) GO TO 999
      IW(KTARC+LMHCOL)=LTARCA
      NARCS=0
      LTATC= IW(KTCTC)
      IW(1)=1
      CALL AUBOS('TATC',0,LTATC,KTATC,IGARB)
      IF (IGARB.EQ.2) GO TO 999
      IW(KTATC+LMHCOL)=1
      LTATC=0
C
      KTCHA = IW(NTCHA)
      KTCTC = IW(NTCTC)
      KTLNK = IW(NTLNK)
      KTCAL = IW(NTCAL)
      KTARC = IW(NTARC)
      KTTCC = IW(NTTCC)
C
C---> Create workbank for temporary storage of candidates
C
      IW(1)=1
      CALL WBANK(IW,ITCALW,3*NCHAN,*995)
      IW(ITCALW-3)=INTCHA('TCAW')
      CALL VZERO(IW(ITCALW+1),NCHAN)
      JPLNKW = ITCALW+NCHAN
      JTYPEW = ITCALW+2*NCHAN
C
      NPUT  = KTTCC+LMHLEN
C          (= absolute offset for next coordinates to be stored)
      NSTOR = 0
C          (= number of coordinates stored already )
      NCAND = 0
C          (= number of track candidates)
C-------------------------------------------------------------------C
C              TREATMENT OF CHAINS WITH A SINGLE LINK               C
C              **************************************               C
C-------------------------------------------------------------------C
      DO 400 ICHAN = 1,NCHAN
        IF (TAKEN(ICHAN))                       GO TO 400
        KSLNK = KROW(KTLNK,ICHAN)
        NLNKS  = 0
        KC1    = IW(KSLNK+JTLNUP)
        KC2    = IW(KSLNK+JTLNDN)
        KC3    = IW(KSLNK+JTLNTP)
        KC4    = IW(KSLNK+JTLNBM)
        IF (KC1.GT.0 .AND. (.NOT.TAKEN(KC1))) NLNKS = NLNKS +1
        IF (KC2.GT.0 .AND. (.NOT.TAKEN(KC2))) NLNKS = NLNKS +1
        IF (KC3.GT.0 .AND. (.NOT.TAKEN(KC3))) NLNKS = NLNKS +1
        IF (KC4.GT.0 .AND. (.NOT.TAKEN(KC4))) NLNKS = NLNKS +1
        IF (NLNKS.NE.1)                          GO TO 400
        NCAND = NCAND + 1
        KSCAL = KROW(KTCAL,NCAND)
        IW(KSCAL+JTCAIO) = NSTOR
        JCH = ICHAN
        NJCH= 1
        NTB = 0
        IW(JPLNKW+1) = ICHAN
        IW(JPLNKW+NJCH) = JCH
C
C-----> Mark the chain used.  Then loop over linked chains.
C
        IW(ITCALW+JCH) = 1
  110   CONTINUE
          KSLNK = KROW(KTLNK,JCH)
          KCH = 0
          JCH = IW(KSLNK+JTLNUP)
          IF (JCH.NE.0 .AND. (.NOT.TAKEN(JCH))) THEN
            IW(JTYPEW+NJCH)=JTLNUP
            KCH = JCH
          END IF
          JCH = IW(KSLNK+JTLNDN)
          IF (JCH.NE.0 .AND. (.NOT.TAKEN(JCH))) THEN
            IW(JTYPEW+NJCH)=JTLNDN
            KCH = JCH
          END IF
          JCH = IW(KSLNK+JTLNTP)
          IF (JCH.NE.0 .AND. (.NOT.TAKEN(JCH))) THEN
            IW(JTYPEW+NJCH)=JTLNTP
            KCH = JCH
            NTB = NTB+1
          END IF
          JCH = IW(KSLNK+JTLNBM)
          IF (JCH.NE.0 .AND. (.NOT.TAKEN(JCH))) THEN
            IW(JTYPEW+NJCH)=JTLNBM
            KCH = JCH
            NTB = NTB+1
          END IF
          IF (KCH.EQ.0)           GO TO 111
          JCH  = KCH
          NJCH = NJCH + 1
          IW(JPLNKW+NJCH)=JCH
C
C-------> Mark the chain used, then go to the next link
C
          IW(ITCALW+JCH) = 1
          GO TO 110
  111   CONTINUE
        IW(JTYPEW+NJCH)=-1
C
C--->   NCAND = current track candidate number
C       NTB   = number of tob / bottom links
C       NJCH  = number of chains making the spiral
C
C--->   Now decide in which order chains are stored and reverse the
C       order of the list if necessary.   The algorithm is:
C       Fit a straight line to the points x=link number vs. y=1/R.
C       If the slope is positive, then the list is in the right order,
C       otherwise it must be reversed.  If the slope is consistant with
C       zero and the links pass close to the beam line, then the
C       first arc is that with the smallest z0.
C
        FSWAP=.FALSE.
        IF (NJCH.GT.1 .AND. NTB.GT.0) THEN
          NFIT=0
          KCH=1
  113     IF (KCH.GT.NJCH) GO TO 118
            PT=0.
            SPT=0.
            RNPT=0.
  114       CONTINUE
              PT=PT+ABS(RTABL(KTCHA,IPLNK(KCH),JTCHIR))
              SPT=SPT + 1./RTABL(KTCHA,IPLNK(KCH),JTCHER)
              RNPT=RNPT+1.
              IF (CTYPE(KCH).NE.JTLNUP .AND. CTYPE(KCH).NE.JTLNDN) THEN
                GO TO 115
              ENDIF
              KCH=KCH+1
              GO TO 114
  115       CONTINUE
            PT=PT/RNPT
            SPT=SPT/RNPT
            IF (NFIT.LT.MXFIT) THEN
              NFIT=NFIT+1
              XFT(NFIT)=FLOAT(NFIT)
              YFT(NFIT)=PT
              EFT(NFIT)=SPT
            ENDIF
            KCH=KCH+1
            GO TO 113
  118     CONTINUE
C
C--->     Weighted fit to a straight line
C
          SX=0.
          SX2=0.
          SY=0.
          SXY=0.
          SS=0.
          DO 119 I=1,NFIT
            W=1./EFT(NFIT)
            SX=SX+XFT(I)*W
            SY=SY+YFT(I)*W
            SXY=SXY+XFT(I)*YFT(I)*W
            SX2=SX2+(XFT(I)**2)*W
            SS=SS+W
  119     CONTINUE
          DET= SS*SX2 - SX**2
          IF (DET.LE.0.) THEN
            B=1.0
            SB=100.
          ELSE
            B= (SS*SXY - SX*SY)/DET
            SB= SQRT(SS/DET)
          ENDIF
C
C--->     Decide which end of the helix is the start
C
          IF (B/SB.LT.-SCUT) THEN
            FSWAP=.TRUE.
          ELSEIF (B/SB.LT.SCUT) THEN
            D01=ABS(RTABL(KTCHA,IPLNK(1),JTCHD0))
            Z01= RTABL(KTCHA,IPLNK(1),JTCHZ0)
            D02=ABS(RTABL(KTCHA,IPLNK(NJCH),JTCHD0))
            Z02= RTABL(KTCHA,IPLNK(NJCH),JTCHZ0)
            IF (ABS(Z02).LT.ABS(Z01) .AND. D02.LT.D0CUT) THEN
              FSWAP=.TRUE.
            ELSEIF (D01.GE.D0CUT) THEN
              IF (B.LT.0.) FSWAP=.TRUE.
            ENDIF
          ENDIF
        ENDIF
C
C--->   Reverse the order of chains if necessary
C
        IF (FSWAP) THEN
          LAST = NJCH/2
          DO 120 I=1,LAST
            IDUMY = IPLNK(I)
            CDUMY = CTYPE(I)
            IW(JPLNKW+I) = IPLNK(1+NJCH-I)
            IW(JTYPEW+I) = CTYPE(NJCH-I)
            IW(JPLNKW+1+NJCH-I) = IDUMY
            IW(JTYPEW+NJCH-I) = CDUMY
  120     CONTINUE
          DO 121 I=1,NJCH
            CDUMY = CTYPE(I)
            IF (CDUMY.EQ.JTLNUP) IW(JTYPEW+I) = JTLNDN
            IF (CDUMY.EQ.JTLNDN) IW(JTYPEW+I) = JTLNUP
  121     CONTINUE
        END IF
C
C--->   Decide order of points in the first chain
C
        IORDR = 1
        IF (CTYPE(1).EQ.JTLNDN .OR. CTYPE(1).EQ.JTLNBM) IORDR= -1
        IF (NTB.EQ.0) IORDR= 1
C
C--->   Reset intermediates: ISTRT = start addresse in buffer
C                            IPICE = 1 -> NJCH
C                            JCHA1 = 1st chain in IBUFF
        ISTRT = 0
        IPICE = 0
        JCHA1 = IPLNK(1)
        IBMAP = 0
        NARC1 = 1
C
C-----> Loop over chains of the candidate and fill the TTCC list
C       of coordinates in order of direction of flight of the particle
C
  150   CONTINUE
          IPICE = IPICE + 1
          JCH = IPLNK(IPICE)
          IBMAP = IOR(IBMAP,ITABL(KTCHA,JCH,JTCHPI))
          NPT = ITABL(KTCHA,JCH,JTCHNC)
C Protect against mistakes when there are more than 21 pad rows
          IF( NPT+ISTRT .GT. LTPDRO ) THEN
            CALL RERROR('TFCAND',5,'Chain with >21 pad rows')
            NPT = 0
            GOTO 250
          ENDIF
C - end protection
          KSCHC = KKTCTC(JCH)
          DO 200 IPT =1,NPT
            IBUFF(ISTRT+IPT) = IW(KSCHC+IPT)
  200     CONTINUE
          IF (ISTRT.NE.0) THEN
C
C------->   Order buffer by ripple sort in case of up/down link
C
            LPT = NPT+ISTRT
            DO 240 I=1,LPT-1
              FSWAP=.FALSE.
              DO 230 J=1,LPT-1
                IF (IBUFF(J+1).LT.IBUFF(J)) THEN
                  KTAKE = IBUFF(J+1)
                  IBUFF(J+1) = IBUFF(J)
                  IBUFF(J)   = KTAKE
                  FSWAP=.TRUE.
                END IF
  230         CONTINUE
              IF (.NOT.FSWAP) GO TO 241
  240       CONTINUE
  241       CONTINUE
          END IF
C
C-------> End of buffer reordering
C
  250     CONTINUE
          IF (CTYPE(IPICE).EQ.JTLNUP.OR.CTYPE(IPICE).EQ.JTLNDN) THEN
            ISTRT = ISTRT+NPT
            GO TO 150
C                  |
C      <-----------'
          ENDIF
          LPT    = ISTRT + NPT
C
C--->     Fill the arc banks.  Use helix parameters from the
C--->     chain closest to the origin.  1st arc only goes in TCAL.
C
          IF (NARC1.GT.1) THEN
            IF (CTYPE(IPICE).EQ.JTLNTP) THEN
              JCHAN=JCHA1
            ELSE
              JCHAN=JCH
            ENDIF
            NARCS=NARCS+1
            KSARC=KROW(KTARC,NARCS)
            IW(KSARC+JTARTN)=NCAND
            IW(KSARC+JTAROF)=LTATC
            IW(KSARC+JTARNC)=LPT
            IW(KSARC+JTARAN)=NARC1*IORDR
            DO 317 IPT =1,LPT
              IW((KTATC+LMHLEN+LTATC)+IPT) = IBUFF(IPT)
  317       CONTINUE
            LTATC=LTATC+LPT
            IW(KSARC+JTARPI)=IBMAP
C
C------->   Convert from TASSO sign convention (used in TCHA) to
C           the more standard convention (as used in TGFT, FRFT)
C
            RW(KSARC+JTARIR)=-RTABL(KTCHA,JCHAN,JTCHIR)
            RW(KSARC+JTARTL)= RTABL(KTCHA,JCHAN,JTCHTL)
            RW(KSARC+JTARP0)= RTABL(KTCHA,JCHAN,JTCHP0)
            RW(KSARC+JTARD0)= SIGN(1.0,RW(KSARC+JTARIR))
     &                           * RTABL(KTCHA,JCHAN,JTCHD0)
            RW(KSARC+JTARZ0)= RTABL(KTCHA,JCHAN,JTCHZ0)
            RW(KSARC+JTARC1)= RTABL(KTCHA,JCHAN,JTCHC1)
            RW(KSARC+JTARC2)= RTABL(KTCHA,JCHAN,JTCHC2)
            LMOFF = 0
            DO 305 LM=1,5
              LMOFF = LMOFF + LM
              RW(KSARC+JTARER-1+LM)=RTABL(KTCHA,JCHAN,JTCHER-1+LMOFF)
  305       CONTINUE
          ENDIF
          NARC1=NARC1+1
C
          ISTRT = 0
          IF (IORDR.EQ.1) THEN
            DO 310 IPT =1,LPT
              IW(NPUT+IPT) = IBUFF(IPT)
  310       CONTINUE
          ELSE
            DO 320 IPT =1,LPT
              IW(NPUT+IPT) = IBUFF(1+LPT-IPT)
  320       CONTINUE
          END IF
          NPUT= NPUT + LPT
          NSTOR= NSTOR+ LPT
          IORDR=-IORDR
          JCHA1= IPLNK(IPICE+1)
          IBMAP= 0
        IF (IPICE.LT.NJCH)         GO TO 150
C                                        |
C      <---------------------------------'
C-----> Now fill 'TCAL' columns 2 and 3
C
        N1ARC = ITABL(KTCHA,IPLNK(1),JTCHNC)
        DO 330 I=1,NJCH
          IF (CTYPE(I).NE.JTLNUP .AND. CTYPE(I).NE.JTLNDN) GO TO 331
          N1ARC = N1ARC+ITABL(KTCHA,IPLNK(I+1),JTCHNC)
  330   CONTINUE
  331   CONTINUE
        IW(KSCAL+JTCANF)=N1ARC
        IW(KSCAL+JTCANR)=NSTOR-IW(KSCAL+JTCAIO)-N1ARC
  400 CONTINUE
C-------------------------------------------------------------------C
C              TREATMENT OF REMAINING CHAINS                        C
C              *****************************                        C
C-------------------------------------------------------------------C
      DO 500 ICHAN = 1,NCHAN
        KSLNK = KROW(KTLNK,ICHAN)
        IF (TAKEN(ICHAN))              GO TO 500
        NCAND = NCAND + 1
        NPT    = ITABL(KTCHA,ICHAN,JTCHNC)
        KSCHC = KKTCTC(ICHAN)
        KSCAL = KROW(KTCAL,NCAND)
        IW(KSCAL+JTCAIO) = NSTOR
        IW(KSCAL+JTCANF) = NPT
        IW(KSCAL+JTCANR) = 0
        DO 490 IPT =1,NPT
          IW(NPUT+IPT) = IW(KSCHC+IPT)
  490   CONTINUE
        NPUT   = NPUT + NPT
        NSTOR  = NSTOR+ NPT
  500 CONTINUE
C
C---> Define correct lengths of banks
C
      IW(KTTCC+LMHROW) = NSTOR
      IW(KTCAL+LMHROW) = NCAND
      IW(KTARC+LMHROW) = NARCS
      IW(KTATC+LMHROW) = LTATC
      CALL AUBPRS('TCALTARCTATC')
      GO TO 994
C
  995 CONTINUE
      CALL RERROR('TFCAND',4,'No room to book workbank')
      IER=4
      CALL BDROP(IW,'TCALTTCCTARCTATC')
      GO TO 994
  998 CONTINUE
C     input banks not found
      IER=2
      GO TO 994
  999 CONTINUE
      CALL RERROR('TFCAND',3,'Not enough space to create banks')
      IER=3
      CALL BDROP(IW,'TCALTTCCTARCTATC')
  994 CONTINUE
C
C---> Release the BOS space used by the workbank
C
      CALL WDROP(IW,ITCALW)
      RETURN
      END
#endif
