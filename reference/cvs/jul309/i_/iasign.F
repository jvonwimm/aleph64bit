      SUBROUTINE IASIGN(SIGMAX, ADJCUT, ITK, VV0, IER)
C-----------------------------------------------------------------------
C! Fill /ITCINF/ common with information to analyse the ITC
C!
C!    Author  :  W. B. Atwood  2-Oct-89
C!    Modified:  J.Sedgbeer 16/01/90 Rules etc.
C!    Modified:  J.Carr  12/2/91 various changes
C!
C!   Input:
C!     SIGMAX /R4 : Max. allowed No. of sigmas for inclusions of hits
C!                                                 on tracks
C!     ADJCUT /R4 : Fraction of cell beyond which to include next cell
C!     ITK    /I4 : number of track in bank (ITFT)
C!     VV0(5) /R  : track params as 1st 5 in ITFT.
C!     need common /BCS/ for BOS banks.
C!     need common /ITWICC/ for ITC geom.
C!
C! IMPORTANT: If you wish to use this subroutine, then at the beginning
C!            of each run, you must call the ALEPHLIB routine, IRDDAF,
C!            to fill commons with the ITC geometry etc.
C!
C!            ALSO: YOU MUST ZERO NTKITC from COMMON/ITCINF/ before
C!                  calling IASIGN!!!!
C!   Output:
C!     IER    /I  : Error flag: = 0 if O.K., =1 otherwise
C!     see common/ITCINF/
C!
C!   calls     : ITCXNG, IUDOCA, UTSWCO, ITROTN
C!
C!   Libraries required: BOS
C!
C?
C!======================================================================
#ifndef DOC
#include "bcs.h"
#include "itcojj.h"
#include "itwicc.h"
#include "alcons.h"
C-----------------------------------------------------------------------
#include "itcinf.h"
C-----------------------------------------------------------------------
      EXTERNAL NAMIND,NLINK
      REAL  VV0(*), DDS(2), DFN(8)
      LOGICAL  FIRST
      DATA FIRST/.TRUE./
C-----------------------------------------------------------------------
#include "bmacro.h"
C-----------------------------------------------------------------------
C Establishes bank pointers once
      IF(FIRST) THEN
         NIWCR = NAMIND('IWCR')
         NILIV = NAMIND('ILIV')
         FIRST = .FALSE.
      ENDIF
      IER = 1
C
C Set up links
C
      KITCO = NLINK('ITCO',0)
      IF (KITCO.LE.0) GOTO 997
C
      KIWCR = IW(NIWCR)
      IF (KIWCR.LE.0) GOTO 997
C
      KILIV = IW(NILIV)
C
C Register track and bump pointers
      IF(NTKITC.GE.JRESMX) GOTO 998
C
      IER = 0
      NTKITC = NTKITC + 1
      ITKITC(NTKITC) = ITK
      NHTITC(NTKITC) = -1
C
C Get the track crossings in the ITC
      CALL ITCXNG(VV0,CLWITC(1,NTKITC),HTXITC(1,1,NTKITC),
     *                                 HTDITC(1,1,NTKITC),DFN)
C
C Get tracking errors at average r=20
      CALL ITERR(20.,ITK,RPHERR,IFLAG)
      IF(IFLAG.EQ.0) THEN
C    limit to 0.025 cms
         TERITC(NTKITC)=AMIN1(RPHERR,0.025)
      ELSE
         TERITC(NTKITC)=0.025
      ENDIF
C Initialize Xi**2 and hit count
      RSQITC(NTKITC) = 0.
      NHTITC(NTKITC) = 0
      NGHITC(NTKITC) = 0.
      NDHITC(NTKITC)= 0.
      NRHITC(NTKITC)= 0
C
C Loop over the ITC Layers and find the hits
      DO 50 LAY=1,8
C
C Check if track crossed this ITC Layer
        IF(CLWITC(LAY,NTKITC).LE.0) THEN
           FLGITC(LAY,NTKITC) = 5
           ICOITC(LAY,NTKITC) = 0
           GOTO 50
        ENDIF
C
        IWIRE = CLWITC(LAY,NTKITC)
        GBWITC(LAY,NTKITC) = 0
        ICOITC(LAY,NTKITC) = 0
        ZSW   = HTXITC(3,LAY,NTKITC)
C
C Check if wire is present in the event
        IC = IW(KIWCR+LMHLEN+IWIRE)
        IF(IC.GT.0) THEN
           GBWITC(LAY,NTKITC) = IWIRE
           FLGITC(LAY,NTKITC) = 0
           ICOITC(LAY,NTKITC) = IC
           NHTITC(NTKITC) = NHTITC(NTKITC) + 1
        ELSE
C
C Call the wire Inefficient.. (default)
           GBWITC(LAY,NTKITC) = 0
           FLGITC(LAY,NTKITC) = 3
C
C See if this wire is alive.
           IF(KILIV.GT.0) THEN
              IFGWIR = IW(KILIV+LMHLEN+IWIRE)
           ELSE
              IFGWIR = 0
           ENDIF
C
C
           IF(IFGWIR.EQ.1 .OR. IFGWIR.EQ.4) THEN
              FLGITC(LAY,NTKITC) = 4
              NDHITC(NTKITC)=NDHITC(NTKITC)+1
C
C Find out if there is a hit in an adjacent cell
           ELSEIF(ABS(DFN(LAY)).GT.ADJCUT) THEN
              IF(DFN(LAY).LT.0.) THEN
                 IWIRP = IWIRE - 1
                 SGN = 1.
                 IF(IWIRP.LT.IWIRIT(LAY)+1)
     *              IWIRP = IWIRIT(LAY)+NWIRIT(LAY)
              ELSE
                 IWIRP = IWIRE + 1
                 SGN = -1.
                 IF(IWIRP.GT.IWIRIT(LAY)+NWIRIT(LAY))
     *              IWIRP = IWIRIT(LAY)+1
              ENDIF
              IC = IW(KIWCR+LMHLEN+IWIRP)
              IF(IC.GT.0) THEN
                 FLGITC(LAY,NTKITC) = -1
                 IWIRE = IWIRP
                 GBWITC(LAY,NTKITC) = IWIRE
                 ICOITC(LAY,NTKITC) = IC
                 NHTITC(NTKITC) = NHTITC(NTKITC) + 1
              ENDIF
           ENDIF
        ENDIF
C
C  Compute the co-ordinates of the wire..
        CALL UTSWCO(LAY,IWIRE,ZSW,RSW,FSW)
        CALL ITROTN(RSW,FSW,ZSW)
        FSW = AMOD(FSW + TWOPI, TWOPI)
C
C
C  Get the drift distance and sign it
        IF(FLGITC(LAY,NTKITC).LE.2) THEN
           DTMITC(LAY,NTKITC) = RTABL(KITCO,IC,JITCDT)
           ZOLD               = RTABL(KITCO,IC,JITCZH)
C
           CALL ICDRIF(LAY,IWIRE,DTMITC(LAY,NTKITC),RSW,ZOLD,ZSW,
     *                 DDS(1),DDS(2),DERITC(LAY,NTKITC))
C
C
C  No wire was found, so set defaults...
         ELSE
           DTMITC(LAY,NTKITC) = -1.
           DERITC(LAY,NTKITC) =  1.
           DDSITC(LAY,NTKITC) =  0.
         ENDIF
C
C  Output wire in /ITCINF/ arrays
         FIWITC(LAY,NTKITC) = FSW
         SINFI = SIN(FSW)
         COSFI = COS(FSW)
         WRXITC(1,LAY,NTKITC) = RSW*COSFI
         WRXITC(2,LAY,NTKITC) = RSW*SINFI
         WRXITC(3,LAY,NTKITC) = ZSW
         WRDITC(1,LAY,NTKITC) = 0.
         WRDITC(2,LAY,NTKITC) = 0.
         WRDITC(3,LAY,NTKITC) =-1.
C
C
C Now calculate DOCA and Residuals....
C
C
        CALL IUDOCA(WRXITC(1,LAY,NTKITC),WRDITC(1,LAY,NTKITC),
     *              HTXITC(1,LAY,NTKITC),HTDITC(1,LAY,NTKITC),
     *              DCAITC(LAY,NTKITC),DIST1,DIST2)
C
        IF(FLGITC(LAY,NTKITC).LE. 2) THEN
           IF(FLGITC(LAY,NTKITC).EQ.0) THEN
             IF(ABS(DDS(1)-DCAITC(LAY,NTKITC)) .LT.
     *          ABS(DDS(2)-DCAITC(LAY,NTKITC))) THEN
                DDSITC(LAY,NTKITC) = DDS(1)
             ELSE
                DDSITC(LAY,NTKITC) = DDS(2)
             ENDIF
           ELSE
             IF(SGN.GT.0.) THEN
                DDSITC(LAY,NTKITC) = DDS(1)
             ELSE
                DDSITC(LAY,NTKITC) = DDS(2)
             ENDIF
           ENDIF
C
           RSDITC(LAY,NTKITC)=ABS(DDSITC(LAY,NTKITC)) -
     *                        ABS(DCAITC(LAY,NTKITC))
           RSFITC(LAY,NTKITC)=DDSITC(LAY,NTKITC)-DCAITC(LAY,NTKITC)
C
C
C  Determine whether or not to include hit in next fit...
           IF(FLGITC(LAY,NTKITC) .LE. 0) THEN
              TERSUM=TERITC(NTKITC)+DERITC(LAY,NTKITC)
              DEVSG = RSFITC(LAY,NTKITC)/TERSUM
              IF(ABS(DEVSG) .GT. SIGMAX) THEN
                 FLGITC(LAY,NTKITC) = 1
              ELSE
                 RSQITC(NTKITC) = RSQITC(NTKITC)+ DEVSG**2
                 NGHITC(NTKITC) = NGHITC(NTKITC) + 1
                 ICOITC(LAY,NTKITC) = IC
              ENDIF
           ENDIF
        ELSE
           RSDITC(LAY,NTKITC) = 0.
           RSFITC(LAY,NTKITC) = 0.
        ENDIF
C
        DO 40 I=1,3
          HTXITC(I,LAY,NTKITC) = HTXITC(I,LAY,NTKITC) +
     *                           DIST2*HTDITC(I,LAY,NTKITC)
   40   CONTINUE
C
C
   50 CONTINUE
C
C  Normalize Xi**2 to number of hits
      RSQITC(NTKITC) = RSQITC(NTKITC) /
     ,                        AMAX1(FLOAT(NGHITC(NTKITC)),1.)
C
C Calculate ITC alone Xi**2
      RMEAN=0.
      DO 60 LAY=1,8
         IF(FLGITC(LAY,NTKITC).LE.0) RMEAN=RMEAN+RSFITC(LAY,NTKITC)
   60 CONTINUE
      RMEAN = RMEAN / AMAX1(FLOAT(NGHITC(NTKITC)),1.)
      RMNITC(NTKITC)=RMEAN
      RMAX=0.
      XISITC(NTKITC)=0.
      DO 70 LAY=1,8
        IF(FLGITC(LAY,NTKITC).LE.0) THEN
           RSC= (RSFITC(LAY,NTKITC)-RMEAN)/DERITC(LAY,NTKITC)
           XISITC(NTKITC)=XISITC(NTKITC)+RSC**2
           IF(ABS(RSC).GT.ABS(RMAX)) THEN
               RMAX=RSC
               LRXITC(NTKITC)=LAY
           ENDIF
        ENDIF
   70 CONTINUE
      XISITC(NTKITC)=XISITC(NTKITC) /
     ,                          AMAX1(FLOAT(NGHITC(NTKITC)-1),1.)
C
      GOTO 999
C-----------------------------------------------------------------------
C Errors
  997 CONTINUE
      CALL RERROR('IASIGN',1,' No ITCO/IWCR Banks!!!!')
      GOTO 999
C
  998 CONTINUE
      CALL RERROR('IASIGN',2,' Common/ITCINF/ Overflow')
C-----------------------------------------------------------------------
  999 CONTINUE
      END
#endif
