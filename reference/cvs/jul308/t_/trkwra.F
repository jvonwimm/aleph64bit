      SUBROUTINE TRKWRA(IER)
C
C-----------------------------------------------------------------------
C! Match wire hits to tracks
C!
C!    Author:   R. Johnson    7-02-89
C!    Modified: R. Johnson   12-01-91 make a more careful check of
C!                           which sectors the tracks pass through
C!                           24-08-92 make list of dead wires for TFTWTB
C!                           2-3-93 database control of constants
C!
C!    Output:  IER     /I      =1 if BOS is out of space
C!                             =0 if all is OK
C!    Input banks:
C!           TWRR              Reduced and packed wire pulses
C!           TGFT,TGFT,TGCL    TPC tracks
C!           TPCO              TPC pad coordinates
C!           TWPU              Optional expanded wire bank
C!    Output banks:
C!           TWTB,TWIT,TWAT    Created and filled
C!           TWPU              Pointer filled if present
C!
C!    Called by TPCREC
C!
C!    Description
C!    -----------
C!    For each track in TGFT, the program determines which sectors
C!    it crosses by looking at which sectors it has coordinates on.
C!    For each wire on those sectors it calculates the crossing
C!    point of the x-y projection of the track.  If this point is
C!    within the bounds of the wire and not too far from the
C!    track origin, in terms of turning angle, then the track parmameter
C!    are used to find the z position of the track at that point.  A
C!    search is made over all hits on the wire to find those within
C!    a z window of the track position.  If only ONE such pulse is
C!    found and if this pulses matches in this fashion only ONE
C!    track, then the pulse is associated with the wire by entering
C!    it into the TWTB bank.  Also included are some calibration
C!    corrections and a calculation of the length of the track
C!    element which contributes charge to the wire.
C!
C!----------------------------------------------------------------------
#ifndef DOC
C
#include "tclbjj.h"
#include "tparac.h"
#include "tpgpar.h"
#include "alcons.h"
#include "tpcojj.h"
#include "tgftjj.h"
#include "tarcjj.h"
#include "tgtljj.h"
#include "twtbjj.h"
#include "twitjj.h"
#include "tflags.h"
#include "bcs.h"
#include "tpgeom.h"
#include "tpgeop.h"
#include "tpgeow.h"
#include "tpccon.h"
#include "twirct.h"
#include "tkrwwc.h"
#include "twoljj.h"
C
      PARAMETER (LMXSEC=3,MXADJ=10)
      DIMENSION PGT(5),PS(5),SGNA(2),PG(5),IADJ(MXADJ)
      DIMENSION BV(2),XCS(2)
      LOGICAL FIRST,TSCINT
C
C     JSTRK = for each sector, the number of tracks and offset in
C             ITRKSW(3) of the first
      DIMENSION JSTRK(2,LTSECT),IXKAP(2,LTSECT)
      SAVE FIRST,SGNA,NTWIT,NTWAT,NTWTB,NTGFT,NTWRR,NTGTL,NTGCL
      SAVE NTPCO,NTARC,NTATC,NTWOL,NTCLB,NTKAP
C
      DATA FIRST/.TRUE./,SGNA/-1.,1./
C
C----------------------------------------------------------------------
C
#include "bmacro.h"
C
C----------------------------------------------------------------------
C
      IER=0
C
      IF (FIRST) THEN
        FIRST=.FALSE.
        ITWATW=0
        ITGFTW=0
        ITKWRW=0
        ITRKSW(1)=0
        ITRKSW(2)=0
        ITRKSW(3)=0
        ITKAPW=0
        NTWIT=NAMIND('TWIT')
        NTWAT=NAMIND('TWAT')
        NTWTB=NAMIND('TWTB')
        NTGFT=NAMIND('TGFT')
        NTWRR=NAMIND('TWRR')
        NTGTL=NAMIND('TGTL')
        NTGCL=NAMIND('TGCL')
        NTPCO=NAMIND('TPCO')
        NTARC=NAMIND('TARC')
        NTATC=NAMIND('TATC')
        NTWOL=NAMIND('TWOL')
        NTCLB=NAMIND('TCLB')
        NTKAP=NAMIND('TKAP')
      ENDIF
C
C++   Drop the output banks if already present
C
      IF (IW(NTWTB).NE.0) CALL BDROP(IW,'TWTB')
      IF (IW(NTWIT).NE.0) CALL BDROP(IW,'TWIT')
      IF (IW(NTWAT).NE.0) CALL BDROP(IW,'TWAT')
      IF (IW(NTWOL).NE.0) CALL BDROP(IW,'TWOL')
C
C++   Put all created banks on the temporary list
C
      CALL BLIST(IW,'T+','TWTBTWITTWATTWOL')
C
C++   Check whether the input banks are available
C
      KTGFT=IW(NTGFT)
      IF (KTGFT.EQ.0) GO TO 999
      KTWRR=IW(NTWRR)
      IF (KTWRR.EQ.0) GO TO 999
      KTGTL=IW(NTGTL)
      IF (KTGTL.EQ.0) GO TO 999
      KTGCL=IW(NTGCL)
      IF (KTGCL.EQ.0) GO TO 999
      KTPCO=IW(NTPCO)
      IF (KTPCO.EQ.0) GO TO 999
C
C Book TWOL which is simply a list of tracks with overlapping hits
C
      LEN = LMHLEN + LTWOLA*10
      CALL AUBOS('TWOL',0,LEN,KTWOL,IGARB)
      IF (IGARB.EQ.2) GOTO 997
      IW(KTWOL+LMHROW) = 0
      IW(KTWOL+LMHCOL) = LTWOLA
C
C++   Create the TWIT bank of pointers from TGFT to TWTB
C
      KTGFT=IW(NTGFT)
      LEN= LROWS(KTGFT)*LTWITA + LMHLEN
      IW(1)=1
      CALL AUBOS('TWIT',0,LEN,KTWIT,IGARB)
      IF (IGARB.EQ.2) GO TO 997
      KTGFT=IW(NTGFT)
      IW(KTWIT+LMHCOL)=LTWITA
      IW(KTWIT+LMHROW)=LROWS(KTGFT)
      CALL VZERO(IW(KTWIT+LMHLEN+1),LROWS(KTGFT)*LTWITA)
C
C++   Create a workbank for pointers into TWTB.  At the end, this
C++   will be copied to bank TWAT.
C
      NWR= NTWIRE(1)+NTWIRE(2)
      LEN= LROWS(KTGFT)*NWR+LMHLEN
      IW(1)=1
      CALL WBANK(IW,ITWATW,LEN,*998)
      IW(ITWATW-3)=INTCHA('TWAT')
      IW(ITWATW+LMHCOL)=NWR
      IW(ITWATW+LMHROW)=LROWS(KTGFT)
C
C++   Find the total number of wire hits
C
      MXLEN=0
      KTWRR=IW(NTWRR)
    3 IF (KTWRR.EQ.0) GO TO 4
        MXLEN=MXLEN+LROWS(KTWRR)
        KTWRR=IW(KTWRR-1)
        GO TO 3
    4 CONTINUE
C
C++   Bank for track wire association, TWTB.
C++   Make it initially only 1/2 the maximum possible size
C++   in the case of large events.  It gets extended in TFTWTB,
C++   so make sure it is always the last named bank booked, so
C++   that it is sitting before the gap and won't get copied when
C++   extended.
C
      NTK=MAX(2,LROWS(KTGFT))
      IF (NTK.GE.8) THEN
        MXLEN=MIN(MXLEN,(NTK*(NTWIRE(1)+NTWIRE(2)))/2)
      ELSE
        MXLEN=MIN(MXLEN,NTK*(NTWIRE(1)+NTWIRE(2)))
      ENDIF
      LEN= LTWTBA*MXLEN + LMHLEN
      IW(1)=1
      CALL AUBOS('TWTB',0,LEN,KTWTB,IGARB)
      IF (IGARB.EQ.2) GO TO 997
      IW(KTWTB+LMHCOL)=LTWTBA
      IW(KTWTB+LMHROW)=0
C
C++   Create and fill work banks for track-sector cross reference
C
      KTARC=IW(NTARC)
      IF (KTARC.EQ.0) THEN
        NCHAI=0
      ELSE
        NCHAI=LROWS(KTARC)
        KTATC=IW(NTATC)
        IF (KTATC.EQ.0) GO TO 999
      ENDIF
      LEN= (NCHAI+LROWS(KTGFT))*LMXSEC
      DO 71 I=1,3
        IW(1)=1
        CALL WBANK(IW,ITRKSW(I),LEN,*998)
        IW(ITRKSW(I)-3)=INTCHA('WKSW')
   71 CONTINUE
C
C++   First find all sectors crossed by each fitted track in TGFT bank.
C
      NUMB=0
      DO 273 ITK=1,LROWS(KTGFT)
C
C++     If the track has a coordinate in the sector, then it clearly
C++     passes through the sector.
C
        NBEG=NUMB+1
        DO 275 I=1,ITABL(KTGTL,ITK,JTGTN1)
          IC= IW(KTGCL+LMHLEN+ITABL(KTGTL,ITK,JTGTIO)+I)
          ISLOT= MOD(ITABL(KTPCO,IC,JTPCIN),100000)/1000
          DO 278 J=NBEG,NUMB
            IOLD=IW(ITRKSW(2)+J)
            IF (ISLOT.EQ.IOLD) GO TO 275
  278     CONTINUE
          NUMB=NUMB+1
          IF (NUMB.GT.IW(ITRKSW(1))) THEN
            CALL RERROR('TRKWRA',4,
     &              'Extension of ITRKSW workbank was required')
            LEN=IW(ITRKSW(1))+50
            DO 72 II=1,3
              IW(1)=1
              CALL WBANK(IW,ITRKSW(II),LEN,*998)
   72       CONTINUE
          ENDIF
          IW(ITRKSW(1)+NUMB)=ITK
          IW(ITRKSW(2)+NUMB)=ISLOT
  275   CONTINUE
C
C++     Check also whether the track passes through any of the
C++     nearby sectors in which it has no coordinates.
C
        NIN=NUMB-NBEG+1
        CALL TSNEXT(NIN,IW(ITRKSW(2)+NBEG),MXADJ,NADJ,IADJ)
        DO 4283 I=1,NADJ
          IF (TSCINT(RW(KROW(KTGFT,ITK)+JTGFIR),IADJ(I))) THEN
            NUMB=NUMB+1
            IF (NUMB.GT.IW(ITRKSW(1))) THEN
              CALL RERROR('TRKWRA',4,
     &              'Extension of ITRKSW workbank was required')
              LEN=IW(ITRKSW(1))+50
              DO 372 II=1,3
                IW(1)=1
                CALL WBANK(IW,ITRKSW(II),LEN,*998)
  372         CONTINUE
            ENDIF
            IW(ITRKSW(1)+NUMB)=ITK
            IW(ITRKSW(2)+NUMB)=IADJ(I)
          ENDIF
 4283   CONTINUE
  273 CONTINUE
C
C++   Now find all sectors crossed by each arc in the TARC bank
C
      DO 297 ICH=1,NCHAI
        NBEG=NUMB+1
        DO 293 I=1,ITABL(KTARC,ICH,JTARNC)
          IC=IW(KTATC+LMHLEN+ITABL(KTARC,ICH,JTAROF)+I)
          ISLOT= MOD(ITABL(KTPCO,IC,JTPCIN),100000)/1000
          DO 298 J=NBEG,NUMB
            IOLD=IW(ITRKSW(2)+J)
            IF (ISLOT.EQ.IOLD) GO TO 293
  298     CONTINUE
          NUMB=NUMB+1
          IF (NUMB.GT.IW(ITRKSW(1))) THEN
            CALL RERROR('TRKWRA',4,
     &              'Extension of ITRKSW workbank was required')
            LEN=IW(ITRKSW(1))+50
            DO 73 II=1,3
              IW(1)=1
              CALL WBANK(IW,ITRKSW(II),LEN,*998)
   73       CONTINUE
          ENDIF
          IW(ITRKSW(1)+NUMB)=-ICH
          IW(ITRKSW(2)+NUMB)=ISLOT
  293   CONTINUE
C
C++     Check also whether the track passes through any of the
C++     nearby sectors in which it has no coordinates.
C
        NIN=NUMB-NBEG+1
        CALL TSNEXT(NIN,IW(ITRKSW(2)+NBEG),MXADJ,NADJ,IADJ)
        DO 5283 I=1,NADJ
          IF (TSCINT(RW(KROW(KTARC,ICH)+JTARIR),IADJ(I))) THEN
            NUMB=NUMB+1
            IF (NUMB.GT.IW(ITRKSW(1))) THEN
              CALL RERROR('TRKWRA',4,
     &              'Extension of ITRKSW workbank was required')
              LEN=IW(ITRKSW(1))+50
              DO 572 II=1,3
                IW(1)=1
                CALL WBANK(IW,ITRKSW(II),LEN,*998)
  572         CONTINUE
            ENDIF
            IW(ITRKSW(1)+NUMB)=-ICH
            IW(ITRKSW(2)+NUMB)=IADJ(I)
          ENDIF
 5283   CONTINUE
  297 CONTINUE
C
C++   Now sort the tracks and arcs by sector number
C
      CALL SORTZV(IW(ITRKSW(2)+1),IW(ITRKSW(3)+1),NUMB,-1,0,0)
C
C++   Find the number of tracks in each sector and offset to each
C
      CALL VZERO(JSTRK,2*LTSECT)
      ISLST=-1
      DO 751 IND=1,NUMB
        ISORT=IW(ITRKSW(3)+IND)
        ISLOT=IW(ITRKSW(2)+ISORT)
        IF (ISLOT.NE.ISLST) THEN
          ISLST=ISLOT
          JSTRK(1,ISLOT)=0
          JSTRK(2,ISLOT)=IND-1
        ENDIF
        JSTRK(1,ISLOT)=JSTRK(1,ISLOT)+1
  751 CONTINUE
C
C++   Find the maximum number of tracks within one sector
C
      MXTRK=0
      DO 763 ISLOT=1,LTSECT
        IF (JSTRK(1,ISLOT).GT.MXTRK) MXTRK=JSTRK(1,ISLOT)
  763 CONTINUE
C
C++   Book workbank for storage of track info within a sector
C
      LEN= LTGFTW*MXTRK+LMHLEN
      IW(1)=1
      CALL WBANK(IW,ITGFTW,LEN,*998)
      IW(ITGFTW+LMHCOL)=LTGFTW
      IW(ITGFTW-3)=INTCHA('TGFW')
C
C++   Book workbank for storage of track-wire intersection points
C++   Make twice as long as the maximum number of tracks, since a track
C++   circle can intersect a wire up to two times.
C
      LEN=2*LTKWRW*MXTRK+LMHLEN
      IW(1)=1
      CALL WBANK(IW,ITKWRW,LEN,*998)
      IW(ITKWRW+LMHCOL)=LTKWRW
      IW(ITKWRW-3)=INTCHA('TKWR')
C
C++   Make list of offsets into TKAP
C
      KTCLB=IW(NTCLB)
      KTKAP=IW(NTKAP)
      IF (KTCLB.NE.0.AND.KTKAP.NE.0) THEN
        NDEAD=0
        DO 7837 ISLOT=1,LROWS(KTCLB)
          IXKAP(1,ISLOT)=NDEAD
          IXKAP(2,ISLOT)=ITABL(KTCLB,ISLOT,JTCLNT)
          NDEAD=NDEAD+IXKAP(2,ISLOT)
 7837   CONTINUE
      ENDIF
C
C++   Create a workbank for storing dead wire info for 1 sector
C
      IW(1)=1
      CALL WBANK(IW,ITKAPW,LMHLEN+LTWIRE,*998)
      IW(ITKAPW+LMHCOL)=1
C
C++   Loop over all sectors
C
      DO 10 ISLOT=1,NTSECT
        IF (JSTRK(1,ISLOT).EQ.0) GO TO 10
C
C++     Make a list of dead wires for this sector
C
        IW(ITKAPW+LMHROW)=0
        IF (KTCLB.NE.0.AND.KTKAP.NE.0) THEN
          IOFF=IXKAP(1,ISLOT)
          NDEAD=IXKAP(2,ISLOT)
          DO 6502 I=1,NDEAD
            IWRD=IW(KTKAP+LMHLEN+IOFF+I)
            IROW=IBITS(IWRD,8,8)
            IF (IROW.NE.0) GO TO 6502
            ITHR=IBITS(IWRD,16,8)
            IF (ITHR.NE.255) GO TO 6502
            IWIR=IBITS(IWRD,0,8)
            JSLOT=IBITS(IWRD,24,8)
            IF (JSLOT.NE.ISLOT) THEN
              CALL RERROR('TRKWRA',3,
     &           'Pointers from TCLB to TKAP are screwed up!')
            ELSE
              IW(ITKAPW+LMHROW)=LROWS(ITKAPW)+1
              IW(ITKAPW+LMHLEN+LROWS(ITKAPW))=IWIR
            ENDIF
 6502     CONTINUE
        ENDIF
C
C++     Store information on all tracks within the sector
C
        IW(ITGFTW+LMHROW)=0
        IOFF=JSTRK(2,ISLOT)
        DO 693 II=1,JSTRK(1,ISLOT)
          ISORT=IW(ITRKSW(3)+IOFF+II)
C
C++       Get the track number and helix parameters
C
          ITK=IW(ITRKSW(1)+ISORT)
          IF (ITK.LT.0) THEN
            PG(1)= RTABL(KTARC,-ITK,JTARIR)
            PG(2)= RTABL(KTARC,-ITK,JTARTL)
            PG(3)= RTABL(KTARC,-ITK,JTARP0)
            PG(4)= RTABL(KTARC,-ITK,JTARD0)
            PG(5)= RTABL(KTARC,-ITK,JTARZ0)
            IR1=0
            IR2=0
          ELSE
            PG(1)=RTABL(KTGFT,ITK,JTGFIR)
            PG(2)=RTABL(KTGFT,ITK,JTGFTL)
            PG(3)=RTABL(KTGFT,ITK,JTGFP0)
            PG(4)=RTABL(KTGFT,ITK,JTGFD0)
            PG(5)=RTABL(KTGFT,ITK,JTGFZ0)
C
C++         Find first and last rows of the track.
C
            IC1=IW(KTGCL+LMHLEN+ITABL(KTGTL,ITK,JTGTIO)+1)
            IR1=ITABL(KTPCO,IC1,JTPCIN)/100000
            IC2=IW(KTGCL+LMHLEN+ITABL(KTGTL,ITK,JTGTIO)
     &                         + ITABL(KTGTL,ITK,JTGTN1))
            IR2=ITABL(KTPCO,IC2,JTPCIN)/100000
          ENDIF
C
C++       Transform helix parameters to the sector frame
C
          IF (FTPC90) THEN
            DO 567 IH=1,5
              PS(IH)=PG(IH)
  567       CONTINUE
          ELSE
C
C++         First tranform from ALEPH to TPC frame
C
            CALL TGHPAL(PG,PGT)
C
C++         Then go from TPC frame to sector frame
C
            CALL TGHPTS(ISLOT,PGT,PS)
          ENDIF
          R=1./PS(1)
          RAD=ABS(R)
          COSL=1./SQRT(1.+PS(2)**2)
C
C++       Find the center of the track circle
C
          SGN=SIGN(1.,PS(1))
          SPHI0=SIN(PS(3))
          CPHI0=COS(PS(3))
          XCS(1)=-(R-PS(4))*SPHI0
          XCS(2)= (R-PS(4))*CPHI0
C
C++       Store the track information in the ITGFTW workbank
C
          IW(ITGFTW+LMHROW)=LROWS(ITGFTW)+1
          IST=LROWS(ITGFTW)
          RW(KROW(ITGFTW,IST)+JTGWIR)=PS(1)
          RW(KROW(ITGFTW,IST)+JTGWTL)=PS(2)
          RW(KROW(ITGFTW,IST)+JTGWCL)=COSL
          RW(KROW(ITGFTW,IST)+JTGWP0)=PS(3)
          RW(KROW(ITGFTW,IST)+JTGWD0)=PS(4)
          RW(KROW(ITGFTW,IST)+JTGWZ0)=PS(5)
          RW(KROW(ITGFTW,IST)+JTGWXC)=XCS(1)
          RW(KROW(ITGFTW,IST)+JTGWYC)=XCS(2)
          IW(KROW(ITGFTW,IST)+JTGWFR)=IR1
          IW(KROW(ITGFTW,IST)+JTGWLR)=IR2
          IW(KROW(ITGFTW,IST)+JTGWTK)=ITK
  693   CONTINUE
C
C++     Associate wire hits with the tracks for this sector
C
        CALL TFTWTB(ISLOT,JGARB)
        IF (JGARB.EQ.2) GO TO 997
        IF (JGARB.EQ.1) THEN
          KTGFT=IW(NTGFT)
          KTARC=IW(NTARC)
          KTGCL=IW(NTGCL)
          KTPCO=IW(NTPCO)
          KTGTL=IW(NTGTL)
        ENDIF
   10 CONTINUE
C
C++   Drop work banks here to make more room for TWAT bank
C
      CALL WDROP(IW,ITKAPW)
      CALL WDROP(IW,ITGFTW)
      CALL WDROP(IW,ITRKSW(3))
      CALL WDROP(IW,ITRKSW(2))
      CALL WDROP(IW,ITRKSW(1))
C
C++   Copy pointers from ITWATW to TWAT
C
      KTWIT=IW(NTWIT)
      NAS=0
      DO 749 ITK=1,LROWS(KTWIT)
        NAS=NAS+ITABL(KTWIT,ITK,JTWINB)+ITABL(KTWIT,ITK,JTWING)
  749 CONTINUE
      LEN=NAS+LMHLEN
      CALL AUBOS('TWAT',0,LEN,KTWAT,IGARB)
      IF (IGARB.EQ.2) GO TO 997
      IW(KTWAT+LMHCOL)=1
      IW(KTWAT+LMHROW)=NAS
C
      KTWIT=IW(NTWIT)
      KTWTB=IW(NTWTB)
      IPT=0
      DO 851 ITK=1,LROWS(KTWIT)
        NBAD=ITABL(KTWIT,ITK,JTWINB)
        NGOOD=ITABL(KTWIT,ITK,JTWING)
        IW(KROW(KTWIT,ITK)+JTWIOS)=IPT
C
C++     Delete strings of zeroes, and all zeroes at beginning and
C++     end of the track.
C
        NGS=NGOOD
        NRUN=0
        DO 801 I=1,NGS
          IPT=IPT+1
          IWB=ITABL(ITWATW,ITK,I)
          CHRG=RTABL(KTWTB,IWB,JTWTCE)
          IF (CHRG.EQ.0.) THEN
            NRUN=NRUN+1
            IF (I.EQ.NGS) THEN
              IPT=IPT-NRUN
              NGOOD=NGOOD-NRUN
              GO TO 802
            ENDIF
          ELSE
            IF (NRUN.GT.MXRNTW.OR.NRUN.EQ.I-1) THEN
              IPT=IPT-NRUN
              NGOOD=NGOOD-NRUN
            ENDIF
            NRUN=0
          ENDIF
          IW((KTWAT+LMHLEN)+IPT)=IWB
  801   CONTINUE
  802   CONTINUE
        IW(KROW(KTWIT,ITK)+JTWING)=NGOOD
        DO 821 I=1,NBAD
          IPT=IPT+1
          IW((KTWAT+LMHLEN)+IPT)=IW(KROW(ITWATW,ITK+1)+1-I)
  821   CONTINUE
  851 CONTINUE
C
C++   Set the length of the TWTB bank and drop the workbank
C
      CALL AUBPRS('TWTBTWOL')
      CALL WDROP(IW,ITWATW)
      RETURN
C
  997 CONTINUE
        CALL RERROR('TRKWRA',1,'No space available for named bank')
        IER=1
        GO TO 999
  998 CONTINUE
        CALL RERROR('TRKWRA',2,'No space available for work bank')
        IER=2
  999 CONTINUE
C
C++   Drop all the banks in case of error
C
      CALL BDROP(IW,'TWTBTWITTWAT')
      CALL WDROP(IW,ITGFTW)
      CALL WDROP(IW,ITRKSW(3))
      CALL WDROP(IW,ITRKSW(2))
      CALL WDROP(IW,ITRKSW(1))
      CALL WDROP(IW,ITWATW)
      CALL WDROP(IW,ITKAPW)
C
      RETURN
      END
#endif
