      SUBROUTINE TUN1NC(IC,ITK,ITPCO,RTPCO,IER)
C
C---------------------------------------------------------------------
C! Unpack single coordinate from PTNC
C!
C!    Author:  R. Johnson    17-06-90
C!
C!    Input:  IC       /I         Coordinate number to unpack
C!            ITK      /I         Track number in FRFT
C!    Output: ITPCO(6) /I         Integer attributes of bank TPCO
C!            RTPCO(6) /R         Real attributes of bank TPCO
C!            IER      /I         Error return= nonzero if coordinate
C!                                cannot be found.
C!     Called by PTPCOJ
C!
C----------------------------------------------------------------------
      SAVE
C
C! define universal constants
      REAL PI, TWOPI, PIBY2, PIBY3, PIBY4, PIBY6, PIBY8, PIBY12
      REAL RADEG, DEGRA
      REAL CLGHT, ALDEDX
      PARAMETER (PI=3.141592653589)
      PARAMETER (RADEG=180./PI, DEGRA=PI/180.)
      PARAMETER (TWOPI = 2.*PI , PIBY2 = PI/2., PIBY4 = PI/4.)
      PARAMETER (PIBY6 = PI/6. , PIBY8 = PI/8.)
      PARAMETER (PIBY12= PI/12., PIBY3 = PI/3.)
      PARAMETER (CLGHT = 29.9792458, ALDEDX = 0.000307)
      INTEGER LMHLEN, LMHCOL, LMHROW
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2)
C
      COMMON /BCS/   IW(1000)
      INTEGER IW
      REAL RW(1000)
      EQUIVALENCE (RW(1),IW(1))
C
      PARAMETER(JPTNSL=1,JPTNSR=2,JPTNRP=3,JPTNZV=4,JPTNSP=5,JPTNSZ=6,
     +          LPTNCA=6)
      PARAMETER(JTPCIN=1,JTPCRV=2,JTPCPH=3,JTPCZV=4,JTPCSR=5,JTPCSZ=6,
     +          JTPCOF=7,JTPCTN=8,JTPCCN=9,JTPCIT=10,JTPCRR=11,
     +          JTPCRZ=12,LTPCOA=12)
      PARAMETER (LTPDRO=21,LTTROW=19,LTSROW=12,LTWIRE=200,LTSTYP=3,
     +           LTSLOT=12,LTCORN=6,LTSECT=LTSLOT*LTSTYP,LTTPAD=4,
     +           LMXPDR=150,LTTSRW=11)
C
      COMMON /TPGEOP/ NTPDRW(LTSTYP),NTPDPR(LTSROW,LTSTYP),
     &                TPDRBG(LTSTYP),TPDRST(LTSTYP),TPDHGT(LTSTYP),
     &                TPDSEP(LTSTYP),TPDWID(LTSTYP),TPDHWD(LTSTYP),
     &                TPDPHF(LTSROW,LTSTYP),TPDPHW(LTSROW,LTSTYP),
     &                TPDPHS(LTSROW,LTSTYP)
C
      COMMON /TPGEOM/RTPCMN,RTPCMX,ZTPCMX,DRTPMN,DRTPMX,DZTPMX,
     &               TPFRDZ,TPFRDW,TPAVDZ,TPFOF1,TPFOF2,TPFOF3,
     &               TPPROW(LTPDRO),TPTROW(LTTROW),NTSECT,NTPROW,
     &               NTPCRN(LTSTYP),TPCORN(2,LTCORN,LTSTYP),
     &               TPPHI0(LTSECT),TPCPH0(LTSECT),TPSPH0(LTSECT),
     &               ITPTYP(LTSECT),ITPSEC(LTSECT),IENDTP(LTSECT)
C
      PARAMETER(JPTUID=1,JPTUVR=2,JPTURS=4,JPTUPS=5,JPTUZS=6,JPTUSR=7,
     +          JPTUSZ=8,JPTUPB=9,JPTUZB=10,JPTUTM=11,JPTUTL=12,
     +          JPTUAD=13,JPTURP=14,LPTUNA=14)
C
      DIMENSION ITPCO(*),RTPCO(*)
      LOGICAL FIRST
      DATA FIRST/.TRUE./,MODE/1/
C
C!    set of intrinsic functions to handle BOS banks
C - # of words/row in bank with index ID
      LCOLS(ID) = IW(ID+1)
C - # of rows in bank with index ID
      LROWS(ID) = IW(ID+2)
C - index of next row in the bank with index ID
      KNEXT(ID) = ID + LMHLEN + IW(ID+1)*IW(ID+2)
C - index of row # NRBOS in the bank with index ID
      KROW(ID,NRBOS) = ID + LMHLEN + IW(ID+1)*(NRBOS-1)
C - # of free words in the bank with index ID
      LFRWRD(ID) = ID + IW(ID) - KNEXT(ID)
C - # of free rows in the bank with index ID
      LFRROW(ID) = LFRWRD(ID) / LCOLS(ID)
C - Lth integer element of the NRBOSth row of the bank with index ID
      ITABL(ID,NRBOS,L) = IW(ID+LMHLEN+(NRBOS-1)*IW(ID+1)+L)
C - Lth real element of the NRBOSth row of the bank with index ID
      RTABL(ID,NRBOS,L) = RW(ID+LMHLEN+(NRBOS-1)*IW(ID+1)+L)
C
C
      IF (FIRST) THEN
        FIRST=.FALSE.
        NPTNC=NAMIND('PTNC')
        NPTUN=NAMIND('PTUN')
      ENDIF
      KPTNC=IW(NPTNC)
      IF (KPTNC.EQ.0) THEN
        IER=1
        GO TO 999
      ENDIF
      KPTUN=IW(NPTUN)
      IF (KPTUN.EQ.0) THEN
        IER=2
        GO TO 999
      ENDIF
      KPTUN=KPTUN+LMHLEN
      IF (IC.LT.1 .OR. IC.GT.LROWS(KPTNC)) THEN
        IER=3
        GO TO 999
      ENDIF
      IROW=ITABL(KPTNC,IC,JPTNSR)
      ISLOT=ITABL(KPTNC,IC,JPTNSL)
      ISTYP=ITPTYP(ISLOT)
      IF (ISTYP.NE.1) THEN
        IROWG=IROW+NTPDRW(1)
      ELSE
        IROWG=IROW
      ENDIF
C
C++   The new procedure is to store raw sector coordinates on the
C++   POT, from which TPCO can be derived by applying all alignment
C++   and field corrections.
C
      RPHIS= FLOAT(ITABL(KPTNC,IC,JPTNRP))*RW(KPTUN+JPTURP)
      ZS=    FLOAT(ITABL(KPTNC,IC,JPTNZV))*RW(KPTUN+JPTUZS)
      RS=    TPPROW(IROWG)
C
C++   Transform the z coordinate according changes in drift velocity
C++   and t0
C
      CALL TCRZVD(IENDTP(ISLOT),ZS,ZCR)
      ZS = ZCR
C
C++   Transform the coordinate to the ALEPH frame of reference,
C++   including all necessary alignment corrections.
C
      CALL TCTGLB(ISLOT,RS,RPHIS,ZS,R,RPHI,Z)
C
C++   Correct coordinate z for time-of-flight
C
      CALL TCRTOF(ALFIEL(IROW),R,Z,ITK,'FRFT',ZCR)
      Z=ZCR
C
C++   Correct for drift field distortions
C
      PHI= RPHI/R
      CALL TLACOR(R,PHI,Z,RCR,PHICR,ZCR,MODE)
      R=RCR
      PHI=PHICR
      ZG=ZCR
      CALL TCRTRA(IENDTP(ISLOT),R,PHI,ZG,RG,PHIG)
C
C++   Correct for z distortions measured by VDET
C
      CALL TZCRVD(IENDTP(ISLOT),RG,ZG,ZCR)
      ZG=ZCR
C
C++   Correct for Rz sector alignment measured by VDET
C
      CALL TZCSVD(ISLOT,RG,PHIG,ZG,ZCR)
      ZG=ZCR
C
C++   Correct for Residual field distortions
C
      CALL TCORES(IENDTP(ISLOT),IROWG,RG,PHIG,ZG,RCR,PHICR,ZCR)
      RG  =RCR
      PHIG=PHICR
      ZG  =ZCR
      CALL TFICOR(IENDTP(ISLOT),IROWG,RG,PHIG,ZG,RCR,PHICR,ZCR)
      RG  =RCR
      PHIG=PHICR
      ZG  =ZCR
C
      ITPCO(JTPCIN)=100000*IROWG+1000*ISLOT
      RTPCO(JTPCRV)=RG
      RTPCO(JTPCPH)=PHIG
      RTPCO(JTPCZV)=ZG
      RTPCO(JTPCSR)=(FLOAT(ITABL(KPTNC,IC,JPTNSP))*RW(KPTUN+JPTUSR))**2
      RTPCO(JTPCSZ)=(FLOAT(ITABL(KPTNC,IC,JPTNSZ))*RW(KPTUN+JPTUSZ))**2
      ITPCO(JTPCOF)=0
      ITPCO(JTPCTN)=0
      ITPCO(JTPCCN)=0
      ITPCO(JTPCIT)=0
      RTPCO(JTPCRR)=RPHIS
      RTPCO(JTPCRZ)=ZS
      IER=0
C
  999 CONTINUE
      RETURN
      END
