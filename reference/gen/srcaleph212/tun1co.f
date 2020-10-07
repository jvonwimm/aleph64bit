      SUBROUTINE TUN1CO(IC,ITPCO,RTPCO,IER)
C
C---------------------------------------------------------------------
C! Unpack single coordinate from PTCO
C!
C!    Author:  R. Johnson    15-06-88
C!
C!    Input:  IC       /I         Coordinate number to unpack
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
      PARAMETER(JPTCSL=1,JPTCSR=2,JPTCPS=3,JPTCZV=4,JPTCSP=5,JPTCSZ=6,
     +          JPTCDR=7,LPTCOA=7)
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
      DATA FIRST/.TRUE./
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
        NPTCO=NAMIND('PTCO')
        NPTUN=NAMIND('PTUN')
      ENDIF
      KPTCO=IW(NPTCO)
      IF (KPTCO.EQ.0) THEN
        IER=1
        GO TO 999
      ENDIF
      KPTUN=IW(NPTUN)
      IF (KPTUN.EQ.0) THEN
        IER=2
        GO TO 999
      ENDIF
      KPTUN=KPTUN+LMHLEN
      IF (IC.LT.1 .OR. IC.GT.LROWS(KPTCO)) THEN
        IER=3
        GO TO 999
      ENDIF
      IROW=ITABL(KPTCO,IC,JPTCSR)
      ISLOT=ITABL(KPTCO,IC,JPTCSL)
      ISTYP=ITPTYP(ISLOT)
      IF (ISTYP.NE.1) THEN
        IROWG=IROW+NTPDRW(1)
      ELSE
        IROWG=IROW
      ENDIF
C
C++   The following IF statement allows one to read MC events
C++   generated before JPTCDR was added to the end of the PTCO bank.
C
      IF (JPTCDR.LE.LCOLS(KPTCO)) THEN
        DR= FLOAT(ITABL(KPTCO,IC,JPTCDR))*RW(KPTUN+JPTURS)
      ELSE
        DR= 0.
      ENDIF
      RG= TPPROW(IROWG)+DR
      PHI=FLOAT(ITABL(KPTCO,IC,JPTCPS))*RW(KPTUN+JPTUPS)
      DPHI=PHI+TPDPHW(IROW,ISTYP)
      PAD= (RG*DPHI)/TPDSEP(ISTYP)
      IPAD=MIN(NTPDPR(IROW,ISTYP)+2,INT(PAD+1.5))
      IPAD=NTPDPR(IROW,ISTYP)+2 - IPAD
      ITPCO(JTPCIN)=IROWG*100000 + ISLOT*1000 + IPAD
      ZS= FLOAT(ITABL(KPTCO,IC,JPTCZV))*RW(KPTUN+JPTUZS)
C
C++   Convert to the global reference frame
C
      IF (IENDTP(ISLOT).EQ.2) THEN
        PHIG= PHI + TPPHI0(ISLOT)
        ZG= ZS - ZTPCMX
      ELSE
        PHIG= TPPHI0(ISLOT) - PHI
        ZG=  ZTPCMX - ZS
      ENDIF
      IF (PHIG.GT.TWOPI) THEN
        PHIG=PHIG-TWOPI
      ELSEIF (PHIG.LT.0.) THEN
        PHIG=PHIG+TWOPI
      ENDIF
C
      RTPCO(JTPCRV)=RG
      RTPCO(JTPCPH)=PHIG
      RTPCO(JTPCZV)=ZG
      RTPCO(JTPCSR)=(FLOAT(ITABL(KPTCO,IC,JPTCSP))*RW(KPTUN+JPTUSR))**2
      RTPCO(JTPCSZ)=(FLOAT(ITABL(KPTCO,IC,JPTCSZ))*RW(KPTUN+JPTUSZ))**2
      ITPCO(JTPCOF)=0
      ITPCO(JTPCTN)=0
      ITPCO(JTPCCN)=0
      ITPCO(JTPCIT)=0
      IER=0
C
  999 CONTINUE
      RETURN
      END
