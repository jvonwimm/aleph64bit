      SUBROUTINE PTBCOJ(LIST,IER)
C
C-----------------------------------------------------------------------
C! Transfer the PTBC bank to the TBCO bank
C!
C!    Author:   R. Johnson  16-06-88
C!    Input :    LIST      BOS event list
C                          if LIST(2:2).eq.'-' then drop POT banks
C     Output:    IER       = 0  successful
C                          = 1  input bank does not exist or is empty
C                          = 2  not enough space
C                          =-1  OK but garbage collection
C!
C!    Called by TPTOJ
C!
C-----------------------------------------------------------------------
      SAVE
C
      INTEGER LMHLEN, LMHCOL, LMHROW
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2)
C
      COMMON /BCS/   IW(1000)
      INTEGER IW
      REAL RW(1000)
      EQUIVALENCE (RW(1),IW(1))
C
      PARAMETER(JTBCIN=1,JTBCRV=2,JTBCPH=3,JTBCZV=4,JTBCRW=5,JTBCZW=6,
     +          JTBCSC=7,LTBCOA=7)
      PARAMETER(JPTBSL=1,JPTBSR=2,JPTBPS=3,JPTBZV=4,JPTBNP=5,JPTBNB=6,
     +          LPTBCA=6)
      PARAMETER(JPTUID=1,JPTUVR=2,JPTURS=4,JPTUPS=5,JPTUZS=6,JPTUSR=7,
     +          JPTUSZ=8,JPTUPB=9,JPTUZB=10,JPTUTM=11,JPTUTL=12,
     +          JPTUAD=13,JPTURP=14,LPTUNA=14)
      PARAMETER (LTPDRO=21,LTTROW=19,LTSROW=12,LTWIRE=200,LTSTYP=3,
     +           LTSLOT=12,LTCORN=6,LTSECT=LTSLOT*LTSTYP,LTTPAD=4,
     +           LMXPDR=150,LTTSRW=11)
      COMMON /TPGEOM/RTPCMN,RTPCMX,ZTPCMX,DRTPMN,DRTPMX,DZTPMX,
     &               TPFRDZ,TPFRDW,TPAVDZ,TPFOF1,TPFOF2,TPFOF3,
     &               TPPROW(LTPDRO),TPTROW(LTTROW),NTSECT,NTPROW,
     &               NTPCRN(LTSTYP),TPCORN(2,LTCORN,LTSTYP),
     &               TPPHI0(LTSECT),TPCPH0(LTSECT),TPSPH0(LTSECT),
     &               ITPTYP(LTSECT),ITPSEC(LTSECT),IENDTP(LTSECT)
C
C
      COMMON /TPGEOP/ NTPDRW(LTSTYP),NTPDPR(LTSROW,LTSTYP),
     &                TPDRBG(LTSTYP),TPDRST(LTSTYP),TPDHGT(LTSTYP),
     &                TPDSEP(LTSTYP),TPDWID(LTSTYP),TPDHWD(LTSTYP),
     &                TPDPHF(LTSROW,LTSTYP),TPDPHW(LTSROW,LTSTYP),
     &                TPDPHS(LTSROW,LTSTYP)
C
C
      CHARACTER*(*) LIST, PLIST*4, JLIST*4
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
        NPTBC=NAMIND('PTBC')
        NPTUN=NAMIND('PTUN')
        NTBCO=NAMIND('TBCO')
        CALL BKFMT('TBCO','2I,(I,6F,I)')
      ENDIF
C
      KPTBC=IW(NPTBC)
      KPTUN=IW(NPTUN)
      IER = 1
      IF (KPTBC.EQ.0 .OR. KPTUN.EQ.0) GOTO 999
      IF (LROWS(KPTBC).EQ.0 .OR. LROWS(KPTUN).EQ.0) GOTO 999
C
      LEN=LTBCOA*LROWS(KPTBC)+LMHLEN
      IW(1)=1
      CALL AUBOS('TBCO',0,LEN,KTBCO,IER)
      IF (IER.EQ.2) GOTO 999
      JLIST = 'TBCO'
      IF (IER.EQ.1) THEN
        KPTUN=IW(NPTUN)
        KPTBC=IW(NPTBC)
      ENDIF
      IW(KTBCO+LMHCOL)=LTBCOA
      IW(KTBCO+LMHROW)=LROWS(KPTBC)
C
      IPTUN=KPTUN+LMHLEN
      DO 500 IC=1,LROWS(KPTBC)
        ISLOT=ITABL(KPTBC,IC,JPTBSL)
        IROW=ITABL(KPTBC,IC,JPTBSR)
        ISTYP=ITPTYP(ISLOT)
        IF (ISTYP.NE.1) THEN
          IROWG=IROW+NTPDRW(1)
        ELSE
          IROWG=IROW
        ENDIF
        PHI=FLOAT(ITABL(KPTBC,IC,JPTBPS))*RW(IPTUN+JPTUPB)
        Z=FLOAT(ITABL(KPTBC,IC,JPTBZV))*RW(IPTUN+JPTUZB)
        R= TPDRBG(ISTYP)+(IROW-1)*TPDRST(ISTYP)
        DPHI=PHI+TPDPHW(IROW,ISTYP)
        PAD= (R*DPHI)/TPDSEP(ISTYP)
        IPAD=MIN(NTPDPR(IROW,ISTYP)+2,INT(PAD+1.5))
        RPHI=R*PHI
        CALL TCTGLB(ISLOT,R,RPHI,Z,RG,RPHIG,ZG)
        PHIG=RPHIG/RG
        IW(KROW(KTBCO,IC)+JTBCIN)=100000*IROW+1000*ISLOT+IPAD
        RW(KROW(KTBCO,IC)+JTBCRV)=RG
        RW(KROW(KTBCO,IC)+JTBCPH)=PHIG
        RW(KROW(KTBCO,IC)+JTBCZV)=ZG
        IW(KROW(KTBCO,IC)+JTBCRW)=ITABL(KPTBC,IC,JPTBNP)
        IW(KROW(KTBCO,IC)+JTBCZW)=ITABL(KPTBC,IC,JPTBNB)
        IW(KROW(KTBCO,IC)+JTBCSC)=0
  500 CONTINUE
C
  998 CONTINUE
C - get the drop flag if any, then drop POT banks if required,
C   add JUL banks to S-list
C   POT banks are on PLIST, JUL banks on JLIST
      PLIST = 'PTBC'
C! add JLIST to S-list, drop PLIST if required
      IF (LNBLNK(LIST).EQ.2) THEN
         IF (LIST(2:2).EQ.'-' .AND. LNBLNK(PLIST).GE.4) THEN
            CALL BDROP (IW,PLIST)
            CALL BLIST (IW,LIST,PLIST(1:LNBLNK(PLIST)))
         ENDIF
      ENDIF
      CALL BLIST (IW,'S+',JLIST(1:LNBLNK(JLIST)))
C
C
      IF (IER.EQ.1) IER = -1
C
  999 CONTINUE
      RETURN
      END
