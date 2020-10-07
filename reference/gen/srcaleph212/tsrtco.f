      SUBROUTINE TSRTCO(LIST,IER)
C
C----------------------------------------------------------------------
C! Sort the TPCO bank and produce the rowlist (TCRL) bank
C!
C!    Author:  R. Johnson   28-06-88
C!    Modified: I. Tomalin  12-05-92 to correct bug in reordering of
C!                                   FTCL bank.
C!    Input :    LIST      BOS event list
C                          if LIST(2:2).eq.'-' drop POT banks
C     Output:    IER       = 0  successful
C                          = 1  input bank does not exist or is empty
C                          = 2  not enough space
C                          =-1  OK but garbage collection
C!
C!    Called by TPTOJ
C!
C!    Comments
C!    ========
C!    This is meant to be used when unpacking the POT.
C!    If the FRTL and FTCL banks exist, then the pointers in FTCL
C!    are updated to point properly into the new TPCO bank.  Thus
C!    the track bank should be unpacked before calling this routine.
C!
C----------------------------------------------------------------------
      SAVE
      CHARACTER*(*) LIST, PLIST*1, JLIST*4
      INTEGER LMHLEN, LMHCOL, LMHROW
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2)
C
      COMMON /BCS/   IW(1000)
      INTEGER IW
      REAL RW(1000)
      EQUIVALENCE (RW(1),IW(1))
C
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
      PARAMETER(JTPCIN=1,JTPCRV=2,JTPCPH=3,JTPCZV=4,JTPCSR=5,JTPCSZ=6,
     +          JTPCOF=7,JTPCTN=8,JTPCCN=9,JTPCIT=10,JTPCRR=11,
     +          JTPCRZ=12,LTPCOA=12)
      PARAMETER(JTCROC=1,JTCRNC=2,JTCRN1=3,LTCRLA=3)
      PARAMETER(JFRTIV=1,JFRTNV=2,JFRTII=3,JFRTNI=4,JFRTNE=5,JFRTIT=6,
     +          JFRTNT=7,JFRTNR=8,LFRTLA=8)
C
      COMMON/TSRTWK/ JSORTW
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
        JSORTW=0
        NTPCO=NAMIND('TPCO')
        NTCRL=NAMIND('TCRL')
        NTTTT=NAMIND('TTTT')
        NFRTL=NAMIND('FRTL')
        NFTCL=NAMIND('FTCL')
      ENDIF
C
      IER = 1
      KTPCO=IW(NTPCO)
      IF (KTPCO.EQ.0) GOTO 999
      IF (LROWS(KTPCO).EQ.0) GOTO 999
C
C++   Book some workspace for sorting
C
      IER = 2
      NCOOR=LROWS(KTPCO)
      LEN=3*NCOOR
      IW(1)=1
      CALL WBANK(IW,JSORTW,LEN,*999)
C
C++   Book space for the rowlist bank
C
      IF (IW(NTCRL).NE.0) CALL BDROP(IW,'TCRL')
C
      LEN= LTPDRO*LTCRLA+LMHLEN
      CALL AUBOS('TCRL',0,LEN,KTCRL,IER)
      IF (IER.EQ.2) GOTO 999
      JLIST = 'TCRL'
      IER2 = IER
      IW(KTCRL+LMHCOL)=LTCRLA
      IW(KTCRL+LMHROW)=LTPDRO
      KTPCO = IW(NTPCO)
C
C++   Temporarily use the name TTTT for the new TPCO bank
C
      IW(1)=1
      CALL AUBOS('TTTT',0,IW(KTPCO),KTTTT,IER)
      IF (IER.EQ.2) GOTO 998
      IER1 = IER
      KTPCO=IW(NTPCO)
      IW(KTTTT+LMHCOL)=LCOLS(KTPCO)
      IW(KTTTT+LMHROW)=LROWS(KTPCO)
C
C++   Make sure the coordinates are pointing to the FRFT track
C
      KFRTL=IW(NFRTL)
      KFTCL=IW(NFTCL)
      IF (KFRTL.NE.0 .AND. KFTCL.NE.0) THEN
        DO 20 IC=1,NCOOR
          IW(KROW(KTPCO,IC)+JTPCTN)=0
   20   CONTINUE
        DO 50 ITK=1,LROWS(KFRTL)
          DO 40 II=1,ITABL(KFRTL,ITK,JFRTNT)
            IC=IW(KFTCL+LMHLEN+ITABL(KFRTL,ITK,JFRTIT)+II)
            IW(KROW(KTPCO,IC)+JTPCTN)=ITK
   40     CONTINUE
          IOFF=ITABL(KFRTL,ITK,JFRTIT)+ITABL(KFRTL,ITK,JFRTNT)
          DO 45 II=1,ITABL(KFRTL,ITK,JFRTNR)
            IC=IW(KFTCL+LMHLEN+IOFF+II)
            IW(KROW(KTPCO,IC)+JTPCTN)=-ITK
   45     CONTINUE
   50   CONTINUE
      ENDIF
C
C++   Make a list of the coordinate indices for sorting
C
      DO 100 IC=1,NCOOR
        IW(JSORTW+IC)=ITABL(KTPCO,IC,JTPCIN)
  100 CONTINUE
C
C++   Sort the coordinates
C
      CALL SORTZV(IW(JSORTW+1),IW(JSORTW+NCOOR+1),NCOOR,-1,0,0)
C
C++   Fill array of where coordinates went to
C
      DO 150 JJ=1,NCOOR
        IC=IW(JSORTW+NCOOR+JJ)
        IW(JSORTW+2*NCOOR+IC)=JJ
  150 CONTINUE
C
C++   Copy the coordinates to the new bank
C
      DO 200 II=1,NCOOR
        IC=IW(JSORTW+NCOOR+II)
        CALL UCOPY(IW(KROW(KTPCO,IC)+1),
     &                   IW(KROW(KTTTT,II)+1),LCOLS(KTPCO))
  200 CONTINUE
C
C++   Update FTCL bank
C
      IF (KFRTL.NE.0 .AND. KFTCL.NE.0) THEN
        DO 250 JJ=1,LROWS(KFTCL)
          ICO=IW(KFTCL+LMHLEN+JJ)
          IW(KFTCL+LMHLEN+JJ)=IW(JSORTW+2*NCOOR+ICO)
  250   CONTINUE
      END IF
C
C++   Swap the name TPCO to the new bank and drop the old one
C
      CALL BSWAP(IW,'TPCO','TTTT')
      CALL BDROP(IW,'TTTT')
      KTPCO=IW(NTPCO)
C
C++   Fill the rowlist bank
C
      IRLST=-1
      DO 300 IC=1,NCOOR
        ID=ITABL(KTPCO,IC,JTPCIN)
        IROW=ID/100000
        ISLOT=(ID-IROW*100000)/1000
        IF (IROW.NE.IRLST) THEN
          IRLST=IROW
          IW(KROW(KTCRL,IROW)+JTCROC)=IC-1
        ENDIF
        IW(KROW(KTCRL,IROW)+JTCRNC)=ITABL(KTCRL,IROW,JTCRNC)+1
        IF (IENDTP(ISLOT).EQ.2) THEN
          IW(KROW(KTCRL,IROW)+JTCRN1)=ITABL(KTCRL,IROW,JTCRN1)+1
        ENDIF
  300 CONTINUE
C
  998 CONTINUE
C - get the drop flag if any, then drop POT banks if required,
C   add JUL banks to S-list
C   POT banks are on PLIST, JUL banks on JLIST
      PLIST = ' '
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
      IF (IER1+IER2 .GT. 0) IER = -1
C
  999 CONTINUE
      CALL WDROP(IW,JSORTW)
      RETURN
      END
