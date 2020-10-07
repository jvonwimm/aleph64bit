      SUBROUTINE PHSTOJ (LIST,IER )
C----------------------------------------------------------------------
C!   Change HCAL POT banks into JULIA banks
C
C   Author   :D. SCHLATTER              5-NOV-1988
C   modified by: G.Capon , F.Ranjard    1-DEC-1988
C   Inputs:    banks:  PHST
C              LIST  /C         BOS event list
C                               if LIST(2:2).eq.'-' drop POT banks
C
C   Outputs:   IER              = 0 successful
C                               = 1 input bank does not exist or empty
C                               = 2 not enough space
C                               =-1 OK but garbage collection
C              banks:  HSDA, HSTO
C                      IF the number of storeys .lt. MXSTO(=1000) THEN
C                         fill HSDA word(JHSDNS)
C                         in this case  JDWORK is used
C                      ENDIF
C        -
C   Calls:     HFNDMD,BDROP,BKFMT,AUBOS
C              SORTZV from KERNLIB
C======================================================================
      SAVE
      INTEGER LMHLEN, LMHCOL, LMHROW
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2)
C
      COMMON /BCS/   IW(1000)
      INTEGER IW
      REAL RW(1000)
      EQUIVALENCE (RW(1),IW(1))
C
      PARAMETER(JHSDTI=1,JHSDPI=2,JHSDSN=3,JHSDDE=4,JHSDMN=5,JHSDSC=6,
     +          JHSDRN=7,JHSDCN=8,JHSDNS=9,LHSDAA=9)
      PARAMETER(JHSTXI=1,JHSTXO=5,JHSTIX=9,JHSTOX=10,JHSTSX=11,
     +          JHSTYI=12,JHSTYO=16,JHSTIY=20,JHSTOY=21,JHSTSY=22,
     +          JHSTZI=23,JHSTZO=27,JHSTIZ=31,JHSTOZ=32,JHSTSZ=33,
     +          JHSTSN=34,JHSTMN=35,JHSTOF=36,LHSTOA=36)
      PARAMETER(JPHSTI=1,JPHSPI=2,JPHSCE=3,JPHSPH=4,LPHSTA=4)
      COMMON /LOCAL/ JDWORK
      CHARACTER*5 SYSTM
      PARAMETER (LHPOI=8, MXSTO=1000)
      REAL CORNR(3,11)
      INTEGER HNREG
      LOGICAL FIRST
      CHARACTER*(*) LIST, PLIST*4, JLIST*8
      DATA FIRST/.TRUE./
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
      IF(FIRST) THEN
        FIRST=.FALSE.
        NAPHST=NAMIND('PHST')
        NAHSDA=NAMIND('HSDA')
        NAHSTO=NAMIND('HSTO')
        JDWORK = 0
        CALL BKFMT('HSDA','2I,(3I,F,5I)')
        CALL BKFMT('HSTO','2I,(33F,3I)')
      ENDIF
C
      IER = 1
      IF(IW(NAPHST).LE.0) GOTO 999
      NHSTO=LROWS (IW(NAPHST))
      IF(NHSTO.LE.0) GOTO 999
C
C           create HSDA bank
      LNHSD=LMHLEN+NHSTO*LHSDAA
      CALL AUBOS('HSDA',0,LNHSD,KHSDA,IER)
      IF (IER.EQ.2) GOTO 999
      JLIST = 'HSDA'
      IER1 = IER
      IW(KHSDA+LMHCOL)=LHSDAA
      IW(KHSDA+LMHROW)=NHSTO
C
      KPHST=IW(NAPHST)
      DO 100 IST=1,NHSTO
C?           get module #, subcomp #
        ITETHS=ITABL(KPHST,IST,JPHSTI)
        ISTKHS=1
        IF(ITETHS.GT.128) THEN
          ITETHS=ITETHS-128
          ISTKHS=2
        ENDIF
        IPHIHS=ITABL(KPHST,IST,JPHSPI)
C
        CALL HFNDMD(ITETHS,IPHIHS,ISTKHS,ISUBHS,IMODHS,IOVRHS)
C
        IW(KROW(KHSDA,IST)+JHSDTI)=ITETHS
        IW(KROW(KHSDA,IST)+JHSDPI)=IPHIHS
        IW(KROW(KHSDA,IST)+JHSDSN)=ISTKHS
        RW(KROW(KHSDA,IST)+JHSDDE)=RTABL(KPHST,IST,JPHSCE)
        IW(KROW(KHSDA,IST)+JHSDMN)=IMODHS
        IW(KROW(KHSDA,IST)+JHSDSC)=ISUBHS
        IW(KROW(KHSDA,IST)+JHSDRN)=HNREG(ITETHS)
        IW(KROW(KHSDA,IST)+JHSDCN)=ITABL(KPHST,IST,JPHSPH)
  100 CONTINUE
C
C -  get next storeys of a cluster/calobject when the number of storeys
C    is reasonable (max. number of storeys MXSTO=1000)
C
      IF (NHSTO .GT. MXSTO) GOTO 190
C
C      create a work bank JDWORK to sort HSDA
C      If not enough space skip this part: next storey is not filled
      CALL WBANK (IW,JDWORK,NHSTO,*190)
      DO 102 IST = 1,NHSTO
         IW(JDWORK+IST)=(IST-1)*LHSDAA+1
 102  CONTINUE
C
      JJHSDA=KHSDA+LMHLEN+JHSDCN-1
      IIHSDA=KHSDA+LMHLEN+JHSDNS-1
      CALL SORTZV(IW(JJHSDA+1),IW(JDWORK+1),NHSTO,-1,0,1)
C
      IW(IIHSDA+1)=0
      IF(NHSTO.GT.1) THEN
        DO 101 IST=2,NHSTO
          IW(IIHSDA+IW(JDWORK+IST))=0
          IF(IW(JJHSDA+IW(JDWORK+IST)).EQ.IW(JJHSDA+IW(JDWORK+IST-1)) )
     2     IW(IIHSDA+IW(JDWORK+IST-1))=IW(JDWORK+IST)/LHSDAA+1
  101   CONTINUE
      ENDIF
C
C          drop work bank JDWORK
      CALL WDROP (IW,JDWORK)
C
 190  CONTINUE
C
C           create HSTO bank
      LNHST=LMHLEN+NHSTO*LHSTOA
      CALL AUBOS('HSTO',0,LNHST,JHSTO,IER)
      IF (IER.EQ.2) GOTO 998
      IER2 = IER
      JLIST = JLIST(1:LENOCC(JLIST)) // 'HSTO'
      JPHST=IW(NAPHST)
      IW(JHSTO+LMHCOL) = LHSTOA
      IW(JHSTO+LMHROW) = NHSTO
      KHSTO=JHSTO+LMHLEN
C
      DO 200 IST=1,NHSTO
C
        ITETHS=ITABL(JPHST,IST,JPHSTI)
        ISTKHS=1
        IF(ITETHS.GT.128) THEN
          ITETHS=ITETHS-128
          ISTKHS=2
        ENDIF
        IPHIHS=ITABL(JPHST,IST,JPHSPI)
C
C?           get module #, subcomp #
C
        CALL HFNDMD(ITETHS,IPHIHS,ISTKHS,ISUBHS,IMODHS,IOVRHS)
C
C?           compute coordinates of storey corners
C
        CALL VZERO(CORNR,33)
        SYSTM='ALEPH'
        CALL HSRCR(SYSTM,ITETHS,IPHIHS,ISTKHS,LHPOI,CORNR)
C
C?            compute coordinates of center of inner face,outer face,
C
        DO 11 K=1,3
           DO 12 N=1,4
              CORNR(K,9)=CORNR(K,9)+CORNR(K,N)/4.
              CORNR(K,10)=CORNR(K,10)+CORNR(K,N+4)/4.
   12      CONTINUE
           CORNR(K,11)=(CORNR(K,9)+CORNR(K,10))/2.
   11   CONTINUE
C
C?            fill HSTO bank
C
        I=0
        DO 20 K=1,3
           DO 20 N=1,11
              I=I+1
              RW(KHSTO+I)=CORNR(K,N)
   20   CONTINUE
C
        IW(KHSTO+JHSTSN)=ISUBHS
        IW(KHSTO+JHSTMN)=IMODHS
        IW(KHSTO+JHSTOF)=IOVRHS
        KHSTO=KHSTO+LHSTOA
 200  CONTINUE
C
 998  CONTINUE
C - get the drop flag if any, then drop POT banks if required,
C   add JUL banks to S-list
C   POT banks are on PLIST, JUL banks on JLIST
      PLIST = 'PHST'
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
      END
