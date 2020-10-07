      SUBROUTINE HCCRTO
C --------------------------------------------
C
C!   Builds the tower hit bank
C
C!     Author      :G.Zito      86/05/21
C!     Modified    :G.Catanesi  86/12/03
C!                 :G.Catanesi  87/10/21
C!                 :G.Catanesi  88/08/20
C!
C!     Input bank  : JDHCSE   McHcTrachSegment
C!                   JDHCHI   McHcHits
C!     Output bank : 'HTHT'   McHcStoreys
C!
C!         Called by: HCASIG
C!        -Calls : HFNDTR, HCSTEN     from this .HLB
C!                 HFNDTW             from Alephlib
C------------------------------------------------------
      SAVE
      INTEGER LMHLEN, LMHCOL, LMHROW
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2)
C
      COMMON /BCS/   IW(1000)
      INTEGER IW
      REAL RW(1000)
      EQUIVALENCE (RW(1),IW(1))
C
      PARAMETER (LOFFMC = 1000)
      PARAMETER (LHIS=20, LPRI=20, LTIM=6, LPRO=6, LRND=3)
      PARAMETER (LBIN=20, LST1=LBIN+3, LST2=3)
      PARAMETER (LSET=15, LTCUT=5, LKINP=20)
      PARAMETER (LDET=9,  LGEO=LDET+4, LBGE=LGEO+5)
      PARAMETER (LCVD=10, LCIT=10, LCTP=10, LCEC=15, LCHC=10, LCMU=10)
      PARAMETER (LCLC=10, LCSA=10, LCSI=10)
      COMMON /JOBCOM/   JDATJO,JTIMJO,VERSJO
     &                 ,NEVTJO,NRNDJO(LRND),FDEBJO,FDISJO
     &                 ,FBEGJO(LDET),TIMEJO(LTIM),NSTAJO(LST1,LST2)
     &                 ,IDB1JO,IDB2JO,IDB3JO,IDS1JO,IDS2JO
     &                 ,MBINJO(LST2),MHISJO,FHISJO(LHIS)
     &                 ,IRNDJO(LRND,LPRO)
     &                 ,IPRIJO(LPRI),MSETJO,IRUNJO,IEXPJO,AVERJO
     3                 ,MPROJO,IPROJO(LPRO),MGETJO,MSAVJO,TIMLJO,IDATJO
     5                 ,TCUTJO(LTCUT),IBREJO,NKINJO,BKINJO(LKINP),IPACJO
     6                 ,IDETJO(LDET),IGEOJO(LGEO),LVELJO(LGEO)
     7                 ,ICVDJO(LCVD),ICITJO(LCIT),ICTPJO(LCTP)
     8                 ,ICECJO(LCEC),ICHCJO(LCHC),ICLCJO(LCLC)
     9                 ,ICSAJO(LCSA),ICMUJO(LCMU),ICSIJO(LCSI)
     &                 ,FGALJO,FPARJO,FXXXJO,FWRDJO,FXTKJO,FXSHJO,CUTFJO
     &                 ,IDAFJO,IDCHJO,TVERJO
      LOGICAL FDEBJO,FDISJO,FHISJO,FBEGJO,FGALJO,FPARJO,FXXXJO,FWRDJO
     &       ,FXTKJO,FXSHJO
      COMMON /JOBKAR/   TITLJO,TSETJO(LSET),TPROJO(LPRO)
     1                 ,TKINJO,TGEOJO(LBGE),TRUNJO
      CHARACTER TRUNJO*60
      CHARACTER*4 TKINJO,TPROJO,TSETJO,TITLJO*40
      CHARACTER*2 TGEOJO
C
      PARAMETER (LERR=20)
      COMMON /JOBERR/   ITELJO,KERRJO,NERRJO(LERR)
      COMMON /JOBCAR/   TACTJO
      CHARACTER*6 TACTJO
C
      PARAMETER (LFIL=6)
      COMMON /IOCOM/   LGETIO,LSAVIO,LGRAIO,LRDBIO,LINPIO,LOUTIO
      DIMENSION LUNIIO(LFIL)
      EQUIVALENCE (LUNIIO(1),LGETIO)
      COMMON /IOKAR/   TFILIO(LFIL),TFORIO(LFIL)
      CHARACTER TFILIO*60, TFORIO*4
C
      COMMON /HCCOUN/NHCC01,NHCC02,NHCC03,NHCC04,NHCC05,HCEAVE ,HCANST,
     +               HCEPOR(3) ,FHCDEB,FHCDB1,FHCDB2
      LOGICAL FHCDEB,FHCDB1,FHCDB2
C
      PARAMETER(NHTTR=192,NHWTR=36)
      PARAMETER (MHCSE=200)
      PARAMETER(MHCTH=300)
      COMMON /HCNAMC/NAHTHT,NAHWHT,NAHTTR,NAHWTR,NAHTDI,NAHWDI, NAHTDT,
     &               NAHTTD,NAHWDT,NAHWTD,NAHPDI,NAHPHT,NAHLWD,
     &               JDHCSE,JDHCHI,JDHCTH
      PARAMETER(JHCSTN=1,JHCSLN=2,JHCSMN=3,JHCSPN=4,JHCSTA=5,JHCSTX=6,
     +          JHCSTY=7,JHCSPW=8,JHCSXA=9,JHCSYA=10,JHCSZA=11,
     +          JHCSTR=12,JHCSHT=13,JHCSFT=14,JHCSFS=15,LHCSEA=15)
      PARAMETER(JHCHTS=1,JHCHSY=2,JHCHSP=3,JHCHNS=4,LHCHIA=4)
      PARAMETER(JHTHSA=1,JHTHPT=2,JHTHTT=3,JHTHED=4,LHTHTA=4)
      DIMENSION ANGL(2), ITOW(3), ITRI(2)
      CHARACTER*6 SYSRF
C
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
C       Loop on hits
C
      NHCHI = LROWS (JDHCHI)
      IF(NHCHI.EQ.0) RETURN
      KHCHI = JDHCHI + LMHLEN
      JHTHT = IW(NAHTHT)
      NHTHT = 0
      KHTHT = JHTHT + LMHLEN
C
C       ANGL  = phi, theta
C       ITOW  = nphi, ntheta, nstac
C       ITRI  = nphi trigger, ntheta trigger
C
      DO 50 I=1, NHCHI
         NSEG = IW(KHCHI+1)
         KHCSE = KROW (JDHCSE,NSEG)
         IPOR = ITABL (JDHCSE,NSEG,4)
         ILAY = ITABL (JDHCSE,NSEG,2)
C
C find tower adress i,j,k
C
         CALL HFNDTW(RW(KHCSE+9),SYSRF,ITOW(2),ITOW(1),ITOW(3), ISUBC,
     +   IMOD,IPL,IDEAD)
C
         IF(IDEAD.GT.0) GOTO 50
C
         CALL HFNDTR(RW(KHCSE+9),IPOR,ITOW,ITRI)
C
         NTOW = ITOW(1)*10000 + ITOW(2)*100 + ITOW(3)
C
C - IF it is not the 1st entry in 'HTHT' THEN
C      IF this tower# has already been entered into 'HTHT' THEN
C         sum up energy into this tower
C         jump to the next tower
C   ELSE  check dead regions if not Hcdetail
C         fill tower bank
C   ENDIF
C
         IF (NHTHT.EQ.0) GOTO 30
         DO 10 JROW =1,NHTHT
            IF (NTOW .EQ. ITABL(JHTHT,JROW,1)) GOTO 20
   10    CONTINUE
         GOTO 30
C        the tower# has already been entered
   20    EDEP = HCSTEN(IW(KHCHI+4))
         KJROW = KROW(JHTHT,JROW)
         RW(KJROW+4) = RW(KJROW+4) + EDEP
         IF(FHCDB2)THEN
            HCEPOR(IPOR) = HCEPOR(IPOR) + EDEP
            IROW = LOCTAB (IW(JDHCTH+LMHLEN+1),LCOLS(JDHCTH) ,LROWS
     +      (JDHCTH),1,IW(KHCSE+12))
            KHCTH = KROW(JDHCTH,IROW)
            RW(KHCTH+5) =RW(KHCTH+5)+EDEP
         ENDIF
         IW(KHCSE+JHCSFS) = JROW
         GOTO 50
C
   30    CONTINUE
C
C - Fill 'HTHT' with new tower
         NHTHT = NHTHT + 1
         IW(KHTHT+1) = NTOW
         IW(KHTHT+2) = ITRI(1)
         IW(KHTHT+3) = ITRI(2)
         RW(KHTHT+4) = HCSTEN(IW(KHCHI+4))
C
         IF(FHCDB2)THEN
            HCEPOR(IPOR) = HCEPOR(IPOR) + RW(KHTHT+4)
            IROW = LOCTAB (IW(JDHCTH+LMHLEN+1),LCOLS(JDHCTH) ,LROWS
     +      (JDHCTH),1,IW(KHCSE+12))
            KHCTH = KROW(JDHCTH,IROW)
            RW(KHCTH+5) = RW(KHCTH+5) + RW(KHTHT+4)
         ENDIF
         KHTHT = KHTHT + LHTHTA
C
   40    IW(KHCSE+JHCSFS) = NHTHT
   50 KHCHI = KHCHI + LHCHIA
      IW(JHTHT+LMHROW) = NHTHT
C
      IF (FHCDEB) THEN
         ETOT = 0.
         KHTHT = JHTHT + LMHLEN
         DO 60 I = 1,NHTHT
            ETOT = ETOT + RW(KHTHT+4)
 60      KHTHT = KHTHT + LHTHTA
         KHTH1 = JHTHT + LMHLEN
         WRITE (LOUTIO,500) LROWS(JHTHT),ETOT
         WRITE (LOUTIO,510) ((IW(K+I),I=1,3),RW(K+4),K=KHTH1, KHTH1
     +   +NHTHT*LHTHTA-1,LHTHTA)
      ENDIF
      RETURN

 500  FORMAT (/1X,'+++HCCRTO+++ McHcStoreys - ',5X,I3,' storeys , ',
     &        F5.2,' GeV deposited energy'/
     +3(4X,'PhThSt  Pt  Tt' ,'   Energy', 6X))
  510 FORMAT (3(I10,I4,I4,F10.5,5X))
      END
