      SUBROUTINE HCCRPL
C --------------------------------------------
C
C!   Builds the plane hit bank
CKEY HCAL HVPLANE HIT / INTERNAL
C!     Author      : G.Catanesi  89/02/03
C!     Mod. by     : L.Silvestris 24/6/92
C!
C!     Input bank  : JDHCSE   McHcTrachSegment
C!                   JDHCHI   McHcHits
C!     Output bank : 'HPHT'   McHcPlanes
C!
C!         Called by: HCASIG
C -----------------------------------------
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
      PARAMETER(JHPHPA=1,JHPHED=2,LHPHTA=2)
      COMMON /HCCOUN/NHCC01,NHCC02,NHCC03,NHCC04,NHCC05,HCEAVE ,HCANST,
     +               HCEPOR(3) ,FHCDEB,FHCDB1,FHCDB2
      LOGICAL FHCDEB,FHCDB1,FHCDB2
C
      INTEGER GTSTUP
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
      JHPHT = IW(NAHPHT)
      NHPHT = 0
      KHPHT = JHPHT + LMHLEN
C
C     decode date period
C
      IHC = GTSTUP('HC',IRUN)
C
      DO 50 I=1, NHCHI
         NSEG = IW(KHCHI+1)
         IPOR = ITABL (JDHCSE,NSEG,4)
         IMOD = ITABL (JDHCSE,NSEG,3)
         ILAY = ITABL (JDHCSE,NSEG,2)
C
C  decode plane number for different period..
C  89-90-91 double planes in barrel and endcaps
C  92       double planes in barrel and single planes in endcaps
C
      IF(IHC.GT.2) THEN
         IF (IPOR .NE. 2.OR. ILAY.EQ.1) THEN
             NPL = ILAY
         ELSE
             NPL = ILAY/2 + 1
         ENDIF
      ELSE
         IF (ILAY.EQ.1) THEN
             NPL = ILAY
         ELSE
             NPL = ILAY/2 + 1
         ENDIF
      ENDIF
C
         NADR = IPOR*10000 + IMOD*100 + NPL
C
C - If it is not the 1st entry sum up energy on identical plane
         IF(NHPHT.GT.0) THEN
            DO 10 JROW = 1,NHPHT
               IF(NADR.EQ.ITABL(JHPHT,JROW,1)) THEN
                  KJROW = KROW(JHPHT,JROW)
                  RW(KJROW+2) = RW(KJROW+2) + HCSTEN(IW(KHCHI+4))
                  GOTO 50
               ENDIF
   10       CONTINUE
         ENDIF
C
C - Fill 'HPHT' with new plane
         NHPHT = NHPHT + 1
         IW(KHPHT+JHPHPA) = NADR
         RW(KHPHT+JHPHED) = HCSTEN(IW(KHCHI+4))
C
         KHPHT = KHPHT + LHPHTA
C
   50 KHCHI = KHCHI + LHCHIA
      IW(JHPHT+LMHROW) = NHPHT
C
      IF (FHCDEB) THEN
         KHPHT = JHPHT + LMHLEN
         WRITE (LOUTIO,500) IW(JHPHT+LMHROW)
         WRITE (LOUTIO,510) (IW(K+1),RW(K+2),K=KHPHT,
     &                        KHPHT+NHPHT*LHPHTA-1,LHPHTA)
      ENDIF
      RETURN
C
  500 FORMAT (/1X,'+++HCCRPL+++ HPHT McHcPlanes - ', 5X,I3,' planes'/
     +4(4X,'PoMdPl' ,'   Energy', 6X))
  510 FORMAT (4(I10,F10.5,5X))
      END
