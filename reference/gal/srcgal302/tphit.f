      SUBROUTINE TPHIT
C---------------------------------------------------------------------
C  M.Mermikides                                       24/7/85
C
C  Steering for computation of track intersectons with padrows
C  using current track element parameters.
C -Track elements are saved (optionally) in BOS bank 'TPTE' in the form
C   (Track ID, x,y,z,dx/ds,dy/ds/dz/ds,p,s,TOF,m,Q)
C  for detailed simulation in TPDIGI. These are also useful for
C  display purposes.
C -First order pad hits (intersections) are computed in routine TPGETH
C -To avoid excessive numbers of track elements and hits, we stop
C  following low momentum tracks after a given path length)
C----------------------------------------------------------------------
      SAVE
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
      INTEGER LMHLEN, LMHCOL, LMHROW
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2)
C
      COMMON /BCS/   IW(1000)
      INTEGER IW
      REAL RW(1000)
      EQUIVALENCE (RW(1),IW(1))
C
      COMMON /NAMCOM/   NARUNH, NAPART, NAEVEH, NAVERT, NAKINE, NAKRUN
     &                 ,NAKEVH, NAIMPA, NAASEV, NARUNE, NAKLIN, NARUNR
     &                 ,NAKVOL, NAVOLU
      EQUIVALENCE (NAPAR,NAPART)
C
      PARAMETER (LFIL=6)
      COMMON /IOCOM/   LGETIO,LSAVIO,LGRAIO,LRDBIO,LINPIO,LOUTIO
      DIMENSION LUNIIO(LFIL)
      EQUIVALENCE (LUNIIO(1),LGETIO)
      COMMON /IOKAR/   TFILIO(LFIL),TFORIO(LFIL)
      CHARACTER TFILIO*60, TFORIO*4
C
      PARAMETER (LETRK=27, LITRK=11, LNTRK=10)
      COMMON /TRKCOM/   ITRKEL(LITRK), TRKELE(LETRK), TRKNXT(LNTRK)
     &                , FTRHAD
      LOGICAL FTRHAD
      COMMON /TRKKAR/   TRKVOL
      CHARACTER*4 TRKVOL
C
      PARAMETER (LTPTE=12, LBTE=1000*LTPTE, LBTEX=LBTE/2)
      PARAMETER (NWPHT=6 , LBHT=1000*NWPHT, LBHTX=LBHT/2)
      PARAMETER (NWPHE=1 , LBHE=1000*NWPHE, LBHEX=LBHE/2)
      COMMON /TPNAMC/ NATPTE,NATPHT,NATPHE,NATTHT,NATTHE,NATPCO,NATPCH,
     &                 NATCRL
     &               ,JDTWRK,JDSORT,JDWORK
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
C --------------------------------------------------------------
C - if 1st entry of the event check BOS banks
C
      DATA PCUT/0.10/, MAXEL/20/
C
      IF (FBEGJO(3)) THEN
         CALL TPIBOS
         FBEGJO(3) = .FALSE.
      ENDIF
C
C  Skip if first entry or step length = 0.
C
      IF (ITRKEL(8).EQ.1.OR.TRKELE(11).EQ.0.0) THEN
         ITE = 0
         IKLST = ITRKEL(1)
         NEL = 0
         GO TO 999
      ENDIF
C
C For low momentum tracks skip storage of track elements and hits
C after MAXEL steps
C
      IF (IKLST.EQ.ITRKEL(1).AND.TRKELE(7).LT.PCUT) THEN
         NEL = NEL + 1
         IF(NEL.GT.MAXEL) GO TO 999
      ENDIF
C
      IF (ICTPJO(4).GT.0)  THEN
C
C  Save track segment parameters in 'TPTE' bank, if requested
C  Create 'TPTE' bank if it doesn't exist.
C
         ITPTE = IW(NATPTE)
         IF (ITPTE.EQ.0) THEN
            CALL ALBOS ('TPTE',0,LBTE,ITPTE,IGARB)
            CALL BLIST(IW,'E+','TPTE')
            IW(ITPTE + 1) = LTPTE
            IW(ITPTE + 2) = 0
         ENDIF
C  If no room to store next hit, increase size of bank
         IF(LFRWRD(ITPTE) .LT. LCOLS(ITPTE)) THEN
            NDATA = IW(ITPTE) + LBTEX
            CALL ALBOS ('TPTE',0,NDATA,ITPTE,IGARB)
         ENDIF
C  KTE1 = address of next row
         KTE1 = KNEXT(ITPTE)
         IW(KTE1 + 1)  = ITRKEL(1)
         CALL UCOPY(TRKELE(1), RW(KTE1 + 2), 7)
         RW(KTE1 +  9) = TRKELE(11)
         RW(KTE1 + 10) = TRKELE(10)
         RW(KTE1 + 11) = TRKELE(13)
         RW(KTE1 + 12) = TRKELE(14)
         IW(ITPTE + 2) = IW(ITPTE + 2) + 1
         ITE = IW(ITPTE + 2)
      ENDIF
C
C  Compute intersections of charged track segment with padrows
C
      IF (NINT(TRKELE(14)).NE.0)  THEN
         IF(ICTPJO(2).GT.0) CALL TPGETH(ITE)
         IF(ICTPJO(3).GT.0) CALL TTGETH(ITE)
      ENDIF
C
999   RETURN
      END
