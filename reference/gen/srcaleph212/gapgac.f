      SUBROUTINE GAPGAC
C----------------------------------------------------------------------
C!    From GAPGPC routine to fill extended photon bank PGAC
C!  - BUILD PGAC BANK (RESULT OF GAMPEC)
C!          EGPR bank (list of photon tower)
C!   Author   :  MN Minard             27-09-1994
CKEY GAMPACK PGAC EGPR
C?
C!======================================================================
      PARAMETER (JPGAEC=1,JPGATC=2,JPGAPC=3,JPGAR1=4,JPGAR2=5
     +,JPGAF4=6,JPGADM=7,JPGAST=8,JPGAQU=9,JPGAQ1=10,JPGAQ2=11
     +,JPGAM1=12,JPGAM2=13,JPGAMA=14,JPGAER=15,JPGATR=16,JPGAPR=17
     +,JPGAEF=18,JPGAGC=19,JPGAZS=20,JPGAPL=21,JPGAPH=22,JPGAPN=23
     +,JPGAFA=24,JPGAPE=25,LPGACA= 25)
      PARAMETER(JEGPPR=1,JEGPGR=2,LEGPRA=2)
      PARAMETER(JEGRSE=1,JEGRC1=2,JEGRC2=3,JEGRC3=4,JEGRCU=5,JEGRDS=6,
     &          JEGR12=7,JEGR23=8,LEGRPA=8)
      PARAMETER(JPECER=1,JPECE1=2,JPECE2=3,JPECTH=4,JPECPH=5,JPECEC=6,
     +          JPECKD=7,JPECCC=8,JPECRB=9,JPECPC=10,LPECOA=10)
      INTEGER LMHLEN, LMHCOL, LMHROW
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2)
C
      COMMON /BCS/   IW(1000)
      INTEGER IW
      REAL RW(1000)
      EQUIVALENCE (RW(1),IW(1))
C
C! Arrays used in fake photon analysis:
      PARAMETER ( NFPHO1 = 20 )
      COMMON / GAFAKE / PF (5,NFPHO1)
C  Additional informations:
      COMMON / GGMORE / GAMORE(21,NFPHO1)
C --- max number of cluster per PECO cluster
      PARAMETER ( NKLUM1 = 50     )
      DIMENSION GFL   (10,NKLUM1)
      DIMENSION GAMFL (28),      IGAMFL(28)
      EQUIVALENCE(GAMFL,IGAMFL)
C
      PARAMETER ( NPCOR = 20)
      COMMON / GACORF / GACORA (3,NPCOR)
C --- max number of STOREYS per PHOTON
      PARAMETER ( NSTTTT = 500    )
C --- max number of photon per peco cluster
      PARAMETER ( NFPHOT = 20     )
      COMMON/GASTIN/ LGASTO(NFPHOT,NSTTTT) , NNSTGA(NFPHOT)
      DIMENSION STACE(6),BARY(4)
      PARAMETER ( EMIN0 = 0.25 , NFOMAX = 20 , NGAMVE = 20 )
      PARAMETER ( PSATU = 0.00078 )
      DIMENSION GAMVE(NGAMVE,NFOMAX) , GAMCE(NGAMVE,NFOMAX)
      DIMENSION PFAKE(4)
      DATA NAPEST,NAPECO,NAPGAC,NAEGPR,NAEGRP /5*0/
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
C ---------------------------------------------------------------------
C-  Set name index
C
      IF (NAPEST.EQ.0) THEN
        CALL BKFMT('PGAC','2I,(7F,2I,13F,3I)')
        NAPEST = NAMIND('PEST')
        NAPECO = NAMIND('PECO')
        NAPGAC = NAMIND('PGAC')
        NAEGPR = NAMIND('EGPR')
        NAEGRP = NAMIND('EGRP')
        KEGRP  = IW(NAEGRP)
        IF (KEGRP.EQ.0) THEN
          LRCONS = JUNIDB(0)
          KEGRP = MDARD(IW,LRCONS,'EGRP',0)
        ENDIF
        IF (KEGRP.EQ.0) THEN
          EMIN = EMIN0
        ELSE
          EMIN = RTABL(KEGRP,1,JEGRCU)
        ENDIF
      ENDIF
C
C-    Built EGAM
C
      KPECO = IW(NAPECO)
      NPECO = 0
      IF ( KPECO.NE.0 ) NPECO = LROWS(KPECO)
      NGAMX = 3*NPECO
C
C-    BUILT PGAC BANK
C
      KPGAC = NDROP('PGAC',0)
      NLENG = NGAMX*LPGACA+LMHLEN
      CALL AUBOS('PGAC',0,NLENG,KPGAC,IGARB)
      IF (IGARB.EQ.2) GO TO 998
      CALL BLIST(IW,'E+','PGAC')
      IW(KPGAC+LMHCOL) = LPGACA
      IW(KPGAC+LMHROW) = 0
      IF ( IGARB.NE.0) KPECO = IW(NAPECO)
C
C-    Built EGPR bank
C
      KEGPR = NDROP('EGPR',0)
      KPEST = IW(NAPEST)
      NPEST = 0
      IF (KPEST.NE.0) NPEST = LROWS(KPEST)
      NLENG = NPEST*LEGPRA + LMHLEN
      CALL AUBOS('EGPR',0,NLENG,KEGPR,IGARB)
      IF(IGARB.EQ.2) GO TO 998
      CALL BLIST(IW,'E+','EGPR')
      IF ( IGARB.NE.0) THEN
         KPECO = IW(NAPECO)
         KPGAC = IW(NAPGAC)
         KPEST = IW(NAPEST)
      ENDIF
      IW(KEGPR+LMHROW) = 0
      IW(KEGPR+LMHCOL) = LEGPRA
C
C-    Now loop on PECO and fill PGaC
C
      INRES = 0
      NGAM = 0
      DO IPECO=1,NPECO
        IF( RTABL(KPECO,IPECO,JPECEC).GT.EMIN ) THEN
          CALL GAMPEX (IPECO,EMIN,NGAMVE*NFOMAX,NNGA,GAMVE,GAMCE,IRTG)
          IF ( IRTG.GE.0.AND.NNGA.GT.0) THEN
            IF ( NGAM+NNGA.GT.NGAMX) THEN
              NGAMX = NGAMX+NNGA + 10
              NLENG = NGAMX*LPGACA+LMHLEN
              CALL AUBOS('PGAC',0,NLENG,KPGAC,IGARB)
              IF ( IGARB.EQ.2) GO TO 998
              IF ( IGARB.NE.0) THEN
                KPECO = IW(NAPECO)
                KEGPR = IW(NAEGPR)
                KPEST = IW(NAPEST)
              ENDIF
            ENDIF
            DO IGAM = 1,NNGA
              JPGAC = KROW(KPGAC,IGAM+NGAM)
              RW(JPGAC+JPGAEC) = GAMVE(1,IGAM)*GAMVE(4,IGAM)
              RW(JPGAC+JPGATC) = GAMVE(2,IGAM)
              RW(JPGAC+JPGAPC) = GAMVE(3,IGAM)
              RW(JPGAC+JPGAR1) = GAMVE(9,IGAM)
              RW(JPGAC+JPGAR2) = GAMVE(10,IGAM)
              RW(JPGAC+JPGAF4) = GAMVE(6,IGAM)
              RW(JPGAC+JPGADM) = GAMVE(16,IGAM)
              IW(JPGAC+JPGAST) = INT(GAMVE(8,IGAM))
              IW(JPGAC+JPGAQU) = INT(GAMVE(13,IGAM))
     &        +INT(GAMVE(11,IGAM))*100
              RW(JPGAC+JPGAQ1) = GAMVE(7,IGAM)
              RW(JPGAC+JPGAQ2) = GAMVE(20,IGAM)
              RW(JPGAC+JPGAM1) = GAMCE(1,IGAM)
              RW(JPGAC+JPGAM2) = GAMCE(2,IGAM)
              RW(JPGAC+JPGAMA) = GAMCE(20,IGAM)
C
C-           Correct for leakage  & saturation
C
              COROV = GAMCE(19,IGAM)
              RW(JPGAC+JPGAER) = GAMVE(12,IGAM)*GACORA(3,IGAM)
              RW(JPGAC+JPGATR) = GAMVE(17,IGAM)
              RW(JPGAC+JPGAPR) = GAMVE(18,IGAM)
              RW(JPGAC+JPGAEF) = GACORA(1,IGAM)
              RW(JPGAC+JPGAGC) = GACORA(2,IGAM)*COROV
              RW(JPGAC+JPGAZS) = GAMVE(4,IGAM)
C
C-   Fill Fake probability and flags
C
              CALL GAMFAK (IGAM,PFAKE)
              RW(JPGAC+JPGAPL) = PFAKE(1)
              RW(JPGAC+JPGAPH) = PFAKE(2)
              IW(JPGAC+JPGAPN) = INT(PFAKE(4))
              IW(JPGAC+JPGAFA) = INT( PFAKE(3))
              IW(JPGAC+JPGAPE) = IPECO
              NSTEG = NNSTGA(IGAM)
              DO ISTEG = 1,NSTEG
                JEGPR = KROW(KEGPR,ISTEG+INRES)
                IW(JEGPR+JEGPGR) = IGAM+NGAM
                IW(JEGPR+JEGPPR) = LGASTO(IGAM,ISTEG)
              ENDDO
              INRES = INRES + NSTEG
              IW(KEGPR+LMHROW) = IW(KEGPR+LMHROW)+NSTEG
            ENDDO
            NGAM = NGAM + NNGA
            IW(KPGAC+LMHROW) = IW(KPGAC+LMHROW)+NNGA
          ENDIF
        ENDIF
      ENDDO
      CALL AUBPRS('PGACEGPR')
C
  998 RETURN
      END
