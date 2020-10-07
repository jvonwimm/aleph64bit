      SUBROUTINE MUDGTZ
C
C***********************************************************************
C
C T.Wang -860112
C
C           - modified for final dbase
C             by A. Antonelli, F. Bossi 1 July 89
C
C       Routine to digitize all signals ( track signals
C       and track uncorrelated noises) and fill digitization
C       information in BOS bank 'MUDG', fill digi-track cross reference
C       information in BOS bank 'MUDT' and 'MUTD'.
C
C       Called by MUDIGI
C       Calls ALBOS,MUFIDI,MUFLDT,MUFLTD       in this .HLB
C       Calls BKFMT,BLIST                      in BOS
C       Calls SORTZV                           in CERNLIB
C
C***********************************************************************
C
      SAVE
      PARAMETER (MXPHT = 200, MXDHT =  40, MXEMD = 94)
      PARAMETER (MXTRK = 100, MXTDI = 100)
C
      PARAMETER(JMUHTN=1,JMUHEL=2,JMUHSP=3,JMUHSA=4,LMUHTA=4)
      PARAMETER(JMUDEA=1,LMUDGA=1)
      PARAMETER(JMTHW1=1,JMTHW2=2,JMTHW3=3,LMTHTA=3)
      PARAMETER (NHTIN = 40 , INCHT = 10 , NAVTD = 4)
      COMMON/MUNAMC/ NAMUHT,NAMUDI,NAMUDT,NAMUTD,NAMUDG
     +             , NAMUOG,NAMBAG,NAMBSG,NAMBTG,NAMBBG
     +             , NAMECG,NAMEBG,NAMETG,NAMESG
     +             , NAMMAG,NAMMBG,NAMMTG,NAMMSG
     &             , JDMUST,JDMTHT
C
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
      INTEGER ISTP(MXPHT),IHIT(MXPHT)
      INTEGER INDX(MXPHT),IADHT(MXPHT),IDTRK(MXPHT)
      PARAMETER (NBAR=34,NECA=116,NEMA=200)
      PARAMETER (NAS1=100,NAS2=166)
C
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
C       Link to BOS bank 'MUHT'
C
      JMUHT = IW(NAMUHT)
      IF ( JMUHT .EQ. 0) RETURN
      IF (LROWS( JMUHT) .EQ. 0) RETURN
      NMUHT = LROWS( JMUHT)
C - Test the array length
      IF (NMUHT .GT. MXPHT) THEN
       CALL ALTELL('MUDGTZ: too many hits for local arrays',2,'RETURN')
       NMUHT = MXPHT
       IW(JMUHT+LMHROW) = NMUHT
      ENDIF
C
C       Create BOS bank  'MUDT'
C
      KMDT1 = IW(NAMUDT)
      IF( KMDT1 .EQ. 0 )THEN
         ND = NMUHT + LMHLEN
         CALL ALBOS ('MUDT',1,ND,KMDT1,IGARB)
         IW(KMDT1+1) = 1
         CALL BLIST(IW,'E+','MUDT')
         CALL ALBOS ('MUDT',2,ND,KMDT2,IGARB)
         IW(KMDT2+1) = 1
         CALL ALBOS ('MUDT',3,NMUHT*NAVTD+LMHLEN,KMDT3,IGARB)
         IW(KMDT3+1) = 1
      ENDIF
C
C       Create BOS bank 'MUDG and 'JDMTHT'
C
      JMUDG = IW(NAMUDG)
      IF( JMUDG .EQ. 0 )THEN
         ND = LMHLEN + NMUHT*LMUDGA
         CALL ALBOS ('MUDG',0,ND,JMUDG,IGARB)
         CALL BLIST(IW,'E+','MUDG')
         IW(JMUDG + LMHCOL) = LMUDGA
      ENDIF
C
      MXWRD = NMUHT*LMTHTA
      IF( JDMTHT .NE. 0 ) CALL WDROP(IW,JDMTHT)
      CALL WBANK(IW,JDMTHT,MXWRD+LMHLEN,*998)
      IW(JDMTHT-3) = INTCHA('MTHT')
      IW(JDMTHT+LMHCOL) = LMTHTA
C
C
C               Filling of temporary bank JDMTHT
C
      JMUHT = IW(NAMUHT)
      KMUHT = JMUHT + LMHLEN
      KMTHT = JDMTHT+ LMHLEN
      NHIT = 0
      DO 110 ISGNL = 1,NMUHT
         JEMD = IW(KMUHT + JMUHEL)
         JPLN = IW(KMUHT + JMUHSP)
         JSTP = IW(KMUHT + JMUHSA)
C
C      Find  bus channel and astros
C
C      Barrel
         IF(JEMD.LE.NBAR)
     &     CALL MUINCB(JEMD,JPLN,JSTP,NAST,NBUS,NSTP,IFLAG)
C
C      End-Cap
C
         IF(JEMD.GT.NBAR.AND.JEMD.LE.NECA)
     &     CALL MUINCE(JEMD,JPLN,JSTP,NAST,NBUS,NSTP,IFLAG)
C
C      Middle angle
C
         IF(JEMD.GT.NEMA)
     &     CALL MUINCM(JEMD,JPLN,JSTP,NAST,NBUS,NSTP,IFLAG)
         IF (IFLAG.EQ.0) GO TO 110
         NHIT = NHIT + 1
         IW(KMTHT+JMTHW1) = NAST
         IW(KMTHT+JMTHW2) = NBUS
         IW(KMTHT+JMTHW3) = NSTP
         KMTHT = KMTHT + LMTHTA
 110  KMUHT = KMUHT + LMUHTA
      IW(JDMTHT+LMHROW) = NHIT
C
C   Loop on Astros number
C
      DO 270 IEMD = NAS1,NAS2,2
      DO 260 IBUS = 0,25
C
C       Preset MHIT (# of hit in current plane) to 0
C
            MHIT = 0
C
C       Loop of signals in 'JDMTHT'
C
            KMTHT = JDMTHT + LMHLEN
            DO 210 ISGNL = 1,NHIT
               JEMD = IW(KMTHT + JMTHW1)
               IF( JEMD .NE. IEMD )GOTO 210
               JBUS = IW(KMTHT + JMTHW2)
               IF( JBUS .NE. IBUS )GOTO 210
               JSTP = IW(KMTHT + JMTHW3)
               MHIT = MHIT + 1
               ISTP(MHIT) = JSTP
               IHIT(MHIT) = ISGNL
 210        KMTHT = KMTHT + LMTHTA
C
            IF( MHIT .EQ.  0)GOTO 260
C
C       Sort array ISTP(*), ISTP(*) is not changed, the sorting
C       results are kept in INDX(*)
C
            CALL SORTZV(ISTP,INDX,MHIT,-1,0,0)
C
C       Find cluster
C
            LCLST = 1
            IFRST = ISTP(INDX(1))
            IOLST = IFRST
            NHTCL = 0
            DO 240 I=1,MHIT
               ID = INDX(I)
               IF( ID .EQ.  0)GOTO 240
               INWST = ISTP(ID)
               NJUMP = INWST - IOLST
               IF( NJUMP .EQ.  0)THEN
                  NHTCL = NHTCL + 1
                  IADHT(NHTCL) = IHIT(ID)
               ELSE IF( NJUMP .EQ.  1)THEN
                  NHTCL = NHTCL + 1
                  IADHT(NHTCL) = IHIT(ID)
                  LCLST = LCLST + 1
                  IF( LCLST .EQ. 8)THEN
                     J = I
 220                 IF( J .EQ. MHIT )GOTO 230
                     J = J + 1
                     ID = INDX(J)
                     INEXT = ISTP(ID)
                     IF( INEXT .EQ. INWST )THEN
                        NHTCL = NHTCL + 1
                        IADHT(NHTCL) = IHIT(ID)
                        INDX(J) = 0
                        GOTO 220
                     ELSE
                        CALL MUFLDI(IEMD,IBUS,LCLST,INWST)
                        CALL MUFLDT (LROWS(JMUDG),NHTCL,IADHT)
                        LCLST = 1
                        IFRST = INEXT
                        INWST = INEXT
                        NHTCL = 0
                     ENDIF
                  ENDIF
               ELSE
                  CALL MUFLDI(IEMD,IBUS,LCLST,IOLST)
                  CALL MUFLDT (LROWS(JMUDG),NHTCL,IADHT)
                  LCLST = 1
                  NHTCL = 1
                  IFRST = INWST
                  IADHT(1) = IHIT(ID)
               ENDIF
               IOLST = INWST
C
               IF( I .NE. MHIT )GOTO 240
 230           CALL MUFLDI(IEMD,IBUS,LCLST,IOLST)
               CALL MUFLDT (LROWS(JMUDG),NHTCL,IADHT)
 240        CONTINUE
 250        CONTINUE
 260     CONTINUE
 270   CONTINUE
C
C       Fill track-digi cross-reference BOS bank 'MUTD'
C
CCC   CALL MUFLTD
C
C
C
 998    CONTINUE
        RETURN
C
      END
