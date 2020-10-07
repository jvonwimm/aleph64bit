      SUBROUTINE MUFILL
C
C***********************************************************************
C
C T.Wang -860808
C
C! fill histograms for MUON
C
C       Called from MUDIGI            in this .HLB
C       Calls       HFILL             in      HBOOK
C
C***********************************************************************
C
      SAVE
      PARAMETER (KGWBK=69000,KGWRK=5200)
      COMMON /GCBANK/   NGZEBR,GVERSN,GZVERS,IGXSTO,IGXDIV,IGXCON,
     &                  GFENDQ(16),LGMAIN,LGR1,GWS(KGWBK)
      DIMENSION IGB(1),GB(1),LGB(8000),IGWS(1)
      EQUIVALENCE (GB(1),IGB(1),LGB(9)),(LGB(1),LGMAIN),(IGWS(1),GWS(1))
C
      COMMON/GCLINK/JGDIGI,JGDRAW,JGHEAD,JGHITS,JGKINE,JGMATE,JGPART
     +        ,JGROTM,JGRUN,JGSET,JGSTAK,JGGSTA,JGTMED,JGTRAC,JGVERT
     +        ,JGVOLU,JGXYZ,JGPAR,JGPAR2,JGSKLT
C
      INTEGER LMHLEN, LMHCOL, LMHROW
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2)
C
      COMMON /BCS/   IW(1000)
      INTEGER IW
      REAL RW(1000)
      EQUIVALENCE (RW(1),IW(1))
C
      PARAMETER (NHTIN = 40 , INCHT = 10 , NAVTD = 4)
      COMMON/MUNAMC/ NAMUHT,NAMUDI,NAMUDT,NAMUTD,NAMUDG
     +             , NAMUOG,NAMBAG,NAMBSG,NAMBTG,NAMBBG
     +             , NAMECG,NAMEBG,NAMETG,NAMESG
     +             , NAMMAG,NAMMBG,NAMMTG,NAMMSG
     &             , JDMUST,JDMTHT
C
      PARAMETER (MXPHT = 200, MXDHT =  40, MXEMD = 94)
      PARAMETER (MXTRK = 100, MXTDI = 100)
C
      PARAMETER(JMUHTN=1,JMUHEL=2,JMUHSP=3,JMUHSA=4,LMUHTA=4)
      PARAMETER(JMUDEA=1,LMUDGA=1)
      INTEGER IEMD(2,MXEMD),ITRK(2,MXTRK)
      PARAMETER (NB1=1,NB2=8,NB3=25,NB4=1)
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
      JMUDG = IW(NAMUDG)
      IF( JMUDG .EQ. 0 )GOTO 900
      NDIGI = LROWS(JMUDG)
      KMUDG = JMUDG+LMHLEN
C
C       # of digits / event
C
      X = FLOAT(NDIGI)
      CALL HFILL(814,X)
C
      NEMD = 0
      DO 20 IDG=1,NDIGI
C
      IWORD = IW(KMUDG+IDG)
      CALL CBYT(IWORD,NB3,IMD,NB1,NB2)
      CALL CBYT(IWORD,NB4,JCL,NB1,NB2)
C       Length of clusters
C
      X = FLOAT(JCL)
      CALL HFILL(821,X)
 20   CONTINUE
C
      KMTD1 = IW(NAMUTD)
      KMTD2 = IW(KMTD1-1)
      NTRK = LROWS(KMTD1)
C
C       # of tracks that generate MUON digits
C
      X = NTRK
      CALL HFILL(841,X)
C
      DO 40 IT=1,NTRK
C
C       # of digits / track
C
      X = ITABL(KMTD2,IT,1)
      CALL HFILL(812,X)
   40 CONTINUE
C
       JMUHT = IW(NAMUHT)
C
C       # of hits / event
C
      NHIT = LROWS( JMUHT)
      X = NHIT
      CALL HFILL(813,X)
C
      NTRK = 0
      NEMD = 0
      DO 70 IH=1,NHIT
      ITR = ITABL( JMUHT,IH,JMUHTN)
      IMD = ITABL( JMUHT,IH,JMUHEL)
      IF(IMD.GT.100.AND.IMD.LT.200) IMD = IMD - 100 + 38
      IF(IMD.GT.200) IMD = IMD - 200 + 54
      IF( NTRK .EQ. 0 )THEN
        NTRK = 1
        ITRK(1,1) = ITR
        ITRK(2,1) = 1
      ELSE
        DO 50 IT=1,NTRK
        IF( ITRK(1,IT) .NE. ITR )GOTO 50
        ITRK(2,IT) = ITRK(2,IT) + 1
        GOTO 55
   50   CONTINUE
        NTRK = NTRK + 1
        ITRK(1,NTRK) = ITR
        ITRK(2,NTRK) = 1
   55   CONTINUE
      ENDIF
C
      IF( NEMD .EQ. 0 )THEN
         NEMD = 1
         IEMD(1,1) = IMD
         IEMD(2,1) = 1
      ELSE
         DO 60 IE=1,NEMD
         IF( IEMD(1,IE) .NE. IMD )GOTO 60
         IEMD(2,IE) = IEMD(2,IE) + 1
         GOTO 65
   60    CONTINUE
         NEMD = NEMD + 1
         IEMD(1,NEMD) = IMD
         IEMD(2,NEMD) = 1
   65    CONTINUE
      ENDIF
   70 CONTINUE
C
C       # of hits / track
C
      DO 80 IT=1,NTRK
      X = ITRK(2,IT)
      CALL HFILL(811,X)
   80 CONTINUE
C
C       # of hits / module
C
      DO 90 IE=1,NEMD
      X = IEMD(1,IE)
      W = IEMD(2,IE)
      CALL HFILL(831,X,0.,W)
   90 CONTINUE
C
  900 RETURN
      END
