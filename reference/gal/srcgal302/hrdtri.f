      SUBROUTINE HRDTRI (LDBAS,IRUN,IFLAG)
C -------------------------------------------------------------------
C - G.Catanesi - 871007 -                    F.Ranjard - 880203
C! load /HCTRIG/ with DB trigger banks
C- input arguments:
C        LDBAS    : data base logical unit#
C        IRUN     : run number
C - output argument:
C        IFLAG     : return flag ( = ALGTDB(LDBAS,list,IRUN) )
C
C -----------------------------------------------------
      PARAMETER (LPHCTR= 12,LPHCTF=24,LHCRT=62)
      COMMON /HCTRIG/ NHCEPR,NHCBPR,NHCETR,NHCBTR , IHCTRG(LHCRT),NHCBTS
     +(LHCRT),NHCETS(LHCRT) , IYHCFI(LPHCTF),IXHCFI(LPHCTR),IXHCSE
     +(LPHCTR) , MHCETR,MHCBTR

      PARAMETER(JHTDTA=1,JHTDED=2,LHTDIA=2)
      PARAMETER(JHTHSA=1,JHTHPT=2,JHTHTT=3,JHTHED=4,LHTHTA=4)
      PARAMETER(JHWHTA=1,LHWHTA=1)
      PARAMETER(JHWDCA=1,LHWDIA=1)
      PARAMETER(JHTTED=1,LHTTRA=1)
      PARAMETER(JHWTNO=1,LHWTRA=1)
      PARAMETER(JHCTID=1,JHCTVR=2,JHCTEP=4,JHCTBP=5,JHCTET=6,JHCTBT=7,
     +          JHCTTM=8,LHCTGA=193)
      PARAMETER(JHSBID=1,JHSBVR=2,JHSBEN=4,JHSBSN=5,JHSBMT=6,JHSBXS=7,
     +          JHSBRS=10,JHSBPM=13,JHSBHB=14,LHSBAA=14)
      PARAMETER(LHSBXS=3,LHSBRS=3)
      PARAMETER(JHSEID=1,JHSEVR=2,JHSEFN=4,JHSESN=5,JHSESL=6,JHSEMT=7,
     +          JHSEXS=8,JHSERS=11,JHSEPM=14,JHSEHE=15,LHSECA=15)
      PARAMETER(LHSEXS=3,LHSERS=3)
      PARAMETER(JHTRID=1,JHTRVR=2,JHTRNB=4,JHTRLE=5,JHTRUE=6,JHTRSR=7,
     +          LHTREA=106)
      PARAMETER(JHCOTA=1,JHCOTS=2,JHCOOL=3,JHCOMT=4,JHCOEA=5,JHCOEF=6,
     +          JHCOEB=7,JHCOCF=8,JHCOIF=9,JHCOMA=10,LHCOSA=10)
      INTEGER LMHLEN, LMHCOL, LMHROW
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2)
C
      COMMON /BCS/   IW(1000)
      INTEGER IW
      REAL RW(1000)
      EQUIVALENCE (RW(1),IW(1))
C
      INTEGER ALGTDB
      EXTERNAL ALGTDB
      CHARACTER*12 LISTT
      DATA LISTT /'HCTGHSBAHSEC'/
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
C -----------------------------------------------------------------
C
C  If the trigger constants are required loads the banks
C    from the D.A file ADBSCOMB.DAF (unit 4) to the
C            HCTRIG common
C
      IFLAG=ALGTDB(LDBAS,LISTT,IRUN)
      IF(IFLAG.EQ.0)RETURN
C
C      Get index for Trigger banks
C
      JHCTG = IW(NAMIND( 'HCTG'))
      JHSBA = IW(NAMIND( 'HSBA'))
      JHSEC = IW(NAMIND( 'HSEC'))
C
      IF(JHCTG.NE.0)THEN
C
C  Store the content of the HCTG bank
C
         NHCEPR = ITABL(JHCTG,1,JHCTEP)
         NHCBPR = ITABL(JHCTG,1,JHCTBP)
         NHCETR = ITABL(JHCTG,1,JHCTET)
         NHCBTR = ITABL(JHCTG,1,JHCTBT)
C
         KHCTG = JHCTG + LMHLEN + JHCTTM - 1
         DO 10 J=1,LHCRT
            IHCTRG(J) = IW(KHCTG+1)
            NHCBTS(J) = IW(KHCTG+2)
            NHCETS(J) = IW(KHCTG+3)
            KHCTG = KHCTG + 3
   10    CONTINUE
C
C
C  Trigger quantities used in Galeph
C
         MHCETR = (NHCEPR-1) * NHCETR
         MHCBTR = (NHCBPR-1) * NHCBTR
C
      ENDIF
C
C Store the content of a part of HSBA bank
C
      IF(JHSBA.NE.0)THEN
C
         LHSBA = LCOLS(JHSBA)
         NHSBA = LROWS(JHSBA)
         DO 20 J=1,NHSBA
            IYHCFI(J) = ITABL(JHSBA,J,JHSBEN)
   20    CONTINUE
C
      ENDIF
C
C Store the content of a part of HSEC bank
C
      IF(JHSEC.NE.0)THEN
C
         LHSEC = LCOLS(JHSEC)
         NHSEC = LROWS(JHSEC)
         DO 30 J=1,NHSEC
            IXHCFI(J) = ITABL(JHSEC,J,JHSEFN)
            IXHCSE(J) = ITABL(JHSEC,J,JHSESN)
   30    CONTINUE
C
      ENDIF
      END
