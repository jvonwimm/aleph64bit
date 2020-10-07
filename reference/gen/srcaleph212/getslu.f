      SUBROUTINE GETSLU(IRUN,IFOUN,ILUQR,NBHA,XLUSI,BAK,EWCOR)
C----------------------------------------------------------------------
C! Gets the SICAL Luminosity for run IRUN
C  Author  J.Boucrot  16-Mar-1993
C  Modified 19-May-1993 to get informations from Sical bank 'SLUM'
CKEY ALEF LFIL
C Input argument :
C  IRUN = run number to be searched in bank 'RSLU'
C Output arguments :
C   IFOUN = 0 if no information found for run IRUN
C         = 1 if information found in bank 'RSLU' of ADBSCONS DAF
C         = 2 if information found in run header bank 'SLUM'
C
C The following output arguments are defined ONLY if IFOUN.GT.0  :
C   ILUQR = SICAL Flag  for Luminosity  :
C           0 = unknown     1 = provisional value   2 = definitive value
C    NBHA = Number of SICAL Bhabha events
C  XLUSI  = Best estimate of lumi from SICAL , in nb**-1
C The following quantities are defined ONLY if IFOUN = 1
C    BAK  = background Bhabha events
C  EWCOR  = Electroweak correction
C
C Description:
C gets the SICAL LUMI informations from the 'RSLU' bank.
C----------------------------------------------------------------------
      INTEGER LMHLEN, LMHCOL, LMHROW
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2)
C
      COMMON /BCS/   IW(1000)
      INTEGER IW
      REAL RW(1000)
      EQUIVALENCE (RW(1),IW(1))
C
      PARAMETER(JRSLRN=1,JRSLLQ=2,JRSLNB=3,JRSLLU=4,JRSLBK=5,JRSLEW=6,
     +          LRSLUA=6)
      PARAMETER(JSLUSI=1,JSLUSU=2,JSLUDP=3,JSLUTI=4,JSLUTO=5,JSLULI=6,
     +          JSLULO=7,JSLUVR=8,JSLUHR=9,JSLULU=10,JSLULV=11,
     +          JSLULH=12,JSLUSE=13,JSLUSY=14,LSLUMA=14)
      INTEGER ALGTRO
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
C----------------------------------------------------------------------
      IFOUN = 0
      JRSLU = ALGTRO ('RSLU',IRUN,JRSLRN,JROWR)
      IF (JRSLU.NE.0 .AND. JROWR.GT.0) THEN
C  RSLU bank found : get the output arguments
         JRSLU = IABS (JRSLU)
         IFOUN = 1
         KRSLU = KROW(JRSLU,JROWR)
         ILUQR = IW(KRSLU+JRSLLQ)
         NBHA  = IW(KRSLU+JRSLNB)
         XLUSI = RW(KRSLU+JRSLLU)
         BAK   = RW(KRSLU+JRSLBK)
         EWCOR = RW(KRSLU+JRSLEW)
C  Nothing found in RSLU : try the run header bank 'SLUM' :
      ELSE
         JSLUM=NLINK('SLUM',IRUN)
         IF (JSLUM.GT.0) THEN
            IMETH=2
            NBHA = RTABL (JSLUM,IMETH,JSLUTI)
            XLUSI = RTABL (JSLUM,IMETH,JSLULU)
            ILUQR = 1
            BAK = 0.
            EWCOR = 0.
            IFOUN = 2
         ENDIF
      ENDIF
C
 999  RETURN
      END
