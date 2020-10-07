      SUBROUTINE VDFIRW(IFIRS,ILAST,IMOD,IWAF)
C!----------------------------------------------------------------------
C! Compute signal in fired strips for one wafer
CKEY VDET DIGITIZE
C!
C!  Author         A. Bonissent 15-Jan-1994
C!
C!  Description
C!  ===========
C!  For each row in VDLH bank,
C!        build the list of all track segments (routine VDMKTE)
C!        Then, call VDPRTE to process them.
C!        Then, go to next hit
C!
C! Input :  IFIRS : first row in bank VDLH for this wafer             I
C!          ILAST : last  row in bank VDLH for this wafer             I
C!          IMOD  : module number                                     I
C!          IWAF  : Wafer number                                      I
C!
C! Input :  VDLH bank
C!
C! Output : VWS1, VWS2 banks, created by VDPRTE
C!
C-----------------------------------------------------------------------
C
      SAVE NAVDLH,NAVWSX
C
      DIMENSION NAVWSX(2),MAXS(2)
      PARAMETER(JVWSSC=1,JVWSVT=2,LVWS1A=2)
      INTEGER LMHLEN, LMHCOL, LMHROW
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2)
C
      COMMON /BCS/   IW(1000)
      INTEGER IW
      REAL RW(1000)
      EQUIVALENCE (RW(1),IW(1))
C
      LOGICAL FIRST
      DATA FIRST /.TRUE./
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
      IF(FIRST)THEN
        FIRST=.FALSE.
        NAVDLH=NAMIND('VDLH')
        NAVWSX(1)=NAMIND('VWS1')
        NAVWSX(2)=NAMIND('VWS2')
      ENDIF
      KVDLH=IW(NAVDLH)
C
C Reset strips array
C
      DO 30 IV=1,2
      KBNK = IW(NAVWSX(IV))
      NDATA = IW(KBNK)-LMHLEN
      CALL VZERO(IW(KBNK+LMHLEN+1),NDATA)
 30   CONTINUE
C
C Loop over track elements for this wafer
C
      DO 10 IVDLH=IFIRS,ILAST
        NHITW = ILAST-IFIRS+1
C
C build list of small segments for this track element
C
        CALL VDMKTE(IVDLH,IVDHT)
C
C compute signal on strips caused by the track element
C
        CALL VDPRTE(IMOD,IWAF,IVDHT,NHITW)
 10   CONTINUE
      RETURN
      END
