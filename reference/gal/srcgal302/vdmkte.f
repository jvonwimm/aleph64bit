      SUBROUTINE VDMKTE(IVDLH,IVDHT)
C!----------------------------------------------------------------------
C! Make the small track elements for one track hit
CKEY VDET DIGITIZE
C!
C!
C!  Author         A. Bonissent 15-Jan-1994
C!
C!  Description
C!  ===========
C!  Call VDMKLA which will return array of track elements
C!  with their position and energy, Landau fluctuation included
C!
C!  fill the bank
C!
C! Input :  IVDLH  : row number in the VDLH bank which we process     I
C!          IVDHT  : VDHT hit          number                         I
C! Input :  VDLH bank
C!
C! Output : VDTE bank
C!
C-----------------------------------------------------------------------
C
      SAVE NAVDLH,NAVDTE
C
      DIMENSION XIN(3),XOUT(3)
      PARAMETER(NDIM=50)
C Vdet dimensions
      PARAMETER (RVDMIN=6.5, RVDMAX=11.2, ZVDMAX=20.)
C size of one track element (subdivision of a track inside silicon wafer
      PARAMETER (TRESIZ=0.0010)
C
      DIMENSION XEL(3,NDIM),EEL(NDIM)
      PARAMETER(JVDLXI=1,JVDLXO=4,JVDLMO=7,JVDLWA=8,JVDLCO=9,JVDLER=10,
     +          JVDLTR=11,JVDLHT=12,LVDLHA=12)
      PARAMETER(JVDTXB=1,JVDTRE=4,LVDTEA=4)
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
        NAVDTE=NAMIND('VDTE')
      ENDIF
      KVDLH=IW(NAVDLH)
      EREL  = RTABL(KVDLH,IVDLH,JVDLER)
C
C Get IVDHT just to return to caller (VDFIRW) to be passed
C  later to subsequent routines
C
      IVDHT = ITABL(KVDLH,IVDLH,JVDLHT)
C
C Get Geant track number to be used for Landau fluctuations
C   (need particle type)
C
      ITK = ITABL(KVDLH,IVDLH,JVDLTR)
      DO 11 IDIM=1,3
        XIN(IDIM)=RTABL(KVDLH,IVDLH,JVDLXI-1+IDIM)
        XOUT(IDIM)=RTABL(KVDLH,IVDLH,JVDLXO-1+IDIM)
 11   CONTINUE
      KVDTE=IW(NAVDTE)
      IF(KVDTE.EQ.0)THEN
C
C Create bank VDTE (track elements for one hit) with reasonable size
C
        NMAX = 2*VWTHCK()*SQRT(RVDMIN**2+ZVDMAX**2)/(RVDMIN*TRESIZ)
        NDATA = LMHLEN+LVDTEA*NMAX
        CALL ALBOS('VDTE',0,NDATA,KVDTE,IGARB)
      ENDIF
      IW(KVDTE+LMHROW)=0
      IW(KVDTE+LMHCOL)=LVDTEA
C
      CALL VDMKLA(ITK,XIN,XOUT,NDIM,EREL,XEL,EEL,NSTEP)
C
C Reset VDTE bank
C
      IW(KVDTE+LMHROW)=0
      NMAX = LFRROW(KVDTE)
      IF(NSTEP.GT.NMAX)THEN
        NDATA = LMHLEN+LVDTEA*NSTEP
        CALL ALBOS('VDTE',0,NDATA,KVDTE,IGARB)
        KVDLH=IW(NAVDLH)
      ENDIF
      IW(KVDTE+LMHROW)=NSTEP
      DO 13 ITE = 1,NSTEP
C
C Fill the bank
C
        KLINE = KROW(KVDTE,ITE)
        DO 15 IDIM=1,3
          RW(KLINE+JVDTXB-1+IDIM)=XEL(IDIM,ITE)
 15     CONTINUE
        RW(KLINE+JVDTRE)=EEL(ITE)
 13   CONTINUE
 999  CONTINUE
      RETURN
      END
