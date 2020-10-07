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
      DIMENSION XIN(3),XOUT(3),PRJ(3),TMP(3)
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
      DATA NAAVDLH, NAVDTE /2*0/
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
      IF (NAVDLH.EQ.0) THEN
        NAVDLH=NAMIND('VDLH')
        NAVDTE=NAMIND('VDTE')
      ENDIF
      KVDLH=IW(NAVDLH)
      IF(KVDLH.LE.0) RETURN
      EREL  = RTABL(KVDLH,IVDLH,JVDLER)
C
C Get IVDHT just to return to caller (VDFIRW) to be passed
C  later to subsequent routines
C
      IVDHT = ITABL(KVDLH,IVDLH,JVDLHT)
C
      DO 11 IDIM=1,3
        XIN(IDIM)=RTABL(KVDLH,IVDLH,JVDLXI-1+IDIM)
        XOUT(IDIM)=RTABL(KVDLH,IVDLH,JVDLXO-1+IDIM)
 11   CONTINUE
      KVDTE=IW(NAVDTE)
      IF(KVDTE.LE.0)THEN
C
C Create bank VDTE (track elements for one hit) with reasonable size
C
        NMAX = 2*VWTHCK()*SQRT(RVDMIN**2+ZVDMAX**2)/(RVDMIN*TRESIZ)
        NDATA = LMHLEN+LVDTEA*NMAX
        CALL ALBOS('VDTE',0,NDATA,KVDTE,IGARB)
        IF(IGARB.EQ.1) KVDLH=IW(NAVDLH)
      ENDIF
      IW(KVDTE+LMHROW)=0
      IW(KVDTE+LMHCOL)=LVDTEA
C
      IF (EREL.GT..0)THEN
C-- the length of the track in silicon
       RLTOT = VDIST(XIN,XOUT,3)
C-- ideal leght of the small track elements
       RLEN = 10.0E-4
C-- PRJ is a vector pointing from xin to xout
       CALL VSUB(XOUT,XIN,PRJ,3)
C-- first element  = r-phi proj,
C-- second ellemet = z proj
C-- which projection is larger, rphi or z?
       RPRJ = MAX(ABS(PRJ(1)),ABS(PRJ(2)))
C-- number of steps, at least 1
       NSTEP = INT(RPRJ/RLEN) + 1
C-- at most NDIM
       NSTEP = MIN(NDIM,NSTEP)
C-- the step size, close to (and smaller than) RLEN, but a variable
       STEP = RLTOT/REAL(NSTEP)
C-- PRJ now is the increment vector
       CALL VSCALE(PRJ,1.0/REAL(NSTEP),PRJ,3)
C--  compute the positions of the track elements
       CALL VSCALE(PRJ,0.5,TMP,3)
       CALL VADD(XIN,TMP,XEL(1,1),3)
       DO 12 K = 2, NSTEP
        CALL VADD(XEL(1,K-1),PRJ,XEL(1,K),3)
 12    CONTINUE
       CALL VFILL(EEL, NSTEP, EREL/REAL(NSTEP))
      ENDIF
C
C Reset VDTE bank
C
      IW(KVDTE+LMHROW)=0
      NMAX = LFRROW(KVDTE)
      IF(NSTEP.GT.NMAX)THEN
        NDATA = LMHLEN+LVDTEA*NSTEP
        CALL ALBOS('VDTE',0,NDATA,KVDTE,IGARB)
        IF (IGARB.EQ.1) KVDLH=IW(NAVDLH)
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
      RETURN
      END
