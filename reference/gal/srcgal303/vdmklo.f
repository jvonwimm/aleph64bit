      SUBROUTINE VDMKLO
C!----------------------------------------------------------------------
C! Compute local coordinates produced by a hit
CKEY VDET DIGITIZE
C!
C!
C!  Author         Manoj Thulasidas 15-Dec-1993
C!
C!  Description
C!  ===========
C!  VDet MaKe LOCal hits will perform a global to local to transformatio
C!  of the hit positions (using the GEOM package routing VDGTOL), create
C!  the local info bank and sorts it so that all the hit modules and waf
C!  are in a sequence.
C!
C!
C! Input :  VDSS bank, Global hits bank, indexed by geant track #
C!
C! Output : VDLH bank, Local hits bank.
C!
C-----------------------------------------------------------------------
C
      PARAMETER(JVDLXI=1,JVDLXO=4,JVDLMO=7,JVDLWA=8,JVDLCO=9,JVDLER=10,
     +          JVDLTR=11,JVDLHT=12,LVDLHA=12)
      INTEGER JVDSTN, JVDSLN, JVDSPN, JVDSXE, JVDSYE, JVDSZE,
     $   JVDSXL, JVDSYL, JVDSZL, JVDSER, JVDSRN, JVDSES, LVDSSA
      PARAMETER(JVDSTN=1,JVDSLN=2,JVDSPN=3,JVDSXE=4,JVDSYE=5,JVDSZE=6,
     $   JVDSXL=7,JVDSYL=8,JVDSZL=9,JVDSER=10,JVDSRN=11,JVDSES=12,
     $   LVDSSA=12)
      INTEGER LMHLEN, LMHCOL, LMHROW
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2)
C
      COMMON /BCS/   IW(1000)
      INTEGER IW
      REAL RW(1000)
      EQUIVALENCE (RW(1),IW(1))
C
C
      INTEGER VDGTOL
C-- The maximum number of tracks allowed
      INTEGER MAXTRK
      INTEGER NAMIND, NAVDLH, KVDLH, IGARB, KVDST, IVDST, ITR,
     $   ILAY, IPHI, NENT, IENT, I, NVDST, IMOD(2), IWAF(2), IER,
     $   INDNXT, INDF
      REAL XIN(3), XOUT(3), EREL(2), ALIN(3,2), ALOU(3,2), VDIST,
     $   DIST1, DIST2
      EXTERNAL VDIST
C
      DATA NAVDSS, NAVDLH /2*0/
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
      IF (NAVDSS.EQ.0) THEN
         NAVDSS = NAMIND('VDSS')
         NAVDLH = NAMIND('VDLH')
      ENDIF
C
C-- first drop the VDLH (local hits) bank
      CALL BDROP(IW,'VDLH')
C
C-- find the VDSS bank.
      KVDSS = IW(NAVDSS)
      IF (KVDSS.EQ.0) RETURN
C
C-- lenght of VDSS
      NVDSS = LROWS(KVDSS)
C
C-- no point in proceeding if the VDSS bank is empty.
      IF (NVDSS.LE.0) RETURN
C
C--  create the VDLH bank
C--  assume that the length is going to be VDSS size
      MAXTRK = NVDSS
      IF (IW(NAVDLH).EQ.0) THEN
        CALL ALBOS('VDLH', 0, MAXTRK*LVDLHA+LMHLEN, KVDLH, IGARB)
        IF(IGARB.EQ.1) KVDSS = IW(NAVDSS)
        IW(KVDLH+LMHROW) = 0
        IW(KVDLH+LMHCOL) = LVDLHA
      ENDIF
      KVDLH = IW(NAVDLH)

      DO 80 IVDSS = 1, NVDSS
C--  Transport hit bank into readable local variables.
        ITR = ITABL(KVDSS, IVDSS, JVDSTN)
        ILAY = ITABL(KVDSS, IVDSS, JVDSLN)
        IPHI = ITABL(KVDSS, IVDSS, JVDSPN)
        XIN(1) = RTABL(KVDSS, IVDSS, JVDSXE)
        XIN(2) = RTABL(KVDSS, IVDSS, JVDSYE)
        XIN(3) = RTABL(KVDSS, IVDSS, JVDSZE)
        XOUT(1) = RTABL(KVDSS, IVDSS, JVDSXL)
        XOUT(2) = RTABL(KVDSS, IVDSS, JVDSYL)
        XOUT(3) = RTABL(KVDSS, IVDSS, JVDSZL)
        EREL(1) = RTABL(KVDSS, IVDSS, JVDSER)
C--  do the global to local trans (see comments in the beginning)
C--    VDGTOL(GIN, GOUT, ILAY, IPHI, NENT, IMOD, IWAF, ALIN, ALOU)
C--  where,
C--   inputs:  GIN(3), GOUT(3) are two 3 vectors,
C--             entry and exit points of a track (global coords)
C--           IPHI, ILAY - specifies the face.
C--   outputs: NENT = 1 or 2, number of entries for the track
C--             (track can go through the boundary between two wafers)
C--            ALIN(3,2), ALOU(3,2) are two 3 vectors,
C--             entries and exits in local coords
C--            IMOD(2) is the module numbers, -24 to 24
C--            IWAF(2) is the wafer numbers, 1 to 3, 1 closest to XY pla
C--            IER  = 0  successful completion
C--                 = 1  track doesnt exit (GIN=GOUT in VDSS language)
C--                 = 2  ??
C--  protect against zero lenght tracks.
        IF (VDIST(XIN,XOUT,3).LE.1.0E-15) GOTO 80
        IER =  VDGTOL
     $     (XIN,XOUT,ILAY,IPHI,NENT,IMOD,IWAF,ALIN,ALOU)
C--  an error occurred, but
C--  the error is usually inocuous.
C-- The silly return code from VDGTOL is 1 for OK !
        IF (IER.NE.1) GOTO 80
C--  if there is not exactly one entry point
        IF(NENT.EQ.1) THEN
          INDNXT = KNEXT(KVDLH)
          DO 82 I = 1, 3
            RW(INDNXT+JVDLXI+I-1) = ALIN(I,1)
            RW(INDNXT+JVDLXO+I-1) = ALOU(I,1)
 82       CONTINUE
          IW(INDNXT+JVDLMO) = IMOD(1)
          IW(INDNXT+JVDLWA) = IWAF(1)
          IW(INDNXT+JVDLCO) = 1000*IMOD(1) + ISIGN(IWAF(1),IMOD(1))
          RW(INDNXT+JVDLER) = EREL(1)
          IW(INDNXT+JVDLTR) = ITR
          IW(INDNXT+JVDLHT) = ITABL(KVDSS, IVDSS, JVDSRN)
          IW(KVDLH+LMHROW) = IW(KVDLH+LMHROW) + 1
        ELSE
C--  there is a bug somewhere...
          CALL ALTELL('VDMKLO: Error in NENT', 0, 'RETURN')
        ENDIF
 80   CONTINUE
C
C--  compress the VDLH bank
      CALL AUBPRS('VDLH')
C
C--  sort the bank on the encoded WAFER address:  IW(KVDLH+JVDLCO)
C--  we are going to sort IW into itself, starting from the index of the
C--  row, first element, ending in the index of the last row, last eleme
C--  we treat it as a matrix, number of rows = LROWS, and columns = LCOL
      INDF = KROW(KVDLH,1) + 1
      CALL SORTIQ(IW(INDF),LCOLS(KVDLH),LROWS(KVDLH),JVDLCO)
      RETURN
      END
