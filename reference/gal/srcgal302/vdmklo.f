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
C! Input :  VDHT bank, Global hits bank, indexed by geant track #
C!
C! Output : VDLH bank, Local hits bank.
C!
C-----------------------------------------------------------------------
C
      PARAMETER(JVDLXI=1,JVDLXO=4,JVDLMO=7,JVDLWA=8,JVDLCO=9,JVDLER=10,
     +          JVDLTR=11,JVDLHT=12,LVDLHA=12)
      INTEGER JVDHTN, JVDHLN, JVDHPN, JVDHXE, JVDHYE, JVDHZE,
     $   JVDHXL, JVDHYL, JVDHZL, JVDHER, LVDHTA
      PARAMETER(JVDHTN=1,JVDHLN=2,JVDHPN=3,JVDHXE=4,JVDHYE=5,JVDHZE=6,
     $   JVDHXL=7,JVDHYL=8,JVDHZL=9,JVDHER=10,LVDHTA=10)
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
      INTEGER NAMIND, NAVDLH, KVDLH, IGARB, KVDHT, IVDHT, ITR,
     $   ILAY, IPHI, NENT, IENT, I, NVDHT, IMOD(2), IWAF(2), IER,
     $   INDNXT, INDF
      REAL XIN(3), XOUT(3), EREL(2), ALIN(3,2), ALOU(3,2), VDIST,
     $   DIST1, DIST2
      EXTERNAL VDIST
      DATA NAVDHT /0/
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
C-- first drop the VDLH (local hits) bank
      CALL BDROP(IW,'VDLH')
C
C-- find the VDHT bank.
      IF (NAVDHT.EQ.0) NAVDHT = NAMIND('VDHT')
      KVDHT = IW(NAVDHT)
      IF (KVDHT.EQ.0) GOTO 99
C
C-- lenght of VDHT
      NVDHT = LROWS(KVDHT)
C
C-- no point in proceeding if the VDHT bank is empty.
      IF (NVDHT.LE.0) GOTO 99
C
C--  create the VDLH bank
C--  assume that the length is going to be twice VDHT size
      MAXTRK = 2*NVDHT
      NAVDLH = NAMIND('VDLH')
      IF (IW(NAVDLH).EQ.0) THEN
        CALL ALBOS('VDLH', 0, MAXTRK*LVDLHA+LMHLEN, KVDLH, IGARB)
        IW(KVDLH+LMHROW) = 0
        IW(KVDLH+LMHCOL) = LVDLHA
      ENDIF
      KVDLH = IW(NAVDLH)

      DO 80 IVDHT = 1, NVDHT
C--  Transport hit bank into readable local variables.
        ITR = ITABL(KVDHT, IVDHT, JVDHTN)
        ILAY = ITABL(KVDHT, IVDHT, JVDHLN)
        IPHI = ITABL(KVDHT, IVDHT, JVDHPN)
        XIN(1) = RTABL(KVDHT, IVDHT, JVDHXE)
        XIN(2) = RTABL(KVDHT, IVDHT, JVDHYE)
        XIN(3) = RTABL(KVDHT, IVDHT, JVDHZE)
        XOUT(1) = RTABL(KVDHT, IVDHT, JVDHXL)
        XOUT(2) = RTABL(KVDHT, IVDHT, JVDHYL)
        XOUT(3) = RTABL(KVDHT, IVDHT, JVDHZL)
        EREL(1) = RTABL(KVDHT, IVDHT, JVDHER)
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
C--                 = 1  track doesnt exit (GIN=GOUT in VDHT language)
C--                 = 2  ??
C--  protect against zero lenght tracks.
        IF (VDIST(XIN,XOUT,3).LE.1.0E-15) GOTO 80
        IER =  VDGTOL
     $     (XIN,XOUT,ILAY,IPHI,NENT,IMOD,IWAF,ALIN,ALOU)
C--  an error occurred, but
C--  the error is usually inocuous.
C-- The silly return code from VDGTOL is 1 for OK !
        IF (IER.NE.1) GOTO 80
C--  if there are two entry points (track goes through two wafers)
C--  one has to divide the energy in proportion to the track lengths.
        IF (NENT.EQ.2) THEN
          DIST1 = VDIST(ALIN(1,1), ALOU(1,1), 3)
          DIST2 = VDIST(ALIN(1,2), ALOU(1,2), 3)
C--  error if either of the dists = 0
          IF (DIST1*DIST2 .LE. 0.0) THEN
            CALL ALTELL('VDMKLO: Error in track lengths', 7, 'RETURN')
            GOTO 80
          ENDIF
          EREL(1) = EREL(1)*(DIST1/(DIST1+DIST2))
          EREL(2) = EREL(1)*DIST2/DIST1
        ENDIF
        DO 81 IENT = 1, NENT
          INDNXT = KNEXT(KVDLH)
          DO 82 I = 1, 3
            RW(INDNXT+JVDLXI+I-1) = ALIN(I,IENT)
            RW(INDNXT+JVDLXO+I-1) = ALOU(I,IENT)
 82       CONTINUE
          IW(INDNXT+JVDLMO) = IMOD(IENT)
          IW(INDNXT+JVDLWA) = IWAF(IENT)
          IW(INDNXT+JVDLCO) = 1000*IMOD(IENT) +
     $       ISIGN(IWAF(IENT),IMOD(IENT))
          RW(INDNXT+JVDLER) = EREL(IENT)
          IW(INDNXT+JVDLTR) = ITR
          IW(INDNXT+JVDLHT) = IVDHT
          IW(KVDLH+LMHROW) = IW(KVDLH+LMHROW) + 1
 81     CONTINUE
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
 99   RETURN
      END
