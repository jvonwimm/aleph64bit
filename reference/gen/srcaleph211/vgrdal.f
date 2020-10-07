      SUBROUTINE VGRDAL (LUN,IRUN,IFLAG)
C ----------------------------------------------------------------------
CKEY VDETDES GEOMETRY
C!  Read alignment banks and fill the VGPAAL common
C - Francesco Forti, 18 August 1990
C - Modified to use new geometry package, S. Wasserbaech, January 1995
C
C  Check existence of the alignment banks in the BOS common and their
C  validity range.
C  If they do not exist yet or are no longer valid try to load them
C  from the data base file. IFLAG is set to 0 if an error occurs.
C  The routine uses the alignment banks to compute the complete
C  transformation that goes from the wafer coordinate to the ALEPH
C  main coordinates and vice versa and stores it in the common VGPAAL.
C
C  Called by :    VDET initialisation routine
C  Calls     :    FUNCTION ALGTDB               from ALEPHLIB
C                 VGEXRO, VGCMTR, VGINTR        from ALEPHLIB
C
C - Input:
C   LUN   / I  Logical unit number of DAF file
C   IRUN  / I  Run number
C
C - Output:
C   IFLAG / I  = 1 if routine ends successfully;
C              = 0 if an error occurred
C ----------------------------------------------------------------------
C     IMPLICIT NONE
C!    Parameters for VDET geometry package
C ----------------------------------------------------------------------
C
C     Labels for return codes:
C
      INTEGER VDERR, VDOK
      PARAMETER (VDERR = -1)
      PARAMETER (VDOK  = 1)
C
C     Labels for views:
C
      INTEGER VVIEWZ, VVIEWP
      PARAMETER (VVIEWZ = 1)
      PARAMETER (VVIEWP = 2)
C
C     Fixed VDET geometry parameters:
C
      INTEGER NVLAYR, NVMODF, NVVIEW, NPROMM, IROMAX
      PARAMETER (NVLAYR = 2)
      PARAMETER (NVMODF = 2)
      PARAMETER (NVVIEW = 2)
      PARAMETER (NPROMM = 1)
      PARAMETER (IROMAX = 4)
C
C     Array dimensions:
C
      INTEGER NVWMMX, NVWFMX, NVFLMX, NVFMAX, NVMMAX, NVWMAX
      INTEGER NVZRMX, NVPRMX
      PARAMETER (NVWMMX = 3)
      PARAMETER (NVWFMX = NVWMMX*NVMODF)
      PARAMETER (NVFLMX = 15)
      PARAMETER (NVFMAX = 24)
      PARAMETER (NVMMAX = NVFMAX*NVMODF)
      PARAMETER (NVWMAX = NVFMAX*NVWFMX)
      PARAMETER (NVZRMX = NVFMAX*IROMAX)
      PARAMETER (NVPRMX = NVMMAX*NPROMM)
C
      INTEGER JVAGTR, JVAGRO, JVAGEM, LVAGBA
      PARAMETER (JVAGTR=1, JVAGRO=4, JVAGEM=7, LVAGBA=27)
      INTEGER JVALWI, JVALTR, JVALRO, JVALEM, LVALCA
      PARAMETER (JVALWI=1, JVALTR=2, JVALRO=5, JVALEM=8, LVALCA=28)
C! Parameters used in the alignment geometrical package
      INTEGER LVEMLN, LVTFOR, LVTEXP, JVTFTR, JVTFRO, JVTFEM,
     &        JVTETR, JVTERO, JVTEEM
      PARAMETER( JVTFTR=1, JVTFRO=4, JVTFEM=7,
     &           JVTETR=1, JVTERO=4, JVTEEM=13,
     &           LVEMLN=21, LVTFOR=LVEMLN+JVTFEM-1,
     &           LVTEXP=LVEMLN+JVTEEM-1 )
C
C     (Other dimension parameters are defined in VGLOBL.)
C
      REAL VTEXPD, VTEXPI, RFDIST
      COMMON /VGPAAL/ VTEXPD(LVTEXP,NVFLMX,NVWFMX,NVLAYR),
     &                VTEXPI(LVTEXP,NVFLMX,NVWFMX,NVLAYR),
     &                RFDIST(NVFLMX,NVWFMX,NVLAYR)
      INTEGER LMHLEN, LMHCOL, LMHROW
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2)
C
      COMMON /BCS/   IW(1000)
      INTEGER IW
      REAL RW(1000)
      EQUIVALENCE (RW(1),IW(1))
C
      INTEGER LUN, IRUN, IFLAG
      INTEGER ALGTDB,GTSTUP
      EXTERNAL ALGTDB,GTSTUP
      INTEGER I, IRO, ILAY, IWFF, IFAC, IVIEW, IND, NAMIND
      INTEGER VNRFAC
      INTEGER KLC, KGB, IFAIL
      INTEGER LCOLS, LROWS, KROW, KNEXT, ITABL,LFRWRD, LFRROW
      REAL RTABL
      INTEGER ID, NRBOS, L
C
      CHARACTER*8 LIST1
      SAVE LIST1
      DATA LIST1 /'VAGBVALC'/
C
C     Transformation structures to be used for intermediate results:
C
      REAL TELC(LVTEXP), TEGB(LVTEXP), TENO(LVTEXP), TE(LVTEXP)
C
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
C ----------------------------------------------------------------------
C
      IFLAG = 1
C
C! Get banks in LIST1 from DB depending on run and setup code
C
      IF (IRUN.LE.2000) THEN
         ITP = GTSTUP ('VD',IRUN)
      ELSE
         ITP = IRUN
      ENDIF
      IND = ALGTDB(LUN,LIST1,-ITP)
C
C     Return if banks have not been correctly accessed:
C
      IF (IND .EQ. 0) THEN
        IFLAG = 0
        GO TO 999
      ENDIF
C
C     Get the indices to banks just read in:
C
      KLC = IW(NAMIND('VALC'))
      KGB = IW(NAMIND('VAGB'))
      IF ((KLC .LE. 0) .OR. (KGB .LE. 0)) THEN
        IFLAG = 0
        GO TO 999
      ENDIF
C
C     Initialize the matrices to zero:
C
      CALL VZERO(VTEXPD,LVTEXP*NVFLMX*NVWFMX*NVLAYR)
      CALL VZERO(VTEXPI,LVTEXP*NVFLMX*NVWFMX*NVLAYR)
C
C     Construct the global transformation:
C
      CALL VGEXRO(1,RW(KROW(KGB,1)+JVAGTR),TEGB)
C
C     Loop on the rows of VALC to get transformation of each wafer:
C
      DO IRO=1,LROWS(KLC)
C
C     Decode the wafer index, and make sure this is a valid wafer:
C
        CALL VADEWA(ITABL(KLC,IRO,JVALWI),ILAY,IWFF,IFAC,IVIEW)
        IF (IFAC .LE. VNRFAC(ILAY)) THEN
C
C     Local alignment of the wafer:
C
          CALL VGEXRO(2,RW(KROW(KLC,IRO)+JVALTR),TELC)
C
C     Nominal transformation for the wafer:
C
          CALL VGGTNO(ILAY,IWFF,IFAC,TENO)
C
C     Compute the total transformation for the wafer:
C
          CALL VGCMTR(TELC,TENO,TE)
          CALL VGCMTR(TE,TEGB,VTEXPD(1,IFAC,IWFF,ILAY))
C
C     Compute the inverse transformation:
C
          CALL VGINTR(VTEXPD(1,IFAC,IWFF,ILAY),
     &                VTEXPI(1,IFAC,IWFF,ILAY),IFAIL)
          IF (IFAIL .NE. 0) THEN
            IFLAG = 0
          ENDIF
C
        ENDIF
      ENDDO
C
C     Calculate the distance to wafer midplanes:
C
      CALL VGCADI
C
C     Routine is terminated and flag has been set properly.
C
  999 CONTINUE
      RETURN
      END
