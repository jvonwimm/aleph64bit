      INTEGER FUNCTION VDARGE (LUNDAF,IGET)
C ----------------------------------------------------------------------
CKEY VDETDES READ DBASE / INTERNAL
C!  Read VDET geography banks from DAF
C     J. Rothberg      24 Oct. 1995
C - based on Steve Wasserbaech, January 1994
C   (Based on VRDDAF, G. Triggiani, 17/02/87.)
C   Modified 31 July 1995, S. Wasserbaech: add NWFBIT to VRDOCO
C
C  Called by: VINIVG
C  Calls:     ALGTDB
C
C - Input:
C   LUNDAF / I  Logical unit number of DAF file
C   IGET   / I  VDET setup code to be read
C
C - Output:
C   VDARGE / I  = VDOK if successful
C               = VDERR if an error occurred
C ----------------------------------------------------------------------
      IMPLICIT NONE
C!    Parameters for VDET geometry package
C ----------------------------------------------------------------------
C
C     Labels for return codes:
C
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
      INTEGER LMHLEN, LMHCOL, LMHROW
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2)
C
      COMMON /BCS/   IW(1000)
      INTEGER IW
      REAL RW(1000)
      EQUIVALENCE (RW(1),IW(1))
C
C     Arguments:
      INTEGER LUNDAF, IGET
C
C     Parameters:
C     List of VDET geography banks to read:
      CHARACTER*12 LIST1
      PARAMETER (LIST1 = 'VFMCVUECVXCH')
C
C     Local variables
      INTEGER LOUT, IRET, I
      INTEGER KVFMC, KVUEC, KVXCH, KVMBE, KVUBE
      INTEGER ILAY, IFAC, IMOD
      LOGICAL MISS
C
C     External functions:
      INTEGER ALGTDB, NAMIND, MDARD
      INTEGER LCOLS, LROWS, KROW, KNEXT, ITABL,LFRWRD, LFRROW
      REAL RTABL
      INTEGER ID, NRBOS, L
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
      VDARGE = VDERR
      LOUT = IW(6)
C
C     Initialize all VDET geometry commons:
C
C ----------------------------------------------------------------------
C
C     Read the banks from the DAF:
      IRET = ALGTDB(LUNDAF,LIST1,-IGET)
C
      IF (IRET .EQ. 0) THEN
C       Something went wrong...
        GO TO 1000
      ENDIF
C
C ----------------------------------------------------------------------
C
C     Get the indices to banks just read in:
C
      KVFMC = IW(NAMIND('VFMC'))
      KVUEC = IW(NAMIND('VUEC'))
      KVXCH = IW(NAMIND('VXCH'))
C
      MISS = (KVFMC .LE. 0) .OR. (KVUEC .LE. 0) .OR.
     >       (KVXCH .LE. 0)
C
      IF (MISS) THEN
        GO TO 1000
      ENDIF
C
C ----------------------------------------------------------------------
C Read bonding error banks; bank number is seial module number
      DO I =1,100
        IRET= MDARD(IW,LUNDAF,'VMBE',I)
      ENDDO
C ----------------------------------------------------------------------
C
C     Success!
C
      VDARGE = VDOK
C
 1000 CONTINUE
      RETURN
      END
