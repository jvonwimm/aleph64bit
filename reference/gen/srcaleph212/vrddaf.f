      SUBROUTINE VRDDAF (LUNDAF,IRUN,IFLAG)
C ----------------------------------------------------------------------
CKEY VDETDES READ DBASE / USER
C!  Initialize VDET geometry commons
C - Steve Wasserbaech, January 1994
C   (Based on VRDDAF, G. Triggiani, 17/02/87.)
C
C   Initialize VDET geometry commons.  This subroutine calls VDAFRD to
C   read the geometry banks from the database/cards, VINDXI to fill the
C   lookup tables for index conversions, and VDETGI to calculate various
C   quantities which are derived from the database quantities.
C
C - Input:
C   LUNDAF / I  Logical unit number of DAF file
C   IRUN   / I  Run number
C
C - Output:
C   IFLAG  / I  = 1 if routine ended successfully
C               = 0 if an error occurred
C ----------------------------------------------------------------------
      INTEGER LMHLEN, LMHCOL, LMHROW
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2)
C
      COMMON /BCS/   IW(1000)
      INTEGER IW
      REAL RW(1000)
      EQUIVALENCE (RW(1),IW(1))
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
C!    Common for lookup tables for index manipulation
C ----------------------------------------------------------------------
      INTEGER IVSTUP, NMODUL, NWAFER, NFACEL, NWAFEM, JJFACM, JJMODW
      INTEGER JIFACF, JIMODM, JIWAFW, IJFACE, IJMODU, IJWAFR, JIWFFW
      INTEGER IJWFFR
      CHARACTER*4 TXMODU
C
      COMMON / VGINDX / IVSTUP, NMODUL, NWAFER, NFACEL(NVLAYR),
     >                  NWAFEM, JJFACM(NVMMAX), JJMODW(NVWMAX),
     >                  JIFACF(NVFMAX), JIMODM(NVMMAX),
     >                  JIWAFW(NVWMAX), IJFACE(NVLAYR,NVFLMX),
     >                  IJMODU(NVLAYR,NVFLMX,NVMODF),
     >                  IJWAFR(NVLAYR,NVFLMX,NVMODF,NVWMMX),
     >                  JIWFFW(NVWMAX), IJWFFR(NVLAYR,NVFLMX,NVWFMX),
     >                  TXMODU(NVLAYR,NVFMAX,NVMODF)
C
C
C     Arguments:
      INTEGER LUNDAF, IRUN, IFLAG
C
C     Local variables
      INTEGER LOUT, IGET, IRET
C
C     External functions:
      INTEGER GTSTUP, VDAFRD, VINDXI, VDETGI
C
C     LASTUP is the last setup number
C     successfully read from the database:
      INTEGER LASTUP
      DATA LASTUP / -1 /
C
C ----------------------------------------------------------------------
C
      IFLAG = 0
      LOUT = IW(6)
C
C     Get setup code and read the database banks if necessary:
C
      IGET = GTSTUP('VD',IRUN)
C
      IF (IGET .LE. 0) THEN
C       Something went wrong...
        WRITE (LOUT,'(1X,A,I10)')
     >        '+++VRDDAF+++  Invalid setup code: GTSTUP=', IGET
        GO TO 1000
      ENDIF
C
      IF (IGET .EQ. LASTUP) THEN
C       We need not do anything...
        IFLAG = 1
        GO TO 1000
      ENDIF
C
C ----------------------------------------------------------------------
C
C     Read the geometry banks from the DAF,
C     and fill the corresponding commons:
C
      IRET = VDAFRD(LUNDAF,IGET)
C
      IF (IRET .NE. VDOK) THEN
         JADBS = MDARD (IW,JUNIDB(0),'ADBS',0)
         IF (JADBS.GT.0) THEN
            IDB = IW(JADBS+LMHLEN+1)
            IF (IDB.LT.196) THEN
               WRITE (6,*) '  '
               WRITE (6,*) ' ===================================== '
               WRITE (6,*) ' +++VRDDAF+++ you are using DB # ',IDB,
     &         ' which does not contain new VDET geometry banks - ',
     &         ' please use DB version > 195'
               WRITE (6,*) ' ==================================== '
               WRITE (6,*) '  '
               GOTO 1000
            ENDIF
         ENDIF
         GOTO 1000
      ENDIF
C
C ----------------------------------------------------------------------
C
C     Fill the VGINDX common block.
C     This common contains lookup tables to be used
C     by the index conversion routines.
C
      IRET = VINDXI()
C
      IF (IRET .NE. VDOK) THEN
C       Something went wrong...
        WRITE (LOUT,'(1X,A,I5)')
     >        '+++VRDDAF+++  Error in VINDXI: rc=', IRET
        GO TO 1000
      ENDIF
C
C ----------------------------------------------------------------------
C
C     Fill the VDETGE common block.
C     This common contains various quantities calculated
C     from the database banks.
C
      IRET = VDETGI()
C
      IF (IRET .NE. VDOK) THEN
C       Something went wrong...
        WRITE (LOUT,'(1X,A,I5)')
     >        '+++VRDDAF+++  Error in VDETGI: rc=', IRET
        GO TO 1000
      ENDIF
C
C
C     Success!
C     Now that everything is OK, we can fill in
C     the setup code in VGINDX common:
C
      IVSTUP = IGET
      LASTUP = IGET
      WRITE (LOUT,'(1X,A,I4)')
     >    'Vrddaf>  VDET Geometry Package initialized for setup', IGET
      IFLAG = 1
C
 1000 CONTINUE
      RETURN
      END
