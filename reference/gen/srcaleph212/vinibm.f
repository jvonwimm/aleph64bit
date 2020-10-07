      SUBROUTINE VINIBM(IRUN,IFLAG)
C ----------------------------------------------------------------
CKEY VDETDES INDEX / USER
C----------------------------------------------------------------------
C! Initialization of VDET geography and Bonding Maps
C!   Author   :- J.Rothberg                   13-OCT-1995
C!
C!   Inputs:
C!   IRUN   / I run number
C!
C!   Outputs:
C!   IFLAG  / I = 1 if subroutine ended successfully
C!              = 0 if an error occurred
C!                  in this case the program should STOP
C!   Libraries required:
C!
C!======================================================================
      IMPLICIT NONE
C -------------------------------------------------------
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
C Arguments
      INTEGER IRUN, IFLAG
C
C Local Variables
      INTEGER IRET, JMOD, ISMOD, IGET
      INTEGER LOUT, LUNDAF
C Functions
      INTEGER JUNIDB, VINIVG, GTSTUP, VSMJMD, VDARGE
C --------------------------------------------------------
      IFLAG = 0
      LOUT = 6
      LUNDAF = JUNIDB(0)
C
C     Get setup code and read the database banks if necessary:
C
      IGET = GTSTUP('VD',IRUN)
C
      IF (IGET .LE. 0) THEN
C       Something went wrong...
        GO TO 999
      ENDIF
C ------------------------------------------------------------------
C read data base
      IRET = VDARGE(LUNDAF,IGET)
      IF( IRET .NE. VDOK) THEN
         GOTO 999
      ENDIF
C ------------------------------------------------------------------
C initialize geography
      IRET = VINIVG()
      IF( IRET .NE. VDOK) THEN
         GOTO 999
      ENDIF
C --------------------------------------------------
C Banks VMBE,    bonding errors
C read unpack banks; store in  VMBU
        CALL VRVMBE
C
C store errors in arrays by global module number
       IRET = GTSTUP('VD',irun)
       DO JMOD = 1, NMODUL
         IRET = VSMJMD(JMOD,ISMOD)
         IF(IRET .EQ. VDOK) THEN
             CALL VGTBER(ISMOD,JMOD)
         ENDIF
C
       ENDDO
C ---------------------------------------------------------
       IFLAG = 1
C
  999 CONTINUE
      END
