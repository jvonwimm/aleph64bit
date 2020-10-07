      INTEGER FUNCTION VSCHAN(IFLBE,JMOD,IVIEW,IDATC,ISTRSB,IFLTC)
C ---------------------------------------------------------------
CKEY VDETDES INDEX / USER
C! Returns strip number in each wafer corresponding to Data Channel
C
C   Author   :- J. Rothberg                   12-OCT-1995
C
C   Inputs:
C   IFLBE   / I     0 = nominal strips;  1 = Bonding errors used
C   JMOD    / I     Global Module number
C   IVIEW   / I     View
C   IDATC   / I     Data Channel (CERN convention: 0,1,...)
C
C   Outputs:
C   ISTRSB(3)  /I    Strip numbers in each wafer (1,2,3...)
C   IFLTC(3)   /I    Fault code by wafer (same as VMBE convention)
C
C   Libraries required:
C
C   Description
C   ===========
C     Returns strip number in each wafer corresponding to Data Channel
C           Bonding errors are taken into account.
C
C======================================================================
      IMPLICIT NONE
C ----------------------------------------------------------------------
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
C!   VDET Unconnected, extra channels; Face-module content
C ------------------------------------------------------------
      INTEGER VUECH, VEXCH, VIGBM
      INTEGER MAXFACE
      PARAMETER(MAXFACE=40)
      CHARACTER*4 FACEC
      INTEGER FACEN,MODNEG,MODPOS
c
      COMMON/VDUEFC/VUECH(2),VEXCH(2),VIGBM,
     >      FACEN(MAXFACE),FACEC(MAXFACE),
     >      MODNEG(MAXFACE),MODPOS(MAXFACE)
C ---------------------------------------------------------------------
C Arguments
       INTEGER IFLBE, IVIEW, JMOD, IDATC, ISTRSB(3), IFLTC(3)
C Functions
       INTEGER VDCFLT, VDCWRS
C Local Variables
       INTEGER I, IWFRS(3), ISTRS(3)
       INTEGER IRET, IV
       INTEGER JMODLAST/0/
       SAVE JMODLAST
C -----------------------------------------------------------------
       VSCHAN = VDERR
       IF(JMOD .LE. 0 .OR. JMOD .GT. NMODUL) GOTO 999
       IF(IVIEW .LE. 0 .OR. IVIEW .GT. 2) GOTO 999
       IF(IDATC .LT. 0 .OR. IDATC .GT. 1023) GOTO 999
C
C    IF VIGBM =1 igore bond maps
       IF(IFLBE .EQ. 1 .AND. VIGBM .EQ. 0) THEN
C new module requested, initialize tables
         IF(JMOD .NE. JMODLAST) THEN
            CALL VINIST
            DO IV=1,2
               CALL VCORMP(IV,JMOD)
            ENDDO
            JMODLAST = JMOD
         ENDIF
C
         IRET = VDCFLT(IDATC,IVIEW,ISTRSB,IFLTC)
         VSCHAN = IRET
C
         ELSEIF(IFLBE .EQ. 0 .OR. VIGBM .EQ. 1) THEN
C nominal wafers, strips
            DO I= 1,3
                   IWFRS(I)=-1
                   ISTRSB(I)=-1
                   IFLTC(I) = 0
            ENDDO
C
            IRET = VDCWRS(IDATC,IVIEW,IWFRS,ISTRS)
            DO I = 1,3
               IF(IWFRS(I) .GT. 0) ISTRSB(IWFRS(I)) = ISTRS(I)
            ENDDO
C
            VSCHAN = IRET
         ENDIF
C -------------------------------------------------------------------
  999 RETURN
      END
