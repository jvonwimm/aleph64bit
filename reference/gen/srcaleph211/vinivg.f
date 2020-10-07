      INTEGER FUNCTION VINIVG()
C ----------------------------------------------------------------------
CKEY VDETDES INDEX / USER
C! initialize VDET geography package, unconnected channels, face/module
C
C  VZGEOG.FOR
C       extensions to VDET geometry package        21 Sept. 1995
C                                                   J. Rothberg
C                               modified  6 Oct. 1995  for VEXCH
C ---------------------------------------------------------------------
C - Joe Rothberg, August 20 1995
C ---------------------------------------------------------------------
C     IMPLICIT NONE
C ---------------------------------------------------------------------
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
C!    Common for VSLT data: VDET slots
C ----------------------------------------------------------------------
      INTEGER NSLOTS, JJLAYF, ISSFLG
      REAL PHIOFF
C
      COMMON / VSLTCO / NSLOTS, JJLAYF(NVFMAX), PHIOFF(NVFMAX),
     >                  ISSFLG(NVFMAX)
C
      INTEGER LMHLEN, LMHCOL, LMHROW
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2)
C
      COMMON /BCS/   IW(1000)
      INTEGER IW
      REAL RW(1000)
      EQUIVALENCE (RW(1),IW(1))
C
C ---------------------------------------------------------------------
      INTEGER NAMI, NAMIND, GTSTUP
      INTEGER i, nrows, irow
C
      INTEGER KVUEC, KVXCH, KVUBE, KVFMC
      INTEGER version
      CHARACTER*4 CHAINT
      INTEGER LCOLS, LROWS, KROW, KNEXT, ITABL,LFRWRD, LFRROW
      REAL RTABL
      INTEGER ID, NRBOS, L
C --------------------------------------------------------------
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
C ---------------------------------------------------------------------
       VINIVG = VDERR
C
C Bank VUEC, disconnected channels
       nami = NAMIND('VUEC')
       KVUEC = IW(nami)
       IF(KVUEC .EQ. 0)THEN
         GOTO 999
       ENDIF
C
       vuech(1)  = ITABL(kvuec,1,1)
       vuech(2)  = ITABL(kvuec,1,2)
C
C Bank VXCH, extra readout channels
       nami = NAMIND('VXCH')
       KVXCH = IW(nami)
       IF(KVXCH .EQ. 0)THEN
         GOTO 999
       ENDIF
C
       vexch(1)  = ITABL(kvxch,1,1)
       vexch(2)  = ITABL(kvxch,1,2)
C --------------------------------------
C Flag to ignore bonding maps
C    VIGBM=1 ignore bonding map
       VIGBM = 0
       nami = NAMIND('VUBE')
       KVUBE = IW(nami)
C
       IF(KVUBE .EQ. 0)THEN
       ELSE
          VIGBM  = ITABL(KVUBE,1,1)
       ENDIF
C notify that bond map is not being used
       IF (VIGBM.EQ.1 .AND. IW(6).GT.0) THEN
         WRITE(IW(6),*)' VINIVG: bond map ignored'
       ENDIF
C
C ----------------------------------------------------------
C Bank VFMC, face module content
       nami = NAMIND('VFMC')
       KVFMC = IW(nami)
       IF(KVFMC .EQ. 0)THEN
         GOTO 999
       ENDIF
C
       nrows = LROWS(kvfmc)
       DO i = 1, nrows
        facec(i)  = CHAINT(ITABL(kvfmc,i,1))
        facen(i)  = ITABL(kvfmc,i,2)
        modneg(i) = ITABL(kvfmc,i,3)
        modpos(i) = ITABL(kvfmc,i,4)
      ENDDO
C
      VINIVG = VDOK

C error return
 999  CONTINUE
      RETURN
      END
