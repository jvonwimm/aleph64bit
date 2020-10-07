      SUBROUTINE VGTBER(ISMOD,JMOD)
C ----------------------------------------------------------------------
CKEY VDETDES INDEX / USER
C!     Access unpacked bonding error banks, place into commons
C
C - Joe Rothberg, August 1995
C
C - Input:
C   ISMOD   / I  Module serial number
C   JMOD    / I  Global module number
C
C - Output:
C--------------------------------------------------------------
      IMPLICIT NONE
C -------------------------------------------------------------
C Arguments
      INTEGER JMOD, ISMOD
      INTEGER LMHLEN, LMHCOL, LMHROW
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2)
C
      COMMON /BCS/   IW(1000)
      INTEGER IW
      REAL RW(1000)
      EQUIVALENCE (RW(1),IW(1))
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
C!    VDET common for bonding errors, peculiar channels
C ----------------------------------------------------------
C bonding errors
      INTEGER MAXERR
      PARAMETER(MAXERR=100)
      INTEGER MXMOD
      PARAMETER(MXMOD = 48)
      INTEGER NUMERR, IBNERR
      INTEGER LSVPCH, LVVPCH, LFVPCH
C
      COMMON/VBNDER/NUMERR(MXMOD,2),IBNERR(MXMOD,2,MAXERR,5),
     > LSVPCH(MXMOD,MAXERR),LVVPCH(MXMOD,MAXERR),
     >     LFVPCH(MXMOD,MAXERR)
C
C local variables
      INTEGER ie, j, ivview, nerr
      INTEGER  kvmbu, nvmbu
C
C -----------------------------------------------------------
      INTEGER NLINK
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
C ----------------------------------------------------------

C Bank VMBU, unpacked errors

        kvmbu  = 0
        kvmbu = NLINK('VMBU',ismod)

       IF(kvmbu .EQ. 0) THEN
         GOTO 999
       ENDIF
C
       nvmbu = LROWS(kvmbu)
C
       IF(nvmbu .EQ. 0) THEN
         GOTO 999
       ENDIF
C
       numerr(JMOD,1) = 0
       numerr(JMOD,2) = 0
C
       DO ie = 1, MIN(maxerr,nvmbu)
C
         ivview = ITABL(kvmbu,ie,1)
         numerr(JMOD,ivview) = numerr(JMOD,ivview) + 1
         nerr = numerr(JMOD,ivview)
         DO j = 2,6
            ibnerr(JMOD,ivview,nerr,j-1)=ITABL(kvmbu,ie,j)
         ENDDO
C
C convert address from IC/Pisa system to CERN DataChannels
         ibnerr(JMOD,ivview,nerr,1) =
     >        ibnerr(JMOD,ivview,nerr,1) + vexch(ivview)
         ibnerr(JMOD,ivview,nerr,2) =
     >        ibnerr(JMOD,ivview,nerr,2) + vexch(ivview)
C
       ENDDO
C
 999   CONTINUE
C
       RETURN
       END
