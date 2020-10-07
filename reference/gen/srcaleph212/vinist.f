       SUBROUTINE VINIST
C-----------------------------------------------------
CKEY VDETDES INDEX / USER
C
C!   initialize electronics channel-strip arrays
C     use connected electronics channels, bank VUEC, and VSTREG
C
C ---------------------------------------------------------------
      IMPLICIT NONE
C -------------------------------------------------
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
C! VDET Electronics channels arrays
C
C  electronics channels; 3 wafers; 3 wafer flags
C  index is electronics channel starting from 1
      INTEGER EFLAG
      PARAMETER(EFLAG=7)
      INTEGER IELCHP(1024,eflag)
      INTEGER IELCHZ(1024,eflag)
      COMMON/VELCHN/IELCHP, IELCHZ
C ------------------------------------------------
C -----------------------------
      INTEGER IVIEW
      INTEGER NREG, NDIM
      INTEGER ISRGS(8,3)
      INTEGER LMHLEN, LMHCOL, LMHROW
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2)
C
      COMMON /BCS/   IW(1000)
      INTEGER IW
      REAL RW(1000)
      EQUIVALENCE (RW(1),IW(1))
C
      INTEGER LCOLS, LROWS, KROW, KNEXT, ITABL,LFRWRD, LFRROW
      REAL RTABL
      INTEGER ID, NRBOS, L
      INTEGER nami, kvuec, nfrstz, nfrstp, IVIEWP, IVIEWZ
      INTEGER NR, I,j, ireg, IWAF, idisc
C ----------------------------------------------------------
      DATA IVIEWP, IVIEWZ /2,1/
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

C Bank VUEC, connected electronics channels,
       nfrstz = VUECH(1) + VEXCH(1)
       nfrstp = VUECH(2) + VEXCH(2)
C --------------------------------------------------------------
C r-phi view
      IVIEW = iviewp

C initialize full array to 'disconnected'
      DO I = 1,7
        DO IDISC = 1, 1024
          IELCHP(IDISC,I) = -1
        ENDDO
      ENDDO

      CALL VSTREG(IVIEW,NREG,NDIM,ISRGS)

      DO IREG = 1, NREG
         j = 0
         DO i = isrgs(1,IREG), isrgs(2,IREG)
            IELCHP(i+nfrstp-1, ISRGS(3,IREG)) = j + isrgs(4,IREG)
            IELCHP(i+nfrstp-1, ISRGS(5,IREG)) = j + isrgs(6,IREG)
            IELCHP(i+nfrstp-1, ISRGS(7,IREG)) = j + isrgs(8,IREG)
              j =  j + 1
         ENDDO
      ENDDO
C ---------------------------------------------------------------------
C z view
      IVIEW = iviewz

C initialize full array to 'disconnected'
      DO IWAF = 1,7
        DO IDISC = 1, 1024
          IELCHZ(IDISC,IWAF) = -1
        ENDDO
      ENDDO
C
C nominal regions
      CALL VSTREG(IVIEW,NREG,NDIM,ISRGS)
C
      DO IREG = 1, NREG
         j = 0
         DO i = isrgs(1,IREG), isrgs(2,IREG)
            IELCHZ(i+nfrstz-1, ISRGS(3,IREG)) = j + isrgs(4,IREG)
            IELCHZ(i+nfrstz-1, ISRGS(5,IREG)) = j + isrgs(6,IREG)
              j =  j + 1
         ENDDO
      ENDDO
C
 999  CONTINUE
C
      RETURN
      END
