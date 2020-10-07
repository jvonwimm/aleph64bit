      SUBROUTINE NVPECO(IPECO, ILINE, NLINE, NMAX, IERR)
C ------------------------------------------------------------------
CKEY PTOJ PECO PCPA / INTERNAL
C     Author : J.Carr            Date : 25- 3-91
C!    Navigate from PECO to PCPA
C     PECO <- PEHY <- PPRL -> PCPA
C     IPECO : number of the row in bank PECO
C     ILINE (NLINE) : list of the rows corresponding to IPECO in bank PC
C     IERR  =   0 : OK
C     IERR  =  98 : no solution
C     IERR  = -99 : NLINE > NMAX
C     IERR  = - 1 : error on banks
C
C ---------------------------------------------------------------
      LOGICAL FIRST
      SAVE FIRST, MPECO, MPEHY, MPPRL, MPCPA
      DATA FIRST /.TRUE./
C     Common defining the names of the columns in Fortran
      PARAMETER(JPECER=1,JPECE1=2,JPECE2=3,JPECTH=4,JPECPH=5,JPECEC=6,
     +          JPECKD=7,JPECCC=8,JPECRB=9,JPECPC=10,LPECOA=10)
      PARAMETER(JPEHBH=1,JPEHKT=2,JPEHPE=3,LPEHYA=3)
      PARAMETER(JPPRPC=1,JPPRPE=2,JPPRPH=3,JPPRPP=4,LPPRLA=4)
      PARAMETER(JPCPNA=1,JPCPEN=2,JPCPTE=3,JPCPFI=4,JPCPR1=5,JPCPR2=6,
     +          JPCPPC=7,LPCPAA=7)
C
      DIMENSION ILINE (*)
      INTEGER LMHLEN, LMHCOL, LMHROW
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2)
C
      COMMON /BCS/   IW(1000)
      INTEGER IW
      REAL RW(1000)
      EQUIVALENCE (RW(1),IW(1))
C
CD BMACRO  reduced set of intrinsic functions to handle BOS banks
      LROWS(ID)         = IW(ID+2)
      KROW (ID,NRBOS)   = ID + LMHLEN + IW(ID+1)*(NRBOS-1)
      ITABL(ID,NRBOS,L) = IW(ID+LMHLEN+(NRBOS-1)*IW(ID+1)+L)
      RTABL(ID,NRBOS,L) = RW(ID+LMHLEN+(NRBOS-1)*IW(ID+1)+L)
C ----------------------------------------------------------------
C     Initializations
C     Subroutine of type NAMIND
      IF (FIRST) THEN
         FIRST = .FALSE.
         MPECO = NAMIND ('PECO')
         MPEHY = NAMIND ('PEHY')
         MPPRL = NAMIND ('PPRL')
         MPCPA = NAMIND ('PCPA')
      END IF
      IERR  = 98
      NLINE = 0
C     Bank 1 : PECO
      KPECO  = IW (MPECO)
      KPEHY  = IW (MPEHY)
      KPPRL  = IW (MPPRL)
      KPCPA  = IW (MPCPA)
      IF(KPECO.EQ.0.OR.KPEHY.EQ.0.OR.KPPRL.EQ.0.OR.KPCPA.EQ.0)THEN
         IERR = -  1
         GO TO 999
      ENDIF
      LPECO = LROWS (KPECO)
      LPEHY = LROWS (KPEHY)
      LPPRL = LROWS (KPPRL)
      LPCPA = LROWS (KPCPA)
C     *** Reverse path from PECO to PEHY
      DO 10, JPEHY = 1, LPEHY
         JCOUR = ITABL (KPEHY ,JPEHY, JPEHPE)
         IF (IPECO .EQ. JCOUR) THEN
            IPEHY = JPEHY
C           *** Reverse path from PEHY to PPRL
            DO 11, JPPRL = 1, LPPRL
               JCOUR = ITABL (KPPRL ,JPPRL, JPPRPE)
               IF (IPEHY .EQ. JCOUR) THEN
                  IPPRL = JPPRL
C                 *** Direct path from PPRL to PCPA
                  IPCPA = ITABL (KPPRL, IPPRL, JPPRPC)
C                 Suppression of multiple paths
                  DO 5, J = 1, NLINE
                     IF(IPCPA.EQ.ILINE(J))GOTO 7
    5                CONTINUE
                  IF(NLINE.EQ.NMAX) THEN
                       IERR = -99
                       GOTO 999
                  ENDIF
                  NLINE = NLINE + 1
                  ILINE (NLINE) = IPCPA
                  IERR = 0
    7             CONTINUE
               END IF
   11          CONTINUE
         END IF
   10    CONTINUE
  999 RETURN
      END
