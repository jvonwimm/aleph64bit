      SUBROUTINE CMPLIS(LIST,NB)
C-----------------------------------------------------------------------
C!    Compress banks contained in list LIST
CKEY PRESS CMPLIS COMPRESS /USER
C!
C!    Author : D.Harvatis    APR 89
C!
C!    INPUT:
C!          - LIST  : (CHARACTER*1) list of banks to be compressed.
C!                    After the compression, the original names of the
C!                    banks that were compressed are removed from the
C!                    list, and the modified names are added.
C!                    The modified names are the same as the original
C!                    ones, only the first character is in lower case.
C!
C!    OUTPUT:
C!          - NB    : number of banks that were compressed
C!
C!    Calls : CMPBAN, NLIST, BLIST, BKFMT
C!
C!    Description :
C!
C!    This subroutine loops through all bank names contained
C!    in list LIST. The bank compression function CMPBAN is
C!    called for every bank found.
C-----------------------------------------------------------------------
#ifndef DOC
      SAVE
      INTEGER      CMPBAN
      CHARACTER*1    LIST
      CHARACTER*4    BNAM2,BNAME,NAME,NLIST,CHAINT
      EQUIVALENCE    (BNAM2,IBNA2)
      EQUIVALENCE    (BNAME,IBNAM)
      EQUIVALENCE    (NAME,INAM)
#include "bcs.h"
#include "cmpinf.h"
C     If TOLOWER is added to an integer equivalent with a CHARACTER*4
C     variable, the first character of this variable is turned to lower
C     case (if it was in upper case before the addition). If TOLOWER is
C     substracted, and the first character was in lower case, it is
C     turned to upper case. TOLOWER depends on the character set (ASCII
C     or EBCDIC) and on the position that the 1st character occupies in
C     the 32-bit word (on VAX it is the LSB, on IBM and UNIX it is the
C     MSB).
C-----------------------------------------------------------------------
      NB=0
C     if NUMBNK <= 0 then, no compression
      IF (NUMBNK.LE.0) RETURN
      LIPOS=0
   10 LIPOS=LIPOS+1
          BNAME=NLIST(IW,LIPOS,LIST)
          IF (BNAME.EQ.' ') RETURN
          IF (LGE(BNAME(1:1),'a')) GO TO 10
C         find index of BNAME in INDEXW array
C         binary search
          IST=(NUMBNK+2)/2
          IB=IST
  100     INAM=IW(INDEXW(IRANKB(IB))-3)
          IF (IBNAM.NE.INAM) THEN
              IF (IST.EQ.1) GO TO 10
              IST=(IST+1)/2
              IF (LGT(BNAME,NAME))THEN
                  IB=IB+IST
                  IF (IB.GT.NUMBNK) IB=NUMBNK
              ELSE
                  IB=IB-IST
                  IF (IB.LT.1) IB=1
              END IF
              GO TO 100
          END IF
C         calculate information block size
C         find Number of Compressible Columns
          IB=IRANKB(IB)
          NCOL=IW(INDEXW(IB))/2
          INFS=3
          NCC=0
          DO 20 IC=1,2*NCOL-1,2
              IF (IW(INDEXW(IB)+IC).GT.2) NCC=NCC+1
   20         INFS=INFS+INFSIZ(IW(INDEXW(IB)+IC))
          IF (NCC.EQ.0) GO TO 10
C         CMPBAN is negative if no banks were compressed,
C                 zero if some banks were compressed
C                 and possitive if all banks were compressed.
          IF (CMPBAN(IB,INFS,NCC)) 91,92,93
   93     CALL BLIST(IW,LIST//'-',BNAME)
          LIPOS=LIPOS-1
   92     IBNA2=TOLOWER+IBNAM
          CALL BLIST(IW,LIST//'+',BNAM2)
          NB=NB+1
   91     GOTO 10
      END
#endif
