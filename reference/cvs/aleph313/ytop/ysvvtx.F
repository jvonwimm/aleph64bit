      SUBROUTINE YSVVTX(IBNK,IVTYP,VXOUT,VVXOU,CHISQ,IVDOF,IXHX,IVTRN,
     $  IER,LGARB)
C----------------------------------------------------------------------
CKEY YTOP PYER
C! fill PYER bank
C!   Author   :- Jochen Lauber         24-OCT-1991
C!
C!   Inputs:
C!        -  ibnk     :    bank number of PYER,PYFR
C!                         for ordinary users = 0
C!        -  ivtyp    :    vertex type  0..255 1 = main
C!                                             2 = V0
C!                                             3 = main for 2-prongs
C!                                             4 = conversion
C!                                             5,6 = secondary
C!        -  vxout(3) :    vertex coordinates
C!        -  vvxout(6) :   packed covariance matrix
C!        -  chisq
C!        -  ivdof     :   degreees of freedom usually: 2*ivtrn - 3
C!        -  ixhx(ivtrn) : track numbers of tracks belonging to vertex
C!        -  ivtrn    :    count of tracks belonging to the vertex
C!
C!   Outputs:
C!        -  ier      :  0 means o.k.
C!        -  lgarb (logical) true means garbage collection has taken place
C!
C!   Libraries required:
C!
C!   Description
C!   ===========
C!   saves the vertex in the bank PYER, the covariance matrix and the chisqare
C!   and the degrees of freedoms and the vertextype
C!   save the track indices belonging to the secondary vertex in the bank PYFR
C?
C!======================================================================
#ifndef DOC
      SAVE
#include "bcs.h"
#include "pyerjj.h"
#include "pyfrjj.h"
C
      DIMENSION VXOUT(*),VVXOU(*),IXHX(*)
      LOGICAL LGARB
C
#include "bmacro.h"
C
      IER = 0
      LGARB=.FALSE.
C     ..................... save the vertex in the bank PYER
C     ............................ output to bos-bank PYER
      KPYER=NLINK('PYER',IBNK)
      IF(KPYER.GT.0) THEN
C       ...................... bank already exists
        KLAST = LROWS(KPYER)+1
      ELSE
        KLAST = 1
      ENDIF
      KYWI  = LPYERA*KLAST
C     ..........................   we book here the space for the bank
      CALL AUBOS('PYER',IBNK,LMHLEN+KYWI,KPYER,IRET)
C     ........................? no space
      IF(IRET.EQ.2) GOTO 997
      IF(IRET.EQ.1) LGARB=.TRUE.
      IW(KPYER+LMHCOL) = LPYERA
      IW(KPYER+LMHROW) = KLAST
C     ......?
      IPYER = KROW(KPYER,KLAST)
C     ...... store information
C     ......... type of vertex 0..255 1=main 2=v0,3=main for 2-prongs
C                                                4=conversion
      IW(IPYER+JPYETY)      = IVTYP
C     .................................... copy the vertex position
      CALL UCOPY(VXOUT(1),RW(IPYER+JPYEVX),3)
C     .......................................... copy the variances
C     ... covariance matrix 1 2 4
C     ...                     3 5
C     ...                       6
      CALL UCOPY(VVXOU(1),RW(IPYER+JPYEVM),6)
C     ........ copy the chisq
C     ......... c2 chisquare 0.0 ...255.
      RW(IPYER+JPYEC2) = CHISQ
C     ..........  copy the number of degrees of freedom,
C                    2x2 for each track - 3 for vertex constraint
      IW(IPYER+JPYEDF) = IVDOF

C     ......... save the track indices belonging to the vertex
C     ......... in the bank PYFR
C     .... but only if there are more than zero tracks
      IF (IVTRN.LE.0) RETURN
C      KPYFR=IW(NAMIND('PYFR'))
      KPYFR=NLINK('PYFR',IBNK)
      IF(KPYFR.GT.0) THEN
C       ................ bank already exists
        NMPYF = LROWS(KPYFR)
        NRPYF = NMPYF + IVTRN
      ELSE
        NMPYF = 0
        NRPYF = IVTRN
      ENDIF
      CALL AUBOS('PYFR',IBNK,LMHLEN+LPYFRA*NRPYF,KPYFR,IRET)
      IF(IRET.EQ.2) GOTO 996
      IF(IRET.EQ.1) LGARB=.TRUE.
      IW(KPYFR+LMHCOL) = LPYFRA
      IW(KPYFR+LMHROW) = NRPYF
      DO  ITR = 1,IVTRN
        IPYFR = KROW(KPYFR,ITR+NMPYF)
C       ................................. vertex number
        IW(IPYFR+JPYFVN) = KLAST
C       ................................. track number
        IW(IPYFR+JPYFTN) = IXHX(ITR)
      ENDDO
      RETURN
  996 CALL ALTELL('YSVVTX :  no space to create bank PYFR IER=1',0,
     &   ' RETURN ')
      IER=1
      RETURN
  997 CALL ALTELL('YSVVTX :  no space to create bank PYER IER=1',0,
     &   ' RETURN ')
      IER=1
      RETURN
      END
#endif
