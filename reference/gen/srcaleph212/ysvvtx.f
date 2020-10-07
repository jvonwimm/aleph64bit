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
C!        -  lgarb (logical) true means garbage collection has taken pla
C!
C!   Libraries required:
C!
C!   Description
C!   ===========
C!   saves the vertex in the bank PYER, the covariance matrix and the ch
C!   and the degrees of freedoms and the vertextype
C!   save the track indices belonging to the secondary vertex in the ban
C?
C!======================================================================
      SAVE
      INTEGER LMHLEN, LMHCOL, LMHROW
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2)
C
      COMMON /BCS/   IW(1000)
      INTEGER IW
      REAL RW(1000)
      EQUIVALENCE (RW(1),IW(1))
C
      PARAMETER(JPYETY=1,JPYEVX=2,JPYEVY=3,JPYEVZ=4,JPYEVM=5,JPYEC2=11,
     +          JPYEDF=12,LPYERA=12)
      PARAMETER(JPYFTN=1,JPYFVN=2,LPYFRA=2)
C
      DIMENSION VXOUT(*),VVXOU(*),IXHX(*)
      LOGICAL LGARB
C
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
