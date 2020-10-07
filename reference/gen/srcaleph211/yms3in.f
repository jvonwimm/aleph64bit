      SUBROUTINE YMS3IN(A,B,IFAIL)
C----------------------------------------------------------*
C!    inverse a 3x3 symmetric matrix in double precision
CKEY YTOP MATRIX / USER
C!    Author :     G. Lutz   30/03/88
C!    Modified :   M. Bosman 01/12/88
C!    Modified :   G.Lutz,M. Bosman 07/07/91
C!
C!
C!    Description
C!    ===========
C!    This routine inverses a 3X3 symmetric matrix
C!    input double precision, internal calculations
C!    in double precision
C!
C----------------------------------------------------------*
      INTEGER LMHLEN, LMHCOL, LMHROW
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2)
C
      COMMON /BCS/   IW(1000)
      INTEGER IW
      REAL RW(1000)
      EQUIVALENCE (RW(1),IW(1))
C
      DOUBLE PRECISION A(3,*),B(3,*)
      DOUBLE PRECISION D11,D12,D22,D13,D23,D33,DET
      DOUBLE PRECISION A11,A12,A22,A13,A23,A33
      DOUBLE PRECISION DET2,DET3
      DATA ICNERR/0/,ICNMAX/20/
C----------------------------------------------------------*
C
C-- Define the logical unit for printout
      LOUT = IW(6)
      IFAIL=0
C
      A11=A(1,1)
      A12=A(1,2)
      A22=A(2,2)
      A13=A(1,3)
      A23=A(2,3)
      A33=A(3,3)
C
      D11=A22*A33-A23**2
      D12=A12*A33-A23*A13
      D22=A11*A33-A13**2
      D13=A12*A23-A22*A13
      D23=A11*A23-A12*A13
      D33=A11*A22-A12**2
C
      DET=A11*D11-A12*D12+A13*D13
C
      IF(DET.LE.0.) THEN
        IFAIL=1
C
      ELSE
C
        B(1,1)=D11/DET
        B(1,2)=-D12/DET
        B(2,2)=D22/DET
        B(1,3)=D13/DET
        B(2,3)=-D23/DET
        B(3,3)=D33/DET
C
        B(2,1)=B(1,2)
        B(3,1)=B(1,3)
        B(3,2)=B(2,3)
C
      ENDIF
C
      RETURN
      END
