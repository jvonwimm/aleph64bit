      SUBROUTINE TALINI
C
C---------------------------------------------------------------------
C - R.Johnson - 880627
C! Establish TPC alignment matrices at run beginning for 1997 alignment
C! This routine is compatible with either old or new alignment constants
C
C  W.Wiedenmann - 05-03-97
C             -  modified to do Rz correction from TNLO bank
C             -  If no TNLO bank is found then the old TSLO bank    
C                is used and the Rz correction is not done in
C                TALINI but in TZCSVD from the old TRZS bank.  
C                Thus TALINI should be backward compatible to     
C                old setup.  
C----------------------------------------------------------------------
#ifndef DOC
      SAVE
C
#include "alcons.h"
#include "bcs.h"
C
#include "tpgpar.h"
#include "talign.h"
#include "tcgdjj.h"
#include "tcrljj.h"
#include "tmtyjj.h"
#include "tpcojj.h"
#include "tphejj.h"
#include "tphtjj.h"
#include "tptejj.h"
#include "tsgmjj.h"
#include "tslojj.h"
#include "tnlojj.h"
#include "tthejj.h"
#include "tthtjj.h"
#include "tscojj.h"
#include "tncojj.h"
#include "t1fcjj.h"
#include "t2fcjj.h"
#include "tnfcjj.h"
#include "tposjj.h"
#include "tnosjj.h"
#include "trzsjj.h"
#include "tbdrjj.h"
C
#include "tpcdrift.h"
C
      DIMENSION ASTOE(3,3),DSTOE(3),AETOG(3,3),DETOG(3)
      DIMENSION GTRN(3),GROT(3,3)
      Dimension a_s_xyz(3,3), a_s_phi(3,3)
      Dimension xtemp(3,3), aout(3,3), bfield(3)
C
#include "bmacro.h"
C
C
C - These banks are common to old or new alignment
C
      KTCGD=IW(NAMIND('TCGD'))
      KTMTY=IW(NAMIND('TMTY'))
C
C - Old alignment banks; note that the formats of TPOS/TNOS and 
C   TSCO/TNCO are identical. This is *not* the case for TNLO/TSLO!
C
      KTSLO=IW(NAMIND('TSLO'))
      KTPOS=IW(NAMIND('TPOS'))
      KTSCO=IW(NAMIND('TSCO'))
C
C - New alignment banks (see note above)
C
      KTNLO=IW(NAMIND('TNLO'))
      KTNCO=IW(NAMIND('TNCO'))
      KTNOS=IW(NAMIND('TNOS'))
      KTBDR=IW(NAMIND('TBDR'))
C
C - Check for consistency of banks
C
      IF (.NOT.((KTNLO.GT.0 .AND. KTNCO.GT.0 
     &    .AND. KTNOS.GT.0 .AND. KTBDR.GT.0) .XOR.
     &    (KTSLO.GT.0 .AND. KTSCO.GT.0 .AND. KTPOS.GT.0)))
     &    CALL ALTELL('TALINI - Incompatible banks. Contact expert',
     &      -1,'END')
C
C++   B-field direction; assume z direction if no data
C
      IF (KTBDR.NE.0) THEN
         BFIELD(1) = RTABL(KTBDR,1,JTBDBX)
         BFIELD(2) = RTABL(KTBDR,1,JTBDBY)
         BFIELD(3) = RTABL(KTBDR,1,JTBDBZ)
      ELSE
         BFIELD(1) = 0.
         BFIELD(2) = 0.
         BFIELD(3) = 1.
      ENDIF
C
C++   Get translation from ALEPH to TPC-ITC coordinates
C
      IF (KTNOS.GT.0) THEN
        DO I=1,3
            GTRN(I)=RTABL(KTNOS,1,JTNOTL-1+I)
        ENDDO
        THE=RTABL(KTNOS,1,JTNORT)
        DEL=RTABL(KTNOS,1,JTNORT+1)
        PHI=RTABL(KTNOS,1,JTNORT+2)
      ELSE
        DO I=1,3
            GTRN(I)=RTABL(KTPOS,1,JTPOTL-1+I)
        ENDDO
        THE=RTABL(KTPOS,1,JTPORT)
        DEL=RTABL(KTPOS,1,JTPORT+1)
        PHI=RTABL(KTPOS,1,JTPORT+2)
      ENDIF
      GROT(1,1)= COS(DEL)*COS(PHI)
      GROT(1,2)=-COS(DEL)*SIN(PHI)
      GROT(1,3)= SIN(DEL)
      GROT(2,1)= SIN(THE)*SIN(DEL)*COS(PHI) + COS(THE)*SIN(PHI)
      GROT(2,2)= COS(THE)*COS(PHI) - SIN(THE)*SIN(DEL)*SIN(PHI)
      GROT(2,3)=-SIN(THE)*COS(DEL)
      GROT(3,1)= SIN(THE)*SIN(PHI) - COS(THE)*SIN(DEL)*COS(PHI)
      GROT(3,2)= SIN(THE)*COS(PHI) + COS(THE)*SIN(DEL)*SIN(PHI)
      GROT(3,3)= COS(THE)*COS(DEL)
C
C++   First build the relation from sector to global coordinates.  It wi
C++   then be inverted to get the inverse relation.
C
      RSTEP=RTABL(KTCGD,1,JTCGRS)
      DO 500 IS=1,LTSECT
        If (ktnlo.ne.0) Then 
           ISLOT= ITABL(KTNLO,IS,JTNLSB)
           ISTYP= ITABL(KTNLO,IS,JTNLTM)
           ISS=   ITABL(KTNLO,IS,JTNLSS)
           IEND=  ITABL(KTNLO,IS,JTNLTN)
        Else
           ISLOT= ITABL(KTSLO,IS,JTSLSB)
           ISTYP= ITABL(KTSLO,IS,JTSLTM)
           ISS=   ITABL(KTSLO,IS,JTSLSS)
           IEND=  ITABL(KTSLO,IS,JTSLTS)
        Endif
C
C++     Get distance from the padrow centers to the midpoint between
C++     the first and last padrows
C
        NPADR= ITABL(KTMTY,ISTYP,JTMTNP)
        RFST= RTABL(KTMTY,ISTYP,JTMTRF)
        RLST= RFST + FLOAT(NPADR-1)*RSTEP
        XC= 0.5*(RFST+RLST)
C
C++     Get half-length of TPC active volume
C
        ZMX= RTABL(KTCGD,1,JTCGTO+2)-RTABL(KTCGD,1,JTCGTT+2)
C
C++     Get the nominal angle from x axis to sector position
C
        ANFIR= RTABL(KTMTY,ISTYP,JTMTPP)
        ANGSP= RTABL(KTCGD,1,JTCGAS)
        PHI0= ANFIR + ANGSP*FLOAT(MOD(ISS-1,6))
C
C++     Get the correction to phi0
C
        If (ktnlo.ne.0) Then 
           phi_s= Rtabl(ktnlo,is,jtnlas)
        Else 
           phi_s= Rtabl(ktslo,is,jtslas)
        Endif 
C
C++     Get angle of rotation of sector about its center
C
        If (ktnlo.ne.0) Then 
           psi_s= Rtabl(ktnlo,is,jtnlrs)
        Else 
           psi_s= Rtabl(ktslo,is,jtslrs)
        Endif 
C
C++     Get radial shift of the sector
C
        If (ktnlo.ne.0) Then 
           delr_s=Rtabl(ktnlo,is,jtnlds)
        Else 
           delr_s=Rtabl(ktslo,is,jtslds)
        Endif 
C
C++     Get angle of sector rotation around x-direction
C
        the_s = 0.
        If (ktnlo.ne.0) the_s = Rtabl(ktnlo,is,jtnlaz)
C
C++     Get angle of sector rotation around y-direction
C
        del_s = 0.
        If (ktnlo.ne.0) del_s = Rtabl(ktnlo,is,jtnlrz)
C
C++     Get shift of sector in z-direction
C
        delz_s = 0.
        If (ktnlo.ne.0) delz_s = Rtabl(ktnlo,is,jtnldz)
C
C++     Matrix A_s_phi : correction + nominal sector position
C
        a_s_phi(1,1) =  Cos(phi_s+phi0)
        a_s_phi(1,2) = -Sin(phi_s+phi0)
        a_s_phi(1,3) =  0.
        a_s_phi(2,1) =  Sin(phi_s+phi0)
        a_s_phi(2,2) =  Cos(phi_s+phi0)
        a_s_phi(2,3) =  0.
        a_s_phi(3,1) =  0.
        a_s_phi(3,2) =  0.
        a_s_phi(3,3) =  1.
C
C++     Matrix A_s_xyz = A(theta_s)*A(delta_s)*A(psi_s)
C
        a_s_xyz(1,1) = Cos(del_s)*Cos(psi_s)
        a_s_xyz(1,2) =-Cos(del_s)*Sin(psi_s)
        a_s_xyz(1,3) = Sin(del_s)
        a_s_xyz(2,1) = Sin(the_s)*Sin(del_s)*Cos(psi_s) +
     +                 Cos(the_s)*Sin(psi_s)
        a_s_xyz(2,2) = Cos(the_s)*Cos(psi_s) -
     +                 Sin(the_s)*Sin(del_s)*Sin(psi_s)
        a_s_xyz(2,3) =-Sin(the_s)*Cos(del_s)
        a_s_xyz(3,1) = Sin(the_s)*Sin(psi_s) -
     +                 Cos(the_s)*Sin(del_s)*Cos(psi_s)
        a_s_xyz(3,2) = Sin(the_s)*Cos(psi_s) +
     +                 Cos(the_s)*Sin(del_s)*Sin(psi_s)
        a_s_xyz(3,3) = Cos(the_s)*Cos(del_s)
C
C++     Construct matrix to rotate from sector frame to endplate frame
C++     Matrix astoe = A_s_phi*A_s_xyz
C
        Do j=1,3
           Do k=1,3
              astoe(j,k) = 0.
              Do l=1,3
                 astoe(j,k) = astoe(j,k) +
     +                        a_s_phi(j,l)*a_s_xyz(l,k)
              Enddo
           Enddo
        Enddo
C
C++     For endplate A, FIRST rotate by pi radians about the sector
C++     x axis.
C
        If (iend.eq.1) Then
           Do j=1,3
              Do k=2,3
                 astoe(j,k)=-astoe(j,k)
              Enddo
           Enddo
        Endif
C
C++     Construct translation from sector frame to endplate frame
C
        dstoe(1) = (xc+delr_s)*a_s_phi(1,1) - xc*astoe(1,1)
        dstoe(2) = (xc+delr_s)*a_s_phi(2,1) - xc*astoe(2,1)
        dstoe(3) = delz_s                   - xc*astoe(3,1)

        IF (KTNCO.GT.0) THEN
C
C++     Endplate-to-global rotatation angles
C
            THE= RTABL(KTNCO,IEND,JTNCAX)
            DEL= RTABL(KTNCO,IEND,JTNCAY)
            PHI= RTABL(KTNCO,IEND,JTNCAZ)
C
C++     Translation from endplate frame to global frame
C
            DETOG(1)= RTABL(KTNCO,IEND,JTNCRP+1)
            DETOG(2)= RTABL(KTNCO,IEND,JTNCRP+2)
            DETOG(3)= RTABL(KTNCO,IEND,JTNCRP)
        ELSE
C
C++     Endplate-to-global rotatation angles
C
            THE= RTABL(KTSCO,IEND,JTSCAX)
            DEL= RTABL(KTSCO,IEND,JTSCAY)
            PHI= RTABL(KTSCO,IEND,JTSCAZ)
C
C++     Translation from endplate frame to global frame
C
            DETOG(1)= RTABL(KTSCO,IEND,JTSCRP+1)
            DETOG(2)= RTABL(KTSCO,IEND,JTSCRP+2)
            DETOG(3)= RTABL(KTSCO,IEND,JTSCRP)
        ENDIF
C
C++     Construct rotation from endplate frame to global frame
C
        AETOG(1,1)= COS(DEL)*COS(PHI)
        AETOG(1,2)=-COS(DEL)*SIN(PHI)
        AETOG(1,3)= SIN(DEL)
        AETOG(2,1)= SIN(THE)*SIN(DEL)*COS(PHI) + COS(THE)*SIN(PHI)
        AETOG(2,2)= COS(THE)*COS(PHI) - SIN(THE)*SIN(DEL)*SIN(PHI)
        AETOG(2,3)=-SIN(THE)*COS(DEL)
        AETOG(3,1)= SIN(THE)*SIN(PHI) - COS(THE)*SIN(DEL)*COS(PHI)
        AETOG(3,2)= SIN(THE)*COS(PHI) + COS(THE)*SIN(DEL)*SIN(PHI)
        AETOG(3,3)= COS(THE)*COS(DEL)
C
C++     Multiply two transformations together to get transformation
C++     from the sector frame to the TPC frame
C
        DO 156 I=1,3
          DSTOTP(I,ISLOT)= DETOG(I)
          DO 154 J=1,3
            ASTOTP(I,J,ISLOT)=0.
            DSTOTP(I,ISLOT)= DSTOTP(I,ISLOT)
     &                        + AETOG(I,J)*DSTOE(J)
            DO 146 L=1,3
              ASTOTP(I,J,ISLOT)=ASTOTP(I,J,ISLOT)
     &                      + AETOG(I,L)*ASTOE(L,J)
  146       CONTINUE
  154     CONTINUE
  156   CONTINUE
C
C++     Now multiply the three transformations together to get
C++     transformation from the sector frame to the ALEPH frame
C
        DO 100 I=1,3
          DSTOGL(I,ISLOT)=GTRN(I)
          DO 50 J=1,3
            DSTOGL(I,ISLOT)= DSTOGL(I,ISLOT) + GROT(I,J)*DETOG(J)
            ASTOGL(I,J,ISLOT)=0.
            DO 40 L=1,3
              DSTOGL(I,ISLOT)= DSTOGL(I,ISLOT)
     &                + GROT(I,L)*AETOG(L,J)*DSTOE(J)
              DO 35 K=1,3
                ASTOGL(I,J,ISLOT)=ASTOGL(I,J,ISLOT)
     &                      + GROT(I,L)*AETOG(L,K)*ASTOE(K,J)
   35         CONTINUE
   40       CONTINUE
   50     CONTINUE
  100   CONTINUE
C
C++     Now, invert the relation in order to go from global to sector
C
        DO 200 I=1,3
          DO 150 J=1,3
            AGLTOS(I,J,ISLOT)=ASTOGL(J,I,ISLOT)
            ATPTOS(I,J,ISLOT)=ASTOTP(J,I,ISLOT)
  150     CONTINUE
  200   CONTINUE
C
        DO 300 I=1,3
          DGLTOS(I,ISLOT)=0.
          DTPTOS(I,ISLOT)=0.
          DO 250 J=1,3
            DGLTOS(I,ISLOT)=DGLTOS(I,ISLOT)
     &                         - AGLTOS(I,J,ISLOT)*DSTOGL(J,ISLOT)
            DTPTOS(I,ISLOT)=DTPTOS(I,ISLOT)
     &                         - ATPTOS(I,J,ISLOT)*DSTOTP(J,ISLOT)
  250     CONTINUE
  300   CONTINUE
C
C++     Compute TPC transverse drift
C
        DO I=1,3
           DO K=1,3
              AOUT(I,K) = 0.
           ENDDO
        ENDDO
C
C++     Compute aout
C
        DO IC = 1,3
           EFAC = 0.
           DO I=1,3
              EFAC = EFAC + GROT(3,I)*ASTOGL(I,IC,ISLOT)
           ENDDO
           CALL TMATUT(ASTOGL,ASTOGL,ISLOT,IC,XTEMP)
           DO I=1,3
            DO K=1,3
               AOUT(I,K) = AOUT(I,K) + EFAC*XTEMP(I,K)
            ENDDO
           ENDDO
        ENDDO
C
C++     Compute transverse drift velocities
C
        FAC = 0.
        DO I=1,3
           FAC = GROT(I,3)*BFIELD(I)
        ENDDO
C
        DO IC = 1,2
           TPC_UXY(IC,ISLOT) = GROT(IC,3) + WT2*BFIELD(IC)*FAC
           DO K=1,3
              TPC_UXY(IC,ISLOT) = TPC_UXY(IC,ISLOT) +
     +                            WT*AOUT(IC,K)*BFIELD(K)
           ENDDO
           TPC_UXY(IC,ISLOT) = C2*TPC_UXY(IC,ISLOT)
        ENDDO
C
  500 CONTINUE
C
  999 CONTINUE
      RETURN
      END
#endif
