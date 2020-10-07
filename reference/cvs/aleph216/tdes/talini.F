      SUBROUTINE TALINI
C
C---------------------------------------------------------------------
C - R.Johnson - 880627
C! Establish TPC alignment matrices at run beginning
C
C----------------------------------------------------------------------
#ifndef DOC
      SAVE
C
#include "alcons.h"
#include "bcs.h"
#include "tpgpar.h"
#include "talign.h"
#include "tjjpar.h"
#include "tposjj.h"
C
      DIMENSION ASTOE(3,3),DSTOE(3),AETOG(3,3),DETOG(3)
      DIMENSION GTRN(3),GROT(3,3)
C
#include "bmacro.h"
C
      KTSCO=IW(NAMIND('TSCO'))
      KTCGD=IW(NAMIND('TCGD'))
      KTSLO=IW(NAMIND('TSLO'))
      KTMTY=IW(NAMIND('TMTY'))
      KTPOS=IW(NAMIND('TPOS'))
C
C++   Get translation from ALEPH to TPC-ITC coordinates
C
      DO 32 I=1,3
        GTRN(I)=RTABL(KTPOS,1,JTPOTL-1+I)
   32 CONTINUE
      THE=RTABL(KTPOS,1,JTPORT)
      DEL=RTABL(KTPOS,1,JTPORT+1)
      PHI=RTABL(KTPOS,1,JTPORT+2)
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
C++   First build the relation from sector to global coordinates.  It will
C++   then be inverted to get the inverse relation.
C
      RSTEP=RTABL(KTCGD,1,JTCGRS)
      DO 500 IS=1,LTSECT
        ISLOT= ITABL(KTSLO,IS,JTSLSB)
        ISTYP= ITABL(KTSLO,IS,JTSLTM)
        ISS=   ITABL(KTSLO,IS,JTSLSS)
        IEND=  ITABL(KTSLO,IS,JTSLTS)
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
        DELTA= RTABL(KTSLO,IS,JTSLAS)
C
C++     Get angle of rotation of sector about its center
C
        ETA= RTABL(KTSLO,IS,JTSLRS)
C
C++     Get radial shift of the sector
C
        DELR=RTABL(KTSLO,IS,JTSLDS)
C
C++     Construct matrix to rotate from sector frame to endplate frame
C
        ASTOE(1,1)= COS(PHI0+DELTA+ETA)
        ASTOE(1,2)=-SIN(PHI0+DELTA+ETA)
        ASTOE(1,3)= 0.
        ASTOE(2,1)= SIN(PHI0+DELTA+ETA)
        ASTOE(2,2)= COS(PHI0+DELTA+ETA)
        ASTOE(2,3)= 0.
        ASTOE(3,1)= 0.
        ASTOE(3,2)= 0.
        ASTOE(3,3)= 1.
C
C++     For endplate A, FIRST rotate by pi radians about the sector
C++     x axis.
C
        IF (IEND.EQ.1) THEN
          ASTOE(1,2)=-ASTOE(1,2)
          ASTOE(2,2)=-ASTOE(2,2)
          ASTOE(3,3)=-ASTOE(3,3)
        ENDIF
C
C++     Construct translation from sector frame to endplate frame
C
        DSTOE(1)= (COS(PHI0+DELTA)-COS(PHI0+DELTA+ETA))*XC
     &           + COS(PHI0+DELTA)*DELR
        DSTOE(2)= (SIN(PHI0+DELTA)-SIN(PHI0+DELTA+ETA))*XC
     &           + SIN(PHI0+DELTA)*DELR
        DSTOE(3)= 0.
C
C++     Construct rotation from endplate frame to global frame
C
        THE= RTABL(KTSCO,IEND,JTSCAX)
        DEL=   RTABL(KTSCO,IEND,JTSCAY)
        PHI=   RTABL(KTSCO,IEND,JTSCAZ)
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
C++     Construct translation from endplate frame to global frame
C
        DETOG(1)= RTABL(KTSCO,IEND,JTSCRP+1)
        DETOG(2)= RTABL(KTSCO,IEND,JTSCRP+2)
        DETOG(3)= RTABL(KTSCO,IEND,JTSCRP)
C
C++     Muliply two transformations together to get transformation
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
  500 CONTINUE
C
  999 CONTINUE
      RETURN
      END
#endif
