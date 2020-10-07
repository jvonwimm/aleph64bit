         SUBROUTINE EBRACK(ITMX,CLUR,ECLS,ICRE,CRAC,IFLG)
C ----------------------------------------------------
C   AUTHOR   :  R.Clifft   08/06/88
C               J.Badier   29/11/89
C! Main routine for treatment of neutral clusters in crack regions of
C!   ECAL.( Crack clusters )
CKEY PHOTONS CRACK MAIN / INTERNAL
C   The general procedure which uses ECAL storey energies only makes
C   use of the ratios :
C        RATIO1  energy of last pad row/total energy ,for ECAL module
C                wuth greater energy
C        RATIO2  energy of lesser energy module/total cluster energy
C   These ratios are used in empirical relations derived from Monte Carlo
C   studies to evaluate an energy and position for an assumed incident
C   photon. For particles calculated to have impinged on a sensitive
C   region of ECAL near a crack, a final energy and phi coordinate is
C   output.For the others where in general most of the energy does not
C   enter ECAL, a nominal energy correction is made.In this case a final
C   correction requires the use of HCAL data together with particle
C   hypothesis assumptions and hence will be made elsewhere.
C
C
C   Input     ITMX(1)     Theta index of higher storey.
C             ITMX(2)     Phi index of the higher storey.
C             CLUR(3,3,3) Storeys surrounding the stack 2 storey
C                         in the TETA,PHI direction.
C             ECLS(3,3)   Stacks content for each hitted module.
C             ICRE(4)     Extension code( region,crack in overlap,crack,
C                         overlap)
C
C   Output    CRAC(5)
C                       1 Corrected energy
C                       2 Corrected azimuth
C                       3 Main module energy
C                       4 Energy of the pad row adjacent to the crack
C                       5 Energy ratio stack 1+2 / 1+2+3 .
C             IFLG        Treatment flag
C                      <0 No treatment : CRAC(I) = .0 , I=1,5
C                       0 An energy estimation is given, but the standard
C                         one is recommended.
C                       1 Energy and azimuth are estimated, the standard
C                         identification is valid.
C                       2 Energy and azimuth are estimated,the standard
C                         identification is wrong,the HCAL information
C                         is necessary to terminate the treatment.
C
C     called by      USER
C     calls          EBTSTC,EBCREP,EBRANC
C
C     banks          NONE
C
C ----------------------------------------------------
#ifndef DOC
      SAVE
      DIMENSION ECLS(3,*) , CLUR(3,3,*) , CRAC(*) , ITMX(*) , PT(3)
#include "ebenec.h"
C
      ITRWEB = ITMX(1)
      JFCLEB = ITMX(2)
C
      CALL EBRGCD( ITRWEB , JFCLEB , KODEEB , NREGEB , IER)
C
C   TETA , PHI out of range.
      IF( IER .NE. 0 ) GO TO 101
C   Direction  has to be near the crack.
      IF( KODEEB(3) .EQ. 0 ) GO TO 101
C   Skip overlap region
      IF( KODEEB(4) .NE. 0 ) GO TO 101
C
      DTET = ITRWEB
      DTET = DTET + .5
      DFI = JFCLEB
      DFI = DFI + .5
      CALL ESRPT( 'ALEPH' , DTET , DFI , 1. , PT )
      PC = PT(1)**2 + PT(2)**2
      HY = SQRT( PC + PT(3)**2 )
C     Sin( Incidence angle ) : SINCEB
      IF( NREGEB(1) .EQ. 2 ) THEN
        SINCEB = PT(3) / HY
      ELSE
        SINCEB = SQRT( PC ) / HY
      ENDIF
C   Prepare EBENEC common.
      CALL EBCREP( ECLS , CLUR )
C
C *** Save main crack energy correction variables.
C
      CRAC(3)= ENECRA(1)
      CRAC(4)= ENECA1(1)
      CRAC(5)= R12STY
C
C   Estimate energy and azimuth.
      CALL EBRANC( IFLG )
C
C *** Save corrected cluster energy and azimuthal coordinate.
C
      CRAC(1) =  ENETOT
      CRAC(2) =  PHICOR
C
      GO TO 98
C
  101 CONTINUE
      IFLG = - IER
      DO 1 I = 1 , 5
        CRAC(I) = 0.
    1 CONTINUE
C
   98 CONTINUE
      RETURN
      END
#endif
