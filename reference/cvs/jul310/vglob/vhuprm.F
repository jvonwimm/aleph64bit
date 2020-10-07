        subroutine vhuprm(id, view, hit, bank)
C--------------------------------------------------------------
C
C! Unpack and transform to an address in the VDXY or VDZT bank
C
C       Author:  D. Casper      24/04/97
C
C  INPUT:  integer ID    - pack hit ID
C          integer VIEW  - 1=w, 2=v
C OUTPUT:  integer HIT   - hit number within wafer
C          integer BANK  - encoded readout module address
C---------------------------------------------------------------
#ifndef DOC
        IMPLICIT NONE
        INTEGER id, view, hit, bank
        INTEGER wafer

C Unpack the wafer number and hit id

        call vhupck(id, wafer, hit)

C Get the readout module (bank)

        call vrmwf(wafer, view, bank)

        return
        end
#endif
