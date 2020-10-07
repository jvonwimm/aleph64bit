CKEY HCALDES HCAL GEOM
C! HCAL geometry package
   The HCAL Geometry Package contains a set of routines wich allows,
 starting from the informations collected in the Aleph Data Base,
 to describe properly the Hadron Calorimeter.
 In more details the package contains routines useful to:

           1) Read and expand the D.B Geometrical informations
           2) Trasmit these informations outside
           3) Change Ref. System
           4) Coding and Decoding adress
           5) Correlate Point --> Tower#
                                  Wire#
                                  Module#            and viceversa
                                  SubComponent#
                                  ecc. ecc.
           6)Check Status of components (Dead Zones ,broken Channels and so on)
           7)Print Geometrical Constants

 The comunications between the HCAL package and the outside id done only via
 subroutines and functions calls. Now it is used by Galeph and Julia but can
 be used by any other program previous initialisation of the package
 (routine HRDDAF).
 A Note with a full description of the package will be provided soon.

 ! ALEPHLIB 15.6
   HFBAHI : correct a bug in the hit finding
   HFBAHIO: new routine which is a copy of HFBAHI before the
            correction of the bug. This routine will be used
            for the '91 and '92 reprocessing to have a coherent
            processing data vs MC.

 ! ALEPHLIB 15.5
   HRHTUB : put a protection when running on MC data made
            with JULIA <271
