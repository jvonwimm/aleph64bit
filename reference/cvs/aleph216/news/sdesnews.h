C! 1st entry in SCALDES set

 ! ALEPHLIB 21.3
     new routine STRIGF ( B.Bloch ) to identify trigger type
     new routine STMASK ( B.Bloch ) to build trigger masks

 ! ALEPHLIB 15.8
   SILTOX, SIXTOL - to go to/from ARS and local sytems .


 ! ALEPHLIB 15.7
     new routine SIIDPO from B.Bloch
         SIIDPO (IMD,IST,IPH,IRD,Z,PHI,RAD) transform indices(Module,
         layer,phi,rad ) to semi-cylindrical coordinates (Z,Phi,R) with
         ideal alignment
         Phi is in degrees
         an error code is returned with RAD <0 in case of problem

 ! ALEPHLIB 15.1
   SIRDAF : use ALGTDB instead of MDARD to get banks with setup code
            to avoid to duplicate banks with different NR but identical
            contents.

 ! ALEPHLIB 14.6
   the  ADBSCONS 179 is mandatory
   -------------------------------------------------------------------
         SCALDES - SICAL GEOMETRY PACKAGE
         ----------------------------------

 This package is a stand alone code which may be called to access
the SICAL geometry with different level of complexity.It knows about
readout geometry as well as space geometry.

 It uses banks from the data base ( SIGO,SALG,SINT,STYP )to take into
account internal geometry and alignment .

 It needs initialisation performed through a call to SIRDAF which
will access the Data Base and initialise the package.

 Then any of the internal subroutine can be called.
 As a general rule ,a negative value for any output argument means
 an error occurred.

 Two addressing schemes are considered , one corresponding to  RAW Data
( which applies to triplets ) , the other one corresponding to extended
address including the position in triplet as used in reconstructed data.

 RAW data format ( SIDI and SIFO banks)
 ----------------
            -------------------------------------
            |11|10| 9| 8| 7| 6| 5| 4| 3| 2| 1| 0|    Address in [0,4095]
            -------------------------------------
            <- <--------------<-----------<-----|
             Mod   Phi Bin       R bin     Triplet
             [0,1]  [0,31]       [0,15]     [0,3]

 Recontructed Data (SPDA,SMPD banks)
 ----------------
     ------------------------------------------
    |13|12|11|10| 9| 8| 7| 6| 5| 4| 3| 2| 1| 0|  +1 Address in [1,12288]
     ------------------------------------------  ==
    <---- <- <--------------<-----------<----->
    Tptpos Mod   Phi Bin       R bin     Triplet
     [0,2] [0,1]  [0,31]       [0,15]     [0,3]

 A first set of subroutines and function provide access to the package
 and some geometry quantities.

 A second set of subroutines provides decoding and encoding of addresses
 to and from indices. This , of course , doesn't use any alignment info,
 only the readout geometry.Both addressing schemes are provided.

 A third set of subroutines provides decoding and encoding of addresses
 to and from spatial position in cartesian and cylindrical coordinates.
 This uses internal geometry ,alignment information and the readout
 geometry.
 The addressing scheme is the Raw data scheme as described above.

 A fourth set of subroutines provides decoding and encoding of address
 indices to and from spatial position in cartesian and cylindrical
 coordinates.This uses internal geometry ,alignment information and the
 readout geometry.
 As it uses address indices , it can work in either addressing scheme.

 1_General routines
 ----------------
         SIRDAF (LUNDAF , IRUN,IOK): Initialise the package , loads
                necessary banks from Data base unit LUNDAF for run
                IRUN  , computes auxilliary quantities,fills internal
                common .IOK is zero if no error , negative if error.

         SIGTGO (ICONS,RCONS) : Returns in array ICONS(5),RCONS(20) the
                following quantities
                ICONS(1) = number of modules
                ICONS(2) = number of z layers ( called minimod)
                ICONS(3) = number of Radial bins
                ICONS(4) = number of Phi bins
                ICONS(5) not yet used

                RCONS(1) = z position of first sens layer side A
                RCONS(2) = z position of first sens layer side B
                RCONS(3) = z width of a minimod
                RCONS(4) = lower R position of first padrow side A
                RCONS(5) = lower R position of first padrow side B
                RCONS(6) = R width of a pad
                RCONS(7) = Phi offset in first triplet plane
                RCONS(8) = Phi offset in second triplet plane
                RCONS(9) = Phi offset in third triplet plane
                RCONS(10)= Phi width of a pad
   The next quantities are related to parametrization and correspond to
 amounts of material :
                RCONS(11)= # of radlength before first sens layer
                RCONS(12)= # of radlength per minimod
                RCONS(13)= width of material(cm) before first minimod
                RCONS(14)= width of material(cm) after last minimod
                RCONS(15)= width of last minimod(cm)
                RCONS(16)= width of material(cm) before first sens layer
                RCONS(17)= overlap of adjacent crystals (cm)
                RCONS(18-20)  not yet used

         SIPRGO : Prints out a summary of the current geometry constants

         SIDALI (IFL,IBD) : function that returns,for address IBD in the
                Reconstructed Rata scheme , a crude position which does
                not include internal alignment and fine offsets, only
                the basic alignment.
                For IFL = 1  , SIDALI is R (cm)
                For IFL = 2  , SIDALI is Phi ( degrees)
                For IFL = 3  , SIDALI is Z ( cm)
                In case of problem , SIDALI is 0.

2_Encoding/decoding addresses from and to indices
------------------------------------------------
      SIDCOD (IAD,IOR,IMD,IST,IPH,IRD) decode IAD,IOR (in Raw Data scheme)
      SIDCOZ (IBD,IMD,IST,IPH,IRD) decode IBD (in Reconstructed Data scheme)
      SIENCD (IAD,IOR,IMD,IST,IPH,IRD) encode to IAD,IOR (in Raw Data scheme)
      SIENCZ (IBD,IMD,IST,IPH,IRD) encode to IBD (in Reconstructed Data scheme)

3_Encoding/decoding to/from addresses from/to spatial position
--------------------------------------------------------------
      In case of error ( missing bank ,wrong address or space point outside
      detector) an error code is returned with IOK <0

      SIATOR (IAD,IOR,RAD,TETA,PHI,IOK)decode IAD,IOR (in Raw Data scheme)
                                       To cylindrical coordinates (R,Theta,Phi)
      SIATOX (IAD,IOR,POS,IOK)         decode IAD,IOR (in Raw Data scheme)
                                       To cartesian coordinates POS(3)=X,Y,Z
      SIRTOA (IAD,IOR,RAD,TETA,PHI,IOK)encode to IAD,IOR (in Raw Data scheme)
                                     from cylindrical coordinates (R,Theta,Phi)
      SIXTOA (IAD,IOR,X,Y,,Z,IOK)      encode to IAD,IOR (in Raw Data scheme)
                                     from cartesian coordinates (X,Y,Z)

4_Encoding/decoding to/from add indices from/to spatial position
--------------------------------------------------------------
   SIITOR (IMD,IST,IPH,IRD,Z,PHI,RAD) transform indices(Module,layer,phi,rad)
                     to semi-cylindrical coordinates (Z,Phi,R).
                     Phi is in degrees
                     an error code is returned with RAD <0 in case of problem

   SIXTOI (R,PHI,Z,IRD,IPH,IST,DR,DP,IOK)encode to indices (rad,Phi,layer)
                     from semi-cylindrical coordinates (R,Phi,Z)
      additionnal output : DR = normalised position within R bin [0.,1.]
                       DP(3)= normalised position within Phi bin [0.,1.]
                              with the 3 possible offsets
               an error code is returned with IOK <0 in case of problem

   SIZFRI(ZPOS,IST,IMD) from indices (layer,module) get ZPOS coordinate

   SIZTOI(ZPOS,IST,IMD) transform ZPOS coor into indices ( layer,module)
                        IST < 0 means outside detector

