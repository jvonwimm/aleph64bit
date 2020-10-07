 SUBSCHEMA GeneralAttribute
 :'Contains the definitions of the attributes which are general
   for the whole ALEPH Detector Description System (ADDS)'
 
 
 AUTHOR   'A. Putzer'
 REVIEWER 'S. Fisher'
 VERSION  '1.4'
 DATE     '26/05/89'
 
 DEFINE ATTRIBUTE
 
         Angle        = REAL [-6.300000,6.300000]
                                           : 'angle of something in rad';
 
         Date         = INTE [0,99999999]  : 'Date of something';
 
         Length       = REAL [-2000.0000,2000.0000]
                                           : 'Length of something in CM';
 
         Name         = CHA4               : 'Name of something';
 
         SiGn         = INTE -1|1          : 'Sign of something';
 
 
 
         HwName       = Name               : 'Name of a physical module
                                              (given by hardware)';
 
         ValRng       = Date               : 'Validity range';
 
         RelPos       = Length             : 'Nominal relative position';
 
         RelRot       = Angle              : 'Nominal relative rotation';
 
         CmpTyp       = Name               : 'Name of a det. component';
 
         ScType       = Name               : 'Name of a det. subcomp.';
 
 
 
 END ATTRIBUTE
 
 END SUBSCHEMA
 
