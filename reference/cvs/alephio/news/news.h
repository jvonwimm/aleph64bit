C! ALEPH machine dependant code
   ALE:ALEPHIO.OLB, _D.OLB, .NEWS, ALEPHIO:[*]*.F, [C]*.c, [INC]*.h

   /aleph/src/alephio      ---> $ALROOT/alephio/*
   alephio is part of alephlib
   /aleph/lib/libalephlib.a ---> /aleph/gen/libalephlib.a
   ----------------------------------------------------------------------
 ! 010205 - ALEPHIO 8.3
   ASTAGE - introduce "Z"tape number handling for CASTOR (JBO)

   ----------------------------------------------------------------------
 ! 001110 - ALEPHIO 8.2
   ACLOSE - do not call BWRND at end of job for EPIO output unit
            to avoid to call cfrew twice.

   ----------------------------------------------------------------------
 ! 000818 - ALEPHIO 8.1 
   ALK7COM - increase the number of segments: maximum number of runs=15000
   SXSDR   - fix a precision problem on data written on DEC-VAX and read on
             Linux (J.B.Hansen)

   -----------------------------------------------------------------------
 ! 991202 - ALEPHIO 8.0  Y2K compliant
            BECAREFUL - it should be used with ADBSCONS 2000
   ADBVER - date print format
   ALVSN  - update IYR to 4 digits.
   BKRHAL - update IDAY to 4 digits.
   GTSTUP - year is now 4-digit integer but RUNH is still 2-digit
   GTYEAR - integer function : returns the year of data taking or the
            year of the MC geometry.

   ----------------------------------------------------------------------
 ! 981214 - ALEPHIO 7.4
   ALK7COM - increase the number of segments: maximum number of runs=12500
   ALGTENV - replace I_tapes with Y_tapes to get the full name.

   ----------------------------------------------------------------------
 ! 971124 - ALEPHIO 7.3

   ALK7FIL - enforce "-s 600" and not "-S 600" as it was done in alephio 7.2

   -----------------------------------------------------------------------
 ! 971114 - ALEPHIO 7.2

   ALK7FIL - enforce -s 600 for DLTs and Redwood if not given.

   ----------------------------------------------------------------------
 ! 971102 - ALEPHIO 7.1
   
   ADSPOS, ASTAGE - 
           add the variable index in LNBLNK calling sequences (Linux).

   AOPTAP, AWRTAP - 
           remove -b parameter, keep -L parameter in the staging message.

   BKRAHB -
           include krunjj.h which was forgotten when switching to CVS.

   -----------------------------------------------------------------------
 ! 970502 - ALEPHIO 7.0

   GTFTYP - remove the UNIX path name before looking for the file type.
            on /afs there are 2 "." in the file name :
            /afs/cern.ch/..../fname.ftyp

   -------------------------------------------------------------------------
 ! 970117 - ALEPHIO 6.9 this correction is removed

   AOPEN, AOPENW, AOPTAP, AWRTAP
   on VAX, set EPIO-C flag on: EPIO is calling CFIO package on all platforms.

   ------------------------------------------------------------------------- 
 ! 961218 - ALEPHIO 6.8

   introduce ALEPH_LINUX to build libraries on Linux operating system
   to compile under Linux select -DUNIX -DALEPH_LINUX

   ADSPOS, ALSTGQRY, AOPEN, AOPENW, AOPTAP, ASTAGE, AWRTAP - 
           declare EXTERNAL SYSTEM to please Linux.

   CMPHOST, CTOLOW, CMPBAN, DMPBAN, DMPCHR -
           add ALEPH_LINUX to ALEPH_DEC so that Linux and DEC use VAX packing.

   ALMACH - add ALEPH_LINUX

   SDRSX, SIBSDR, SXSDR - add ALEPH_LINUX to ALEPH_DEC in floating point
           converting routines (not used anyway).

   CMPBU3, CMPBU4, DMPBL4 - remove an apostrophe in a C-comment to please
           Linux C-preprocessor.

   ----------------------------------------------------------------------
 ! 961021 - ALEPHIO 6.7

   ASTAGE, AWRTAP - on HP UNIX only
            use the same CERN stager on all UNIX patforms.
            introduce a ALEPH_OUTSIDE flag to keep the previous code for
            outside HP machines if they need it.
            To use the old code, these 2 routines have to be compiled
            with $FC $FCOPT -I$ALROOT/alephio/inc -DALEPH_OUTSIDE
 
   ---------------------------------------------------------------------
 ! 960903 - ALEPHIO 6.6

   AWRTAP - on UNIX only,
            replace '-v' argument with '-V' as in AOPTAP.
            introduce an #if defined(ALEPH_HP) which was missing.

   ----------------------------------------------------------------------
 ! 960814 - ALEPHIO 6.5

   AOPTAP - on UNIX only,
            replace '-v' argument with '-V' : -V is followed by the VID
            which is given on the FILI card,
            -v is followed by the VSN which must be specified on the 
            FILI card if different from the VID.

   ---------------------------------------------------------------------
 ! 960718 - ALEPHIO 6.4

   ADSPOS, AOPEN, AOPENW, AOPTAP, ASTAGE, AWRTAP - change CALL SYSTEM to
           IST = SYSTEM( ) and then check IST. If not 0 quit.

   ---------------------------------------------------------------------
 ! 960715 - ALEPHIO 6.3

   AOPTAP, MYTAPE - cut AOPTAP.F into AOPTAP.F and MYTAPE.F to please
                    Wisconsin AXP/VMS

   ---------------------------------------------------------------------
               
 ! 960611 - ALEPHIO 6.2

   ALSTGQRY - re-introduce this routine which had disappeared !!!

   --------------------------------------------------------------------
 ! 960529 - ALEPHIO 6.1

   ADBVER, OPENDA - rewrite a WRITE statment to please fpp on alws.

   ----------------------------------------------------------------------
 ! 960422 - ALEPHIO 6.0

        move ALPH, DBAS, a part of ALEF from ALEPHLIB to ALEPHIO
        the corresponding include files have been moved as well.
        a module C which contains the IO C routines is introduced.
        
        C-comilation is made with : -DALEPH_C 

        F77-compilation on UNIX machine is made with :
             -DUNIX -DALEPH_${VENDOR} (VENDOR = DEC SGI HP)

        F77-compilation on AXP/VMS is made with :
             -DALEPH_DEC         for *.F

        In alph/
        replace TIMAx by TIMEx subroutine calls in abrrec.F

   -----------------------------------------------------------------------
 ! 960312 - ALEPHIO 5.3

        Replace flag VAX by DEC.AND..NOT.UNIX
        rearrange the code to fit this new flag
        to compile VMS version define DEC
        to compile a UNIX version define UNIX and DEC (or HP, or SGI)

        In *CD -> ARDNEWS
   ACLOSE - avoid to call BRWND twice for the same file.
   ASTAGE, AWRTAP - introduce a flag HP to distinguish between the 
                    two different stagers.

   ----------------------------------------------------------------------
 ! 960111 - ALEPHIO 5.2
        In *CD -> ARDNEWS
   ALSTGQRY - on UNIX look also on ALWS to find whether a file is staged.
              BECAREFUL this FUNCTION works at CERN, outside it should be
              customized to use other stager if necessary.
              For the moment this FUNCTION is called by LOOK on request,
              and by DALI when a FILM card contains a CART parameter
              to request the staging of a file.

   ----------------------------------------------------------------------
 ! 951128 - ALEPHIO 5.1
        IN *CD -> ARDNEWS
   AWRTAP   - change the name of the script 'alstagewrt' to 'alstageout'
              (UNIX change only).
   ALSTGQRY - remove options if any

   ---------------------------------------------------------------------
 ! 950321 - ALEPHIO 5.0
        In *CD -> ARDNEWS
   ASTAGE, AWRTAP - to adapt to new stageout script
   ALSTGQRY - integer function to make a stagequery on VAX or UNIX
              IER = ALSTGQRY (fname)   i.e. - fname = ab1234.1.sl
              IER =0 if file is NOT staged, =1 if it is staged

   ---------------------------------------------------------------------
 ! 940808 - ALEPHIO 4.0
        In *CD -> IONEWS
   ALMACH - distinguish between VAX station and AXP.

   --------------------------------------------------------------------------
 ! 940531 - ALEPHIO 3.0
            CD IO
   add following *CD: ADBSJJ, IANDSH, JOBERR.
   add following *DK: ALMACH, ALTELL, OPENDA, OPENDB, OPENSQ, FPRROW,
                      SDRSX, SIBSDR, SXSDR, EXIT, JALREC, NOTIMA.

   ---------------------------------------------------------------------
 ! 940518 - ALEPHIO 2.0
            CD ARDNEWS
   AOPTAP - modify the test to distinguish between AB1234.1.SL and AB1234.11.SL
            AB1234.1.SL is kept as ab1234.epio
            AB1234.2.SL is kept as ab1234.2.epio
            AB1234.11.SL is kept as ab1234.11.epio

   ---------------------------------------------------------------------
 ! 940401 - ALEPHIO 1.0

   1st version of the ALEPH machine dependant code library.

   To facilitate the ALEPHLIB installation the 3 sets which contain machine
   dependant code have been extracted from ALEPHLIB and put into ALEPHIO.

   ALEPHIO contains the following sets: ALREAD, EPIO, PRESS.

   ALEPHLIB has no more machine dependant code.

   ALEPHIO 1.0 is included into ALEPHLIB 20.0
   ------------------------------------------------------------------------








