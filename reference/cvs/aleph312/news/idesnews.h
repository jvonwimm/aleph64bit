CKEY ITCDES ITC GEOM
C! ITC geometry package
 ! 980902 ALEPHLIB 309.0
    IINRES : New Drift-time params: use Spline from IDSP bank for 1997 
             data onwards (run > 43000). MC will still use IDRP. (J.Sedgbeer)
    IPRDAF : Add print of new variables in /IDRPCC/              (J.Sedgbeer)

 ! ALEPHLIB 30.2
   new alignment (W.Wiedenman) is introduced.
   IINALI - support for new alignment TPC bank TNOS.

 ! ALEPHLIB 15.4
   IDTCCC - Comments modified.
   IDRPJJ - New comdeck:
            IDRP HAC params.
   IDRPCC - New comdeck:
            New Drift-time relation parameters.
   IINRES - IDTC   fill drift-time parametrisation (old) into /IDTCCC/
                   and /IDRPCC/. IDTC bank used for 1989-1992 data.
            IDRP   fill drift-time parametrisation (new) into /IDTCCC/
                   and /IDRPCC/. IDRP bank used for 1993 onwards.
   IPRDAF - Add print for IDRP values.

                                                             21/10/91
 Package to set-up ITC constants from DAF (data-base). Gets constants
 for geometry, alignment, front-end electronics and resolution, i.e.
 all constants necessary for ITC prepare data.

 Subroutine IRDDAF:  (Itc ReaD DAF)
 ------------------
       Gets constants for a particular run from data base banks.
       IRDDAF must be called once per run.
       See subroutine header for details of calling arguments and
       error returns etc.

           calls - IGEOMW   wire geometry
                   IINALI   alignment
                   IFECON   front-end constants
                   IINRES   resolution

 Subroutine    banks  comment
 IGEOMW        ITCC   wire sag and length into /ITWICC/
               ILYR   fill /ITWICC/  IWIRIT(8), ... etc.
                      fill /ISWPHI/PHSWIS(960) phi of each wire in ITC frame.
               IEWP   Correct each PHSWIS value.
               IWST   get wire status bank from cards/run header/dbase
               ICAE   get cabling errors bank from Dbase.
 IINALI        IALI   Fill /IALIGC/ ITC alignment wrt TPC
               TPOS   Fill /IALIGG/ ITC-TPC alignment wrt ALEPH
                      Note that the TPC is NOT defined to be the ALEPH
                      frame. IALI is defined as the ITC wrt TPC.
                      To get ITC coords in the ALEPH frame first transform
                      into TPC frame then into ALEPH frame.
 IFECON        IRFE   fill /IRFECC/ R-phi front end consts.
                                           ( from dbase or run header)
               IZFE   Fill /IZFECC/ Z front end Consts (dbase or run header)
               IZNL   Fill /IZNLCC/ Z non linearity consts. (dbase)
               ISFE   contains flags indicating if R-phi and/or Z TDC
                      data is O.K., i.e. coords. can be made.
 IINRES        IDTC   fill /IDTCCC/ drift-time relation parametrisation
               IRES   fill /IRESCC/ r-phi resolution params.
                      fill /IRESOL/ nominal resolution in R-phi
               IRRF   get from Dbase - r-phi resol. ued in track fits.
               IZRS   fill /IZRSCC/ z resolution params.
                      fill /IRESOL/ nominal res. in z
               IEDD   get from dbase - look-up tables of fine corrections
                      to drift-time relation in IDTC
               IET0   get from Dbase - correction to each channel for
                      T0 offset

 IGETDB        used to get banks that can be on database or run header -
               see comments below on Origin of Banks.
               Note that IGETDB does not get the database or check that
               the existing database is the correct one. This is O.K.
               as the sequence of calls in IRDDAF is such that ALGTDB
               has been called before IGETDB and so the correct database
               will be open.

 Note: Origin of banks - Cards, Run Header record, Dbase.
 --------------------------------------------------------

 The majority of banks are only found in the Dbase or as input cards.
 For these banks the above routines use the ALEPHLIB function AGETDB
 to get the appropriate bank. The hierarchy is 1) cards, 2) Dbase.

 Input cards should have bank number=-1 in ALPHA and JULIA jobs.

 For the ITC the banks  IRFE, IZFE and IWST may be present in the
 run header record as well as in the Dbase or input cards. Run header
 banks have number=0 (up to now , i.e. 1n 1989,90,91)
 To ensure that the correct  IRFE, IZFE and IWST banks are located
 the subroutine IGETDB is used.

 The hierarchy for IRFE and IZFE (in both ALPHA and JULIA) is:
   without UDAF card              :   1)Cards, 2)Dbase,      3)Run header
   with    UDAF card (i.e. FALCON):   1)Cards, 2)Run header, 3)Dbase

 The hierarchy for IWST is:
   with or without UDAF card          1)Cards, 2)Run header  3)Dbase


 Subroutine IPRDAF:
 ------------------
       Print current contents of commons filled by IRDDAF.
       IRDDAF must be called before IPRDAF.

