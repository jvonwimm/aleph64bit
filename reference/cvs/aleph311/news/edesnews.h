    The ECAL geometry package contains a set of routines which expands
 the geometrical description of the ECAL contained in the data base
 providing utilities to access easily this geometrical information.
    This package is  used in GALEPH and JULIA and can be called
 by any program provided a correct initialisation is done.
    Particular care has been taken to present the data to the user in
 various convenient forms,especially for ECAL analysis and graphics
 applications.
    The communication between the ECAL geometry package and the user
 program is done only through subroutines or functions calls.
 Data transmission goes through arguments.Therefore,the data base access
 and all common blocks internal to the package are hidden to the user.
   The package contains 2 parts :
    - Readout geometry description and utilities : ECAL storeys
    - Volume description and utilities : The mechanical structure
    All ECAL geometrical constants are read from the ALEPH data base.

    The package must be initialized  by 2 calls at the run
 initialization stage , after BOS initialization and DAF opening.
               CALL ERDDAF(LUNDAF,IRUN,IGOOD)
    which reads all ECAL banks from DAF ,fills internal ECAL commons
 and returns IGOOD = 1 when successfull.
               CALL ECDFRD
    which expands the geometry description to ease the access.

 A full description of the package can be found in ALEPH note 88-177
         ECAL GEOMETRY PACKAGE  M.Rumpf ,H.Videau



 ! Correction file 6 for Alephlib 30.7
   EXPFRF : Bug fix. Correct treatment of d0 sign (C.Mannert)
            The bug affected mainly the calculation of the impact point on
            ECAL for GAMPEX photons

 ! ALEPHLIB 21.3
   EPRCOM, ERDDAF : Fix multiline strings

 ECALDES version 211
 ----------------------------------------------------------------------
 Changes since version 2.10
    Safety checks on cl1 et cl2 in EPLSQL, EPLSCC, EPLSCF
    Correction of a bug in EDEFLG,EGPTLG
    Modifications for alignement in EALIGN
    Modification in EPRCOM
    Change of sign in ESRPL in such a way that the power of inner
     points is positive.
    Safety checks in EXPFRF in case of ITC tracks.
 The name of the routine EXPAND has been changed to EXPNEC to avoid
clashes with a routine from the GKS drivers.
     Correction in EDISGN, order of non diagonal matrices.
 ----------------------------------------------------------------------
