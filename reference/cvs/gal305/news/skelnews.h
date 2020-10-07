*CD skelnews
C! 1st entry in SKELeton set
  ! GALEPH 30.1
    remove LEVEL 3 trigger
    call new LEVEL 1 trigger

  ! GALEPH 25.4
    to avoid linking problems on UNIX machines, the KINE options 'LUND'
    ans 'SJET' are removed.
    They can be replaced by KINE 'USER' or by reading a KINGAL file
    ASKLUN, ASKLUI, ASKJET - purged
    ASREDC, ASRTYP, ASKINE - modified
   --------------------------------------------------------------------

   The SKEL set contains *CD common to every modules and all manegerial
   subroutines.
   A set is a module which contains *CD called only by the subroutines of
   this module.
 ! GALEPH 25.5
  Modifications activated only with flag IFINTER  (B.Bloch May 1993)
    ASPRUN: update to call KUWHAT instead of GINTER
    GALEPH: update Zebra common length for interactive graphic version


