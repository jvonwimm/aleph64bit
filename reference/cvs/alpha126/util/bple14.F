

C=======================================================================
      logical function bpl_e14(module,iplane)
C------------------------------------------------------------
C Was this wire plane dead during the high energy running
C Auxiliary to QGETE14_MOD
C Author G. Taylor Oct 1997
C------------------------------------------------------------
      bpl_e14=.false.
      if (module.eq.15 .and. iplane.eq. 1) bpl_e14=.true. ! .1 Gev
      if (module.eq.22 .and. iplane.eq. 1) bpl_e14=.true. ! .1 Gev
      if (module.eq. 7 .and. iplane.eq. 2) bpl_e14=.true. ! .2 Gev
      if (module.eq.20 .and. iplane.eq. 2) bpl_e14=.true. ! .2 Gev
      if (module.eq.11 .and. iplane.eq. 6) bpl_e14=.true. ! .4 Gev
      if (module.eq.10 .and. iplane.eq.12) bpl_e14=.true. ! .8 Gev
      if (module.eq.3  .and. iplane.eq.18) bpl_e14=.true. ! .9 Gev
      if (module.eq.11 .and. iplane.eq.18) bpl_e14=.true. ! .9 Gev
      if (module.eq.11 .and. iplane.eq.27) bpl_e14=.true. ! .5 Gev
      if (module.eq. 9 .and. iplane.eq.30) bpl_e14=.true. ! .4 Gev
      if (module.eq.27 .and. iplane.eq.32) bpl_e14=.true. ! .3 Gev
      if (module.eq.20 .and. iplane.eq.34) bpl_e14=.true. ! .7 Gev
      if (module.eq. 6 .and. iplane.eq.39) bpl_e14=.true. ! .2 Gev
      if (module.eq. 4 .and. iplane.eq.40) bpl_e14=.true. ! .2 Gev
      if (module.eq. 5 .and. iplane.eq.42) bpl_e14=.true. ! .1 Gev
      if (module.eq. 1 .and. iplane.eq.45) bpl_e14=.true. ! .1 Gev
      if (module.eq. 2 .and. iplane.eq.45) bpl_e14=.true. ! .1 Gev
C new in 98
      if (module.eq. 3 .and. iplane.eq.42) bpl_e14=.true. ! .1 Gev
      if (module.eq.17 .and. iplane.eq.43) bpl_e14=.true. ! .1 Gev
      if (module.eq.17 .and. iplane.eq.45) bpl_e14=.true. ! .1 Gev
      return
      end
