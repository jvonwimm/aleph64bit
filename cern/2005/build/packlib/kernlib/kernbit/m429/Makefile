# Makefile generated by imake - do not edit!

# ----------------------------------------------------------------------
# Makefile generated from "Imake.tmpl" and </home/wimmer/aleph64bit/cern/2005/src/packlib/kernlib/kernbit/m429/Imakefile>
# $XConsortium: Imake.tmpl,v 1.224.1.1 95/06/19 17:51:01 gildea Exp $
#

.SUFFIXES: .i

# $XConsortium: Imake.cf,v 1.19 95/01/05 19:24:32 kaleb Exp $

# -----------------------------------------------------------------------
# site-specific configuration parameters that need to come before
# the platform-specific parameters - edit site.def to change

# site:  $XConsortium: site.sample,v 1.9 94/04/08 17:02:06 rws Exp $

# -----------------------------------------------------------------------
# platform-specific configuration parameters - edit linux-lp64.cf to change

# $XConsortium: lnxLib.rules,v 1.8 95/01/16 21:11:00 kaleb Exp $

# -----------------------------------------------------------------------
# site-specific configuration parameters that go after
# the platform-specific parameters - edit site.def to change

# site:  $XConsortium: site.sample,v 1.9 94/04/08 17:02:06 rws Exp $

# -----------------------------------------------------------------------
# Imake rules for building libraries, programs, scripts, and data files
# rules:  $XConsortium: Imake.rules,v 1.197.1.1 95/06/19 18:01:48 gildea Exp $

 _NULLCMD_ = @ echo -n

          PATHSEP = /
	        / = /
            SHELL = /bin/sh

              TOP = /home/wimmer/aleph64bit/cern/2005/build
      CURRENT_DIR = /packlib/kernlib/kernbit/m429

	QUOTE="
\ENDQUOTE="

ifeq "$(strip $(IMAKE))" ""
            IMAKE = imake
endif
ifeq "$(strip $(MAKEDEPEND))" ""
           DEPEND = makedepend
else
	   DEPEND = $(MAKEDEPEND)
endif
        MKDIRHIER = mkdirhier
        CONFIGSRC = $(CVSCOSRC)/config
         IMAKESRC = $(CONFIGSRC)/imake
        DEPENDSRC = $(CONFIGSRC)/makedepend
           IXXSRC = $(UNSUPPORTEDSRC)/programs/ixx
              IXX = ixx
         IXXFLAGS = -s BaseObject -m TypeObj -r RequestObj -p Xf
      IXXINCLUDES = -i '<X11/Fresco/enter-scope.h>'

          INCROOT = $(CVSCOSRC)/include
        USRLIBDIR = $(CVSCOSRC)/lib
         SHLIBDIR = $(CVSCOSRC)/lib
       LINTLIBDIR = $(USRLIBDIR)/lint
          MANPATH = $(CVSCOSRC)/man
    MANSOURCEPATH = $(MANPATH)/man
           MANDIR = $(MANSOURCEPATH)1
        LIBMANDIR = $(MANSOURCEPATH)3
       FILEMANDIR = $(MANSOURCEPATH)5

                AR = ar clq

	    XARGS = xargs

  BOOTSTRAPCFLAGS =
               CC = gcc
               AS = as

        CCDEFINES =

               FC = gfortran -no-pie
              FPP = /lib/cpp
      FDEBUGFLAGS = -O
FORTRANSAVEOPTION =
   FORTRANOPTIONS = -fno-automatic -fno-second-underscore -fno-range-check -fPIC $(FORTRANSAVEOPTION)
           FCLINK = gfortran -no-pie
  FORTRANINCLUDES = $(INCLUDES) $(EXTRA_INCLUDES) $(PACKAGE_INCLUDES) $(TOP_INCLUDES)
   FORTRANDEFINES =
FORTRANALLDEFMDEPEND = $(FORTRANINCLUDES) $(FORTRANDEFINES) $(CERNDEFINES) $(EXTRA_DEFINES) $(DEFINES)

  FORTRANALLDEFINES = $(FORTRANALLDEFMDEPEND)

          FCFLAGS = $(FDEBUGFLAGS) $(FORTRANOPTIONS)

.SUFFIXES: .cxx

              CXX = g++
    CXXDEBUGFLAGS = -O1 -fomit-frame-pointer
 CXXEXTRA_DEFINES =
CXXEXTRA_INCLUDES =
  CXXIDL_INCLUDES = -I$(TOP)/include
   CXXSTD_DEFINES =
       CXXOPTIONS =
      CXXINCLUDES = $(INCLUDES) $(TOP_INCLUDES) $(CXXEXTRA_INCLUDES) $(CXXIDL_INCLUDES)
       CXXDEFINES = $(CXXINCLUDES) $(CXXSTD_DEFINES) $(THREADS_CXXDEFINES) $(CXXEXTRA_DEFINES)
         CXXFLAGS = $(CXXDEBUGFLAGS) $(CXXOPTIONS) $(THREADS_CXXFLAGS) $(CXXDEFINES)

         COMPRESS = compress
              CPP = /lib/cpp $(STD_CPP_DEFINES)
    PREPROCESSCMD = gcc -E $(STD_CPP_DEFINES)
          INSTALL = install
     INSTALLFLAGS = -c
               LD = ld
              LEX = flex -l
           LEXLIB = -lfl
             YACC = bison -y
           CCYACC = bison -y
             LINT = lint
      LINTLIBFLAG = -C
         LINTOPTS = -axz
               LN = ln -s
             MAKE = gmake
               MV = mv
               CP = cp

           RANLIB = ranlib
  RANLIBINSTFLAGS =

               RM = rm -f
        MANSUFFIX = 1x
     LIBMANSUFFIX = 3x
    FILEMANSUFFIX = 5x
            TROFF = psroff
         MSMACROS = -ms
              TBL = tbl
              EQN = eqn
            TOUCH = touch

            DVIPS = dvips
            LATEX = latex

     STD_INCLUDES =
  STD_CPP_DEFINES = -traditional
      STD_DEFINES =
 EXTRA_LOAD_FLAGS =
  EXTRA_LDOPTIONS =
  EXTRA_LIBRARIES =
             TAGS = ctags

           MFLAGS=$(filter-out --%,$(filter -%,$(MAKEFLAGS)))

    SHAREDCODEDEF =
         SHLIBDEF =

     SHLIBLDFLAGS =

         PICFLAGS = -B/usr/dll/jump/

      CXXPICFLAGS = -B/usr/dll/jump/

      PIFFLAGS = -B/usr/dll/jump/

    PROTO_DEFINES = -DFUNCPROTO=15 -DNARROWPROTO

     INSTPGMFLAGS = -s

     INSTBINFLAGS = -m 0755
     INSTUIDFLAGS = -s -m 4755
     INSTLIBFLAGS = -m 0644
     INSTINCFLAGS = -m 0444
     INSTMANFLAGS = -m 0444
     INSTDATFLAGS = -m 0444
    INSTKMEMFLAGS = -s -m 4755

      PROJECTROOT = $(CVSCOSRC)

     TOP_INCLUDES = -I$(INCROOT)

      CDEBUGFLAGS = -O1 -fomit-frame-pointer
        CCOPTIONS = -fPIC

      ALLINCLUDES = $(INCLUDES) $(EXTRA_INCLUDES) $(PACKAGE_INCLUDES) $(TOP_INCLUDES) $(STD_INCLUDES)
       ALLDEFINES = $(ALLINCLUDES) $(STD_DEFINES) $(EXTRA_DEFINES) $(PROTO_DEFINES) $(THREADS_DEFINES) $(DEFINES) $(CCDEFINES) $(CERNDEFINES)
           CFLAGS = $(CDEBUGFLAGS) $(CCOPTIONS) $(THREADS_CFLAGS) $(ALLDEFINES)
        LINTFLAGS = $(LINTOPTS) -DLINT $(ALLDEFINES) $(DEPEND_DEFINES)
         LDPRELIB = -L$(USRLIBDIR)
        LDPOSTLIB =
        LDOPTIONS = $(CDEBUGFLAGS) $(CCOPTIONS)  $(EXTRA_LDOPTIONS) $(THREADS_LDFLAGS) $(LOCAL_LDFLAGS) $(LDPRELIB)
     CXXLDOPTIONS = $(CXXDEBUGFLAGS) $(CXXOPTIONS) $(EXTRA_LDOPTIONS) $(THREADS_CXXLDFLAGS) $(LOCAL_LDFLAGS) $(LDPRELIB)
      FCLDOPTIONS =  $(FDEBUGFLAGS) $(FORTRANOPTIONS) $(EXTRA_LDOPTIONS) $(THREADS_LDFLAGS) $(LOCAL_LDFLAGS) $(LDPRELIB)

           LDLIBS = $(LDPOSTLIB) $(THREADS_LIBS) $(SYS_LIBRARIES) $(EXTRA_LIBRARIES)

           CCLINK = gcc

          CXXLINK = $(CXX)

     LDSTRIPFLAGS = -x
   LDCOMBINEFLAGS = -r
      DEPENDFLAGS =

        MACROFILE = linux-lp64.cf
           RM_CMD = $(RM)

    IMAKE_DEFINES =
    IMAKE_INCLUDES=

         IRULESRC = $(CONFIGDIR)
        IMAKE_CMD = $(IMAKE) -DUseInstalled -DLocalTop=$(LOCALTOP)  			-I$(IRULESRC) $(IMAKE_INCLUDES) $(IMAKE_DEFINES)

     ICONFIGFILES = $(IRULESRC)/Imake.tmpl $(IRULESRC)/Project.tmpl 			$(IRULESRC)/site.def $(IRULESRC)/$(MACROFILE) 			 $(EXTRA_ICONFIGFILES)

archive/%.o : %.c archive/%.d
	$(RM) $@
	$(CC) -c $(CDEBUGFLAGS) $(CCOPTIONS) $(ALLDEFINES) $< -o $@

archive/%.d : %.c
	-@if [ ! -d archive ] ; then mkdir archive; fi
	@ $(TOUCH) $@.tmp
	 $(DEPEND) -f$@.tmp -p archive/ -- $(ALLDEFINES) $(_NOOP_) $<
	@ sed 's=$*\.o=& $@=g' $@.tmp > $@
	@ $(RM) $@.tmp $@.tmp.bak

debug/%.o : %.c debug/%.d
	$(RM) $@
	$(CC) -c -g $(CCOPTIONS) $(ALLDEFINES) $< -o $@

debug/%.d : %.c
	-@if [ ! -d debug ] ; then mkdir debug; fi
	@ $(TOUCH) $@.tmp
	 $(DEPEND) -f$@.tmp -p debug/ -- $(ALLDEFINES) $(_NOOP_) $<
	@ sed 's=$*\.o=& $@=g' $@.tmp > $@
	@ $(RM) $@.tmp $@.tmp.bak

shared/%.o : %.c shared/%.d
	$(RM) $@
	$(CC) -c $(CDEBUGFLAGS) $(PICFLAGS) $(CCOPTIONS) $(ALLDEFINES) $< -o $@

shared/%.d : %.c
	-@if [ ! -d shared ] ; then mkdir shared; fi
	@ $(TOUCH) $@.tmp
	 $(DEPEND) -f$@.tmp -p shared/ -- $(ALLDEFINES) $(_NOOP_) $<
	@ sed 's=$*\.o=& $@=g' $@.tmp > $@
	@ $(RM) $@.tmp $@.tmp.bak

.SUFFIXES: .F

.F.o:
	$(RM) $@
	$(FC) -c $(FDEBUGFLAGS) $(FORTRANOPTIONS) $(FORTRANALLDEFINES) -o $@ $<

archive/%.o : %.F archive/%.d
	$(RM) $@
	$(FC) -c $(FDEBUGFLAGS) $(FORTRANOPTIONS) $(FORTRANALLDEFINES) -o $@ $<

archive/%.d : %.F
	-@if [ ! -d archive ] ; then mkdir archive; fi
	@ $(TOUCH) $@.tmp
	 $(DEPEND) -f$@.tmp -p archive/ -- $(FORTRANALLDEFMDEPEND) $(_NOOP_) $<
	@ sed 's=$*\.o=& $@=g' $@.tmp > $@
	@ $(RM) $@.tmp $@.tmp.bak

debug/%.o : %.F debug/%.d
	$(RM) $@
	$(FC) -c -g $(FORTRANOPTIONS) $(FORTRANALLDEFINES) -o $@ $<

debug/%.d : %.F
	-@if [ ! -d debug ] ; then mkdir debug; fi
	@ $(TOUCH) $@.tmp
	 $(DEPEND) -f$@.tmp -p debug/ -- $(FORTRANALLDEFMDEPEND) $(_NOOP_) $<
	@ sed 's=$*\.o=& $@=g' $@.tmp > $@
	@ $(RM) $@.tmp $@.tmp.bak

shared/%.o : %.F shared/%.d
	$(RM) $@
	$(FC) -c $(FDEBUGFLAGS) $(PIFFLAGS) $(FORTRANOPTIONS) $(FORTRANALLDEFINES) -o $@ $<

shared/%.d : %.F
	-@if [ ! -d shared ] ; then mkdir shared; fi
	@ $(TOUCH) $@.tmp
	 $(DEPEND) -f$@.tmp -p shared/ -- $(FORTRANALLDEFMDEPEND) $(_NOOP_) $<
	@ sed 's=$*\.o=& $@=g' $@.tmp > $@
	@ $(RM) $@.tmp $@.tmp.bak

ifeq ($(strip $(CERN)),)
CERN=/cern
endif

ifeq ($(strip $(CERN_LEVEL)),)
CERN_LEVEL=new
endif

ifeq ($(strip $(CERN_LIBDIR)),)
CERN_LIBDIR=$(CERN)$/$(CERN_LEVEL)$/lib
endif

ifeq ($(strip $(CERN_SHLIBDIR)),)
CERN_SHLIBDIR=$(CERN)$/$(CERN_LEVEL)$/shlib
endif

ifeq ($(strip $(CERN_BINDIR)),)
CERN_BINDIR=$(CERN)$/$(CERN_LEVEL)$/bin
endif

        CONFIGDIR = $(CVSCOSRC)$/config

	CERNDEFINES=-DCERNLIB_LINUX -DCERNLIB_UNIX -DCERNLIB_LNX -DCERNLIB_QMGLIBC -DCERNLIB_QMLXIA64 -DCERNLIB_GFORTRAN -DCERNLIB_GCC4 -DCERNLIB_VERBOSE
        PACKAGETOP=/packlib/kernlib/kernbit
        PACKAGE_NAME=kernbit
	LOCALTOP=

        PACKAGE_INCLUDES=-I$(TOP)$(PACKAGETOP) -I$(PROJECTROOT)$(LOCALTOP)$(PACKAGETOP) -I$(VPATH)

	VPATH=$(PROJECTROOT)$(LOCALTOP)$(CURRENT_DIR)

OBJS_C=$(SRCS_C:.c=.o)
OBJS_F=$(SRCS_F:.F=.o)
OBJS_CDF=$(SRCS_CDF:.cdf=.o)

OBJS=$(OBJS_C) $(OBJS_F) $(OBJS_CDF)

# -----------------------------------------------------------------------
# start of Imakefile : </home/wimmer/aleph64bit/cern/2005/src/packlib/kernlib/kernbit/m429/Imakefile>

SRCS_F= numbit.F

ifeq ($(strip $(VERSION)),)
VERSION=archive
endif
ifneq ($(strip $(VERSION)),archive)
LIBRARY=NullParameter_$(VERSION)
else
LIBRARY=
endif
LIBPATH=$(TOP)$(CURRENT_DIR)
SUBDIRLIST=
SUBDIRSTAMPS=$(foreach dir,,$(dir)/$(VERSION)/stamp)
ifeq ($(strip $(PACKAGE_LIB)),)
override PACKAGE_LIB=$(LIBPATH)/lib$(LIBRARY).a
endif

ifneq (,)
.PHONY:  -debug -shared

::
	@$(MAKE) VERSION=archive lib$@.a

-debug::
	@$(MAKE) VERSION=debug lib$@.a

-shared::
	@$(MAKE) VERSION=shared libNullParameter.sl

endif

archive_OBJS=$(foreach obj,$(OBJS),archive/$(obj))
debug_OBJS=$(foreach obj,$(OBJS),debug/$(obj))
shared_OBJS=$(foreach obj,$(OBJS),shared/$(obj))

ifeq ($(VERSION),archive)
VEROBJS:=archive_OBJS

endif

ifeq ($(VERSION),debug)
VEROBJS:=debug_OBJS

endif

ifeq ($(VERSION),shared)
VEROBJS:=shared_OBJS

endif

ifeq ($(VERSION),nil)
VERDIR=
VEROBJS=
endif

ifneq ($(strip $(VERSION)),)
VERDIR=$(VERSION)/
else
VERDIR=
VEROBJS=
endif

$(foreach ver,archive debug shared,$(ver)/objects.list):: \
		$(foreach dir,,$(dir)/Makefile)

define build-object-list
@[ -n "$(@D)" -a ! -d "$(@D)" ] && mkdir $(@D) || true
@$(RM) $@
@echo $(strip $($(@D)_OBJS)) | tr ' ' '\012' > $@
@ (set -e;\
	 case '${MFLAGS}' in *[i]*) set +e;; esac; \
	 subdirs=$(strip $(QUOTE)  $(QUOTE));\
	 list=`/bin/pwd`/$@;\
	 for dir in $$subdirs; do \
	  (set -e;cd $$dir;sed -e "\=^[^/]=s=^.=$$dir/&=" $@ >> $$list);\
	 done) || (rm $@; exit 1)
endef

ifeq ($(strip $(VERSION)),archive)
archive/objects.list:: Makefile $(archive_OBJS) \
		$(foreach dir,,$(dir)/archive/objects.list)
	$(build-object-list)

else
archive/objects.list::
	$(MAKE) VERSION=archive archive/objects.list

endif
debug/objects.list:: Makefile $(debug_OBJS) \
		$(foreach dir,,$(dir)/debug/objects.list)
	$(build-object-list)

shared/objects.list:: Makefile $(shared_OBJS) \
		$(foreach dir,,$(dir)/shared/objects.list)
	$(build-object-list)

$(foreach dir,,$(foreach ver,archive debug shared,$(dir)/$(ver)/objects.list))::
	@ echo checking $(CURRENT_DIR)/$(firstword $(subst /, ,$@))
	@ $(MAKE) -C $(firstword $(subst /, ,$@)) VERSION=$(filter archive debug shared,$(subst /, ,$@))	\
		   $(patsubst $(firstword $(subst /, ,$@))/%,%,$@)

ifneq ($(strip $(QUOTE)  $(QUOTE)),$(QUOTE)$(QUOTE))

libNullParameter.a: archive/objects.list
	@echo rebuild archive library $@ in $(CURRENT_DIR)
	@$(RM) $@
	@date
	@cat archive/objects.list | \
	   $(XARGS) $(AR) $@
	@date
	$(RANLIB) $@

libNullParameter-debug.a: debug/objects.list
	@echo rebuild debug library $@ in $(CURRENT_DIR)
	@$(RM) $@
	@date
	@cat debug/objects.list | \
	   $(XARGS) $(AR) $@
	@date
	$(RANLIB) $@

libNullParameter.sl:
	@echo No shared library available yet for  `uname`

endif

ifneq ($($(VEROBJS)),)
$($(VEROBJS):.o=.d): Makefile
include $($(VEROBJS):.o=.d)

endif

clean::
	$(RM) $(LIBRARY) $(VERDIR)stamp $(VERDIR)objects.list

emptyrule::

.SUFFIXES: .d

.c.d:
	$(SHELL) -ec "gcc -M $(ALLDEFINES) $< \
                      | sed 's/^[ ]*$*\.o/& $@/g' >$@"

.F.d:
	-@if [ ! -d . ] ; then mkdir .; fi
	@ $(TOUCH) $@.tmp
	 $(DEPEND) -f$@.tmp -p ./ -- $(FORTRANALLDEFMDEPEND)  $<
	@ sed 's=$*\.o=& $@=g' $@.tmp > $@
	@ $(RM) $@.tmp $@.tmp.bak

# -----------------------------------------------------------------------
# common rules for all Makefiles - do not edit

.c.i:
	$(RM) $@
	$(CC) -E $(CFLAGS) $(_NOOP_) $*.c > $@

emptyrule::

clean::
	$(RM_CMD) *.CKP *.ln *.BAK *.bak *.o core errs ,* *~ *.a .emacs_* tags TAGS make.log MakeOut  "#"*

Makefile:: Imakefile
	-@if [ -f $@ ]; then set -x; \
	$(RM) $@.bak; $(MV) $@ $@.bak; \
	else exit 0; fi
	$(IMAKE_CMD) -DTOPDIR=$(TOP) -DCURDIR=$(CURRENT_DIR) \
	             -DPackageTop=$(PACKAGETOP)	\
		     -DPackageName=$(PACKAGE_NAME) \
		      -f $<

tags::
	$(TAGS) -w *.[ch]
	$(TAGS) -xw *.[ch] > TAGS

# -----------------------------------------------------------------------
# empty rules for directories that do not have SUBDIRS - do not edit

install::
	@echo "install in $(CURRENT_DIR) done"

install.man::
	@echo "install.man in $(CURRENT_DIR) done"

install.linkkit::
	@echo "install.linkkit in $(CURRENT_DIR) done"

Makefiles::

includes::

depend::

tree::
	@true

all::

# -----------------------------------------------------------------------
# dependencies generated by makedepend

