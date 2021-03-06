ifeq ($(SRCDIR)x,x)
  SRCDIR = $(CURDIR)/../..
  BUILDDIR = $(SRCDIR)
endif
SUBDIR = converter/other
VPATH=.:$(SRCDIR)/$(SUBDIR)

include $(BUILDDIR)/config.mk

ifeq ($(shell xml2-config --version),)
  XML2_LIBS=NONE
  XML2_CFLAGS=NONE
else
  XML2_LIBS=$(shell xml2-config --libs)
  XML2_CFLAGS=$(shell xml2-config --cflags)
endif

SUBDIRS = jbig pnmtopalm jpeg2000 cameratopam pamtosvg
ifneq ($(BUILD_FIASCO), N)
  SUBDIRS += fiasco
endif

ifneq ($(TIFFLIB),NONE)
  ifneq ($(TIFFHDR_DIR)x,x)
    EXTERN_INCLUDES += -I$(TIFFHDR_DIR)
  endif
endif

ifeq ($(shell libpng$(PNGVER)-config --version),)
  ifneq ($(PNGLIB),NONE)
    HAVE_PNGLIB = Y
    ifneq ($(PNGHDR_DIR)x,x)
      EXTERN_INCLUDES += -I$(PNGHDR_DIR)
    endif
    ifneq ($(ZHDR_DIR)x,x)
      EXTERN_INCLUDES += -I$(ZHDR_DIR)
    endif
  endif
else
  HAVE_PNGLIB = Y
  EXTERN_INCLUDES += $(shell libpng$(PNGVER)-config --cflags)
endif

ifneq ($(JPEGLIB),NONE)
  ifneq ($(JPEGHDR_DIR)x,x)
    EXTERN_INCLUDES += -I$(JPEGHDR_DIR)
  endif
endif
ifneq ($(URTLIB),NONE)
  ifneq ($(URTHDR_DIR)x,x)
    EXTERN_INCLUDES += -I$(URTHDR_DIR)
  endif
endif
ifneq ($(XML2_LIBS),NONE)
  ifneq ($(XML2_CFLAGS),NONE)
    EXTERN_INCLUDES += $(XML2_CFLAGS)
  endif
endif

ifeq ($(TIFFLIB),NONE)
  TIFF_PREREQ_MISSING = Y
endif

TIFFLIB_EXTRALIBS =
ifeq ($(TIFFLIB_NEEDS_JPEG),Y)
  ifeq ($(JPEGLIB),NONE)
    TIFF_PREREQ_MISSING = Y
  else
    TIFFLIB_EXTRALIBS += $(JPEGLIB)
  endif
endif
ifeq ($(TIFFLIB_NEEDS_Z),Y)
  ifeq ($(ZLIB),NONE)
    TIFF_PREREQ_MISSING = Y
  else
    TIFFLIB_EXTRALIBS += $(ZLIB)
  endif
endif

PORTBINARIES =  bmptopnm fitstopnm \
		gemtopnm giftopnm hdifftopam infotopam \
		pamtodjvurle pamtofits pamtogif \
		pamtohdiff pamtohtmltbl pamtompfont pamtooctaveimg \
		pamtopam pamtopfm pamtopnm pamtouil \
		pamtoxvmini \
		pbmtopgm pfmtopam \
	        pgmtopbm pgmtoppm ppmtopgm pnmtoddif \
		pnmtopclxl \
		pnmtosgi pnmtosir pamtotga pnmtoxwd \
		rlatopam sgitopnm sirtopnm xwdtopnm zeisstopnm

ifneq ($(DONT_HAVE_PROCESS_MGMT),Y)
  PORTBINARIES += pstopnm
endif

BINARIES = $(PORTBINARIES) pnmtorast rasttopnm

ifeq ($(HAVE_PNGLIB),Y)
  BINARIES += pnmtopng pngtopnm pngtopam pamrgbatopng
endif
ifneq ($(JPEGLIB),NONE)
  BINARIES += jpegtopnm pnmtojpeg
endif
ifneq ($(TIFF_PREREQ_MISSING),Y)
  BINARIES += tifftopnm pamtotiff pnmtotiffcmyk
endif
ifneq ($(URTLIB),NONE)
  BINARIES += rletopnm pnmtorle
endif
ifneq ($(ZLIB),NONE)
  BINARIES += pnmtops
endif

ifneq ($(XML2_LIBS),NONE)
  BINARIES += svgtopam
endif 

MERGEBINARIES = $(BINARIES)

EXTRA_OBJECTS = exif.o rast.o bmepsoe.o
ifeq ($(HAVE_PNGLIB),Y)
  EXTRA_OBJECTS += pngtxt.o
endif
ifneq ($(JPEGLIB),NONE)
  EXTRA_OBJECTS += jpegdatasource.o
endif
ifneq (($TIFF_PREREQ_MISSING),Y)
  EXTRA_OBJECTS += tiff.o
endif

OBJECTS = $(BINARIES:%=%.o) $(EXTRA_OBJECTS)
MERGE_OBJECTS = $(MERGEBINARIES:%=%.o2) $(EXTRA_OBJECTS)


SCRIPTS = anytopnm pnmtoplainpnm

.PHONY: all
all:	$(BINARIES) $(SUBDIRS:%=%/all)

include $(SRCDIR)/common.mk

ifeq ($(NEED_RUNTIME_PATH),Y)
  LIBOPTR = -runtime
else
  LIBOPTR =
endif

LIBOPTS_TIFF = $(shell $(LIBOPT) $(NETPBMLIB) \
  $(LIBOPTR) $(TIFFLIB) $(TIFFLIB_EXTRALIBS))

tifftopnm pamtotiff pnmtotiffcmyk: %: %.o tiff.o $(NETPBMLIB) $(LIBOPT)
	$(LD) -o $@ $@.o tiff.o \
	  $(LIBOPTS_TIFF) $(MATHLIB) $(LDFLAGS) $(LDLIBS) $(RPATH) $(LADD)

ifeq ($(shell libpng$(PNGVER)-config --version),)
  PNGLIB_LIBOPTS = $(shell $(LIBOPT) $(LIBOPTR) $(PNGLIB) $(ZLIB))
else
  PNGLIB_LIBOPTS = $(shell libpng$(PNGVER)-config --ldflags)
endif

pngtopnm pngtopam: %: %.o $(NETPBMLIB) $(LIBOPT)
	$(LD) -o $@ $@.o \
	  $(shell $(LIBOPT) $(NETPBMLIB)) \
	  $(PNGLIB_LIBOPTS) $(MATHLIB) $(LDFLAGS) $(LDLIBS) $(RPATH) $(LADD)

pnmtopng: %: %.o pngtxt.o $(NETPBMLIB) $(LIBOPT)
	$(LD) -o $@ $@.o pngtxt.o \
	  $(shell $(LIBOPT) $(NETPBMLIB)) \
	  $(PNGLIB_LIBOPTS) $(MATHLIB) $(LDFLAGS) $(LDLIBS) $(RPATH) $(LADD)

pamrgbatopng: %: %.o $(NETPBMLIB) $(LIBOPT)
	$(LD) -o $@ $@.o \
	  $(shell $(LIBOPT) $(NETPBMLIB)) $(PNGLIB_LIBOPTS) \
	  $(MATHLIB) $(LDFLAGS) $(LDLIBS) $(RPATH) $(LADD)

jpegtopnm: %: %.o jpegdatasource.o exif.o $(NETPBMLIB) $(LIBOPT)
	$(LD) -o $@ $< jpegdatasource.o exif.o \
	  $(shell $(LIBOPT) $(NETPBMLIB) $(LIBOPTR) $(JPEGLIB)) \
	  $(MATHLIB) $(LDFLAGS) $(LDLIBS) $(RPATH) $(LADD) 

pnmtojpeg: %: %.o $(NETPBMLIB) $(LIBOPT)
	$(LD) -o $@ $@.o \
	  $(shell $(LIBOPT) $(NETPBMLIB) $(LIBOPTR) $(JPEGLIB)) \
	  $(MATHLIB) $(LDFLAGS) $(LDLIBS) $(RPATH) $(LADD)

svgtopam: %: %.o $(NETPBMLIB) $(LIBOPT)
	$(LD) -o $@ $@.o \
	  $(shell $(LIBOPT) $(NETPBMLIB) $(LIBOPTR)) \
	  $(XML2_LIBS) $(MATHLIB) $(LDFLAGS) $(LDLIBS) $(RPATH) $(LADD)

# If URTLIB is BUNDLED_URTLIB, then we're responsible for building it, which
# means it needs to be a dependency:
ifeq ($(URTLIB), $(BUNDLED_URTLIB))
  URTLIBDEP = $(URTLIB)
endif

rletopnm pnmtorle: %: %.o $(NETPBMLIB) $(URTLIBDEP) $(LIBOPT)
	$(LD) -o $@ $@.o \
	  $(shell $(LIBOPT) $(URTLIB) $(NETPBMLIB)) \
	  $(MATHLIB) $(LDFLAGS) $(LDLIBS) $(RPATH) $(LADD)

pnmtops: %: %.o bmepsoe.o $(NETPBMLIB) $(LIBOPT)
	$(LD) -o $@ $@.o bmepsoe.o \
	  $(shell $(LIBOPT) $(NETPBMLIB) $(ZLIB)) \
	  $(MATHLIB) $(LDFLAGS) $(LDLIBS) $(RPATH) $(LADD)

pnmtorast rasttopnm: %: %.o rast.o $(NETPBMLIB) $(LIBOPT)
	$(LD) -o $@ $@.o rast.o \
	  $(shell $(LIBOPT) $(NETPBMLIB)) \
	  $(MATHLIB) $(LDFLAGS) $(LDLIBS) $(RPATH) $(LADD)

bmptopnm.o bmptopnm.o2: bmp.h

pamtotga.o pamtotga.o2: tga.h

install.bin: install.bin.local
.PHONY: install.bin.local
install.bin.local: $(PKGDIR)/bin
# Remember that $(SYMLINK) might just be a copy command.
# backward compatibility: program used to be named pnmnoraw
	cd $(PKGDIR)/bin ; \
	$(SYMLINK) pnmtoplainpnm pnmnoraw
# backward compatibility: program used to be gemtopbm
	cd $(PKGDIR)/bin ; \
	$(SYMLINK) gemtopnm$(EXE) gemtopbm$(EXE)
# In October 2001, pnmtojpeg replaced ppmtojpeg
ifneq ($(JPEGLIB),NONE)
	cd $(PKGDIR)/bin ; \
	$(SYMLINK) pnmtojpeg$(EXE) ppmtojpeg$(EXE)
endif
# In March 2002, bmptopnm replaced bmptoppm
	cd $(PKGDIR)/bin ; \
	$(SYMLINK) bmptopnm$(EXE) bmptoppm$(EXE)
# In May 2002, pamtouil replaced ppmtouil
	cd $(PKGDIR)/bin ; \
	$(SYMLINK) pamtouil$(EXE) ppmtouil$(EXE)
# In March 2005, we realized that pamtopnm obviates pnmtopnm
	cd $(PKGDIR)/bin ; \
	$(SYMLINK) pamtopnm$(EXE) pnmtopnm$(EXE)
# In October 2005, pamtofits replaced pnmtofits
	cd $(PKGDIR)/bin ; \
	$(SYMLINK) pamtofits$(EXE) pnmtofits$(EXE)
ifneq ($(TIFF_PREREQ_MISSING),Y)
# In October 2005, pamtotiff replaced pnmtotiff
	cd $(PKGDIR)/bin ; \
	$(SYMLINK) pamtotiff$(EXE) pnmtotiff$(EXE)
endif
