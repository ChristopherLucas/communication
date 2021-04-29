dnl  Require autoconf >= 2.13
AC_PREREQ(2.13)

AC_INIT([speech], 0.0.0)
AC_CONFIG_SRCDIR([src])
AC_LANG(C++)
AC_LANG(C)
AC_REQUIRE_CPP
AC_PROG_CXX
ACX_PTHREAD 

dnl extra variables
AC_SUBST(PORTAUDIO_SRCS_TEMPLATE)
AC_SUBST(PORTAUDIO_LIBS)
AC_SUBST(PORTAUDIO_DEFINES_INCLUDES)
AC_SUBST(PORTAUDIO_CFLAGS)
AC_SUBST(MACOSX_DEPLOYMENT_TARGET)
MACOSX_DEPLOYMENT_TARGET="10.11"
dnl if test "$CI" != "0"; then
	MACOSX_DEPLOYMENT_TARGET="11.0"
dnl fi
AC_MSG_RESULT("CI = $CI   MACOSX_DEPLOYMENT_TARGET = $MACOSX_DEPLOYMENT_TARGET")
dnl This must be one of the first tests we do or it will fail...
AC_C_BIGENDIAN
if [[ $ac_cv_c_bigendian = "yes" ]] ; then
   PORTAUDIO_DEFINES_INCLUDES="$PORTAUDIO_DEFINES_INCLUDES -DPA_BIG_ENDIAN"
else
   PORTAUDIO_DEFINES_INCLUDES="$PORTAUDIO_DEFINES_INCLUDES -DPA_LITTLE_ENDIAN"
fi

dnl Specify options

AC_ARG_WITH(alsa,
            AS_HELP_STRING([--with-alsa], [Enable support for ALSA @<:@autodetect@:>@]),
            [with_alsa=$withval])

AC_ARG_WITH(jack,
            AS_HELP_STRING([--with-jack], [Enable support for JACK @<:@autodetect@:>@]),
            [with_jack=$withval])

AC_ARG_WITH(oss,
            AS_HELP_STRING([--with-oss], [Enable support for OSS @<:@autodetect@:>@]),
            [with_oss=$withval])

AC_ARG_WITH(asihpi,
            AS_HELP_STRING([--with-asihpi], [Enable support for ASIHPI @<:@autodetect@:>@]),
            [with_asihpi=$withval])

AC_ARG_WITH(winapi,
            AS_HELP_STRING([--with-winapi],
                           [Select Windows API support (@<:@wmme|directx|asio|wasapi|wdmks@:>@@<:@,...@:>@) @<:@wmme@:>@]),
            [with_winapi=$withval], [with_winapi="wmme"])
case "$R_PLATFORM" in *mingw* | *cygwin*)
     with_wmme=no
     with_directx=no
     with_asio=no
     with_wasapi=no
     with_wdmks=no
     for api in $(echo $with_winapi | sed 's/,/ /g'); do
         case "$api" in
             wmme|directx|asio|wasapi|wdmks)
                 eval with_$api=yes
                 ;;
             *)
                 AC_MSG_ERROR([unknown Windows API \"$api\" (do you need --help?)])
                 ;;
         esac
     done
     ;;
esac

AC_ARG_WITH(asiodir,
            AS_HELP_STRING([--with-asiodir], [ASIO directory @<:@/usr/local/asiosdk2@:>@]),
            with_asiodir=$withval, with_asiodir="/usr/local/asiosdk2")

AC_ARG_WITH(dxdir,
            AS_HELP_STRING([--with-dxdir], [DirectX directory @<:@/usr/local/dx7sdk@:>@]),
            with_dxdir=$withval, with_dxdir="/usr/local/dx7sdk")

AC_ARG_ENABLE(cxx,
              AS_HELP_STRING([--enable-cxx], [Enable C++ bindings @<:@no@:>@]),
              enable_cxx=$enableval, enable_cxx="no")

AC_ARG_ENABLE(mac-debug,
              AS_HELP_STRING([--enable-mac-debug], [Enable Mac debug @<:@no@:>@]),
              enable_mac_debug=$enableval, enable_mac_debug="no")

AC_ARG_ENABLE(mac-universal,
              AS_HELP_STRING([--enable-mac-universal], [Build Mac universal binaries @<:@yes@:>@]),
              enable_mac_universal=$enableval, enable_mac_universal="yes")

dnl Continue to accept --host_os for compatibility but do not document
dnl it (the correct way to change host_os is with --host=...).  Moved
dnl here because the empty help string generates a blank line which we
dnl can use to separate PA options from libtool options.
dnl AC_ARG_WITH(host_os, [], host_os=$withval)

dnl Checks for programs.

dnl ASIO and CXX bindings need a C++ compiler
if [[ "$with_asio" = "yes" ] || [ "$enable_cxx" = "yes" ]] ; then
       AC_PROG_CXX
fi

dnl AC_PROG_LIBTOOL
dnl AC_PROG_LN_S

dnl checks for various host APIs and arguments to configure that
dnl turn them on or off

have_alsa=no
if test "x$with_alsa" != "xno"; then
    AC_CHECK_LIB(asound, snd_pcm_open, have_alsa=yes, have_alsa=no)
fi
have_asihpi=no
if test "x$with_asihpi" != "xno"; then
    AC_CHECK_LIB(hpi, HPI_SubSysCreate, have_asihpi=yes, have_asihpi=no, -lm)
fi
have_libossaudio=no
have_oss=no
if test "x$with_oss" != "xno"; then
    AC_CHECK_HEADERS([sys/soundcard.h linux/soundcard.h machine/soundcard.h], [have_oss=yes])
    if test "x$have_oss" = "xyes"; then
        AC_CHECK_LIB(ossaudio, _oss_ioctl, have_libossaudio=yes, have_libossaudio=no)
    fi
fi
dnl have_jack=no
dnl if test "x$with_jack" != "xno"; then
dnl       PKG_CHECK_MODULES(JACK, jack, have_jack=yes, have_jack=no)
dnl fi

dnl sizeof checks: we will need a 16-bit and a 32-bit type

AC_CHECK_SIZEOF(short)
AC_CHECK_SIZEOF(int)
AC_CHECK_SIZEOF(long)

save_LIBS="${LIBS}"
AC_CHECK_LIB(rt, clock_gettime, [rt_libs=" -lrt"])
LIBS="${LIBS}${rt_libs}"
DLL_LIBS="${DLL_LIBS}${rt_libs}"
AC_CHECK_FUNCS([clock_gettime nanosleep])
LIBS="${save_LIBS}"


dnl Include directories needed by all implementations
PORTAUDIO_DEFINES_INCLUDES="$PORTAUDIO_DEFINES_INCLUDES -I portaudio/include -I portaudio/src/common"

case "${R_PLATFORM}" in
  *darwin*)
        dnl Mac OS X configuration

        AC_DEFINE(PA_USE_COREAUDIO,1)

        PORTAUDIO_DEFINES_INCLUDES="$PORTAUDIO_DEFINES_INCLUDES -I portaudio/src/os/unix"
        CFLAGS="$CFLAGS -I portaudio/src/os/unix"
        PORTAUDIO_LIBS="$PORTAUDIO_LIBS -framework CoreAudio -framework AudioToolbox -framework AudioUnit -framework Carbon"

        if test "x$enable_mac_universal" = "xyes" ; then
					 mac_sysroot="-isysroot $(xcrun --sdk macosx --show-sdk-path)"

           dnl Pick which architectures to build for based on what
           dnl the compiler supports.
           mac_arches=""
           for arch in i386 x86_64 ppc ppc64
           do
              save_CFLAGS="$CFLAGS"
              CFLAGS="$CFLAGS -arch $arch"
              AC_TRY_COMPILE([], [return 0;],
                 [
                    if [[ -z "$mac_arches" ]] ; then
                       mac_arches="-arch $arch"
                    else
                       mac_arches="$mac_arches -arch $arch"
                    fi
                 ])
              CFLAGS="$save_CFLAGS"
           done
        else
           mac_arches=""
           mac_sysroot=""
           mac_version=""
        fi
        PORTAUDIO_LIBS="$PORTAUDIO_LIBS -dynamiclib $mac_arches $mac_sysroot" dnl "$mac_version_min"
        dnl PORTAUDIO_CFLAGS="-std=c99 $PORTAUDIO_CFLAGS $mac_arches $mac_sysroot $mac_version_min"
        PORTAUDIO_CFLAGS="-stdlib=libc++ $PORTAUDIO_CFLAGS $mac_arches $mac_sysroot" dnl "$mac_version_min"

        PORTAUDIO_SRCS_TEMPLATE="$PORTAUDIO_SRCS_TEMPLATE portaudio/src/os/unix/pa_unix_hostapis.c portaudio/src/os/unix/pa_unix_util.c portaudio/src/hostapi/coreaudio/pa_mac_core_blocking.c portaudio/src/hostapi/coreaudio/pa_mac_core_utilities.c portaudio/src/hostapi/coreaudio/pa_mac_core.c"
        ;;

          *mingw*)
                dnl MingW configuration

                CFLAGS="$CFLAGS -mthreads"
                PORTAUDIO_CFLAGS="$PORTAUDIO_CFLAGS -mthreads"

                PORTAUDIO_DEFINES_INCLUDES="$PORTAUDIO_DEFINES_INCLUDES -I portaudio/src/os/win -DPA_USE_WMME=0 -DPA_USE_ASIO=0 -DPA_USE_WDMKS=0 -DPA_USE_DS=0 -DPA_USE_WASAPI=0"

                if [[ "x$with_directx" = "xyes" ]]; then
                    DXDIR="$with_dxdir"
                    PORTAUDIO_SRCS_TEMPLATE="$PORTAUDIO_SRCS_TEMPLATE portaudio/src/os/win/*.c portaudio/src/hostapi/dsound/*.c"
                    PORTAUDIO_LIBS="$PORTAUDIO_LIBS -lwinmm -lm -ldsound -lole32"
                    PORTAUDIO_DEFINES_INCLUDES="$PORTAUDIO_DEFINES_INCLUDES -I$DXDIR/include -UPA_USE_DS -DPA_USE_DS=1"
                    CFLAGS="$CFLAGS -I$DXDIR/include -UPA_USE_DS -DPA_USE_DS=1"
                fi

                if [[ "x$with_asio" = "xyes" ]]; then
                    ASIODIR="$with_asiodir"
                    PORTAUDIO_SRCS_TEMPLATE="$PORTAUDIO_SRCS_TEMPLATE portaudio/src/os/win/*.c portaudio/src/hostapi/wdmks/*.c"
                    dnl add_objects src/hostapi/asio/pa_asio.o src/common/pa_ringbuffer.o src/os/win/pa_win_hostapis.o src/os/win/pa_win_util.o src/os/win/pa_win_coinitialize.o src/hostapi/asio/iasiothiscallresolver.o $ASIODIR/common/asio.o $ASIODIR/host/asiodrivers.o $ASIODIR/host/pc/asiolist.o
                    PORTAUDIO_LIBS="$PORTAUDIO_LIBS -lwinmm -lm -lole32 -luuid"
                    PORTAUDIO_DEFINES_INCLUDES="$PORTAUDIO_DEFINES_INCLUDES -ffast-math -fomit-frame-pointer -I\$(top_srcdir)/src/hostapi/asio -I$ASIODIR/host/pc -I$ASIODIR/common -I$ASIODIR/host -UPA_USE_ASIO -DPA_USE_ASIO=1 -DWINDOWS"
                    CFLAGS="$CFLAGS -ffast-math -fomit-frame-pointer -I\$(top_srcdir)/src/hostapi/asio -I$ASIODIR/host/pc -I$ASIODIR/common -I$ASIODIR/host -UPA_USE_ASIO -DPA_USE_ASIO=1 -DWINDOWS"

                    dnl Setting the windows version flags below resolves a conflict between Interlocked*
                    dnl definitions in mingw winbase.h and Interlocked* hacks in ASIO SDK combase.h
                    dnl combase.h is included by asiodrvr.h
                    dnl PortAudio does not actually require Win XP (winver 501) APIs
                    CFLAGS="$CFLAGS -D_WIN32_WINNT=0x0501 -DWINVER=0x0501"
                    PORTAUDIO_DEFINES_INCLUDES="$PORTAUDIO_DEFINES_INCLUDES -D_WIN32_WINNT=0x0501 -DWINVER=0x0501"
                    CXXFLAGS="$CFLAGS"
                fi

                if [[ "x$with_wdmks" = "xyes" ]]; then
                    DXDIR="$with_dxdir"
                    PORTAUDIO_SRCS_TEMPLATE="$PORTAUDIO_SRCS_TEMPLATE portaudio/src/os/win/*.c portaudio/src/hostapi/wdmks/*.c"
                    PORTAUDIO_LIBS="$PORTAUDIO_LIBS -lwinmm -lm -luuid -lsetupapi -lole32"
                    #VC98="\"/c/Program Files/Microsoft Visual Studio/VC98/Include\""
                    #CFLAGS="$CFLAGS -I$VC98 -DPA_NO_WMME -DPA_NO_ASIO"
                    CFLAGS="$CFLAGS -I$DXDIR/include -UPA_USE_WDMKS -DPA_USE_WDMKS=1"
                    PORTAUDIO_DEFINES_INCLUDES="$PORTAUDIO_DEFINES_INCLUDES -I$DXDIR/include -UPA_USE_WDMKS -DPA_USE_WDMKS=1"
                fi

                if [[ "x$with_wmme" = "xyes" ]]; then
                    PORTAUDIO_SRCS_TEMPLATE="$PORTAUDIO_SRCS_TEMPLATE portaudio/src/os/win/*.c portaudio/src/hostapi/wmme/*.c"
                    PORTAUDIO_LIBS="$PORTAUDIO_LIBS -lwinmm -lm -lole32 -luuid"
                    PORTAUDIO_DEFINES_INCLUDES="$PORTAUDIO_DEFINES_INCLUDES -UPA_USE_WMME -DPA_USE_WMME=1"
                    CFLAGS="$CFLAGS -UPA_USE_WMME -DPA_USE_WMME=1"
                fi

                if [[ "x$with_wasapi" = "xyes" ]]; then
                    PORTAUDIO_SRCS_TEMPLATE="$PORTAUDIO_SRCS_TEMPLATE portaudio/src/os/win/*.c portaudio/src/hostapi/wasapi/*.c"
                    PORTAUDIO_LIBS="$PORTAUDIO_LIBS -lwinmm -lm -lole32 -luuid"
                    PORTAUDIO_DEFINES_INCLUDES="$PORTAUDIO_DEFINES_INCLUDES -I portaudio/src/hostapi/wasapi/mingw-include -UPA_USE_WASAPI -DPA_USE_WASAPI=1"
                    CFLAGS="$CFLAGS -I\$(top_srcdir)/src/hostapi/wasapi/mingw-include -UPA_USE_WASAPI -DPA_USE_WASAPI=1"
                fi
                ;;


  *)
        dnl Unix configuration
        PORTAUDIO_DEFINES_INCLUDES="$PORTAUDIO_DEFINES_INCLUDES -I portaudio/src/os/unix"

        AC_CHECK_LIB(pthread, pthread_create,[have_pthread="yes"],
                AC_MSG_ERROR([libpthread not found!]))

        if [[ "$have_alsa" = "yes" ] && [ "$with_alsa" != "no" ]] ; then
                PORTAUDIO_LIBS="$PORTAUDIO_LIBS -lasound"
                PORTAUDIO_SRCS_TEMPLATE="$PORTAUDIO_SRCS_TEMPLATE portaudio/src/hostapi/alsa/pa_linux_alsa.c"
                PORTAUDIO_DEFINES_INCLUDES="$PORTAUDIO_DEFINES_INCLUDES -DPA_USE_ALSA=1"
        fi

        dnl if [[ "$have_jack" = "yes" ] && [ "$with_jack" != "no" ]] ; then
        dnl    PORTAUDIO_LIBS="$PORTAUDIO_LIBS $JACK_LIBS"
        dnl    PORTAUDIO_CFLAGS="$PORTAUDIO_CFLAGS $JACK_CFLAGS"
        dnl    PORTAUDIO_SRCS_TEMPLATE="$PORTAUDIO_SRCS_TEMPLATE portaudio/src/hostapi/jack/pa_jack.c"
        dnl    PORTAUDIO_DEFINES_INCLUDES="$PORTAUDIO_DEFINES_INCLUDES -DPA_USE_JACK=1"
        dnl fi

        if [[ "$with_oss" != "no" ]] ; then
           PORTAUDIO_SRCS_TEMPLATE="$PORTAUDIO_SRCS_TEMPLATE portaudio/src/hostapi/oss/pa_unix_oss.c"
           if [[ "$have_libossaudio" = "yes" ]] ; then
                   PORTAUDIO_LIBS="$PORTAUDIO_LIBS -lossaudio"
           fi
           PORTAUDIO_DEFINES_INCLUDES="$PORTAUDIO_DEFINES_INCLUDES -DPA_USE_OSS=1"
        fi

        if [[ "$have_asihpi" = "yes" ] && [ "$with_asihpi" != "no" ]] ; then
           PORTAUDIO_LIBS="$PORTAUDIO_LIBS -lhpi"
           PORTAUDIO_SRCS_TEMPLATE="$PORTAUDIO_SRCS_TEMPLATE portaudio/src/hostapi/asihpi/pa_linux_asihpi.c"
           PORTAUDIO_DEFINES_INCLUDES="$PORTAUDIO_DEFINES_INCLUDES -DPA_USE_ASIHPI=1"
        fi

        PORTAUDIO_LIBS="$PORTAUDIO_LIBS -lm -pthread"

        ## support sun cc compiler flags
        case "${R_PLATFORM}" in
           solaris*)
              CFLAGS="$CFLAGS -mt"
              PORTAUDIO_CFLAGS="$PORTAUDIO_CFLAGS -mt"
              ;;
           *)
              CFLAGS="$CFLAGS -pthread"
              PORTAUDIO_CFLAGS="$PORTAUDIO_CFLAGS -pthread"
              ;;
        esac

        PORTAUDIO_SRCS_TEMPLATE="$PORTAUDIO_SRCS_TEMPLATE portaudio/src/os/unix/pa_unix_hostapis.c portaudio/src/os/unix/pa_unix_util.c"
esac

dnl create and report output
AC_CONFIG_FILES([src/Makevars])
AC_OUTPUT
echo
echo "Final src/Makevars"
cat src/Makevars


AC_MSG_RESULT([
Configuration summary:
  
  Target....................... $R_PLATFORM] )

case "${R_PLATFORM}" in *linux*)
    AC_MSG_RESULT([
  ALSA ........................ $have_alsa
  ASIHPI ...................... $have_asihpi])
    ;;
esac
case "${R_PLATFORM}" in
    *mingw* | *cygwin*)
        test "x$with_directx" = "xyes" && with_directx="$with_directx (${with_dxdir})"
        test "x$with_wdmks" = "xyes" && with_wdmks="$with_wdmks (${with_dxdir})"
        test "x$with_asio" = "xyes" && with_asio="$with_asio (${with_asiodir})"
        test "x$with_wasapi" = "xyes"
        AC_MSG_RESULT([
  WMME ........................ $with_wmme
  DSound ...................... $with_directx
  ASIO ........................ $with_asio
  WASAPI ...................... $with_wasapi
  WDMKS ....................... $with_wdmks
])
        ;;
    *darwin*)
        AC_MSG_RESULT([
  Mac debug flags ............. $enable_mac_debug
])
        ;;
     *)
	AC_MSG_RESULT([
  OSS ......................... $have_oss
])
        ;;
esac
