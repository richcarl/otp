# overall flavour control
%bcond_with developer

# legacy build options
%bcond_with doc
%bcond_with java
%bcond_with odbc
%bcond_with tcl
%bcond_with wx

%if %{with developer}
%global FLAVOUR developer
%else
%global FLAVOUR live
%endif

Summary:	Kred Erlang OTP package
Name:		kred_erlang_%{FLAVOUR}
Version:	%{version}
Release:	%{revision}%{?dist}
Source:		kred_erlang-%{version}-%{revision}.tar.gz
License:	ERPL
Group:		Development/Languages
Packager:	Klarna AB, Kred Core
Conflicts:	erlang, klarna_erlang

# lib/crypto-*/priv/obj/crypto.o lacks build ID note for some reason
# lib/runtime_tools-*/priv/obj/dyntrace.o: ditto
%undefine _missing_build_ids_terminate_build

BuildRequires:	ncurses-devel
BuildRequires:	openssl-devel
BuildRequires:	zlib-devel
BuildRequires:	flex
BuildRequires:	m4

%if %{with doc}
BuildRequires:	fop
BuildRequires:	libxslt
%endif

%if %{with java}
BuildRequires:	java-devel
%endif

%if %{with odbc}
BuildRequires:	unixODBC-devel
%endif

%if %{with tcl}
BuildRequires:	tcl-devel
BuildRequires:	tk-devel
Requires:	tk
%endif

%if %{with wx}
BuildRequires:	wxGTK-devel
%endif

Vendor: erlang

%description
This is the Erlang OTP package for Kred

%prep
%setup -q -n kred_erlang-%{version}-%{revision}

%build
./otp_build autoconf
# redhat-rpm-config in F20 passes options in CFLAGS that aren't in vanilla GCC
CFLAGS=`echo ${CFLAGS:-%optflags} | sed 's,-fstack-protector-strong --param=ssp-buffer-size=4,,'`
%configure \
%if !%{with developer}
    --without-common_test \
    --without-dialyzer \
    --without-edoc \
    --without-eunit \
    --without-tools \
%endif
    --without-debugger \
    --without-diameter \
    --without-eldap \
    --without-erl_docgen \
    --without-et \
%if !%{with java}
    --without-jinterface \
%endif
    --without-megaco \
    --without-observer \
%if !%{with odbc}
    --without-odbc \
%endif
    --without-os_mon \
    --without-otp_mibs \
    --without-reltool \
    --without-snmp \
%if !%{with wx}
    --without-wx \
%endif
    --disable-hipe
rm -f .githash; echo %{git_rev} > .githash
make

%install
rm -rf $RPM_BUILD_ROOT

make DESTDIR=$RPM_BUILD_ROOT install

# remove outdated script
rm -f $RPM_BUILD_ROOT%{_libdir}/erlang/Install

# replace identical executables with symlinks
for exe in $RPM_BUILD_ROOT%{_libdir}/erlang/erts-*/bin/*
do
	base="$(basename "$exe")"
	next="$RPM_BUILD_ROOT%{_libdir}/erlang/bin/${base}"
	rel="$(echo "$exe" | sed "s,^$RPM_BUILD_ROOT%{_libdir}/erlang/,../,")"
	if cmp "$exe" "$next"; then
		ln -sf "$rel" "$next"
	fi
done
for exe in $RPM_BUILD_ROOT%{_libdir}/erlang/bin/*
do
	base="$(basename "$exe")"
	next="$RPM_BUILD_ROOT%{_bindir}/${base}"
	rel="$(echo "$exe" | sed "s,^$RPM_BUILD_ROOT,,")"
	if cmp "$exe" "$next"; then
		ln -sf "$rel" "$next"
	fi
done

# make erl_call easily available (needed by upgrade_test)
next=`echo $RPM_BUILD_ROOT%{_libdir}/erlang/lib/erl_interface-*/bin/erl_call`
rel=`echo "${next}" | sed "s,^$RPM_BUILD_ROOT%{_libdir}/erlang/,../,"`
ln -s "${rel}" $RPM_BUILD_ROOT%{_libdir}/erlang/bin/
ln -s %{_libdir}/erlang/bin/erl_call $RPM_BUILD_ROOT%{_bindir}/

%clean
rm -rf $RPM_BUILD_ROOT

%files
%defattr(-,root,root)
%{_libdir}/erlang/*
%{_bindir}/*
