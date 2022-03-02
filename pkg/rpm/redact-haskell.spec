Name:          redact-haskell
Version:       {{VERSION}}
Release:       1%{?dist}
Summary:       Hide secret text on the terminal
License:       MIT
URL:           https://github.com/ExtremaIS/redact-haskell
Source0:       redact-haskell-{{VERSION}}.tar.xz
BuildArch:     {{ARCH}}
BuildRequires: make
Requires:      glibc,gmp
#ExcludeArch:

%description
redact is a utility for hiding secret text on the terminal.  It is designed to
work with Markdown syntax.  Inline code (text enclosed in backticks) and code
blocks that are fenced with backticks are hidden.

%global debug_package %{nil}

%prep
%setup -q

%build

%install
make install DESTDIR=%{buildroot} PREFIX=/usr

%check
make test

%files
%{_bindir}/redact
%{_mandir}/man1/redact.1.gz
%{_datadir}/doc/%{name}/

%changelog
* {{DATE}} {{RPMFULLNAME}} <{{RPMEMAIL}}> - {{VERSION}}-1
- Release {{VERSION}}
