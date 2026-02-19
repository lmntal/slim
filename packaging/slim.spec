# RPM spec file for SLIM
Name:           slim-lmntal
Version:        2.8.0
Release:        1%{?dist}
Summary:        Runtime and model checker for the LMNtal language

License:        GPLv2+
URL:            https://www.ueda.info.waseda.ac.jp/lmntal
Source0:        %{name}-%{version}.tar.gz

BuildRequires:  cmake >= 3.16
BuildRequires:  gcc-c++
BuildRequires:  bison >= 3.0
BuildRequires:  flex
BuildRequires:  re2c
BuildRequires:  ruby

%description
SLIM (Slim LMNtal IMprementation) is a runtime and model checker 
for the LMNtal (pronounced "elemental") language, a modeling 
language based on hierarchical graph rewriting.

%prep
%setup -q

%build
mkdir -p build
cd build
%cmake .. -DCMAKE_BUILD_TYPE=Release
%cmake_build

%install
cd build
%cmake_install

%check
cd build
%ctest

%files
%doc README.md NEWS CLAUDE.md
%license COPYING
%{_bindir}/slim
%{_mandir}/man1/slim.1*

%changelog
* Thu Jan 01 2025 Developer <dev@example.com> - 2.8.0-1
- Initial RPM package with CMake build system