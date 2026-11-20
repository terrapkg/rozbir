module Arbitrary (SpecText (..), AnyText (..)) where

import Data.Text (Text)
import Data.Text qualified as T
import Test.QuickCheck

newtype SpecText = SpecText Text
  deriving stock (Show)

newtype AnyText = AnyText Text
  deriving stock (Show)

instance Arbitrary AnyText where
  arbitrary = AnyText . T.pack <$> listOf (elements "%{}()[]\\\n\r\t :?!#*-ab1\"'")
  shrink (AnyText t) = AnyText . T.pack <$> shrink (T.unpack t)

instance Arbitrary SpecText where
  arbitrary = SpecText . T.concat <$> listOf piece
  shrink (SpecText t) = SpecText . T.pack <$> shrink (T.unpack t)

piece :: Gen Text
piece =
  frequency
    [ (4, elements ["Name: foo", "Version:\t%{ver}", "Release: 1%{?dist}", "Requires(post): bar", "BuildRequires: pkgconfig(x) >= 1.0"]),
      (3, elements ["\n", "\r\n", " \\\n", "\n\n"]),
      (3, elements ["%global ver 1.2.3", "%define foo(a:) %{-a*}", "%undefine foo", "%bcond tests 1", "%bcond_without docs"]),
      (3, elements ["%if 0%{?fedora} >= 40", "%ifarch x86_64 %{arm}", "%else", "%elif %{with docs}", "%endif", "  %endif"]),
      (3, elements ["%package devel", "%description -n foo", "%files -f list", "%prep", "%build -a", "%changelog", "%post -p /sbin/ldconfig"]),
      (3, elements ["%{name}", "%{?foo:bar}", "%{!?x:%{error:y}}", "%(echo hi)", "%[1 + 2]", "%{lua: print(1)}", "%%", "%", "%{", "%(", "%[", "%{expand:%%{a}}"]),
      (2, elements ["%(c=%{commit}; echo ${c:0:7})", "%(echo %{ver} | sed 's/-/~/g;s/^v//')", "%(echo x | tr -d 'x')", "%(echo a | sed -E 's/(a|b)+/x/')", "%(v='a b'; echo ${v//a/} | cut -d' ' -f2)", "%(echo $HOME)", "%(sed 's/[/x/')"]),
      (2, elements ["# comment %{x}", "* Mon Jan 01 2024 A <a@b> - 1-1", "- item", "make %{?_smp_mflags}"]),
      (2, T.pack <$> listOf1 (elements "abc {}()[]:%\\#"))
    ]
