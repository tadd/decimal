Gem::Specification.new do |s|
  s.name = "decimal"
  s.version = "0.1.1"
  s.date = "2014-10-28"
  s.summary = "(yet another) multi-precision decimal arithmetic library"
  s.homepage = "http://github.com/tadd/decimal"
  s.description = <<-EOS.split("\n").map{|l|l.lstrip}.join(" ")
    Decimal is (yet another) multi-precision decimal arithmetic library.  It
    provides simple, compact, fast, precise, stable and easy-to-use solution.
  EOS
  s.extensions = "extconf.rb"
  s.files = %w(.document COPYING BSDL INSTALL README.1st README.rdoc TODO decimal.c
               decimal.gemspec depend extconf.rb inum193.h inum200.h inum21.h inum22.h
               ruby18compat.h lib/decimal.rb lib/decimal/math.rb
               test_decimal.rb)
  s.has_rdoc = true
  s.rdoc_options = %w(-t Decimal -m README.rdoc)
  s.extra_rdoc_files = %w(README.rdoc decimal.c)
  s.authors = "Tadashi Saito"
  s.email = "tad.a.digger@gmail.com"
  s.license = 'Ruby'
  s.post_install_message = "\n\t\t" << IO.read("README.1st") << "\n"
end
