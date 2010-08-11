Gem::Specification.new do |s|
  s.name = "decimal"
  s.rubyforge_project = s.name
  s.version = "0.0.91.pre"
  s.date = "2010-08-12"
  s.summary = "(yet another) multi-precision decimal arithmetic library"
  s.homepage = "http://decimal.rubyforge.org/"
  s.description = <<-EOS.split("\n").map{|l|l.lstrip}.join(" ")
    Decimal is (yet another) multi-precision decimal arithmetic library.  It
    provides simple, compact, fast, precise, stable and easy-to-use solution.
  EOS
  s.extensions = "extconf.rb"
  s.files = %w(.document COPYING GPL INSTALL README.1st README TODO decimal.c
  	       decimal.gemspec depend extconf.rb inum18.h inum191.h inum192.h
	       ruby18compat.h lib/decimal.rb lib/decimal/math.rb
	       test_decimal.rb)
  s.has_rdoc = true
  s.rdoc_options = %w(-t Decimal -m README)
  s.extra_rdoc_files = %w(README decimal.c)
  s.authors = "Tadashi Saito"
  s.email = "tad.a.digger@gmail.com"
  s.post_install_message = "\n\t\t" << IO.read("README.1st") << "\n"
end
