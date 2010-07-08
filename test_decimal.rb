require 'decimal'
require 'test/unit'

class TestDecimal < Test::Unit::TestCase
  ONE = Decimal(1)
  ZERO = Decimal(0)
  INFINITY = Decimal::INFINITY 
  NAN = Decimal::NAN
  
  def test_initialize
    assert_nothing_raised {Decimal(1)}
    assert_nothing_raised {Decimal(2**64)}
    assert_nothing_raised {Decimal("1")}
    assert_nothing_raised {Decimal("1.1")}
    assert_nothing_raised {Decimal("1e10")}
    assert_nothing_raised {Decimal("299_792_458")}
    assert_nothing_raised {Decimal("2.99_792_458e8")}
    assert_raise(ArgumentError) {Decimal(1.1)}
  end

  def test_stringize
    assert_equal("1", ONE.to_s)
    assert_equal("1", Decimal("1").to_s)
    assert_equal("1.1", Decimal("1.1").to_s)
    assert_equal("18446744073709551616", Decimal(2**64).to_s)
    assert_equal("10000000000", Decimal("1e10").to_s)
    assert_equal("299792458", Decimal("299_792_458").to_s)
    assert_equal("299792458", Decimal("2.99_792_458e8").to_s)
    assert_equal("Infinity", INFINITY.to_s)
    assert_equal("-Infinity", (-INFINITY).to_s)
    assert_equal("NaN", NAN.to_s)

    assert_equal("Decimal(1)", ONE.inspect)
    assert_equal("Decimal(1)", Decimal("1").inspect)
    assert_equal("Decimal(1.1)", Decimal("1.1").inspect)
    assert_equal("Decimal(18446744073709551616)", Decimal(2**64).inspect)
    assert_equal("Decimal(10000000000)", Decimal("1e10").inspect)
    assert_equal("Decimal(299792458)", Decimal("299_792_458").inspect)
    assert_equal("Decimal(299792458)", Decimal("2.99_792_458e8").inspect)
    assert_equal("Decimal(Infinity)", INFINITY.inspect)
    assert_equal("Decimal(-Infinity)", (-INFINITY).inspect)
    assert_equal("Decimal(NaN)", NAN.inspect)
  end

  def test_coerce
    assert_equal([Decimal(2), ONE], ONE.coerce(2))
    assert_equal([Decimal(2), ONE], ONE.coerce(Decimal(2)))
    assert_raise(TypeError) {ONE.coerce(2.5)}
  end

  def test_uminus
    assert_equal(Decimal(-1), -ONE)
    assert_equal(Decimal(-(2**64)), -Decimal(2**64))
    assert(Decimal("-0").eql?(-ZERO)) # should use Decimal#eql?,
    assert(ZERO.eql?(-Decimal("-0"))) # because `-0 == 0`
    assert(!ZERO.eql?(-ZERO))
    assert(!Decimal("-0").eql?(-Decimal("-0")))
    assert_not_equal(-NAN, -NAN)
    assert_equal(Decimal(-1).divide(0), -INFINITY)
    assert_equal(INFINITY, -(Decimal(-1).divide(0)))
  end

  def test_add
    assert_equal(Decimal("1.1"), Decimal("0.1") + 1)
    sum = 0
    10.times {sum += Decimal("0.1")}
    assert_equal(ONE, sum)
    assert_raise(TypeError) {Decimal("0.1") + 1.0}
  end

  def test_sub
    assert_equal(Decimal("0.1"), Decimal("1.1") - 1)
    sum = 0
    10.times {sum -= Decimal("0.1")}
    assert_equal(Decimal(-1), sum)
    assert_raise(TypeError) {Decimal("0.1") - 1.0}
  end

  def test_mul
    assert_equal(ONE, Decimal("0.1") * 10)
    sum = ONE
    10.times {sum *= Decimal("0.1")}
    assert_equal(Decimal("0.0000000001"), sum)
    assert_raise(TypeError) {Decimal("0.1") * 1.0}
  end

  def test_div
    assert_equal(Decimal("0.1"), ONE.divide(10, 1))
    sum = Decimal(1 << 10)
    10.times {sum = sum.divide(2)}
    assert_equal(ONE, sum)
    assert_raise(TypeError) {Decimal("0.1").divide(1.0)}
  end

  def test_idiv
    assert_equal(2, Decimal("11.5").div(4))
    assert_equal(-3, Decimal("11.5").div(-4))
    assert_equal(-3, Decimal("-11.5").div(4))
    assert_equal(2, Decimal("-11.5").div(-4))
    assert_raise(TypeError) {Decimal(11).div(4.0)}
    assert((Decimal::NAN.div(1)).nan?)
    assert((Decimal(1).div(Decimal::NAN)).nan?)
    assert((Decimal::INFINITY.div(1)).nan?)
  end

  def test_mod
    assert_equal(0, Decimal(1) % 1)
    assert_equal(Decimal("3.5"), Decimal("11.5") % 4)
    assert_equal(Decimal("-0.5"), Decimal("11.5") % -4)
    assert_equal(Decimal("0.5"), Decimal("-11.5") % 4)
    assert_equal(Decimal("-3.5"), Decimal("-11.5") % -4)
    assert_raise(TypeError) {Decimal(11) % 4.0}
    assert((Decimal::NAN % 1).nan?)
    assert((Decimal(1) % Decimal::NAN).nan?)    
    assert((Decimal::INFINITY % 1).nan?)
  end

  def test_pow
    assert_equal(1 << 10, Decimal(2) ** 10)
    assert_equal(1, ONE ** 10)
    assert_equal(Decimal("0.0000000001"), Decimal("0.1") ** 10)
    assert_raise(TypeError) {ONE ** 10.0}
  end

  def test_eq
    assert_equal(ONE, 1)
    assert_equal(Decimal("1.0"), 1)
    assert_equal(Decimal("100e-2"), 1)
    assert_equal(Decimal("0.01e2"), 1)

    assert(ONE.eql?(ONE))
    assert((-ONE).eql?(-ONE))
    assert(INFINITY.eql?(INFINITY))
    assert((-INFINITY).eql?(-INFINITY))
    assert(!ONE.eql?(Decimal("1.0")))

    assert_not_equal(ONE, 2**32)
    assert_not_equal(ONE, NAN)
    assert_not_equal(ONE, nil)
    assert_not_equal(ONE, 1.0)
  end

  def test_cmp
    assert_equal(0, ONE <=> ONE)
    assert_equal(1, ONE <=> ZERO)
    assert_equal(-1, ONE <=> Decimal(2))
    assert_nil(ONE <=> nil)
    assert_nil(ONE <=> NAN)
    assert_nil(NAN <=> ONE)
    assert_nil(ONE <=> 1.0)

    assert_equal(0, ONE <=> 1)
    assert_equal(1, ONE <=> 0)
    assert_equal(-1, ONE <=> 2)

    assert_equal(-1, ONE <=> 2**32)

    assert_raise(ArgumentError) {ONE > nil}
    assert_raise(ArgumentError) {ONE >= nil}
    assert_raise(ArgumentError) {ONE < nil}
    assert_raise(ArgumentError) {ONE <= nil}

    assert_raise(ArgumentError) {ONE > 1.0}
    assert_raise(ArgumentError) {ONE >= 1.0}
    assert_raise(ArgumentError) {ONE < 1.0}
    assert_raise(ArgumentError) {ONE <= 1.0}
  end

  def test_hash
    assert_equal(ONE.hash, ONE.hash)
    assert_equal(ZERO.hash, ZERO.hash)
    assert_equal(NAN.hash, NAN.hash)
    assert_equal(INFINITY.hash, INFINITY.hash)

    assert_not_equal(ONE.hash, Decimal("1.0").hash)
    assert_not_equal(ZERO.hash, Decimal("0.0").hash)
  end

  def test_abs
    assert_equal(Decimal(-1).abs, ONE)
    assert_equal(Decimal(-(2**64)).abs, 2**64)
    assert(Decimal("-0").abs.eql?(ZERO))
    assert_equal((-INFINITY).abs, INFINITY)
    assert_not_equal(NAN.abs, NAN)
  end

  def test_divmod
    assert_equal([2, Decimal("3.5")], Decimal("11.5").divmod(4))
    assert_equal([-3, Decimal("-0.5")], Decimal("11.5").divmod(-4))
    assert_equal([-3, Decimal("0.5")], Decimal("-11.5").divmod(4))
    assert_equal([2, Decimal("-3.5")], Decimal("-11.5").divmod(-4))
    assert_raise(TypeError) {Decimal(11).divmod(4.0)}
    div, mod = Decimal::NAN.divmod(1)
    assert(div.nan?)
    assert(mod.nan?)
    div, mod = Decimal(1).divmod(Decimal::NAN)
    assert(div.nan?)
    assert(mod.nan?)
    div, mod = Decimal::INFINITY.divmod(1)
    assert(div.nan?)
    assert(mod.nan?)
  end

  def test_div
    assert_equal(1, Decimal(1).div(1))
    assert_equal(2, Decimal("11.5").div(4))
    assert_equal(-3, Decimal("11.5").div(-4))
    assert_equal(-3, Decimal("-11.5").div(4))
    assert_equal(2, Decimal("-11.5").div(-4))
  end

  def test_remainder
    assert_equal(0, Decimal(1).remainder(1))
    assert_equal(Decimal("3.5"), Decimal("11.5").remainder(4))
    assert_equal(Decimal("3.5"), Decimal("11.5").remainder(-4))
    assert_equal(Decimal("-3.5"), Decimal("-11.5").remainder(4))
    assert_equal(Decimal("-3.5"), Decimal("-11.5").remainder(-4))
  end

  def test_to_f
    max, min = Float::MAX_10_EXP+10, Float::MIN_10_EXP-10
    assert_equal(Decimal("1e#{max}").to_f, 1.0/0.0)
    assert_equal(Decimal("-1e#{max}").to_f, -1.0/0.0)
    assert_equal(Decimal("1e#{min}").to_f, 0.0)
    assert_equal(Decimal("-1e#{min}").to_f, -0.0)
  end
end
