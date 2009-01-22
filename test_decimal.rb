require 'decimal'
require 'test/unit'

class TestDecimal < Test::Unit::TestCase
  INFINITY = Decimal(1).divide(0)
  NaN = Decimal(0).divide(0)
  ZERO = Decimal(0)
  
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
    assert_equal("1", Decimal(1).to_s)
    assert_equal("1", Decimal("1").to_s)
    assert_equal("1.1", Decimal("1.1").to_s)
    assert_equal("18446744073709551616", Decimal(2**64).to_s)
    assert_equal("10000000000", Decimal("1e10").to_s)
    assert_equal("299792458", Decimal("299_792_458").to_s)
    assert_equal("299792458", Decimal("2.99_792_458e8").to_s)
    assert_equal("Infinity", INFINITY.to_s)
    assert_equal("-Infinity", (-INFINITY).to_s)
    assert_equal("NaN", NaN.to_s)

    assert_equal("Decimal(1)", Decimal(1).inspect)
    assert_equal("Decimal(1)", Decimal("1").inspect)
    assert_equal("Decimal(1.1)", Decimal("1.1").inspect)
    assert_equal("Decimal(18446744073709551616)", Decimal(2**64).inspect)
    assert_equal("Decimal(10000000000)", Decimal("1e10").inspect)
    assert_equal("Decimal(299792458)", Decimal("299_792_458").inspect)
    assert_equal("Decimal(299792458)", Decimal("2.99_792_458e8").inspect)
    assert_equal("Decimal(Infinity)", INFINITY.inspect)
    assert_equal("Decimal(-Infinity)", (-INFINITY).inspect)
    assert_equal("Decimal(NaN)", NaN.inspect)
  end

  def test_coerce
    assert_equal([Decimal(2), Decimal(1)], Decimal(1).coerce(2))
    assert_equal([Decimal(2), Decimal(1)], Decimal(1).coerce(Decimal(2)))
    assert_raise(TypeError) {Decimal(1).coerce(2.5)}
  end

  def test_uminus
    assert_equal(Decimal(-1), -Decimal(1))
    assert_equal(Decimal(-(2**64)), -Decimal(2**64))
    assert(Decimal("-0").eql?(-ZERO)) # should use Decimal#eql?,
    assert(ZERO.eql?(-Decimal("-0"))) # because `-0 == 0`
    assert(!ZERO.eql?(-ZERO))
    assert(!Decimal("-0").eql?(-Decimal("-0")))
    assert_not_equal(-NaN, -NaN)
    assert_equal(Decimal(-1).divide(0), -INFINITY)
    assert_equal(INFINITY, -(Decimal(-1).divide(0)))
  end

  def test_add
    assert_equal(Decimal("1.1"), Decimal("0.1") + 1)
    sum = 0
    10.times {sum += Decimal("0.1")}
    assert_equal(Decimal(1), sum)
    assert_raise(TypeError) {Decimal("0.1") + 1.0}
  end

  def test_sub
    assert_equal(Decimal("0.1"), Decimal("1.1") - 1)
    sum = 0
    10.times {sum -= Decimal("0.1")}
    assert_equal(Decimal(-1), sum)
    assert_raise(TypeError) {Decimal("0.1") - 1.0}
  end
end
