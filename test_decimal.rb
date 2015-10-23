require 'decimal'
require 'test/unit'

class TestDecimal < Test::Unit::TestCase
  ONE = Decimal(1)
  ZERO = Decimal(0)
  INFINITY = Decimal::INFINITY 
  NAN = Decimal::NAN

  def assert_nan(a, *rest)
    rest = ["not nan: #{a.inspect}"] if rest.empty?
    assert(a.nan?, *rest)
  end

  def assert_infinity(a, *rest)
    rest = ["not infinity: #{a.inspect}"] if rest.empty?
    assert(a.infinite?, *rest)
  end

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

  def test_divide
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
    assert_nan(NAN.div(1))
    assert_nan(ONE.div(NAN))
    assert_nan(INFINITY.div(1))
  end

  def test_mod
    assert_equal(0, ONE % 1)
    assert_equal(Decimal("3.5"), Decimal("11.5") % 4)
    assert_equal(Decimal("-0.5"), Decimal("11.5") % -4)
    assert_equal(Decimal("0.5"), Decimal("-11.5") % 4)
    assert_equal(Decimal("-3.5"), Decimal("-11.5") % -4)
    assert_raise(TypeError) {Decimal(11) % 4.0}
    assert_nan(NAN % 1)
    assert_nan(ONE % Decimal::NAN)
    assert_nan(INFINITY % 1)
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
    assert_equal(-1, ZERO <=> ONE)
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
    div, mod = NAN.divmod(1)
    assert_nan(div)
    assert_nan(mod)
    div, mod = ONE.divmod(NAN)
    assert_nan(div)
    assert_nan(mod)
    div, mod = INFINITY.divmod(1)
    assert_nan(div)
    assert_nan(mod)
  end

  def test_div
    assert_equal(1, ONE.div(1))
    assert_equal(2, Decimal("11.5").div(4))
    assert_equal(-3, Decimal("11.5").div(-4))
    assert_equal(-3, Decimal("-11.5").div(4))
    assert_equal(2, Decimal("-11.5").div(-4))
  end

  def test_remainder
    assert_equal(0, ONE.remainder(1))
    assert_equal(Decimal("3.5"), Decimal("11.5").remainder(4))
    assert_equal(Decimal("3.5"), Decimal("11.5").remainder(-4))
    assert_equal(Decimal("-3.5"), Decimal("-11.5").remainder(4))
    assert_equal(Decimal("-3.5"), Decimal("-11.5").remainder(-4))
  end

  def test_to_f
    max, min = Float::MAX_10_EXP+10, Float::MIN_10_EXP-10
    assert_equal(1.0/0.0, Decimal("1e#{max}").to_f)
    assert_equal(-1.0/0.0, Decimal("-1e#{max}").to_f)
    assert_equal("0.0", Decimal("1e#{min}").to_f.to_s)
    assert_equal("-0.0", Decimal("-1e#{min}").to_f.to_s)
  end

  def test_zero_scale
    assert_equal('-0.0', (-Decimal('0.0')).to_s)

    scaled_zero = Decimal('0.0')
    scaled_one = Decimal('1.0')

    assert_equal("0.0", (ZERO + scaled_zero).to_s)
    assert_equal("0.0", (ZERO + -scaled_zero).to_s)
    assert_equal("0.0", (scaled_zero + ZERO).to_s)
    assert_equal("0.0", (-scaled_zero + ZERO).to_s)

    assert_equal("0.0", (-ZERO + scaled_zero).to_s)
    assert_equal("-0.0", (-ZERO + -scaled_zero).to_s)
    assert_equal("0.0", (scaled_zero + -ZERO).to_s)
    assert_equal("-0.0", (-scaled_zero + -ZERO).to_s)

    assert_equal("0.0", (ONE + -scaled_one).to_s)
    assert_equal("0.0", (-scaled_one + ONE).to_s)
    assert_equal("0.0", (-ONE + scaled_one).to_s)
    assert_equal("0.0", (scaled_one + -ONE).to_s)

    assert_equal("0.0", (ZERO - scaled_zero).to_s)
    assert_equal("0.0", (ZERO - -scaled_zero).to_s)
    assert_equal("0.0", (scaled_zero - ZERO).to_s)
    assert_equal("-0.0", (-scaled_zero - ZERO).to_s)

    assert_equal("-0.0", (-ZERO - scaled_zero).to_s)
    assert_equal("0.0", (-ZERO - -scaled_zero).to_s)
    assert_equal("0.0", (scaled_zero - -ZERO).to_s)
    assert_equal("0.0", (-scaled_zero - -ZERO).to_s)

    assert_equal("0.0", (ONE - scaled_one).to_s)
    assert_equal("0.0", (scaled_one - ONE).to_s)
    assert_equal("0.0", (-ONE - -scaled_one).to_s)
    assert_equal("0.0", (-scaled_one - -ONE).to_s)

    assert_equal('0.00', (scaled_zero * scaled_zero).to_s)
    assert_equal('-0.00', (-scaled_zero * scaled_zero).to_s)
    assert_equal('-0.00', (scaled_zero * -scaled_zero).to_s)
    assert_equal('0.00', (-scaled_zero * -scaled_zero).to_s)

    assert_equal('0.00', (scaled_zero * scaled_one).to_s)
    assert_equal('-0.00', (-scaled_zero * scaled_one).to_s)
    assert_equal('-0.00', (scaled_zero * -scaled_one).to_s)
    assert_equal('0.00', (-scaled_zero * -scaled_one).to_s)

    assert_equal('0.00', (scaled_one * scaled_zero).to_s)
    assert_equal('-0.00', (scaled_one * -scaled_zero).to_s)
    assert_equal('-0.00', (-scaled_one * scaled_zero).to_s)
    assert_equal('0.00', (-scaled_one * -scaled_zero).to_s)
  end

  # math part
  
  M = Decimal::Math
  SCALE = Float::DIG * 2
  PI = M.pi(SCALE*2)
  def check(expected, actual)
    expected = Decimal(expected).round(SCALE/2)
    actual = Decimal(actual).round(SCALE/2)
    assert_in_delta(expected, actual, Decimal("1e-#{SCALE/2}"))
  end

  def test_math_atan2
    #assert_raise(Math::DomainError) { Math.atan2(0, 0) }
    #assert_raise(Math::DomainError) { Math.atan2(Float::INFINITY, Float::INFINITY) }
    #assert_raise(Math::DomainError) { Math.atan2(Float::INFINITY, -Float::INFINITY) }
    #assert_raise(Math::DomainError) { Math.atan2(-Float::INFINITY, Float::INFINITY) }
    #assert_raise(Math::DomainError) { Math.atan2(-Float::INFINITY, -Float::INFINITY) }
    check(0, M.atan2(0, 1, SCALE))
    check(PI.divide(4, SCALE, :down), M.atan2(1, 1, SCALE))
    check(PI.divide(2, SCALE, :down), M.atan2(1, 0, SCALE))
  end

  def test_math_cos
    check(1, M.cos(0, SCALE))
    sqrt_2 = M.sqrt(2, SCALE)
    pi_q = PI.divide(4, SCALE, :half_up)
    check(ONE.divide(sqrt_2, SCALE, :half_up), M.cos(pi_q, SCALE))
    check(0, M.cos(2 * pi_q, SCALE))
    check(-1, M.cos(4 * pi_q, SCALE))
    check(0, M.cos(6 * pi_q, SCALE))
  end

  def test_math_sin
    check(0,  M.sin(0, SCALE))
    sqrt_2 = M.sqrt(2, SCALE)
    pi_q = PI.divide(4, SCALE, :half_up)
    check(ONE.divide(sqrt_2, SCALE, :half_up), M.sin(pi_q, SCALE))
    check(1,  M.sin(2 * pi_q, SCALE))
    check(0,  M.sin(4 * pi_q, SCALE))
    check(-1, M.sin(6 * pi_q, SCALE))
  end

  def test_math_tan
    check(0, M.tan(0, SCALE))
    pi_q = PI.divide(4, SCALE, :half_up)
    check(1, M.tan(pi_q, SCALE))
    assert(M.tan(2 * pi_q, SCALE).abs > Decimal("1e#{SCALE}"))
    check(0, M.tan(4 * pi_q, SCALE))
    assert(M.tan(6 * pi_q, SCALE).abs > Decimal("1e#{SCALE}"))
  end

  def test_math_acos
    pi_q = PI.divide(4, SCALE, :half_up)
    check(0 * pi_q, M.acos(1, SCALE))
    sqrt_2 = M.sqrt(2, SCALE)
    check(1 * pi_q, M.acos(ONE.divide(sqrt_2, SCALE, :half_up), SCALE))
    check(2 * pi_q, M.acos(0, SCALE))
    check(4 * pi_q, M.acos(-1, SCALE))
    assert_raise(Errno::EDOM) {M.acos(+1 + Decimal("1e-#{SCALE*2}"), SCALE)} # XXX
    assert_raise(Errno::EDOM) {M.acos(-1 - Decimal("1e-#{SCALE*2}"), SCALE)} # XXX
    assert_raise(Errno::EDOM) {M.acos(2, SCALE)} # XXX
  end

  def test_math_asin
    pi_q = PI.divide(4, SCALE, :half_up)
    check(0 * pi_q, M.asin(0, SCALE))
    sqrt_2 = M.sqrt(2, SCALE)
    check(1 * pi_q, M.asin(ONE.divide(sqrt_2, SCALE, :half_up), SCALE))
    check(2 * pi_q, M.asin(1, SCALE))
    check(-2 * pi_q, M.asin(-1, SCALE))
    assert_raise(Errno::EDOM) {M.asin(+1 + Decimal("1e-#{SCALE*2}"), SCALE)} # XXX
    assert_raise(Errno::EDOM) {M.asin(-1 - Decimal("1e-#{SCALE*2}"), SCALE)} # XXX
    assert_raise(Errno::EDOM) {M.asin(2, SCALE)} # XXX
  end

  def test_math_atan
    pi_q = PI.divide(4, SCALE, :half_up)
    check(0 * pi_q, M.atan(0, SCALE))
    check(1 * pi_q, M.atan(1, SCALE))
    check(2 * pi_q, M.atan(INFINITY, SCALE))
    check(-1 * pi_q, M.atan(-1, SCALE))
  end

  E = M.e(SCALE*2)
  E_INV = ONE.divide(E, SCALE*2, :down)
  def test_math_cosh
    check(1, M.cosh(0, SCALE))
    check((E ** 1 + E_INV ** 1).divide(2, SCALE, :down), M.cosh(1, SCALE))
    check((E ** 2 + E_INV ** 2).divide(2, SCALE, :down), M.cosh(2, SCALE))
  end

  def test_math_sinh
    check(0, M.sinh(0, SCALE))
    check((E ** 1 - E_INV ** 1).divide(2, SCALE, :down), M.sinh(1, SCALE))
    check((E ** 2 - E_INV ** 2).divide(2, SCALE, :down), M.sinh(2, SCALE))
  end

  def test_math_tanh
    x, y = M.sinh(0, SCALE), M.cosh(0, SCALE)
    check(x.divide(y, SCALE, :down), M.tanh(0, SCALE))
    x, y = M.sinh(1, SCALE), M.cosh(1, SCALE)
    check(x.divide(y, SCALE, :down), M.tanh(1, SCALE))
    x, y = M.sinh(2, SCALE), M.cosh(2, SCALE)
    check(x.divide(y, SCALE, :down), M.tanh(2, SCALE))
  end

  def test_math_acosh
    check(0, M.acosh(1, SCALE))
    check(1, M.acosh((E ** 1 + E_INV ** 1).divide(2, SCALE, :down), SCALE))
    check(2, M.acosh((E ** 2 + E_INV ** 2).divide(2, SCALE, :down), SCALE))
    assert_raise(Errno::EDOM) {M.acosh(1 - Decimal("1e-1000"), SCALE)} # XXX
    assert_raise(Errno::EDOM) {M.acosh(0, SCALE)} # XXX
  end

  def test_math_asinh
    check(0, M.asinh(0, SCALE))
    check(1, M.asinh((E ** 1 - E_INV ** 1).divide(2, SCALE, :down), SCALE))
    check(2, M.asinh((E ** 2 - E_INV ** 2).divide(2, SCALE, :down), SCALE))
  end

  def test_math_atanh
    x, y = M.sinh(0, SCALE), M.cosh(0, SCALE)
    check(0, M.atanh(x.divide(y, SCALE, :down), SCALE))
    x, y = M.sinh(1, SCALE), M.cosh(1, SCALE)
    check(1, M.atanh(x.divide(y, SCALE, :down), SCALE))
    x, y = M.sinh(2, SCALE), M.cosh(2, SCALE)
    check(2, M.atanh(x.divide(y, SCALE, :down), SCALE))
    #assert_nothing_raised { assert_infinity(Math.atanh(1)) }
    #assert_nothing_raised { assert_infinity(-Math.atanh(-1)) }
    assert_raise(Errno::EDOM) {M.atanh(+1 + Decimal("1e-1000"), SCALE)}
    assert_raise(Errno::EDOM) {M.atanh(-1 - Decimal("1e-1000"), SCALE)}
  end

  def test_math_exp_and_e
    check(1, M.exp(0, SCALE))
    check(M.sqrt(M.e(SCALE), SCALE), M.exp(Decimal("0.5"), SCALE))
    check(M.e(SCALE), M.exp(1, SCALE))
    check(M.e(SCALE) ** 2, M.exp(2, SCALE))
  end

  def test_math_log
    check(0, M.log(1, SCALE))
    check(1, M.log(M.e(SCALE), SCALE))
    check(0, M.log(1, 10, SCALE))
    check(1, M.log(10, 10, SCALE))
    check(2, M.log(100, 10, SCALE))
    assert_infinity(M.log(INFINITY, SCALE))
    #assert_nothing_raised {assert_infinity(-M.log(+0, SCALE))} # really??
    #assert_nothing_raised {assert_infinity(-M.log(-0, SCALE))} # (ditto)
    assert_raise(Errno::EDOM) {M.log(-1, SCALE)} # XXX
    assert_raise(TypeError) {M.log(1,:foo, SCALE)}
  end

  def test_math_log2
    check(0, M.log2(1, SCALE))
    check(1, M.log2(2, SCALE))
    check(2, M.log2(4, SCALE))
    assert_equal(INFINITY, M.log2(INFINITY, SCALE))
    #assert_nothing_raised {assert_infinity(-M.log2(+0, SCALE))} # XXX
    #assert_nothing_raised {assert_infinity(-M.log2(-0, SCALE))} # XXX
    assert_raise(Errno::EDOM) {M.log2(-1, SCALE)} # XXX
  end

  def test_math_log10
    check(0, M.log10(1, SCALE))
    check(1, M.log10(10, SCALE))
    check(2, M.log10(100, SCALE))
    assert_equal(INFINITY, M.log10(INFINITY, SCALE))
    #assert_nothing_raised {assert_infinity(-M.log10(+0, SCALE))} # XXX
    #assert_nothing_raised {assert_infinity(-M.log10(-0, SCALE))} # XXX
    assert_raise(Errno::EDOM) {M.log10(-1, SCALE)} # XXX
  end

  def test_math_pi
    check("3.141592653589793238", M.pi(SCALE))
  end

  def test_math_sqrt
    check(0, M.sqrt(0, SCALE))
    check(1, M.sqrt(1, SCALE))
    check(2, M.sqrt(4, SCALE))
    assert_equal(INFINITY, M.sqrt(INFINITY, SCALE))
    assert_equal("0.0", M.sqrt(Decimal("-0.0"), SCALE).to_s[0..2]) # insure it is +0.0, not -0.0
    assert_raise(Errno::EDOM) {M.sqrt(-1, SCALE)} # XXX
  end

  def test_math_frexp10
    check(0, M.frexp10(0).first)
    assert_equal(0, M.frexp10(0).last)
    check("0.1", M.frexp10(Decimal("0.1")).first)
    assert_equal(0, M.frexp10(Decimal("0.1")).last)
    check("0.1", M.frexp10(1).first)
    assert_equal(1, M.frexp10(1).last)
    check("0.1", M.frexp10(10).first)
    assert_equal(2, M.frexp10(10).last)
    check("0.1", M.frexp10(100).first)
    assert_equal(3, M.frexp10(100).last)
    check("0.123", M.frexp10(Decimal("12.3")).first)
    assert_equal(2, M.frexp10(Decimal("12.3")).last)
    check("-0.123", M.frexp10(Decimal("-12.3")).first)
    assert_equal(2, M.frexp10(Decimal("-12.3")).last)
  end

  def test_math_ldexp10
    check(0, M.ldexp10(0, 0))
    check("0.1", M.ldexp10(Decimal("0.1"), 0))
    check(1, M.ldexp10(Decimal("0.1"), 1))
    check(10, M.ldexp10(Decimal("0.1"), 2))
    check(100, M.ldexp10(Decimal("0.1"), 3))
    check("0.123", M.ldexp10(Decimal("12.3"), -2))
    check("-0.123", M.ldexp10(Decimal("-12.3"), -2))
  end

  def test_math_hypot
    check(5, M.hypot(3, 4, SCALE))
  end

  def test_math_erf
    check(0, M.erf(0, SCALE))
    check(1, M.erf(INFINITY, SCALE))
    check("0.842700792949715", M.erf(1, SCALE))
  end

  def test_math_erfc
    check(1, M.erfc(0, SCALE))
    check(0, M.erfc(INFINITY, SCALE))
    check("0.157299207050285", M.erfc(1, SCALE))
  end

  SQRT_PI = M.sqrt(PI, SCALE*2)
  def test_math_gamma
    check((4 * SQRT_PI).divide(3, SCALE, :down),
          M.gamma(Decimal("-1.5"), SCALE))
    check(-2 * SQRT_PI, M.gamma(Decimal("-0.5"), SCALE))
    check(SQRT_PI, M.gamma(Decimal("0.5"), SCALE))
    check(1, M.gamma(1, SCALE))
    check(SQRT_PI.divide(2, SCALE, :down), M.gamma(Decimal("1.5"), SCALE))
    check(1, M.gamma(2, SCALE))
    check((3 * SQRT_PI).divide(4, SCALE, :down),
          M.gamma(Decimal("2.5"), SCALE))
    check(2, M.gamma(3, SCALE))
    check((15 * SQRT_PI).divide(8, SCALE, :down),
          M.gamma(Decimal("3.5"), SCALE))
    check(6, M.gamma(4, SCALE))

    assert_raise(Errno::EDOM) {M.gamma(-Decimal::INFINITY, SCALE)} # XXX
  end

  def test_math_lgamma
    g, s = M.lgamma(Decimal("-1.5"), SCALE)
    check(M.log((4 * SQRT_PI).divide(3, SCALE, :down), SCALE), g)
    assert_equal(1, s)

    g, s = M.lgamma(Decimal("-0.5"), SCALE)
    check(M.log(2 * SQRT_PI, SCALE), g)
    assert_equal(-1, s)

    g, s = M.lgamma(Decimal("0.5"), SCALE)
    check(M.log(SQRT_PI, SCALE), g)
    assert_equal(1, s)

    assert_equal([0, 1], M.lgamma(1, SCALE))

    g, s = M.lgamma(Decimal("1.5"), SCALE)
    check(M.log(SQRT_PI.divide(2, SCALE, :down), SCALE), g)
    assert_equal(1, s)

    assert_equal([0, 1], M.lgamma(2, SCALE))

    g, s = M.lgamma(Decimal("2.5"), SCALE)
    check(M.log((3 * SQRT_PI).divide(4, SCALE, :down), SCALE), g)
    assert_equal(1, s)

    g, s = M.lgamma(3, SCALE)
    check(M.log(2, SCALE), g)
    assert_equal(1, s)

    g, s = M.lgamma(Decimal("3.5"), SCALE)
    check(M.log((15 * SQRT_PI).divide(8, SCALE, :down), SCALE), g)
    assert_equal(1, s)

    g, s = M.lgamma(4, SCALE)
    check(M.log(6, SCALE), g)
    assert_equal(1, s)

    assert_raise(Errno::EDOM) {M.lgamma(-Decimal::INFINITY, SCALE)} # XXX
  end

  def test_math_cbrt
    check(0, M.cbrt(0, SCALE))
    check(1, M.cbrt(1, SCALE))
    check(2, M.cbrt(8, SCALE))
    assert_equal(INFINITY, M.cbrt(INFINITY, SCALE))
    assert_equal(-INFINITY, M.cbrt(-INFINITY, SCALE)) 
    assert_equal("0.0", M.cbrt(Decimal("0.0"), SCALE).to_s[0..2])
    assert_equal("-0.0", M.cbrt(Decimal("-0.0"), SCALE).to_s[0..3]) # insure it is -0.0, not +0.0
    check(-1, M.cbrt(-1, SCALE))
    check(-2, M.cbrt(-8, SCALE))
  end
end
