module Decimal::Math
  module_function

  # algorithm of functions below are from book: ISBN4-87408-414-1
  #   sqrt, cbrt, erf, erfc, *p_gamma, *q_gamma, *bernoulli

  # functions and variables that prefixed by "_decimal_internal" are
  # indended to internal use only
  def _decimal_internal_p_gamma(a, x, loggamma_a, scale, rounding=:down)
    if x >= 1 + a
      y = 1 - _decimal_internal_q_gamma(a, x, loggamma_a, scale+1, :down)
      return y.round(scale, rounding)
    end
    return Decimal("0e#{-scale}") if x.zero?
    y0 = exp(a * log(x, scale+1, :down) - x - loggamma_a, scale+1, :down)
    y = term = y0.divide(a, scale+1, :down)
    k = 1
    loop do
      tmp = x.divide(a + k, scale+1, :down)
      term = (term * tmp).floor(scale+1)
      break if term.zero?
      y += term
      k += 1
    end
    y.round(scale, rounding)
  end

  def _decimal_internal_q_gamma(a, x, loggamma_a, scale, rounding=:down)
    la, lb = 1, 1 + x - a
    if x < 1 + a
      y = 1 - _decimal_internal_p_gamma(a, x, loggamma_a, scale+1, :down)
      return y.round(scale, rounding)
    end
    w = exp(a * log(x, scale+1, :down) - x - loggamma_a, scale+1, :down)
    y = w.divide(lb, scale+1, :down)
    k = 2
    loop do
      tmp0 = (k - 1 - a) * (lb - la) + (k + x) * lb
      tmp = tmp0.divide(k, scale+1, :down)
      la, lb = lb, tmp
      w0 = (k - 1 - a).divide(k, scale+1, :down)
      w = (w * w0).floor(scale+1) 
      term = w.divide(la * lb, scale+1, :down)
      break if term.zero?
      y += term
    end
    y.round(scale, rounding)
  end

  def _decimal_internal_bernoulli(x, scale, rounding=:down)
    return Decimal(1) if x == 0
    return Decimal("-0.5") if x == 1
    return Decimal(0) unless x % 2 == 0

    t = Hash.new(0)
    t[1] = 1
    q = 1
    n = 2
    sign = -1
    loop do
      (1...n).each do |i|
        t[i-1] = i * t[i]
      end
      t[n-1] = 0
      n.downto(2) do |i|
        t[i] += t[i - 2]
      end
      if n.even?
        sign = -sign
        q *= 4
        b1 = n * t[0]
        b2 = q * (q - 1)
        return sign * Decimal(b1).divide(b2, scale, rounding) if n == x
      end
      n += 1
    end
  end

  def _decimal_internal_fact(n)
    return 1 if n < 2
    (2..n).inject {|a, x| a * x}
  end

  #
  # public functions
  #

  def sqrt(x, scale, rounding=:down)
    x = Decimal(x) if x.integer?
    return Decimal::NAN if x.nan?
    return Decimal("0e#{-scale}") if x.zero?
    raise Errno::EDOM if x < 0 # XXX
    return Decimal::INFINITY if x.infinite?

    s = x > 1 ? x : (x + 1).divide(2, scale+1, :down)
    begin
      last = s
      t = x.divide(s, scale+1, :down) + s
      s = t.divide(2, scale+1, :down)
    end while s < last
    last.round(scale, rounding)
  end

  def cbrt(x, scale, rounding=:down)
    x = Decimal(x) if x.integer?
    return x if x.nan? or x.infinite?
    if x.zero?
      negative_zero = Decimal(1).divide(x) < 0 # TODO
      return negative_zero ? Decimal("-0e#{-scale}") : Decimal("0e#{-scale}")
    end
    x = -x if negative = x < 0

    s = x > 1 ? x : Decimal(1)
    begin
      last = s
      t = x.divide(s * s, scale+1, :down) + 2 * s
      s = t.divide(3, scale+1, :down)
    end while s < last
    last = -last if negative
    last.round(scale, rounding)
  end

  # functions below are copied from BigDecimal:
  #   sin, cos, exp, log, atan, pi

  def sin(x, scale, rounding=:down)
    x = Decimal(x) if x.integer?
    return Decimal::NAN if x.infinite? or x.nan?
    return Decimal("0e#{-scale}") if x.zero?
    x = -x if negative = x < 0
    x1 = x
    x2 = x * x
    sign = 1
    y = x
    i = z = 1
    loop do
      sign = -sign
      x1 *= x2
      i += 2
      z *= (i - 1) * i
      d = sign * x1.divide(z, scale+1, :down)
      break if d.zero?
      y += d
    end
    y = -y if negative
    y.round(scale, rounding)
  end

  def cos(x, scale, rounding=:down)
    x = Decimal(x) if x.integer?
    return Decimal::NAN if x.infinite? or x.nan?
    x = -x if x < 0
    x1 = 1
    x2 = x * x
    sign = 1
    y = Decimal(1)
    i = 0
    z = 1
    loop do
      sign = -sign
      x1 *= x2
      i += 2
      z *= (i - 1) * i
      d = sign * x1.divide(z, scale+1, :down)
      break if d.zero?
      y += d
    end
    y.round(scale, rounding)
  end

  def tan(x, scale, rounding=:down)
    x = Decimal(x) if x.integer?
    return Decimal::NAN if x.infinite? or x.nan?
    return Decimal("0e#{-scale}") if x.zero?
    raise Errno::EDOM if x.infinite? # XXX
    s, c = sin(x, scale+1), cos(x, scale+1)
    s.divide(c, scale, rounding)
  end

  def exp(x, scale, rounding=:down)
    x = Decimal(x) if x.integer?
    return Decimal::NAN if x.infinite? or x.nan?
    return Decimal(1) if x.zero?
    if x.infinite?
      return x > 0 ? Decimal::INFINITY : Decimal("0e#{-scale}")
    end
    x = -x if negative = x < 0
    z = x1 = y = 1
    i = 0
    loop do
      x1 *= x
      i += 1
      z *= i
      d = x1.divide(z, scale+1, :down)
      break if d.zero?
      y += d
    end
    unless negative
      y.round(scale, rounding)
    else
      Decimal(1).divide(y, scale, rounding)
    end
  end

  def e(scale, rounding=:down)
    exp(1, scale, rounding)
  end

  def log(x, *args) # args:(base=nil, scale, rounding=:down)
    case args.size
    when 1
      base = nil
      scale = args[0]
      rounding = :down
    when 2
      case args[1]
      when Symbol
        base = nil
        scale, rounding = *args
        unless scale.is_a?(Integer)
          raise TypeError, "scale #{scale} must be Integer"
        end
      when Integer
        base, scale = *args
        unless base.is_a?(Integer)
          raise TypeError, "base #{base} must be Integer"
        end
        rounding = :down
      else
        raise ArgumentError, "3rd argument #{args[1]} must be Integer or Symbol"
      end
    when 3
      base, scale, rounding = *args
    else
      raise ArgumentError, "wrong number of arguments (#{args.size+1} for 2..4)"
    end

    x = Decimal(x) if x.integer?
    return Decimal::NAN if x.nan?
    return Decimal("0e#{-scale}") if x == 1
    raise Errno::EDOM if x < 0 # XXX
    raise Errno::ERANGE if x.zero? # XXX
    return Decimal::INFINITY if x.infinite?

    if x > 10 or x < Decimal("0.1")
      x2, n = frexp10(x)
      y = n * log(10, base, scale+1, :down) + log(x2, base, scale+1, :down)
      return y.round(scale, rounding)
    end

    u  = (x - 1).divide(x + 1, scale+1, :down)
    u2 = u * u
    y = u
    i = 1
    loop do
      u = (u * u2).floor(scale+1)
      i += 2
      d = u.divide(i, scale+1, :down)
      break if d.zero?
      y += d
    end
    y *= 2
    if base
      divisor = log(base, scale+1, :down) # divisor = ln(base)
      y = y.divide(divisor, scale+1, :down)
    end
    y.round(scale, rounding)
  end

  # internal use only
  @@_decimal_internal_sqrt_01 = Decimal("0.316227766016838")
  @@_decimal_internal_sqrt_10 = Decimal("3.16227766016838")
  # TODO: better thresholds needed
  def log10(x, scale, rounding=:down)
    x = Decimal(x) if x.integer?
    return Decimal::INFINITY if x.infinite?
    if x > @@_decimal_internal_sqrt_10 or x < @@_decimal_internal_sqrt_01
      x2, n = frexp10(x)
      (log(x2, 10, scale+1) + n).round(scale, rounding)
    else
      log(x, 10, scale, rounding)
    end
  end

  def log2(x, scale, rounding=:down)
    log(x, 2, scale, rounding)
  end

  def pi(scale, rounding=:down)
    pi = 0

    k = 1
    t = Decimal(-80)
    u = nil
    loop do
      t = t.divide(-25, scale+1, :down) 
      u = t.divide(k, scale+1, :down)
      break if u.zero?
      pi += u
      k += 2
    end

    k = 1
    t = Decimal(956)
    loop do
      t = t.divide(-57121, scale+1, :down)
      u = t.divide(k, scale+1, :down)
      break if u.zero?
      pi += u
      k += 2
    end
    pi.round(scale, rounding)
  end

  def atan(x, scale, rounding=:down)
    x = Decimal(x) if x.integer?
    return Decimal::NAN if x.nan?
    return Decimal("0e#{-scale}") if x.zero?
    x = -x if negative = x < 0
    local_pi = lambda{pi(scale+1, :down)}
    return local_pi[].divide(negative ? -2 : 2, scale, rounding) if x.infinite?
    return local_pi[].divide(negative ? -4 : 4, scale, rounding) if x == 1
    x = Decimal(1).divide(x, scale+1, :down) if inverse = x > 1
    if double = x > Decimal("0.5")
      inx = -1 + sqrt(1 + x * x, scale+1, :down)
      x = inx.divide(x, scale+1,:down)
    end
    t = y = x
    r = 3
    x2 = (x * x).floor(scale+1)
    loop do
      t = ((-t) * x2).floor(scale+1)
      d = t.divide(r, scale+1, :down)
      break if d.zero?
      y += d
      r += 2
    end
    y *= 2 if double
    y = local_pi[].divide(2, scale+1, :down) - y if inverse
    y = -y if negative
    y.round(scale, rounding)
  end

  def asin(x, scale, rounding=:down)
    x = Decimal(x) if x.integer?
    return Decimal::NAN if x.nan?
    return Decimal("0e#{-scale}") if x.zero?
    raise Errno::EDOM if x > 1 or x < -1 # XXX
    x2 = sqrt(1 - x * x, scale+1, :down)
    atan(x.divide(x2, scale+1, :down), scale, rounding)
  end
  
  def acos(x, scale, rounding=:down)
    x = Decimal(x) if x.integer?
    return Decimal::NAN if x.nan?
    return Decimal("0e#{-scale}") if x == 1
    raise Errno::EDOM if x.infinite? or x > 1 or x < -1 # XXX
    pi_half = pi(scale+1, :down).divide(2, scale+1, :down)
    as = asin(x, scale+1, :down)
    (pi_half - as).round(scale, rounding)
  end

  def atan2(y, x, scale, rounding=:down)
    x = Decimal(x) if x.integer?
    y = Decimal(y) if y.integer?
    return Decimal::NAN if x.nan? or y.nan?
    if y_sign = y.infinite?
      z = pi(scale+1, :down)
      divisor = nil
      if x_sign = x.infinite?
        z *= 3 if x_sign < 0
        divisor = 4
      else
        divisor = 2
      end
      return y_sign * z.divide(divisor, scale, rounding)
    elsif y.zero?
      y_sign = Decimal(1).divide(y) < 0
      x_negative = x < 0 or (x.zero? and Decimal(1).divide(x) < 0)
      z = x_negative ? pi(scale, rounding) : Decimal("0e#{-scale}")
      return y_sign ? -z : z
    elsif x.zero?
      z = pi(scale+1, :down).divide(2, scale, rounding)
      return y < 0 ? -z : z
    elsif x_sign = x.infinite?
      z = x_sign < 0 ? pi(scale, rounding) : Decimal("0e#{-scale}")
      return y < 0 ? -z : z
    end

    z = x.divide(y, scale+1, :down)
    atan(z, scale, rounding)
  end

  def sinh(x, scale, rounding=:down)
    x = Decimal(x) if x.integer?
    return Decimal::NAN if x.nan?
    return x if x.infinite?
    return Decimal("0e#{-scale}") if x.zero?
    e_x = exp(x, scale+1, :down)
    inv_e_x = Decimal(1).divide(e_x, scale+1, :down)
    (e_x - inv_e_x).divide(2, scale, rounding)
  end

  def cosh(x, scale, rounding=:down)
    x = Decimal(x) if x.integer?
    return Decimal::NAN if x.nan?
    return Decimal::INFINITY if x.infinite?
    if x.zero?
      return scale > 0 ? Decimal(1) : Decimal("1e#{-scale}")
    end
    e_x = exp(x, scale+1, :down)
    inv_e_x = Decimal(1).divide(e_x, scale+1, :down)
    (e_x + inv_e_x).divide(2, scale, rounding)
  end

  def tanh(x, scale, rounding=:down)
    x = Decimal(x) if x.integer?
    return Decimal::NAN if x.nan?
    return Decimal("0e#{-scale}") if x.zero?
    if y = x.infinite?
      return y
    end
    s, c = sinh(x, scale+1, :down), cosh(x, scale+1, :down)
    s.divide(c, scale, rounding)
  end

  def asinh(x, scale, rounding=:down)
    x = Decimal(x) if x.integer?
    return Decimal::NAN if x.nan?
    return Decimal("0e#{-scale}") if x.zero?
    return x if x.infinite?
    x2 = sqrt(x * x + 1, scale, :down)
    log(x + x2, scale, rounding)
  end

  def acosh(x, scale, rounding=:down)
    x = Decimal(x) if x.integer?
    return Decimal::NAN if x.nan?
    return Decimal("0e#{-scale}") if x == 1
    raise Errno::EDOM if x < 1
    return Decimal::INFINITY if x.infinite?
    x2 = sqrt(x * x - 1, scale, :down)
    log(x + x2, scale, rounding)
  end

  def atanh(x, scale, rounding=:down)
    x = Decimal(x) if x.integer?
    return Decimal::NAN if x.nan?
    return Decimal("0e#{-scale}") if x.zero?
    raise Errno::ERANGE if x == 1 or x == -1 # XXX
    raise Errno::EDOM if x.abs > 1 # XXX
    x2 = (1 + x).divide(1 - x, scale+1, :down)
    y = Decimal("0.5") * log(x2, scale+1, :down)
    y.round(scale, rounding)
  end

  def hypot(x, y, scale, rounding=:down)
    x = Decimal(x) if x.integer?
    y = Decimal(y) if y.integer?
    return Decimal::INFINITY if x.infinite? or y.infinite?
    return Decimal::NAN if x.nan? or y.nan?
    sqrt(x * x + y * y, scale, rounding)
  end

  def erf(x, scale, rounding=:down)
    x = Decimal(x) if x.integer?
    return Decimal::NAN if x.nan?
    if y = x.infinite?
      return y
    end
    half = Decimal("0.5")
    half_log_pi = half * log(pi(scale+1, :down), scale+1, :down)
    x2 = (x * x).floor(scale+1)
    y = if x >= 0
          _decimal_internal_p_gamma(half, x2, half_log_pi, scale+1)
        else
          -_decimal_internal_p_gamma(half, x2, half_log_pi, scale+1)
        end
    y.round(scale, rounding)
  end

  def erfc(x, scale, rounding=:down)
    x = Decimal(x) if x.integer?
    return Decimal::NAN if x.nan?
    if x.zero?
      return scale > 0 ? Decimal("1."+"0"*scale) : Decimal(1)
    end
    if y = x.infinite?
      if y > 0
        return Decimal("0e#{-scale}")
      else
        return scale > 0 ? Decimal("2."+"0"*scale) : Decimal(2)
      end
    end
    half = Decimal("0.5")
    half_log_pi = half * log(pi(scale+1, :down), scale+1, :down)
    x2 = (x * x).floor(scale+1)
    y = if x >= 0
          _decimal_internal_q_gamma(half, x2, half_log_pi, scale+1)
        else
          1 + _decimal_internal_p_gamma(half, x2, half_log_pi, scale+1)
        end
    y.round(scale, rounding)
  end

  def gamma(x, scale, rounding=:down)
    x = Decimal(x) if x.integer?
    return Decimal::NAN if x.nan?
    if inf = x.infinite?
      return Decimal::INFINITY if inf > 0
      raise Errno::EDOM # XXX
    end
    raise Errno::EDOM if x < 0 and x % 1 == 0 # XXX
    raise Errno::ERANGE if x.zero? # XXX

    if x < 0
      pi_tmp = pi(scale+2, :down)
      pi_x = (pi_tmp * x).floor(scale+2)
      div1 = sin(pi_x, scale+2, :down)
      div2 = exp(lgamma(1 - x, scale+2, :down)[0], scale+2, :down)
      divisor = (div1 * div2).floor(scale+2)
      return pi_tmp.divide(divisor, scale, rounding)
    end
    return Decimal(_decimal_internal_fact(x.to_i - 1)) if (x % 1).zero?
    exp(lgamma(x, scale+2, :down)[0], scale, rounding)
  end
  
  def lgamma(x, scale, rounding=:down)
    x = Decimal(x) if x.integer?
    sign = 1
    return [Decimal::NAN, sign] if x.nan?
    return [Decimal("0e#{-scale}"), sign] if x == 1 or x == 2
    if inf = x.infinite?
      raise Errno::EDOM if inf < 0
      return [Decimal::INFINITY, sign]
    end

    if (x % 1).zero?
      raise Errno::ERANGE if x <= 0 # XXX
      return [log(_decimal_internal_fact(x.to_i-1), scale, rounding), sign]
    end
    sign = -1 if x < 0 and x % 2 > 1
    threshold = scale # XXX
    v = 1
    while x < threshold
      v *= x
      x += 1
    end
    half = Decimal("0.5")
    y = (x - half) * log(x, scale+2, :down) - x +
        half * log(2 * pi(scale+2, :down), scale+2, :down) -
        log(v.abs, scale+2, :down)
    n = 2
    x2 = x * x
    w = x
    loop do
      bn = _decimal_internal_bernoulli(n, scale+2)
      term = bn.divide(n * (n - 1) * w, scale+2, :down)
      break if term.zero?
      y += term
      n += 2
      w *= x2
    end
    [y.round(scale, rounding), sign]
  end
end
