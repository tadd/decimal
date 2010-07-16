module Decimal::Math
  module_function

  # algorithm of functions below are from book: ISBN4-87408-414-1
  #   sqrt, cbrt
  
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
    x  = (x - 1).divide(x + 1, scale+1, :down)
    x2 = x * x
    y = x
    i = 1
    loop do
      x  = (x * x2).floor(scale+1)
      i += 2
      d  = x.divide(i, scale+1, :down)
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

  # TODO: better thresholds needed
  @@_decimal_internal_sqrt_01 = Decimal("0.316227766016838")
  @@_decimal_internal_sqrt_10 = Decimal("3.16227766016838")
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
end
