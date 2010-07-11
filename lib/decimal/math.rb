module Decimal::Math
  module_function

  # from book: ISBN4-87408-414-1
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
    y = 1
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
    raise Errno::EDOM if x.infinity? # XXX
    s, c = sin(x, scale+1), cos(x, scale+1)
    s.divide(c, scale, rounding)
  end

  def exp(x, scale, rounding=:down)
    x = Decimal(x) if x.integer?
    return Decimal::NAN if x.infinite? or x.nan?
    return Decimal("0e#{-scale}") if x.zero?
    if x.infinite?
      return x > 0 ? Decimal::INFINITY : Decimal("0e#{-scale}")
    end
    x = -x if negative = x < 0
    z = d = x1 = y = 1
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

  def log(x, scale, rounding=:down)
    x = Decimal(x) if x.integer?
    return Decimal::NAN if x.nan?
    return Decimal("0e#{-scale}") if x == 1
    raise Errno::EDOM if x < 0 # XXX
    raise Errno::ERANGE if x.zero? # XXX
    return Decimal::INFINITY if x.infinite?
    x  = (x - 1).divide(x + 1, scale+1, :down)
    x2 = x * x
    d = y = x
    i = 1
    loop do
      x  = (x * x2).floor(scale+1)
      i += 2
      d  = x.divide(i, scale+1, :down)
      break if d.zero?
      y += d
    end
    (y * 2).round(scale, rounding)
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
    t = d = y = x
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
