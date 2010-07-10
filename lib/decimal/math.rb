module Decimal::Math
  module_function

  # from book: ISBN4-87408-414-1
  def sqrt(x, scale, rounding=:down)
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
    last
  end

  # copied from BigDecimal
  def sin(x, scale, rounding=:down)
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

  # copied from BigDecimal
  def cos(x, scale, rounding=:down)
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
    s, c = sin(x, scale+1), cos(x, scale+1)
    s.divide(c, scale, rounding)
  end

  # copied from BigDecimal
  def exp(x, scale, rounding=:down)
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

  # copied from BigDecimal
  def log(x, scale, rounding=:down)
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
    y * 2
  end
end
