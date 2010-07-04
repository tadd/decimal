module Decimal::Math
  module_function

  # copied from BigDecimal
  def sin(x, scale, rounding=:down)
    return Decimal::NAN if x.infinite? or x.nan?
    return Decimal("0e#{-scale}") if x.zero?
    x = -x if negative = x < 0
    x1 = x
    x2 = x * x
    sign = 1
    d = y = x
    i = z = 1
    while d.nonzero?
      sign = -sign
      x1 *= x2
      i += 2
      z *= (i - 1) * i
      d = sign * x1.divide(z, scale+1, :down)
      y += d
    end
    y = -y if negative
    y.round(scale, rounding)
  end

  # copied from BigDecimal
  def cos(x, scale, rounding=:down)
    return Decimal::NaN if x.infinite? or x.nan?
    x = -x if x < 0
    x1 = 1
    x2 = x * x
    sign = 1
    d = y = 1
    i = 0
    z = 1
    while d.nonzero?
      sign = -sign
      x1 *= x2
      i += 2
      z *= (i - 1) * i
      d = sign * x1.divide(z, scale+1, :down)
      y += d
    end
    y.round(scale, rounding)
  end

  def tan(x, scale, rounding=:down)
    s, c = sin(x, scale+1), cos(x, scale+1)
    s.divide(c, scale, rounding)
  end
end
