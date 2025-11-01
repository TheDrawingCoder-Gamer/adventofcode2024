package gay.menkissing.common

enum AxisDirection:
  case Positive, Negative

  def unary_! : AxisDirection =
    this match
      case AxisDirection.Negative => AxisDirection.Positive
      case AxisDirection.Positive => AxisDirection.Negative
