package value

case class Boole(val Value: Boolean) extends Value:
//  def &&(other: Value): Boole =
//    this.Value && other

  //  def ||(other: Value): Boole =
  //    this.Value || other

    def unary_!(): Boole =
      Boole(!this.Value)