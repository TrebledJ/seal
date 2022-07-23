object SimpleClass {
    abstract class Option
    case class None() extends Option
    case class Some(v: Int) extends Option

    val x: Option = Some(1);
    x
}