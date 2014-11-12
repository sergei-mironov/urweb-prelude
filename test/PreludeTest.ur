structure P = Prelude

val x = (4,"str")

val o1 : option int = Some 44
val o2 : option string = None
val o3 = Some x

fun main {} : transaction page =
  return
  <xml>
    {[x]}
    {[o1]}
    {[o2]}
    {[o3]}
  </xml>

