val prices = List(1.0, 2.0, 4.0)
val ops = "+"::"+" :: "min" :: Nil
ops
  .zip(prices)
  .foldLeft(ops.head match
    case "min" => prices.head
    case "+"   => 0.0
  ) { case (acc, (op, price)) =>
    op match {
      case "+" => {
        println(s"$acc + $price")
        acc + price
      }
      case "min" => {
        println(s"$acc min $price")
        acc min price
      }
      case _ => acc
    }
  }
  "this  is a     test".replaceAll("\\s+", " ")
//explain the previous line of code 