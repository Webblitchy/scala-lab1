package Chat

import scala.compiletime.ops.int

/**
  * This sealed trait represents a node of the tree.
  */
sealed trait ExprTree

/**
  * Declarations of the nodes' types.
  */
object ExprTree:
  // DONE - Part 2 Step 3
  // Example cases
  case object Thirsty extends ExprTree
  case object Hungry extends ExprTree
  case object AskSold extends ExprTree
  //a sentence to command some product 
  case class Buy(command: ExprTree) extends ExprTree
  case class AskPrice(command: ExprTree) extends ExprTree

  case class Login(user: String) extends ExprTree

  case class Or(left: Command, right: ExprTree) extends ExprTree
  case class And(left: Command, right: ExprTree) extends ExprTree  
  // case object Or extends ExprTree
  // case object And extends ExprTree
  // a sentence describing some command : NUM, PRODUCT (MARQUE) [ (ET | OU ) Product]
  case class Command(num:Int, product:String, brand : Option[String]) extends ExprTree

