import io.StdIn.readLine

object SolvePicross extends App {

  def possibleSeq(baseSeq: Seq[Option[Boolean]]): Seq[Seq[Boolean]] =
    baseSeq.foldLeft(Seq(Seq.empty[Boolean])) { (acc, ele) =>
      ele match {
        case Some(b) => acc.map(_ ++ Seq(b))
        case None => acc.flatMap(s => Seq(s ++ Seq(true), s ++ Seq(false)))
      }
    }

  def reduceSeq(possibleSeqs: Seq[Seq[Boolean]]): Seq[Option[Boolean]] = {
    if (possibleSeqs.nonEmpty)
      possibleSeqs.tail.foldLeft(possibleSeqs.head.map(Some(_)): Seq[Option[Boolean]]) { (acc, ele) =>
        acc.zip(ele).iterator.map {
          case (a, b) if (a.contains(b)) => a
          case _ => None
        }.toSeq
      }
    else {
      println("定まらない行または列があります。入力を見直してください。")
      sys.exit(0)
    }
  }

  def serialize(seq: Seq[Boolean]): Seq[Int] =
    seq.foldLeft((0, Seq.empty[Int])) { (acc, ele) =>
      ele match {
        case true => (acc._1 + 1, acc._2)
        case false if acc._1 > 0 => (0, acc._2 ++ Seq(acc._1))
        case _ => acc
      }
    } match {
      case (0, Seq()) => Seq(0)
      case (0, res) => res
      case (las, res) => res ++ Seq(las)
    }

  def checkValid(seq: Seq[Boolean], nums: Seq[Int]): Boolean = {
    serialize(seq) == nums
  }

  def rowUpdate(table: Seq[Seq[Option[Boolean]]]) =
    table.zip(rowNums).map { tpl =>
      reduceSeq(possibleSeq(tpl._1).filter(checkValid(_, tpl._2)))
    }

  def colUpdate(table: Seq[Seq[Option[Boolean]]]) =
    table.transpose.zip(colNums).map { tpl =>
      reduceSeq(possibleSeq(tpl._1).filter(checkValid(_, tpl._2)))
    }.transpose

  def doIter(table: Seq[Seq[Option[Boolean]]]) = colUpdate(rowUpdate(table))

  def printout(res: Seq[Seq[Option[Boolean]]]): Unit = {
    res.foreach(row =>
      println(row.map {
        case Some(true) => "■"
        case Some(false) => "□"
        case None => "◆"
      }.mkString(""))
    )
  }

  println("Input table height / 盤面の縦幅(nxmのn)を入力してください")
  val tableHeight = readLine().toInt
  println("Input table width / 盤面の横幅(nxmのm)を入力してください")
  val tableWidth = readLine().toInt
  println("Input numbers on the left (separated by space) / 左側に付与される数字をスペース区切りで各行入力してください")
  val rowNums = (1 to tableHeight).map(_ => readLine().split(" ").map(_.toInt))
  println("Input numbers on the top (separated by space) /上側に付与される数字をスペース区切りで各行入力してください")
  val colNums = (1 to tableWidth).map(_ => readLine().split(" ").map(_.toInt))
  println("Executing... / 計算します")

  val table: Seq[Seq[Option[Boolean]]] = (1 to tableHeight).map(_ =>
    (1 to tableWidth).map(_ => None))

  var res = table
  var prevNonDecided = Int.MaxValue
  var nonDecided = Int.MaxValue - 1
  var cnt = 0
  while (nonDecided > 0 && prevNonDecided != nonDecided) {
    prevNonDecided = nonDecided
    res = doIter(res)
    nonDecided = res.foldLeft(0)((acc, ele) => acc + ele.filter(_.isEmpty).size)
    cnt += 1
    println("")
    println(s"試行回数${cnt}回目\t未確定マス数:$nonDecided")
    printout(res)
    println("")
  }

}
