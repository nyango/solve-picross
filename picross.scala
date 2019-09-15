import io.StdIn.readLine

object SolvePicross extends App {

  // OK sample: 4 3 2, expected 4 3 4 5 6
  // NG sample: 5 3 2, expected 4 3 4 5 6
  def isValid(ss: Seq[Boolean], expected: Seq[Int], seqSize: Int) = {
    val target = serialize(ss)
    val targetInit = target.init
    val remains = expected.drop(targetInit.size)
    val res1 = targetInit == expected.take(targetInit.size)
    val res2 = remains.headOption.exists(_ >= target.last)
    val neededCells = remains.sum + remains.size - 1 - target.last
    val res3 = seqSize - ss.size >= neededCells
    // println(s"seq:$ss\ttarget:$target\tseqsize:$seqSize\tnums:$expected\tneeded:$neededCells\tres3:$res3")
    res1 && res2 && res3
  }

  def possibleSeq(baseSeq: Seq[Option[Boolean]], nums: Seq[Int]): Seq[Seq[Boolean]] = {
    val sumOfNum = nums.sum
    // println(s"baseSeq:${baseSeq}\tsize:${baseSeq.size}\tnums:$nums")
    baseSeq.foldLeft(Seq(Seq.empty[Boolean])) { (acc, ele) =>
      ele match {
        case Some(b) => acc.map(_ ++ Seq(b)).filter(isValid(_, nums, baseSeq.size))
        case None =>
          acc.flatMap { s =>
            Seq(s ++ Seq(true), s ++ Seq(false))
          }.filter(isValid(_, nums, baseSeq.size))
      }
    }
  }

  def reduceSeq(possibleSeqs: Seq[Seq[Boolean]], size: Int): Seq[Option[Boolean]] = {
    if (possibleSeqs.nonEmpty)
      possibleSeqs.tail.foldLeft(possibleSeqs.head.map(Some(_)): Seq[Option[Boolean]]) { (acc, ele) =>
        acc.zip(ele).iterator.map {
          case (a, b) if (a.contains(b)) => a
          case _ => None
        }.toList
      }
    else {
      (1 to size).map(_ => None).toList
//      println("定まらない行または列があります。入力を見直してください。")
//      sys.exit(0)
    }
  }

  def serialize(seq: Seq[Boolean]): Seq[Int] = {
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
  }

  def checkValid(seq: Seq[Boolean], nums: Seq[Int]): Boolean = {
    serialize(seq) == nums
  }

  def partialRowUpdate(table: Seq[Seq[Option[Boolean]]], rowNums: Seq[Seq[Int]], rowIdx: Int) =
    table.zipWithIndex.map { case (row,idx) =>
      if (idx == rowIdx) {
        val rr = reduceSeq(possibleSeq(row, rowNums(rowIdx)).filter(checkValid(_, rowNums(rowIdx))), row.size)
        if (rr.filter(_.isEmpty).size > row.filter(_.isEmpty).size) {
          println(s"なぜかNonefueteru rr:${rr}\tr2:$row\tnums${rowNums(rowIdx)}")
          row
        } else rr
      } else
        row
    }

//  def partialColUpdate(table: Seq[Seq[Option[Boolean]]], colNums: Seq[Seq[Int]], colIdx: Int) = {
//    table.transpose.zipWithIndex.map { case (col,idx) =>
//      if (idx == colIdx)
//        reduceSeq(possibleSeq(col, colNums(colIdx)).filter(checkValid(_, colNums(colIdx))))
//      else
//        col
//    }.transpose
//  }

//  def rowUpdate(table: Seq[Seq[Option[Boolean]]], rowNums: Seq[Seq[Int]]) =
//    table.zip(rowNums).map { tpl =>
//      reduceSeq(possibleSeq(tpl._1, tpl._2).filter(checkValid(_, tpl._2)))
//    }
//
//  def colUpdate(table: Seq[Seq[Option[Boolean]]], colNums: Seq[Int]) = {
//    table.transpose.zip(colNums).map { tpl =>
//      reduceSeq(possibleSeq(tpl._1, tpl._2).filter(checkValid(_, tpl._2)))
//    }.transpose
//  }

  def doIter(table: Seq[Seq[Option[Boolean]]], rowNums: Seq[Seq[Int]], colNums: Seq[Seq[Int]], cnt: Int) = {
    val rowSize = rowNums.size
    val colSize = colNums.size
    val i = cnt % (rowSize + colSize)
    val isRow = i < rowSize
    if (isRow)
      partialRowUpdate(table, rowNums, i)
    else
      partialRowUpdate(table.transpose, colNums, i - rowSize).transpose
  }


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
  val rowNums = (1 to tableHeight).toList.map(_ => readLine().split(" ").toList.map(_.toInt))
  println("Input numbers on the top (separated by space) /上側に付与される数字をスペース区切りで各行入力してください")
  val colNums = (1 to tableWidth).toList.map(_ => readLine().split(" ").toList.map(_.toInt))
  println("Executing... / 計算します")

  val table: Seq[Seq[Option[Boolean]]] = (1 to tableHeight).map(_ =>
    (1 to tableWidth).map(_ => None))

  var res = table
  var prevNonDecided = Int.MaxValue
  var nonDecided = Int.MaxValue - 1
  var cnt = 0
//  while (nonDecided > 0 && prevNonDecided != nonDecided) {
  while (nonDecided > 0) {
    prevNonDecided = nonDecided
    res = doIter(res, rowNums, colNums, cnt)
    nonDecided = res.foldLeft(0)((acc, ele) => acc + ele.filter(_.isEmpty).size)
    cnt += 1
    println("")
    println(s"試行回数${cnt}回目\t未確定マス数:$nonDecided")
    printout(res)
    println("")
  }

}
