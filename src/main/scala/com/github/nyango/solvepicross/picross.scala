package com.github.nyango.solvepicross

import scala.io.StdIn.readLine
import util.control.Breaks._

object SolvePicross extends App {
  /**
   * 左端にあるnumsに対応した埋め方を返す。
   * 残りのnums
   * e.g.
   * 下表のokに対応するもののみ返す。
   * 5 ◆◆■◆◆◆■◆◆◆◆■
   * ↓
   * ok ■■■■■□■□□□□■
   * ng □■■■■■■□□□□■ // 長さが5でない
   * ok □□■■■■■□□□□■
   * ng □□□■■■■■■□□■ // 長さが5でない
   * ng □□□■□■■■■■□■ // 左端のカタマリでなくなった
   */
  def leftistMatch(baseSeq: Seq[Option[Boolean]], nums: Seq[Int]): Seq[Int] = {
    val firstTrue = Option(baseSeq.indexOf(Some(true))).filterNot(_ == -1)
    val firstNone = Option(baseSeq.indexOf(None)).filterNot(_ == -1)
    val range = (firstNone, firstTrue) match {
      case (None, None) => Seq.empty[Int]
      case (Some(fn), None) => fn to (baseSeq.size - nums.tail.size - nums.tail.sum - nums.head)
      case (None, Some(ft)) => Seq(Seq(ft, baseSeq.size - nums.head).min)
      case (Some(fn), Some(ft)) => Seq(fn, ft, baseSeq.size - nums.head).min to Seq(ft, baseSeq.size - nums.head).min
    }
    for {
      startIdx <- range
      if (baseSeq.slice(startIdx, startIdx + nums.head).forall(e => e.isEmpty || e.contains(true)))
      if (baseSeq.lift(startIdx + nums.head).forall(e => e.isEmpty || e.contains(false)))
      if (baseSeq.lift(startIdx - 1).forall(e => e.isEmpty || e.contains(false)))
    } yield {
      startIdx
    }
  }

  def matchAll(baseSeq: Seq[Option[Boolean]], nums: Seq[Int]): Set[Seq[Int]] = {
    var resSet = Set.empty[Seq[Int]]

    def matchAllLoop(baseSeq: Seq[Option[Boolean]], nums: Seq[Int], result: Seq[Int] = Seq.empty): Unit = {
      if (nums.size > result.size) {
        val offset = result.lastOption.map(_ + nums(result.size - 1) + 1).getOrElse(0)
        leftistMatch(
          baseSeq.drop(offset),
          nums.drop(result.size)
        ).map(_ + offset)
          .foreach { rIdx =>
            matchAllLoop(baseSeq, nums, result ++ Seq(rIdx))
          }
      } else resSet += result
    }

    matchAllLoop(baseSeq, nums)
    resSet
  }

  def idxResConverter(nums: Seq[Int], res: Seq[Int], max: Int): Seq[Seq[Boolean]] = {
    if (res.nonEmpty)
      nums.zip(res).map { tpl =>
        (0 until max).map(idx => if (idx >= tpl._2 && idx < tpl._1 + tpl._2) true else false)
      }
    else Seq((0 until max).map(_ => false))
  }

  def partialRowUpdate(baseSeq: Seq[Option[Boolean]], nums: Seq[Int]): Seq[Option[Boolean]] = {
    if (baseSeq.exists(_.isEmpty)) {
      // res/ num/ seq -> num/ seq/ res
      val newSeq = matchAll(baseSeq, nums).toList.map { idxRes =>
        idxResConverter(nums, idxRes, baseSeq.size)
      }.transpose.map {
        _.transpose.map {
          case seq if seq.forall(identity) => Some(true)
          case seq if seq.forall(!_) => Some(false)
          case _ => None
        }
      }.transpose.map {
        case seq if seq.forall(_.contains(false)) => Some(false)
        case seq if seq.count(_.contains(true)) == 1 && !seq.exists(_.isEmpty) => Some(true)
        case _ => None
      }
      baseSeq.zip(newSeq).map {
        case (None, Some(b)) => Some(b)
        case (bOpt, _) => bOpt
      }
    } else baseSeq
  }

  def doIter(table: Seq[Seq[Option[Boolean]]], rowNums: Seq[Seq[Int]], colNums: Seq[Seq[Int]], rowIdx: Int, isLeft: Boolean): Seq[Seq[Option[Boolean]]] = {
    if (isLeft) {
      table.zip(rowNums).zipWithIndex.map { case ((row, nums), idx) =>
        if (idx == rowIdx) {
          partialRowUpdate(row, nums)
        } else row
      }
    } else {
      table.transpose.zip(colNums).zipWithIndex.map { case ((col, nums), idx) =>
        if (idx == rowIdx) {
          partialRowUpdate(col, nums)
        } else col
      }.transpose
    }
  }

  def printout(res: Seq[Seq[Option[Boolean]]], isRow: Boolean, changedIdx: Int): Unit = {
    if (isRow)
      println("")
    else {
      print(" ")
      (0 until res.transpose.size).foreach(idx =>
        if (idx == changedIdx) print("V") else print(" ")
      )
      println("")
    }
    res.zipWithIndex.map { case (row, idx) =>
      if (idx == changedIdx && isRow)
        print(">")
      else
        print(" ")
      println(row.map {
        case Some(true) => "■"
        case Some(false) => "□"
        case None => "◆"
      }.mkString(""))
    }
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
  var nextIdx = 0
  var isRow = true
  var validRows = res.map(_.exists(_.isEmpty))
  var validCols = res.transpose.map(_.exists(_.isEmpty))
  var lastFailure = (Int.MaxValue, -1, true)
  while (nonDecided > 0) {
    if (nonDecided == lastFailure._1 && lastFailure._2 == nextIdx && lastFailure._3 == isRow) {
      println("Give up ;;")
      sys.exit(0)
    }
    if (nonDecided < lastFailure._1) {
      if (prevNonDecided == nonDecided) {
        lastFailure = (nonDecided, nextIdx, isRow)
      } else {
        lastFailure = (Int.MaxValue, -1, true)
      }
    }
    // search next non decided cell
    nextIdx += 1
    if (isRow) {
      if (nextIdx >= rowNums.size) {
        nextIdx = 0
        isRow = false
      }
    } else {
      if (nextIdx >= colNums.size) {
        nextIdx = 0
        isRow = true
      }
    }
    breakable {
      if (isRow) {
        if (!res(nextIdx).exists(_.isEmpty)) {
          break
        }
      } else {
        if (!res.transpose.apply(nextIdx).exists(_.isEmpty)) {
          break
        }
      }
      println(s"nextIdx:$nextIdx\tisRow:$isRow")
      prevNonDecided = nonDecided
      res = doIter(res, rowNums, colNums, nextIdx, isRow)
      nonDecided = res.foldLeft(0)((acc, ele) => acc + ele.filter(_.isEmpty).size)
      cnt += 1
      println("")
      println(s"試行回数${cnt}回目\t未確定マス数:$nonDecided")
      printout(res, isRow, nextIdx)
      println("")
    }
  }

}
