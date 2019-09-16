package com.github.nyango.solvepicross

import org.scalatest._

class SolvePicrossSpec extends FlatSpec with Matchers {
  val target = SolvePicross

  /**
   * 左端にあるnumに対応した埋め方を返す
   * e.g.
   * 下表のokに対応するもののみ返す。
   *  5 ◆◆■◆◆◆■◆◆◆◆■
   *  ↓
   * ok ■■■■■□■□□□□■
   * ng □■■■■■■□□□□■ // 長さが5でない
   * ok □□■■■■■□□□□■
   * ng □□□■■■■■■□□■ // 長さが5でない
   * ng □□□■□■■■■■□■ // 左端のカタマリでなくなった
   */
  val example1 = Seq(Some(false), Some(false), None, None, Some(true), None, None, None, Some(true), None, None, None, None, Some(true))
  val example2 = Seq(None, None, Some(true), None, None, None, Some(true), None, None, None, None, Some(true))
  val nums = Seq(5, 1)
  val renderExample1 = Seq(Seq(false, false, true, true, true, true, true, false, false, false, false, false, false, false), Seq(false, false, false, false, false, false, false, false, true, false, false, false, false, false))
  val expectedResult1 = List(Some(false), Some(false), None, None, Some(true), Some(true), Some(true), None, None, Some(false), None, None, Some(false), None)

  "leftistMatch" should "show all possible locations for leftist num" in {
    target.leftistMatch(example1, nums) should be(Seq(2, 4))
    target.leftistMatch(example2, nums) should be(Seq(0, 2))
  }
  "matchAll" should "work" in {
    target.matchAll(example1, nums) should be(Set(List(2, 8), List(4, 10), List(4, 11), List(4, 13)))
    target.matchAll(example2, nums) should be(Set(List(0, 6), List(2, 8), List(2, 9), List(2, 11)))
  }
  "idxResConverter" should "work" in {
    target.idxResConverter(nums, Seq(2, 8), example1.size) should be(renderExample1)
  }
  "partialRowUpdate" should "work" in {
    target.partialRowUpdate(example1, nums) should be(expectedResult1)
  }
}