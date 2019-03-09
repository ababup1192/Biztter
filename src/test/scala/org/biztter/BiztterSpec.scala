package org.biztter

import org.scalatest._

class BiztterSpec extends FunSpec with Matchers {
  def signUpUsers(users: User*): Biztter = users.foldLeft(Biztter()) { _.signUp(_) }

  describe("The Biztter Project") {
    describe("ユーザ登録") {
      it("Biztterがオープンしました!") {
        assert(Biztter().users === Seq.empty)
      }

      it("JohnがBiztterに登録した。") {
        val john = User("John", 18)
        val biztter = Biztter().signUp(john)

        assert(biztter.users === Seq(john))
      }

      it("JohnとMikeがBiztterに登録した。") {
        val john = User("John", 18)
        val mike = User("Mike", 18)
        val biztter = signUpUsers(john, mike)

        assert(biztter.users === Seq(mike, john))
      }

      it("同じ名前の人は登録できない。") {
        val john1 = User("John", 18)
        val john2 = User("John", 20)
        val biztter = Biztter().signUp(john1)

        assert(biztter.signUp(john2).users === Seq(john1))
      }

      it("18歳未満は登録できない。") {
        assert(Biztter().signUp(User("Mary", 17)).users === Seq.empty)
      }

      it("空の名前は登録できない") {
        assert(Biztter().signUp(User("", 55)).users === Seq.empty)
      }
    }
  }
}
