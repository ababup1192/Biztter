package org.biztter

case class Biztter(users: Seq[User] = Seq.empty) {

  def signUp(user: User): Biztter = {
    var b = this
    var f = false
    var c = 0

    if (user.name == "" || user.name.trim == "") b = Biztter(users)
    else if (user.age < 18) {
      b = Biztter(users)
    } else {
      while (users.length > c) {
        if (users(c).name == user.name) {
          f = true
        }
        c += 1
      }
      if (f)
        b = Biztter(users)
      else b = Biztter(user +: users)
    }
    b
  }

}

case class User(name: String, age: Int)




