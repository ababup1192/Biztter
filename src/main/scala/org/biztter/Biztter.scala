package org.biztter

case class Biztter(users: Seq[User] = Seq.empty) {

  def signUp(user: User): Biztter =
    if (user.name == "" || user.name.trim == "" || user.age < 18 || users.exists(u => u.name == user.name))
      Biztter(users)
    else
      Biztter(user +: users)


  def screenNameBy(name: String): Option[String] =
    users
      .find(user => user.name == name)
      .map(user =>
        if (user.screenName.isDefined)
          user.screenName.get
        else
          user.name
      )

}

case class User(name: String, age: Int, screenName: Option[String] = None)


