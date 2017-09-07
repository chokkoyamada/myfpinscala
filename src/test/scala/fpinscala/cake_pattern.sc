/*
case class User(id: Int, name: String, mail: String)

class DB(host: String, port: Int) {
  def findUserByMail(mail: String): Option[User] = ???
}

class App(db: DB) {
  def showUserName(mail: String) {
    db.findUserByMail(mail) match {
      case None => println("no such user: " + mail)
      case Some(user) => println("name: " + user.name)
    }
  }
}
*/

trait DBComponent {
  val db: DBComponent
  class DB {

  }
}

trait AppComponet {
  this: DBComponent =>
  val app: App
  class App {

  }
}

trait NiceEnvironment extends DBComponent with AppComponet {
  val db= new DB("nice.environment", 777)
  val app = new App
}

object Great extends NiceEnvironment {

}
