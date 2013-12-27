package fermat
import scala.xml.XML

object _Main {
  def main(args: Array[String]) = {
   
  val xml = <my_users>
        <follow>
                <account><id>1</id><name>����follow����</name><follower_num>1</follower_num></account>
                <account><id>2</id><name>follow1</name><follower_num>2</follower_num></account>
                <account><id>3</id><name>follow1</name><follower_num>3</follower_num></account>
        </follow>
        <follower>
                <account><id>4</id><name>follower1</name><follower_num>4</follower_num></account>
                <account><id>5</id><name>follower2</name><follower_num>5</follower_num></account>
                <account><id>1</id><name>����follow����</name><follower_num>1</follower_num></account>
        </follower>
</my_users>

    val my_follow = xml \ "follow"
    val my_follower = xml \ "follower"

    //-----follow���Ă���l�̖��O��\������ꍇ-----//
    println("##########follow���Ă���l�̖��O��\��##########")
    (my_follow \ "account").foreach(n => println((n \ "name").text))
    //���������ł�OK
    //my_follow.foreach(p => (p \ "account").foreach(n => println((n \ "name").text)))

    //-----follow���Ă���l�̒���follower����l�ȏア��l�̖��O��\��-----//
    println("##########follow���Ă���l�̒���follower����l�ȏア��l�̖��O��\��##########")
    (my_follow \ "account").filter(n => (n \ "follower_num").text.toInt >= 2).foreach(s => println((s \ "name").text))

    //-----����follow���Ă���l�̖��O��\��-----//
    println("##########����follow���Ă���l�̖��O��\��##########")
    val follower_ids = (my_follower \ "account").map(n => (n \ "id").text.toInt)
    (my_follow \ "account").filter(p => follower_ids.find(_ == (p \ "id").text.toInt) != None).foreach(s => println((s \ "name").text))
  }
}