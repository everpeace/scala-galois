package secret_share

import akka.util.Timeout
import scala.concurrent.duration._

package object shamir {
  // actor ? something
  implicit val timeout = Timeout(500 seconds)
}
