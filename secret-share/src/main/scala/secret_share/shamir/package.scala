package secret_share

import akka.util.Timeout
import akka.util.duration._
import java.util.concurrent.Executors
import akka.dispatch.ExecutionContext

package object shamir {
  // actor ? something
  implicit val timeout = Timeout(500 seconds)
}
