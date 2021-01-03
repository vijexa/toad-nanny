
package toadnanny

import io.circe.generic.extras.{ Configuration, ConfiguredJsonCodec }

object Messages {
  implicit val config: Configuration = 
    Configuration.default.withSnakeCaseMemberNames

  object Request {
    
  }
  
  object Response {
    @ConfiguredJsonCodec case class GetHistory (response: GetHistoryResponse)
    @ConfiguredJsonCodec case class GetHistoryResponse (items: List[DialogMessage])
    @ConfiguredJsonCodec case class DialogMessage (id: Long, fromId: Long, body: String)

    @ConfiguredJsonCodec case class SendMessageResponse (response: Long)
  }
}
