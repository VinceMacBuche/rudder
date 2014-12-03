package com.normation.rudder.web.snippet

import bootstrap.liftweb.RudderConfig
import net.liftweb.common._
import net.liftweb.http.js.JsCmds._
import net.liftweb.http.js.JE.JsRaw
import net.liftweb.http.DispatchSnippet
import net.liftweb.http.SHtml
import com.normation.rudder.web.model.CurrentUser

class SendMetricsPopup extends DispatchSnippet with Loggable {

  private[this] val configService = RudderConfig.configService

  def dispatch = {
    case "display" => xml => Script(display)
  }

  def display = {
    configService.send_server_metrics match {
      case Full(Some(a)) =>
        Noop
      case Full(None) =>

        def ajaxCall(value : Option[Boolean]) = {
          SHtml.ajaxInvoke(() => configService.set_send_server_metrics(value,None,CurrentUser.getActor) match {
            case Full(_) => JsRaw(s"""$$("#sendMetricsPopup").modal('hide')""")
            case eb : EmptyBox =>
              logger.error(eb)
              JsRaw(s"""console.log('error')""")
            }
          )
        }
        OnLoad(JsRaw(s"""
        $$("#noSendMetrics").click(function(){${ajaxCall(Some(false)).toJsCmd}});
        $$("#yesSendMetrics").click(function(){${ajaxCall(Some(true)).toJsCmd}});
        $$("#sendMetricsPopup").modal()"""))
      case eb : EmptyBox =>
              logger.error(eb)
        Noop
    }
  }
}