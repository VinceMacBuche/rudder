package com.normation.rudder.web.rest

import net.liftweb.http.rest.RestHelper
import net.liftweb.common.Loggable
import com.normation.rudder.web.rest.RestUtils._
import net.liftweb.common.Full
import org.apache.commons.codec.binary.Base64
import net.liftweb.util.SecurityHelpers._
import net.liftweb.common.Box
import net.liftweb.common.Failure
import org.springframework.security.authentication.AuthenticationProvider
import bootstrap.liftweb.RudderInMemoryUserDetailsService
import bootstrap.liftweb.AppConfigAuth
import net.liftweb.common.EmptyBox
import com.normation.rudder.web.model.CurrentUser

class RestAuth extends RestHelper with Loggable {
  serve( "api" / "auth" prefix {

    case Nil JsonGet req =>
      val auth = req.header("Authorization") match {case Full(Basic(entry)) =>       logger.warn(new String(Base64.decodeBase64(entry),"UTF-8"))
        val Full((username,pass)) = convertAuthToUserCred(entry)
        logger.info(convertAuthToUserCred(entry))
        try {
      //            logger.info(authprov.loadUserByUsername(username))
        //  val user = authprov.loadUserByUsername(username)
          //if (user.password == pass)
          CurrentUser.get.get.getUsername
          Full (   CurrentUser.get.get.getUsername)
          //else Failure("password error")
        } catch { case e:Exception => Failure("wrong username") }

      case _ => logger error ("blabla")
      Failure("error while retrieving user")}
      auth match { case Full(ok) =>
        toJsonResponse("N/A", ok, RestOk)("Auth")
      case eb:EmptyBox => val msg = eb ?~ "could not identify"
               toJsonResponse("N/A", msg.msg, RestError)("Auth")
      }

    } )

  def convertAuthToUserCred (auth:String) : Box[(String,String)] = new String(base64Decode(auth),"utf-8") match {
    case User(name,pass) => Full((name,pass))
    case _ => Failure("not a valid user")
  }
  val Basic = """Basic (.*)""".r

  val User = """(.*):(.*)""".r

//  val authprov : RudderInMemoryUserDetailsService = new AppConfigAuth().userService
}
