package com.normation.rudder.web.rest.directive

import com.normation.rudder.repository.RoDirectiveRepository
import com.normation.rudder.web.services.rest.RestExtractorService
import com.normation.rudder.web.rest.RestUtils._
import net.liftweb.http.Req
import net.liftweb.http.rest.RestHelper
import net.liftweb.common._
import net.liftweb.http.LiftResponse
import com.normation.rudder.web.rest.RestError
import com.normation.rudder.web.rest.directive.DirectiveAPI

class DirectiveAPIHeaderVersion (
    readDirective             : RoDirectiveRepository
  , restExtractor        : RestExtractorService
  , apiV1_0              : DirectiveAPIService1_0
) extends RestHelper with DirectiveAPI with Loggable{


  val requestDispatch : PartialFunction[Req, () => Box[LiftResponse]] = {

    case Get(Nil, req) => {
      req.header("X-API-VERSION") match {
        case Full("1.0") => apiV1_0.listDirectives(req)
        case _ => notValidVersionResponse("listDirectives")
      }
    }

    case Put(Nil, req) => {
      req.header("X-API-VERSION") match {
        case Full("1.0") =>
          val restDirective = restExtractor.extractDirective(req.params)
          apiV1_0.createDirective(restDirective, req)
        case _ => notValidVersionResponse("createDirective")
      }
    }

    case Get(id :: Nil, req) => {
      req.header("X-API-VERSION") match {
        case Full("1.0") => apiV1_0.directiveDetails(id, req)
        case _ => notValidVersionResponse("listDirectives")
      }
    }

    case Delete(id :: Nil, req) => {
      req.header("X-API-VERSION") match {
        case Full("1.0") => apiV1_0.deleteDirective(id,req)
        case _ => notValidVersionResponse("listDirectives")
      }
    }

    case Post(id:: Nil, req) => {
      req.header("X-API-VERSION") match {
        case Full("1.0") =>
          val restDirective = restExtractor.extractDirective(req.params)
          apiV1_0.updateDirective(id,req,restDirective)
        case _ => notValidVersionResponse("listDirectives")
      }
    }

/*    case id :: Nil JsonPost body -> req => {
      req.header("X-API-VERSION") match {
        case Full("1.0") =>
      req.json match {
        case Full(arg) =>
          val restDirective = restExtractor.extractDirectiveFromJSON(arg)
          apiV1_0.updateDirective(id,req,restDirective)
        case eb:EmptyBox=>    toJsonResponse(id, "no args arg", RestError)("Empty",true)
      }
        case _ => notValidVersionResponse("listDirectives")
      }

    }
*/
    case content => println(content)
         toJsonResponse("nothing", "rien", RestError)("error",true)

  }
  serve( "api" / "directives" prefix requestDispatch)



  def notValidVersionResponse(action:String) = {
    toJsonResponse("badversion", "version x does not exists", RestError)(action,true)
   }

}
