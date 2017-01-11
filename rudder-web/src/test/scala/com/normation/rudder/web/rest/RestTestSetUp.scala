/*
*************************************************************************************
* Copyright 2016 Normation SAS
*************************************************************************************
*
* This file is part of Rudder.
*
* Rudder is free software: you can redistribute it and/or modify
* it under the terms of the GNU General Public License as published by
* the Free Software Foundation, either version 3 of the License, or
* (at your option) any later version.
*
* In accordance with the terms of section 7 (7. Additional Terms.) of
* the GNU General Public License version 3, the copyright holders add
* the following Additional permissions:
* Notwithstanding to the terms of section 5 (5. Conveying Modified Source
* Versions) and 6 (6. Conveying Non-Source Forms.) of the GNU General
* Public License version 3, when you create a Related Module, this
* Related Module is not considered as a part of the work and may be
* distributed under the license agreement of your choice.
* A "Related Module" means a set of sources files including their
* documentation that, without modification of the Source Code, enables
* supplementary functions or services in addition to those offered by
* the Software.
*
* Rudder is distributed in the hope that it will be useful,
* but WITHOUT ANY WARRANTY; without even the implied warranty of
* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
* GNU General Public License for more details.
*
* You should have received a copy of the GNU General Public License
* along with Rudder.  If not, see <http://www.gnu.org/licenses/>.

*
*************************************************************************************
*/

package com.normation.rudder.web.rest

import org.junit._
import org.junit.Assert._
import org.junit.runner._
import org.junit.runner.RunWith
import org.specs2.matcher.MatchResult
import org.specs2.mutable._
import org.specs2.runner._

import net.liftweb.common.Full
import net.liftweb.http.LiftRules
import net.liftweb.http.LiftRulesMocker
import net.liftweb.http.PlainTextResponse
import net.liftweb.http.Req
import net.liftweb.mocks.MockHttpServletRequest
import net.liftweb.mockweb.MockWeb
import com.normation.utils.StringUuidGeneratorImpl
import com.normation.cfclerk.services.UpdateTechniqueLibrary
import com.normation.eventlog.EventActor
import com.normation.eventlog.ModificationId
import com.normation.cfclerk.services.TechniquesLibraryUpdateNotification
import net.liftweb.common.Box
import com.normation.cfclerk.services.TechniquesLibraryUpdateType
import com.normation.cfclerk.domain.TechniqueName
import net.liftweb.http.LiftResponse
import net.liftweb.util.NamedPF
import net.liftweb.http.S
import net.liftweb.common.EmptyBox
import net.liftweb.json.JsonAST.JValue
import com.normation.rudder.repository.RoDirectiveRepository
import com.normation.cfclerk.domain._
import com.normation.rudder.domain.policies._
import com.normation.rudder.repository.FullActiveTechnique
import com.normation.rudder.repository.FullActiveTechniqueCategory
import scala.collection.SortedMap
import org.joda.time.DateTime
import com.normation.cfclerk.services.TechniqueRepository
import scala.collection.SortedSet
import com.normation.rudder.repository.CategoryWithActiveTechniques
import java.io.InputStream
import com.normation.cfclerk.services.TechniquesInfo
import com.normation.rudder.web.rest.directive.DirectiveAPIService2
import com.normation.rudder.repository.WoDirectiveRepository
import net.liftweb.common.Failure
import com.normation.rudder.services.policies.DeploymentServiceImpl
import com.normation.rudder.batch.AsyncDeploymentAgent
import com.normation.rudder.services.eventlog.EventLogDeploymentService
import com.normation.rudder.services.marshalling.DeploymentStatusSerialisationImpl
import com.normation.rudder.domain.Constants
import com.normation.rudder.services.workflows.ChangeRequestServiceImpl
import com.normation.rudder.services.workflows.NoWorkflowServiceImpl
import com.normation.rudder.repository.inmemory.InMemoryChangeRequestRepository
import com.normation.rudder.web.services.DirectiveEditorServiceImpl
import bootstrap.liftweb.AppConfig
import bootstrap.liftweb.RudderConfig

object RestBaseObjects {
     val techniqueId = TechniqueId(
        TechniqueName("techniqueName")
      , TechniqueVersion("1.0")
    )

  /* create representation of meta techniques */
  def makePredefinedSectionSpec(name: String, providedValues: (String, Seq[String])) =
    PredefinedValuesVariableSpec(
        reportKeysVariableName(name)
      , "description"
      , providedValues
    )
  def makeComponentSectionSpec(name:String) =
    SectionSpec(
        name
      , true
      , true
      , Some(reportKeysVariableName(name))
      , HighDisplayPriority
      , ""
      , Seq(makePredefinedSectionSpec(name, ("variable_"+name, Seq("variable_"+name+"one", "variable_"+name+"two"))))
    )

  def makeRootSectionSpec() =
    SectionSpec(
        "root section"
      , false
      , false
      , None
      , HighDisplayPriority
      , ""
      , Seq(
            makeComponentSectionSpec("component1")
          , makeComponentSectionSpec("component2")
        )
    )

  def makeMetaTechnique(id: TechniqueId) =
    Technique(
        id
      , "meta" + id
      , ""
      , Seq()
      , Seq()
      , TrackerVariableSpec(None)
      , makeRootSectionSpec
      , None
      , Set()
      , None
      , false
      , ""
      , false
      , true
    )

  val technique = makeMetaTechnique(techniqueId)

  val fullActiveTechnique =
    FullActiveTechnique(
        ActiveTechniqueId("activeTechId")
      , techniqueId.name
      , SortedMap((techniqueId.version, new DateTime()))
      , SortedMap((techniqueId.version, technique))
      , Nil
      , true
      , false
    )

  // create the ActiveTechniqueCategory
  val fullActiveTechniqueCategory =
    FullActiveTechniqueCategory(
         ActiveTechniqueCategoryId("id")
       , "name"
       , "description"
       , Nil
       , List(fullActiveTechnique)
       , false
    )
}

object MemoryTechRepository extends TechniqueRepository {

  import RestBaseObjects._

  var techniques = Map((techniqueId,technique))
  def getMetadataContent[T](techniqueId: TechniqueId)(useIt: Option[InputStream] => T): T = ???
  def getTemplateContent[T](templateName: Cf3PromisesFileTemplateId)(useIt: Option[InputStream] => T): T = ???
  def getReportingDetailsContent[T](techniqueId: TechniqueId)(useIt: Option[InputStream] => T): T = ???
  def getTechniquesInfo(): TechniquesInfo = ???
  def getAll(): Map[TechniqueId, Technique] = techniques
  def get(techniqueId: TechniqueId): Option[Technique] = techniques.get(techniqueId)
  def getLastTechniqueByName(techniqueName: TechniqueName): Option[Technique] = ???
  def getByIds(techniqueIds: Seq[TechniqueId]): Seq[Technique] = ???
  def getTechniqueVersions(name:TechniqueName) : SortedSet[TechniqueVersion] = ???
  def getByName(name:TechniqueName) : Map[TechniqueVersion, Technique] =  ???
  def getTechniqueLibrary: RootTechniqueCategory = ???
  def getTechniqueCategory(id: TechniqueCategoryId): Box[TechniqueCategory] = ???
  def getParentTechniqueCategory_forTechnique(id: TechniqueId): Box[TechniqueCategory] = ???

}

object MemoryDirectiveRepository extends RoDirectiveRepository with WoDirectiveRepository {

  import RestBaseObjects._

  var rootCategory =
    FullActiveTechniqueCategory(
        ActiveTechniqueCategoryId("root")
      , "root"
      , ""
      , fullActiveTechniqueCategory :: Nil
      , Nil
      , false
    )
  def getFullDirectiveLibrary() : Box[FullActiveTechniqueCategory] = Full(rootCategory)
  def getDirective(directiveId:DirectiveId) : Box[Directive] = {
     getActiveTechniqueAndDirective(directiveId).map(_._2)
   }
  def getDirectiveWithContext(directiveId:DirectiveId) : Box[(Technique, ActiveTechnique, Directive)] = {
    for {
      (activeTech,directive) <- getActiveTechniqueAndDirective(directiveId)
      activeTechniqueId = TechniqueId(activeTech.techniqueName, directive.techniqueVersion)
      technique         <- Box(MemoryTechRepository.get(activeTechniqueId)) ?~! "No Technique with ID=%s found in reference library.".format(activeTechniqueId)
    } yield {
      (technique, activeTech, directive)
    }
  }
  def getActiveTechniqueAndDirective(id:DirectiveId) : Box[(ActiveTechnique, Directive)] = {
    for {
      (activeTech,directive) <- Box(rootCategory.allDirectives.get(id)) ?~! s"Directive ${id.value} not found"
    } yield {
      (activeTech.toActiveTechnique,directive)
    }
  }
  def getDirectives(activeTechniqueId:ActiveTechniqueId, includeSystem:Boolean = false) : Box[Seq[Directive]] = {
    rootCategory.allDirectivesByActiveTechniques.get(activeTechniqueId)
  }
  def getActiveTechniqueByCategory(includeSystem:Boolean = false) : Box[SortedMap[List[ActiveTechniqueCategoryId], CategoryWithActiveTechniques]] = ???
  def getActiveTechnique(id:ActiveTechniqueId) : Box[Option[ActiveTechnique]] = {
    Full(rootCategory.allActiveTechniques.get(id).map(_.toActiveTechnique()))
  }
  def getActiveTechnique(techniqueName: TechniqueName) : Box[Option[ActiveTechnique]] = {
    Full(rootCategory.allActiveTechniques.find(_._2.techniqueName == techniqueName).map(_._2.toActiveTechnique()))
  }
  def activeTechniqueBreadCrump(id:ActiveTechniqueId) : Box[List[ActiveTechniqueCategory]] = ???
  def getActiveTechniqueLibrary : Box[ActiveTechniqueCategory] = ???
  def getAllActiveTechniqueCategories(includeSystem:Boolean = false) : Box[Seq[ActiveTechniqueCategory]] = ???
  def getActiveTechniqueCategory(id:ActiveTechniqueCategoryId) : Box[ActiveTechniqueCategory] = ???
  def getParentActiveTechniqueCategory(id:ActiveTechniqueCategoryId) : Box[ActiveTechniqueCategory] = ???
  def getParentsForActiveTechniqueCategory(id:ActiveTechniqueCategoryId) : Box[List[ActiveTechniqueCategory]] = ???
  def getParentsForActiveTechnique(id:ActiveTechniqueId) : Box[ActiveTechniqueCategory] = ???
  def containsDirective(id: ActiveTechniqueCategoryId) : Boolean = ???

  def saveDirective(inActiveTechniqueId:ActiveTechniqueId, directive:Directive, modId: ModificationId, actor:EventActor, reason:Option[String]) : Box[Option[DirectiveSaveDiff]] = {

    def saveInCategory(current : FullActiveTechniqueCategory)  : Box[Option[DirectiveSaveDiff]] = {
       current.activeTechniques.find { _.id == inActiveTechniqueId } match {
         case Some(toSaveIn) => saveInActiveTechniqueId(toSaveIn)
           Full(Some(AddDirectiveDiff(toSaveIn.techniqueName, directive)))
         case None =>
           val fail : Box[Option[DirectiveSaveDiff]] = Failure("error saving")
           (fail  /: current.subCategories.map(saveInCategory)) {
             case (_,a@Full(diff)) => Full(diff)
             case (a,_) => a
           }
       }
    }
    def saveInActiveTechniqueId(current : FullActiveTechnique) {
      val newDirectives = current.directives.filterNot { _.id == directive.id } :+ directive
      Some(current.copy(directives = newDirectives))
    }

    saveInCategory(rootCategory)
  }

  def saveSystemDirective(inActiveTechniqueId:ActiveTechniqueId,directive:Directive, modId: ModificationId, actor:EventActor, reason:Option[String]) : Box[Option[DirectiveSaveDiff]] = ???
  def delete(id:DirectiveId, modId: ModificationId, actor:EventActor, reason:Option[String]) : Box[DeleteDirectiveDiff] = ???
  def addTechniqueInUserLibrary(
      categoryId   : ActiveTechniqueCategoryId
    , techniqueName: TechniqueName
    , versions     : Seq[TechniqueVersion]
    , modId        : ModificationId
    , actor        : EventActor
    , reason       : Option[String]
  ) : Box[ActiveTechnique] = ???

  def move(id:ActiveTechniqueId, newCategoryId:ActiveTechniqueCategoryId, modId: ModificationId, actor: EventActor, reason: Option[String]) : Box[ActiveTechniqueId] = ???

  def changeStatus(id:ActiveTechniqueId, status:Boolean, modId: ModificationId, actor: EventActor, reason: Option[String]) : Box[ActiveTechniqueId] = ???

  def setAcceptationDatetimes(id:ActiveTechniqueId, datetimes: Map[TechniqueVersion,DateTime], modId: ModificationId, actor: EventActor, reason: Option[String]) : Box[ActiveTechniqueId] = ???

  def delete(id:ActiveTechniqueId, modId: ModificationId, actor: EventActor, reason: Option[String]) : Box[ActiveTechniqueId] = ???

  def addActiveTechniqueCategory(
      that : ActiveTechniqueCategory
    , into : ActiveTechniqueCategoryId //parent category
    , modificationId: ModificationId
    , actor: EventActor
    , reason: Option[String]
  ) : Box[ActiveTechniqueCategory] = ???

  def saveActiveTechniqueCategory(category:ActiveTechniqueCategory, modificationId: ModificationId, actor: EventActor, reason: Option[String]) : Box[ActiveTechniqueCategory] = ???

  def delete(id:ActiveTechniqueCategoryId, modificationId: ModificationId, actor: EventActor, reason: Option[String], checkEmpty:Boolean = true) : Box[ActiveTechniqueCategoryId] = ???

  def move(categoryId:ActiveTechniqueCategoryId, intoParent:ActiveTechniqueCategoryId, modificationId: ModificationId, actor: EventActor, reason: Option[String]) : Box[ActiveTechniqueCategoryId] = ???

}

/*
 * This file provides all the necessary plumbing to allow test REST API.
 *
 * Also responsible for setting up datas and mock services.
 */
object RestTestSetUp {

  val uuidGen = new StringUuidGeneratorImpl()
  val reloadTechniques = new RestTechniqueReload(new UpdateTechniqueLibrary() {
    def update(modId: ModificationId, actor:EventActor, reason: Option[String]) : Box[Map[TechniqueName, TechniquesLibraryUpdateType]] = {
      Full(Map())
    }
    def registerCallback(callback:TechniquesLibraryUpdateNotification) : Unit = {}
  }, uuidGen)

  val restExtractorService =
  RestExtractorService (
      null //roRuleRepository
    , null //roDirectiveRepository
    , null //roNodeGroupRepository
    , null //techniqueRepository
    , null //queryParser
    , null //userPropertyService
    , null //workflowService
  )

  val deployService = new DeploymentServiceImpl(
            null
          , null
          , null
          , null
          , null
          , null
          , null
          , null
          , null
          , null
          , null
          , null
          , null
          , null
          , null
          , null
          , null
          , null
          , null
          , null
          , null
          , null
          , null
        ) {
    override def deploy() =  Full(Set())
  }
  val agent = new AsyncDeploymentAgent (deployService, new EventLogDeploymentService(null,null), new DeploymentStatusSerialisationImpl(Constants.XML_CURRENT_FILE_FORMAT.toString))
  val restDataSerializer = RestDataSerializerImpl(
      null //techniqueRepository
    , null //diffService
    , () => Full(false)
  )

  val directiveAPIService =
    new DirectiveAPIService2 (
        MemoryDirectiveRepository
      , MemoryDirectiveRepository
      , uuidGen
      , agent

      , new ChangeRequestServiceImpl(null,null,null, null, null)
      , new NoWorkflowServiceImpl(null,new InMemoryChangeRequestRepository)
      , restExtractorService
      , () => Full(false)
      ,  new DirectiveEditorServiceImpl(MemoryTechRepository, null)
      , restDataSerializer
      , MemoryTechRepository
    )
  /*
  val dataSourceApiService = new DataSourceApiService(datasourceRepo, restDataSerializer, restExtractorService)
  val dataSourceApi9 = new DataSourceApi9(restExtractorService, dataSourceApiService, uuidGen)
*/
  val api = APIDispatcher( Map((ApiVersion(42,false)-> List())), restExtractorService)

  val liftRules = {
    val l = new LiftRules()
    l.statelessDispatch.append(RestStatus)
    l.statelessDispatch.append(reloadTechniques)
    l.statelessDispatch.append(api)
    //TODO: add all other rest classes here
    l
  }

  /*
   * Correctly build and scope mutable things to use the request in a safe
   * way in the context of LiftRules.
   */
  def doReq[T](mockReq: MockHttpServletRequest)(tests: Req => MatchResult[T]) = {
    LiftRulesMocker.devTestLiftRulesInstance.doWith(liftRules) {
      MockWeb.useLiftRules.doWith(true) {
        MockWeb.testReq(mockReq)(tests)
      }
    }
  }

  /**
   * Execute the request and get the response.
   * The request must be a stateless one, else a failure
   * will follow.
   */
  def execRequestResponse[T](mockReq: MockHttpServletRequest)(tests: Box[LiftResponse] => MatchResult[T])= {
    doReq(mockReq){ req =>
      //the test logic is taken from LiftServlet#doServices.
      //perhaps we should call directly that methods, but it need
      //much more set-up, and I don't know for sure *how* to set-up things.
      NamedPF.applyBox(req, LiftRules.statelessDispatch.toList).map(_.apply() match {
        case Full(a) => Full(LiftRules.convertResponse((a, Nil, S.responseCookies, req)))
        case r => r
      }) match {
        case Full(x)      => tests(x)
        case eb: EmptyBox => tests(eb)
      }
    }
  }

  private[this] def mockRequest (path : String, method : String) = {
    val mockReq = new MockHttpServletRequest("http://localhost:8080")
    mockReq.method = method
    mockReq.path = path
    mockReq
  }
  def GET(path: String) = mockRequest(path,"GET")
  def DELETE(path: String) = mockRequest(path,"DELETE")

  private[this] def mockJsonRequest (path : String, method : String, data : JValue) = {
    val mockReq = mockRequest(path,method)
    mockReq.body = data
    mockReq
  }

  private[this] def mockDataRequest (path : String, method : String, data : Map[String,String]) = {
    val mockReq = mockRequest(path,method)
    mockReq.body = data.map{case (key,value) => s"$key=$value"}.mkString("\n")
    mockReq
  }

  def jsonPUT(path: String, json : JValue) = {
    mockJsonRequest(path,"PUT", json)
  }

  def jsonPOST(path: String, json : JValue) = {
    mockJsonRequest(path,"POST", json)
  }

  def testGET[T](path: String)(tests: Req => MatchResult[T]) = {
    doReq(GET(path))(tests)
  }
  def testDELETE[T](path: String)(tests: Req => MatchResult[T]) = {
    doReq(DELETE(path))(tests)
  }

  def testPUT[T](path: String, json : JValue)(tests: Req => MatchResult[T]) = {
    doReq(jsonPUT(path, json))(tests)
  }
  def testPOST[T](path: String, json : JValue)(tests: Req => MatchResult[T]) = {
    doReq(jsonPOST(path, json))(tests)
  }
}
