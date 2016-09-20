/*
*************************************************************************************
* Copyright 2011 Normation SAS
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

package com.normation.rudder.web.model

import net.liftweb.util.Helpers
import com.normation.rudder.domain.policies.DirectiveId
import net.liftweb.common.Box
import scala.collection.mutable.Buffer
import net.liftweb.util.BaseField
import bootstrap.liftweb.LiftSpringApplicationContext.inject
import net.liftweb.common._
import org.joda.time.{ DateTime, LocalDate, LocalTime, Duration, Period }
import org.joda.time.format._
import com.normation.utils.Utils._
import java.util.Locale
import org.slf4j.LoggerFactory
import scala.xml._
import net.liftweb.http._
import js._
import JsCmds._
import JE._
import net.liftweb.util.Helpers._
import com.normation.cfclerk.domain.{ VariableSpec, TechniqueId, Technique }
import com.normation.exceptions.TechnicalException
import org.slf4j.LoggerFactory
import com.normation.utils.HashcodeCaching

/**
 * A displayable field has 2 methods :
 * -> toHtmlNodeSeq and toFormNodeSeq : to display the form
 */
trait DisplayableField extends {
  def toHtmlNodeSeq: NodeSeq

  def toFormNodeSeq: NodeSeq
}

sealed trait SectionChildField extends DisplayableField with Loggable {
  //retrieve the value as a client string
  def toClient: String

  // get current sections and variables in sub section
  def getAllSectionFields: List[SectionField] = this match {
    case variable: DirectiveField => Nil
    case section: SectionField =>
      section :: section.childFields.flatMap(_.getAllSectionFields).toList
  }

  /*
   * Convention: displayHtml is a "read only"
   * version of toFormNodeSeq
   */
  def displayHtml: Text

  /**
   * Remove duplicate section of that section.
   * Only mutlivalued section actually have something
   * to do here
   */
  final def removeDuplicateSections : Unit = this match {
    case m:MultivaluedSectionField => m.doRemoveDuplicateSections //stop recursion here: no multivalued section on multivalued section
    case other : DirectiveField  => ()
    case other : SectionField => other.childFields.foreach { _.removeDuplicateSections }
  }
}

trait DirectiveField extends BaseField with SectionChildField {
  val id: String

  require(nonEmpty(id), "A field ID can not be null nor empty")

  def manifest: Manifest[ValueType]
  override def required_? = true

  // deprecated but has to be defined
  def is = get

  /* parseClient / toClient : get and set value from/to
   * web ui.
   *
   * this.get should be invariant with
   * parseClient(toClient).
   *
   */

  //Set value from a client value.
  //update list error accordingly
  def parseClient(s: String): Unit

  //reference to other fields used by that field
  protected var _usedFields = Seq[DirectiveField]()
  def usedFields_=(fields: Seq[DirectiveField]): Unit = {
    _usedFields = fields
  }
  def usedFields = _usedFields

  private var description: String = ""
  override def displayName = description
  def displayName_=(s: String): Unit = description = s

  //long description
  private var longDescription: String = ""
  def tooltip = longDescription
  def tooltip_=(s: String): Unit = longDescription = s

  private var mayBeEmpty: Boolean = false
  def optional = mayBeEmpty
  def optional_=(b: Boolean): Unit = mayBeEmpty = b

  //long description
  private var zone: Option[String] = None
  def section = zone
  def section_=(s: Option[String]): Unit = zone = s

  /**
   * Get possible values for that fields, if supported.
   * If not supported, return none.
   * filter may be given
   */
  def getPossibleValues(filters: (ValueType => Boolean)*): Option[Set[ValueType]]

  def getDefaultValue: ValueType

  /**
   * Define if the field is readonly or not
   */
  private var readOnly: Boolean = false
  def isReadOnly = readOnly
  def isReadOnly_=(s:Boolean):Unit = readOnly = s

  override def displayHtml = Text(toClient)

  def tooltipElem = {
    if (tooltip == "") {
      NodeSeq.Empty
    } else {
      val tooltipid = Helpers.nextFuncName
      <span class="tw-bs" ><span tooltipid={tooltipid} class="ruddericon tooltipable glyphicon glyphicon-question-sign" title=""></span></span>
      <div class="tooltipContent" id={tooltipid}>{tooltip}</div>
    }
  }

  def display(value: NodeSeq) = {
    <div class="form-group">
      <label class="topAlign" for={id} >
        { if (optional) displayName else <b>{ displayName}</b> }
        {tooltipElem}
        {if (optional) <span class="tw-bs"> - <small style="color:#999;">Optional</small></span>}
      </label>
      {value}
    </div>
  }

  override def toFormNodeSeq = {
    toForm match {
      case Failure(m, _, _) =>
        val errorMess = "Can not map field %s to an input, error message: %s"
        logger.error(errorMess.format(displayName, m))
        NodeSeq.Empty
      case Empty =>
        val errorMess = "Can not map field %s to an input, " +
          "form representation of the field was empty"
        logger.error(errorMess.format(displayName))
        NodeSeq.Empty
      case Full(form) =>
        display( form)
    }
  }

  def toHtmlNodeSeq = display( displayValue )

  // This is only used when showing a PT, hence the values are the default values
  def displayValue: NodeSeq = {
    displayHtml match {
      case value: NodeSeq if value.toString.trim == "" => <span></span>
      case value: NodeSeq => Text("(defaults to ") ++ value ++ Text(")")
    }
  }
}

object DirectiveField {
  val logger = LoggerFactory.getLogger(classOf[DirectiveField])
}

trait SectionField extends SectionChildField {
  def name: String
  def childFields: Seq[SectionChildField]
  def values: Map[String, () => String]
  def mapValueSeq: Map[String, Seq[String]]

  def displayHtml = Text(toClient)

  // A Section may be displayed or not-displayed by default
  // for the current user (we need to figure out
  // how to specify it per user)
  def displayedByDefault : Boolean
  // this is the user selected value :
  // - the user may not have choosen anything, so the default applied
  // - the user want to have the section displayed
  // - the user want to have the section hidden
  var displayed : Option[Boolean] = Option.empty[Boolean]

  def collectVariables(onlyDirect: Boolean): Map[String, DirectiveField] = {
    childFields.flatMap { x => x match {
        case v: DirectiveField => Seq((v.id -> v))
        case s: SectionField   =>
          if(onlyDirect) {
            Seq[(String, DirectiveField)]()
          } else {
            s.collectVariables(onlyDirect)
      }
    } }.toMap
  }

  // get all variables in that section
  def getAllDirectVariables: Map[String, DirectiveField] = collectVariables(true)
  //get all sub-variables
  def getAllVariables: Map[String, DirectiveField] = collectVariables(false)

  def isMultivalued = this match {
    case _: MultivaluedSectionField => true
    case _ => false
  }

  /**
   * Ajax method to define the visibility status of a section
   * Takes sectionId : the id of the section as a parameter
   * Caution : it mutates the current field
   */
  def visibilityCallBack(sectionId: String) : net.liftweb.http.GUIDJsExp = {
    SHtml.ajaxCall(
       JsRaw("")
     , (v:String) => {
            displayed = Some(!displayed.getOrElse(displayedByDefault))
            JsRaw(s"""
               $$('#${sectionId} .box-header .glyphicon').toggleClass('glyphicon-chevron-right glyphicon-chevron-down')
               $$('#${sectionId}-content').toggle(400); """)
      }
   )
  }

  /**
   * Based on the default visibility and user selected visibility,
   * returns the proper display classes
   */
  def visibilityClasses : String = {
    displayed.getOrElse(displayedByDefault) match {
      case true => "unfoldedSection"
      case false => "foldedSection"
    }
  }
}

case class SectionFieldImp(
  val name               : String,
  val childFields        : Seq[SectionChildField],
  val displayedByDefault : Boolean,
  // Only variables of the current section have entries in the values map
  // the key of type String is the id (variable name),
  // the value is a function which should be called at validation time
  val values     : Map[String, () => String]) extends SectionField with HashcodeCaching {

  def copy(): Nothing = throw new TechnicalException("Can't copy DirectiveFieldGroup, it contains mutable datas")
  def toClient = childFields.mkString

  def mapValueSeq: Map[String, Seq[String]] = values.map { case (k, v) => (k, Seq(v())) }

  // If a section is empty, we want to hide it.

  override def toFormNodeSeq: NodeSeq = {
    val childrenXml = childFields map (f => f.toFormNodeSeq)
    val sectionId = Helpers.nextFuncName
    val changeVisibility = visibilityCallBack(sectionId)
    // set the method name for the ajax call back (the guid plus the mandatory () to define/call it
    val methodName = changeVisibility.guid + "()"

    val classes = "box box-default " + visibilityClasses
    if(childrenXml.isEmpty) NodeSeq.Empty
    else
        <div  id={sectionId} class={classes}>
         <div class="box-header" onClick={methodName}>
           <h4 class="box-title">{ name }</h4>
           <div class="box-tools pull-right">
           <button data-widget="collapse" class="btn btn-box-tool"><i class="fa fa-chevron-down"></i></button>
           </div>
         </div>
         <div id={sectionId+"-content"} class="box-body" >    { childrenXml }</div>
        </div> ++ Script(JsRaw(""" function %s { %s } """.format(methodName, changeVisibility.toJsCmd)))
  }

  override def toHtmlNodeSeq = {
    val childrenXml = childFields map (f => f.toHtmlNodeSeq)
    if(childrenXml.isEmpty) NodeSeq.Empty
    else
        <div>
        <h4>{ name }</h4>
              { childrenXml }
        </div>
  }
}

case class MultivaluedSectionField(
    val sections          : Seq[SectionField]
  , private val newSection: () => SectionField
  , val displayedByDefault: Boolean
  , val readOnlySection   : Boolean
) extends SectionField with HashcodeCaching {
  require(!sections.isEmpty)

  val name: String = sections.head.name

  def childFields: Seq[SectionChildField] = allSections.foldLeft(Seq[SectionChildField]())((seq, section) => seq ++ section.childFields)
  def values: Map[String, () => String] = allSections.foldLeft(Map[String, () => String]())((map, child) => map ++ child.values)

  private val htmlId = Helpers.nextFuncName

  private def logError(box: Box[_]): Unit = box match {
    case Failure(m, _, _) => logger.error(m)
    case Empty => logger.error("Empty value was returned")
    case _ => //ok
  }

  private val allSections = sections.toBuffer

  def toClient: String = childFields.mkString

  def add(section: SectionField = newSection()): Int = {
    if (!readOnlySection) {
      synchronized {
        allSections += section
        allSections.size - 1
      }
    } else {
      allSections.size-1
    }
  }

  /**
   * Remove list group with index "index"
   * @param index
   * @return the new size of the otherSections, or an error
   */
  def delete(index: Int): Box[Int] = {
    if (!readOnlySection) {
      synchronized {
        if (index < 0) {
          Failure("Index must be a positive integer")
        } else if (index >= allSections.size) {
          Failure("Index (%s) must be lesser than number of sections (%s)".format(index, allSections.size))
        } else {
          allSections remove index
          Full(allSections.size)
        }
      }
    } else {
      Failure("Cannot modify read only parameters")
    }
  }

  def doRemoveDuplicateSections : Unit = {
    val sects = allSections.map{ sect => sect.getAllSectionFields.map { _.mapValueSeq } }.zipWithIndex

    //find duplicates: set of ids to remove
    val toRemove  = sects.map { case (s, i) =>
      sects.collect { case(s2, i2) if i2 > i && s == s2 => i2 }
    }.flatten.toSet

    //section to keep
    val toKeep = sects.collect { case (_,i) if(!toRemove.contains(i)) => allSections(i) }

    //ok, remove duplicate: swap current section with toKeep
    synchronized {
      allSections.clear
      allSections ++= toKeep
    }
    ()
  }

  def size = synchronized { allSections.size }
  def iterator = synchronized { allSections.iterator }

  /**
   * Return the Map of (variable name -> seq of values)
   * with values ordered by listname index:
   * for each variable name "key", values(key)(i) belongs
   * to the same iteration of listname.
   */
  def mapValueSeq: Map[String, Seq[String]] = {
    import scala.collection.mutable.{ Buffer, Map }
    val map = Map[String, Buffer[String]]()
    for {
      sect <- allSections
      (name, values) <- sect.mapValueSeq
    } {
      if (!map.isDefinedAt(name))
        map(name) = Buffer[String]()
      map(name) ++= values
    }
    map.toMap
  }
  /**
   * Simple form presentation: each section is iterated, and a
   * delete button is added to them.
   * A add button is added at the bottom.
   * @return
   */
  def toFormNodeSeq: NodeSeq = {
    <div id={ htmlId }>{ content }</div>
  }

  private def content: NodeSeq = {
      <div class="directiveGroup">{
        (allSections.zipWithIndex.map {
          case (section, i) =>
            val sectionId = Helpers.nextFuncName
            val changeVisibility = section.visibilityCallBack(sectionId)
            // set the method name for the ajax call back (the guid plus the mandatory () to define/call it
            val methodName = changeVisibility.guid + "()"

            val classes = "box box-default " + section.visibilityClasses
            <div  id={sectionId} class={classes}>
              <div class="box-header" onClick={methodName}>
                <h4 class="box-title">{ "%s #%s".format(name, i + 1) } {
                if (!readOnlySection) {
                  val attr= (if (size > 1) ("" -> "") else ("disabled" -> "true"))
                  SHtml.ajaxButton(
                      "Delete"
                    , { () =>
                        logError(delete(i))
                        //refresh UI - all item of that group
                        SetHtml(htmlId, this.content) & postModificationJS()
                      }
                    , attr
                    , ("class" -> "btn btn-default dangerButton")
                  )
                }
              }
                </h4>
                <div class="box-tools pull-right">
                  <button data-widget="collapse" class="btn btn-box-tool"><i class="fa fa-chevron-down"></i></button>
                </div>

              </div>
              <div class="box-body" id={sectionId+"-content"}>{ showFormEntry(section, i) }</div>
              </div> ++
              { // showAddAnother under the last element
                if ((i + 1) == size) {
              <hr class="spacer"/>
                  showAddAnother()
                } else {
                  NodeSeq.Empty
                }
              } ++ Script(JsRaw(""" function %s { %s } """.format(methodName, changeVisibility.toJsCmd)))
        })
      }</div> ++  Script(OnLoad(JsVar("""
          $("input").not("#treeSearch").keydown( function(event) {
            processKey(event , 'policyConfigurationSave')
          } ); """)
          ))
  }

  private def showAddAnother(): NodeSeq = {
    if (!readOnlySection) {
        SHtml.ajaxButton(
            <span class="glyphicon glyphicon-plus-sign"></span>++Text("Add another")
          , { () =>
              add()
              //refresh UI - all item of that group
              SetHtml(htmlId, this.content) & postModificationJS()
            }
          , ("class" -> "btn btn-default btn-block")
        )
    } else {
      NodeSeq.Empty
    }
  }

  private def showFormEntry(section: SectionField, i: Int): NodeSeq = {
    section.childFields flatMap (f => f.toFormNodeSeq)
  }

  /**
   * Command to correct display and behaviour after modifying sections
   */
  private[this] def postModificationJS() : JsExp = {
    JsRaw("""createTooltip(); """)
  }

  override def toHtmlNodeSeq = {
          <div class="directiveGroup tw-bs">{
            (allSections.map { sect =>
              <div class="groupFieldset">
                <h4>{ name }</h4>
                    { sect.toHtmlNodeSeq }
              </div>
            })
          }</div>
  }
}

/**
 * A stateful class that maintains information about
 * a Directive and every things needed in the web part to
 * configure it (fields, etc).
 *
 * If it the Technique provides expected reports, we don't show anything (for the moment)
 *
 * @parameter Directive
 *   Directive: the Directive for witch this editor is build
 */
case class DirectiveEditor(
  //       techniqueId / directiveId here.
    val techniqueId            : TechniqueId
  , val directiveId            : DirectiveId
  , val name                   : String
  , val description            : String
  , val sectionField           : SectionField
  , val variableSpecs          : Map[String, VariableSpec]
  , val providesExpectedReports: Boolean
  )  extends HashcodeCaching with Loggable {

  // We do not remove duplicate in case of meta-technique
  def removeDuplicateSections : Unit = providesExpectedReports match {
    case true => Unit
    case false => sectionField.removeDuplicateSections
  }

  /**
   * Get the map of (varname, list(values)),
   * as awaited by LDAPRuleID
   */
  def mapValueSeq: Map[String, Seq[String]] = {
    def mergeMap(m1:Map[String, Seq[String]], m2:Map[String, Seq[String]]) = {
      val res = scala.collection.mutable.Map[String,Seq[String]]() ++ m1
      for {
        (k,v) <- m2
      } {
        res(k) = res.getOrElse(k, Seq()) ++ v
      }
      res.toMap
    }

    sectionField.getAllSectionFields.foldLeft(Map[String, Seq[String]]()) { (map, sect) =>
      mergeMap(map, sect.mapValueSeq)
    }
  }

  def toFormNodeSeq: NodeSeq = {
    <div class="variableDefinition tw-bs">
        { if (!providesExpectedReports) sectionField.childFields.flatMap(_.toFormNodeSeq) }
    </div>
  }

  def toHtmlNodeSeq: NodeSeq = {
    <div class="policyDisplay">
      <div class="variableDefinition">
        <br/>
        <div>Variables to be defined for this Technique</div>
          { sectionField.childFields.flatMap(_.toHtmlNodeSeq) }
      </div>
    </div>
  }
}
