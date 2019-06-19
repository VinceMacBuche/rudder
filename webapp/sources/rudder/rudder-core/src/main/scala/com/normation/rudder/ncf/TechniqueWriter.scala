/*
*************************************************************************************
* Copyright 2017 Normation SAS
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

package com.normation.rudder.ncf


import cats.implicits._
import com.normation.errors._
import com.normation.eventlog.EventActor
import com.normation.eventlog.ModificationId
import java.nio.charset.StandardCharsets

import com.normation.inventory.domain.AgentType
import com.normation.rudder.repository.GitModificationRepository
import com.normation.rudder.repository.xml.GitArchiverUtils
import com.normation.cfclerk.services.GitRepositoryProvider
import java.io.{File => JFile}
import java.nio.file.Files
import java.nio.file.Paths

import better.files.File
import com.normation.cfclerk.services.UpdateTechniqueLibrary
import com.normation.errors.IOResult
import com.normation.rudder.repository.xml.RudderPrettyPrinter
import com.normation.rudder.services.user.PersonIdentService
import net.liftweb.common.Full
import net.liftweb.common.Loggable
import zio._
import zio.syntax._

import scala.xml.NodeSeq
import scala.xml.{Node => XmlNode}
import com.normation.rudder.services.policies.InterpolatedValueCompiler
import net.liftweb.common.EmptyBox
import org.eclipse.jgit.api.Git

//import scala.language.implicitConversions

trait NcfError extends RudderError {
  def message : String
  def exception : Option[Throwable]
  def msg = message
}

final case class IOError(message : String, exception : Option[Throwable]) extends NcfError
final case class TechniqueUpdateError(message : String, exception : Option[Throwable]) extends NcfError
final case class MethodNotFound(message : String, exception : Option[Throwable]) extends NcfError

class TechniqueWriter (
    archiver         : TechniqueArchiver
  , techLibUpdate    : UpdateTechniqueLibrary
  , translater       : InterpolatedValueCompiler
  , xmlPrettyPrinter : RudderPrettyPrinter
  , basePath         : String
) extends Loggable {

  private[this] var agentSpecific = new ClassicTechniqueWriter(basePath) :: new DSCTechniqueWriter(basePath, translater) :: Nil

  def techniqueMetadataContent(technique : Technique, methods: Map[BundleName, GenericMethod]) : PureResult[XmlNode] = {

    def reportingValuePerMethod (component: String, calls :Seq[MethodCall]) : PureResult[Seq[XmlNode]] = {

      for {
        spec <- calls.toList.traverse { call =>
          for {
            method <- methods.get(call.methodId) match {
              case None => Left(MethodNotFound(s"Cannot find method ${call.methodId.value} when writing a method call of Technique '${technique.bundleName.value}'", None))
              case Some(m) => Right(m)
            }
            class_param <- call.parameters.get(method.classParameter) match {
              case None => Left(MethodNotFound(s"Cannot find call parameter of ${call.methodId.value} when writing a method call of Technique '${technique.bundleName.value}'", None))
              case Some(m) => Right(m)
            }

          } yield {
            <VALUE>{class_param}</VALUE>
          }

        }
      } yield {

        <SECTION component="true" multivalued="true" name={component}>
          <REPORTKEYS>
            {spec}
          </REPORTKEYS>
        </SECTION>

      }
    }

    def parameterSection(parameter : TechniqueParameter) : Seq[XmlNode] = {
      // Here we translate technique parameters into Rudder variables
      // ncf technique parameters ( having an id and a name, which is used inside the technique) were translated into Rudder variables spec
      // (having a name, which acts as an id, and allow to do templating on techniques, and a description which is presented to users) with the following Rule
      //  ncf Parameter | Rudder variable
      //      id        |      name
      //     name       |   description
      <INPUT>
        <NAME>{parameter.id.value.toUpperCase()}</NAME>
        <DESCRIPTION>{parameter.name.value}</DESCRIPTION>
        <CONSTRAINT>
          <TYPE>textarea</TYPE>
          <MAYBEEMPTY>false</MAYBEEMPTY>
        </CONSTRAINT>
        <LONGDESCRIPTION></LONGDESCRIPTION>
      </INPUT>
    }
    // Regroup method calls from which we expect a reporting
    // We filter those starting by _, which are internal methods
    val expectedReportingMethodsWithValues =
      for {
        (component, methodCalls) <- technique.methodCalls.filterNot(_.methodId.value.startsWith("_")).groupBy(_.component).toList.sortBy(_._1)
      } yield {
        (component, methodCalls)
      }

    for {
      reportingSection     <- expectedReportingMethodsWithValues.traverse((reportingValuePerMethod _).tupled)
      agentSpecificSection <- agentSpecific.traverse(_.agentMetadata(technique, methods))
    } yield {
      <TECHNIQUE name={technique.name}>
        { if (technique.parameters.nonEmpty) {
            <POLICYGENERATION>separated-with-parameters</POLICYGENERATION>
            <MULTIINSTANCE>true</MULTIINSTANCE>
          }
        }
        <DESCRIPTION>{technique.description}</DESCRIPTION>
        <USEMETHODREPORTING>true</USEMETHODREPORTING>
        {agentSpecificSection}
        <SECTIONS>
          {reportingSection}
          { if (technique.parameters.nonEmpty) {
              <SECTION name="Technique parameters">
                { technique.parameters.map(parameterSection) }
              </SECTION>
            }

          }
        </SECTIONS>
      </TECHNIQUE>
    }
  }

  // Write and commit all techniques files
  def writeAll(technique : Technique, methods: Map[BundleName, GenericMethod], modId : ModificationId, committer : EventActor) : IOResult[Seq[String]] = {
    for {
      agentFiles <- writeAgentFiles(technique, methods, modId, committer)
      metadata   <- writeMetadata(technique, methods, modId, committer)
      commit     <- archiver.commitTechnique(technique,metadata +: agentFiles, modId, committer, s"Committing technique ${technique.name}")
      libUpdate  <- techLibUpdate.update(modId, committer, Some(s"Update Technique library after creating files for ncf Technique ${technique.name}")).
        toIO.chainError(s"An error occured during technique update after files were created for ncf Technique ${technique.name}")
    } yield {
      metadata +: agentFiles
    }
  }

  def writeAgentFiles(technique : Technique, methods: Map[BundleName, GenericMethod], modId : ModificationId, commiter : EventActor) : IOResult[Seq[String]] = {
    for {
      // Create/update agent files, filter None by flattenning to list
      files  <- ZIO.foreach(agentSpecific)(_.writeAgentFiles(technique, methods)).map(_.flatten)
    } yield {
      files
    }
  }

  def writeMetadata(technique : Technique, methods: Map[BundleName, GenericMethod], modId : ModificationId, commiter : EventActor) : IOResult[String] = {

    val metadataPath = s"techniques/ncf_techniques/${technique.bundleName.value}/${technique.version.value}/metadata.xml"

    val path = s"${basePath}/${metadataPath}"
    for {
      content <- techniqueMetadataContent(technique, methods).map(n => xmlPrettyPrinter.format(n)).toIO
      _       <- IOResult.effect(s"An error occured while creating metadata file for Technique '${technique.name}'") {
                   implicit val charSet = StandardCharsets.UTF_8
                   val file = File (path).createFileIfNotExists (true)
                   file.write (content)
                 }
    } yield {
      metadataPath
    }
  }


}

trait AgentSpecificTechniqueWriter {

  def writeAgentFiles( technique : Technique, methods : Map[BundleName, GenericMethod] ) : IOResult[Seq[String]]

  def agentMetadata ( technique : Technique, methods : Map[BundleName, GenericMethod] ) : PureResult[NodeSeq]
}

class ClassicTechniqueWriter(basePath : String) extends AgentSpecificTechniqueWriter {

  // We need to add a reporting bundle for this method to generate a na report for any method with a condition != any/cfengine (which ~= true
  def methodNeedReporting(method : MethodCall) =  method.condition != "any" && method.condition != "cfengine-community"
  def needReportingBundle(technique : Technique) = technique.methodCalls.exists(methodNeedReporting)

  def canonifyCondition(methodCall: MethodCall) = {
    methodCall.condition.replaceAll("""(\$\{[^\}]*})""","""",canonify("$1"),"""")
  }

  def reportingContext(methodCall: MethodCall, classParameterValue: String ) = {
    // regex to match quote characters not preceded by a backslash
    val component  = methodCall.component.replaceAll("""(?<!\\)"""", """\\"""")
    val value = classParameterValue.replaceAll("""(?<!\\)"""", """\\"""")
    s"""_method_reporting_context("${component}", "${value}")"""
  }



  def writeAgentFiles( technique : Technique, methods : Map[BundleName, GenericMethod] )  : IOResult[Seq[String]] = {

    val bundleParams = if (technique.parameters.nonEmpty) technique.parameters.map(_.name.canonify).mkString("(",",",")") else ""

    val methodCalls =
      ( for {
        (method,index) <- technique.methodCalls.zipWithIndex
        method_info <- methods.get(method.methodId)
        classParameterValue <- method.parameters.get(method_info.classParameter)

      } yield {
        val condition = canonifyCondition(method)
        val promiser = s"${method.component.replaceAll("""(?<!\\)"""", """\\"""")}_${index}"
        // Check constraint and missing value
        val args = method_info.parameters.map(p => method.parameters(p.id).replaceAll("""(?<!\\)"""", """\\"""")).mkString("\"","\", \"","\"")

        s"""      "${promiser}" usebundle => ${reportingContext(method, classParameterValue)}
           |       ${promiser.map(_ => ' ')}         if => concat("${condition}");
           |      "${promiser}" usebundle =>  ${method.methodId.value}(${args})
           |       ${promiser.map(_ => ' ')}         if => concat("${condition}");""".stripMargin('|')

      }).mkString("\n")

    val content =
      s"""# @name ${technique.name}
         |# @description ${technique.description}
         |# @version ${technique.version.value}
         |${technique.parameters.map(p =>s"""# @parameter { "name": "${p.name.value}", "id": "${p.id.value}" }""" ).mkString("\n")}
         |
         |bundle agent ${technique.bundleName.value}${bundleParams}
         |{
         |  vars:
         |    "resources_dir" string => "$${this.promise_dir}/resources";
         |  methods:
         |${methodCalls}
         |}""".stripMargin('|')

    implicit val charset = StandardCharsets.UTF_8
    val techFile = File(basePath) / "techniques"/ "ncf_techniques" / technique.bundleName.value / technique.version.value / "technique.cf"
    val t = IOResult.effect(s"Could not write na reporting Technique file '${technique.name}' in path ${techFile.path.toString}") {
      techFile.createFileIfNotExists(true).write(content.stripMargin('|'))
      File(basePath).relativize(techFile.path).toString
    }



    val t2 = if ( ! needReportingBundle(technique)) {
      ZIO.succeed(Nil)
    } else {

      val bundleParams = if (technique.parameters.nonEmpty) technique.parameters.map(_.name.canonify).mkString("(",",",")") else ""
      val args = technique.parameters.map(p => s"$${${p.name.canonify}}").mkString(", ")

      val methodsReporting =
        ( for {
          (method,index) <- technique.methodCalls.zipWithIndex
          // Skip that method if name starts with _
          if ! method.methodId.value.startsWith("_")
          method_info <- methods.get(method.methodId)
          classParameterValue <- method.parameters.get(method_info.classParameter)

          classPrefix = s"$${class_prefix}_${method_info.classPrefix}_${classParameterValue}"
          escapedClassParameterValue = classParameterValue.replaceAll("""(?<!\\)"""", """\\"""")
          promiser = s"dummy_report_${index}"
        } yield {
          def naReport(condition : String, message : String) = {
            s"""      "${promiser}" usebundle => _classes_noop(canonify("${classPrefix}"))
               |       ${promiser.map(_ => ' ')}     unless => ${condition};
               |      "${promiser}" usebundle => ${reportingContext(method, classParameterValue)}
               |       ${promiser.map(_ => ' ')}     unless => ${condition};
               |      "${promiser}" usebundle => log_rudder("${message}", "${escapedClassParameterValue}", canonify("${classPrefix}"), canonify("${classPrefix}"), @{args})
               |       ${promiser.map(_ => ' ')}     unless => ${condition};""".stripMargin('|')
          }


          // Write report if the method does not support CFEngine ...
          ( if (! method_info.agentSupport.contains(AgentType.CfeCommunity)) {
            val message = s"""'${method_info.name}' method is not available on classic Rudder agent, skip"""
            val condition = "false"
            Some((condition,message))
         } else {
           // ... or if the condition needs rudder_reporting
           if (methodNeedReporting(method)) {
             val message =  s"""Skipping method '${method_info.name}' with key parameter '${escapedClassParameterValue}' since condition '${method.condition}' is not reached"""
             val condition = s"""concat("${canonifyCondition(method)}")""""
             Some((condition,message))
           } else {
             None
           }
         }).map((naReport _).tupled)
       }).flatten

       val content =
        s"""bundle agent ${technique.bundleName.value}_rudder_reporting${bundleParams}
           |{
           |  vars:
           |    "args"               slist => { ${args} };
           |    "report_param"      string => join("_", args);
           |    "full_class_prefix" string => canonify("${technique.bundleName.value}_rudder_reporting_$${report_param}");
           |    "class_prefix"      string => string_head("$${full_class_prefix}", "1000");
           |
           |  methods:
           |${methodsReporting.mkString("\n")}
           |}"""

      val reportingFile = File(basePath) / "techniques"/ "ncf_techniques" / technique.bundleName.value / technique.version.value / "rudder_reporting.cf"
      IOResult.effect(s"Could not write na reporting Technique file '${technique.name}' in path ${reportingFile.path.toString}") {
        reportingFile.createFileIfNotExists(true).write(content.stripMargin('|'))
        Seq(File(basePath).relativize(reportingFile.path).toString)
      }
    }

    for {
      tech <- t
      repo <- t2
    } yield {
      tech +: repo
    }
  }
  def agentMetadata ( technique : Technique, methods : Map[BundleName, GenericMethod] )  : PureResult[NodeSeq] = {
    // We need to add reporting bundle if there is a method call that does not supporval t cfengine (agent support does not contains both cfe agent)
    val noAgentSupportReporting = technique.methodCalls.exists(  m =>
                         methods.get(m.methodId).exists( gm =>
                           ! (gm.agentSupport.contains(AgentType.CfeEnterprise) || (gm.agentSupport.contains(AgentType.CfeCommunity)))
                         )
                       )
    val needReporting = needReportingBundle(technique)
    val xml = <AGENT type="cfengine-community,cfengine-nova">
      <BUNDLES>
        <NAME>{technique.bundleName.value}</NAME>
        {if (noAgentSupportReporting || needReporting) <NAME>{technique.bundleName.value}_rudder_reporting</NAME>}
      </BUNDLES>
      <FILES>
        <FILE name={s"RUDDER_CONFIGURATION_REPOSITORY/ncf/50_techniques/${technique.bundleName.value}/${technique.bundleName.value}.cf"}>
          <INCLUDED>true</INCLUDED>
        </FILE>
        { if (noAgentSupportReporting || needReporting)
          <FILE name={s"RUDDER_CONFIGURATION_REPOSITORY/techniques/ncf_techniques/${technique.bundleName.value}/${technique.version.value}/rudder_reporting.cf"}>
            <INCLUDED>true</INCLUDED>
          </FILE>
        }
        { for {
            resource <- technique.ressources
            if resource.state != ResourceFile.Deleted
          } yield {
            <FILE name={s"RUDDER_CONFIGURATION_REPOSITORY/techniques/ncf_techniques/${technique.bundleName.value}/${technique.version.value}/resources/${resource.path}"}>
              <INCLUDED>true</INCLUDED>
            </FILE>
          }
        }
      </FILES>
    </AGENT>
    Right(xml)
  }

}

class DSCTechniqueWriter(
    basePath   : String
  , translater : InterpolatedValueCompiler
) extends AgentSpecificTechniqueWriter{

  val genericParams =
    "-reportId $reportId -techniqueName $techniqueName -auditOnly:$auditOnly"

  def computeTechniqueFilePath(technique : Technique) =
    s"dsc/ncf/50_techniques/${technique.bundleName.value}/${technique.version.value}/${technique.bundleName.value}.ps1"


  def writeAgentFiles(technique : Technique, methods : Map[BundleName, GenericMethod] ): IOResult[Seq[String]] = {

    def toDscFormat(call : MethodCall) : PureResult[String]= {

      val componentName = s"""-componentName "${call.component.replaceAll("\"", "`\"")}""""

      def naReport(method : GenericMethod, expectedReportingValue : String) =
        s"""_rudder_common_report_na ${componentName} -componentKey "${expectedReportingValue}" -message "Not applicable" ${genericParams}"""
      for {

        // First translate parameters to Dsc values
        params    <- ((call.parameters.toList).traverse {
                        case (id, arg) =>
                          translater.translateToAgent(arg, AgentType.Dsc) match {
                            case Full(dscValue) => Right((id,dscValue))
                            case eb : EmptyBox =>
                              Left(IOError("",None))
                          }
                     }).map(_.toMap)

        // Translate condition
        condition <- translater.translateToAgent(call.condition, AgentType.Dsc) match {
                       case Full(c) => Right(c)
                       case eb : EmptyBox =>
                         Left(IOError("",None))
                     }

        methodParams =
          ( for {
            (id, arg) <- params
          } yield {
          s"""-${id.validDscName} "${arg.replaceAll("\"", "`\"")}""""
          }).mkString(" ")

        effectiveCall =
          s"""$$local_classes = Merge-ClassContext $$local_classes $$(${call.methodId.validDscName} ${methodParams} ${componentName} ${genericParams}).get_item("classes")"""

        // Check if method exists
        method <- methods.get(call.methodId) match {
                    case Some(method) =>
                      Right(method)
                    case None =>
                      Left(MethodNotFound(s"Method '${call.methodId.value}' not found when writing dsc Technique '${technique.name}' methods calls", None))
                  }
        // Check if class parameter is correctly defined
        classParameter <- params.get(method.classParameter).map(_.replaceAll("\"", "`\"")) match {
                            case Some(classParameter) =>
                              Right(classParameter)
                            case None =>
                              Left(MethodNotFound(s"Parameter '${method.classParameter.value}' for method '${method.id.value}' not found when writing dsc Technique '${technique.name}' methods calls",None))
                          }

      } yield {
       if (method.agentSupport.contains(AgentType.Dsc)) {
         if (condition == "any" ) {
           s"  ${effectiveCall}"
         } else{
           s"""|  $$class = "${condition}"
               |  if (Evaluate-Class $$class $$local_classes $$system_classes) {
               |    ${effectiveCall}
               |  } else {
               |    ${naReport(method,classParameter)}
               |  }""".stripMargin('|')
         }
       } else {
         s"  ${naReport(method,classParameter)}"
       }
      }
    }

    val filteredCalls = technique.methodCalls.filterNot(_.methodId.value.startsWith("_"))

    val parameters = technique.parameters match {
      case Nil => ""
      case params =>
        params.map( p =>
          s"""      [parameter(Mandatory=$$true)]
             |      [string]$$${p.name.validDscName},"""
        ).mkString("\n","\n","").stripMargin('|')
    }

    val techniquePath = computeTechniqueFilePath(technique)

    for {

      calls <- filteredCalls.toList.traverse(toDscFormat).toIO

      content =
        s"""|function ${technique.bundleName.validDscName} {
            |  [CmdletBinding()]
            |  param (
            |      [parameter(Mandatory=$$true)]
            |      [string]$$reportId,
            |      [parameter(Mandatory=$$true)]
            |      [string]$$techniqueName,${parameters}
            |      [switch]$$auditOnly
            |  )
            |
            |  $$local_classes = New-ClassContext
            |
            |${calls.mkString("\n\n")}
            |
            |}""".stripMargin('|')

      path  <-  IOResult.effect(s"Could not find dsc Technique '${technique.name}' in path ${basePath}/${techniquePath}") (
                  Paths.get(s"${basePath}/${techniquePath}")
                )
      // Powershell files needs to have a BOM added at the beginning of all files when using UTF8 enoding
      // See https://docs.microsoft.com/en-us/windows/desktop/intl/using-byte-order-marks
      contentWithBom : List[Byte] =
        // Bom, three bytes: EF BB BF https://en.wikipedia.org/wiki/Byte_order_mark
        239.toByte :: 187.toByte :: 191.toByte  ::
        content.getBytes(StandardCharsets.UTF_8).toList

      files <-  IOResult.effect(s"Could not write dsc Technique file '${technique.name}' in path ${basePath}/${techniquePath}") {
                  Files.createDirectories(path.getParent)
                  Files.write(path, contentWithBom.toArray)
                }
    } yield {
      techniquePath :: Nil
    }
  }

  def agentMetadata(technique : Technique, methods : Map[BundleName, GenericMethod] ) = {
    val xml = <AGENT type="dsc">
      <BUNDLES>
        <NAME>{technique.bundleName.validDscName}</NAME>
      </BUNDLES>
      <FILES>
        <FILE name={s"RUDDER_CONFIGURATION_REPOSITORY/${computeTechniqueFilePath(technique)}"}>
          <INCLUDED>true</INCLUDED>
        </FILE>
      </FILES>
    </AGENT>
    Right(xml)
  }

}

trait TechniqueArchiver {
  def commitTechnique(technique : Technique, filesToAdd : Seq[String], modId: ModificationId, commiter:  EventActor, msg : String) : IOResult[Unit]
}

class TechniqueArchiverImpl (
    override val gitRepo                   : GitRepositoryProvider
  , override val gitRootDirectory          : JFile
  , override val xmlPrettyPrinter          : RudderPrettyPrinter
  , override val relativePath              : String
  , override val gitModificationRepository : GitModificationRepository
  , personIdentservice : PersonIdentService
) extends GitArchiverUtils with TechniqueArchiver with Loggable {

  override def loggerName: String = this.getClass.getName

  override val encoding : String = "UTF-8"

  import collection.JavaConverters._
  def commitTechnique(technique : Technique, gitPath : Seq[String], modId: ModificationId, commiter:  EventActor, msg : String) = {

    val filesToAdd = gitPath ++ (technique.ressources.filter(f => f.state == ResourceFile.New || f.state == ResourceFile.Modified)).map(f => s"techniques/ncf_techniques/${technique.bundleName.value}/${technique.version.value}/resources/${f.path}")
    val filesToDelete = technique.ressources.filter(f => f.state == ResourceFile.Deleted ).map(f => s"techniques/ncf_techniques/${technique.bundleName.value}/${technique.version.value}/resources/${f.path}")
    (for {

      git <- gitRepo.git
      ident  <- personIdentservice.getPersonIdentOrDefault(commiter.name)
      _  <-  ZIO.foreach(filesToAdd) { f =>
               IOResult.effect (git.add.addFilepattern(f).call())
             }
      _  <-  ZIO.foreach(filesToDelete) { f =>
               IOResult.effect(git.rm.addFilepattern(f).call())
             }
      _ <- IOResult.effect(git.commit.setCommitter(ident).setMessage(msg).call())
    } yield {
      gitPath
    }).chainError(s"error when commiting file ${gitPath} for Technique '${technique.name}").unit
  }

}
