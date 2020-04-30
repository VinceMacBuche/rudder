/*
*************************************************************************************
* Copyright 2014 Normation SAS
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

package com.normation.rudder.services.policies


import com.normation.box._
import com.normation.errors._
import com.normation.inventory.domain.AgentType
import com.normation.rudder.domain.parameters.ParameterName
import com.normation.rudder.domain.policies.PolicyModeOverrides
import com.normation.rudder.services.policies.PropertyParserTokens._
import net.liftweb.common._
import net.liftweb.json.JsonAST.JValue

/**
 * A parser that handle parameterized value of
 * directive variables.
 *
 * The parameterization is to be taken in the context of
 * a rule (i.e, a directive applied to
 * a target), and in the scope of one node of the target
 * (as if you were processing one node at a time).
 *
 * The general parameterized value are of the form:
 * ${rudder.xxx}
 * were "xxx" is the parameter to lookup.
 *
 * We handle 3 kinds of parameterizations:
 * 1/ ${rudder.param.XXX}
 *    where:
 *    - XXX is a parameter configured in Rudder
 *      (for now global, but support for node-contextualised is implemented)
 *    - XXX is case sensisite
 *    - XXX's value can contains other interpolation
 *
 * 2/ ${rudder.node.ACCESSOR}
 *    where:
 *    - "node" is a keyword ;
 *    - ACCESSOR is an accessor for that node, explained below.
 *    - the value can not contains other interpolation
 *
 * 3/ ${node.properties[keyone][keytwo]} or
 *   ${node.properties[keyone][keytwo] | node } or
 *    ${node.properties[keyone][keytwo] | default = XXXX }
 *
 *    where:
 *    - keyone, keytwo are path on json values
 *    - the return value is the string representation, in compact mode, of the resulting access
 *    - if the key is not found, we raise an error
 *    - spaces are authorized around separators ([,],|,}..)
 *    - options can be given by adding "| option", with available option:
 *      - "node" : mean that the interpolation will be done on the node,
 *        and so the parameter must be outputed as an equivalent string
 *        parameter for the node (the same without "| node")
 *      - default = XXX mean that if the properties is not found, "XXX" must
 *        be used in place, with XXX one of:
 *        - a double-quoted string, like: "default value",
 *        - a triple-double quoted string, like: """ some "default" must be used""",
 *        - an other parameter among the 3 described here.
 *        Quoted string may contain parameters, like in:
 *        ${node.properties[charge] | default = """Node "color" is: ${node.properties[color] | "blue" }""" }
 *
 * Accessor are keywords which allows to reach value in a context, exactly like
 * properties in object oriented programming.
 *
 * Accessors for parameters
 *    ${rudder.param.ACCESSOR} : replace by the value for the parameter with the name ACCESSOR
 *
 * Accessors for node
 * ------------------
 *   ${rudder.node.id} : internal ID of the node (generally an UUID)
 *   ${rudder.node.hostname} : hostname of the node
 *   ${rudder.node.admin} : login (or username) of the node administrator, or root, or at least
 *                   the login to use to run the agent
 *   ${rudder.node.policyserver.ACCESSOR} : information about the policyserver of the node.
 *                                    ACCESSORs are the same than for ${rudder.node}
 *
 *  We do have all the logistic to give access to any given inventory parameter, but for now,
 *  we still need to decide:
 *  - how we are managing it in a generic way, i.e given any addition to the inventory, having
 *    access to it without modifying that code (xpath like access to information from parameter
 *    structure)
 *  - what are the consequences in the reporting & expected reports, in particular what happen
 *    to a parameter whose value is a list (iteration, list => string, etc)
 */

trait InterpolatedValueCompiler {

  /**
   *
   * Parse a value looking for interpolation variable in it.
   *
   * Return a Box, where Full denotes a successful
   * parsing of all values, and EmptyBox. an error.
   */
  def compile(value: String): PureResult[InterpolationContext => PureResult[String]]
  def compileParam(value: String): PureResult[ParamInterpolationContext => PureResult[String]]

  /**
   *
   * Parse a value to translate token to a valid value for the agent passed as parameter.
   *
   * Return a Box, where Full denotes a successful
   * parsing of all values, and EmptyBox. an error.
   */
  def translateToAgent (value : String, agentType : AgentType) : Box[String]

}

object PropertyParserTokens {

  /*
   * Our AST for interpolated variable:
   * A string to look for interpolation is a list of token.
   * A token can be a plain string with no variable, or something
   * to interpolate. For now, we can interpolate two kind of variables:
   * - node information (thanks to a pointed path to the interesting property)
   * - rudder parameters (only globals for now)
   */
  sealed trait Token extends Any//could be Either[CharSeq, Interpolation]

  // a string that is not part of a interpolated value
  final case class CharSeq(s:String) extends AnyVal with Token {
    def prefix(p: String) = CharSeq(p + s)
  }

  // ${} but not for rudder
  final case class NonRudderVar(s: String) extends AnyVal with Token

  // an interpolation
  sealed trait     Interpolation     extends Any with Token

  //everything is expected to be lower case
  final case class NodeAccessor(path:List[String]) extends AnyVal with Interpolation
  //everything is expected to be lower case
  final case class Param(name:String)              extends AnyVal with Interpolation
  //here, we keep the case as it is given
  final case class Property(path: List[String], opt: Option[PropertyOption])    extends Interpolation

  //here, we have node property option
  sealed trait PropertyOption extends Any
  final case object InterpreteOnNode                 extends             PropertyOption
  final case class  DefaultValue(value: List[Token]) extends AnyVal with PropertyOption

  def containsVariable(tokens: List[Token]): Boolean = {
    tokens.exists(t => t.isInstanceOf[Interpolation] || t.isInstanceOf[NonRudderVar])
  }

}


trait AnalyseInterpolation[T, I <: GenericInterpolationContext[T]] {

  /*
   * Number of time we allows to recurse for interpolated variable
   * evaluated to other interpolated variables.
   *
   * It allows to detect cycle by brut force, but may raise false
   * positive too:
   *
   * Ex: two variable calling the other one:
   * a => b => a => b => a STOP
   *
   * Ex: a false positive:
   *
   * a => b => c => d => e => f => ... => "42"
   *
   * This is not a very big limitation, as we are not building
   * a programming language, and users can easily resolve
   * the problem by just making smaller cycle.
   *
   */
  val maxEvaluationDepth = 5

  /*
   * The funny part that for each token adds the interpretation of the token
   * by composing interpretation function.
   */
  def parseToken(tokens:List[Token]): I => PureResult[String] = {
    def build(context: I) = {
      val init: PureResult[String] = Right("")
      (tokens).foldLeft(init) {
        case (Right(str), token) => analyse(context, token).map(s => (str + s))
        case (Left(err) , _    ) => Left(err)
      }
    }

    build _
  }

  /*
   * The three following methods analyse token one by one and
   * given the token, build the function to execute to get
   * the final string (that may not succeed at run time, because of
   * unknown parameter, etc)
   */
  def analyse(context: I, token:Token): PureResult[String] = {
    token match {
      case CharSeq(s)         => Right(s)
      case NonRudderVar(s)    => Right(s)
      case NodeAccessor(path) => getNodeAccessorTarget(context, path)
      case Param(name)         => getRudderGlobalParam(context, ParameterName(name))
      case Property(path, opt) => opt match {
        case None =>
          getNodeProperty(context, path)
        case Some(InterpreteOnNode) =>
          //in that case, we want to exactly output the agent-compatible string. For now, easy, only one string
          Right(("${node.properties[" + path.mkString("][") + "]}"))
        case Some(DefaultValue(optionTokens)) =>
          //in that case, we want to find the default value.
          //we authorize to have default value = ${node.properties[bla][bla][bla]|node},
          //because we may want to use prop1 and if node set, prop2 at run time.
          for {
            default <- parseToken(optionTokens)(context)
            prop    <- getNodeProperty(context, path) match {
                         case Left(_)  => Right(default)
                         case Right(s) => Right(s)
                       }
          } yield {
            prop
          }
      }
    }
  }

  /**
   * Retrieve the global parameter from the node context.
   */
  def getRudderGlobalParam(context: I, paramName: ParameterName): PureResult[String]


  /**
   * Get the targeted accessed node information, checking that it exists.
   */
  def getNodeAccessorTarget(context: I, path: List[String]): PureResult[String] = {
    val error = Left(Unexpected(s"Unknow interpolated variable $${node.${path.mkString(".")}}" ))
    path match {
      case Nil => Left(Unexpected("In node interpolated variable, at least one accessor must be provided"))
      case access :: tail => access.toLowerCase :: tail match {
        case "id"         :: Nil => Right(context.nodeInfo.id.value)
        case "hostname"   :: Nil => Right(context.nodeInfo.hostname)
        case "admin"      :: Nil => Right(context.nodeInfo.localAdministratorAccountName)
        case "state"      :: Nil => Right(context.nodeInfo.state.name)
        case "policymodeLeft(" :: Nil =>
          val effectivePolicyMode = context.globalPolicyMode.overridable match {
            case PolicyModeOverrides.Unoverridable =>
              context.globalPolicyMode.mode.name
            case PolicyModeOverrides.Always =>
              context.nodeInfo.policyMode.getOrElse(context.globalPolicyMode.mode).name
          }
          Right(effectivePolicyMode)
        case "policyserver" :: tail2 => tail2 match {
          case "id"       :: Nil => Right(context.policyServerInfo.id.value)
          case "hostname" :: Nil => Right(context.policyServerInfo.hostname)
          case "admin"    :: Nil => Right(context.policyServerInfo.localAdministratorAccountName)
          case _ => error
        }
        case seq => error
      }
    }
  }

  /**
   * Get the node property value, or fails if it does not exists.
   * If the path length is 1, only check that the property exists and
   * returned the corresponding string value.
   * If the path length is more than one, try to parse the string has a
   * json value and access the remaining part as a json path.
   */
  def getNodeProperty(context: I, path: List[String]): PureResult[String] = {
    val errmsg = s"Missing property '$${node.properties[${path.mkString("][")}]}' on node '${context.nodeInfo.hostname}' [${context.nodeInfo.id.value}]"
    path match {
      //we should not reach that case since we enforce at leat one match of [...] in the parser
      case Nil       => Left(Unexpected(s"The syntax $${node.properties} is invalid, only $${node.properties[propertyname]} is accepted"))
      case h :: tail => context.nodeInfo.properties.find(p => p.name == h) match {
        case None       => Left(Unexpected(errmsg))
        case Some(prop) => tail match {
          case Nil     => Right(prop.renderValue)
          //here, we need to parse the value in json and try to find the asked path
          case subpath => getJsonProperty(subpath, prop.value).chainError(errmsg)
        }
      }
    }
  }

  def getJsonProperty(path: List[String], json: JValue): PureResult[String] = {
    import net.liftweb.json._

    @scala.annotation.tailrec
    def access(json: => JValue, path: List[String]): JValue = path match {
      case Nil       => json
      case h :: tail => access( json \ h, tail)
    }

    for {
      prop <- access(json, path) match {
                case JNothing   => Left(Unexpected(s"Can not find property in JSON '${compactRender(json)}'"))
                case JString(s) => Right(s) //needed to special case to not have '\"' everywhere
                case x          => Right(compactRender(x))
              }
    } yield {
      prop
    }
  }
}

object AnalyseParamInterpolation extends AnalyseInterpolation[ParamInterpolationContext => PureResult[String], ParamInterpolationContext] {

  /**
   * Retrieve the global parameter from the node context.
   */
  def getRudderGlobalParam(context: ParamInterpolationContext, paramName: ParameterName): PureResult[String] = {
    context.parameters.get(paramName.value) match {
      case Some(value) =>
        if(context.depth >= maxEvaluationDepth) {
          Left(Unexpected(s"""Can not evaluted global parameter "${paramName.value}" because it uses an interpolation variable that depends upon """
           + s"""other interpolated variables in a stack more than ${maxEvaluationDepth} in depth. We fear it's a circular dependancy."""))
        } else value(context.copy(depth = context.depth+1))
      case _ => Left(Unexpected(s"Error when trying to interpolate a variable: Rudder parameter not found: '${paramName.value}'"))
    }
  }
}

object AnalyseNodeInterpolation extends AnalyseInterpolation[String, InterpolationContext] {

  /**
   * Retrieve the global parameter from the node context.
   */
  def getRudderGlobalParam(context: InterpolationContext, paramName: ParameterName): PureResult[String] = {
    context.parameters.get(paramName.value) match {
      case Some(value) =>
        Right(value)
      case _           =>
        Left(Unexpected(s"Error when trying to interpolate a variable: Rudder parameter not found: '${paramName.value}'"))
    }
  }
}

class InterpolatedValueCompilerImpl extends InterpolatedValueCompiler {

  /*
   * just call the parser on a value, and in case of successful parsing, interprete
   * the resulting AST (seq of token)
   */
  override def compile(value: String): PureResult[InterpolationContext => PureResult[String]] = {
    PropertyParser.parse(value).map(AnalyseNodeInterpolation.parseToken)
  }

  override def compileParam(value: String): PureResult[ParamInterpolationContext => PureResult[String]] = {
    PropertyParser.parse(value).map(AnalyseParamInterpolation.parseToken)
  }

  def translateToAgent(value: String, agent : AgentType): Box[String] = {
    PropertyParser.parse(value).map(_.map(translate(agent, _) ).mkString("")).toBox
  }

  // Transform a token to its correct value for the agent passed as parameter
  def translate(agent: AgentType, token:Token): String = {
    token match {
      case CharSeq(s)         => s
      case NonRudderVar(s)    => s
      case NodeAccessor(path) => s"$${rudder.node.${path.mkString(".")}}"
      case Param(name)         => s"$${rudder.param.${name}}"
      case Property(path, opt) => agent match {
        case AgentType.Dsc =>
          s"$$($$node.properties[${path.mkString("][")}])"
        case AgentType.CfeCommunity | AgentType.CfeEnterprise =>
          s"$${node.properties[${path.mkString("][")}]}"
      }
    }
  }

}

object PropertyParser {

  import fastparse._, NoWhitespace._

  def parse(value: String): PureResult[List[Token]] = {
    fastparse.parse(value, all(_)) match {
      case Parsed.Success(value, index)    => Right(value)
      case Parsed.Failure(label, i, extra) => Left(Unexpected(s"""Error when parsing value "${value}", error message is: ${label}"""))
    }
  }

  def all[_: P] : P[List[Token]] = P( Start ~ ( (plainStringNoDollar | varOrString).rep(1) | emptyVar) ~ End).map(_.toList)

  //empty string is a special case that must be look appart from plain string.
  //also parse full blank string, because no need to look for more case for them.
  def emptyVar[_: P]: P[List[CharSeq]] = P(CharsWhile(_.isSpaceChar, 0).!).map(x => List(CharSeq(x)) )
  def space[_:P] = P(CharsWhile(_.isWhitespace, 0))
  // plain string must not match our identifier, ${rudder.* and ${node.properties.*}
  // here we defined a function to build them
  def plainStringNoDollar[_: P] : P[CharSeq] = P( (!"${" ~ AnyChar).rep(1).! ).map { CharSeq(_) }
  def plainStringNoEndCurly[_: P] : P[CharSeq] = P( (!"}" ~ AnyChar).rep(1).! ).map { CharSeq(_) }

  /*
   * This bit is a bit complicated, because we need to differentiate:
   * - a variable ${.... }
   *   - and here, ${rudder.node.plop} should be an error,
   *   - but ${foobar} should an "other prop"
   * - a string ${ ....
   * - a string $ .... }
   */
  def varOrString[_: P] = P( "${" ~/ (
                                     space ~ varname ~ space ~ "}"
                                     | plainStringNoEndCurly.map(_.prefix("${") )
                                     )
                          )

  def varname[_: P] = P( interpol | otherProp )

  //identifier for step in the path or param names
  def propId[_: P]      : P[String] = P(CharIn("""\-_a-zA-Z0-9""").rep(1).!)

  //an interpolated variable looks like: ${rudder.XXX}, or ${RuDder.xXx}
  // after "${rudder." there is no backtracking to an "otherProp" or string possible.
  def rudderProp[_: P]  : P[Interpolation] = P(IgnoreCase("rudder") ~ space ~ "." ~ space ~/ (nodeAccess | parameter) )

  //a node path looks like: ${rudder.node.HERE.PATH}
  def nodeAccess[_: P]  : P[Interpolation] = P(IgnoreCase("node") ~/ space ~ "." ~ space ~/ propId.rep(sep = space ~ "." ~ space) ).map { seq => NodeAccessor(seq.toList) }

  //a parameter looks like: ${rudder.PARAM_NAME}
  def parameter[_: P]  : P[Interpolation] = P(IgnoreCase("param") ~/ space ~ "." ~ space ~/ propId).map{ p => Param(p) }

  //a node property looks like: ${node.properties[.... Cut after "properties".
  def nodeProp[_: P]    : P[Interpolation] = P(IgnoreCase("node") ~ space ~ "." ~ space ~ IgnoreCase("properties") ~/ (space ~ "[" ~ space ~ propId ~ space ~ "]" ).rep(1) ~/
                                               propOption.? ).map { case (path, opt) => Property(path.toList, opt) }

  // other cases of ${}: cfengine variables, etc
  def otherProp[_: P]: P[NonRudderVar] = P( CharsWhile(c => c != '}').!).map { s => NonRudderVar("${"+s+"}") }

  // all interpolation
  def interpol [_: P]   : P[Interpolation] = P( rudderProp | nodeProp )

  //here, the number of " must be strictly decreasing - ie. triple quote before
  def propOption[_: P]  : P[PropertyOption] = P(space ~ "|" ~/ space ~ (onNodeOpt | defaultOpt) )

  def defaultOpt[_: P]  : P[DefaultValue] = P(IgnoreCase("default") ~/ space ~ "=" ~/ space ~/ (interpolOpt | emptyTQuote | tqString | emptySQuote | sqString) )
  def onNodeOpt[_: P]   : P[InterpreteOnNode.type] = P( IgnoreCase("node")).map(_ => InterpreteOnNode)

  def variable[_: P] = P("${" ~ space ~/ varname ~/ space ~ "}")
  def interpolOpt[_: P] : P[DefaultValue] = variable.map { x => DefaultValue(x::Nil) }
  def emptySQuote[_: P] : P[DefaultValue] = P( "\"\"" ).map { _ => DefaultValue(CharSeq("")::Nil) }
  def emptyTQuote[_: P] : P[DefaultValue] = P("\"\"\"\"\"\"").map { _ => DefaultValue(CharSeq("")::Nil) }

  //string must be simple or triple quoted string
  // single quoted strings or variable
  def sqString[_: P]    : P[DefaultValue] = P( "\"" ~ (sqplainStr | variable ).rep(1) ~ "\"").map { case x => DefaultValue(x.toList) }
  // single quoted strings part
  def sqplainStr[_: P]  : P[Token]        = P( CharsWhile(c => c != '"').! ).map { str => CharSeq(str) }

  // triple quoted strings or variable
  def tqString[_: P]    : P[DefaultValue]   = P( "\"\"\"" ~ (tqplainStr | variable).rep(1) ~ "\"\"\"" ).map { case x => DefaultValue(x.toList) }
  // triple quoted strings part
  def tqplainStr[_: P]  : P[Token]          = P( (!"\"\"\"" ~ AnyChar).rep(1).! ).map { str => CharSeq(str) }
}
