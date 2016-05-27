import scala.util.matching.Regex

/**
  * Created by thelese on 21/04/2016.
  */
class NQuadParser(var quad: String) {
  //regex strings
  val uriref = """<([^:]+:[^\s"<>]+)>"""
  val nodeId = """_:([A-Za-z0-9]*)"""
  val literal = """"([^"\\]*(?:\\.[^"\\]*)*)""""
  val litinfo = """(?:@([a-z]+(?:-[a-zA-Z0-9]+)*)|\^\^""" + uriref + """)?"""

  //regex objects
  val r_wspaces = new Regex("""[ \t]+""")
  val r_tail = new Regex("""[ \t]*\.[ \t]*(#.*)?""")
  val r_uriref = new Regex(uriref)
  val r_nodeId = new Regex(nodeId)
  val r_literal = new Regex(literal + litinfo)

  var quadSubj: String = ""
  var quadPred: String = ""
  var quadObj: String = ""
  var quadContext: String = ""

  // cut off first match of matching regex "pattern from str
  def eat(str: String, pattern: Regex): String = {
    val firstMatch = (pattern findFirstIn str).mkString(",")
    return str.substring(str.indexOf(firstMatch) + firstMatch.length)
  }

  // extract subject component
  def getSubject(): Unit = {
    if (quad.startsWith("<")) {
      //uriref
      quadSubj = (r_uriref findFirstIn quad).mkString(",")
      quad = eat(quad, r_uriref)
    } else if (quad.startsWith("_")) {
      //nodeid
      quadSubj = (r_nodeId findFirstIn quad).mkString(",")
      quad = eat(quad, r_nodeId)
    } else {
      throw new Exception("exception in getSubject()") //shouldn't happen
    }
  }

  // extract predicate component
  def getPredicate(): Unit = {
    if (!quad.startsWith("<")) {
      throw new Exception("exception in getPrediate() " + quad) //shouldn't happen
    }
    quadPred = (r_uriref findFirstIn quad).mkString(",")
    quad = eat(quad, r_uriref)
  }

  // extract object component
  def getObject(): Unit = {
    if (quad.startsWith("<")) { //uriref
      quadObj = (r_uriref findFirstIn quad).mkString(",")
      quad = eat(quad, r_uriref)
    } else if (quad.startsWith("_")) { //nodeid
      quadObj = (r_nodeId findFirstIn quad).mkString(",")
      quad = eat(quad, r_nodeId)
    } else if (quad.startsWith("""""")) { //literal
      quadObj = (r_literal findFirstIn quad).mkString(",")
      quad = eat(quad, r_literal)
    }
  }

  // extract context component
  def getContext(): Unit = {
    if (quad.startsWith("<")) {
      //uriref
      quadContext = (r_uriref findFirstIn quad).mkString(",")
      quad = eat(quad, r_uriref)
    } else if (quad.startsWith("_")) {
      //nodeid
      quadContext = (r_nodeId findFirstIn quad).mkString(",")
      quad = eat(quad, r_nodeId)
    } else {
      throw new Exception("exception in getContext()") //shouldn't happen
    }
  }

  // extract individual components of quad
  def parseQuad(): Unit = {
    getSubject()
    quad = eat(quad, r_wspaces)

    getPredicate()
    quad = eat(quad, r_wspaces)

    getObject()
    quad = eat(quad, r_wspaces)

    getContext()
  }

  parseQuad()
}
