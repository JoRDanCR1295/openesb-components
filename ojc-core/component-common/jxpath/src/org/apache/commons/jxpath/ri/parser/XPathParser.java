/*
 * BEGIN_HEADER - DO NOT EDIT
 * 
 * The contents of this file are subject to the terms
 * of the Common Development and Distribution License
 * (the "License").  You may not use this file except
 * in compliance with the License.
 *
 * You can obtain a copy of the license at
 * https://open-jbi-components.dev.java.net/public/CDDLv1.0.html.
 * See the License for the specific language governing
 * permissions and limitations under the License.
 *
 * When distributing Covered Code, include this CDDL
 * HEADER in each file and include the License file at
 * https://open-jbi-components.dev.java.net/public/CDDLv1.0.html.
 * If applicable add the following below this CDDL HEADER,
 * with the fields enclosed by brackets "[]" replaced with
 * your own identifying information: Portions Copyright
 * [year] [name of copyright owner]
 */

/*
 * @(#)XPathParser.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

    package org.apache.commons.jxpath.ri.parser;

    import org.apache.commons.jxpath.ri.Compiler;
    import java.util.ArrayList;

    public class XPathParser implements XPathParserConstants {
        private Compiler compiler;

        public void setCompiler(Compiler compiler){
            this.compiler = compiler;
        }

        private String unescape(String string){
            int index = string.indexOf("&apos;");
            while (index != -1){
                string = string.substring(0, index) + "\'" + string.substring(index + 6);
                index = string.indexOf("&apos;");
            }
            index = string.indexOf("&quot;");
            while (index != -1){
                string = string.substring(0, index) + "\"" + string.substring(index + 6);
                index = string.indexOf("&quot;");
            }
            return string;
        }

// Note: XPath does not have reserved words, so we have to include all these terminals
  final public String NCName() throws ParseException {
    switch (jj_nt.kind) {
    case OR:
    case AND:
    case MOD:
    case DIV:
    case NCName:
      NCName_Without_CoreFunctions();
      break;
    case NODE:
      jj_consume_token(NODE);
      break;
    case TEXT:
      jj_consume_token(TEXT);
      break;
    case COMMENT:
      jj_consume_token(COMMENT);
      break;
    case PI:
      jj_consume_token(PI);
      break;
    case FUNCTION_LAST:
      jj_consume_token(FUNCTION_LAST);
      break;
    case FUNCTION_POSITION:
      jj_consume_token(FUNCTION_POSITION);
      break;
    case FUNCTION_COUNT:
      jj_consume_token(FUNCTION_COUNT);
      break;
    case FUNCTION_ID:
      jj_consume_token(FUNCTION_ID);
      break;
    case FUNCTION_LOCAL_NAME:
      jj_consume_token(FUNCTION_LOCAL_NAME);
      break;
    case FUNCTION_NAMESPACE_URI:
      jj_consume_token(FUNCTION_NAMESPACE_URI);
      break;
    case FUNCTION_NAME:
      jj_consume_token(FUNCTION_NAME);
      break;
    case FUNCTION_STRING:
      jj_consume_token(FUNCTION_STRING);
      break;
    case FUNCTION_CONCAT:
      jj_consume_token(FUNCTION_CONCAT);
      break;
    case FUNCTION_STARTS_WITH:
      jj_consume_token(FUNCTION_STARTS_WITH);
      break;
    case FUNCTION_CONTAINS:
      jj_consume_token(FUNCTION_CONTAINS);
      break;
    case FUNCTION_SUBSTRING_BEFORE:
      jj_consume_token(FUNCTION_SUBSTRING_BEFORE);
      break;
    case FUNCTION_SUBSTRING_AFTER:
      jj_consume_token(FUNCTION_SUBSTRING_AFTER);
      break;
    case FUNCTION_SUBSTRING:
      jj_consume_token(FUNCTION_SUBSTRING);
      break;
    case FUNCTION_STRING_LENGTH:
      jj_consume_token(FUNCTION_STRING_LENGTH);
      break;
    case FUNCTION_NORMALIZE_SPACE:
      jj_consume_token(FUNCTION_NORMALIZE_SPACE);
      break;
    case FUNCTION_TRANSLATE:
      jj_consume_token(FUNCTION_TRANSLATE);
      break;
    case FUNCTION_BOOLEAN:
      jj_consume_token(FUNCTION_BOOLEAN);
      break;
    case FUNCTION_NOT:
      jj_consume_token(FUNCTION_NOT);
      break;
    case FUNCTION_TRUE:
      jj_consume_token(FUNCTION_TRUE);
      break;
    case FUNCTION_FALSE:
      jj_consume_token(FUNCTION_FALSE);
      break;
    case FUNCTION_NULL:
      jj_consume_token(FUNCTION_NULL);
      break;
    case FUNCTION_LANG:
      jj_consume_token(FUNCTION_LANG);
      break;
    case FUNCTION_NUMBER:
      jj_consume_token(FUNCTION_NUMBER);
      break;
    case FUNCTION_SUM:
      jj_consume_token(FUNCTION_SUM);
      break;
    case FUNCTION_FLOOR:
      jj_consume_token(FUNCTION_FLOOR);
      break;
    case FUNCTION_CEILING:
      jj_consume_token(FUNCTION_CEILING);
      break;
    case FUNCTION_ROUND:
      jj_consume_token(FUNCTION_ROUND);
      break;
    case FUNCTION_KEY:
      jj_consume_token(FUNCTION_KEY);
      break;
    case FUNCTION_FORMAT_NUMBER:
      jj_consume_token(FUNCTION_FORMAT_NUMBER);
      break;
    default:
      jj_la1[0] = jj_gen;
      jj_consume_token(-1);
      throw new ParseException();
    }
        {if (true) return token.image;}
    throw new Error("Missing return statement in function");
  }

  final public String NCName_Without_CoreFunctions() throws ParseException {
    switch (jj_nt.kind) {
    case NCName:
      jj_consume_token(NCName);
      break;
    case OR:
      jj_consume_token(OR);
      break;
    case AND:
      jj_consume_token(AND);
      break;
    case MOD:
      jj_consume_token(MOD);
      break;
    case DIV:
      jj_consume_token(DIV);
      break;
    default:
      jj_la1[1] = jj_gen;
      jj_consume_token(-1);
      throw new ParseException();
    }
        {if (true) return token.image;}
    throw new Error("Missing return statement in function");
  }

  final public int CoreFunctionName() throws ParseException {
    int code;
    switch (jj_nt.kind) {
    case FUNCTION_LAST:
      jj_consume_token(FUNCTION_LAST);
                                      code = Compiler.FUNCTION_LAST;
      break;
    case FUNCTION_POSITION:
      jj_consume_token(FUNCTION_POSITION);
                                      code = Compiler.FUNCTION_POSITION;
      break;
    case FUNCTION_COUNT:
      jj_consume_token(FUNCTION_COUNT);
                                      code = Compiler.FUNCTION_COUNT;
      break;
    case FUNCTION_ID:
      jj_consume_token(FUNCTION_ID);
                                      code = Compiler.FUNCTION_ID;
      break;
    case FUNCTION_LOCAL_NAME:
      jj_consume_token(FUNCTION_LOCAL_NAME);
                                      code = Compiler.FUNCTION_LOCAL_NAME;
      break;
    case FUNCTION_NAMESPACE_URI:
      jj_consume_token(FUNCTION_NAMESPACE_URI);
                                      code = Compiler.FUNCTION_NAMESPACE_URI;
      break;
    case FUNCTION_NAME:
      jj_consume_token(FUNCTION_NAME);
                                      code = Compiler.FUNCTION_NAME;
      break;
    case FUNCTION_STRING:
      jj_consume_token(FUNCTION_STRING);
                                      code = Compiler.FUNCTION_STRING;
      break;
    case FUNCTION_CONCAT:
      jj_consume_token(FUNCTION_CONCAT);
                                      code = Compiler.FUNCTION_CONCAT;
      break;
    case FUNCTION_STARTS_WITH:
      jj_consume_token(FUNCTION_STARTS_WITH);
                                      code = Compiler.FUNCTION_STARTS_WITH;
      break;
    case FUNCTION_CONTAINS:
      jj_consume_token(FUNCTION_CONTAINS);
                                      code = Compiler.FUNCTION_CONTAINS;
      break;
    case FUNCTION_SUBSTRING_BEFORE:
      jj_consume_token(FUNCTION_SUBSTRING_BEFORE);
                                      code = Compiler.FUNCTION_SUBSTRING_BEFORE;
      break;
    case FUNCTION_SUBSTRING_AFTER:
      jj_consume_token(FUNCTION_SUBSTRING_AFTER);
                                      code = Compiler.FUNCTION_SUBSTRING_AFTER;
      break;
    case FUNCTION_SUBSTRING:
      jj_consume_token(FUNCTION_SUBSTRING);
                                      code = Compiler.FUNCTION_SUBSTRING;
      break;
    case FUNCTION_STRING_LENGTH:
      jj_consume_token(FUNCTION_STRING_LENGTH);
                                      code = Compiler.FUNCTION_STRING_LENGTH;
      break;
    case FUNCTION_NORMALIZE_SPACE:
      jj_consume_token(FUNCTION_NORMALIZE_SPACE);
                                      code = Compiler.FUNCTION_NORMALIZE_SPACE;
      break;
    case FUNCTION_TRANSLATE:
      jj_consume_token(FUNCTION_TRANSLATE);
                                      code = Compiler.FUNCTION_TRANSLATE;
      break;
    case FUNCTION_BOOLEAN:
      jj_consume_token(FUNCTION_BOOLEAN);
                                      code = Compiler.FUNCTION_BOOLEAN;
      break;
    case FUNCTION_NOT:
      jj_consume_token(FUNCTION_NOT);
                                      code = Compiler.FUNCTION_NOT;
      break;
    case FUNCTION_TRUE:
      jj_consume_token(FUNCTION_TRUE);
                                      code = Compiler.FUNCTION_TRUE;
      break;
    case FUNCTION_FALSE:
      jj_consume_token(FUNCTION_FALSE);
                                      code = Compiler.FUNCTION_FALSE;
      break;
    case FUNCTION_NULL:
      jj_consume_token(FUNCTION_NULL);
                                      code = Compiler.FUNCTION_NULL;
      break;
    case FUNCTION_LANG:
      jj_consume_token(FUNCTION_LANG);
                                      code = Compiler.FUNCTION_LANG;
      break;
    case FUNCTION_NUMBER:
      jj_consume_token(FUNCTION_NUMBER);
                                      code = Compiler.FUNCTION_NUMBER;
      break;
    case FUNCTION_SUM:
      jj_consume_token(FUNCTION_SUM);
                                      code = Compiler.FUNCTION_SUM;
      break;
    case FUNCTION_FLOOR:
      jj_consume_token(FUNCTION_FLOOR);
                                      code = Compiler.FUNCTION_FLOOR;
      break;
    case FUNCTION_CEILING:
      jj_consume_token(FUNCTION_CEILING);
                                      code = Compiler.FUNCTION_CEILING;
      break;
    case FUNCTION_ROUND:
      jj_consume_token(FUNCTION_ROUND);
                                      code = Compiler.FUNCTION_ROUND;
      break;
    case FUNCTION_KEY:
      jj_consume_token(FUNCTION_KEY);
                                      code = Compiler.FUNCTION_KEY;
      break;
    case FUNCTION_FORMAT_NUMBER:
      jj_consume_token(FUNCTION_FORMAT_NUMBER);
                                      code = Compiler.FUNCTION_FORMAT_NUMBER;
      break;
    default:
      jj_la1[2] = jj_gen;
      jj_consume_token(-1);
      throw new ParseException();
    }
        {if (true) return code;}
    throw new Error("Missing return statement in function");
  }

  final public Object QName() throws ParseException {
 String nc1, nc2 = null;
    nc1 = NCName();
    switch (jj_nt.kind) {
    case 79:
      jj_consume_token(79);
      nc2 = NCName();
      break;
    default:
      jj_la1[3] = jj_gen;
      ;
    }
        if (nc2 == null){
            {if (true) return compiler.qname(null, nc1);}
        }
        else {
            {if (true) return compiler.qname(nc1, nc2);}
        }
    throw new Error("Missing return statement in function");
  }

  final public Object QName_Without_CoreFunctions() throws ParseException {
    String nc1, nc2 = null;
    if (jj_2_1(2147483647)) {
      nc1 = NCName();
      jj_consume_token(79);
      nc2 = NCName();
    } else {
      switch (jj_nt.kind) {
      case OR:
      case AND:
      case MOD:
      case DIV:
      case NCName:
        nc1 = NCName_Without_CoreFunctions();
        break;
      default:
        jj_la1[4] = jj_gen;
        jj_consume_token(-1);
        throw new ParseException();
      }
    }
        if (nc2 == null){
            {if (true) return compiler.qname(null, nc1);}
        }
        else {
            {if (true) return compiler.qname(nc1, nc2);}
        }
    throw new Error("Missing return statement in function");
  }

  final public Object parseExpression() throws ParseException {
    Object ex;
    ex = Expression();
    jj_consume_token(0);
        {if (true) return ex;}
    throw new Error("Missing return statement in function");
  }

/* ################################################################################### */
/* XSLT Patterns (http://www.w3.org/1999/08/WD-xslt-19990813)                          */
/* ################################################################################### */

/* [XSLT1] Pattern ::= LocationPathPattern | Pattern '|' LocationPathPattern  */

//void Pattern() :
//{}
//{
//        LocationPathPattern() ( <UNION> LocationPathPattern() )* <EOF>
//}
//
//
///* [XSLT2] LocationPathPattern ::=
//   '/' RelativePathPattern? | IdKeyPattern (('/' | '//' RelativePathPattern)? | '//'? RelativePathPattern
//*/
//
//void LocationPathPattern() :
//{}
//{
//        <SLASH> ( RelativePathPattern() )?
//    |    (
//        LOOKAHEAD(IdKeyPattern())
//            IdKeyPattern() ( ( <SLASH> | <SLASHSLASH>) RelativePathPattern() )?
//        |    ( <SLASHSLASH> )? RelativePathPattern()
//        )
//}
//
//
//
///* [XSLT3] IdKeyPattern    ::=    'id' '(' Literal ')' | 'key' '(' Literal ',' Literal ')'  */
//
//void IdKeyPattern() :
//{}
//{
//        <ID> "(" <Literal> ")"
//    |    <KEY>  "(" <Literal> "," <Literal> ")"
//}
//
//
///* [XSLT4] RelativePathPattern    ::=    StepPattern | RelativePathPattern '/' StepPattern
//                           | RelativePathPattern '//' StepPattern
//*/
//void RelativePathPattern() :
//{}
//{
//        StepPattern() ( ( <SLASH>| <SLASHSLASH> ) StepPattern()    )*
//}
//
//
///* [XSLT5]    StepPattern    ::=    AbbreviatedAxisSpecifier NodeTest Predicate*   */
//void StepPattern() :
//{}
//{
//        AbbreviatedAxisSpecifier() NodeTest() (Predicate())*
//}



// See XPath Syntax (http://www.w3.org/TR/xpath )


//void XPath() :
//{}
//{
//    LocationPath()
//    <EOF>
//}


/* [1] LocationPath ::= RelativeLocationPath | AbsoluteLocationPath  */
  final public Object LocationPath() throws ParseException {
 Object ex = null;
    switch (jj_nt.kind) {
    case OR:
    case AND:
    case MOD:
    case DIV:
    case NODE:
    case TEXT:
    case COMMENT:
    case PI:
    case AXIS_SELF:
    case AXIS_CHILD:
    case AXIS_PARENT:
    case AXIS_ANCESTOR:
    case AXIS_ATTRIBUTE:
    case AXIS_NAMESPACE:
    case AXIS_PRECEDING:
    case AXIS_FOLLOWING:
    case AXIS_DESCENDANT:
    case AXIS_ANCESTOR_OR_SELF:
    case AXIS_FOLLOWING_SIBLING:
    case AXIS_PRECEDING_SIBLING:
    case AXIS_DESCENDANT_OR_SELF:
    case FUNCTION_LAST:
    case FUNCTION_POSITION:
    case FUNCTION_COUNT:
    case FUNCTION_ID:
    case FUNCTION_KEY:
    case FUNCTION_LOCAL_NAME:
    case FUNCTION_NAMESPACE_URI:
    case FUNCTION_NAME:
    case FUNCTION_STRING:
    case FUNCTION_CONCAT:
    case FUNCTION_STARTS_WITH:
    case FUNCTION_CONTAINS:
    case FUNCTION_SUBSTRING_BEFORE:
    case FUNCTION_SUBSTRING_AFTER:
    case FUNCTION_SUBSTRING:
    case FUNCTION_STRING_LENGTH:
    case FUNCTION_NORMALIZE_SPACE:
    case FUNCTION_TRANSLATE:
    case FUNCTION_BOOLEAN:
    case FUNCTION_NOT:
    case FUNCTION_TRUE:
    case FUNCTION_FALSE:
    case FUNCTION_NULL:
    case FUNCTION_LANG:
    case FUNCTION_NUMBER:
    case FUNCTION_SUM:
    case FUNCTION_FLOOR:
    case FUNCTION_CEILING:
    case FUNCTION_ROUND:
    case FUNCTION_FORMAT_NUMBER:
    case NCName:
    case 82:
    case 83:
    case 86:
    case 88:
      ex = RelativeLocationPath();
      break;
    case SLASH:
    case SLASHSLASH:
      ex = AbsoluteLocationPath();
      break;
    default:
      jj_la1[5] = jj_gen;
      jj_consume_token(-1);
      throw new ParseException();
    }
        {if (true) return ex;}
    throw new Error("Missing return statement in function");
  }

/* [2] AbsoluteLocationPath ::= '/' RelativeLocationPath? | AbbreviatedAbsoluteLocationPath  */
/* [10]    AbbreviatedAbsoluteLocationPath    ::=    '//' RelativeLocationPath  */
  final public Object AbsoluteLocationPath() throws ParseException {
    ArrayList steps = new ArrayList();
    if (jj_2_2(2147483647)) {
      LocationStep(steps);
      label_1:
      while (true) {
        switch (jj_nt.kind) {
        case SLASH:
        case SLASHSLASH:
          ;
          break;
        default:
          jj_la1[6] = jj_gen;
          break label_1;
        }
        LocationStep(steps);
      }
    } else {
      switch (jj_nt.kind) {
      case SLASH:
        jj_consume_token(SLASH);
        break;
      default:
        jj_la1[7] = jj_gen;
        jj_consume_token(-1);
        throw new ParseException();
      }
    }
        {if (true) return compiler.locationPath(true, steps.toArray());}
    throw new Error("Missing return statement in function");
  }

/* [3] RelativeLocationPath ::= Step | RelativeLocationPath '/' Step | AbbreviatedRelativeLocationPath */
  final public Object RelativeLocationPath() throws ParseException {
    ArrayList steps = new ArrayList();
    NodeTest(steps);
    label_2:
    while (true) {
      switch (jj_nt.kind) {
      case SLASH:
      case SLASHSLASH:
        ;
        break;
      default:
        jj_la1[8] = jj_gen;
        break label_2;
      }
      LocationStep(steps);
    }
        {if (true) return compiler.locationPath(false, steps.toArray());}
    throw new Error("Missing return statement in function");
  }

/* [3] RelativeLocationPath ::= Step | RelativeLocationPath '/' Step | AbbreviatedRelativeLocationPath */
/* [11]    AbbreviatedRelativeLocationPath    ::=    RelativeLocationPath '//' Step  */


/*--------------------*/
/* 2.1 Location Steps */
/*--------------------*/

/* [4] Step ::= AxisSpecifier NodeTest Predicate*   | AbbreviatedStep  */
  final public void LocationStep(ArrayList steps) throws ParseException {
    Object t;
    Object s;
    switch (jj_nt.kind) {
    case SLASH:
      jj_consume_token(SLASH);
      break;
    case SLASHSLASH:
      jj_consume_token(SLASHSLASH);
            // Abbreviated step: descendant-or-self::node()
            t = compiler.nodeTypeTest(Compiler.NODE_TYPE_NODE);
            steps.add(compiler.step(Compiler.AXIS_DESCENDANT_OR_SELF, t, null));
      break;
    default:
      jj_la1[9] = jj_gen;
      jj_consume_token(-1);
      throw new ParseException();
    }
    NodeTest(steps);
  }

/* [7] NodeTest ::= WildcardName | NodeType '(' ')' | 'processing-instruction' '(' Literal ')' */
  final public void NodeTest(ArrayList steps) throws ParseException {
    int axis;
    int type = -1;
    String instruction = null;
    Object name = null;
    Object s;
    Object p;
    ArrayList ps = new ArrayList();
    switch (jj_nt.kind) {
    case OR:
    case AND:
    case MOD:
    case DIV:
    case NODE:
    case TEXT:
    case COMMENT:
    case PI:
    case AXIS_SELF:
    case AXIS_CHILD:
    case AXIS_PARENT:
    case AXIS_ANCESTOR:
    case AXIS_ATTRIBUTE:
    case AXIS_NAMESPACE:
    case AXIS_PRECEDING:
    case AXIS_FOLLOWING:
    case AXIS_DESCENDANT:
    case AXIS_ANCESTOR_OR_SELF:
    case AXIS_FOLLOWING_SIBLING:
    case AXIS_PRECEDING_SIBLING:
    case AXIS_DESCENDANT_OR_SELF:
    case FUNCTION_LAST:
    case FUNCTION_POSITION:
    case FUNCTION_COUNT:
    case FUNCTION_ID:
    case FUNCTION_KEY:
    case FUNCTION_LOCAL_NAME:
    case FUNCTION_NAMESPACE_URI:
    case FUNCTION_NAME:
    case FUNCTION_STRING:
    case FUNCTION_CONCAT:
    case FUNCTION_STARTS_WITH:
    case FUNCTION_CONTAINS:
    case FUNCTION_SUBSTRING_BEFORE:
    case FUNCTION_SUBSTRING_AFTER:
    case FUNCTION_SUBSTRING:
    case FUNCTION_STRING_LENGTH:
    case FUNCTION_NORMALIZE_SPACE:
    case FUNCTION_TRANSLATE:
    case FUNCTION_BOOLEAN:
    case FUNCTION_NOT:
    case FUNCTION_TRUE:
    case FUNCTION_FALSE:
    case FUNCTION_NULL:
    case FUNCTION_LANG:
    case FUNCTION_NUMBER:
    case FUNCTION_SUM:
    case FUNCTION_FLOOR:
    case FUNCTION_CEILING:
    case FUNCTION_ROUND:
    case FUNCTION_FORMAT_NUMBER:
    case NCName:
    case 86:
    case 88:
      axis = AxisSpecifier();
      if (jj_2_3(2147483647)) {
        type = NodeType();
        jj_consume_token(80);
        jj_consume_token(81);
      } else if (jj_2_4(2147483647)) {
        jj_consume_token(PI);
        jj_consume_token(80);
        jj_consume_token(Literal);
                    instruction = unescape(token.image.substring(1, token.image.length() - 1));
        jj_consume_token(81);
      } else {
        switch (jj_nt.kind) {
        case OR:
        case AND:
        case MOD:
        case DIV:
        case NODE:
        case TEXT:
        case COMMENT:
        case PI:
        case FUNCTION_LAST:
        case FUNCTION_POSITION:
        case FUNCTION_COUNT:
        case FUNCTION_ID:
        case FUNCTION_KEY:
        case FUNCTION_LOCAL_NAME:
        case FUNCTION_NAMESPACE_URI:
        case FUNCTION_NAME:
        case FUNCTION_STRING:
        case FUNCTION_CONCAT:
        case FUNCTION_STARTS_WITH:
        case FUNCTION_CONTAINS:
        case FUNCTION_SUBSTRING_BEFORE:
        case FUNCTION_SUBSTRING_AFTER:
        case FUNCTION_SUBSTRING:
        case FUNCTION_STRING_LENGTH:
        case FUNCTION_NORMALIZE_SPACE:
        case FUNCTION_TRANSLATE:
        case FUNCTION_BOOLEAN:
        case FUNCTION_NOT:
        case FUNCTION_TRUE:
        case FUNCTION_FALSE:
        case FUNCTION_NULL:
        case FUNCTION_LANG:
        case FUNCTION_NUMBER:
        case FUNCTION_SUM:
        case FUNCTION_FLOOR:
        case FUNCTION_CEILING:
        case FUNCTION_ROUND:
        case FUNCTION_FORMAT_NUMBER:
        case NCName:
        case 88:
          name = WildcardName();
          break;
        default:
          jj_la1[10] = jj_gen;
          jj_consume_token(-1);
          throw new ParseException();
        }
      }
      break;
    case 82:
      jj_consume_token(82);
                    axis = Compiler.AXIS_SELF;
                    type = Compiler.NODE_TYPE_NODE;
      break;
    case 83:
      jj_consume_token(83);
                    axis = Compiler.AXIS_PARENT;
                    type = Compiler.NODE_TYPE_NODE;
      break;
    default:
      jj_la1[11] = jj_gen;
      jj_consume_token(-1);
      throw new ParseException();
    }
    label_3:
    while (true) {
      switch (jj_nt.kind) {
      case 84:
        ;
        break;
      default:
        jj_la1[12] = jj_gen;
        break label_3;
      }
      p = Predicate();
                ps.add(p);
    }
        if (name != null){
            s = compiler.nodeNameTest(name);
        }
        else if (instruction != null){
            s = compiler.processingInstructionTest(instruction);
        }
        else {
            s = compiler.nodeTypeTest(type);
        }
        steps.add(compiler.step(axis, s, ps.toArray()));
  }

/* [5] AxisSpecifier ::=    AxisName '::' | AbbreviatedAxisSpecifier  */
  final public int AxisSpecifier() throws ParseException {
    int axis;
    switch (jj_nt.kind) {
    case AXIS_SELF:
    case AXIS_CHILD:
    case AXIS_PARENT:
    case AXIS_ANCESTOR:
    case AXIS_ATTRIBUTE:
    case AXIS_NAMESPACE:
    case AXIS_PRECEDING:
    case AXIS_FOLLOWING:
    case AXIS_DESCENDANT:
    case AXIS_ANCESTOR_OR_SELF:
    case AXIS_FOLLOWING_SIBLING:
    case AXIS_PRECEDING_SIBLING:
    case AXIS_DESCENDANT_OR_SELF:
      axis = AxisName();
      break;
    default:
      jj_la1[13] = jj_gen;
      axis = AbbreviatedAxisSpecifier();
    }
        {if (true) return axis;}
    throw new Error("Missing return statement in function");
  }

/*----------*/
/* 2.2 Axes */
/*----------*/

/* [6] AxisName ::= 'ancestor' | 'ancestor-or-self' | 'attribute'  | 'child' | 'descendant'
                    | 'descendant-or-self' | 'following' | 'following-sibling' | 'namespace'
                       | 'parent' | 'preceding' | 'preceding-sibling' | 'self'
*/
  final public int AxisName() throws ParseException {
    int axis = 0;
    switch (jj_nt.kind) {
    case AXIS_SELF:
      jj_consume_token(AXIS_SELF);
                                          axis = Compiler.AXIS_SELF;
      break;
    case AXIS_CHILD:
      jj_consume_token(AXIS_CHILD);
                                          axis = Compiler.AXIS_CHILD;
      break;
    case AXIS_PARENT:
      jj_consume_token(AXIS_PARENT);
                                          axis = Compiler.AXIS_PARENT;
      break;
    case AXIS_ANCESTOR:
      jj_consume_token(AXIS_ANCESTOR);
                                          axis = Compiler.AXIS_ANCESTOR;
      break;
    case AXIS_ATTRIBUTE:
      jj_consume_token(AXIS_ATTRIBUTE);
                                          axis = Compiler.AXIS_ATTRIBUTE;
      break;
    case AXIS_NAMESPACE:
      jj_consume_token(AXIS_NAMESPACE);
                                          axis = Compiler.AXIS_NAMESPACE;
      break;
    case AXIS_PRECEDING:
      jj_consume_token(AXIS_PRECEDING);
                                          axis = Compiler.AXIS_PRECEDING;
      break;
    case AXIS_FOLLOWING:
      jj_consume_token(AXIS_FOLLOWING);
                                          axis = Compiler.AXIS_FOLLOWING;
      break;
    case AXIS_DESCENDANT:
      jj_consume_token(AXIS_DESCENDANT);
                                          axis = Compiler.AXIS_DESCENDANT;
      break;
    case AXIS_ANCESTOR_OR_SELF:
      jj_consume_token(AXIS_ANCESTOR_OR_SELF);
                                          axis = Compiler.AXIS_ANCESTOR_OR_SELF;
      break;
    case AXIS_FOLLOWING_SIBLING:
      jj_consume_token(AXIS_FOLLOWING_SIBLING);
                                          axis = Compiler.AXIS_FOLLOWING_SIBLING;
      break;
    case AXIS_PRECEDING_SIBLING:
      jj_consume_token(AXIS_PRECEDING_SIBLING);
                                          axis = Compiler.AXIS_PRECEDING_SIBLING;
      break;
    case AXIS_DESCENDANT_OR_SELF:
      jj_consume_token(AXIS_DESCENDANT_OR_SELF);
                                          axis = Compiler.AXIS_DESCENDANT_OR_SELF;
      break;
    default:
      jj_la1[14] = jj_gen;
      jj_consume_token(-1);
      throw new ParseException();
    }
        {if (true) return axis;}
    throw new Error("Missing return statement in function");
  }

/*----------------*/
/* 2.3 Node Tests */
/*----------------*/

/*----------------*/
/* 2.4 Predicates */
/*----------------*/

/* [8] Predicate ::= '[' PredicateExpr ']'  */
/* [9] PredicateExpr ::=  Expr  */
  final public Object Predicate() throws ParseException {
    Object ex;
    jj_consume_token(84);
    ex = Expression();
    jj_consume_token(85);
        {if (true) return ex;}
    throw new Error("Missing return statement in function");
  }

/* [12]    AbbreviatedStep    ::=    '.'  | '..'  */

/* [13]    AbbreviatedAxisSpecifier    ::=    '@'? */
  final public int AbbreviatedAxisSpecifier() throws ParseException {
    int axis = Compiler.AXIS_CHILD;
    switch (jj_nt.kind) {
    case 86:
      jj_consume_token(86);
           axis = Compiler.AXIS_ATTRIBUTE;
      break;
    default:
      jj_la1[15] = jj_gen;
      ;
    }
        {if (true) return axis;}
    throw new Error("Missing return statement in function");
  }

/*---------------*/
/* 3 Expressions */
/*---------------*/

/*------------*/
/* 3.1 Basics */
/*------------*/

/*
The effect of the grammar is that the order of precedence is (lowest precedence first):
    or
    and
    =, !=
    <=, <, >=, >
and all operators are left associative.
For example, 3 > 2 > 1 is equivalent to (3 > 2) > 1, which evaluates to false.
*/

/* [14] Expr ::= OrExpr */
  final public Object Expression() throws ParseException {
 Object ex;
    ex = OrExpr();
        {if (true) return ex;}
    throw new Error("Missing return statement in function");
  }

/* [15] PrimaryExpr ::= VariableReference | '(' Expr ')' | Literal | Number | FunctionCall */
  final public Object PrimaryExpr() throws ParseException {
    Object ex = null;
    switch (jj_nt.kind) {
    case VARIABLE:
      ex = VariableReference();
      break;
    case 80:
      jj_consume_token(80);
      ex = Expression();
      jj_consume_token(81);
      break;
    case Literal:
      jj_consume_token(Literal);
                      ex = compiler.literal(unescape(token.image.substring(1, token.image.length() - 1)));
      break;
    case Number:
      jj_consume_token(Number);
                      ex = compiler.number(token.image);
      break;
    default:
      jj_la1[16] = jj_gen;
      if (jj_2_5(2147483647)) {
        ex = CoreFunctionCall();
      } else {
        switch (jj_nt.kind) {
        case OR:
        case AND:
        case MOD:
        case DIV:
        case NODE:
        case TEXT:
        case COMMENT:
        case PI:
        case FUNCTION_LAST:
        case FUNCTION_POSITION:
        case FUNCTION_COUNT:
        case FUNCTION_ID:
        case FUNCTION_KEY:
        case FUNCTION_LOCAL_NAME:
        case FUNCTION_NAMESPACE_URI:
        case FUNCTION_NAME:
        case FUNCTION_STRING:
        case FUNCTION_CONCAT:
        case FUNCTION_STARTS_WITH:
        case FUNCTION_CONTAINS:
        case FUNCTION_SUBSTRING_BEFORE:
        case FUNCTION_SUBSTRING_AFTER:
        case FUNCTION_SUBSTRING:
        case FUNCTION_STRING_LENGTH:
        case FUNCTION_NORMALIZE_SPACE:
        case FUNCTION_TRANSLATE:
        case FUNCTION_BOOLEAN:
        case FUNCTION_NOT:
        case FUNCTION_TRUE:
        case FUNCTION_FALSE:
        case FUNCTION_NULL:
        case FUNCTION_LANG:
        case FUNCTION_NUMBER:
        case FUNCTION_SUM:
        case FUNCTION_FLOOR:
        case FUNCTION_CEILING:
        case FUNCTION_ROUND:
        case FUNCTION_FORMAT_NUMBER:
        case NCName:
          ex = FunctionCall();
          break;
        default:
          jj_la1[17] = jj_gen;
          jj_consume_token(-1);
          throw new ParseException();
        }
      }
    }
        {if (true) return ex;}
    throw new Error("Missing return statement in function");
  }

/*--------------------*/
/* 3.2 Function Calls */
/*--------------------*/

/* [16]    FunctionCall    ::=    FunctionName '(' ( Argument ( ',' Argument)*)? ')'  */
  final public Object FunctionCall() throws ParseException {
    Object name;
    ArrayList args;
    name = FunctionName();
    args = ArgumentList();
        if (args == null){
            {if (true) return compiler.function(name, null);}
        }
        else {
            {if (true) return compiler.function(name, args.toArray());}
        }
    throw new Error("Missing return statement in function");
  }

  final public Object CoreFunctionCall() throws ParseException {
    int code = 0;
    ArrayList args;
    code = CoreFunctionName();
    args = ArgumentList();
        if (args == null){
            {if (true) return compiler.function(code, null);}
        }
        else {
            {if (true) return compiler.function(code, args.toArray());}
        }
    throw new Error("Missing return statement in function");
  }

  final public ArrayList ArgumentList() throws ParseException {
    ArrayList args = null;
    Object arg;
    jj_consume_token(80);
    switch (jj_nt.kind) {
    case SLASH:
    case SLASHSLASH:
    case MINUS:
    case VARIABLE:
    case Literal:
    case Number:
    case OR:
    case AND:
    case MOD:
    case DIV:
    case NODE:
    case TEXT:
    case COMMENT:
    case PI:
    case AXIS_SELF:
    case AXIS_CHILD:
    case AXIS_PARENT:
    case AXIS_ANCESTOR:
    case AXIS_ATTRIBUTE:
    case AXIS_NAMESPACE:
    case AXIS_PRECEDING:
    case AXIS_FOLLOWING:
    case AXIS_DESCENDANT:
    case AXIS_ANCESTOR_OR_SELF:
    case AXIS_FOLLOWING_SIBLING:
    case AXIS_PRECEDING_SIBLING:
    case AXIS_DESCENDANT_OR_SELF:
    case FUNCTION_LAST:
    case FUNCTION_POSITION:
    case FUNCTION_COUNT:
    case FUNCTION_ID:
    case FUNCTION_KEY:
    case FUNCTION_LOCAL_NAME:
    case FUNCTION_NAMESPACE_URI:
    case FUNCTION_NAME:
    case FUNCTION_STRING:
    case FUNCTION_CONCAT:
    case FUNCTION_STARTS_WITH:
    case FUNCTION_CONTAINS:
    case FUNCTION_SUBSTRING_BEFORE:
    case FUNCTION_SUBSTRING_AFTER:
    case FUNCTION_SUBSTRING:
    case FUNCTION_STRING_LENGTH:
    case FUNCTION_NORMALIZE_SPACE:
    case FUNCTION_TRANSLATE:
    case FUNCTION_BOOLEAN:
    case FUNCTION_NOT:
    case FUNCTION_TRUE:
    case FUNCTION_FALSE:
    case FUNCTION_NULL:
    case FUNCTION_LANG:
    case FUNCTION_NUMBER:
    case FUNCTION_SUM:
    case FUNCTION_FLOOR:
    case FUNCTION_CEILING:
    case FUNCTION_ROUND:
    case FUNCTION_FORMAT_NUMBER:
    case NCName:
    case 80:
    case 82:
    case 83:
    case 86:
    case 88:
      arg = Argument();
                                args = new ArrayList(); args.add(arg);
      label_4:
      while (true) {
        switch (jj_nt.kind) {
        case 87:
          ;
          break;
        default:
          jj_la1[18] = jj_gen;
          break label_4;
        }
        jj_consume_token(87);
        arg = Argument();
                                       args.add(arg);
      }
      break;
    default:
      jj_la1[19] = jj_gen;
      ;
    }
    jj_consume_token(81);
        {if (true) return args;}
    throw new Error("Missing return statement in function");
  }

/* [17]    Argument    ::=    Expr */
  final public Object Argument() throws ParseException {
    Object ex;
    ex = Expression();
        {if (true) return ex;}
    throw new Error("Missing return statement in function");
  }

/*---------------*/
/* 3.3 Node-sets */
/*---------------*/

/* [18] UnionExpr    ::=    PathExpr | UnionExpr '|' PathExpr */
  final public Object UnionExpr() throws ParseException {
    Object ex, r;
    ArrayList list = null;
    ex = PathExpr();
    label_5:
    while (true) {
      switch (jj_nt.kind) {
      case UNION:
        ;
        break;
      default:
        jj_la1[20] = jj_gen;
        break label_5;
      }
      jj_consume_token(UNION);
      r = PathExpr();
                if (list == null){
                    list = new ArrayList();
                    list.add(ex);
                }
                list.add(r);
    }
        if (list != null){
            ex = compiler.union(list.toArray());
        }
        {if (true) return ex;}
    throw new Error("Missing return statement in function");
  }

/* [19] PathExpr ::= LocationPath | FilterExpr | FilterExpr '/' RelativeLocationPath | FilterExpr '//' RelativeLocationPath  */
  final public Object PathExpr() throws ParseException {
    Object ex = null;
    Object[] steps;
    if (jj_2_6(2147483647)) {
      ex = FilterExpr();
    } else {
      switch (jj_nt.kind) {
      case SLASH:
      case SLASHSLASH:
      case OR:
      case AND:
      case MOD:
      case DIV:
      case NODE:
      case TEXT:
      case COMMENT:
      case PI:
      case AXIS_SELF:
      case AXIS_CHILD:
      case AXIS_PARENT:
      case AXIS_ANCESTOR:
      case AXIS_ATTRIBUTE:
      case AXIS_NAMESPACE:
      case AXIS_PRECEDING:
      case AXIS_FOLLOWING:
      case AXIS_DESCENDANT:
      case AXIS_ANCESTOR_OR_SELF:
      case AXIS_FOLLOWING_SIBLING:
      case AXIS_PRECEDING_SIBLING:
      case AXIS_DESCENDANT_OR_SELF:
      case FUNCTION_LAST:
      case FUNCTION_POSITION:
      case FUNCTION_COUNT:
      case FUNCTION_ID:
      case FUNCTION_KEY:
      case FUNCTION_LOCAL_NAME:
      case FUNCTION_NAMESPACE_URI:
      case FUNCTION_NAME:
      case FUNCTION_STRING:
      case FUNCTION_CONCAT:
      case FUNCTION_STARTS_WITH:
      case FUNCTION_CONTAINS:
      case FUNCTION_SUBSTRING_BEFORE:
      case FUNCTION_SUBSTRING_AFTER:
      case FUNCTION_SUBSTRING:
      case FUNCTION_STRING_LENGTH:
      case FUNCTION_NORMALIZE_SPACE:
      case FUNCTION_TRANSLATE:
      case FUNCTION_BOOLEAN:
      case FUNCTION_NOT:
      case FUNCTION_TRUE:
      case FUNCTION_FALSE:
      case FUNCTION_NULL:
      case FUNCTION_LANG:
      case FUNCTION_NUMBER:
      case FUNCTION_SUM:
      case FUNCTION_FLOOR:
      case FUNCTION_CEILING:
      case FUNCTION_ROUND:
      case FUNCTION_FORMAT_NUMBER:
      case NCName:
      case 82:
      case 83:
      case 86:
      case 88:
        ex = LocationPath();
        break;
      default:
        jj_la1[21] = jj_gen;
        jj_consume_token(-1);
        throw new ParseException();
      }
    }
        {if (true) return ex;}
    throw new Error("Missing return statement in function");
  }

/* [20]    FilterExpr    ::=    PrimaryExpr    | FilterExpr Predicate */
  final public Object FilterExpr() throws ParseException {
    Object ex, p;
    ArrayList ps = new ArrayList();
    boolean path = false;
    ArrayList steps = new ArrayList();
    ex = PrimaryExpr();
    label_6:
    while (true) {
      switch (jj_nt.kind) {
      case 84:
        ;
        break;
      default:
        jj_la1[22] = jj_gen;
        break label_6;
      }
      p = Predicate();
                path = true;
                ps.add(p);
    }
    label_7:
    while (true) {
      switch (jj_nt.kind) {
      case SLASH:
      case SLASHSLASH:
        ;
        break;
      default:
        jj_la1[23] = jj_gen;
        break label_7;
      }
      LocationStep(steps);
                path = true;
    }
        if (path){
            {if (true) return compiler.expressionPath(ex, ps.toArray(), steps.toArray());}
        }
        else {
            {if (true) return ex;}
        }
    throw new Error("Missing return statement in function");
  }

/*--------------*/
/* 3.4 Booleans */
/*--------------*/

/* [21] OrExpr    ::=    AndExpr | OrExpr 'or' AndExpr */
  final public Object OrExpr() throws ParseException {
    Object ex, r;
    ArrayList list = null;
    ex = AndExpr();
    label_8:
    while (true) {
      switch (jj_nt.kind) {
      case OR:
        ;
        break;
      default:
        jj_la1[24] = jj_gen;
        break label_8;
      }
      jj_consume_token(OR);
      r = AndExpr();
                if (list == null){
                    list = new ArrayList();
                    list.add(ex);
                }
                list.add(r);
    }
        if (list != null){
            ex = compiler.or(list.toArray());
        }
        {if (true) return ex;}
    throw new Error("Missing return statement in function");
  }

/* [22] AndExpr    ::=    EqualityExpr  | AndExpr 'and' EqualityExpr  */
  final public Object AndExpr() throws ParseException {
    Object ex, r;
    ArrayList list = null;
    ex = EqualityExpr();
    label_9:
    while (true) {
      switch (jj_nt.kind) {
      case AND:
        ;
        break;
      default:
        jj_la1[25] = jj_gen;
        break label_9;
      }
      jj_consume_token(AND);
      r = EqualityExpr();
                if (list == null){
                    list = new ArrayList();
                    list.add(ex);
                }
                list.add(r);
    }
        if (list != null){
            ex = compiler.and(list.toArray());
        }
        {if (true) return ex;}
    throw new Error("Missing return statement in function");
  }

/* [23] EqualityExpr    ::=    RelationalExpr | EqualityExpr '=' RelationalExpr | EqualityExpr '!=' RelationalExpr */
  final public Object EqualityExpr() throws ParseException {
 Object ex, r;
    ex = RelationalExpr();
    label_10:
    while (true) {
      switch (jj_nt.kind) {
      case EQ:
      case NEQ:
        ;
        break;
      default:
        jj_la1[26] = jj_gen;
        break label_10;
      }
      switch (jj_nt.kind) {
      case EQ:
        jj_consume_token(EQ);
        r = RelationalExpr();
                                          ex = compiler.equal(ex, r);
        break;
      case NEQ:
        jj_consume_token(NEQ);
        r = RelationalExpr();
                                          ex = compiler.notEqual(ex, r);
        break;
      default:
        jj_la1[27] = jj_gen;
        jj_consume_token(-1);
        throw new ParseException();
      }
    }
        {if (true) return ex;}
    throw new Error("Missing return statement in function");
  }

/* [24] RelationalExpr    ::=    AdditiveExpr | RelationalExpr '<' AdditiveExpr | RelationalExpr '>' AdditiveExpr
                       | RelationalExpr '<=' AdditiveExpr  | RelationalExpr '>=' AdditiveExpr */
  final public Object RelationalExpr() throws ParseException {
 Object ex, r;
    ex = AdditiveExpr();
    label_11:
    while (true) {
      switch (jj_nt.kind) {
      case LT:
      case LTE:
      case GT:
      case GTE:
        ;
        break;
      default:
        jj_la1[28] = jj_gen;
        break label_11;
      }
      switch (jj_nt.kind) {
      case LT:
        jj_consume_token(LT);
        r = AdditiveExpr();
                                        ex = compiler.lessThan(ex, r);
        break;
      case GT:
        jj_consume_token(GT);
        r = AdditiveExpr();
                                        ex = compiler.greaterThan(ex, r);
        break;
      case LTE:
        jj_consume_token(LTE);
        r = AdditiveExpr();
                                        ex = compiler.lessThanOrEqual(ex, r);
        break;
      case GTE:
        jj_consume_token(GTE);
        r = AdditiveExpr();
                                        ex = compiler.greaterThanOrEqual(ex, r);
        break;
      default:
        jj_la1[29] = jj_gen;
        jj_consume_token(-1);
        throw new ParseException();
      }
    }
        {if (true) return ex;}
    throw new Error("Missing return statement in function");
  }

/*-------------*/
/* 3.5 Numbers */
/*-------------*/

/* [25] AdditiveExpr ::= MultiplicativeExpr  | AdditiveExpr '+' MultiplicativeExpr  | AdditiveExpr '-' MultiplicativeExpr  */
  final public Object AdditiveExpr() throws ParseException {
    Object ex, r;
    ArrayList list = null;
    ex = SubtractiveExpr();
    label_12:
    while (true) {
      switch (jj_nt.kind) {
      case PLUS:
        ;
        break;
      default:
        jj_la1[30] = jj_gen;
        break label_12;
      }
      jj_consume_token(PLUS);
      r = SubtractiveExpr();
                if (list == null){
                    list = new ArrayList();
                    list.add(ex);
                }
                list.add(r);
    }
        if (list != null){
            ex = compiler.sum(list.toArray());
        }
        {if (true) return ex;}
    throw new Error("Missing return statement in function");
  }

  final public Object SubtractiveExpr() throws ParseException {
    Object ex, r = null;
    ex = MultiplicativeExpr();
    label_13:
    while (true) {
      switch (jj_nt.kind) {
      case MINUS:
        ;
        break;
      default:
        jj_la1[31] = jj_gen;
        break label_13;
      }
      jj_consume_token(MINUS);
      r = MultiplicativeExpr();
                                               ex = compiler.minus(ex, r);
    }
        {if (true) return ex;}
    throw new Error("Missing return statement in function");
  }

/* [26] MultiplicativeExpr ::= UnaryExpr | MultiplicativeExpr MultiplyOperator UnaryExpr
            | MultiplicativeExpr 'div' UnaryExpr | MultiplicativeExpr 'mod' UnaryExpr  */
  final public Object MultiplicativeExpr() throws ParseException {
    Object ex, r;
    ex = UnaryExpr();
    label_14:
    while (true) {
      switch (jj_nt.kind) {
      case MOD:
      case DIV:
      case 88:
        ;
        break;
      default:
        jj_la1[32] = jj_gen;
        break label_14;
      }
      switch (jj_nt.kind) {
      case 88:
        jj_consume_token(88);
        r = UnaryExpr();
                                    ex = compiler.multiply(ex, r);
        break;
      case DIV:
        jj_consume_token(DIV);
        r = UnaryExpr();
                                    ex = compiler.divide(ex, r);
        break;
      case MOD:
        jj_consume_token(MOD);
        r = UnaryExpr();
                                    ex = compiler.mod(ex, r);
        break;
      default:
        jj_la1[33] = jj_gen;
        jj_consume_token(-1);
        throw new ParseException();
      }
    }
        {if (true) return ex;}
    throw new Error("Missing return statement in function");
  }

/* [27]    UnaryExpr    ::=    UnionExpr  | '-' UnaryExpr  */
  final public Object UnaryExpr() throws ParseException {
    Object ex;
    switch (jj_nt.kind) {
    case SLASH:
    case SLASHSLASH:
    case VARIABLE:
    case Literal:
    case Number:
    case OR:
    case AND:
    case MOD:
    case DIV:
    case NODE:
    case TEXT:
    case COMMENT:
    case PI:
    case AXIS_SELF:
    case AXIS_CHILD:
    case AXIS_PARENT:
    case AXIS_ANCESTOR:
    case AXIS_ATTRIBUTE:
    case AXIS_NAMESPACE:
    case AXIS_PRECEDING:
    case AXIS_FOLLOWING:
    case AXIS_DESCENDANT:
    case AXIS_ANCESTOR_OR_SELF:
    case AXIS_FOLLOWING_SIBLING:
    case AXIS_PRECEDING_SIBLING:
    case AXIS_DESCENDANT_OR_SELF:
    case FUNCTION_LAST:
    case FUNCTION_POSITION:
    case FUNCTION_COUNT:
    case FUNCTION_ID:
    case FUNCTION_KEY:
    case FUNCTION_LOCAL_NAME:
    case FUNCTION_NAMESPACE_URI:
    case FUNCTION_NAME:
    case FUNCTION_STRING:
    case FUNCTION_CONCAT:
    case FUNCTION_STARTS_WITH:
    case FUNCTION_CONTAINS:
    case FUNCTION_SUBSTRING_BEFORE:
    case FUNCTION_SUBSTRING_AFTER:
    case FUNCTION_SUBSTRING:
    case FUNCTION_STRING_LENGTH:
    case FUNCTION_NORMALIZE_SPACE:
    case FUNCTION_TRANSLATE:
    case FUNCTION_BOOLEAN:
    case FUNCTION_NOT:
    case FUNCTION_TRUE:
    case FUNCTION_FALSE:
    case FUNCTION_NULL:
    case FUNCTION_LANG:
    case FUNCTION_NUMBER:
    case FUNCTION_SUM:
    case FUNCTION_FLOOR:
    case FUNCTION_CEILING:
    case FUNCTION_ROUND:
    case FUNCTION_FORMAT_NUMBER:
    case NCName:
    case 80:
    case 82:
    case 83:
    case 86:
    case 88:
      ex = UnionExpr();
      break;
    case MINUS:
      jj_consume_token(MINUS);
      ex = UnaryExpr();
                                    ex = compiler.minus(ex);
      break;
    default:
      jj_la1[34] = jj_gen;
      jj_consume_token(-1);
      throw new ParseException();
    }
        {if (true) return ex;}
    throw new Error("Missing return statement in function");
  }

/*-------------*/
/* 3.6 Strings */
/*-------------*/

/*----------------------------------*/
/* 3.7 Expression Lexical Structure */
/*----------------------------------*/
/*
The following special tokenization rules must be applied in the order
specified to disambiguate the grammar:

1. If there is a preceding token and the preceding token is not one of
   @, ::, (, [, , or an Operator,
   then a * must be recognized as a MultiplyOperator and an NCName must
   be recognized as an OperatorName.

2. If the character following an NCName (possibly after intervening ExprWhitespace)
   is (, then the token must be recognized as a NodeType or a FunctionName.

3. If the two characters following an NCName (possibly after intervening ExprWhitespace)
   are ::, then the token must be recognized as an AxisName.

4. Otherwise, the token must not be recognized as a MultiplyOperator, an OperatorName,
   a NodeType, a FunctionName, or an AxisName.
*/

/*
[28]    ExprToken    ::=    '(' | ')' | '[' | ']' | '.' | '..' | '@' | ',' | '::'
   | WildcardName  | NodeType  | Operator  | FunctionName  | AxisName  | Literal
   | Number  | VariableReference
*/
/* [34]    MultiplyOperator    ::=    '*'  */

/* [35]    FunctionName    ::=    QName - NodeType   */
  final public Object FunctionName() throws ParseException {
    Object qname;
    qname = QName_Without_CoreFunctions();
        {if (true) return qname;}
    throw new Error("Missing return statement in function");
  }

/* [36]    VariableReference    ::=    '$' QName  */
  final public Object VariableReference() throws ParseException {
    Object ex;
    jj_consume_token(VARIABLE);
    ex = QName();
        {if (true) return compiler.variableReference(ex);}
    throw new Error("Missing return statement in function");
  }

/* [37]    WildcardName    ::=    '*'     | NCName ':' '*'     | QName  */
  final public Object WildcardName() throws ParseException {
    Object qn;
    String nc1, nc2 = null;
    switch (jj_nt.kind) {
    case 88:
      jj_consume_token(88);
      break;
    case OR:
    case AND:
    case MOD:
    case DIV:
    case NODE:
    case TEXT:
    case COMMENT:
    case PI:
    case FUNCTION_LAST:
    case FUNCTION_POSITION:
    case FUNCTION_COUNT:
    case FUNCTION_ID:
    case FUNCTION_KEY:
    case FUNCTION_LOCAL_NAME:
    case FUNCTION_NAMESPACE_URI:
    case FUNCTION_NAME:
    case FUNCTION_STRING:
    case FUNCTION_CONCAT:
    case FUNCTION_STARTS_WITH:
    case FUNCTION_CONTAINS:
    case FUNCTION_SUBSTRING_BEFORE:
    case FUNCTION_SUBSTRING_AFTER:
    case FUNCTION_SUBSTRING:
    case FUNCTION_STRING_LENGTH:
    case FUNCTION_NORMALIZE_SPACE:
    case FUNCTION_TRANSLATE:
    case FUNCTION_BOOLEAN:
    case FUNCTION_NOT:
    case FUNCTION_TRUE:
    case FUNCTION_FALSE:
    case FUNCTION_NULL:
    case FUNCTION_LANG:
    case FUNCTION_NUMBER:
    case FUNCTION_SUM:
    case FUNCTION_FLOOR:
    case FUNCTION_CEILING:
    case FUNCTION_ROUND:
    case FUNCTION_FORMAT_NUMBER:
    case NCName:
      NCName();
      break;
    default:
      jj_la1[35] = jj_gen;
      jj_consume_token(-1);
      throw new ParseException();
    }
                           nc1 = token.image;
    switch (jj_nt.kind) {
    case 79:
      jj_consume_token(79);
      switch (jj_nt.kind) {
      case 88:
        jj_consume_token(88);
        break;
      case OR:
      case AND:
      case MOD:
      case DIV:
      case NODE:
      case TEXT:
      case COMMENT:
      case PI:
      case FUNCTION_LAST:
      case FUNCTION_POSITION:
      case FUNCTION_COUNT:
      case FUNCTION_ID:
      case FUNCTION_KEY:
      case FUNCTION_LOCAL_NAME:
      case FUNCTION_NAMESPACE_URI:
      case FUNCTION_NAME:
      case FUNCTION_STRING:
      case FUNCTION_CONCAT:
      case FUNCTION_STARTS_WITH:
      case FUNCTION_CONTAINS:
      case FUNCTION_SUBSTRING_BEFORE:
      case FUNCTION_SUBSTRING_AFTER:
      case FUNCTION_SUBSTRING:
      case FUNCTION_STRING_LENGTH:
      case FUNCTION_NORMALIZE_SPACE:
      case FUNCTION_TRANSLATE:
      case FUNCTION_BOOLEAN:
      case FUNCTION_NOT:
      case FUNCTION_TRUE:
      case FUNCTION_FALSE:
      case FUNCTION_NULL:
      case FUNCTION_LANG:
      case FUNCTION_NUMBER:
      case FUNCTION_SUM:
      case FUNCTION_FLOOR:
      case FUNCTION_CEILING:
      case FUNCTION_ROUND:
      case FUNCTION_FORMAT_NUMBER:
      case NCName:
        NCName();
        break;
      default:
        jj_la1[36] = jj_gen;
        jj_consume_token(-1);
        throw new ParseException();
      }
                               nc2 = token.image;
      break;
    default:
      jj_la1[37] = jj_gen;
      ;
    }
        if (nc2 != null){
            qn = compiler.qname(nc1, nc2);
        }
        else {
            qn = compiler.qname(null, nc1);
        }
        {if (true) return qn;}
    throw new Error("Missing return statement in function");
  }

/* [38]    NodeType    ::=    'comment' | 'text'  | 'processing-instruction'  | 'node'  */
  final public int NodeType() throws ParseException {
    int type;
    switch (jj_nt.kind) {
    case TEXT:
      jj_consume_token(TEXT);
                      type = Compiler.NODE_TYPE_TEXT;
      break;
    case NODE:
      jj_consume_token(NODE);
                      type = Compiler.NODE_TYPE_NODE;
      break;
    case COMMENT:
      jj_consume_token(COMMENT);
                      type = Compiler.NODE_TYPE_COMMENT;
      break;
    case PI:
      jj_consume_token(PI);
                      type = Compiler.NODE_TYPE_PI;
      break;
    default:
      jj_la1[38] = jj_gen;
      jj_consume_token(-1);
      throw new ParseException();
    }
        {if (true) return type;}
    throw new Error("Missing return statement in function");
  }

  final private boolean jj_2_1(int xla) {
    jj_la = xla; jj_lastpos = jj_scanpos = token;
    boolean retval = !jj_3_1();
    jj_save(0, xla);
    return retval;
  }

  final private boolean jj_2_2(int xla) {
    jj_la = xla; jj_lastpos = jj_scanpos = token;
    boolean retval = !jj_3_2();
    jj_save(1, xla);
    return retval;
  }

  final private boolean jj_2_3(int xla) {
    jj_la = xla; jj_lastpos = jj_scanpos = token;
    boolean retval = !jj_3_3();
    jj_save(2, xla);
    return retval;
  }

  final private boolean jj_2_4(int xla) {
    jj_la = xla; jj_lastpos = jj_scanpos = token;
    boolean retval = !jj_3_4();
    jj_save(3, xla);
    return retval;
  }

  final private boolean jj_2_5(int xla) {
    jj_la = xla; jj_lastpos = jj_scanpos = token;
    boolean retval = !jj_3_5();
    jj_save(4, xla);
    return retval;
  }

  final private boolean jj_2_6(int xla) {
    jj_la = xla; jj_lastpos = jj_scanpos = token;
    boolean retval = !jj_3_6();
    jj_save(5, xla);
    return retval;
  }

  final private boolean jj_3R_65() {
    if (jj_scan_token(FUNCTION_ID)) return true;
    if (jj_la == 0 && jj_scanpos == jj_lastpos) return false;
    return false;
  }

  final private boolean jj_3R_104() {
    if (jj_3R_118()) return true;
    if (jj_la == 0 && jj_scanpos == jj_lastpos) return false;
    return false;
  }

  final private boolean jj_3R_64() {
    if (jj_scan_token(FUNCTION_COUNT)) return true;
    if (jj_la == 0 && jj_scanpos == jj_lastpos) return false;
    return false;
  }

  final private boolean jj_3R_63() {
    if (jj_scan_token(FUNCTION_POSITION)) return true;
    if (jj_la == 0 && jj_scanpos == jj_lastpos) return false;
    return false;
  }

  final private boolean jj_3R_62() {
    if (jj_scan_token(FUNCTION_LAST)) return true;
    if (jj_la == 0 && jj_scanpos == jj_lastpos) return false;
    return false;
  }

  final private boolean jj_3R_18() {
    Token xsp;
    xsp = jj_scanpos;
    if (jj_3R_62()) {
    jj_scanpos = xsp;
    if (jj_3R_63()) {
    jj_scanpos = xsp;
    if (jj_3R_64()) {
    jj_scanpos = xsp;
    if (jj_3R_65()) {
    jj_scanpos = xsp;
    if (jj_3R_66()) {
    jj_scanpos = xsp;
    if (jj_3R_67()) {
    jj_scanpos = xsp;
    if (jj_3R_68()) {
    jj_scanpos = xsp;
    if (jj_3R_69()) {
    jj_scanpos = xsp;
    if (jj_3R_70()) {
    jj_scanpos = xsp;
    if (jj_3R_71()) {
    jj_scanpos = xsp;
    if (jj_3R_72()) {
    jj_scanpos = xsp;
    if (jj_3R_73()) {
    jj_scanpos = xsp;
    if (jj_3R_74()) {
    jj_scanpos = xsp;
    if (jj_3R_75()) {
    jj_scanpos = xsp;
    if (jj_3R_76()) {
    jj_scanpos = xsp;
    if (jj_3R_77()) {
    jj_scanpos = xsp;
    if (jj_3R_78()) {
    jj_scanpos = xsp;
    if (jj_3R_79()) {
    jj_scanpos = xsp;
    if (jj_3R_80()) {
    jj_scanpos = xsp;
    if (jj_3R_81()) {
    jj_scanpos = xsp;
    if (jj_3R_82()) {
    jj_scanpos = xsp;
    if (jj_3R_83()) {
    jj_scanpos = xsp;
    if (jj_3R_84()) {
    jj_scanpos = xsp;
    if (jj_3R_85()) {
    jj_scanpos = xsp;
    if (jj_3R_86()) {
    jj_scanpos = xsp;
    if (jj_3R_87()) {
    jj_scanpos = xsp;
    if (jj_3R_88()) {
    jj_scanpos = xsp;
    if (jj_3R_89()) {
    jj_scanpos = xsp;
    if (jj_3R_90()) {
    jj_scanpos = xsp;
    if (jj_3R_91()) return true;
    if (jj_la == 0 && jj_scanpos == jj_lastpos) return false;
    } else if (jj_la == 0 && jj_scanpos == jj_lastpos) return false;
    } else if (jj_la == 0 && jj_scanpos == jj_lastpos) return false;
    } else if (jj_la == 0 && jj_scanpos == jj_lastpos) return false;
    } else if (jj_la == 0 && jj_scanpos == jj_lastpos) return false;
    } else if (jj_la == 0 && jj_scanpos == jj_lastpos) return false;
    } else if (jj_la == 0 && jj_scanpos == jj_lastpos) return false;
    } else if (jj_la == 0 && jj_scanpos == jj_lastpos) return false;
    } else if (jj_la == 0 && jj_scanpos == jj_lastpos) return false;
    } else if (jj_la == 0 && jj_scanpos == jj_lastpos) return false;
    } else if (jj_la == 0 && jj_scanpos == jj_lastpos) return false;
    } else if (jj_la == 0 && jj_scanpos == jj_lastpos) return false;
    } else if (jj_la == 0 && jj_scanpos == jj_lastpos) return false;
    } else if (jj_la == 0 && jj_scanpos == jj_lastpos) return false;
    } else if (jj_la == 0 && jj_scanpos == jj_lastpos) return false;
    } else if (jj_la == 0 && jj_scanpos == jj_lastpos) return false;
    } else if (jj_la == 0 && jj_scanpos == jj_lastpos) return false;
    } else if (jj_la == 0 && jj_scanpos == jj_lastpos) return false;
    } else if (jj_la == 0 && jj_scanpos == jj_lastpos) return false;
    } else if (jj_la == 0 && jj_scanpos == jj_lastpos) return false;
    } else if (jj_la == 0 && jj_scanpos == jj_lastpos) return false;
    } else if (jj_la == 0 && jj_scanpos == jj_lastpos) return false;
    } else if (jj_la == 0 && jj_scanpos == jj_lastpos) return false;
    } else if (jj_la == 0 && jj_scanpos == jj_lastpos) return false;
    } else if (jj_la == 0 && jj_scanpos == jj_lastpos) return false;
    } else if (jj_la == 0 && jj_scanpos == jj_lastpos) return false;
    } else if (jj_la == 0 && jj_scanpos == jj_lastpos) return false;
    } else if (jj_la == 0 && jj_scanpos == jj_lastpos) return false;
    } else if (jj_la == 0 && jj_scanpos == jj_lastpos) return false;
    } else if (jj_la == 0 && jj_scanpos == jj_lastpos) return false;
    return false;
  }

  final private boolean jj_3R_111() {
    if (jj_scan_token(DIV)) return true;
    if (jj_la == 0 && jj_scanpos == jj_lastpos) return false;
    return false;
  }

  final private boolean jj_3R_110() {
    if (jj_scan_token(MOD)) return true;
    if (jj_la == 0 && jj_scanpos == jj_lastpos) return false;
    return false;
  }

  final private boolean jj_3R_109() {
    if (jj_scan_token(AND)) return true;
    if (jj_la == 0 && jj_scanpos == jj_lastpos) return false;
    return false;
  }

  final private boolean jj_3R_108() {
    if (jj_scan_token(OR)) return true;
    if (jj_la == 0 && jj_scanpos == jj_lastpos) return false;
    return false;
  }

  final private boolean jj_3R_107() {
    if (jj_scan_token(NCName)) return true;
    if (jj_la == 0 && jj_scanpos == jj_lastpos) return false;
    return false;
  }

  final private boolean jj_3R_173() {
    if (jj_scan_token(MINUS)) return true;
    if (jj_la == 0 && jj_scanpos == jj_lastpos) return false;
    if (jj_3R_170()) return true;
    if (jj_la == 0 && jj_scanpos == jj_lastpos) return false;
    return false;
  }

  final private boolean jj_3R_98() {
    Token xsp;
    xsp = jj_scanpos;
    if (jj_3R_107()) {
    jj_scanpos = xsp;
    if (jj_3R_108()) {
    jj_scanpos = xsp;
    if (jj_3R_109()) {
    jj_scanpos = xsp;
    if (jj_3R_110()) {
    jj_scanpos = xsp;
    if (jj_3R_111()) return true;
    if (jj_la == 0 && jj_scanpos == jj_lastpos) return false;
    } else if (jj_la == 0 && jj_scanpos == jj_lastpos) return false;
    } else if (jj_la == 0 && jj_scanpos == jj_lastpos) return false;
    } else if (jj_la == 0 && jj_scanpos == jj_lastpos) return false;
    } else if (jj_la == 0 && jj_scanpos == jj_lastpos) return false;
    return false;
  }

  final private boolean jj_3R_171() {
    Token xsp;
    xsp = jj_scanpos;
    if (jj_3R_174()) {
    jj_scanpos = xsp;
    if (jj_3R_175()) {
    jj_scanpos = xsp;
    if (jj_3R_176()) return true;
    if (jj_la == 0 && jj_scanpos == jj_lastpos) return false;
    } else if (jj_la == 0 && jj_scanpos == jj_lastpos) return false;
    } else if (jj_la == 0 && jj_scanpos == jj_lastpos) return false;
    return false;
  }

  final private boolean jj_3R_172() {
    if (jj_3R_177()) return true;
    if (jj_la == 0 && jj_scanpos == jj_lastpos) return false;
    return false;
  }

  final private boolean jj_3R_153() {
    if (jj_scan_token(86)) return true;
    if (jj_la == 0 && jj_scanpos == jj_lastpos) return false;
    return false;
  }

  final private boolean jj_3R_130() {
    Token xsp;
    xsp = jj_scanpos;
    if (jj_3R_153()) jj_scanpos = xsp;
    else if (jj_la == 0 && jj_scanpos == jj_lastpos) return false;
    return false;
  }

  final private boolean jj_3R_54() {
    if (jj_scan_token(FUNCTION_FORMAT_NUMBER)) return true;
    if (jj_la == 0 && jj_scanpos == jj_lastpos) return false;
    return false;
  }

  final private boolean jj_3R_170() {
    Token xsp;
    xsp = jj_scanpos;
    if (jj_3R_172()) {
    jj_scanpos = xsp;
    if (jj_3R_173()) return true;
    if (jj_la == 0 && jj_scanpos == jj_lastpos) return false;
    } else if (jj_la == 0 && jj_scanpos == jj_lastpos) return false;
    return false;
  }

  final private boolean jj_3R_53() {
    if (jj_scan_token(FUNCTION_KEY)) return true;
    if (jj_la == 0 && jj_scanpos == jj_lastpos) return false;
    return false;
  }

  final private boolean jj_3R_52() {
    if (jj_scan_token(FUNCTION_ROUND)) return true;
    if (jj_la == 0 && jj_scanpos == jj_lastpos) return false;
    return false;
  }

  final private boolean jj_3R_51() {
    if (jj_scan_token(FUNCTION_CEILING)) return true;
    if (jj_la == 0 && jj_scanpos == jj_lastpos) return false;
    return false;
  }

  final private boolean jj_3R_50() {
    if (jj_scan_token(FUNCTION_FLOOR)) return true;
    if (jj_la == 0 && jj_scanpos == jj_lastpos) return false;
    return false;
  }

  final private boolean jj_3R_49() {
    if (jj_scan_token(FUNCTION_SUM)) return true;
    if (jj_la == 0 && jj_scanpos == jj_lastpos) return false;
    return false;
  }

  final private boolean jj_3R_48() {
    if (jj_scan_token(FUNCTION_NUMBER)) return true;
    if (jj_la == 0 && jj_scanpos == jj_lastpos) return false;
    return false;
  }

  final private boolean jj_3R_176() {
    if (jj_scan_token(MOD)) return true;
    if (jj_la == 0 && jj_scanpos == jj_lastpos) return false;
    if (jj_3R_170()) return true;
    if (jj_la == 0 && jj_scanpos == jj_lastpos) return false;
    return false;
  }

  final private boolean jj_3R_47() {
    if (jj_scan_token(FUNCTION_LANG)) return true;
    if (jj_la == 0 && jj_scanpos == jj_lastpos) return false;
    return false;
  }

  final private boolean jj_3R_175() {
    if (jj_scan_token(DIV)) return true;
    if (jj_la == 0 && jj_scanpos == jj_lastpos) return false;
    if (jj_3R_170()) return true;
    if (jj_la == 0 && jj_scanpos == jj_lastpos) return false;
    return false;
  }

  final private boolean jj_3R_46() {
    if (jj_scan_token(FUNCTION_NULL)) return true;
    if (jj_la == 0 && jj_scanpos == jj_lastpos) return false;
    return false;
  }

  final private boolean jj_3R_174() {
    if (jj_scan_token(88)) return true;
    if (jj_la == 0 && jj_scanpos == jj_lastpos) return false;
    if (jj_3R_170()) return true;
    if (jj_la == 0 && jj_scanpos == jj_lastpos) return false;
    return false;
  }

  final private boolean jj_3R_45() {
    if (jj_scan_token(FUNCTION_FALSE)) return true;
    if (jj_la == 0 && jj_scanpos == jj_lastpos) return false;
    return false;
  }

  final private boolean jj_3R_44() {
    if (jj_scan_token(FUNCTION_TRUE)) return true;
    if (jj_la == 0 && jj_scanpos == jj_lastpos) return false;
    return false;
  }

  final private boolean jj_3R_43() {
    if (jj_scan_token(FUNCTION_NOT)) return true;
    if (jj_la == 0 && jj_scanpos == jj_lastpos) return false;
    return false;
  }

  final private boolean jj_3R_42() {
    if (jj_scan_token(FUNCTION_BOOLEAN)) return true;
    if (jj_la == 0 && jj_scanpos == jj_lastpos) return false;
    return false;
  }

  final private boolean jj_3R_41() {
    if (jj_scan_token(FUNCTION_TRANSLATE)) return true;
    if (jj_la == 0 && jj_scanpos == jj_lastpos) return false;
    return false;
  }

  final private boolean jj_3R_116() {
    if (jj_scan_token(84)) return true;
    if (jj_la == 0 && jj_scanpos == jj_lastpos) return false;
    if (jj_3R_104()) return true;
    if (jj_la == 0 && jj_scanpos == jj_lastpos) return false;
    if (jj_scan_token(85)) return true;
    if (jj_la == 0 && jj_scanpos == jj_lastpos) return false;
    return false;
  }

  final private boolean jj_3R_40() {
    if (jj_scan_token(FUNCTION_NORMALIZE_SPACE)) return true;
    if (jj_la == 0 && jj_scanpos == jj_lastpos) return false;
    return false;
  }

  final private boolean jj_3R_39() {
    if (jj_scan_token(FUNCTION_STRING_LENGTH)) return true;
    if (jj_la == 0 && jj_scanpos == jj_lastpos) return false;
    return false;
  }

  final private boolean jj_3R_38() {
    if (jj_scan_token(FUNCTION_SUBSTRING)) return true;
    if (jj_la == 0 && jj_scanpos == jj_lastpos) return false;
    return false;
  }

  final private boolean jj_3R_37() {
    if (jj_scan_token(FUNCTION_SUBSTRING_AFTER)) return true;
    if (jj_la == 0 && jj_scanpos == jj_lastpos) return false;
    return false;
  }

  final private boolean jj_3R_36() {
    if (jj_scan_token(FUNCTION_SUBSTRING_BEFORE)) return true;
    if (jj_la == 0 && jj_scanpos == jj_lastpos) return false;
    return false;
  }

  final private boolean jj_3R_168() {
    if (jj_3R_170()) return true;
    if (jj_la == 0 && jj_scanpos == jj_lastpos) return false;
    Token xsp;
    while (true) {
      xsp = jj_scanpos;
      if (jj_3R_171()) { jj_scanpos = xsp; break; }
      if (jj_la == 0 && jj_scanpos == jj_lastpos) return false;
    }
    return false;
  }

  final private boolean jj_3R_35() {
    if (jj_scan_token(FUNCTION_CONTAINS)) return true;
    if (jj_la == 0 && jj_scanpos == jj_lastpos) return false;
    return false;
  }

  final private boolean jj_3R_34() {
    if (jj_scan_token(FUNCTION_STARTS_WITH)) return true;
    if (jj_la == 0 && jj_scanpos == jj_lastpos) return false;
    return false;
  }

  final private boolean jj_3R_33() {
    if (jj_scan_token(FUNCTION_CONCAT)) return true;
    if (jj_la == 0 && jj_scanpos == jj_lastpos) return false;
    return false;
  }

  final private boolean jj_3R_32() {
    if (jj_scan_token(FUNCTION_STRING)) return true;
    if (jj_la == 0 && jj_scanpos == jj_lastpos) return false;
    return false;
  }

  final private boolean jj_3R_31() {
    if (jj_scan_token(FUNCTION_NAME)) return true;
    if (jj_la == 0 && jj_scanpos == jj_lastpos) return false;
    return false;
  }

  final private boolean jj_3R_30() {
    if (jj_scan_token(FUNCTION_NAMESPACE_URI)) return true;
    if (jj_la == 0 && jj_scanpos == jj_lastpos) return false;
    return false;
  }

  final private boolean jj_3R_29() {
    if (jj_scan_token(FUNCTION_LOCAL_NAME)) return true;
    if (jj_la == 0 && jj_scanpos == jj_lastpos) return false;
    return false;
  }

  final private boolean jj_3R_169() {
    if (jj_scan_token(MINUS)) return true;
    if (jj_la == 0 && jj_scanpos == jj_lastpos) return false;
    if (jj_3R_168()) return true;
    if (jj_la == 0 && jj_scanpos == jj_lastpos) return false;
    return false;
  }

  final private boolean jj_3R_28() {
    if (jj_scan_token(FUNCTION_ID)) return true;
    if (jj_la == 0 && jj_scanpos == jj_lastpos) return false;
    return false;
  }

  final private boolean jj_3R_27() {
    if (jj_scan_token(FUNCTION_COUNT)) return true;
    if (jj_la == 0 && jj_scanpos == jj_lastpos) return false;
    return false;
  }

  final private boolean jj_3R_152() {
    if (jj_scan_token(AXIS_DESCENDANT_OR_SELF)) return true;
    if (jj_la == 0 && jj_scanpos == jj_lastpos) return false;
    return false;
  }

  final private boolean jj_3R_26() {
    if (jj_scan_token(FUNCTION_POSITION)) return true;
    if (jj_la == 0 && jj_scanpos == jj_lastpos) return false;
    return false;
  }

  final private boolean jj_3R_25() {
    if (jj_scan_token(FUNCTION_LAST)) return true;
    if (jj_la == 0 && jj_scanpos == jj_lastpos) return false;
    return false;
  }

  final private boolean jj_3R_151() {
    if (jj_scan_token(AXIS_PRECEDING_SIBLING)) return true;
    if (jj_la == 0 && jj_scanpos == jj_lastpos) return false;
    return false;
  }

  final private boolean jj_3R_24() {
    if (jj_scan_token(PI)) return true;
    if (jj_la == 0 && jj_scanpos == jj_lastpos) return false;
    return false;
  }

  final private boolean jj_3R_150() {
    if (jj_scan_token(AXIS_FOLLOWING_SIBLING)) return true;
    if (jj_la == 0 && jj_scanpos == jj_lastpos) return false;
    return false;
  }

  final private boolean jj_3R_23() {
    if (jj_scan_token(COMMENT)) return true;
    if (jj_la == 0 && jj_scanpos == jj_lastpos) return false;
    return false;
  }

  final private boolean jj_3R_149() {
    if (jj_scan_token(AXIS_ANCESTOR_OR_SELF)) return true;
    if (jj_la == 0 && jj_scanpos == jj_lastpos) return false;
    return false;
  }

  final private boolean jj_3R_163() {
    if (jj_scan_token(PLUS)) return true;
    if (jj_la == 0 && jj_scanpos == jj_lastpos) return false;
    if (jj_3R_162()) return true;
    if (jj_la == 0 && jj_scanpos == jj_lastpos) return false;
    return false;
  }

  final private boolean jj_3R_22() {
    if (jj_scan_token(TEXT)) return true;
    if (jj_la == 0 && jj_scanpos == jj_lastpos) return false;
    return false;
  }

  final private boolean jj_3R_148() {
    if (jj_scan_token(AXIS_DESCENDANT)) return true;
    if (jj_la == 0 && jj_scanpos == jj_lastpos) return false;
    return false;
  }

  final private boolean jj_3R_21() {
    if (jj_scan_token(NODE)) return true;
    if (jj_la == 0 && jj_scanpos == jj_lastpos) return false;
    return false;
  }

  final private boolean jj_3R_147() {
    if (jj_scan_token(AXIS_FOLLOWING)) return true;
    if (jj_la == 0 && jj_scanpos == jj_lastpos) return false;
    return false;
  }

  final private boolean jj_3R_20() {
    if (jj_3R_98()) return true;
    if (jj_la == 0 && jj_scanpos == jj_lastpos) return false;
    return false;
  }

  final private boolean jj_3R_146() {
    if (jj_scan_token(AXIS_PRECEDING)) return true;
    if (jj_la == 0 && jj_scanpos == jj_lastpos) return false;
    return false;
  }

  final private boolean jj_3R_145() {
    if (jj_scan_token(AXIS_NAMESPACE)) return true;
    if (jj_la == 0 && jj_scanpos == jj_lastpos) return false;
    return false;
  }

  final private boolean jj_3R_162() {
    if (jj_3R_168()) return true;
    if (jj_la == 0 && jj_scanpos == jj_lastpos) return false;
    Token xsp;
    while (true) {
      xsp = jj_scanpos;
      if (jj_3R_169()) { jj_scanpos = xsp; break; }
      if (jj_la == 0 && jj_scanpos == jj_lastpos) return false;
    }
    return false;
  }

  final private boolean jj_3R_144() {
    if (jj_scan_token(AXIS_ATTRIBUTE)) return true;
    if (jj_la == 0 && jj_scanpos == jj_lastpos) return false;
    return false;
  }

  final private boolean jj_3R_143() {
    if (jj_scan_token(AXIS_ANCESTOR)) return true;
    if (jj_la == 0 && jj_scanpos == jj_lastpos) return false;
    return false;
  }

  final private boolean jj_3R_142() {
    if (jj_scan_token(AXIS_PARENT)) return true;
    if (jj_la == 0 && jj_scanpos == jj_lastpos) return false;
    return false;
  }

  final private boolean jj_3R_15() {
    Token xsp;
    xsp = jj_scanpos;
    if (jj_3R_20()) {
    jj_scanpos = xsp;
    if (jj_3R_21()) {
    jj_scanpos = xsp;
    if (jj_3R_22()) {
    jj_scanpos = xsp;
    if (jj_3R_23()) {
    jj_scanpos = xsp;
    if (jj_3R_24()) {
    jj_scanpos = xsp;
    if (jj_3R_25()) {
    jj_scanpos = xsp;
    if (jj_3R_26()) {
    jj_scanpos = xsp;
    if (jj_3R_27()) {
    jj_scanpos = xsp;
    if (jj_3R_28()) {
    jj_scanpos = xsp;
    if (jj_3R_29()) {
    jj_scanpos = xsp;
    if (jj_3R_30()) {
    jj_scanpos = xsp;
    if (jj_3R_31()) {
    jj_scanpos = xsp;
    if (jj_3R_32()) {
    jj_scanpos = xsp;
    if (jj_3R_33()) {
    jj_scanpos = xsp;
    if (jj_3R_34()) {
    jj_scanpos = xsp;
    if (jj_3R_35()) {
    jj_scanpos = xsp;
    if (jj_3R_36()) {
    jj_scanpos = xsp;
    if (jj_3R_37()) {
    jj_scanpos = xsp;
    if (jj_3R_38()) {
    jj_scanpos = xsp;
    if (jj_3R_39()) {
    jj_scanpos = xsp;
    if (jj_3R_40()) {
    jj_scanpos = xsp;
    if (jj_3R_41()) {
    jj_scanpos = xsp;
    if (jj_3R_42()) {
    jj_scanpos = xsp;
    if (jj_3R_43()) {
    jj_scanpos = xsp;
    if (jj_3R_44()) {
    jj_scanpos = xsp;
    if (jj_3R_45()) {
    jj_scanpos = xsp;
    if (jj_3R_46()) {
    jj_scanpos = xsp;
    if (jj_3R_47()) {
    jj_scanpos = xsp;
    if (jj_3R_48()) {
    jj_scanpos = xsp;
    if (jj_3R_49()) {
    jj_scanpos = xsp;
    if (jj_3R_50()) {
    jj_scanpos = xsp;
    if (jj_3R_51()) {
    jj_scanpos = xsp;
    if (jj_3R_52()) {
    jj_scanpos = xsp;
    if (jj_3R_53()) {
    jj_scanpos = xsp;
    if (jj_3R_54()) return true;
    if (jj_la == 0 && jj_scanpos == jj_lastpos) return false;
    } else if (jj_la == 0 && jj_scanpos == jj_lastpos) return false;
    } else if (jj_la == 0 && jj_scanpos == jj_lastpos) return false;
    } else if (jj_la == 0 && jj_scanpos == jj_lastpos) return false;
    } else if (jj_la == 0 && jj_scanpos == jj_lastpos) return false;
    } else if (jj_la == 0 && jj_scanpos == jj_lastpos) return false;
    } else if (jj_la == 0 && jj_scanpos == jj_lastpos) return false;
    } else if (jj_la == 0 && jj_scanpos == jj_lastpos) return false;
    } else if (jj_la == 0 && jj_scanpos == jj_lastpos) return false;
    } else if (jj_la == 0 && jj_scanpos == jj_lastpos) return false;
    } else if (jj_la == 0 && jj_scanpos == jj_lastpos) return false;
    } else if (jj_la == 0 && jj_scanpos == jj_lastpos) return false;
    } else if (jj_la == 0 && jj_scanpos == jj_lastpos) return false;
    } else if (jj_la == 0 && jj_scanpos == jj_lastpos) return false;
    } else if (jj_la == 0 && jj_scanpos == jj_lastpos) return false;
    } else if (jj_la == 0 && jj_scanpos == jj_lastpos) return false;
    } else if (jj_la == 0 && jj_scanpos == jj_lastpos) return false;
    } else if (jj_la == 0 && jj_scanpos == jj_lastpos) return false;
    } else if (jj_la == 0 && jj_scanpos == jj_lastpos) return false;
    } else if (jj_la == 0 && jj_scanpos == jj_lastpos) return false;
    } else if (jj_la == 0 && jj_scanpos == jj_lastpos) return false;
    } else if (jj_la == 0 && jj_scanpos == jj_lastpos) return false;
    } else if (jj_la == 0 && jj_scanpos == jj_lastpos) return false;
    } else if (jj_la == 0 && jj_scanpos == jj_lastpos) return false;
    } else if (jj_la == 0 && jj_scanpos == jj_lastpos) return false;
    } else if (jj_la == 0 && jj_scanpos == jj_lastpos) return false;
    } else if (jj_la == 0 && jj_scanpos == jj_lastpos) return false;
    } else if (jj_la == 0 && jj_scanpos == jj_lastpos) return false;
    } else if (jj_la == 0 && jj_scanpos == jj_lastpos) return false;
    } else if (jj_la == 0 && jj_scanpos == jj_lastpos) return false;
    } else if (jj_la == 0 && jj_scanpos == jj_lastpos) return false;
    } else if (jj_la == 0 && jj_scanpos == jj_lastpos) return false;
    } else if (jj_la == 0 && jj_scanpos == jj_lastpos) return false;
    } else if (jj_la == 0 && jj_scanpos == jj_lastpos) return false;
    } else if (jj_la == 0 && jj_scanpos == jj_lastpos) return false;
    return false;
  }

  final private boolean jj_3R_141() {
    if (jj_scan_token(AXIS_CHILD)) return true;
    if (jj_la == 0 && jj_scanpos == jj_lastpos) return false;
    return false;
  }

  final private boolean jj_3R_140() {
    if (jj_scan_token(AXIS_SELF)) return true;
    if (jj_la == 0 && jj_scanpos == jj_lastpos) return false;
    return false;
  }

  final private boolean jj_3R_129() {
    Token xsp;
    xsp = jj_scanpos;
    if (jj_3R_140()) {
    jj_scanpos = xsp;
    if (jj_3R_141()) {
    jj_scanpos = xsp;
    if (jj_3R_142()) {
    jj_scanpos = xsp;
    if (jj_3R_143()) {
    jj_scanpos = xsp;
    if (jj_3R_144()) {
    jj_scanpos = xsp;
    if (jj_3R_145()) {
    jj_scanpos = xsp;
    if (jj_3R_146()) {
    jj_scanpos = xsp;
    if (jj_3R_147()) {
    jj_scanpos = xsp;
    if (jj_3R_148()) {
    jj_scanpos = xsp;
    if (jj_3R_149()) {
    jj_scanpos = xsp;
    if (jj_3R_150()) {
    jj_scanpos = xsp;
    if (jj_3R_151()) {
    jj_scanpos = xsp;
    if (jj_3R_152()) return true;
    if (jj_la == 0 && jj_scanpos == jj_lastpos) return false;
    } else if (jj_la == 0 && jj_scanpos == jj_lastpos) return false;
    } else if (jj_la == 0 && jj_scanpos == jj_lastpos) return false;
    } else if (jj_la == 0 && jj_scanpos == jj_lastpos) return false;
    } else if (jj_la == 0 && jj_scanpos == jj_lastpos) return false;
    } else if (jj_la == 0 && jj_scanpos == jj_lastpos) return false;
    } else if (jj_la == 0 && jj_scanpos == jj_lastpos) return false;
    } else if (jj_la == 0 && jj_scanpos == jj_lastpos) return false;
    } else if (jj_la == 0 && jj_scanpos == jj_lastpos) return false;
    } else if (jj_la == 0 && jj_scanpos == jj_lastpos) return false;
    } else if (jj_la == 0 && jj_scanpos == jj_lastpos) return false;
    } else if (jj_la == 0 && jj_scanpos == jj_lastpos) return false;
    } else if (jj_la == 0 && jj_scanpos == jj_lastpos) return false;
    return false;
  }

  final private boolean jj_3R_159() {
    Token xsp;
    xsp = jj_scanpos;
    if (jj_3R_164()) {
    jj_scanpos = xsp;
    if (jj_3R_165()) {
    jj_scanpos = xsp;
    if (jj_3R_166()) {
    jj_scanpos = xsp;
    if (jj_3R_167()) return true;
    if (jj_la == 0 && jj_scanpos == jj_lastpos) return false;
    } else if (jj_la == 0 && jj_scanpos == jj_lastpos) return false;
    } else if (jj_la == 0 && jj_scanpos == jj_lastpos) return false;
    } else if (jj_la == 0 && jj_scanpos == jj_lastpos) return false;
    return false;
  }

  final private boolean jj_3R_158() {
    if (jj_3R_162()) return true;
    if (jj_la == 0 && jj_scanpos == jj_lastpos) return false;
    Token xsp;
    while (true) {
      xsp = jj_scanpos;
      if (jj_3R_163()) { jj_scanpos = xsp; break; }
      if (jj_la == 0 && jj_scanpos == jj_lastpos) return false;
    }
    return false;
  }

  final private boolean jj_3R_122() {
    if (jj_3R_130()) return true;
    if (jj_la == 0 && jj_scanpos == jj_lastpos) return false;
    return false;
  }

  final private boolean jj_3R_121() {
    if (jj_3R_129()) return true;
    if (jj_la == 0 && jj_scanpos == jj_lastpos) return false;
    return false;
  }

  final private boolean jj_3R_167() {
    if (jj_scan_token(GTE)) return true;
    if (jj_la == 0 && jj_scanpos == jj_lastpos) return false;
    if (jj_3R_158()) return true;
    if (jj_la == 0 && jj_scanpos == jj_lastpos) return false;
    return false;
  }

  final private boolean jj_3R_112() {
    Token xsp;
    xsp = jj_scanpos;
    if (jj_3R_121()) {
    jj_scanpos = xsp;
    if (jj_3R_122()) return true;
    if (jj_la == 0 && jj_scanpos == jj_lastpos) return false;
    } else if (jj_la == 0 && jj_scanpos == jj_lastpos) return false;
    return false;
  }

  final private boolean jj_3R_166() {
    if (jj_scan_token(LTE)) return true;
    if (jj_la == 0 && jj_scanpos == jj_lastpos) return false;
    if (jj_3R_158()) return true;
    if (jj_la == 0 && jj_scanpos == jj_lastpos) return false;
    return false;
  }

  final private boolean jj_3R_165() {
    if (jj_scan_token(GT)) return true;
    if (jj_la == 0 && jj_scanpos == jj_lastpos) return false;
    if (jj_3R_158()) return true;
    if (jj_la == 0 && jj_scanpos == jj_lastpos) return false;
    return false;
  }

  final private boolean jj_3R_157() {
    Token xsp;
    xsp = jj_scanpos;
    if (jj_3R_160()) {
    jj_scanpos = xsp;
    if (jj_3R_161()) return true;
    if (jj_la == 0 && jj_scanpos == jj_lastpos) return false;
    } else if (jj_la == 0 && jj_scanpos == jj_lastpos) return false;
    return false;
  }

  final private boolean jj_3R_164() {
    if (jj_scan_token(LT)) return true;
    if (jj_la == 0 && jj_scanpos == jj_lastpos) return false;
    if (jj_3R_158()) return true;
    if (jj_la == 0 && jj_scanpos == jj_lastpos) return false;
    return false;
  }

  final private boolean jj_3R_156() {
    if (jj_3R_158()) return true;
    if (jj_la == 0 && jj_scanpos == jj_lastpos) return false;
    Token xsp;
    while (true) {
      xsp = jj_scanpos;
      if (jj_3R_159()) { jj_scanpos = xsp; break; }
      if (jj_la == 0 && jj_scanpos == jj_lastpos) return false;
    }
    return false;
  }

  final private boolean jj_3R_161() {
    if (jj_scan_token(NEQ)) return true;
    if (jj_la == 0 && jj_scanpos == jj_lastpos) return false;
    if (jj_3R_156()) return true;
    if (jj_la == 0 && jj_scanpos == jj_lastpos) return false;
    return false;
  }

  final private boolean jj_3R_160() {
    if (jj_scan_token(EQ)) return true;
    if (jj_la == 0 && jj_scanpos == jj_lastpos) return false;
    if (jj_3R_156()) return true;
    if (jj_la == 0 && jj_scanpos == jj_lastpos) return false;
    return false;
  }

  final private boolean jj_3R_102() {
    if (jj_3R_116()) return true;
    if (jj_la == 0 && jj_scanpos == jj_lastpos) return false;
    return false;
  }

  final private boolean jj_3_4() {
    if (jj_scan_token(PI)) return true;
    if (jj_la == 0 && jj_scanpos == jj_lastpos) return false;
    return false;
  }

  final private boolean jj_3_3() {
    if (jj_3R_17()) return true;
    if (jj_la == 0 && jj_scanpos == jj_lastpos) return false;
    if (jj_scan_token(80)) return true;
    if (jj_la == 0 && jj_scanpos == jj_lastpos) return false;
    if (jj_scan_token(81)) return true;
    if (jj_la == 0 && jj_scanpos == jj_lastpos) return false;
    return false;
  }

  final private boolean jj_3R_101() {
    if (jj_scan_token(83)) return true;
    if (jj_la == 0 && jj_scanpos == jj_lastpos) return false;
    return false;
  }

  final private boolean jj_3R_100() {
    if (jj_scan_token(82)) return true;
    if (jj_la == 0 && jj_scanpos == jj_lastpos) return false;
    return false;
  }

  final private boolean jj_3R_115() {
    if (jj_3R_123()) return true;
    if (jj_la == 0 && jj_scanpos == jj_lastpos) return false;
    return false;
  }

  final private boolean jj_3R_134() {
    if (jj_3R_156()) return true;
    if (jj_la == 0 && jj_scanpos == jj_lastpos) return false;
    Token xsp;
    while (true) {
      xsp = jj_scanpos;
      if (jj_3R_157()) { jj_scanpos = xsp; break; }
      if (jj_la == 0 && jj_scanpos == jj_lastpos) return false;
    }
    return false;
  }

  final private boolean jj_3R_114() {
    if (jj_scan_token(PI)) return true;
    if (jj_la == 0 && jj_scanpos == jj_lastpos) return false;
    if (jj_scan_token(80)) return true;
    if (jj_la == 0 && jj_scanpos == jj_lastpos) return false;
    if (jj_scan_token(Literal)) return true;
    if (jj_la == 0 && jj_scanpos == jj_lastpos) return false;
    if (jj_scan_token(81)) return true;
    if (jj_la == 0 && jj_scanpos == jj_lastpos) return false;
    return false;
  }

  final private boolean jj_3R_113() {
    if (jj_3R_17()) return true;
    if (jj_la == 0 && jj_scanpos == jj_lastpos) return false;
    if (jj_scan_token(80)) return true;
    if (jj_la == 0 && jj_scanpos == jj_lastpos) return false;
    if (jj_scan_token(81)) return true;
    if (jj_la == 0 && jj_scanpos == jj_lastpos) return false;
    return false;
  }

  final private boolean jj_3R_99() {
    if (jj_3R_112()) return true;
    if (jj_la == 0 && jj_scanpos == jj_lastpos) return false;
    Token xsp;
    xsp = jj_scanpos;
    if (jj_3R_113()) {
    jj_scanpos = xsp;
    if (jj_3R_114()) {
    jj_scanpos = xsp;
    if (jj_3R_115()) return true;
    if (jj_la == 0 && jj_scanpos == jj_lastpos) return false;
    } else if (jj_la == 0 && jj_scanpos == jj_lastpos) return false;
    } else if (jj_la == 0 && jj_scanpos == jj_lastpos) return false;
    return false;
  }

  final private boolean jj_3R_135() {
    if (jj_scan_token(AND)) return true;
    if (jj_la == 0 && jj_scanpos == jj_lastpos) return false;
    if (jj_3R_134()) return true;
    if (jj_la == 0 && jj_scanpos == jj_lastpos) return false;
    return false;
  }

  final private boolean jj_3R_57() {
    Token xsp;
    xsp = jj_scanpos;
    if (jj_3R_99()) {
    jj_scanpos = xsp;
    if (jj_3R_100()) {
    jj_scanpos = xsp;
    if (jj_3R_101()) return true;
    if (jj_la == 0 && jj_scanpos == jj_lastpos) return false;
    } else if (jj_la == 0 && jj_scanpos == jj_lastpos) return false;
    } else if (jj_la == 0 && jj_scanpos == jj_lastpos) return false;
    while (true) {
      xsp = jj_scanpos;
      if (jj_3R_102()) { jj_scanpos = xsp; break; }
      if (jj_la == 0 && jj_scanpos == jj_lastpos) return false;
    }
    return false;
  }

  final private boolean jj_3R_125() {
    if (jj_3R_134()) return true;
    if (jj_la == 0 && jj_scanpos == jj_lastpos) return false;
    Token xsp;
    while (true) {
      xsp = jj_scanpos;
      if (jj_3R_135()) { jj_scanpos = xsp; break; }
      if (jj_la == 0 && jj_scanpos == jj_lastpos) return false;
    }
    return false;
  }

  final private boolean jj_3R_193() {
    if (jj_3R_16()) return true;
    if (jj_la == 0 && jj_scanpos == jj_lastpos) return false;
    return false;
  }

  final private boolean jj_3R_56() {
    if (jj_scan_token(SLASHSLASH)) return true;
    if (jj_la == 0 && jj_scanpos == jj_lastpos) return false;
    return false;
  }

  final private boolean jj_3R_55() {
    if (jj_scan_token(SLASH)) return true;
    if (jj_la == 0 && jj_scanpos == jj_lastpos) return false;
    return false;
  }

  final private boolean jj_3R_126() {
    if (jj_scan_token(OR)) return true;
    if (jj_la == 0 && jj_scanpos == jj_lastpos) return false;
    if (jj_3R_125()) return true;
    if (jj_la == 0 && jj_scanpos == jj_lastpos) return false;
    return false;
  }

  final private boolean jj_3R_16() {
    Token xsp;
    xsp = jj_scanpos;
    if (jj_3R_55()) {
    jj_scanpos = xsp;
    if (jj_3R_56()) return true;
    if (jj_la == 0 && jj_scanpos == jj_lastpos) return false;
    } else if (jj_la == 0 && jj_scanpos == jj_lastpos) return false;
    if (jj_3R_57()) return true;
    if (jj_la == 0 && jj_scanpos == jj_lastpos) return false;
    return false;
  }

  final private boolean jj_3R_190() {
    if (jj_3R_16()) return true;
    if (jj_la == 0 && jj_scanpos == jj_lastpos) return false;
    return false;
  }

  final private boolean jj_3R_118() {
    if (jj_3R_125()) return true;
    if (jj_la == 0 && jj_scanpos == jj_lastpos) return false;
    Token xsp;
    while (true) {
      xsp = jj_scanpos;
      if (jj_3R_126()) { jj_scanpos = xsp; break; }
      if (jj_la == 0 && jj_scanpos == jj_lastpos) return false;
    }
    return false;
  }

  final private boolean jj_3R_188() {
    if (jj_3R_57()) return true;
    if (jj_la == 0 && jj_scanpos == jj_lastpos) return false;
    Token xsp;
    while (true) {
      xsp = jj_scanpos;
      if (jj_3R_190()) { jj_scanpos = xsp; break; }
      if (jj_la == 0 && jj_scanpos == jj_lastpos) return false;
    }
    return false;
  }

  final private boolean jj_3_2() {
    if (jj_3R_16()) return true;
    if (jj_la == 0 && jj_scanpos == jj_lastpos) return false;
    return false;
  }

  final private boolean jj_3R_185() {
    if (jj_3R_16()) return true;
    if (jj_la == 0 && jj_scanpos == jj_lastpos) return false;
    return false;
  }

  final private boolean jj_3R_192() {
    if (jj_scan_token(SLASH)) return true;
    if (jj_la == 0 && jj_scanpos == jj_lastpos) return false;
    return false;
  }

  final private boolean jj_3R_191() {
    if (jj_3R_16()) return true;
    if (jj_la == 0 && jj_scanpos == jj_lastpos) return false;
    Token xsp;
    while (true) {
      xsp = jj_scanpos;
      if (jj_3R_193()) { jj_scanpos = xsp; break; }
      if (jj_la == 0 && jj_scanpos == jj_lastpos) return false;
    }
    return false;
  }

  final private boolean jj_3R_184() {
    if (jj_3R_116()) return true;
    if (jj_la == 0 && jj_scanpos == jj_lastpos) return false;
    return false;
  }

  final private boolean jj_3R_189() {
    Token xsp;
    xsp = jj_scanpos;
    if (jj_3R_191()) {
    jj_scanpos = xsp;
    if (jj_3R_192()) return true;
    if (jj_la == 0 && jj_scanpos == jj_lastpos) return false;
    } else if (jj_la == 0 && jj_scanpos == jj_lastpos) return false;
    return false;
  }

  final private boolean jj_3R_182() {
    if (jj_3R_19()) return true;
    if (jj_la == 0 && jj_scanpos == jj_lastpos) return false;
    Token xsp;
    while (true) {
      xsp = jj_scanpos;
      if (jj_3R_184()) { jj_scanpos = xsp; break; }
      if (jj_la == 0 && jj_scanpos == jj_lastpos) return false;
    }
    while (true) {
      xsp = jj_scanpos;
      if (jj_3R_185()) { jj_scanpos = xsp; break; }
      if (jj_la == 0 && jj_scanpos == jj_lastpos) return false;
    }
    return false;
  }

  final private boolean jj_3_6() {
    if (jj_3R_19()) return true;
    if (jj_la == 0 && jj_scanpos == jj_lastpos) return false;
    return false;
  }

  final private boolean jj_3R_187() {
    if (jj_3R_189()) return true;
    if (jj_la == 0 && jj_scanpos == jj_lastpos) return false;
    return false;
  }

  final private boolean jj_3R_186() {
    if (jj_3R_188()) return true;
    if (jj_la == 0 && jj_scanpos == jj_lastpos) return false;
    return false;
  }

  final private boolean jj_3R_183() {
    Token xsp;
    xsp = jj_scanpos;
    if (jj_3R_186()) {
    jj_scanpos = xsp;
    if (jj_3R_187()) return true;
    if (jj_la == 0 && jj_scanpos == jj_lastpos) return false;
    } else if (jj_la == 0 && jj_scanpos == jj_lastpos) return false;
    return false;
  }

  final private boolean jj_3R_181() {
    if (jj_3R_183()) return true;
    if (jj_la == 0 && jj_scanpos == jj_lastpos) return false;
    return false;
  }

  final private boolean jj_3R_180() {
    if (jj_3R_182()) return true;
    if (jj_la == 0 && jj_scanpos == jj_lastpos) return false;
    return false;
  }

  final private boolean jj_3R_178() {
    Token xsp;
    xsp = jj_scanpos;
    if (jj_3R_180()) {
    jj_scanpos = xsp;
    if (jj_3R_181()) return true;
    if (jj_la == 0 && jj_scanpos == jj_lastpos) return false;
    } else if (jj_la == 0 && jj_scanpos == jj_lastpos) return false;
    return false;
  }

  final private boolean jj_3R_179() {
    if (jj_scan_token(UNION)) return true;
    if (jj_la == 0 && jj_scanpos == jj_lastpos) return false;
    if (jj_3R_178()) return true;
    if (jj_la == 0 && jj_scanpos == jj_lastpos) return false;
    return false;
  }

  final private boolean jj_3R_177() {
    if (jj_3R_178()) return true;
    if (jj_la == 0 && jj_scanpos == jj_lastpos) return false;
    Token xsp;
    while (true) {
      xsp = jj_scanpos;
      if (jj_3R_179()) { jj_scanpos = xsp; break; }
      if (jj_la == 0 && jj_scanpos == jj_lastpos) return false;
    }
    return false;
  }

  final private boolean jj_3R_136() {
    if (jj_3R_104()) return true;
    if (jj_la == 0 && jj_scanpos == jj_lastpos) return false;
    return false;
  }

  final private boolean jj_3R_137() {
    if (jj_scan_token(87)) return true;
    if (jj_la == 0 && jj_scanpos == jj_lastpos) return false;
    if (jj_3R_136()) return true;
    if (jj_la == 0 && jj_scanpos == jj_lastpos) return false;
    return false;
  }

  final private boolean jj_3R_127() {
    if (jj_3R_136()) return true;
    if (jj_la == 0 && jj_scanpos == jj_lastpos) return false;
    Token xsp;
    while (true) {
      xsp = jj_scanpos;
      if (jj_3R_137()) { jj_scanpos = xsp; break; }
      if (jj_la == 0 && jj_scanpos == jj_lastpos) return false;
    }
    return false;
  }

  final private boolean jj_3R_119() {
    if (jj_scan_token(80)) return true;
    if (jj_la == 0 && jj_scanpos == jj_lastpos) return false;
    Token xsp;
    xsp = jj_scanpos;
    if (jj_3R_127()) jj_scanpos = xsp;
    else if (jj_la == 0 && jj_scanpos == jj_lastpos) return false;
    if (jj_scan_token(81)) return true;
    if (jj_la == 0 && jj_scanpos == jj_lastpos) return false;
    return false;
  }

  final private boolean jj_3R_61() {
    if (jj_scan_token(PI)) return true;
    if (jj_la == 0 && jj_scanpos == jj_lastpos) return false;
    return false;
  }

  final private boolean jj_3R_60() {
    if (jj_scan_token(COMMENT)) return true;
    if (jj_la == 0 && jj_scanpos == jj_lastpos) return false;
    return false;
  }

  final private boolean jj_3R_59() {
    if (jj_scan_token(NODE)) return true;
    if (jj_la == 0 && jj_scanpos == jj_lastpos) return false;
    return false;
  }

  final private boolean jj_3R_58() {
    if (jj_scan_token(TEXT)) return true;
    if (jj_la == 0 && jj_scanpos == jj_lastpos) return false;
    return false;
  }

  final private boolean jj_3_1() {
    if (jj_3R_15()) return true;
    if (jj_la == 0 && jj_scanpos == jj_lastpos) return false;
    if (jj_scan_token(79)) return true;
    if (jj_la == 0 && jj_scanpos == jj_lastpos) return false;
    return false;
  }

  final private boolean jj_3R_17() {
    Token xsp;
    xsp = jj_scanpos;
    if (jj_3R_58()) {
    jj_scanpos = xsp;
    if (jj_3R_59()) {
    jj_scanpos = xsp;
    if (jj_3R_60()) {
    jj_scanpos = xsp;
    if (jj_3R_61()) return true;
    if (jj_la == 0 && jj_scanpos == jj_lastpos) return false;
    } else if (jj_la == 0 && jj_scanpos == jj_lastpos) return false;
    } else if (jj_la == 0 && jj_scanpos == jj_lastpos) return false;
    } else if (jj_la == 0 && jj_scanpos == jj_lastpos) return false;
    return false;
  }

  final private boolean jj_3R_155() {
    if (jj_3R_15()) return true;
    if (jj_la == 0 && jj_scanpos == jj_lastpos) return false;
    return false;
  }

  final private boolean jj_3R_139() {
    if (jj_3R_98()) return true;
    if (jj_la == 0 && jj_scanpos == jj_lastpos) return false;
    return false;
  }

  final private boolean jj_3R_138() {
    if (jj_3R_15()) return true;
    if (jj_la == 0 && jj_scanpos == jj_lastpos) return false;
    if (jj_scan_token(79)) return true;
    if (jj_la == 0 && jj_scanpos == jj_lastpos) return false;
    if (jj_3R_15()) return true;
    if (jj_la == 0 && jj_scanpos == jj_lastpos) return false;
    return false;
  }

  final private boolean jj_3R_154() {
    if (jj_scan_token(88)) return true;
    if (jj_la == 0 && jj_scanpos == jj_lastpos) return false;
    return false;
  }

  final private boolean jj_3R_132() {
    if (jj_3R_15()) return true;
    if (jj_la == 0 && jj_scanpos == jj_lastpos) return false;
    return false;
  }

  final private boolean jj_3R_105() {
    if (jj_3R_18()) return true;
    if (jj_la == 0 && jj_scanpos == jj_lastpos) return false;
    if (jj_3R_119()) return true;
    if (jj_la == 0 && jj_scanpos == jj_lastpos) return false;
    return false;
  }

  final private boolean jj_3R_124() {
    if (jj_scan_token(79)) return true;
    if (jj_la == 0 && jj_scanpos == jj_lastpos) return false;
    if (jj_3R_15()) return true;
    if (jj_la == 0 && jj_scanpos == jj_lastpos) return false;
    return false;
  }

  final private boolean jj_3R_133() {
    if (jj_scan_token(79)) return true;
    if (jj_la == 0 && jj_scanpos == jj_lastpos) return false;
    Token xsp;
    xsp = jj_scanpos;
    if (jj_3R_154()) {
    jj_scanpos = xsp;
    if (jj_3R_155()) return true;
    if (jj_la == 0 && jj_scanpos == jj_lastpos) return false;
    } else if (jj_la == 0 && jj_scanpos == jj_lastpos) return false;
    return false;
  }

  final private boolean jj_3R_128() {
    Token xsp;
    xsp = jj_scanpos;
    if (jj_3R_138()) {
    jj_scanpos = xsp;
    if (jj_3R_139()) return true;
    if (jj_la == 0 && jj_scanpos == jj_lastpos) return false;
    } else if (jj_la == 0 && jj_scanpos == jj_lastpos) return false;
    return false;
  }

  final private boolean jj_3R_131() {
    if (jj_scan_token(88)) return true;
    if (jj_la == 0 && jj_scanpos == jj_lastpos) return false;
    return false;
  }

  final private boolean jj_3R_123() {
    Token xsp;
    xsp = jj_scanpos;
    if (jj_3R_131()) {
    jj_scanpos = xsp;
    if (jj_3R_132()) return true;
    if (jj_la == 0 && jj_scanpos == jj_lastpos) return false;
    } else if (jj_la == 0 && jj_scanpos == jj_lastpos) return false;
    xsp = jj_scanpos;
    if (jj_3R_133()) jj_scanpos = xsp;
    else if (jj_la == 0 && jj_scanpos == jj_lastpos) return false;
    return false;
  }

  final private boolean jj_3R_106() {
    if (jj_3R_120()) return true;
    if (jj_la == 0 && jj_scanpos == jj_lastpos) return false;
    if (jj_3R_119()) return true;
    if (jj_la == 0 && jj_scanpos == jj_lastpos) return false;
    return false;
  }

  final private boolean jj_3R_117() {
    if (jj_3R_15()) return true;
    if (jj_la == 0 && jj_scanpos == jj_lastpos) return false;
    Token xsp;
    xsp = jj_scanpos;
    if (jj_3R_124()) jj_scanpos = xsp;
    else if (jj_la == 0 && jj_scanpos == jj_lastpos) return false;
    return false;
  }

  final private boolean jj_3_5() {
    if (jj_3R_18()) return true;
    if (jj_la == 0 && jj_scanpos == jj_lastpos) return false;
    if (jj_scan_token(80)) return true;
    if (jj_la == 0 && jj_scanpos == jj_lastpos) return false;
    return false;
  }

  final private boolean jj_3R_103() {
    if (jj_scan_token(VARIABLE)) return true;
    if (jj_la == 0 && jj_scanpos == jj_lastpos) return false;
    if (jj_3R_117()) return true;
    if (jj_la == 0 && jj_scanpos == jj_lastpos) return false;
    return false;
  }

  final private boolean jj_3R_91() {
    if (jj_scan_token(FUNCTION_FORMAT_NUMBER)) return true;
    if (jj_la == 0 && jj_scanpos == jj_lastpos) return false;
    return false;
  }

  final private boolean jj_3R_90() {
    if (jj_scan_token(FUNCTION_KEY)) return true;
    if (jj_la == 0 && jj_scanpos == jj_lastpos) return false;
    return false;
  }

  final private boolean jj_3R_89() {
    if (jj_scan_token(FUNCTION_ROUND)) return true;
    if (jj_la == 0 && jj_scanpos == jj_lastpos) return false;
    return false;
  }

  final private boolean jj_3R_88() {
    if (jj_scan_token(FUNCTION_CEILING)) return true;
    if (jj_la == 0 && jj_scanpos == jj_lastpos) return false;
    return false;
  }

  final private boolean jj_3R_97() {
    if (jj_3R_106()) return true;
    if (jj_la == 0 && jj_scanpos == jj_lastpos) return false;
    return false;
  }

  final private boolean jj_3R_87() {
    if (jj_scan_token(FUNCTION_FLOOR)) return true;
    if (jj_la == 0 && jj_scanpos == jj_lastpos) return false;
    return false;
  }

  final private boolean jj_3R_96() {
    if (jj_3R_105()) return true;
    if (jj_la == 0 && jj_scanpos == jj_lastpos) return false;
    return false;
  }

  final private boolean jj_3R_86() {
    if (jj_scan_token(FUNCTION_SUM)) return true;
    if (jj_la == 0 && jj_scanpos == jj_lastpos) return false;
    return false;
  }

  final private boolean jj_3R_95() {
    if (jj_scan_token(Number)) return true;
    if (jj_la == 0 && jj_scanpos == jj_lastpos) return false;
    return false;
  }

  final private boolean jj_3R_85() {
    if (jj_scan_token(FUNCTION_NUMBER)) return true;
    if (jj_la == 0 && jj_scanpos == jj_lastpos) return false;
    return false;
  }

  final private boolean jj_3R_94() {
    if (jj_scan_token(Literal)) return true;
    if (jj_la == 0 && jj_scanpos == jj_lastpos) return false;
    return false;
  }

  final private boolean jj_3R_84() {
    if (jj_scan_token(FUNCTION_LANG)) return true;
    if (jj_la == 0 && jj_scanpos == jj_lastpos) return false;
    return false;
  }

  final private boolean jj_3R_93() {
    if (jj_scan_token(80)) return true;
    if (jj_la == 0 && jj_scanpos == jj_lastpos) return false;
    if (jj_3R_104()) return true;
    if (jj_la == 0 && jj_scanpos == jj_lastpos) return false;
    if (jj_scan_token(81)) return true;
    if (jj_la == 0 && jj_scanpos == jj_lastpos) return false;
    return false;
  }

  final private boolean jj_3R_83() {
    if (jj_scan_token(FUNCTION_NULL)) return true;
    if (jj_la == 0 && jj_scanpos == jj_lastpos) return false;
    return false;
  }

  final private boolean jj_3R_92() {
    if (jj_3R_103()) return true;
    if (jj_la == 0 && jj_scanpos == jj_lastpos) return false;
    return false;
  }

  final private boolean jj_3R_82() {
    if (jj_scan_token(FUNCTION_FALSE)) return true;
    if (jj_la == 0 && jj_scanpos == jj_lastpos) return false;
    return false;
  }

  final private boolean jj_3R_81() {
    if (jj_scan_token(FUNCTION_TRUE)) return true;
    if (jj_la == 0 && jj_scanpos == jj_lastpos) return false;
    return false;
  }

  final private boolean jj_3R_120() {
    if (jj_3R_128()) return true;
    if (jj_la == 0 && jj_scanpos == jj_lastpos) return false;
    return false;
  }

  final private boolean jj_3R_80() {
    if (jj_scan_token(FUNCTION_NOT)) return true;
    if (jj_la == 0 && jj_scanpos == jj_lastpos) return false;
    return false;
  }

  final private boolean jj_3R_79() {
    if (jj_scan_token(FUNCTION_BOOLEAN)) return true;
    if (jj_la == 0 && jj_scanpos == jj_lastpos) return false;
    return false;
  }

  final private boolean jj_3R_78() {
    if (jj_scan_token(FUNCTION_TRANSLATE)) return true;
    if (jj_la == 0 && jj_scanpos == jj_lastpos) return false;
    return false;
  }

  final private boolean jj_3R_77() {
    if (jj_scan_token(FUNCTION_NORMALIZE_SPACE)) return true;
    if (jj_la == 0 && jj_scanpos == jj_lastpos) return false;
    return false;
  }

  final private boolean jj_3R_19() {
    Token xsp;
    xsp = jj_scanpos;
    if (jj_3R_92()) {
    jj_scanpos = xsp;
    if (jj_3R_93()) {
    jj_scanpos = xsp;
    if (jj_3R_94()) {
    jj_scanpos = xsp;
    if (jj_3R_95()) {
    jj_scanpos = xsp;
    if (jj_3R_96()) {
    jj_scanpos = xsp;
    if (jj_3R_97()) return true;
    if (jj_la == 0 && jj_scanpos == jj_lastpos) return false;
    } else if (jj_la == 0 && jj_scanpos == jj_lastpos) return false;
    } else if (jj_la == 0 && jj_scanpos == jj_lastpos) return false;
    } else if (jj_la == 0 && jj_scanpos == jj_lastpos) return false;
    } else if (jj_la == 0 && jj_scanpos == jj_lastpos) return false;
    } else if (jj_la == 0 && jj_scanpos == jj_lastpos) return false;
    return false;
  }

  final private boolean jj_3R_76() {
    if (jj_scan_token(FUNCTION_STRING_LENGTH)) return true;
    if (jj_la == 0 && jj_scanpos == jj_lastpos) return false;
    return false;
  }

  final private boolean jj_3R_75() {
    if (jj_scan_token(FUNCTION_SUBSTRING)) return true;
    if (jj_la == 0 && jj_scanpos == jj_lastpos) return false;
    return false;
  }

  final private boolean jj_3R_74() {
    if (jj_scan_token(FUNCTION_SUBSTRING_AFTER)) return true;
    if (jj_la == 0 && jj_scanpos == jj_lastpos) return false;
    return false;
  }

  final private boolean jj_3R_73() {
    if (jj_scan_token(FUNCTION_SUBSTRING_BEFORE)) return true;
    if (jj_la == 0 && jj_scanpos == jj_lastpos) return false;
    return false;
  }

  final private boolean jj_3R_72() {
    if (jj_scan_token(FUNCTION_CONTAINS)) return true;
    if (jj_la == 0 && jj_scanpos == jj_lastpos) return false;
    return false;
  }

  final private boolean jj_3R_71() {
    if (jj_scan_token(FUNCTION_STARTS_WITH)) return true;
    if (jj_la == 0 && jj_scanpos == jj_lastpos) return false;
    return false;
  }

  final private boolean jj_3R_70() {
    if (jj_scan_token(FUNCTION_CONCAT)) return true;
    if (jj_la == 0 && jj_scanpos == jj_lastpos) return false;
    return false;
  }

  final private boolean jj_3R_69() {
    if (jj_scan_token(FUNCTION_STRING)) return true;
    if (jj_la == 0 && jj_scanpos == jj_lastpos) return false;
    return false;
  }

  final private boolean jj_3R_68() {
    if (jj_scan_token(FUNCTION_NAME)) return true;
    if (jj_la == 0 && jj_scanpos == jj_lastpos) return false;
    return false;
  }

  final private boolean jj_3R_67() {
    if (jj_scan_token(FUNCTION_NAMESPACE_URI)) return true;
    if (jj_la == 0 && jj_scanpos == jj_lastpos) return false;
    return false;
  }

  final private boolean jj_3R_66() {
    if (jj_scan_token(FUNCTION_LOCAL_NAME)) return true;
    if (jj_la == 0 && jj_scanpos == jj_lastpos) return false;
    return false;
  }

  public XPathParserTokenManager token_source;
  SimpleCharStream jj_input_stream;
  public Token token, jj_nt;
  private Token jj_scanpos, jj_lastpos;
  private int jj_la;
  public boolean lookingAhead = false;
  private boolean jj_semLA;
  private int jj_gen;
  final private int[] jj_la1 = new int[39];
  static private int[] jj_la1_0;
  static private int[] jj_la1_1;
  static private int[] jj_la1_2;
  static {
      jj_la1_0();
      jj_la1_1();
      jj_la1_2();
   }
   private static void jj_la1_0() {
      jj_la1_0 = new int[] {0xf8000000,0x78000000,0x0,0x0,0x78000000,0xf80000c0,0xc0,0x40,0xc0,0xc0,0xf8000000,0xf8000000,0x0,0x0,0x0,0x0,0x160000,0xf8000000,0x0,0xf81604c0,0x100,0xf80000c0,0x0,0xc0,0x8000000,0x10000000,0x1800,0x1800,0x1e000,0x1e000,0x200,0x400,0x60000000,0x60000000,0xf81604c0,0xf8000000,0xf8000000,0x0,0x80000000,};
   }
   private static void jj_la1_1() {
      jj_la1_1 = new int[] {0xffff0007,0x0,0xffff0000,0x0,0x0,0xffffffff,0x0,0x0,0x0,0x0,0xffff0007,0xffffffff,0x0,0xfff8,0xfff8,0x0,0x0,0xffff0007,0x0,0xffffffff,0x0,0xffffffff,0x0,0x0,0x0,0x0,0x0,0x0,0x0,0x0,0x0,0x0,0x0,0x0,0xffffffff,0xffff0007,0xffff0007,0x0,0x7,};
   }
   private static void jj_la1_2() {
      jj_la1_2 = new int[] {0x7fff,0x4000,0x3fff,0x8000,0x4000,0x14c7fff,0x0,0x0,0x0,0x0,0x1007fff,0x14c7fff,0x100000,0x0,0x0,0x400000,0x10000,0x7fff,0x800000,0x14d7fff,0x0,0x14c7fff,0x100000,0x0,0x0,0x0,0x0,0x0,0x0,0x0,0x0,0x0,0x1000000,0x1000000,0x14d7fff,0x1007fff,0x1007fff,0x8000,0x0,};
   }
  final private JJCalls[] jj_2_rtns = new JJCalls[6];
  private boolean jj_rescan = false;
  private int jj_gc = 0;

  public XPathParser(java.io.InputStream stream) {
    jj_input_stream = new SimpleCharStream(stream, 1, 1);
    token_source = new XPathParserTokenManager(jj_input_stream);
    token = new Token();
    token.next = jj_nt = token_source.getNextToken();
    jj_gen = 0;
    for (int i = 0; i < 39; i++) jj_la1[i] = -1;
    for (int i = 0; i < jj_2_rtns.length; i++) jj_2_rtns[i] = new JJCalls();
  }

  public void ReInit(java.io.InputStream stream) {
    jj_input_stream.ReInit(stream, 1, 1);
    token_source.ReInit(jj_input_stream);
    token = new Token();
    token.next = jj_nt = token_source.getNextToken();
    jj_gen = 0;
    for (int i = 0; i < 39; i++) jj_la1[i] = -1;
    for (int i = 0; i < jj_2_rtns.length; i++) jj_2_rtns[i] = new JJCalls();
  }

  public XPathParser(java.io.Reader stream) {
    jj_input_stream = new SimpleCharStream(stream, 1, 1);
    token_source = new XPathParserTokenManager(jj_input_stream);
    token = new Token();
    token.next = jj_nt = token_source.getNextToken();
    jj_gen = 0;
    for (int i = 0; i < 39; i++) jj_la1[i] = -1;
    for (int i = 0; i < jj_2_rtns.length; i++) jj_2_rtns[i] = new JJCalls();
  }

  public void ReInit(java.io.Reader stream) {
    jj_input_stream.ReInit(stream, 1, 1);
    token_source.ReInit(jj_input_stream);
    token = new Token();
    token.next = jj_nt = token_source.getNextToken();
    jj_gen = 0;
    for (int i = 0; i < 39; i++) jj_la1[i] = -1;
    for (int i = 0; i < jj_2_rtns.length; i++) jj_2_rtns[i] = new JJCalls();
  }

  public XPathParser(XPathParserTokenManager tm) {
    token_source = tm;
    token = new Token();
    token.next = jj_nt = token_source.getNextToken();
    jj_gen = 0;
    for (int i = 0; i < 39; i++) jj_la1[i] = -1;
    for (int i = 0; i < jj_2_rtns.length; i++) jj_2_rtns[i] = new JJCalls();
  }

  public void ReInit(XPathParserTokenManager tm) {
    token_source = tm;
    token = new Token();
    token.next = jj_nt = token_source.getNextToken();
    jj_gen = 0;
    for (int i = 0; i < 39; i++) jj_la1[i] = -1;
    for (int i = 0; i < jj_2_rtns.length; i++) jj_2_rtns[i] = new JJCalls();
  }

  final private Token jj_consume_token(int kind) throws ParseException {
    Token oldToken = token;
    if ((token = jj_nt).next != null) jj_nt = jj_nt.next;
    else jj_nt = jj_nt.next = token_source.getNextToken();
    if (token.kind == kind) {
      jj_gen++;
      if (++jj_gc > 100) {
        jj_gc = 0;
        for (int i = 0; i < jj_2_rtns.length; i++) {
          JJCalls c = jj_2_rtns[i];
          while (c != null) {
            if (c.gen < jj_gen) c.first = null;
            c = c.next;
          }
        }
      }
      return token;
    }
    jj_nt = token;
    token = oldToken;
    jj_kind = kind;
    throw generateParseException();
  }

  final private boolean jj_scan_token(int kind) {
    if (jj_scanpos == jj_lastpos) {
      jj_la--;
      if (jj_scanpos.next == null) {
        jj_lastpos = jj_scanpos = jj_scanpos.next = token_source.getNextToken();
      } else {
        jj_lastpos = jj_scanpos = jj_scanpos.next;
      }
    } else {
      jj_scanpos = jj_scanpos.next;
    }
    if (jj_rescan) {
      int i = 0; Token tok = token;
      while (tok != null && tok != jj_scanpos) { i++; tok = tok.next; }
      if (tok != null) jj_add_error_token(kind, i);
    }
    return (jj_scanpos.kind != kind);
  }

  final public Token getNextToken() {
    if ((token = jj_nt).next != null) jj_nt = jj_nt.next;
    else jj_nt = jj_nt.next = token_source.getNextToken();
    jj_gen++;
    return token;
  }

  final public Token getToken(int index) {
    Token t = lookingAhead ? jj_scanpos : token;
    for (int i = 0; i < index; i++) {
      if (t.next != null) t = t.next;
      else t = t.next = token_source.getNextToken();
    }
    return t;
  }

  private java.util.Vector jj_expentries = new java.util.Vector();
  private int[] jj_expentry;
  private int jj_kind = -1;
  private int[] jj_lasttokens = new int[100];
  private int jj_endpos;

  private void jj_add_error_token(int kind, int pos) {
    if (pos >= 100) return;
    if (pos == jj_endpos + 1) {
      jj_lasttokens[jj_endpos++] = kind;
    } else if (jj_endpos != 0) {
      jj_expentry = new int[jj_endpos];
      for (int i = 0; i < jj_endpos; i++) {
        jj_expentry[i] = jj_lasttokens[i];
      }
      boolean exists = false;
      for (java.util.Enumeration enum = jj_expentries.elements(); enum.hasMoreElements();) {
        int[] oldentry = (int[])(enum.nextElement());
        if (oldentry.length == jj_expentry.length) {
          exists = true;
          for (int i = 0; i < jj_expentry.length; i++) {
            if (oldentry[i] != jj_expentry[i]) {
              exists = false;
              break;
            }
          }
          if (exists) break;
        }
      }
      if (!exists) jj_expentries.addElement(jj_expentry);
      if (pos != 0) jj_lasttokens[(jj_endpos = pos) - 1] = kind;
    }
  }

  public ParseException generateParseException() {
    jj_expentries.removeAllElements();
    boolean[] la1tokens = new boolean[89];
    for (int i = 0; i < 89; i++) {
      la1tokens[i] = false;
    }
    if (jj_kind >= 0) {
      la1tokens[jj_kind] = true;
      jj_kind = -1;
    }
    for (int i = 0; i < 39; i++) {
      if (jj_la1[i] == jj_gen) {
        for (int j = 0; j < 32; j++) {
          if ((jj_la1_0[i] & (1<<j)) != 0) {
            la1tokens[j] = true;
          }
          if ((jj_la1_1[i] & (1<<j)) != 0) {
            la1tokens[32+j] = true;
          }
          if ((jj_la1_2[i] & (1<<j)) != 0) {
            la1tokens[64+j] = true;
          }
        }
      }
    }
    for (int i = 0; i < 89; i++) {
      if (la1tokens[i]) {
        jj_expentry = new int[1];
        jj_expentry[0] = i;
        jj_expentries.addElement(jj_expentry);
      }
    }
    jj_endpos = 0;
    jj_rescan_token();
    jj_add_error_token(0, 0);
    int[][] exptokseq = new int[jj_expentries.size()][];
    for (int i = 0; i < jj_expentries.size(); i++) {
      exptokseq[i] = (int[])jj_expentries.elementAt(i);
    }
    return new ParseException(token, exptokseq, tokenImage);
  }

  final public void enable_tracing() {
  }

  final public void disable_tracing() {
  }

  final private void jj_rescan_token() {
    jj_rescan = true;
    for (int i = 0; i < 6; i++) {
      JJCalls p = jj_2_rtns[i];
      do {
        if (p.gen > jj_gen) {
          jj_la = p.arg; jj_lastpos = jj_scanpos = p.first;
          switch (i) {
            case 0: jj_3_1(); break;
            case 1: jj_3_2(); break;
            case 2: jj_3_3(); break;
            case 3: jj_3_4(); break;
            case 4: jj_3_5(); break;
            case 5: jj_3_6(); break;
          }
        }
        p = p.next;
      } while (p != null);
    }
    jj_rescan = false;
  }

  final private void jj_save(int index, int xla) {
    JJCalls p = jj_2_rtns[index];
    while (p.gen > jj_gen) {
      if (p.next == null) { p = p.next = new JJCalls(); break; }
      p = p.next;
    }
    p.gen = jj_gen + xla - jj_la; p.first = token; p.arg = xla;
  }

  static final class JJCalls {
    int gen;
    Token first;
    int arg;
    JJCalls next;
  }

    }
