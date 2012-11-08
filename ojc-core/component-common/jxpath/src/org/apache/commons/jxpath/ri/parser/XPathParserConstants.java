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
 * @(#)XPathParserConstants.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package org.apache.commons.jxpath.ri.parser;

public interface XPathParserConstants {

  int EOF = 0;
  int SLASH = 6;
  int SLASHSLASH = 7;
  int UNION = 8;
  int PLUS = 9;
  int MINUS = 10;
  int EQ = 11;
  int NEQ = 12;
  int LT = 13;
  int LTE = 14;
  int GT = 15;
  int GTE = 16;
  int VARIABLE = 17;
  int Literal = 18;
  int Digit = 19;
  int Number = 20;
  int Letter = 21;
  int BaseChar = 22;
  int Ideographic = 23;
  int CombiningChar = 24;
  int UnicodeDigit = 25;
  int Extender = 26;
  int OR = 27;
  int AND = 28;
  int MOD = 29;
  int DIV = 30;
  int NODE = 31;
  int TEXT = 32;
  int COMMENT = 33;
  int PI = 34;
  int AXIS_SELF = 35;
  int AXIS_CHILD = 36;
  int AXIS_PARENT = 37;
  int AXIS_ANCESTOR = 38;
  int AXIS_ATTRIBUTE = 39;
  int AXIS_NAMESPACE = 40;
  int AXIS_PRECEDING = 41;
  int AXIS_FOLLOWING = 42;
  int AXIS_DESCENDANT = 43;
  int AXIS_ANCESTOR_OR_SELF = 44;
  int AXIS_FOLLOWING_SIBLING = 45;
  int AXIS_PRECEDING_SIBLING = 46;
  int AXIS_DESCENDANT_OR_SELF = 47;
  int FUNCTION_LAST = 48;
  int FUNCTION_POSITION = 49;
  int FUNCTION_COUNT = 50;
  int FUNCTION_ID = 51;
  int FUNCTION_KEY = 52;
  int FUNCTION_LOCAL_NAME = 53;
  int FUNCTION_NAMESPACE_URI = 54;
  int FUNCTION_NAME = 55;
  int FUNCTION_STRING = 56;
  int FUNCTION_CONCAT = 57;
  int FUNCTION_STARTS_WITH = 58;
  int FUNCTION_CONTAINS = 59;
  int FUNCTION_SUBSTRING_BEFORE = 60;
  int FUNCTION_SUBSTRING_AFTER = 61;
  int FUNCTION_SUBSTRING = 62;
  int FUNCTION_STRING_LENGTH = 63;
  int FUNCTION_NORMALIZE_SPACE = 64;
  int FUNCTION_TRANSLATE = 65;
  int FUNCTION_BOOLEAN = 66;
  int FUNCTION_NOT = 67;
  int FUNCTION_TRUE = 68;
  int FUNCTION_FALSE = 69;
  int FUNCTION_NULL = 70;
  int FUNCTION_LANG = 71;
  int FUNCTION_NUMBER = 72;
  int FUNCTION_SUM = 73;
  int FUNCTION_FLOOR = 74;
  int FUNCTION_CEILING = 75;
  int FUNCTION_ROUND = 76;
  int FUNCTION_FORMAT_NUMBER = 77;
  int NCName = 78;

  int DEFAULT = 0;

  String[] tokenImage = {
    "<EOF>",
    "\" \"",
    "\"\\t\"",
    "\"\\n\"",
    "\"\\r\"",
    "\"\\f\"",
    "\"/\"",
    "\"//\"",
    "\"|\"",
    "\"+\"",
    "\"-\"",
    "\"=\"",
    "\"!=\"",
    "\"<\"",
    "\"<=\"",
    "\">\"",
    "\">=\"",
    "\"$\"",
    "<Literal>",
    "<Digit>",
    "<Number>",
    "<Letter>",
    "<BaseChar>",
    "<Ideographic>",
    "<CombiningChar>",
    "<UnicodeDigit>",
    "<Extender>",
    "\"or\"",
    "\"and\"",
    "\"mod\"",
    "\"div\"",
    "\"node\"",
    "\"text\"",
    "\"comment\"",
    "\"processing-instruction\"",
    "\"self::\"",
    "\"child::\"",
    "\"parent::\"",
    "\"ancestor::\"",
    "\"attribute::\"",
    "\"namespace::\"",
    "\"preceding::\"",
    "\"following::\"",
    "\"descendant::\"",
    "\"ancestor-or-self::\"",
    "\"following-sibling::\"",
    "\"preceding-sibling::\"",
    "\"descendant-or-self::\"",
    "\"last\"",
    "\"position\"",
    "\"count\"",
    "\"id\"",
    "\"key\"",
    "\"local-name\"",
    "\"namespace-uri\"",
    "\"name\"",
    "\"string\"",
    "\"concat\"",
    "\"starts-with\"",
    "\"contains\"",
    "\"substring-before\"",
    "\"substring-after\"",
    "\"substring\"",
    "\"string-length\"",
    "\"normalize-space\"",
    "\"translate\"",
    "\"boolean\"",
    "\"not\"",
    "\"true\"",
    "\"false\"",
    "\"null\"",
    "\"lang\"",
    "\"number\"",
    "\"sum\"",
    "\"floor\"",
    "\"ceiling\"",
    "\"round\"",
    "\"format-number\"",
    "<NCName>",
    "\":\"",
    "\"(\"",
    "\")\"",
    "\".\"",
    "\"..\"",
    "\"[\"",
    "\"]\"",
    "\"@\"",
    "\",\"",
    "\"*\"",
  };

}