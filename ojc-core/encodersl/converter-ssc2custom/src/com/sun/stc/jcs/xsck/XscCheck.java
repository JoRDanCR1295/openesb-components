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
 * @(#)XscCheck.java 
 *
 * Copyright 2004-2008 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.stc.jcs.xsck;

import java.util.*;
import java.io.*;

import javax.xml.parsers.ParserConfigurationException;
import javax.xml.parsers.SAXParser;
import javax.xml.parsers.SAXParserFactory;

import org.w3c.dom.*;
import org.xml.sax.*;

import com.sun.stc.jcs.JCSCompiler;
import com.sun.stc.jcsre.FormatterFactory;
import com.sun.stc.jcsre.Option;
import com.sun.stc.jcsre.StringCoderFactory;

/**
 * Main class to implement a stand-alone XSC semantic checker.
 *
 * XscCheck: semantic validator for event description files in
 * the XSC 0.6 format, as used for e*Gate 4.5.3 and later.
 *
 * TO DO:
 * - finish all the NYI (NotYetImplemented) stuff;
 * - allow multiple input files;
 * - make all per-XSC-file data non-static, to handle includes;
 * - check &lt;method&gt; syntax;
 * - check &lt;javaProps&gt; semantics;
 * - add alternate interface to call from other Java class.
 */
public class XscCheck
{

    public static final String XSC_VERSION_6 = "0.6";

    public static final String XSC_VERSION_LATEST = XSC_VERSION_6;

    /**
     * Represents a fatal error (do not continue checking).
     */
    public static class Epitaph extends RuntimeException
    {
	/**
	 * Creates from an error text string.
	 *
	 * @param msg  the error message
	 */
	public Epitaph (String msg)
	{
	    super(msg);
	}
    }

    /**
     * Represents warnings only.
     */
    public static class Memento extends RuntimeException
    {
	/**
	 * Creates from an error text string.
	 *
	 * @param msg  the error message
	 */
	public Memento (String msg)
	{
	    super(msg);
	}
    }

    boolean debug = false;
    boolean verbose = false;
    boolean mustHaveUid = false; // input for collab. editor?
    boolean wantWarn = false;
    boolean doNameUnique = false; // must <node>'s "javaName" be unique?
    int errors = 0;
    int warnings = 0;
    int globalDepth = 0; // global default delimiter list depth
    boolean wantMeths = true; // are <method> elements expected?

    /*--------------------------------*\
    |  Generic error handling
    \*--------------------------------*/

    private String lastSystemId = null;

    /**
     * Provides location as printable string, suitable for prefixing to
     * and error message for the user.
     *
     * @param node  the offending XSC node
     * @param prefix  error context indication
     * @return a location description
     */
    private String loc (XmlElement node, String prefix)
    {
	String in = "";
	String at = "";

	if (node != null)
	{
	    Locator where = node.getLocation();
	    String systemId = where.getSystemId();
	    if (! systemId.equals(lastSystemId))
	    {
		// Different file; header needed.
		in = "File \"" + systemId + "\"...\n";
		lastSystemId = systemId;
	    }
	    at = "line " + where.getLineNumber()
		+ " col " + where.getColumnNumber() + ": ";
	}
	return in + prefix + at + "in <" + node.getLocalName() + ">: ";
    }

    /**
     * Provides location as printable string, suitable for use as part for
     * further data in an error message for the user.  This should be
     * preceded by a call to loc() for a prior error message.
     *
     * @param node  the offending XSC node
     * @return a location description
     */
    static String also (XmlElement node)
    {
	String at = "";

	if (node != null)
	{
	    Locator where = node.getLocation();
	    at = "line " + where.getLineNumber()
		+ " col " + where.getColumnNumber();
	}
	return at + " in <" + node.getLocalName() + ">";
    }

    // Error output stream, to return in Epitaph.
    ByteArrayOutputStream ba = new ByteArrayOutputStream();
    PrintStream err = new PrintStream(ba);

    // Die again, from other exception.
    private void die (Exception e)
    {
	StringWriter sw = new StringWriter();
	PrintWriter pw = new PrintWriter(sw);
	e.printStackTrace(pw);
	pw.flush();
	errors ++;
	throw new Epitaph(sw.toString());
    }

    // Emit message for fatal error, then throw exception.
    private void die (XmlElement node, String msg)
    {
	if (node != null)
	    err.print(loc(node, "give up: "));
	err.println(msg);
	err.flush();
	throw new Epitaph(new String(ba.toByteArray()));
    }

    // If there were any warnings, throw a Memento exception.
    private void fry ()
    {
	if (warnings > 0)
	{
	    err.flush();
	    throw new Memento(new String(ba.toByteArray()));
	}
    }

    // Emit message for true error, but continue afterwards.
    private void cry (XmlElement node, String msg)
    {
	err.println(loc(node, "mistake: ") + msg);
	errors ++;
    }

    // Emit message for warning, and continue afterwards.
    private void hey (XmlElement node, String msg)
    {
	err.println(loc(node, "warning: ") + msg);
	warnings ++;
    }

    /*--------------------------------*\
    |  Generic attribute checks
    \*--------------------------------*/

    static HashSet primitive = null;

    /**
     * Tests if this a Java type, that can be used as a leaf node type.
     * We accept all Java primitive types, and (is "known" is false)
     * anything with a "." (presumable a fully qualified name).
     * This is only a heuristic, as there is no dependable way of
     * telling Java type names and (non-Java) local template names apart
     * in XSC 0.3 (see gripes the Metadata document).
     * This has been solved in XSC 0.4.
     *
     * @param type  a type name string
     * @param known  flag: must Java type be known here to be valid?
     * @return the test result
     */
    private boolean isJavaType (String type, boolean known)
    {
	if (primitive == null)
	{
	    // Initialisation.
	    primitive = new HashSet();
	    primitive.add("char");
	    primitive.add("byte[]");
	    primitive.add("boolean");
	    primitive.add("byte");
	    primitive.add("short");
	    primitive.add("int");
	    primitive.add("long");
	    primitive.add("float");
	    primitive.add("double");
	    primitive.add("java.lang.String");
	    primitive.add("String");  // sigh...
	}
	return primitive.contains(type) ||
	    (! known && type.indexOf('.') >= 0);
    }

    /**
     * Check a node to make sure it only contains the attributes mentioned
     * in the given list.  Otherwise, die.
     *
     * @param used  attributes checked so far
     * @param node  the XSC node to check
     */
    private void allConsumed (HashSet used, XmlElement node)
    {
	Attributes attr = node.getAttributes();
	int len = attr.getLength();

	for (int i = 0; i < len; i ++)
	{
	    String name = attr.getLocalName(i);
	    if (! used.contains(name))
		cry(node,
		    "unexpected attribute [" + name + "]");
	}
    }

    /**
     * Fetches the attribute value, or null if not present for node.
     * Also adds it to the "used" set if present.
     *
     * @param used  the set of processed atribute names
     * @param node  the XML data for this node
     * @param name  the attribute name
     * @return the attribute value string, or null
     */
    private String getAndUse (HashSet used, XmlElement node, String name)
    {
	Attributes attr = node.getAttributes();
	String result = null;

	if (attr != null)
	{
	    result = attr.getValue(name);
	    if (result != null && used != null)
		used.add(name);
	}
	return result;
    }

    // Return value of optional string attribute, else default.
    private String want_str
	(HashSet used, XmlElement node, String name, String deft)
    {
	String result = getAndUse(used, node, name);
	return (result == null) ? deft : result;
    }

    // Return value of mandatory string attribute.
    private String need_str (HashSet used, XmlElement node, String name)
    {
	String result = getAndUse(used, node, name);

	if (result == null)
	    die(node,
		"missing mandatory [" + name + "] attribute");
	return result;
    }

    // Return value 0-15 of hexadecimal literal digit, or -1 for error.
    static private int hexval (char c)
    {
	if ('0' <= c && c <= '9')
	    return c - '0';
	if ('a' <= c && c <= 'f')
	    return c - 'a' + 10;
	if ('A' <= c && c <= 'F')
	    return c - 'A' + 10;

	// Not a proper digit.
	return -1;
    }

    /**
     * Fetches value of optional normal-safe form string attribute, which uses
     * Unicode escapes (e.g.&nbsp;"\u20AC") to escape anything non-printable.
     * Also complains if the string is empty.
     *
     * @param used  the set of processed atribute names
     * @param node  the XML data for this node
     * @param name  the attribute name
     * @return the attribute value string, or null
     */
    private String want_esc (HashSet used, XmlElement node, String name)
    {
	String raw = getAndUse(used, node, name);
	if (raw == null)
	    return null;
	int len = raw.length();

	if (len == 0)
	    hey(node, "empty [" + name + "] attribute string");

	// Convert all Unicode escapes to actual string.
	StringBuffer b = new StringBuffer(len);
	for (int i = 0; i < len; i ++)
	{
	    char c = raw.charAt(i);
	    if (c == '\\')
	    {
		// Convert & check for Unicode 3.0 validity.
		if (i + 6 > len)
		{
		    // The escape sequence is 6 characters.
		    die(node, "incomplete Unicode escape at end of "
			+ "[" + name + "] attribute string");
		}
		else if (raw.charAt(i+1) != 'u')
		{
		    // The only escapes we allow are backslash + "u".
		    die(node, "unrecognized escape sequence \"\\"
			+ raw.charAt(i+1) + "\" in [" + name + "] attribute");
		}
		else
		{
		    // Must get 4 hexadecimal characters.
		    int d1 = hexval(raw.charAt(i+2));
		    int d2 = hexval(raw.charAt(i+3));
		    int d3 = hexval(raw.charAt(i+4));
		    int d4 = hexval(raw.charAt(i+5));
		    if (d1 < 0 || d2 < 0 || d3 < 0 || d4 < 0)
		    {
			// Some digit was not hexadecimal.
			die(node,
			    "invalid Unicode escape \""
			    + raw.substring(i, i+6) + "\" in [" + name + "]");
		    }
		    c = (char)((d1 << 12) + (d2 << 8) + (d3 << 4) + d4);
		    i += 5;
		}
	    }
	    else if (c == '&')
	    {
		/* In normal-safe form, all special things should have
		 * been escaped, including backslashes and ampersand.
		 */
		cry(node, "unescaped '" + c + "' character in [" + name + "]");
	    }
	    else if (c == ' ')
	    {
		/* Unescaped spaces are allowed if they are in between
		 * non-spaces.
		 */
		if (i == 0 ||
		    i == len-1 ||
		    raw.charAt(i-1) == ' ' ||
		    raw.charAt(i+1) == ' ')
		{
		    cry(node, "unescaped '" + c + "' character in ["
			+ name + "]");
		}
	    }
	    else
	    {
		// If not escaped or special, should be printable ASCII.
		if (c < 0x20 || 0x7E < c)
		{
		    // In normal-safe form, use Unicode escapes for the rest.
		    cry(node,
			"unescaped character #" + ((int) c)
			+ " in [" + name + "]");
		}
	    }
	    if (! Character.isDefined(c))
	    {
		// Not in the valid Unicode 3.0  repertoire.
		die(node,
		    "unassigned Unicode character #" + ((int) c)
		    + " in [" + name + "]");
	    }
	    b.append(c);
	}
	return b.toString();
    }

    // Return value of mandatory normal-safe form string attribute.
    private String need_esc (HashSet used, XmlElement node, String name)
    {
	String result = want_esc(used, node, name);

	if (result == null)
	    die(node,
		"missing mandatory [" + name + "] attribute");
	return result;
    }

    // Return value of optional boolean attribute, else false.
    private boolean want_log (HashSet used, XmlElement node, String name)
    {
	String result = getAndUse(used, node, name);
	if (result == null || result.equals("false"))
	    return false;
	if (! result.equals("true"))
	    die(node, "boolean attribute <" + name
		+ "> has illegal value \"" + result + "\"");
	return true;
    }

    /* Return value of optional integer attribute, or default if missing.
     * As a special case, if the given (optional) "special" string is
     * encountered (typically "unbounded" or "undefined"), return specVal.
     */
    private int want_int (HashSet used, XmlElement node,
	String name, int deft, String special, int specVal)
    {
	String result = getAndUse(used, node, name);
	if (result == null)
	    return deft;
	if (special != null && result.equals(special))
	    return specVal;
	try
	{
	    // Convert as optionally signed decimal literal.
	    return Integer.parseInt(result);
	}
	catch (NumberFormatException ne)
	{
	    // Oops, cannot scan.
	    die(node, "integer attribute [" + name + "] "
		+ "has illegal value \"" + result + "\"");
	    return 0; // dummy
	}
    }
    // Save as above, but specVal = deft.
    private int want_int (HashSet used, XmlElement node,
	String name, int deft, String special)
    {
	return want_int(used, node, name, deft, special, deft);
    }

    /* Check for a "comment" attribute; if present, it should not be empty.
     * Note that comment attributes are always optional.
     */
    private void commOrEmpty (HashSet used, XmlElement node)
    {
	String result = getAndUse(used, node, "comment");
	if (result != null && result.length() == 0)
	    hey(node, "superfluous empty [comment] attribute");
    }

    /**
     * Checks that the given identifier is unique within its scope, by looking
     * it up in a hashtable of (ID,element) pairs.  Die if already defined,
     * otherwise add it to the table.
     *
     * @param used  table of (ID,node) pairs
     * @param node  the node containing the ID
     * @param attr  the name of the attribute containing the ID
     * @param ident  the value of the ID
     * @param what  kind of item, for message
     */
    private void uniqueIdent (Hashtable used, XmlElement node, String attr,
	String ident, String what)
    {
	XmlElement same = (XmlElement) used.get(ident);
	if (same == null)
	{
	    // Add to the global list.
	    used.put(ident, node);
	}
	else
	{
	    // Name clash: another node has the same ID.
	    die(node, "[" + attr + "] value \"" + ident + "\" "
		+ "clashes with " + what + " at "+ also(same));
	}
    }

    // Check that node has given element name.
    void needElement (XmlElement node, String name)
    {
	if (! name.equals(node.getLocalName()))
	    die(node,
		"should be <" + name + ">, not <" + node.getLocalName() + ">");
    }

    /**
     * Class "XscFile" represents a single XSC file.
     * Each such file has its own global name scope for UIDs and templates,
     * its own global delimiter list, and so on.
     */
    class XscFile
    {
	private String infile = "<none>";
	private Hashtable uids = null;
	private XmlElement root = null;
	boolean isSsc = false; // apply SSC checks?
	boolean isDtd = false; // apply DTD checks?

	// Create new XSC file context.
	public XscFile (String file, XmlElement root)
	{
	    this.infile = file;
	    this.root = root;
	    this.uids = new Hashtable();
	}

	/* Check for a "uid" attribute; if present, it should be non-empty
	 * and globally unique.  Uid string are mandatory for collaboration
	 * editor use, but optional for ETD use.
	 */
	private void checkUidOpt (HashSet used, XmlElement node)
	{
	    String uid = getAndUse(used, node, "uid");
	    if (uid == null)
	    {
		// Only mandatory if flag set.
		if (mustHaveUid)
		    die(node, "missing mandatory [uid] attribute");
	    }
	    else
	    {
		// We have one, make it lexically valid and unique.
		if (uid.length() == 0)
		    die(node, "[uid] mustn't be empty");
		uniqueIdent(uids, node, "uid", uid, "element");
	    }
	}

        /**
         * validate an interface node
         *
         * @param _uids a set of already defined uids
         * @param _ifcs a map of already defined interfaces
         * @param _node the node to validate
         */
        private void checkInterface (HashSet _uids, Hashtable _ifcs, XmlElement _node) {

            needElement(_node, "interface");
            String name = need_str(_uids, _node, "name");
            commOrEmpty(_uids, _node);
            allConsumed(_uids, _node);
            uniqueIdent(_ifcs, _node, "name", name, "interface");

            Hashtable mets = new Hashtable();
            XmlElement met = _node.getFirstChild();
            for (; met != null; met = met.getNextSibling())
                checkMethod(mets, met);

        }

        /**
         * validate a jar node
         *
         * @param _uids a set of already defined uids
         * @param _jars a map of already defined interfaces
         * @param _node the node to validate
         */
        private void checkJar (HashSet _uids, Hashtable _jars, XmlElement _node) {
            needElement(_node, "jar");
            String file = need_str(_uids, _node, "file");
            if ("".equals(file)) hey(_node, "file attribute cannot be empty.");
            commOrEmpty(_uids, _node);
            allConsumed(_uids, _node);
            uniqueIdent(_jars, _node, "file", file, "jars");
        }

	/*--------------------------------*\
	|  Special element checks
	\*--------------------------------*/

	// Check a <javaProps> element.
	private void checkJavaProps (XmlElement node)
	{
	    String pack, cnam, jarf, jsrc;
	    boolean code;
	    Attributes attr;
	    HashSet used = new HashSet();

	    needElement(node, "javaProps");
	    pack = need_str(used, node, "package");
	    cnam = need_str(used, node, "class");
	    code = want_log(used, node, "codeAvailable");
	    jarf = want_esc(used, node, "jarFile");
	    jsrc = want_esc(used, node, "source");
	    commOrEmpty(used, node);
	    checkUidOpt(used, node);
	    allConsumed(used, node);

	    if (jarf != null && jarf.length() == 0)
		cry(node, "filename in [jarFile] is empty");

	    // If code has been generated, we must have unique javaName value.
	    doNameUnique = code;

	    //NYI: check package name
	    //NYI: check class name is bean

	    Hashtable ifcs = new Hashtable();
	    Hashtable jars = new Hashtable();
	    XmlElement sub = node.getFirstChild();
	    for (; sub != null; sub = sub.getNextSibling())
	    {
                String elem = sub.getLocalName();
                if("interface".equals(elem)) {
                    checkInterface(used, ifcs, sub);
                } else if("jar".equals(elem)) {
                    checkJar(used, jars, sub);
                } else {
                    die(node, "should be <interface> or <jar>, not <" + elem + ">");
                }
	    }
	}

	/**
	 * Checks a &lt;beginDelim&gt; or &lt;endDelim&gt; element.
	 *
	 * @param node  the XML subtree to check
	 * @return true for begin-delimiter, false for end-delimiter
	 */
	private boolean checkBeginEndDelim (XmlElement node)
	{
	    Attributes attr;
	    HashSet used = new HashSet();
	    String data, text, code;

	    boolean begin = "beginDelim".equals(node.getLocalName());
	    needElement(node, (begin ? "beginDelim" : "endDelim"));
	    data = want_esc(used, node, "bytes");
	    text = want_esc(used, node, "value");
	    code = want_esc(used, node, "encoding");
	    checkUidOpt(used, node);
	    commOrEmpty(used, node);
	    allConsumed(used, node);
	    //-NYI: check attribute values...

	    if (node.getFirstChild() != null)
		cry(node, "cannot have children");
	    return begin;
	}

	/**
	 * Checks a &lt;escape&gt element.
	 *
	 * @param node  the XML subtree to check
	 * @return true for begin-delimiter, false for end-delimiter
	 */
	private void checkEscape (XmlElement node)
	{
	    Attributes attr;
	    HashSet used = new HashSet();
	    String data, text, code;

	    data = want_esc(used, node, "bytes");
	    text = want_esc(used, node, "value");
	    code = want_esc(used, node, "encoding");
	    checkUidOpt(used, node);
	    commOrEmpty(used, node);
	    allConsumed(used, node);
	    //-NYI: check attribute values...

	    if (node.getFirstChild() != null)
		cry(node, "cannot have children");
	}

	// Checks a <delimGroup> element.
	private void checkGroup (XmlElement node)
	{
	    Attributes attr;
	    HashSet used = new HashSet();

	    needElement(node, "delimGroup");
	    checkUidOpt(used, node);
	    commOrEmpty(used, node);
	    allConsumed(used, node);

	    // Walk the groups.
	    int begins = 0, ends = 0;
	    for (XmlElement del = node.getFirstChild(); del != null;
		del = del.getNextSibling())
	    {
		if (checkBeginEndDelim(del))
		    begins ++;
		else
		    ends ++;
	    }
	    if (begins + ends == 0)
		cry(node, "requires at least one <beginDelim> or <endDelim>");
	}

	// Checks a <delim> element.
	private void checkDelim (XmlElement node)
	{
	    String elem, valu, bdel, edel;
	    boolean anch, banc, eanc, arry, erec, reqd, sepa;
	    Attributes attr;
	    HashSet used = new HashSet();

	    needElement(node, "delim");
	    anch = want_log(used, node, "anchored");
	    banc = want_log(used, node, "beginAnchored");
	    eanc = want_log(used, node, "endAnchored");
	    arry = want_log(used, node, "array");
	    erec = want_log(used, node, "endOfRec");
	    reqd = want_log(used, node, "required");
	    sepa = want_log(used, node, "separator");
	    bdel = want_esc(used, node, "beginDelim");
	    edel = want_esc(used, node, "endDelim");
	    valu = want_esc(used, node, "value");
	    checkUidOpt(used, node);
	    commOrEmpty(used, node);
	    allConsumed(used, node);

	    // Delimiters must contain at least 1 character.
	    if (valu != null && valu.length() < 1)
		cry(node, "empty delimiter [value] string");
	    if (bdel != null && bdel.length() < 1)
		cry(node, "empty delimiter [beginDelim] string");
	    if (edel != null && edel.length() < 1)
		cry(node, "empty delimiter [endDelim] string");

	    if (valu != null && edel != null && ! valu.equals(edel))
	    {
		// If both are present, they must be the same string.
		cry(node, "[value] attribute differs from [endDelim]");
	    }
	    else if (valu != null && edel == null)
	    {
		// Deprecated "value" is synonym for "endDelim".
		hey(node, "[value] is deprecated, use [endDelim]");
	    }
	    /*-NYI...
	    if (edel != null)
		uniqueIdent(dels, node, "endDelim", edel, "delimiter");
	    -*/

	    // Walk the groups.
	    int groups = 0;
	    for (XmlElement del = node.getFirstChild(); del != null;
		del = del.getNextSibling())
	    {
	      if ("escape".equals(del.getLocalName()))
		{
		  checkEscape(del);
		}
	      else
		{
		  groups ++;
		  checkGroup(del);
		} 
	    }
	    if (valu == null && bdel == null && edel == null && groups == 0)
	    {
		/* Supplying delimiter as PCDATA instead of "value"
		 * attribute is permitted but deprecated in XSC 0.3.
		 * After that, we deprecated "value" and use "endDelim".
		 */
		cry(node, "requires [value], [beginDelim] or [endDelim],"
		    + " or at least one <group>");
	    }
	}

	// Checks a <delimiters> element.
	private void checkDelimiters (XmlElement node)
	{
	    String elem, valu, bdel, edel;
	    boolean anch, banc, eanc, arry, erec, reqd, sepa;
	    Attributes attr;
	    HashSet used = new HashSet();

	    needElement(node, "delimiters");
	    checkUidOpt(used, node);
	    allConsumed(used, node);

	    // Process the individual <delim> entries.
	    Hashtable dels = new Hashtable();
	    for (XmlElement del = node.getFirstChild(); del != null;
		del = del.getNextSibling())
	    {
		checkDelim(del);
	    }
	    globalDepth = dels.size();
	}

	// Checks a <param> element.
	void checkParam (Hashtable pars, XmlElement node)
	{
	    String name, type, ptyp;
	    Attributes attr;
	    HashSet used = new HashSet();

	    needElement(node, "param");
	    name = need_str(used, node, "name");
	    type = want_str(used, node, "type", "PARAM");
	    ptyp = need_str(used, node, "paramType");
	    checkUidOpt(used, node);
	    commOrEmpty(used, node);
	    allConsumed(used, node);

	    uniqueIdent(pars, node, "name", name, "parameter");
	    if (! type.equals("PARAM"))
	    {
		/* Type used to be mandatory, because XSC 0.2 has &lt;param&gt;
		 * as a synonym for &lt;node&gt; and &lt;class&gt;, but now it
		 * is optional and only retained for backward compatibility.
		 */
		die(node,
		    "optional [type] must be \"PARAM\", not \"" + type + "\"");
	    }
	}

	// Checks a <throws> element.
	private void checkThrows (Hashtable pars, XmlElement node)
	{
	    String type;
	    Attributes attr;
	    HashSet used = new HashSet();

	    needElement(node, "throws");
	    type = need_str(used, node, "excepType");
	    checkUidOpt(used, node);
	    commOrEmpty(used, node);
	    allConsumed(used, node);
	}

	// Checks a <method> element.
	private void checkMethod (Hashtable mets, XmlElement node)
	{
	    String name, type, rest, sign;
	    Attributes attr;
	    HashSet used = new HashSet();

	    needElement(node, "method");
	    name = need_str(used, node, "name");
	    type = want_str(used, node, "type", "METHOD");
	    rest = need_str(used, node, "returnType");
	    sign = need_str(used, node, "signature");
	    checkUidOpt(used, node);
	    commOrEmpty(used, node);
	    allConsumed(used, node);

	    /* Checking name clashes is more complex for methods, since
	     * names may clas as long as the signatre is different.
	     * Hence, we register name + "=" + signature as the unique string.
	     */
	    uniqueIdent(mets, node, "name", name + "=" + sign, "method");

	    if (! type.equals("METHOD"))
	    {
		/* Type used to be mandatory, because XSC 0.2 has &lt;method&gt;
		 * as a synonym for &lt;node&gt; and &lt;class&gt;, but now it
		 * is optional and only retained for backward compatibility.
		 */
		die(node,
		    "optional [type] must be \"METHOD\", not \"" + type + "\"");
	    }
	    //NYI: check signature syntax
	    //NYI: check signature against param count/type & result

	    // Process the parameter list.
	    Hashtable pars = new Hashtable();
	    for (XmlElement par = node.getFirstChild(); par != null;
		par = par.getNextSibling())
	    {
		String elem = par.getLocalName();
		if (elem.equals("param"))
		    checkParam(pars, par);
		else if (elem.equals("throws"))
		    checkThrows(pars, par);
		else
		    die(node,
			"should be <param> or <throws>, not <" + elem + ">");
	    }
	}

	private boolean methodWarned = false;

	// Checks a (possibly nested) element in a template.
	private void checkNode
	    (
	    Hashtable ancs,  // Java names of all ancestors
	    Hashtable sibs,  // Java names of all siblings
	    XmlElement node, // node to check
	    int level	     // nesting level
	    )
	{
	    Attributes attr;
	    HashSet used = new HashSet();
	    Hashtable mets = null;

	    String elem = node.getLocalName();
	    if (elem.equals("class") || elem.equals("node"))
	    {
		// General attributes.
		String name, java, type, ordr, stru;
		int maxs, mins;
		boolean optn, over, read;

		// SSC only.
		String
		    jtyp = null, bdel = null, edel = null, itag = null,
		    defv = null, defb = null, defe = null, fixv = null,
		    scav = null, prec = null, cmin = null, cmax = null,
		    code = null, form = null, nick = null;
		int leng = 0, loff = 0, lsiz = 0, offs = 0;
		boolean
		    anch = false, banc = false, eanc = false, arry = false,
		    erec = false, reqd = false, sepa = false, sout = false,
		    publ = false;

		name = need_esc(used, node, "name");
		java = want_str(used, node, "javaName", null);
		type = need_str(used, node, "type");
		jtyp = want_str(used, node, "javaType", null);
		optn = want_log(used, node, "optional");
		over = want_log(used, node, "override");
		read = want_log(used, node, "readOnly");
		mins = want_int(used, node, "minOccurs", 1, null);
		maxs = want_int(used, node, "maxOccurs", 1, "unbounded", -1);
		stru = want_str(used, node, "structure", "delim");
		ordr = want_str(used, node, "order", "sequence");
		code = want_str(used, node, "encoding", "ASCII");
		form = want_esc(used, node, "format");
		if (0==level)
		    publ = want_log(used, node, "public");
		if (isSsc)
		{
		    nick = want_esc(used, node, "nickName");
		    leng = want_int(used, node, "length", -1, "undefined");
		    loff = want_int(used, node, "lengthFrom", -1, "undefined");
		    lsiz = want_int(used, node, "lengthSize", -1, "undefined");
		    offs = want_int(used, node, "offset", 0, "undefined");
		    anch = want_log(used, node, "anchored");
		    banc = want_log(used, node, "beginAnchored");
		    eanc = want_log(used, node, "endAnchored");
		    arry = want_log(used, node, "array");
		    erec = want_log(used, node, "endOfRec");
		    reqd = want_log(used, node, "required");
		    sepa = want_log(used, node, "separator");
		    bdel = want_esc(used, node, "beginDelim");
		    edel = want_esc(used, node, "endDelim");
		    itag = want_esc(used, node, "inputMatch");
		    defv = want_esc(used, node, "defaultValue");
		    defb = want_esc(used, node, "defaultBytes");
		    defe = want_str(used, node, "defaultEncoding", null);
		    scav = want_esc(used, node, "scavenger");
		    sout = want_log(used, node, "scavOutput");
		    prec = want_str(used, node, "precedence", "child");
		    cmin = want_str(used, node, "childMin", "undefined");
		    cmax = want_str(used, node, "childMax", "undefined");
		}
		else if (isDtd)
		{
		    defv = want_esc(used, node, "defaultValue");
		    fixv = want_esc(used, node, "fixedValue");
		}

		// <method> was a synonym for <node> in XSC 0.2.
		if (type.equals("METHOD"))
		    die(node, "type=METHOD not valid, use <method> element");
		else if (type.equals("PARAM"))
		    die(node, "type=PARAM not valid, use <param> element");

		/* Java name defaults to "name" value, should not duplicate it,
		 * and should be distinct from all ancestors and siblings.
		 */
		if (java == null)
		{
		    // Default used.
		    java = name;
		}
		else if (java.equals(name))
		{
		    // Superfluous "javaName" attribute.
		    hey(node,
			"[javaName] identical to [name] value \""
			+ java + "\"");
		}

		XmlElement anc = (XmlElement) ancs.get(java);
		if (anc != null)
		{
		    if(!isDtd) {
			// Name clash, would cause class naming problem.
			cry(node,
			    "[javaName] clashes with ancestor at " + also(anc));
		    }
		}
		if (doNameUnique)
		    uniqueIdent(sibs, node, "javaName", java, "sibling");
		sibs.put(java, node);

		if (0==level)
		{
		    /* A top-level node (i.e. template) has special
		     * restrictions, basically no abstract syntax.
		     */
		    if (optn)
			die(node,
			    "top-level node must have [optional] = \"false\"");
		    if (mins != 1)
			die(node,
			    "top-level node must have [minOccurs] = \"1\", "
			    + "not " + mins);
		    if (maxs != 1)
			die(node,
			    "top-level node must have [maxOccurs] = \"1\", "
			    + "not " + maxs);
		    if (offs != 0)
			die(node,
			    "top-level node cannot have "
			    + "[offset]=\"" + offs + "\"");
		}
		if (maxs >= 0 && mins > maxs)
		{
		    // Repetition lower-bound must be below upper-bound.
		    die(node,
			"cannot have minOccurs > maxOccurs "
			+ "(" + mins + " > " + maxs + ")");
		}
		if (maxs == 0)
		{
		    // Legal in SSC, but unusual in XSC.
		    hey(node, "maxOccurs=0, so node is superfluous");
		}
		if (mins == 0 && optn)
		{
		    // Either/or: zero repetitions cannot be optional.
		    die(node,
			"optional node must have minOccurs>0");
		}
		if (isSsc && mins > 1 && optn)
		{
		    // In SSC, repeat count >1 must be mandatory.
		    die(node,
			"cannot have optional SSC node with minOccurs>1");
		}

		if (! stru.equals("fixed") &&
		    ! stru.equals("delim") &&
		    ! stru.equals("array") &&
		    ! stru.equals("set"))
		{
		    // Must be one of the above.
		    die(node, "invalid [structure] value \"" + stru + "\"");
		}
		if (! ordr.equals("any") &&
		    ! ordr.equals("sequence") &&
		    ! ordr.equals("choice"))
		{
		    // Must be one of the above.
		    die(node, "invalid [order] value \"" + ordr + "\"");
		}

/*-
	BROKEN CODE
		// delimited and no delimiters specified in node
		if(stru.equals("delim") && null == bdel && null == edel) {
		    // global delimiters unspecified, or we don't have enough
		    if(globalDepth==0 || level>globalDepth) {
			die(node
			    , "must have default delimiter, [beginDelim] or [endDelim]"
			    + " when [structure]=\"delim\""
			);
		    }

		}
-*/

		if (bdel != null)
		{
		    // Begin-delimiter.
		    if (bdel.length() == 0)
			die(node, "the [beginDelim] is empty");
		    if (! (stru.equals("delim") || stru.equals("fixed")))
		    {
			// Cannot combine begin-delim. with array or set.
			die(node,
			    "cannot have [beginDelim] when "
			    + "[structure]=\"" + stru + "\"");
		    }
		}
		if (edel != null)
		{
		    // End-delimiter.
		    if (edel.length() == 0)
			die(node, "the [endDelim] is empty");
		    if (! stru.equals("delim"))
		    {
			// Cannot combine end-delim. with fixed, array or set.
			die(node,
			    "cannot have [endDelim] when "
			    + "[structure]=\"" + stru + "\"");
		    }
		}

		// Output values are mutually exclusive.
		if ((defv != null || defb != null) && fixv != null)
		    cry(node,
			"cannot have both [fixedValue] and [defaultValue]");
		if (defe != null && defv == null && defb == null)
		    hey(node,
			"[defaultEncoding] useless without default value");
		if (defb != null)
		{
		    int len = defb.length();
		    if (len == 0)
			hey(node,
			    "[defaultBytes] should be absent or non-empty");

		    // All chars should be byte-values.
		    for (int i = 0; i < len; i ++)
		    {
			char c = defb.charAt(i);
			if (c > 0xFF)
			    cry(node, "byte " + i + " of [defaultBytes]"
				+ " is " + (int) c + ", out of range 0-255");
		    }
		}

		// Scavenger characters.
		if (scav != null)
		{
		    int len = scav.length();
		    if (len == 0)
			hey(node, "empty [scavenger] is superfluous");
		    for (int i = len; i-- > 0; )
			if (scav.indexOf(scav.charAt(i)) != i)
			    hey(node, "[scavenger] has duplicate char ("
				+ (int) scav.charAt(i) + ")");
		}
		else if (sout)
		    hey(node, "[scavOutput] used without setting [scavenger]");

		if (prec != null)
		{
		    // Delimiter precedence.
		    if (! prec.equals("child") && ! prec.equals("parent"))
			cry(node, "[precedent] cannot be \"" + prec
			    + "\", must be \"parent\" or \"child\"");
		}

		// Child bounds.
		if (cmin == null || cmin.equals("undefined"))
		{
		    if (cmax != null && ! cmax.equals("undefined"))
			cry(node, "can't define [childMax] without [childMin]");
		}
		else
		{
		    if (cmax == null || cmax.equals("undefined"))
			cry(node, "can't define [childMin] without [childMax]");
		    else if (! cmax.equals("unbounded"))
		    {
			// Both bounds must be numeric and ordered.
			try
			{
			    // Convert as optionally signed decimal literal.
			    int min = Integer.parseInt(cmin);
			    int max = Integer.parseInt(cmax);
			    if (min > max)
				die(node, "[childMin] exceeds [childMax]");
			}
			catch (NumberFormatException ne)
			{
			    // Oops, cannot scan.
			    die(node, "non-numeric [childMin] or [childMax] "
				+ "value: \"" + cmin + "\", \"" + cmax + "\"");
			}
		    }
		}

		//NYI: more node-level checks...
		//NYI: local delim. only in SSC
		//NYI: check total depth vs. global delim depth
		//NYI: check non-recursion in SSC
		//NYI: imports (option -r)
		//NYI: warn if local template unused

                // check lengthFrom and lengthSize correspondence
                if(loff < -1) die(node, "Illegal [lengthFrom] value: " + loff);
                if(lsiz < -1) die(node, "Illegal [lengthSzie] value: " + lsiz);
                if(loff != -1 && lsiz < 1) die(node, "[lengthFrom] is defined.  [lengthSzie] must be > 0");
                

		//check [format]
                if(null != form) {

                    if(JCSCompiler.isNumericType(jtyp)) {
                        try {
                            // NYI: Separate format compliance check
                            // null pointer exception indicates no formatter
                            FormatterFactory.getNumberFormat(form).equals(null);
                        } catch(Exception ex) {
                            die(node, "contains illegal numeric format ["+form+"]: " + ex);
                        }

                    } else if("boolean".equalsIgnoreCase(jtyp)) {
                        try {
                            // NYI: Separate format compliance check
                            // null pointer exception indicates no formatter
                            FormatterFactory.getBooleanFormat(form).equals(null);
                        } catch(Exception ex) {
                            die(node, "contains illegal boolean format ["+form+"]: " + ex);
                        }
                    } else {
                        hey(node, "contains a superfluous format for type ["+jtyp+"]");
                    }

                }

		//check [encoding]
                if(null != code) {
                    try {
                        if(code.length()>0) {
                            // NYI: Separate valid code check
                            // null pointer exception indicates no coder
                            StringCoderFactory.getStringCoder(code).equals(null);
                        }  else {
                            hey(node, "contains empty encoding");
                        }
                    } catch(Exception ex) {
                        die(node, "contains illegal encoding ["+code+"]: " + ex);
                    }
                }

		XmlElement sub = node.getFirstChild();
		if (type.equals("CLASS"))
		{
		    // Composite node; check the children.
		    Hashtable subs = new Hashtable();
		    ancs.put(name, node);
		    for (; sub != null; sub = sub.getNextSibling())
		    {
		        if("delim".equals(sub.getLocalName())) {
		            checkDelim(sub);
			} else {
			    checkNode(ancs, subs, sub, level+1);
			}
		    }
		    if (subs.size() == 0 && isSsc)
		    {
			/* Node has no children.  Note that "method" subnodes
			 * are not entered into the subs-list.
			 */
			die(node, "CLASS-type node \"" + name + "\" "
			    + "must have subnodes");
		    }
		    ancs.remove(name);
		}
		else if (type.equals("METHOD") || type.equals("PARAM"))
		{
		    // Obsolete XSC 0.2 usage of <node> as synonym.
		    die(node, type + "-type must use <method> element");
		}
		else if (type.equals("REFERENCE"))
		{
		    // Template reference.
		    if (sub != null)
			die(node, "global reference node \"" + name + "\" "
			    + "cannot have subnodes");
		    //NYI...
		    String reff = want_esc(used, node, "reference");
		    if ("".equals(reff))
			hey(node, "empty [reference] is superfluous");
		    String memb = want_esc(used, node, "member");
		    if ("".equals(memb))
			hey(node, "empty [member] is superfluous");
		    if (reff != null && reff.length() > 0)
		    {
			// External template reference.
			//NYI...
		    }
		    else
		    {
			// Internal template reference.
			//NYI...
		    }
		}
		else if (type.equals("ENUMERATION"))
		{
		    // Enumeration value.
		    //NYI...
		}
		else if (type.equals("FIELD"))
		{
/*
		    // Leaf node field.
		    if (sub != null)
			die(node, "non-CLASS node \"" + name + "\" "
			    + "cannot have subnodes");
*/
		    if (null != sub && "delim".equals(sub.getLocalName())) {
			checkDelim(sub);
		    } else if(null != sub) {
			die(node, "non-CLASS node \"" + name + "\" "
			    + "cannot have subnodes of type " + sub.getLocalName());
		    }

		    if (jtyp == null)
			jtyp = "java.lang.String";
		    if (! isJavaType(jtyp, isSsc))
			die(node, "unsupported [javaType] value \"" + jtyp
			    + "\", should be byte[], String or primitive");
		    //NYI...
		}
		else if (isJavaType(type, isSsc))
		{
		    // Leaf node field.
		    if (sub != null)
			die(node, "non-CLASS node \"" + name + "\" "
			    + "cannot have subnodes");
		    hey(node, "obsolete field syntax, use [javaType] instead");
		    //NYI...
		}
		else
		{
		    // Local template reference.
		    if (sub != null)
			die(node, "local reference node \"" + name + "\" "
			    + "cannot have subnodes");
		    //NYI...
		}

	    }
	    else if (elem.equals("delim"))
	    {
		// Local delimiter, XSC 0.5-style.
		checkDelim(node);
	    }
	    else if (elem.equals("method"))
	    {
		// Got a <method> element to check.
		if (mets == null)
		    mets = new Hashtable();
		if (! (0==level) && ! wantMeths && ! methodWarned)
		{
		    methodWarned = true;
		    hey(node,
			"no <method> allowed here for this ETD type");
		}
		checkMethod(mets, node);
		//NYI: make sure it's not below a Java base type node
		return;
	    }
	    else
	    {
		// Unexpected element.
		die(node,
		    "should be <class>, <node> or <method>, "
		    + "not <" + elem + ">");
	    }
	    checkUidOpt(used, node);
	    commOrEmpty(used, node);
	    allConsumed(used, node);
	}

	/**
	 * Checks if encoding name is a Java encoding that is also
	 * supported  in Monk.  Null is valid.
	 *
	 * @param code  encoding name, or null
	 * @return true if valid
	 */
	private boolean monkSupportsCode (String code)
	{
	    return code == null ||
		code.equals("US_ASCII") ||
		code.equals("UTF-8") ||
		code.equals("SJIS") ||
		code.equals("MS949");
	}

	// Checks top-level <etd> element.
	private void checkEtd ()
	{
	    String elem, etdn, etyp, name, java, vers, type, edit, pack,
		code, data;
	    boolean derv;
	    NodeList kids;
	    Attributes attr;
	    HashSet used = new HashSet();
	    boolean needDelim = false; // is <delimiters> required?

	    needElement(root, "etd");
	    etdn = need_str(used, root, "name");
	    etyp = need_str(used, root, "type");
	    derv = want_log(used, root, "derived");
	    vers = want_str(used, root, "xscVersion", "0.3");
	    edit = want_str(used, root, "editable", null);
	    pack = want_str(used, root, "packageName", null);
	    code = want_str(used, root, "sscEncoding", null);
	    data = want_str(used, root, "dataEncoding", null);
	    checkUidOpt(used, root);	//NYI: not in Metadata; needed Lawrence?
	    commOrEmpty(used, root);
	    allConsumed(used, root);

            // check version information
	    if (! vers.equals(XscCheck.XSC_VERSION_LATEST)) {
		hey(root, "[xscVersion] must be \""
                            +XscCheck.XSC_VERSION_LATEST
                            +"\", not \"" + vers + "\""
                );
            }

	    if (pack != null)
	    {
		// In early XSC 0.3, we had <etd packageName="xxx" ...>.
		hey(root,
		    "[packageName] attribute is deprecated, "
		    + "use [package] in <javaProps> instead");
	    }
	    // Not documented yet; extension for non-ASCII encoded SSC.
	    if (! monkSupportsCode(code))
	    {
		// We only support a few at present.
		hey(root,
		    "[sscEncoding] value \"" + code
		    + "\" not yet supported");
	    }
	    if (data != null)
	    {
		// Not documented yet; extension for non-ASCII encoded SSC.
		if (! monkSupportsCode(data))
		{
		    // We only support a few at present.
		    hey(root,
			"[dataEncoding] value \"" + data
			+ "\" not yet supported");
		}
		if (code != null && data.equals(code))
		    hey(root,
			"superfluous [dataEncoding], same as [sscEncoding]");
	    }

	    if (etyp.equals("SSC"))
	    {
		needDelim = true;
		//wantMeths = false;
		isSsc = true;
		//NYI...
	    }
	    else if (etyp.equals("DB"))
	    {
		//NYI...
	    }
	    else if (etyp.equals("DTD"))
	    {
		wantMeths = true;
		isDtd = true;
		//NYI...
	    }
	    else if (etyp.equals("XSD"))
	    {
		//NYI...
	    }
	    else if (etyp.equals("X12"))
	    {
		//NYI...
	    }
	    else
	    {
		// Unknown type, may have to add.
		hey(root, "unknown [type] value \"" + etyp + "\"");
	    }

	    // Do top-level nodes.
	    boolean gotEtd = false, gotTop = false;
	    boolean gotJavaProps = false;
	    boolean gotDelim = false;
	    HashSet tops = new HashSet();
	    XmlElement top = root.getFirstChild();
	    for (; top != null; top = top.getNextSibling())
	    {
		elem = top.getLocalName();
		if (elem.equals("javaProps"))
		{
		    // Java property list.
		    if (gotDelim || gotTop)
		    {
			// Must precede all <delimiters>, <node> elements.
			cry(top, "the <javaProps> element should come first");
		    }
		    else if (gotJavaProps)
		    {
			// Can only occur one per <etd>.
			cry(top, "duplicate <javaProps> element");
		    }
		    else
		    {
			// Check javaProps contents.
			checkJavaProps(top);
			gotJavaProps = true;
		    }
		}
		else if (elem.equals("delimiters"))
		{
		    // Global default delimiter list.
		    if (gotTop)
		    {
			// Must precede all <class>, <node> elements.
			cry(top,
			    "the <javaProps> element should come before"
			    + " <class> or <node>");
		    }
		    else if (gotDelim)
		    {
			// Can only occur one per <etd>.
			cry(top,
			    "duplicate <delimiters> element");
		    }
		    else
		    {
			// Check javaProps contents.
			checkDelimiters(top);
			gotDelim = true;
		    }
		}
		else if (elem.equals("class") || elem.equals("node"))
		{
		    // Local template or main node.
		    name = need_str(null, top, "name");
		    java = want_str(null, top, "name", name);
		    if (tops.contains(java))
		    {
			// Template names must be unique within file.
			cry(top, "duplicate top-level node \"" + name
			    + "\" (Java name: \"" + java + "\")");
		    }
		    else
		    {
			// Register as template.
			if (name.equals(etdn))
			    gotEtd = true;
			tops.add(java);
			checkNode(new Hashtable(), new Hashtable(), top, 0);
			gotTop = true;
		    }
		}
		else
		{
		    // Unknown top-level item.
		    cry(top, "unexpected <" + elem + "> element");
		}
	    }
	    if (! gotEtd)
	    {
		// There is no template with the name given in the <etd>.
		cry(root, "the main \"" + etdn + "\" template was not found");
	    }
	    if (! gotJavaProps)
	    {
		// Legal but unlikely.
		hey(root, "no <javaProps> element");
	    }
	    if (! gotDelim && needDelim)
	    {
		// SSC requires <delimiters>, even if empty.
		cry(root,
		    "type \"" + etyp + "\" requires <delimiters> element");
	    }
	}
    }

    /*--------------------------------*\
    |  Main glue code.
    \*--------------------------------*/

    /**
     * The XSC root, for resolving external references.
     * This will normally be $EGATE/client/_something_.
     * If null, then we have no known root, and cannot check templates.
     */
    File xscRoot = null;

    // The set of all files referred as global templates.
    Hashtable files = new Hashtable();

    /**
     * set the debugging flag
     * 
     * @param _flag new value
     */
    public void setDebug (boolean _flag) {this.debug = _flag;}

    /**
     * get the debugging flag
     *
     * @return the value
     */
    public boolean getDebug () {return this.debug;}

    /**
     * set the verbose flag
     * 
     * @param _flag the new value
     *
     */
    public void setVerbose (boolean _flag) {this.verbose = _flag;}

    /**
     * get the verbose flag
     *
     * @return the value of the flag
     */
    public boolean getVerbose () {return this.verbose;}

    // Are UIDs mandatory?
    public void setMustHaveUid (boolean flag)
    {
	mustHaveUid = flag;
    }

    // Do we want an exception if only warnings (no errors) occurred?
    public void setWantWarn (boolean flag)
    {
	wantWarn = flag;
    }

    // Set the XSC external reference root directory.
    public void setRoot (File root)
    {
	xscRoot = root;
    }

    // Get the XSC external reference root directory.
    public File getRoot ()
    {
	return xscRoot;
    }

    /**
     * Parses the input document, after conversion to a tree.
     *
     * @param source  the XSC input stream
     * @throws SAXParseException, SACException for XML-level input problems
     * @throws IOException for raw I/O problems
     * @throws ParserConfigurationException 
     */
    static XmlElement parse (InputSource source)
	throws SAXParseException, SAXException, IOException, ParserConfigurationException
    {
	SaxDocumentHandler handler = new SaxDocumentHandler();
	SAXParserFactory parserFactory = SAXParserFactory.newInstance();
	SAXParser parser = parserFactory.newSAXParser();
	parser.parse(source, handler);
	return handler.getRootElement();
    }

    // Adds a document to the list of files to check.
    public XscFile check (XmlElement from, String fileName, boolean isRef)
	throws SAXParseException, SAXException, IOException
    {
	return check(from, new File(xscRoot, fileName), isRef);
    }

    // Adds a document to the list of files to check.
    public XscFile check (XmlElement from, File file, boolean isRef)
	throws SAXParseException, SAXException, IOException
    {
	if (getVerbose())
	    System.out.println("[ check <" + file.getPath() + "> ]");
	if (! isRef)
	    warnings = errors = 0;

	String path = file.getPath();
	XscFile xf = (XscFile) files.get(file.getPath());
	if (xf == null)
	{
	    try
	    {
		if (! file.canRead())
		    die(from, "cannot read file \"" + path + "\"");
		if (getVerbose())
		    System.out.println("[ read <" + file.getPath() + "> ]");
		BufferedReader reader =
		    new BufferedReader(new FileReader(path));
		InputSource source = new InputSource(reader);
		source.setSystemId(path);
		XmlElement doc = parse(source);
		if (getDebug())
		{
		    System.out.println("[ show ]");
		    System.out.println(doc.toString());
		}
		if (getVerbose())
		    System.out.println("[ walk ]");
		xf = new XscFile(path, doc);
		xf.checkEtd();
		if (getVerbose())
		    System.out.println("[ done <" + file.getPath() + "> ]");
	    }
	    catch (SAXException se)
	    {
		die(se);
	    }
	    catch (IOException ie)
	    {
		die(ie);
	    } catch (ParserConfigurationException ie) {
	        die(ie);
        }
	}
	if (! isRef)
	{
	    // Top-level files emits collective errors here.
	    if (errors > 0)
		die(null, "encountered " + errors + " errors");
	    if (wantWarn)
		fry();
	    if (getVerbose())
		System.out.println("[ ready ]");
	}
	return xf;
    }

    /**
     * Checks the given series of files.
     *
     * @param args  the list of files
     */
    public void run (String[] args)
    {
	boolean dumpInput = false;

	for (int i = 0; i < args.length; i ++)
	{
	    String arg = args[i];
	    try
	    {
		// Read the input document.
		if (getVerbose())
		    System.out.println("[ check <" + arg + "> ]");
		File file = new File(arg);
		BufferedReader reader =
		    new BufferedReader(new FileReader(file.getPath()));
		InputSource source = new InputSource(reader);
		source.setSystemId(file.getPath());
		XmlElement doc = parse(source);
		if (getDebug())
		{
		    System.out.println("[ show ]");
		    System.out.println(doc.toString());
		}
		if (getVerbose())
		    System.out.println("[ walk ]");
		XscFile etd = new XscFile(arg, doc);
		etd.checkEtd();
		if (getVerbose())
		    System.out.println("[ done <" + arg + "> ]");
	    }
	    catch (SAXException se)
	    {
		die(se);
	    }
	    catch (IOException ie)
	    {
		die(ie);
	    } catch (ParserConfigurationException ie) {
	        die(ie);
        }
	}
	if (errors > 0)
	    die(null, "encountered " + errors + " errors and "
		+ warnings + " warnings");
	fry();
	if (getVerbose())
	    System.out.println("[ ready ]");
    }

    static public void main (String[] args)
    {
	XscCheck xsck = new XscCheck();
	boolean dumpInput = false;
	int c = 0;

	xsck.setWantWarn(true);
	Option opt = new Option(args, "duvx:");
	while ((c = opt.getOpt()) != Option.EOF)
	{
	    switch (c)
	    {
	    case 'd':
		// Option "-d"; debugging.
		xsck.setDebug(true);
		System.out.println("[ debug ]");
		break;
	    case 'u':
		// Option "-u"; make UIDs mandatory.
		xsck.setMustHaveUid(true);
		System.out.println("[ mustHaveUID ]");
		break;
	    case 'v':
		// Option "-v"; be verbose.
		xsck.setVerbose(true);
		System.out.println("[ verbose ]");
		break;
	    case 'x':
		// Option "-x <dir>"; set XSC-root.
		xsck.setRoot(new File(opt.getOptArg()));
		System.out.println("[ xsc root = " + xsck.getRoot() +" ]");
		break;
	    }
	}
	for (int i = opt.getOptInd(); i < args.length; i ++)
	{
	    try
	    {
		// Read the input document.
		xsck.check(null, new File(xsck.getRoot(), args[i]), false);
		//-xsck.run(new String[] { args[i] });
	    }
	    catch (Memento m)
	    {
		System.out.println("\"" + args[i] + "\": warnings");
		System.out.println(m.getMessage());
		System.out.flush();
	    }
	    catch (Exception e)
	    {
		System.err.println("\"" + args[i] + "\": failed validation");
		e.printStackTrace();
		System.err.flush();
	    }
	}
    }
}
