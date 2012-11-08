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
 * @(#)ParseXsc.java 
 *
 * Copyright 2004-2008 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.stc.jcs.ssc;

import java.util.*;
import java.io.*;

import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;

import org.w3c.dom.*;
import org.xml.sax.*;

import com.sun.stc.common.utils.CommonMsg;
import com.sun.stc.jcs.ssc.ClassTree;
import com.sun.stc.jcs.ssc.Xsc;
import com.sun.stc.jcs.ssc.ClassTree.PathNode;
import com.sun.stc.jcs.ssc.impl.*;
import com.sun.stc.jcs.xsck.XscCheck;
import com.sun.stc.jcsre.JCSProperties;
import com.sun.stc.jcsre.StringCoderFactory;

/**
 * Class to parse XSC input for SSC-based event descriptions.
 * Part of the e*Gate Java collaboration service tools.
 */
public class ParseXsc
{
    static final boolean debugAll =
	JCSProperties.getFlag("ParseXsc.debug", false);

    /**
     * Option to validate the XSC input using com.sun.stc.jcs.xsck.XScCheck.
     * This can be used for development or debugging, because some of those
     * checks are more strict than the input verification done by ParseXsc
     * itself, and give more complete error messages with better location
     * information, at the cost of more processing time.
     * Defaults to the boolean JCS property "ParseXsc.valid", or else "false".
     */
    public boolean valid =
	JCSProperties.getFlag("ParseXsc.valid", false);

    /**
     * Option to supply valid Java names.  Should be switched on for SSC
     * parsing, off when used for non-SSC parsing.
     * Defaults to the boolean JCS property "ParseXsc.jname", or else "true".
     */
    public boolean jname =
	JCSProperties.getFlag("ParseXsc.jname", true);

    public boolean asciiOnly = // option: Java names in flat ASCII?
	JCSProperties.getFlag("ParseXsc.ascii", false);
    public String block = // list of reserved packages, separated by ';'
	JCSProperties.getProperty("ParseXsc.block", "java;sun");
    public boolean ctUse = // use ClassTree for checking?
	JCSProperties.getFlag("ParseXsc.ctUse", true);

    public boolean allowOptionalWithNonZeroMinOccurs =
	JCSProperties.getFlag("ParseXsc.allowOptionalWithNonZeroMinOccurs",
	    false);

    /**
     * Option to register the main template as a local template.
     * This should be switch off for SSC, but on for e.g. DTD-based XSC.
     * Useful when ParseXsc is used by the ETD tester.
     * Defaults to the JCS property "ParseXsc.mtemp", or else to false.
     */
    public boolean mainIsTemp = JCSProperties.getFlag("ParseXsc.mtemp", false);

    /**
     * Option to check that CLASS node has subnodes.
     * Should be switched on for SSC-based XSC, off for DTD/XSD-based XSC.
     * Defaults to the boolean JCS property "ParseXsc.classNeedsChild",
     * or else to "true".
     */
    public boolean classNeedsChild =
	JCSProperties.getFlag("ParseXsc.classNeedsChild", true);

    /**
     * Option to change CLASS node without subnodes to FIELD.
     * Could be switched on for SSC-based XSC.
     * Irrelevant when "classNeedsChild" is set to false.
     * Defaults to the boolean JCS property "ParseXsc.emptyClassIsField",
     * or else to "true".
     */
    public boolean emptyClassIsField =
	JCSProperties.getFlag("ParseXsc.emptyClassIsField", true);

    /**
     * Option to allow boolean attributes to have an empty string as a
     * value, and to interpret that as "false".  This is for backward
     * compatibility with XSC files produced by builder and editor
     * versions that did not properly distinguish between an attribute
     * being absent and having an empty value string.
     * Defaults to the boolean JCS property "ParseXsc.ebool", else to "true".
     */
    public boolean emptyBool =
	JCSProperties.getFlag("ParseXsc.ebool", true);

    public String xscType = // if not empty, mandatory XSC type
	JCSProperties.getProperty("ParseXsc.xscType");

    /**
     * Option to allow multiple delimiters inside a single group.
     * This is not properly supported in the Monk version shipped with 4.5.2.
     * Defaults to the boolean JCS property "ParseXsc.multiDelim", else false.
     */
    public boolean multiDelim =
	JCSProperties.getFlag("ParseXsc.multiDelim", false);

    /**
     * Limit on inner class nesting.  Parsing will fail if this is exceeded.
     * If set to 0, there is no limit.
     * Defaults to the integer JCS property "ParseXsc.depth", else 0.
     */
    public int maxDepth =
	JCSProperties.getInteger("ParseXsc.depth", 0);

    /**
     * Limit on fully qualified generated class name length.  Parsing will
     * fail if this is exceeded.  If set to 0, there is no limit.
     * Defaults to the integer JCS property "ParseXsc.length", else 0.
     */
    public int maxLength =
	JCSProperties.getInteger("ParseXsc.length", 0);

    public boolean debug = debugAll;
    public boolean doCodeVersion = false;

    private SSC_FileSet fileSet;
    private File xscRoot = null;
    public ClassTree ctree = null;

    /**
     * Creates new instance.  In case of resolving external template
     * references, will use xscRoot as the implied root of normalised paths.
     *
     * @param xscRoot  root directory for XSC references
     */
    public ParseXsc (File xscRoot)
    {
	this.fileSet = new SSC_FileSet();
	this.xscRoot = xscRoot;
	if (ctUse)
	{
	    this.ctree = new ClassTree();

	    // Block user from putting anything below a reserved package name.
	    String paths = block;
	    int i;
	    while ((i = paths.indexOf(';')) >= 0)
	    {
		String path = paths.substring(0, i);
		paths = paths.substring(i + 1);
		ctree.block(path);
	    }
	    if (! paths.equals(""))
		ctree.block(paths);
	}
    }

    private boolean recurse = false;

    // Tree node for current file's package. (NYI: move to SSC_File?)
    private ClassTree.PathNode cpack = null;

    /**
     * Parses the given file into an in-core SSC_File object, and
     * adds that to the given file set.  If "recurse" is set, then also
     * follow external template references, and parse those files and
     * add them to the set as well (recursively).
     * If "xscRoot" is set, then "xscFileName" must be relative to it,
     * except when the "abs" parameter is set.
     *
     * @param xscFileName  path of XSC file, normally relative to xscRoot
     * @param recurse  flag: also follow external template references?
     * @param abs  flag: force xscFileName to be absolute?
     * @param prime  flag: is this the prime XSC file (not a global template)?
     * @return the complete set of files reached
     * @throws ParserConfigurationException 
     */
    public SSC_FileSet run (String xscFileName, boolean recurse, boolean abs,
	boolean prime)
	throws IOException, SAXException, ParserConfigurationException
    {
	if (debug)
	    System.out.println("[ run xsc=<" + xscFileName + ">, recurse="
		+ recurse + ", abs=" + abs + " ]");
	this.recurse = recurse;
	File xscFile = new File((abs ? null : xscRoot), xscFileName);

	/* Get a java.io.Reader for the input XML file and
	 * wrap the input file in a SAX input source.
	 */
	InputSource input = null;
	try
	{
	    input = new org.xml.sax.InputSource(new FileInputStream(xscFile));
	}
	catch (FileNotFoundException fnf)
	{
	    throw new RuntimeException("cannot open "
		+ (prime ? "main XSC file" : "external template")
		+ " \"" + xscFileName + "\": " + fnf.getMessage());
	}
	input.setSystemId(xscFileName);
	if (valid)
	{
	    // Run validator in-line.
	    XscCheck xsck = new XscCheck();
	    if (debug) xsck.setDebug(true);
	    xsck.check(null, xscFile, false);
	}
	if (debug)
	    System.out.println("[ ParseXsc.run <" + xscFileName + "> ]");
	// Tell the Xerces parser to parse the input source.
	DocumentBuilderFactory docBuilderFactory =
	    DocumentBuilderFactory.newInstance();
	DocumentBuilder docBuilder = docBuilderFactory.newDocumentBuilder();
	Element root = docBuilder.parse(input).getDocumentElement();
	SSC_File file = new SSC_File(xscFileName);
	fileSet.defineGlobalTemplate(file);
	parseFile(file, root, prime);
	if (debug && ctUse)
	{
	    System.out.println("========< CLASS TREE >========");
	    ctree.print(System.out);
	    System.out.println("========<(-=oO@@Oo=-)>========");
	}
	return fileSet;
    }

    // Flag: is current file not a mere template?
    private boolean isPrime = false;

    /**
     * Parses the XML tree "elem" as an XSC file, and stores the retrieved
     * information in the "file" XSC descriptor.  This does not create any
     * references to the original XML data tree, nor does it use the original
     * input file stream.
     *
     * @param file  extracted XSC info for file
     * @param elem  the SAX document tree for the XSC file
     * @param prime  flag: is this the prime XSC file (not a global template)?
     * @throws IOException for I/O problems
     * @throws SAXException for syntax and semantic errors
     * @throws ParserConfigurationException 
     */
    private void parseFile (SSC_File file, Element elem, boolean prime)
	throws IOException, SAXException, ParserConfigurationException
    {
	if (debug)
	    System.out.println("parse file: " + elem.getNodeName());
	boolean gotMain = false;
	isPrime = prime;
	String etdName = getSafeAttr(elem, "name");
	if (etdName == null)
	    throw new SAXException("file [" + file.getXscFileName()
		+ "] has missing or empty \"name\" value in <etd>");

	file.setGlobalName(etdName);

	/* Note: <etd> attribute "packageName" is deprecated; it should be
	 * specified as attribute "package" in &lt;javaProps&gt;.  Since we
	 * scan the javaProps entity afterwards, it will override the
	 * &lt;etd&gt; value.
	 */
	file.setEtdComment(getSafeAttr(elem, "comment"));
	file.setEtdUid(getUid(file, elem));
	file.setDerived(getBoolAttr(elem, "derived", false));
	file.setEditable(getBoolAttr(elem, "editable", false));
	String value = getTextAttr(elem, "codeVersion");
	file.setCodeVersion((value == null || value.equals("undefined"))
	    ? 0 : Integer.parseInt(value));
	String type = getTextAttr(elem, "type");
	if (xscType == null)
	    xscType = "SSC";
	if (! xscType.equals("") && (type == null || ! type.equals(xscType)))
	{
	    // Wrong kind of XSC file.
	    throw new SAXException("file [" + file.getXscFileName()
		+ "] has missing or wrong \"type\" value in <etd>,"
		+ " should be \"" + xscType + "\"");
	}
	file.setType(type);

	// Default encoding is ASCII, both in <etd> and SSC_File.
	String code = getTextAttr(elem, "sscEncoding");
	file.setEncoding(code);
	if (code != null && debug)
	    System.out.println("[ found encoding \"" + code + "\" ]");

	/* The data encoding (used by the Monk event parser to avoid scanning
	 * for delimiter byte sequences starting halfway in a multi-byte coding
	 * sequence) defaults to the encoding of the original main SSC file.
	 */
	String data = getTextAttr(elem, "dataEncoding");
	file.setDataCode(data);
	if (data != null && debug)
	    System.out.println("[ found datacode \"" + data + "\" ]");

	HashSet tops = new HashSet();
	for (Node child = elem.getFirstChild(); child != null;
	    child = child.getNextSibling())
	{
	    String tag = child.getNodeName();
	    if (tag.equals("javaProps"))
	    {
		// Handle Java properties; optional in principle.
		cpack = parseJavaProps(file, (Element) child);
	    }
	    else if (tag.equals("delimiters"))
	    {
		// Handle global default delimiter list; only for SSC-based XSC.
		parseDelims(file, (Element)child);
	    }
	    else if (tag.equals("class") || tag.equals("node"))
	    {
		/* Local templates, with a special case when the template
		 * is mentioned by the &lt;etd&gt; "name" attribute:
		 * main structure.
		 */
		Element e = (Element)child;
		if (etdName.equals(getSafeAttr(e, "name")))
		{
		    // Main structure; must occur exactly once per file.
		    parseMainTemplate(file, (Element)child, tops);
		    gotMain = true;
		}
		else
		{
		    // Local template; should be used but not recursively.
		    parseLocalTemplate(file, (Element)child, tops);
		}
	    }
	    else if (tag.equals("extra"))
	    {
		//NYI: optional trailing info...
	    }
	    else if (tag.equals("#text"))
	    {
		// Not an element; ignore text data...
	    }
	    else if(tag.equals("#comment")) {
		// Not an element; ignore comments.
	    }
	    else
	    {
		// Unrecognized tag, report it.
		throw new SAXException("unknown entity <" + tag + "> in ETD");
	    }
	}
	file.setPackageName(getTextAttr(elem, "packageName"));
	if (etdName != null)
	{
	    if (! gotMain)
		throw new SAXException("file [" + file.getXscFileName()
		    + "] does not define main template [" + etdName
		    + "], implied by \"name\" attribute of <etd>");
	    String pack = file.getPackageName();
	    if (pack != null)
	    {
		int dot = pack.indexOf('.');
		String top = (dot < 0 ? pack : pack.substring(0, dot));
		if (top.equals(file.getEtd().getJavaName()))
		    throw new SAXException("file [" + file.getXscFileName()
			+ "] package name [" + pack
			+ "] conflicts with root class [" + top + "]");
	    }
	}

	/* Whether we want to generate code for a file or compile it into
	 * a JAR file togehter with the main ETD class depends on the mode
	 * we run in, and on whether the file is an external template, and
	 * whether the code version is known.
	 */
	file.generate = (prime || ! doCodeVersion ||
	    file.getCodeVersion() == 0 ||
	    file.getJavaProps() == null ||
	    file.getJavaProps().getJarFile() == null);
    }

    /**
     * ???
     *
     * @param file  ???
     * @param elem  ???
     * @param tops  ???
     * @throws IOException for ???
     * @throws SAXException for ???
     * @throws ParserConfigurationException 
     */
    private void parseLocalTemplate (SSC_File file, Element elem, HashSet tops)
	throws IOException, SAXException, ParserConfigurationException
    {
	if (debug)
	    System.out.println("parseLocalTemplate: ...");
	String templateName = getSafeAttr(elem, "name");
	// HACK!
	if (templateName.endsWith("-struct"))
	    templateName = templateName.substring(0, templateName.length()-7);
	MessageStructureNode n = parseNode(file, elem, null, false, 0, 0,
	    new HashSet(), tops, null);
	file.defineLocalTemplate(templateName, n);
    }

    /**
     * Checks the fileset to make sure there will be no clashes between
     * the names of the generated Java files.  Each template should have
     * a unique package + root name combination.
     *
     * @throws IOException for Java name clashes
     */
    public void checkClassUnique ()
	throws IOException
    {
	Hashtable map = new Hashtable();
	Iterator iter = fileSet.getFiles();
	while (iter.hasNext())
	{
	    SSC_File ssc = (SSC_File) iter.next();
	    String pack = ssc.getPackageName();
	    if (pack == null)
		pack = "";
	    if (! pack.equals("") && ! pack.endsWith("."))
		pack = pack + ".";
	    String fqName = pack + ssc.getEtd().getJavaName();
	    SSC_File also = (SSC_File) map.get(fqName);
	    if (also != null)
	    {
		// Found name conflict.
		throw new IOException("class [" + fqName
		    + "] is generated by both [" + also.getXscFileName()
		    + "] and [" + ssc.getXscFileName() + "]");
	    }
	    map.put(fqName, ssc);
	}
    }

    /**
     * Checks the fileset to make sure all XSC files included except the
     * main file have been processed by the back-end before.  This means
     * checking all external templates have the "codeAvailable" flag in
     * &lt;etd&gt; set, meaning we have regenerated XSC and SSC files available.
     *
     * @throws IOException for unripe external templates
     */
    public void checkCodeAvailable ()
	throws IOException
    {
	SSC_File main = fileSet.resolveGlobalTemplate(null);
	if (main == null)
	    throw new RuntimeException("no main XSC file");
	Iterator iter = fileSet.getFiles();
	while (iter.hasNext())
	{
	    SSC_File ssc = (SSC_File) iter.next();
	    if (ssc != main && ! ssc.getCodeAvail())
		throw new IOException("external template ["
		    + ssc.getXscFileName() + "] not compiled yet");
	}
    }

    /**
     * Checks the fileset for the use of global default delimiter use.
     * If there would be too few global default delimiters, throw an
     * exception reporting the first field that would lack a delimiter.
     * This also does a check against cycles in the tree of references.
     *
     * @throws SAXException for a lack of delimiters
     */
    public void checkDefaultDelimiters ()
	throws SAXException
    {
	SSC_File main = fileSet.resolveGlobalTemplate(null);
	if (main == null)
	    throw new RuntimeException("no main XSC file");
	Xsc.Delim dels = main.getDelimiters();
	checkDefaultDelimiters(main, main.getEtd(), dels, 0, "",
	    new HashSet());
    }

    /**
     * Checks the given node for the use of global default delimiter use.
     * If there would be too few global default delimiters, throw an
     * exception reporting the first field that would lack a delimiter.
     * Note that this is not secured against endless recursion, so a
     * cycle check must have been done before.
     * The "used" parameter indicates how many global delimiters have been
     * used already, so dels[used] is the next to consume; a special case
     * is used=-1, which means we haven't looked at the ETD root node yet.
     *
     * @param file  the XSC file of the node
     * @param node  the node to check
     * @param dels  the global default delimiter list, from here on
     * @param used  how many delimiters consumed already
     * @param path  path down node hierarchy
     * @param ancs  the set of ancestor nodes (for recursion check)
     * @throws SAXException for a lack of delimiters
     */
    private void checkDefaultDelimiters (SSC_File file,
	MessageStructureNode node, Xsc.Delim dels, int used, String path,
	HashSet ancs)
	throws SAXException
    {
	if (node == null)
	    return;
	path = path + node.getNodeName();
	Xsc.Delim local = node.getDelim();
	if (ancs.contains(node))
	    throw new SAXException("cyclical reference to ["
		+ node.getNodeName() + "], full path is [" + path + "]");
	ancs.add(node);
	int type = node.getNodeType();
	if ((type & node.REFERENCE) != 0)
	{
	    // External template reference.
	    String ref = node.getRefPath();
	    if (ref == null)
		throw new RuntimeException("external reference node [" + path
		    + "] has no file-path");
	    SSC_File extt = fileSet.resolveGlobalTemplate(ref);
	    if (extt == null)
		throw new RuntimeException("template [" + ref
		    + "] from node [" + path + "] not in fileset");
	    checkDefaultDelimiters(extt, extt.getEtd(), dels, used,
		path + "->", ancs);
	    ancs.remove(node);
	    return;
	}
	if ((type & node.LOCAL_REF) != 0)
	{
	    // Internal template reference.
	    String link = node.getLink();
	    if (link == null)
		throw new RuntimeException("internal reference node [" + path
		    + "] has no link");
	    MessageStructureNode intt = file.resolveLocalTemplate(link);
	    if (intt == null)
		throw new RuntimeException("template [" + link
		    + "] from node [" + path + "] not in locals");
	    checkDefaultDelimiters(file, intt, dels, used, path + "->", ancs);
	    ancs.remove(node);
	    return;
	}

	if ((type & node.MAIN_ROOT) != 0)
	{
	    // Root node of ETD, does not consume.
	}
	else if ((type & node.DELIM) != 0)
	{
	    // Plain delimited node.
	    boolean need = (local == null || local.isArray());

	    // Make sure next delimiter is not an array delimiter.
	    if (need &&		  // we have to have one
		(dels == null	  // we've run out of delims
		    || dels.isArray() // next delim is wrong kind
		)
	    )
	    {
			String values[]={path,used+""};
			throw new SAXException(CommonMsg.getMsg("GLOBAL_PLAIN_DEL_NOT_SPECIFIED",values));

		}

	    // Okay.
	    if (dels != null) { dels = dels.getNext(); }
	    used ++;

	    // Skip subsequent array delimiter.
	    if (dels != null && dels.isArray())
		{ dels = dels.getNext(); used ++; }
	}
	else if ((type & node.ARRAY) != 0)
	{
	    // Array delimited node.
	    boolean need = (local == null || ! local.isArray());

	    // Skip delimiter if not an array.
	    if (dels != null && ! dels.isArray())
		{ dels = dels.getNext(); used ++; }

	    // Make sure this one is an array.
	    if (need && (dels == null || ! dels.isArray()))
	    {
		// No array delimiter at this level.
		throw new SAXException("no default array delimiter for ["
		    + path + "], default index = " + used);
	    }

	    // Okay.
	    if (dels != null) { dels = dels.getNext(); }
	    used ++;
	}

	MessageStructureNode child = node.getFirstChild();
	if (child != null)
	{
	    // Do subnodes.
	    path = path + ".";
	    for (; child != null; child = child.getNextSibling())
		checkDefaultDelimiters(file, child, dels, used, path, ancs);
	}
	ancs.remove(node);
    }

    /**
     * Given an XML entity, returns the string value of the named attribute.
     * If the attribute is not present, returns null instead.
     *
     * @param elem  XML entity
     * @param name  attribute name
     * @return attribute value, or null
     */
    private String getAttribute (Element elem, String name)
    {
	Attr attr = elem.getAttributeNode(name);
	return (attr == null)
	    ? null
	    : attr.getValue();
    }

    /**
     * Given an XML entity, returns the boolean value of the named attribute.
     * If the attribute is not present, returns the default value instead.
     * The value, if present, must be "true" or "false".
     * If global "emptyBool" is set, permit empty attribute values as defaults.
     *
     * @param elem  XML entity
     * @param name  attribute name
     * @param deft  default value
     * @return boolean value
     */
    private boolean getBoolAttr (Element elem, String name, boolean deft)
    {
	Attr attr = elem.getAttributeNode(name);
	if (attr == null)
	    return deft;
	String value = attr.getValue();
	if (value == null || (emptyBool && value.equals("")))
	    return deft;
	if (value.equals("true"))
	    return true;
	if (value.equals("false"))
	    return false;
	throw new RuntimeException("invalid value [" + value
	    + "] for boolean \"" + name + "\" attribute of <"
	    + elem.getNodeName() + "> entity");
    }

    /**
     * Given an XML entity, returns the string value of the named attribute.
     * Assumes the attribute is plain text (not normal-safe encoded).
     * If the attribute is empty or not present, returns null instead.
     *
     * @param elem  XML entity
     * @param name  attribute name
     * @return non-empty string value
     */
    private String getTextAttr (Element elem, String name)
    {
	Attr attr = elem.getAttributeNode(name);
	if (attr == null) { return null; }
	String value = attr.getValue();
	return (value == null || "".equals(value)) ? null : value;
    }

    /**
     * Given an XML entity, returns the string value of the named attribute.
     * Assumes the attribute is normal-safe encoded.
     * If the attribute is empty or not present, returns null instead.
     *
     * @param elem  XML entity
     * @param name  attribute name
     * @return non-empty string value
     */
    private String getSafeAttr (Element elem, String name)
    {
	Attr attr = elem.getAttributeNode(name);
	if (attr == null) { return null; }
	String value = attr.getValue();
	return (value == null || "".equals(value)) ? null : uDecode(value);
    }

    /**
     * Parse the main (top-level) local template of an XSC file.
     *
     * @param file  ???
     * @param elem  XML entity
     * @param tops  ???
     * @throws IOException for ???
     * @throws SAXException for ???
     * @throws ParserConfigurationException 
     */
    private void parseMainTemplate (SSC_File file, Element elem, HashSet tops)
	throws IOException, SAXException, ParserConfigurationException
    {
	String globalName = getSafeAttr(elem, "name");
	file.setGlobalName(globalName);
	MessageStructureNode n = parseNode(file, elem, null, true, 0, 0,
	    new HashSet(), tops, null);
	if (mainIsTemp)
	    file.defineLocalTemplate(n.getNodeName(), n);
	if (debug)
	{
	    System.out.println("setting etd: " + n.getNodeName());
	    System.out.println("  global name: " + globalName);
	}
	file.setEtd(n);
    }

    /**
     * Class used for call-backs from ClassTree when problems are found.
     */
    public static class NodeUser implements ClassTree.PathUser
    {
	private SSC_File file;
	private MessageStructureNode node;

	/**
	 * Constructs as a reference to an XSC file.
	 *
	 * @param file  the XSC file
	 */
	public NodeUser (SSC_File file)
	{
	    this.file = file;
	    this.node = null;
	}

	/**
	 * Constructs as a reference to an XSC node.
	 *
	 * @param node  the XSC node
	 */
	public NodeUser (MessageStructureNode node)
	{
	    this.file = node.getFile();
	    this.node = node;
	}

	/**
	 * Returns a string representing this reference.
	 * This method is required by the "PathUser" interface.
	 *
	 * @param pnode  the PathNode instance with a problem (not used here)
	 * @return a string describing this file
	 */
	public String getOrigin (PathNode pnode)
	{
	    return (node == null
		    ? ""
		    : ("node [" + fullName(node, false) + "] in ")
		)
		+ "XSC file \"" + file.getXscFileName() + '"';
	}

	/**
	 * Retrieves the node association, if any.
	 *
	 * @return the associated MessageStructureNode
	 */
	public MessageStructureNode getNode ()
	{
	    return node;
	}

	/**
	 * Retrieves the file association, if any.
	 *
	 * @return the associated SSC_File
	 */
	public SSC_File getFile ()
	{
	    return file != null
		? file
		: (node != null ? node.getFile() : null);
	}
    }

    /**
     * Parses a &lt;jar&gt; entity, which describes an extra JAR file.
     * This entity is currently only used for derived ETDs.
     *
     * @param file  the XSC file info, for UID generation
     * @param jprops  the Java-specific XSC info we're constructing
     * @param elem  the XML input to convert
     */
    private void parseJar (SSC_File file, Xsc.JavaProps jprops, Element elem)
    {
	String jarf, comm, guid;

	verifyTag(elem, "jar", null);
	jarf = getSafeAttr(elem, "file");
	comm = getSafeAttr(elem, "comment");
	guid = getUid(file, elem);
	if (jarf == null)
	    throw new RuntimeException("<jar> lacks \"file\" attribute");
	jprops.addJar(new Xsc.Jar(comm, guid, jarf));
    }

    /**
     * Parses an &lt;interface&gt; entity; currently a dummy...
     *
     * @param elem  the XML input to convert
     */
    private void parseInterface (Element elem)
    {
	verifyTag(elem, "interface", null);
    }

    /**
     * Parses the &lt;javaProps&gt; entity, which describes the Java details.
     * This entity may be absent for newly-created XSC files.
     *
     * Syntax:
     * <code>
     *  JPROPS ::= &lt;javaProps package=NAME [class=NAME]
     *	       [codeAvailable=FLAG] [jarFile=FILE]
     *	       [comment=STRING]&gt; {INTERF} &lt;/javaProps&gt;
     *  INTERF ::= &lt;interface&gt; {METHOD} &lt;/interface&gt;
     *  METHOD ::= &lt;method name=NAME returnType=NAME signature=STRING
     *	       [comment=STRING] {MPARAM} &lt;/method&gt;
     *	MPARAM ::= &lt;param name=NAME type=NAME [comment=STRING]/>
     * </code>
     *
     * @param file  the XSC info we're constructing
     * @param jprops  the SAX-tree of the XSC input for &lt;javaProps&gt;
     * @return the class tree node for the package, or null if no package
     */
    private PathNode parseJavaProps (SSC_File file, Element jprops)
    {
	boolean code;
	String pack, base, jarf, wrap, comm, guid;
	PathNode cpack = null;

	code = getBoolAttr(jprops, "codeAvailable", true);
	pack = getTextAttr(jprops, "package");
	base = getTextAttr(jprops, "class");
	jarf = getSafeAttr(jprops, "jarFile");
	wrap = getSafeAttr(jprops, "source");
	comm = getSafeAttr(jprops, "comment");
	guid = getUid(file, jprops);
	file.setJavaProps(new Xsc.JavaProps(comm, guid, code, pack,
	    base, jarf, wrap));
	//-interfaces NYI...

	/* The package name should be set here.  For backward compatibility,
	 * we also accept the "packageName" attribute of the &lt;etd&gt; node;
	 * if both are present, the &lt;javaProps&gt; "package" attribute
	 * will prevail.
	 */
	if (ctUse)
	    cpack = ctree.findPackage(new NodeUser(file), pack);

	// Check the interfaces.
	for (Node child = jprops.getFirstChild();
	    child != null; child = child.getNextSibling())
	{
	    if (child instanceof Element)
	    {
		if (((Element) child).getTagName().equals("jar"))
		    parseJar(file, file.getJavaProps(), (Element) child);
		else
		    parseInterface((Element) child);
	    }
	}
	return cpack;
    }

    /**
     * Looks up "uid" attribute in given element.
     * If found, registers numeric UID with the given container file.
     *
     * @param elem  the XML element
     * @param file  the container
     */
    private String getUid (SSC_File file, Element elem)
    {
	String uid = getTextAttr(elem, "uid");
	file.seenUid(uid);
	return uid;
    }

    /**
     * Verifies that the given XML node has the given generic ID.
     *
     * @param elem  an XSC element in XML form
     * @param need  required tag
     * @param
     */
    private void verifyTag (Element elem, String need, String also)
    {
	String tag = elem.getTagName();
	if (! (tag.equals(need) || tag.equals(also)))
	    throw new RuntimeException("expected <" + need
		+ (also == null ? "" : ("> or <" + also))
		+ ">, but got <" + tag + "> instead");
    }

    /**
     * Parses the &lt;param&gt; entity, which describes a Java method part.
     *
     * @param file  the XSC file info, for UIDs
     * @param elem  the SAX-tree of the XSC input for &lt;param&gt;
     * @return the parameter description
     */
    private Xsc.Param parseParam (SSC_File file, Element elem)
    {
	verifyTag(elem, "param", null);
	Xsc.Delim delims = null;
	String name = getTextAttr(elem, "name");
	String type = getTextAttr(elem, "paramType");
	String comm = getSafeAttr(elem, "comment");
	String guid = getUid(file, elem);
	return new Xsc.Param(comm, guid, name, type);
    }

    /**
     * Parses the &lt;throws&gt; entity, which describes a Java exception.
     *
     * @param file  the XSC file info, for UIDs
     * @param elem  the SAX-tree of the XSC input for &lt;throws&gt;
     * @return the parameter description
     */
    private Xsc.Throws parseThrows (SSC_File file, Element elem)
    {
	verifyTag(elem, "throws", null);
	Xsc.Delim delims = null;
	String type = getTextAttr(elem, "excepType");
	String comm = getSafeAttr(elem, "comment");
	String guid = getUid(file, elem);
	return new Xsc.Throws(comm, guid, type);
    }

    /**
     * Parses the &lt;method&gt; entity, which describes a Java method.
     *
     * @param file  the XSC file info, for UIDs
     * @param elem  the SAX-tree of the XSC input for &lt;method&gt;
     * @return the method description
     */
    private Xsc.Method parseMethod (SSC_File file, Element elem)
    {
	verifyTag(elem, "method", null);
	Xsc.Delim delims = null;
	String name = getTextAttr(elem, "name");
	String type = getTextAttr(elem, "returnType");
	String comm = getSafeAttr(elem, "comment");
	String guid = getUid(file, elem);
	Xsc.Method method = new Xsc.Method(comm, guid, name, type);
	for (Node child = elem.getFirstChild();
	    child != null; child = child.getNextSibling())
	{
	    if (child instanceof Element)
	    {
		if (((Element) child).getTagName().equals("param"))
		    method.addParam(parseParam(file, (Element) child));
		else
		    method.addThrows(parseThrows(file, (Element) child));
	    }
	}
	return method;
    }

    /**
     * Parses the &lt;delimiters&gt; entity, which describes the default
     * delimiters.  This entity is only supported for SSC-based XSC.
     *
     * @param file  the XSC file info, for UIDs
     * @param elem  the SAX-tree of the XSC input for &lt;delimiters&gt;
     */
    private void parseDelims (SSC_File file, Element elem)
    {
	int levelNo = 0;
	verifyTag(elem, "delimiters", null);
	Xsc.Delim delims = null;
	file.setDelimsComment(getSafeAttr(elem, "comment"));
	file.setDelimsUid(getUid(file, elem));
	for (Node child = elem.getFirstChild();
	    child != null; child = child.getNextSibling())
	{
	    if (child instanceof Element)
	    {
		Xsc.Delim delim = parseDelim(file, (Element) child,
		    null, ++ levelNo);
		if (delim == null)
		    throw new RuntimeException("parseDelims: "
			+ delimLoc(file, null, levelNo, 0, 0)
			+ ": empty delimiter definition");
		delims = delim.appendTo(delims);
	    }
	}
	file.setDelimiters(delims);
    }

    /**
     * Parses the &lt;delim&gt; entity, which describes a single
     * level of delimiters.  This entity is only supported for SSC-based XSC.
     * From XSC 0.5 onwards, the syntax of &lt;delim&gt; has changed, and it
     * also subsumes the role of local delimiter attributes formerly in
     * &lt;node&gt;.
     *
     * @param file  the XSC file info, for UIDs
     * @param elem  the SAX-tree of the XSC input for &lt;delim&gt;
     * @param node  even node, for error messages (null=global)
     * @param levelNo  level number, for error messages (0=local)
     * @return the delimiter level, or null if empty
     */
    private Xsc.Delim parseDelim (SSC_File file, Element elem,
	MessageStructureNode node, int levelNo)
    {
	int groupNo = 0;
	verifyTag(elem, "delim", null);
	byte type = 0;
	if (getBoolAttr(elem, "endOfRec", false))
	    type |= Xsc.Delim.ENDOFREC;
	if (getBoolAttr(elem, "required", false))
	    type |= Xsc.Delim.REQUIRED;
	if (getBoolAttr(elem, "separator", false))
	    type |= Xsc.Delim.SEPARATOR;
	if (getBoolAttr(elem, "array", false))
	    type |= Xsc.Delim.ARRAY;
	if (getBoolAttr(elem, "anchored", false))
	    type |= Xsc.Delim.ANCHORED;
	if (getBoolAttr(elem, "beginAnchored", false))
	    type |= Xsc.Delim.BEGINANCH;
	if (getBoolAttr(elem, "endAnchored", false))
	    type |= Xsc.Delim.ENDANCH;

	String comm = getSafeAttr(elem, "comment");
	String guid = getUid(file, elem);
	Xsc.Delim delim = new Xsc.Delim(comm, guid, type);

	for (Node child = elem.getFirstChild();
	    child != null; child = child.getNextSibling())
	  {
	    if (child instanceof Element)
	      if (((Element) child).getTagName().equals("escape"))
		delim.addEscape(parseEscape(file, (Element) child,
					    node, levelNo, ++ groupNo));
	      else
		parseDelimGroup(file, (Element) child, node,
				levelNo, ++ groupNo, delim);
  
	  }

	/* Delimiters were specified as PCDATA in XSC 0.2;
	 * in XSC 0.3, the value should normally be specified as
	 * a "value" attribute.  For backward compatibility, we
	 * first check if we have a "value" attribute, and if not,
	 * then we take the PCDATA.
	 * In XSC 0.5, delimiter values are stored in separate
	 * <beginDelim> and <endDelim> entities.
	 */
	String beg = getSafeAttr(elem, "beginDelim");
	String end = getSafeAttr(elem, "endDelim");
	if (end == null)
	    end = getSafeAttr(elem, "value");
	if (beg != null || end != null)
	{
	    // Pre-XSC 0.5 style delimiter info; transform to first group.
	    Xsc.DelimGroup group = new Xsc.DelimGroup(null, null);
	    if (beg != null)
		group.addOne(true,
		    new Xsc.OneDelim(null, null, null, beg, "UTF-8"));
	    if (end != null)
		group.addOne(false,
		    new Xsc.OneDelim(null, null, null, end, "UTF-8"));
	    group.setNext(delim.getGroups());
	    delim.setGroups(group);
	}
	return (type == 0 && delim.getGroups() == null)
	    ? null // empty hull
	    : delim;
    }

    /**
     * Parses the &lt;delimGroup&gt; entity, which describes a group of
     * begin/end delimiters.  This entity is only supported for SSC-based XSC.
     * Aside from the usual comment and UID baggage, a group has no attributes,
     * but just serves as a container for a list of begin/end delimiters.
     *
     * @param file  the XSC file info, for UIDs
     * @param elem  the SAX-tree of the XSC input for &lt;delimGroup&gt;
     * @param node  even node, for error messages (null=global)
     * @param levelNo  level number, for error messages (0=local)
     * @param groupNo  group number, for error messages
     * @return the delimiter group
     */
    private Xsc.DelimGroup parseDelimGroup (SSC_File file, Element elem,
	MessageStructureNode node, int levelNo, int groupNo, Xsc.Delim delim)
    {
	verifyTag(elem, "delimGroup", null);
	String		comm		= getSafeAttr(elem, "comment");
	String		guid		= getUid(file, elem);
	Xsc.DelimGroup	group		= new Xsc.DelimGroup(comm, guid);
	Xsc.OneDelim	beginDelim	= null;
	Xsc.OneDelim	endDelim	= null;

	int partNo = 0, begins = 0, ends = 0;

	for (Node child = elem.getFirstChild();
	    child != null; child = child.getNextSibling())
	{
	    if (child instanceof Element)
	    {
		Element one = (Element) child;
		if ("beginDelim".equals(one.getTagName()))
		{
		    begins ++;
		    beginDelim = parseOneDelim(file, one,
					       node, levelNo, groupNo, ++ partNo);
		}
		else
		{
		    ends ++;
		    endDelim = parseOneDelim(file, one,
					     node, levelNo, groupNo, ++ partNo);
		}
	    }
	}
	if (null != delim.getGroups() &&
	    (0 == begins || 0 == ends))	// Modifies the default pairing.
	  {
	    group = delim.getGroups();
	  }
	else				// Else have new pairing.
	  {
	    delim.addGroup(group);
	  }
	if (null != beginDelim)
	  group.setBegins(beginDelim);
	if (null != endDelim)
	  group.setEnds(endDelim);
	if (! multiDelim && (begins > 1 || ends > 1))
	    throw new RuntimeException("parseDelimGroup: "
		+ delimLoc(file, node, levelNo, groupNo, 0)
		+ ": multiple begin/end delimiters (" + begins + "," + ends
		+ ") in one <delimGroup> not supported in this version");
	return group;
    }

    /**
     * Parses a &lt;beginDelim&gt; or &lt;endDelim&gt; entity, which
     * describes single begin/end delimiter.
     * This entity is only supported for SSC-based XSC.
     *
     * @param file  the XSC file info, for UIDs
     * @param elem  the SAX-tree of the XSC input for &lt;delimGroup&gt;
     * @param node  even node, for error messages (null=global)
     * @param levelNo  level number, for error messages (0=local)
     * @param groupNo  group number, for error messages
     * @param partNo  group part number, for error messages
     * @return the delimiter
     */
    private Xsc.OneDelim parseOneDelim (SSC_File file, Element elem,
	MessageStructureNode node, int levelNo, int groupNo, int partNo)
    {
	String tag = elem.getTagName();
	if (! (tag.equals("beginDelim") || tag.equals("endDelim")))
	    throw new RuntimeException("parseOneDelim: "
		+ delimLoc(file, node, levelNo, groupNo, partNo)
		+ ": expected <beginDelim>"
		+ " or <endDelim>, got <" + tag + "> instead");

	String comm = getSafeAttr(elem, "comment");
	String guid = getUid(file, elem);
	String data = getSafeAttr(elem, "bytes");
	String text = getSafeAttr(elem, "value");
	String code = getSafeAttr(elem, "encoding");
	String leng = getTextAttr(elem, "length");
	String offs = getTextAttr(elem, "offset");
	boolean begin = tag.equals("beginDelim");
	Xsc.OneDelim one;
	if (leng == null || leng.equals("undefined"))
	{
	    // Explicit delimiter.
	    if (! (offs == null || offs.equals("undefined")))
		throw new RuntimeException("parseOneDelim: "
		    + delimLoc(file, node, levelNo, groupNo, partNo)
		    + ": <" + tag + "> has offset but no length value");
	    byte[] buf = null;
	    if (text == null)
	    {
		// String not given, convert from bytes.
		if (data == null)
		    throw new RuntimeException(
			delimLoc(file, node, levelNo, groupNo, partNo)
			+ ": explicit delimiter without value or bytes");
		String jenc = (code == null ? file.getEncoding() : code);
		int n = data.length();
		buf = new byte[n];
		for (int i = 0; i < n; i ++)
		{
		    int c = data.charAt(i);
		    if (c < 0 || 0xFF < c)
			throw new RuntimeException(
			    delimLoc(file, node, levelNo, groupNo, partNo)
			    + ": delimiter byte value out of range 0-255: "
			    + c);
		    buf[i] = (byte) c;
		}
		try
		{
		    text = StringCoderFactory.getStringCoder(jenc).decode(buf);
		}
		catch (UnsupportedEncodingException ue)
		{
		    throw new RuntimeException(
			delimLoc(file, node, levelNo, groupNo, partNo)
			+ ": cannot convert delimiter with ["
			+ jenc + "] encoding: " + ue.getMessage());
		}
	    }
	    one = new Xsc.OneDelim(comm, guid, buf, text, code);
	}
	else
	{
	    // Implicit delimiter (a.k.a. encoded delimiter).
	    if (offs == null || offs.equals("undefined"))
		offs = "0";
	    if (data != null ||
		text != null ||
		(code != null && ! code.equals("ASCII") &&
		    ! code.equals("US-ASCII")))
	    {
		throw new RuntimeException("parseOneDelim: "
		    + delimLoc(file, node, levelNo, groupNo, partNo)
		    + ": <" + tag + "> cannot combine length attribute with "
		    + "value, encoding, or bytes attributes");
	    }
	    one = new Xsc.OneDelim(comm, guid,
		Integer.parseInt(leng),
		Integer.parseInt(offs));
	}

	for (Node child = elem.getFirstChild();
	    child != null; child = child.getNextSibling())
	{
	    if (child instanceof Element)
		throw new RuntimeException("parseOneDelim: unexpected <"
		    + ((Element) child).getTagName()
		    + "> below <" + tag + ">; "
		    + delimLoc(file, node, levelNo, groupNo, partNo));
	}
	return one;
    }

    /**
     * Parses a &lt;escape&gt entity, which
     * describes single escape delimiter.
     * This entity is only supported for SSC-based XSC.
     *
     * @param file  the XSC file info, for UIDs
     * @param elem  the SAX-tree of the XSC input for &lt;delim&gt;
     * @param node  even node, for error messages (null=global)
     * @param levelNo  level number, for error messages (0=local)
     * @param groupNo  group number, for error messages
     * @param partNo  group part number, for error messages
     * @return the delimiter
     */
    private Xsc.Escape parseEscape (SSC_File file, Element elem,
	MessageStructureNode node, int levelNo, int groupNo)
    {
	String tag = elem.getTagName();
	int partNo = 0;
	if (! (tag.equals("escape")))
	    throw new RuntimeException("parseEscape: "
		+ delimLoc(file, node, levelNo, groupNo, partNo)
		+ ": expected <escape>, got <" + tag + "> instead");

	String comm = getSafeAttr(elem, "comment");
	String guid = getUid(file, elem);
	String data = getSafeAttr(elem, "bytes");
	String text = getSafeAttr(elem, "value");
	String code = getSafeAttr(elem, "encoding");
	Xsc.Escape escape;
	byte[] buf = null;
	if (text == null)
	  {
	    // String not given, convert from bytes.
	    if (data == null)
	      throw new RuntimeException(
			 delimLoc(file, node, levelNo, groupNo, partNo)
			 + ": escape delimiter without value or bytes");
	    String jenc = (code == null ? file.getEncoding() : code);
	    int n = data.length();
	    buf = new byte[n];
	    for (int i = 0; i < n; i ++)
	      {
		int c = data.charAt(i);
		if (c < 0 || 0xFF < c)
		  throw new RuntimeException(
			    delimLoc(file, node, levelNo, groupNo, partNo)
			    + ": escape delimiter byte value out of range 0-255: "
			    + c);
		buf[i] = (byte) c;
	      }
	    try
	      {
		text = StringCoderFactory.getStringCoder(jenc).decode(buf);
	      }
	    catch (UnsupportedEncodingException ue)
	      {
		throw new RuntimeException(
			delimLoc(file, node, levelNo, groupNo, partNo)
			+ ": cannot convert delimiter with ["
			+ jenc + "] encoding: " + ue.getMessage());
	      }
	  }
	escape = new Xsc.Escape(comm, guid, buf, text, code);

	for (Node child = elem.getFirstChild();
	    child != null; child = child.getNextSibling())
	{
	    if (child instanceof Element)
		throw new RuntimeException("parseEscape: unexpected <"
		    + ((Element) child).getTagName()
		    + "> below <" + tag + ">; "
		    + delimLoc(file, node, levelNo, groupNo, partNo));
	}
	return escape;
    }

    // Ordinal number.
    public String nth (int n)
    {
	switch (n % 10)
	{
	case 1:  return "" + n + "st";
	case 2:  return "" + n + "nd";
	case 3:  return "" + n + "rd";
	default: return "" + n + "th";
	}
    }

    /**
     * Constructs detailed delimiter identification for error message.
     *
     * @param file  XSC file (null=unknown)
     * @param node  event node (null=global)
     * @param levelNo  level number (0=local)
     * @param groupNo  group number
     * @param partNo  group part number
     * @return location string
     */
    private String delimLoc (SSC_File file, MessageStructureNode node,
	int levelNo, int groupNo, int partNo)
    {
	StringBuffer buf = new StringBuffer();
	if (file != null)
	    buf.append(", file [" + file.getXscFileName() + "]");
	if (node != null)
	    buf.append(", local delimiter of [" + node.getNodeName() + "]");
	if (levelNo > 0)
	    buf.append(", " + nth(levelNo) + " global delimiter level");
	if (groupNo > 0)
	    buf.append(", " + nth(groupNo) + " group");
	if (partNo > 0)
	    buf.append(", " + nth(partNo) + " part");
	return buf.toString().substring(2);
    }

    // A list of all accepted Java types for leaf nodes.
    static private HashSet leafTypes = null;

    /**
     * Tests if the given string is the name of one of the supported Java
     * types for leaf nodes, i.e. not generating a separate class.
     *
     * @param type  the Java type name
     * @return the test result
     */
    private boolean isLeafType (String type)
    {
	if (leafTypes == null)
	{
	    // Initialise the set.
	    leafTypes = new HashSet();
	    leafTypes.add("byte[]");
	    leafTypes.add("byte");
	    leafTypes.add("short");
	    leafTypes.add("int");
	    leafTypes.add("long");
	    leafTypes.add("float");
	    leafTypes.add("double");
	    leafTypes.add("boolean");
	    leafTypes.add("char");
	    leafTypes.add("String");
	    leafTypes.add("java.lang.String");
	}
	return leafTypes.contains(type);
    }

    /**
     * Concatenates all ancestors of a node to give the full name.
     *
     * @param node  the node whose name we want
     * @param java  flag: want Java names, not original ones?
     * @return the ancestors list and node name, separated by "."
     */
    public static String fullName (MessageStructureNode node, boolean java)
    {
	if (node == null) { return null; }
	String prefix = fullName(node.getParent(), java);
	return (prefix == null ? "" : (prefix + "."))
	    + (java ? node.getJavaName() : node.getNodeName());
    }

    /**
     * Flag to indicate whether there was a mismatch between any versioned
     * external template reference and the template's actual version.
     */
    public boolean codeVersionMismatch = false;

    /**
     * Parses a &lt;class&gt; or &lt;node&gt; entity, which can represent a
     * leaf node, a composite node, a reference, and a few other things.
     * Note that &lt;node type="METHOD"/type="PARAM" ...&gt; is deprecated,
     * and one should use &lt;method&gt; and &lt;param&gt; entities instead.
     *
     * @param container  the XSC file
     * @param node  the SAX XML info for this node
     * @param isMain  flag: is this the main template?
     * @param depth  the node nesting depth; 0=top
     * @param ancestorJavaNames  the set of Java names of its ancestors
     * @param siblingJavaNames  the Java names of its siblings seen so far
     * @param parentType  the fully qualified Java name of its parent, or null
     * @throws ParserConfigurationException 
     */
    private MessageStructureNode parseNode (SSC_File container, Element node,
	MessageStructureNode parent, boolean isMain, int depth, int length,
	HashSet ancestorJavaNames, HashSet siblingJavaNames, String parentName)
	throws IOException, SAXException, ParserConfigurationException
    {
	// Ignore METHOD and PARAM nodes.
	String tag = node.getTagName();
	if (! (tag.equals("class") || tag.equals("node")))
	    throw new RuntimeException("parseNode: expected <node>, got <"
		+ tag + "> instead");

	if (debug)
	    System.out.println("parseNode: ...");

	String value = getSafeAttr(node, "name");
	if (value == null)
	    throw new RuntimeException("parseNode: node has no name");
	MessageStructureNodeImpl n = new MessageStructureNodeImpl(parent,
	    value);
	n.setFile(container);
	if (debug)
	    System.out.println(" name=" + value);

	int nameLength = 0;
	String nick = getTextAttr(node, "nickName");
	if (nick != null)
	{
	    if (SscConv.seenJavaId(nick, ancestorJavaNames, siblingJavaNames))
		throw new SAXException("alternate name [" + nick
		    + "] of node [" + n.getNodeName()
		    + "] clashes with sibling/ancestor names");
	    n.setNickName(nick);
	    nameLength = nick.length();
	}

	value = getTextAttr(node, "javaName");
	if (debug)
	    System.out.println("[ - node <" + n.getNodeName()
		+ "> has old javaName <"
	    + (value == null ? "-none-" : value) + "> ]");
	if (jname)
	{
	    // Generate proper Java name if necessary.
	    if (value == null ||
		value.equals(n.getNodeName()) ||
		! SscConv.isJavaId(value) ||
		SscConv.seenJavaId(value, ancestorJavaNames, siblingJavaNames))
	    {
		// Value absent or not unique; make new Java name.
		value = SscConv.makeJavaId(n.getNodeName(),
		    ancestorJavaNames, siblingJavaNames, asciiOnly);
		if (depth == 0 && nick != null && ! nick.equals(value))
		{
		    /* Very special case: if the root node has a nickname
		     * that differs from the Java name, then we need to
		     * generate an alternate class.  However, the alternate
		     * class must not clash with case-insensitive comparison
		     * with the Java name, else the *.class file names would
		     * conflict on Windows, so we want to do full
		     * disambiguation on the Java name with respect to the
		     * nickname.  If the two would be *identical* (including
		     * case), however, then we do not need the alternate
		     * class...
		     */
		    SscConv.addJavaId(nick, siblingJavaNames);
		    if (SscConv.seenJavaId(value, null, siblingJavaNames))
		    {
			// Redisambiguation required.
			value = SscConv.makeJavaId(n.getNodeName(),
			    ancestorJavaNames, siblingJavaNames, asciiOnly);
		    }
		}
		if (debug)
		    System.out.println("[ -- made new javaName <"
			+ value + "> ]");
	    }
	    SscConv.addJavaId(value, siblingJavaNames);
	}
	else
	{
	    // Suppress normal Java name repair.
	    if (value == null)
		value = n.getNodeName();
	}
	n.setJavaName(value);
	nameLength = Math.max(nameLength, value.length());
	if (nick != null && ! nick.equals(value))
	{
	    // We have a separate nickname, register as a Java name.
	    SscConv.addJavaId(nick, siblingJavaNames);
	}
	length += 1 + nameLength;
	if (maxDepth > 0 && depth > maxDepth)
	    throw new RuntimeException("[ node <" + fullName(n, true)
		+ "> exceeds maximum nesting depth (" + maxDepth + ") ]");
	if (maxLength > 0 && length > maxLength)
	    throw new RuntimeException("[ node <" + fullName(n, true)
		+ "> exceeds maximum name length (" + maxLength + ") ]");

	int mask = n.DATA_NODE;
	if (depth == 0)
	{
	    mask |= n.TEMP_ROOT;
	    if (isMain)
	    {
		mask |= n.FILE_ROOT;
		if (isPrime)
		    mask |= n.MAIN_ROOT;
	    }
	}

	value = getTextAttr(node, "type");
	if (value == null)
	    throw new SAXException("missing [type] attribute for ["
		+ n.getNodeName() + "]");
	// Ignore METHOD and PARAM nodes.
	if (value.toUpperCase().equals("METHOD") ||
	    value.toUpperCase().equals("PARAM"))
	{
	    // Should emit error message here, really...
	    return null;
	}
	if (parent != null && (parent.getNodeType() & parent.CLASS) == 0)
	    throw new SAXException("nodes below non-CLASS node ["
		+ parent.getNodeName() + "] not allowed");
	if (value.equals("REFERENCE"))
	{
	    /* This is a template reference.  The "reference" attribute
	     * will hold the normalised reference path, which is a relative
	     * path both from the XSC-root to the template XSC, and from the
	     * normalised (output) SSC-root to the template SSC file.
	     * From the filename, we (horrible hack) derive the class name
	     * of the main node of the template...
	     * Of course, what we should *really* do is follow the link to
	     * the template file, and ask for its main node's "javaName" value.
	     * Note that the attribute value does includes a ".xsc" suffix,
	     * which translates to a ".ssc" suffix for the corresponding SSC.
	     * The "reference" attribute may also be absent, which implies
	     * an XSC 0.4-style local template reference.
	     */
	    String refPath = getSafeAttr(node, "reference");
	    if (refPath == null)
	    {
		// XSC 0.4-style local template reference.
		mask |= MessageStructureNode.LOCAL_REF;
		value = getSafeAttr(node, "member");
		if (value == null)
		{
		    /* Implies recursive link to "main" member; impossible
		     * in SSC since its references must not loop.
		     */
		    throw new SAXException
			("local reference node without template name");
		}
		n.setLink(value);
	    }
	    else
	    {
		// External template reference.
		mask |= MessageStructureNode.REFERENCE;
		n.setRefPath(refPath);
		int from = refPath.lastIndexOf('/') + 1;
		n.setLink(refPath.substring(from, refPath.length()));
		if (fileSet.resolveGlobalTemplate(refPath) == null && recurse)
		{
		    // Reference not in the set yet, add it.
		    boolean saveIsPrime = isPrime;
		    run(n.getRefPath(), recurse, false, false);
		    isPrime = saveIsPrime;
		}
		String version = getTextAttr(node, "codeVersion");
		if (version != null && ! version.equals("undefined"))
		    n.setRefCode(Integer.parseInt(version));
		SSC_File ref = fileSet.resolveGlobalTemplate(refPath);
		if (ref != null && ref.getCodeVersion() != n.getRefCode())
		{
		    // Version inconsistent; implies shift.
		    if (debug)
			System.out.println("[ code version update ("
			    + refPath + "): " + n.getRefCode()
			    + " -> " + ref.getCodeVersion()+ " ]");
		    codeVersionMismatch = true;
		    n.setRefCode(ref.getCodeVersion());
		}
	    }
	}
	else if (value.equals("CLASS"))
	{
	    // Nested or top-level composite (parent) node.
	    mask |= MessageStructureNode.CLASS;
	    String jtype = getTextAttr(node, "javaType");
	    if (jtype != null)
		n.setJavaType(jtype);
	}
	else if (value.equals("FIELD"))
	{
	    // Leaf node.
	    mask |= MessageStructureNode.FIELD;
	    String jtype = getTextAttr(node, "javaType");
	    if (jtype == null)
		jtype = "java.lang.String";
	    n.setJavaType(jtype);
	}
	else if (isLeafType(value))
	{
	    /* Must be a leaf node.
	     * (Obsolete XSC 0.3 notation; XSC 0.4 uses "FIELD".)
	     */
	    mask |= MessageStructureNode.FIELD;
	    if (value.equals("String"))
		value = "java.lang.String";
	    n.setJavaType(value);
	    value = "FIELD";
	}
	else
	{
	    // Local template reference.  Should check that it is known...
	    mask |= MessageStructureNode.LOCAL_REF;
	    // HACK!
	    if (value.endsWith("-struct"))
		value = value.substring(0, value.length()-7);
	    n.setLink(value);
	    if (debug)
		System.out.println("  local template name=" + value);
	}
	String nodeType = value;
	if (debug)
	{
	    System.out.println("  type=" + nodeType);
	    System.out.println("  javatype=" + n.getJavaType());
	}

	value = getTextAttr(node, "structure");
	if (value == null || value.equals("set"))
	    mask |= MessageStructureNode.SET;
	else if (value.equals("fixed"))
	    mask |= MessageStructureNode.FIXED;
	else if (value.equals("delim"))
	    mask |= MessageStructureNode.DELIM;
	else if (value.equals("array"))
	    mask |= MessageStructureNode.ARRAY;
	else
	    throw new SAXException("illegal [structure] value \""
		+ value + '"');

	if (nodeType.equals("FIELD") || nodeType.equals("CLASS"))
	{
	    value = getTextAttr(node, "order");
	    if (value == null || value.equals("sequence"))
		mask |= MessageStructureNode.SEQUENCE;
	    else if (value.equals("any"))
		mask |= MessageStructureNode.ANY;
	    else if (value.equals("choice"))
		mask |= MessageStructureNode.CHOICE;
	    else
		throw new SAXException("illegal [order] value \""
		    + value + '"');
	}
	if ((mask & n.FIELD) != 0 && (mask & n.MAIN_ROOT) != 0)
	    throw new SAXException("main root node cannot have type=\"FIELD\"");

	n.setNodeType(mask);
	if (debug)
	    System.out.println("  setting mask: " + mask + " ("
		+ MessageStructureNodeImpl.showNodeType(mask) + ")");

	value = getSafeAttr(node, "fixedValue");
	if(value != null) {
	    n.setFixedValue(value);
	}

	value = getSafeAttr(node, "defaultValue");
	if(value != null) {
	    n.setDefaultValue(value);
	}

	if(n.getFixedValue() != null && n.getDefaultValue() != null) {
	    throw new SAXException("fixedValue and defaultValue cannot "
		+ "appear together in a node");
	}

	// Field access direction.
	byte access = n.A_MODIFY;
	value = getTextAttr(node, "access");
	if (value == null)
	{
	    // Backward compatible with old "readOnly" attribute.
	    if (getBoolAttr(node, "readOnly", false))
		access = n.A_READ;
	}
	else if (value.equals("read"))
	    access = n.A_READ;
	else if (value.equals("write"))
	    access = n.A_WRITE;
	else if (value.equals("none"))
	    access = n.A_NONE;
	else if (! value.equals("modify"))
	    throw new SAXException
		("illegal value for 'access' (" + value + ')');
	n.setAccess(access);

	// Various boolean flags.
	value = getTextAttr(node, "precedence");
	if (value != null)
	{
	    if (value.equals("parent"))
		n.setParentPrec(true);
	    else if (! value.equals("child"))
		throw new SAXException
		    ("illegal value of 'precedence' (" + value + ")");
	}
	n.setOptional(getBoolAttr(node, "optional", false));
	n.setOverride(getBoolAttr(node, "override", false));
	n.setAvoid(getBoolAttr(node, "avoidMatch", false));
	n.setExact(getBoolAttr(node, "exact", false));
	n.setGroup(getBoolAttr(node, "group", false));

	value = getTextAttr(node, "minOccurs");
	if (value != null)
	{
	    /* SSC does not know how to handle "optional", except
	     * by setting lower bound of repetition to 0.
	     */
	    n.setMinRep(Integer.parseInt(value));
	    if (!allowOptionalWithNonZeroMinOccurs && n.getOptional())
	    {
		n.setMinRep(0);
		n.setOptional(false);
	    }
	    if (debug)
		System.out.println("  setting minOccurs: " + n.getMinRep());
	}

	value = getTextAttr(node, "maxOccurs");
	if (value != null)
	{
	    if (value.equals("unbounded") || value.equals("-1"))
	    {
		// Note: allow old alternate notation "-1" for "unbounded".
		n.setMaxRep(n.UNBOUNDED);
	    }
	    else
	    {
		int mo = -1;
		try { mo = Integer.parseInt(value); }
		catch (NumberFormatException nf) { }
		if (mo < 0)
		    throw new SAXException("illegal [maxOccurs] value \""
			+ value + '"');
		n.setMaxRep(mo);
	    }
	    if (debug)
		System.out.println("  setting maxOccurs: " + n.getMaxRep());
	}

	n.setTag(getSafeAttr(node, "inputMatch"));

	value = getSafeAttr(node, "defaultValue");
	n.setDefaultString(value);
	String denc = getSafeAttr(node, "defaultEncoding");
	n.setDefaultCode(denc);
	if (value != null)
	{
	    try
	    {
		n.setDefaultBytes(StringCoderFactory.getStringCoder(
		    (denc == null ? "US-ASCII" : denc)).encode(value));
	    }
	    catch (UnsupportedEncodingException ue)
	    {
		throw new RuntimeException("cannot encode defaultValue using ["
		    + denc + "] encoding");
	    }
	}
	value = getSafeAttr(node, "defaultBytes");
	if (value != null)
	{
	    int len = value.length();
	    byte[] b = new byte[len];
	    for (int i = 0; i < len; i ++)
		b[i] = (byte)(value.charAt(i) & 0xFF);
	    //NYI: check against default bytes
	}

	value = getTextAttr(node, "offset");
	if (value != null && ! value.equals("undefined"))
	{
	    //System.out.println("offset = " + value);
	    n.setOffset(Integer.parseInt(value));
	}

	/* Length can be unknown, fixed or encoded.  Because length can be
	 * negative for fixed nodes (SSC types "OF" and "AF"), we separate
	 * this info into lentyp (UNDEFINE, HAS_VALUE or DECIMAL), length
	 * (the fixed value), and lenoff/lensiz (encoded length info).
	 */
	value = getTextAttr(node, "length");
	if (value == null || value.equals("undefined"))
	{
	    // No explicit length.
	    n.setLenTyp(n.UNDEFINED);
	    n.setLength(0);
	}
	else if (value.equals("decimal"))
	{
	    // Run-time encoded length in data.
	    n.setLenTyp(n.DECIMAL);
	    n.setLength(0);

	    // Next two attributes are only relevant for encoded length.
	    value = getTextAttr(node, "lengthFrom");
	    if (value != null && ! value.equals("undefined"))
	    {
		n.setLenOff(Integer.parseInt(value));
	    }
	    value = getTextAttr(node, "lengthSize");
	    if (value == null || value.equals("undefined"))
		throw new SAXException("node [" + n.getNodeName() +
		    "] has length=\"decimal\" but no lengthSize");
	    n.setLenSiz(Integer.parseInt(value));
	}
	else
	{
	    // Fixed explicit length.
	    n.setLenTyp(n.HAS_VALUE);
	    n.setLength(Integer.parseInt(value));
	}
	if (n.getLenTyp() != n.DECIMAL)
	{
	    value = getTextAttr(node, "lengthSize");
	    if (value != null && ! value.equals("undefined"))
		throw new SAXException("node [" + n.getNodeName() +
		    "] has defined lengthSize, but not length=\"decimal\"");
	}

	/* Local delimiter attributes.  In XSC 0.5 these should actually
	 * occur in a separate <delim> child entity, but we accept them in
	 * <node> as well for backward compatibility.
	 */
	byte localDelim = 0;
	if (getBoolAttr(node, "anchored", false))
	    localDelim |= Xsc.Delim.ANCHORED;
	if (getBoolAttr(node, "beginAnchored", false))
	    localDelim |= Xsc.Delim.BEGINANCH;
	if (getBoolAttr(node, "endAnchored", false))
	    localDelim |= Xsc.Delim.ENDANCH;
	if (getBoolAttr(node, "array", false))
	    localDelim |= Xsc.Delim.ARRAY;
	if (getBoolAttr(node, "endOfRec", false))
	    localDelim |= Xsc.Delim.ENDOFREC;
	if (getBoolAttr(node, "required", false))
	    localDelim |= Xsc.Delim.REQUIRED;
	if (getBoolAttr(node, "separator", false))
	    localDelim |= Xsc.Delim.SEPARATOR;
	String bd = getSafeAttr(node, "beginDelim");
	String ed = getSafeAttr(node, "endDelim");
	if (bd != null || ed != null)
	{
	    /* For backward compatibility, permit beginDelim/endDelim
	     * attributes in <node>, but replace this by a first
	     * <delimGroup>.
	     */
	    Xsc.DelimGroup group = new Xsc.DelimGroup(null, null);
	    if (bd != null)
		group.addBegin(new Xsc.OneDelim(null, null, null, bd, "UTF-8"));
	    if (ed != null)
		group.addEnd(new Xsc.OneDelim(null, null, null, ed, "UTF-8"));
	    Xsc.Delim delim = new Xsc.Delim(null, null, localDelim);
	    delim.addGroup(group);
	    n.setDelim(delim);
	}

	// Formatting.
	n.setEncoding(getSafeAttr(node, "encoding"));
	n.setFormat(getSafeAttr(node, "format"));

	// Scavenger characters ("Sc"/"ScN" modifier).
	value = getSafeAttr(node, "scavenger");
	if (value != null)
	{
	    n.setScavenger(value);
	    if (getBoolAttr(node, "scavOutput", false))
		n.setScavOutput(true);
	}

	// Comment.
	n.setComment(getSafeAttr(node, "comment"));

	// Anchor for collab editor; keep track of maximum so far.
	n.setUid(getUid(container, node));

	// Child bounds.
	value = getTextAttr(node, "childMin");
	int chMin =
	    (value == null || value.equals("undefined"))
	    ? n.UNDEFINED
	    : Integer.parseInt(value);
	value = getTextAttr(node, "childMax");
	int chMax =
	    (value == null || value.equals("undefined"))
	    ? n.UNDEFINED
	    : (value.equals("unbounded")
	    ? n.UNBOUNDED
	    : Integer.parseInt(value));
	if ((chMin == n.UNDEFINED) != (chMax == n.UNDEFINED))
	    throw new SAXException("node [" + n.getNodeName()
		+ "] has inconsistent childMin/childMax");
	n.setChildMin(chMin);
	n.setChildMax(chMax);

	// Walk the kids.
	parentName = (parentName == null ? "" : (parentName + "."))
	    + n.getJavaName();
	Node child = node.getFirstChild();
	if (child != null)
	{
	    SscConv.addJavaId(n.getJavaName(), ancestorJavaNames);
	    HashSet childNames = new HashSet();
	    for (; child != null; child = child.getNextSibling())
	    {
		if (child instanceof Element)
		{
		    Element sub = (Element) child;
		    tag = sub.getTagName();
		    if (tag.equals("delim"))
		    {
			// Local delimiter definition.
			n.setDelim(parseDelim(container, sub, n, 0));
		    }
		    else if (tag.equals("method"))
		    {
			// Method info.
			n.addMethod(parseMethod(container, sub));
		    }
		    else
		    {
			// Subordinate node.
			parseNode(container, sub, n, isMain,
			    depth + 1, length, ancestorJavaNames, childNames,
			    parentName);
		    }
		}
	    }
	    SscConv.removeJavaId(n.getJavaName(), ancestorJavaNames);
	}
	if (classNeedsChild)
	{
	    // Ensure that CLASS node has subnodes (mandatory if SSC-based).
	    if ((n.getNodeType() & n.CLASS) != 0 && n.getFirstChild() == null)
	    {
		if (! emptyClassIsField)
		    throw new SAXException("node [" + n.getNodeName() +
			"] has type=\"CLASS\" but lacks children");

		if((mask & n.MAIN_ROOT) == 0)   // This line for QAI 49292
		    n.setNodeType((n.getNodeType() | n.FIELD) & ~(n.CLASS));
	    }
	}
	if ((n.getNodeType() & n.FIELD) != 0 && n.getFirstChild() != null)
	    throw new SAXException("node [" + n.getNodeName() +
		"] with type=\"FIELD\" has illegitimate children");

	if (ctUse && ! n.isLeaf() && ! n.isReference())
	{
	    // Proper parent node: update class tree info.
	    String jtype = n.getJavaType();
	    PathNode pack = null;
	    if (jtype == null || jtype.equals(""))
	    {
		// Implicit type: package + ancestor chain.
		pack = cpack;
		jtype = parentName;
	    }
	    if (debug)
		System.out.println("[ registering type <" + jtype + "> for <"
		    + n.getNodeName() + "> ]");
	    ctree.makeNewClass(new NodeUser(n), pack, jtype);
	}

	return n;
    }

    /**
     * Converts hexadecimal digit to integer 0-15, or -1 if invalid.
     *
     * @param c  the ASCII hexadecimal digit
     * @return the integer value, or -1
     */
    static public int hexval (char c)
    {
	if ('0' <= c && c <= '9')
	    return (int)(c - '0');
	if ('a' <= c && c <= 'f')
	    return c - 'a' + 10;
	if ('A' <= c && c <= 'F')
	    return c - 'A' + 10;

	// Error.
	return -1;
    }

    /**
     * Decodes the XML string, which has Unicode escapes (e.g. \u0020) for
     * all non-printable or special character to protect against standard
     * XML normalisation (and the deviant implementations thereof).
     *
     * @param s  the normal-safe encoded string
     * @return the decoded string
     */
    static public String uDecode (String s)
    {
	if (s.indexOf('\\') < 0)
	    return s;

	int len = s.length(), d1, d2, d3, d4;
	char c;
	StringBuffer b = new StringBuffer(len);

	for (int i = 0; i < len; i ++)
	{
	    c = s.charAt(i);
	    if (c == '\\')
	    {
		if (i + 6 > len)
		    throw new RuntimeException("'\\u' escape truncated");
		if (s.charAt(i + 1) != 'u')
		    throw new RuntimeException("missing 'u' after '\\'");
		d1 = hexval(s.charAt(i + 2));
		d2 = hexval(s.charAt(i + 3));
		d3 = hexval(s.charAt(i + 4));
		d4 = hexval(s.charAt(i + 5));
		if (d1 < 0 || d2 < 0 || d3 < 0 || d4 < 0)
		    throw new RuntimeException("non-hex digit after '\\u'");
		c = (char) ((d1 << 12) + (d2 << 8) + (d3 << 4) + d4);
		i += 5;
	    }
	    b.append(c);
	}
	return b.toString();
    }
}
