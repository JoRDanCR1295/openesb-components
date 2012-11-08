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
 * @(#)GenXsc.java 
 *
 * Copyright 2004-2008 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.stc.jcs.ssc;

import java.io.*;

import java.text.MessageFormat;

import com.sun.stc.jcsre.JCSProperties;
import com.sun.stc.jcs.JCSConstants;
import com.sun.stc.jcs.ssc.SscConv.XscMethod;

/**
 * Class used for the generation of an XSC file.
 * XSC is e*Gate 4.5.1 event/message description metadata, written in XML.
 * This class generates XSC according to the XSC 0.4 specification for
 * type "SSC" XSC.
 */
public class GenXsc
{
    public final int DENT = 2; // base indentation

    // General debugging.
    public boolean debug = JCSProperties.getFlag("GenXsc.debug", false);

    /**
     * Option: emit &lt;method&gt; info for set/get.
     * When active, we generate &lt;method&gt; entities in the XSC output that
     * describe the generated normal "set"/"get" methods for all nodes
     * explicitly, even though the collaboration editor knows how to
     * derive this information automatically from the rest of the XSC
     * description of each node.
     */
    public boolean doSetGet =
	JCSProperties.getFlag("GenXsc.doSetGet", false);

    /**
     * Option: emit a string setter node.
     * When active, we generate an extra "set" method for each formatted
     * leaf-node (i.e. that is not of type byte[] or String), which takes
     * a String argument instead of the node's normal Java type.
     */
    public static final boolean stringSetter =
	JCSProperties.getFlag("GenXsc.stringSetter", false);

    /**
     * Flag to add the required &lt;throws&gt; entities to the &lt;methods&gt;
     * entities in the XSC file.  Uses for full XSC 0.4 compliance.  Note that
     * the e*Gate 4.5.1 ETD editor will choke on these.
     * Initialized from the boolean JCS property "GenXsc.addThrows";
     * defaults to "true".
     */
    public boolean addThrows =
	JCSProperties.getFlag("GenXsc.addThrows", true);

    /**
     * Flag to add &lt;methods&gt; entities in the XSC file for the removeX,
     * addX, and clearX methods.  Initialized from the boolean JCS property
     * "GenXsc.addRac"; defaults to "false".
     */
    public boolean addRac =
	JCSProperties.getFlag("GenXsc.addRac", false);

    /**
     * Flag to emit access as the "readOnly" attribute, instead of "access".
     * This is for backward compatibility with XSC 0.3 through 0.6.
     * Initialized from the boolean JCS property "GenXsc.readOnly", else true.
     */
    public boolean doReadOnly =
	JCSProperties.getFlag("GenXsc.readOnly", true);

    // The output stream for XSC.
    private PrintWriter out = null;

    /**
     * Outputs string attribute if non-empty.
     *
     * @param name  attribute name
     * @param value  attribute value (may be null)
     */
    private void outText (String name, String value)
    {
	if (value != null && ! value.equals(""))
	    e.part(" " + name + "=\"" + SscConv.sscEscape(value) + '"');
    }

    /**
     * Outputs string attribute if non-empty, in normal-safe encoding.
     *
     * @param name  attribute name
     * @param value  attribute value (may be null)
     */
    private void outSafe (String name, String value)
    {
	if (value != null && ! value.equals(""))
	    e.part(" " + name + "=\"" + SscConv.sscNormal(value) + '"');
    }

    /**
     * Outputs boolean attribute if true.
     *
     * @param name  attribute name
     * @param value  attribute value
     */
    private void outFlag (String name, boolean value)
    {
	if (debug)
	    System.out.println("[ flag <" + name + "> = " + value + " ]");
	if (value)
	    e.part(" " + name + "=\"true\"");
    }

    /**
     * Output bit-flag attribute as boolean if set.
     *
     * @param name  attribute name
     * @param value  attribute bit-masked value
     */
    private void outFlag (String name, int value)
    {
	if (debug)
	    System.out.println("[ flag <" + name + "> = " + value + " ]");
	if (value != 0)
	    e.part(" " + name + "=\"true\"");
    }

    /**
     * Output numeric attribute if not equal to default.
     *
     * @param name  attribute name
     * @param value  attribute value
     * @param deft  default value
     */
    private void outNumb (String name, int value, int deft)
    {
	if (value != deft)
	    e.part(" " + name + "=\"" +
		((value == MessageStructureNode.UNDEFINED) ? "undefined" :
		 (value == MessageStructureNode.UNBOUNDED) ? "unbounded" :
		 Integer.toString(value)) +
		'"');
    }

    /**
     * Output byte-array as normal-safe encoded Unicode chars in range 0-255.
     * No output if array is null or empty.
     *
     * @param name  attribute name
     * @param value  attribute value
     */
    private void outByte (String name, byte[] value)
    {
	if (value != null && value.length > 0)
	{
	    char[] buf = new char[value.length];
	    for (int i = 0; i < value.length; i ++)
		buf[i] = (char)(value[i] & 0xFF);
	    outSafe(name, new String(buf));
	}
    }

    private boolean xsc03 = false;
    private SscConv conv = null;

    public Emit e = null; // output stream
    public SSC_File ssc = null;	// used for UID generation
    public boolean addMethods = false; // add Xsc.Method info?
    public boolean addUid = true; // add UIDs?

    /**
     * Emits given UID string, or make one up if null/empty when flag set.
     * The "addUid" flag should be set if all entities must have a UID.
     * This is required e.g. for output for the collaboration editor.
     *
     * @param uid  the known UID string, or null
     */
    private void outGuid (String uid)
    {
	if ((uid == null || uid.equals("")) && addUid && ssc != null)
	    uid = ssc.makeUid();
	outText("uid", uid);
    }

    /**
     * Generates the XSC file from the "SSC_File" information.
     *
     * @param files  the template reference closure
     * @param file  the XSC file data
     * @param xsc  path of output file
     * @throws IOException for XSC output problems
     */
    public void genFileTo (SSC_FileSet files, SSC_File file, File xsc)
	throws IOException
    {
	this.conv = new SscConv();
	this.xsc03 = conv.xsc03;
	this.ssc = file;
	e = new Emit(
		new BufferedWriter(
		    new OutputStreamWriter(
			new FileOutputStream(xsc),
			"UTF-8")));
	conv.xscHead(e, file.getType(), file.getDerived(),
	    file.getCodeVersion(), file.getGlobalName(),
	    file.getPackageName(), file.getEncoding(), file.getDataCode(),
	file.getEtdComment(), file.getEtdUid(), file.getJavaProps());
	genDelims(file.getDelimiters(), file.getDelimsComment(),
	    file.getDelimsUid());
	genNode(files, file.getEtd(), true, true);
	String names[] = file.localTemplateNames();
	for (int i = 0; i < names.length; i ++)
	    genNode(files, file.resolveLocalTemplate(names[i]), true, false);
	conv.xscTail(e);
	e.close();
	e = null;
    }

    /**
     * Generates a &lt;jar&gt; entity in the XSC output file.
     * This information only occurs for derived ETDs, and indicates
     * extra JAR files necessary at runtime to use this ETD.
     *
     * @param jars  the JAR file descriptor
     */
    private void genJar (Xsc.Jar jar)
    {
	e.part("<jar");
	outSafe("file", jar.getFile());
	outSafe("comment", jar.getComment());
	outGuid(jar.getUid());
	e.emit("/>");
    }

    /**
     * Generates the &lt;javaProps&gt; entity in the XSC output file.
     * This information represents the Java-specific global attributes.
     *
     * @param jprops  the Java properties
     */
    public void genJavaProps (Xsc.JavaProps jprops)
    {
	e.part("<javaProps");
	outText("codeAvailable", (jprops.getCode() ? "true" : "false"));
	outText("package", jprops.getPackage());
	outText("class", jprops.getBase());
	outSafe("jarFile", jprops.getJarFile());
	outSafe("source", jprops.getSource());
	outSafe("comment", jprops.getComment());
	outGuid(jprops.getUid());
	Xsc.Jar jars = jprops.getJars();
	if (jars == null)
	{
	    // Empty set.
	    e.emit("/>");
	}
	else
	{
	    e.down(">");
	    for (; jars != null; jars = jars.getNext())
		genJar(jars);
	    e.done("</javaProps>");
	}
    }

    /**
     * Generates a &lt;delimiters&gt; list in the XSC output file.
     * This information represents the SSC global default delimiter list.
     *
     * @param ones  the delimiter list
     */
    private void genOneDelims (boolean isBegin, Xsc.OneDelim ones)
    {
	for (; ones != null; ones = ones.getNext())
	{
	    e.part("<" + (isBegin ? "begin" : "end") + "Delim");
	    outByte("bytes",    ones.getData());
	    outSafe("value",    ones.getText());
	    outSafe("encoding", ones.getCode());
	    outNumb("offset",   ones.getOffset(),
		MessageStructureNode.UNDEFINED);
	    outNumb("length",   ones.getLength(),
		MessageStructureNode.UNDEFINED);
	    outSafe("comment",  ones.getComment());
	    outGuid(ones.getUid());
	    e.emit("/>");
	}
    }

    /**
     * Generates a &lt;escape&gt; list in the XSC output file.
     * This information represents the SSC global default delimiter list.
     *
     * @param escD  the escape delimiter list
     */
    private void genOneEscapes (Xsc.Escape escD)
    {
	for (; escD != null; escD = escD.getNext())
	{
	    e.part("<escape");
	    outByte("bytes",    escD.getData());
	    outSafe("value",    escD.getText());
	    outSafe("encoding", escD.getCode());
	    outSafe("comment",  escD.getComment());
	    outGuid(escD.getUid());
	    e.emit("/>");
	}
    }

    /**
     * Generates a &lt;delim&gt; description in the XSC output file.
     * This information represents a single local or global delimiter level.
     *
     * @param delim  the delimiter
     */
    public void genDelim (Xsc.Delim delim)
    {
	int type = delim.getFlags();
	e.part("<delim");
	outFlag("anchored",   type & delim.ANCHORED);
	outFlag("beginAnchored", type & delim.BEGINANCH);
	outFlag("endAnchored", type & delim.ENDANCH);
	outFlag("array",      type & delim.ARRAY);
	outFlag("endOfRec",   type & delim.ENDOFREC);
	outFlag("required",   type & delim.REQUIRED);
	outFlag("separator",  type & delim.SEPARATOR);
	outSafe("comment",    delim.getComment());
	outGuid(delim.getUid());
	e.emit(">");
	e.indent();
	for (Xsc.DelimGroup groups = delim.getGroups();
	    groups != null; groups = groups.getNext())
	{
	    e.part("<delimGroup");
	    outSafe("comment",  groups.getComment());
	    outGuid(groups.getUid());
	    e.emit(">");
	    e.indent();
	    genOneDelims(true,  groups.getBegins());
	    genOneDelims(false, groups.getEnds());
	    e.done("</delimGroup>");
	}
	genOneEscapes(delim.getEscapes());
	e.done("</delim>");
    }

    /**
     * Generates the &lt;delimiters&gt; list in the XSC output file.
     * This information represents the SSC global default delimiter list.
     *
     * @param delims  the delimiter list
     */
    private void genDelims (Xsc.Delim delims, String comm, String guid)
    {
	e.part("<delimiters");
	outSafe("comment", comm);
	outGuid(guid);
	if (delims == null)
	{
	    // Empty set.
	    e.emit("/>");
	}
	else
	{
	    e.down(">");
	    for (; delims != null; delims = delims.getNext())
		genDelim(delims);
	    e.done("</delimiters>");
	}
    }

    /**
     * Generates the &lt;method&gt; list in the XSC output file.
     * This information represents the public method signatures of a node.
     *
     * @param mets  the methods list
     */
    private void genMethods (Xsc.Method mets)
    {
	for (; mets != null; mets = mets.getNext())
	{
	    e.part("<method");
	    outText("name", mets.getName());
	    outText("result", mets.getType());
	    outSafe("comment", mets.getComment());
	    outGuid(mets.getUid());
	    Xsc.Param pars = mets.getParams();
	    Xsc.Throws excs = mets.getThrows();
	    if (pars == null && excs == null)
	    {
		// Empty set.
		e.emit("/>");
	    }
	    else
	    {
		e.down(">");
		for (; pars != null; pars = pars.getNext())
		{
		    e.part("<param");
		    outText("name", pars.getName());
		    outText("paramType", pars.getType());
		    outSafe("comment", pars.getComment());
		    outGuid(pars.getUid());
		    e.emit("/>");
		}
		for (; excs != null; excs = excs.getNext())
		{
		    e.part("<throws");
		    outText("excepType", excs.getType());
		    outSafe("comment", excs.getComment());
		    outGuid(excs.getUid());
		    e.emit("/>");
		}
		e.done("</method>");
	    }
	}
    }

    /**
     * Converts a byte array to a Unicode string using ISO-8859-1 encoding.
     * This means each byte with value N becomes a single character N.
     * Converting null will return null.
     *
     * @param b  the byte array (or null)
     * @return a Unicode string
     */
    public static String byteString (byte[] b)
    {
	if (b == null)
	    return null;
	char[] c = new char[b.length];
	for (int i = 0; i < b.length; i ++)
	    c[i] = (char) b[i];
	return new String(c);
    }

    /**
     * Generates the &lt;class&lt XML entity for the given event node
     * description to the XSC output.
     *
     * @param files  the external template reference closure
     * @param node  the node to describe
     * @param root  flag: is top node (no parent)?
     * @param main  flag: main XSC file (i.e. not a template)?
     */
    private void genNode (SSC_FileSet files, MessageStructureNode node,
	boolean root, boolean main)
    {
	String tag = (root ? "class" : "node");

	int type = node.getNodeType();
	e.part("<" + tag);
	String monkName = node.getNodeName();
	String javaName = node.getJavaName();
	String javaType = node.getJavaType();
	String member = node.getLink();
	if (node.getRefPath() != null)
	    member = null;
	if (monkName.equals(javaName)) { javaName = null; }
	outSafe("name", monkName);
	outText("javaName", javaName);
	outSafe("nickName", node.getNickName());
	outText("type",
	    (type & node.REFERENCE) != 0 ? "REFERENCE" :
	    (type & node.LOCAL_REF) != 0 ? (xsc03 ? member : "REFERENCE") :
	    (type & node.ENUMERATE) != 0 ? "ENUMERATION" :
	    (type & node.CLASS) != 0 ? "CLASS" :
	    (type & node.FIELD) != 0 ? (xsc03 ? javaType : "FIELD") :
	    null);
	outText("javaType", (xsc03 ? null : javaType));
	outText("member",   (xsc03 ? null : member));
	outText("structure",
	    (type & node.DELIM) != 0 ? "delim" :
	    (type & node.FIXED) != 0 ? "fixed" :
	    (type & node.ARRAY) != 0 ? "array" :
	    (type & node.SET  ) != 0 ? "set"   : null);
	outText("order",
	    (type & node.SEQUENCE) != 0 ? "sequence" :
	    (type & node.CHOICE)   != 0 ? "choice" :
	    (type & node.ANY)      != 0 ? "any" : null);

	outFlag("optional", node.getOptional());
	if (doReadOnly)
	{
	    // Backward compatibility switch.
	    outFlag("readOnly", node.getAccess() == node.A_READ);
	}
	else
	{
	    // Normal access value for XSC 0.7.
	    switch (node.getAccess())
	    {
	    case MessageStructureNode.A_NONE:
	        outText("access", "none");
		break;
	    case MessageStructureNode.A_READ:
	        outText("access", "read");
		break;
	    case MessageStructureNode.A_WRITE:
	        outText("access", "write");
		break;
	    case MessageStructureNode.A_MODIFY:
	        outText("access", "modify");
		break;
	    default:
		throw new RuntimeException("invalid access value");
	    }
	}
	outFlag("override", node.getOverride());
	outNumb("minOccurs",  node.getMinRep(), 1);
	outNumb("maxOccurs",  node.getMaxRep(), 1);
	outNumb("offset",     node.getOffset(), node.UNDEFINED);
	outText("length",
	    (node.getLenTyp() == node.HAS_VALUE
		? Integer.toString(node.getLength()) :
	    (node.getLenTyp() == node.DECIMAL
		? "decimal" : null)));
	outNumb("lengthFrom", node.getLenOff(), node.UNDEFINED);
	outNumb("lengthSize", node.getLenSiz(), node.UNDEFINED);
	outSafe("inputMatch", node.getTag());
	outFlag("avoidMatch", node.getAvoid());
	outFlag("exact",      node.getExact());
	outFlag("group",      node.getGroup());
	outText("precedence", (node.getParentPrec() ? "parent" : null));
	outText("defaultEncoding", node.getDefaultCode());
	outSafe("defaultBytes", byteString(node.getDefaultBytes()));
	outSafe("defaultValue", node.getDefaultString());
	outSafe("format",     node.getFormat());
	outText("encoding",   node.getEncoding());
	outNumb("childMin",   node.getChildMin(), node.UNDEFINED);
	outNumb("childMax",   node.getChildMax(), node.UNDEFINED);
	outSafe("scavenger",  node.getScavenger());
	outFlag("scavOutput", node.getScavOutput());
	outSafe("reference",  node.getRefPath());
	outNumb("codeVersion", node.getRefCode(), 0);
	outSafe("comment",    node.getComment());
	outGuid(node.getUid());

	// All the sub-nodes.
	Xsc.Delim delim = node.getDelim();
	MessageStructureNode child = node.getFirstChild();
	Xsc.Method methods = node.getMethods();
	boolean solo = true;
	if (delim != null || methods != null || child != null)
	{
	    solo = false;
	    e.down(">");
	    // The local delimiter info.
	    if (delim != null)
		genDelim(delim);
	    // The subordinate <node> entities.
	    for (; child != null; child = child.getNextSibling())
		genNode(files, child, false, main);            
	    // Method info.
	    if (addMethods)
		genMethods(methods);
	    else if (root && main)
		this.conv.xscStdMethods(e, this.ssc, addThrows);
	    // Separate end-tag.
	    e.done("</" + tag + ">");
	}
	else
	{
	    // Single tag.
	    e.emit("/>");
	}
	if (! root) { genNodeMethods(files, node); }
    }

    /**
     * Generates &lt;method&gt; entities describing the generated methods for
     * the specified node.
     *
     * @param files  the set of files, for resolving external references
     * @param node  node to generate methods for
     */
    private void genNodeMethods (SSC_FileSet files, MessageStructureNode node)
    {
	XscMethod m = new XscMethod(e, ssc, true);

	String javaName = JGen.uEncode(node.getJavaName());
	String javaType = node.getJavaType();
	boolean isLocal = JGen.isLocalTemplate(node);
	boolean isGlobal = JGen.isGlobalTemplate(node);
	if (javaType == null)
	{
	    if (node.isLeaf())
	    {
		// Default type for leaf nodes.
		javaType = "java.lang.String";
	    }
	    else
	    {
		/* Non-leaf node; get fully qualified class name.
		 * To get this, resolve what the node is actually pointing
		 * to (if it's a reference), then take the package of the
		 * node and prepend all ancestor names to the node name.
		 * Note that in SSC-based XSC, template roots have no
		 * ancestors, but the code below is generic.
		 */
		SSC_File file = node.getFile();
		MessageStructureNode root = node;
		if (isLocal)
		{
		    root = file.resolveLocalTemplate(node.getLink());
		}
		else if (isGlobal)
		{
		    file = files.resolveGlobalTemplate(node.getRefPath());
		    root = file.getEtd();
		}
		for (javaType = ""; root != null; root = root.getParent())
		    javaType = "." + root.getJavaName() + javaType;
		javaType = JGen.packPrefix(file, null) + javaType.substring(1);
	    }
	}
	String[] messageParams = { javaName };

	if (1 == node.getMaxRep())
	{
	    // Single occurrence node.
	    if (doSetGet)
	    {
		// Add set/get method info.
		m.method("get" + javaName, javaType,
		    MessageFormat.format(JCSConstants.getMethodComment,
		    messageParams));
		m.param("value", javaType,
		    MessageFormat.format(JCSConstants.valueParamComment,
		    messageParams));
		m.method("set" + javaName, "void",
		    MessageFormat.format(JCSConstants.setMethodComment,
		    messageParams));
	    }
	    if (GenXsc.stringSetter
		&& node.isLeaf()
		&& javaType != null
		&& ! javaType.equals("")
		&& ! javaType.equals("java.lang.String")
		&& ! javaType.equals("byte[]"))
	    {
		// Special accessor for non-string fields.
		m.param("value", "java.lang.String", "new value");
		m.method("set" + javaName, "void",
		    MessageFormat.format(JCSConstants.setMethodComment,
		    messageParams));
	    }
	    if (0 == node.getMinRep() || node.getOptional())
	    {
		// Optional node.
		m.method("has" + javaName, "boolean",
		    MessageFormat.format(JCSConstants.hasMethodComment,
		    messageParams));
		m.method("omit" + javaName, "void",
		    MessageFormat.format(JCSConstants.omitMethodComment,
		    messageParams));
	    }
	}
	else
	{
	    // Repeated node.
	    if (doSetGet)
	    {
		// Add set/get method info.
		m.param("i", "int",
		    MessageFormat.format(JCSConstants.indexParamComment,
		    messageParams));
		m.method("get" + javaName, javaType,
		    MessageFormat.format(JCSConstants.getIndexMethodComment,
		    messageParams));

		m.param("i", "int",
		    MessageFormat.format(JCSConstants.indexParamComment,
		    messageParams));
		m.param("value", javaType,
		    MessageFormat.format(JCSConstants.valueParamComment,
		    messageParams));
		m.method("set" + javaName, javaType,
		    MessageFormat.format(JCSConstants.setIndexMethodComment,
		    messageParams));
	    }

	    if (JGen.arrayGetSet)
	    {
		m.method("get" + javaName, javaType+"[]",
		    MessageFormat.format(JCSConstants.getArrayMethodComment,
		    messageParams));
		m.param("value", javaType+"[]",
		    MessageFormat.format(JCSConstants.valueArrayParamComment,
		    messageParams));
		m.method("set" + javaName, javaType,
		    MessageFormat.format(JCSConstants.setArrayMethodComment,
		    messageParams));
	    }

	    if (GenXsc.stringSetter
		&& node.isLeaf()
		&& javaType != null
		&& ! javaType.equals("")
		&& ! javaType.equals("java.lang.String")
		&& ! javaType.equals("byte[]"))
	    {
		// Special accessor for non-string fields.
		m.param("i", "int",
		    MessageFormat.format(JCSConstants.indexParamComment,
		    messageParams));
		m.param("value", "java.lang.String",
		    MessageFormat.format(JCSConstants.valueParamComment,
		    messageParams));
		m.method("set" + javaName, javaType,
		    MessageFormat.format(JCSConstants.setIndexMethodComment,
		    messageParams));
	    }

	    m.method("count" + javaName, "int",
		MessageFormat.format(JCSConstants.countMethodComment,
		messageParams));

	    if (addRac)
	    {
		m.param("i", "int",
		    MessageFormat.format(JCSConstants.indexParamComment,
		    messageParams));
		m.method("remove" + javaName, javaType,
		    MessageFormat.format(JCSConstants.removeIndexMethodComment,
		    messageParams));

		m.param("value", javaType,
		    MessageFormat.format(JCSConstants.valueParamComment,
		    messageParams));
		m.method("add" + javaName, "void",
		    MessageFormat.format(JCSConstants.addMethodComment,
		    messageParams));

		m.param("i", "int",
		    MessageFormat.format(JCSConstants.indexParamComment,
		    messageParams));
		m.param("value", javaType,
		    MessageFormat.format(JCSConstants.valueParamComment,
		    messageParams));
		m.method("add" + javaName, javaType,
		    MessageFormat.format(JCSConstants.addIndexMethodComment,
		    messageParams));
		m.method("clear" + javaName, "void",
		    MessageFormat.format(JCSConstants.clearMethodComment,
		    messageParams));
	    }

	    if (0 == node.getMinRep() || node.getOptional())
	    {
		// Optional node.
		m.method("has" + javaName, "boolean",
		    MessageFormat.format(JCSConstants.hasMethodComment,
		    messageParams));
	    }
	}
    }
}
