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
 * @(#)GenSsc.java 
 *
 * Copyright 2004-2008 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.stc.jcs.ssc;

import java.util.*;
import java.io.*;

import com.sun.stc.jcsre.JCSProperties;
import com.sun.stc.jcsre.StringCoderFactory;

/**
 * Class used to generate normalized SSC files for SSC-based XSC event
 * descriptions.  The SSC files are used at run-time to direct the
 * underlying Monk event parser.
 */
public class GenSsc
{
    public boolean debug = false;
    private File outputDir;
    private SSC_FileSet fileSet;
    static final boolean forceUtf8 = // force output-SSC to be in UTF-8?
	JCSProperties.getFlag("GenSsc.utf8", true);

    /**
     * Registers the given group of files, and the SSC output directory.
     *
     * @param fileSet the set of XSC files
     * @param outputDir  the SSC output directory
     */
    public GenSsc (SSC_FileSet fileSet, String outputDir)
    {
	this.outputDir = new File(outputDir);
	this.fileSet = fileSet;
    }

    /**
     * Converts Monk encoding name to Java encoding name.
     *
     * @param name  Monk encoding name
     * @return Java encoding name
     * @throws UnsupportedEncodingException if not convertible
     */
    public static String monk2javaCode (String name)
	throws UnsupportedEncodingException
    {
	if (name == null || name.equals(""))
	    return null;
	if (name.equals("ASCII"))
	    return  "US-ASCII";
	if (name.equals("ISO88591"))
	    return  "ISO-8859-1";
	if (name.equals("UTF8"))
	    return  "UTF-8";
	if (name.equals("UHC"))
	    return  "MS949";
	if (name.equals("SJIS"))
	    return  "SJIS";
	if (name.equals("EBCDIC"))
	    return  "cp037";
	throw new UnsupportedEncodingException("unknown Monk encoding ["
	    + name + "]");
    }

    /**
     * Converts Java encoding name to Monk encoding name.
     *
     * @param name  Java encoding name
     * @return Monk encoding name
     * @throws UnsupportedEncodingException if not convertible
     */
    public static String java2monkCode (String name)
	throws UnsupportedEncodingException
    {
	if (name == null || name.equals(""))
	    return "";
	if (name.equals("US-ASCII") || name.equals("ASCII"))
	    return  "ASCII";
	if (name.equals("ISO-8859-1"))
	    return  "ISO88591";
	if (name.equals("UTF-8") || name.equals("UTF8"))
	    return  "UTF8";
	if (name.equals("MS949"))
	    return  "UHC";
	if (name.equals("SJIS"))
	    return  "SJIS";
	if (name.equals("cp037"))
	    return  "EBCDIC";
	throw new UnsupportedEncodingException("unknown Java encoding ["
	    + name + "]");
    }

    /**
     * Generates SSC files for all files in the set.
     *
     * @throws IOException for SSC write problems
     */
    public void gen () throws IOException
    {
	Iterator iter = fileSet.getFiles();
	while (iter.hasNext())
	    genFile((SSC_File)iter.next());
    }

    private OutputStream os = null;
    private String sscEncoding = null;

    /**
     * Generates a single SSC file from XSC info.
     *
     * @param file  the XSC file to generate SSC for
     * @throws IOException for SSC write problems
     */
    public void genFile (SSC_File file) throws IOException
    {
	String packageName = file.getPackageName();
	String globalName = file.getGlobalName();

	MessageStructureNode etd = file.getEtd();
	if (etd == null)
	    throw new com.sun.stc.jcs.JCSException("ETD name not found.");

	String name = etd.getNodeName();
	if (name == null)
	    throw new com.sun.stc.jcs.JCSException("Event definition not found.");

	String delimVar = name + "-delm";
	String formatOption = "DELIMITED";
	String structVar = name + "-struct";
	File outFile = new File(outputDir.getAbsolutePath(),
	    new File(file.getSscFileName()).getName());
	if (debug)
	    System.out.println("[ GenSsc <" + outFile.getPath() + "> ]");

	/* The prologue of the SSC file is in 7-bit ASCII encoding; the
	 * rest will be in whatever encoding the ETD specifies.  We create
	 * a single byte stream, and use two differently encoded writers
	 * on top.
	 */
	os = new FileOutputStream(outFile);
	Writer writer = new BufferedWriter
	    (new OutputStreamWriter(os, "US-ASCII"));
	Emit e = new Emit(writer, 8, 2);
	e.emit(";:- STC MsgStruct Version 3.1");
	e.emit("");
	e.emit(";:- MsgStructure Header");
	e.emit(";:- MsgStructure \"" + globalName + "\"");
	e.emit(";:- UserComment \"Created by GenSsc [+]\"");
	e.emit(";:- Version \"eGate Version 4.5\"");
	e.emit(";:- FormatOption " + formatOption);
	e.emit(";:- RepSeparator \"Repetition Delimiter\" \" \"");
	e.emit(";:- Escape \"Escape Character Delimiter\" \"\\\\\"");
	e.emit(";:- DefaultDelimiters \"OTHER\"");
	e.emit(";:- End MsgStructure Header");
	e.flush();

	// Now switch encoding, or stay with ASCII.
	sscEncoding = (forceUtf8 ? "UTF8" : file.getEncoding());
	if (sscEncoding == null || sscEncoding.equals(""))
	    sscEncoding = "UTF8";
	if (! sscEncoding.equals("ASCII"))
	{
	    // Switch encoding to non-ASCII.
	    e.emit("(set-file-encoding-method :" + sscEncoding + ")");
	    String javaCode = monk2javaCode(sscEncoding);
	    if (debug)
		System.out.println("[ switch encoding to \"" + sscEncoding
		    + "\" = Java \"" + javaCode + "\" ]");
	    writer.flush();
	    writer = new BufferedWriter(new OutputStreamWriter(os, javaCode));
	    e.setOut(writer);
	}

	// The actual info: locals, delimiters, local templates, main structure.
	e.emit();
	e.emit(";:- Delimiter Structure");
	e.down("(define " + delimVar + " '(");
	genDelims(file, e);
	e.done("))");
	e.emit("");
	e.emit(";:- Global Template Reference");
	genGlobalTemplates(file, e);
	e.emit(";:- End Global Template Reference");
	e.emit("");
	e.emit(";:- Local Template Definition");
	genLocalTemplates(file, e);
	e.emit(";:- End Local Template Definition");
	e.emit("");
	e.emit(";:- MsgStructure Definition");
	e.down("(define " + structVar
	    + " ($resolve-event-definition (quote");
	genNode(file.getEtd(), e);
	e.done(")))");
	e.emit(";:- End MsgStructure Definition");
	e.close();
	// os.close();
    }

    /**
     * Maps ETD type info to SSC node type string.
     *
     * @param t  type info as encoded in MessageStructureNode.getNodeType()
     * @return the Monk node type name, or null if unknown
     */
    private String mapType (int t)
    {
	String result = null;
	switch (t & MessageStructureNode.SOT_MASK)
	{
	case MessageStructureNode.AFp:  result = "AF"; break;
	case MessageStructureNode.AFs:  result = "AF"; break;
	case MessageStructureNode.ANAp: result = "ANA"; break;
	case MessageStructureNode.ANAs: result = "ANA"; break;
	case MessageStructureNode.ANp:  result = "AN"; break;
	case MessageStructureNode.ANs:  result = "AN"; break;
	case MessageStructureNode.ASp:  result = "AS"; break;
	case MessageStructureNode.ASs:  result = "AS"; break;
	case MessageStructureNode.GTF:  result = "GTF"; break;
	case MessageStructureNode.GTN:  result = "GTN"; break;
	case MessageStructureNode.GTS:  result = "GTS"; break;
	case MessageStructureNode.LTF:  result = "LTF"; break;
	case MessageStructureNode.LTN:  result = "LTN"; break;
	case MessageStructureNode.LTS:  result = "LTS"; break;
	case MessageStructureNode.OFp:  result = "OF"; break;
	case MessageStructureNode.OFs:  result = "OF"; break;
	case MessageStructureNode.ONAp: result = "ONA"; break;
	case MessageStructureNode.ONAs: result = "ONA"; break;
	case MessageStructureNode.ONp:  result = "ON"; break;
	case MessageStructureNode.ONs:  result = "ON"; break;
	case MessageStructureNode.OSp:  result = "OS"; break;
	case MessageStructureNode.OSs:  result = "OS"; break;
	}
	return result;
    }

    /**
     * Generates a single delimiter representation.
     *
     * @param one  the delimiter
     * @param e  the output stream
     * @throws IOException for output problems
     */
    private void genOneDelim (Xsc.OneDelim one, Emit e)
	throws IOException
    {
	String text = one.getText();
	String jenc = one.getCode();
	String menc = java2monkCode(jenc);
	int offset = one.getOffset();
	int length = one.getLength();
	if (length == MessageStructureNode.UNDEFINED)
	{
	    // Explicit delimiter.
	    if (text == null)
		throw new RuntimeException("explicit delimiter has no string");
	    text = makeStringLiteral(text);
	    if (menc != null &&
		! menc.equals("ASCII") &&
		! menc.equals("UTF8"))
	    {
		/* Code differs from output SSC encoding, so use a prefix.
		 * Assumption: output is UTF-8, which includes ASCII.
		 */
		byte[] data = null;
		try
		{
		    data = StringCoderFactory.getStringCoder(jenc).encode(text);
		}
		catch (UnsupportedEncodingException ue)
		{
		    throw new RuntimeException("cannot convert delimiter with ["
			+ jenc + "] encoding: " + ue.getMessage());
		}
		e.part("#" + menc);
		e.flush();
		os.write(data);
	    }
	    else
	    {
		// Assume ASCII string into UTF-8 output, no encoding required.
		e.part(text);
	    }
	}
	else
	{
	    // Implicit delimiter.
	    e.part(Integer.toString(offset));
	    if (length != 1) { e.part(" " + length); }
	}
    }

    /**
     * Generates a single escape delimiter representation.
     *
     * @param escD  the escape delimiter
     * @param e  the output stream
     * @throws IOException for output problems
     */
    private void genOneEscape (Xsc.Escape escD, Emit e)
	throws IOException
    {
      String text = escD.getText();
      String jenc = escD.getCode();
      String menc = java2monkCode(jenc);
      if (text == null)
	throw new RuntimeException("escape delimiter has no string");
      text = makeStringLiteral(text);
      if (menc != null &&
	  ! menc.equals("ASCII") &&
	  ! menc.equals("UTF8"))
	{
	  /* Code differs from output SSC encoding, so use a prefix.
	   * Assumption: output is UTF-8, which includes ASCII.
	   */
	  byte[] data = null;
	  try
	    {
	      data = StringCoderFactory.getStringCoder(jenc).encode(text);
	    }
	  catch (UnsupportedEncodingException ue)
	    {
	      throw new RuntimeException("cannot convert escape delimiter with ["
					 + jenc + "] encoding: " + ue.getMessage());
	    }
	  e.part("#" + menc);
	  e.flush();
	  os.write(data);
	}
      else
	{
	  // Assume ASCII string into UTF-8 output, no encoding required.
	  e.part(text);
	}
    }

    /**
     * Generates delimiter information, global or local.
     *
     * @param delim  the delimiter level description
     * @param global  flag: for global default delimiter list?
     * @param e  the output stream
     * @return whether there was any output
     * @throws IOException for output problems
     */
    private boolean genDelim (Xsc.Delim delim, boolean global, Emit e)
	throws IOException
    {
	if (delim == null)
	    return false;

	boolean first = true;
	for (Xsc.DelimGroup group = delim.getGroups();
	    group != null; group = group.getNext())
	{
	    int beginCount = 0, endCount = 0;
	    Xsc.OneDelim one;
	    for (one = group.getBegins(); one != null; one = one.getNext())
		beginCount ++;
	    for (one = group.getEnds(); one != null; one = one.getNext())
		endCount ++;
	    if (global && first && beginCount == 0 && endCount == 1)
	    {
		// Classic case: single end delimiter only.
		if (! first) { e.part(" "); }
		first = false;
		genOneDelim(group.getEnds(), e);
	    }
	    else if (beginCount + endCount > 0)
	    {
		// New case: begin delimiters, or paired.
		e.part(first ? "(" : " (");
		first = false;
		if (beginCount > 0)
		{
		    e.part("Bd");
		    for (one = group.getBegins();
			one != null; one = one.getNext())
		    {
			e.part(" ");
			genOneDelim(one, e);
		    }
		}
		if (endCount > 0)
		{
		    if (beginCount > 0) { e.part(" "); }
		    e.part("Ed");
		    for (one = group.getEnds();
			one != null; one = one.getNext())
		    {
			e.part(" ");
			genOneDelim(one, e);
		    }
		}
		e.part(")");
	    }
	}
	Xsc.Escape escD = delim.getEscapes();
	if (null != escD)
	  {
	    e.part("(EscD");
	    for (; escD != null; escD = escD.getNext())
	      {
		e.part(" ");
		genOneEscape(escD, e);
	      }
	    e.part(")");
	  }

	int type = delim.getFlags();
	if ((type & delim.ENDOFREC) != 0)
	    e.part(" endofrec");
	if ((type & delim.REQUIRED) != 0)
	    e.part(" required");
	if ((type & delim.SEPARATOR) != 0)
	    e.part(" separator");
	if ((type & delim.ARRAY) != 0)
	    e.part(" array");
	if ((type & delim.ANCHORED) != 0)
	    e.part(" anchored");
	if ((type & delim.BEGINANCH) != 0)
	    e.part(" beginanchored");
	if ((type & delim.ENDANCH) != 0)
	    e.part(" endanchored");

	return ! (type == 0 && first);
    }

    /**
     * Generates the SSC global default delimiter list definition.
     *
     * @param file  the XSC info
     * @param e  the output stream
     * @throws IOException for output problems
     */
    private void genDelims (SSC_File file, Emit e)
	throws IOException
    {
	for (Xsc.Delim delim = file.getDelimiters();
	    delim != null; delim = delim.getNext())
	{
	    e.part("(");
	    genDelim(delim, true, e);
	    e.emit(")");
	}
    }

    /**
     * Adds all external references at or below this node to the set.
     *
     * @param loads  set to modify
     * @param node  field node to recursively scan for references
     */
    private void findRefs (HashSet loads, MessageStructureNode node)
    {
	String refPath = node.getRefPath();
	if (refPath != null)
	    loads.add(refPath);
	for (node = node.getFirstChild(); node != null;
	    node = node.getNextSibling())
		findRefs(loads, node);
    }

    /**
     * Generates the list of external template loads.
     *
     * @param file  internal representation of the XSC file
     * @param e  the output stream
     */
    private void genGlobalTemplates (SSC_File file, Emit e)
    {
	HashSet loads = new HashSet();

	findRefs(loads, file.getEtd());
	String[] names = file.localTemplateNames();
	for (int i = 0; i < names.length; i++)
	    findRefs(loads, file.resolveLocalTemplate(names[i]));
	Object[] paths = loads.toArray();
	for (int i = 0; i < paths.length; i++)
	{
	    String ref = (String) paths[i];
	    if (ref.endsWith(".xsc"))
		ref = ref.substring(0, ref.length() - 4);
	    if (! ref.endsWith(".ssc"))
		ref = ref + ".ssc";
	    e.emit("(load \"" + ref + "\")");
	}
    }

    /**
     * Emits the SSC declarations for all the local templates.
     *
     * @param file  description of the whole XSC file
     * @param e  the output stream
     * @throws IOException for output problems
     */
    private void genLocalTemplates (SSC_File file, Emit e)
	throws IOException
    {
	String[] names = file.localTemplateNames();
	Hashtable from = new Hashtable();
	for (int i = 0; i < names.length; i++)
	    from.put(names[i], file.resolveLocalTemplate(names[i]));
	for (int i = 0; i < names.length; i++)
	{
	    if (debug)
		System.out.print("[ consider <" + names[i] + "> ]\n");
	    genLocalTemplateRecur(names[i], from, e, "[top]");
	}
    }

    /**
     * Emits the SSC declarations for the named local template.
     *
     * @param node  descriptor of the template's root node
     * @param name  the name of the local template
     * @param e  the output stream
     * @throws IOException for output problems
     */
    private void genLocalTemplate
	(MessageStructureNode node, String name, Emit e)
	throws IOException
    {
	if (debug)
	    System.out.print("[ emit ssc <" + name + "> ]\n");
	e.down("(define " + name
	    + "-struct ($resolve-event-definition (quote");
	genNode(node, e);
	e.done(")))");
    }

    /**
     * Emits the SSC declarations for the named local template.
     * Don't emit if it was already emitted earlier.
     * Prior to this, emit the declarations for all other local
     * templated referred to by this template, if they haven't been
     * emitted already.  Keep track of emitted templates in "from".
     *
     * @param name  the name of the local template
     * @param from  a list of all templates, marked if emitted
     * @param e  the output stream
     * @param in  the template reference chain to this point
     * @throws IOException for output problems
     */
    private void genLocalTemplateRecur
	(String name, Hashtable from, Emit e, String in)
	throws IOException
    {
	if (debug)
	    System.out.print("[ template <" + name + "> from <"
		+ in + "> ]\n");
	Object what = from.get(name);
	if (what instanceof MessageStructureNode)
	{
	    MessageStructureNode root = (MessageStructureNode) what;
	    from.put(name, new Boolean(true));
	    genLocalRefsBelow(root, from, e, in + " -> " + name);
	    genLocalTemplate(root, name, e);
	    from.put(name, new Boolean(false));
	}
    }

    /**
     * Emits the code for all internal templates referred to by this node
     * or its children, if they were not emitted yet.
     * The "from" table contains an extry for each define internal template,
     * mapping the template name to a value that specifies if it has been
     * emitted.  If the value is a Boolean, "true" means it's being emitted,
     * and "false" means it has been.  Otherwise, it must be a node descriptor,
     * indicating we have not emitted this one yet.
     *
     * @param node  the node to check recursively
     * @param from  a list of all templates, marked if emitted
     * @param e  the output stream
     * @param in  the template reference chain to this point
     * @throws IOException for output problems
     */
    private void genLocalRefsBelow (MessageStructureNode node, Hashtable from,
	Emit e, String in)
	throws IOException
    {
	if ((node.getNodeType() & MessageStructureNode.LOCAL_REF) != 0)
	{
	    // This is a local template reference.
	    String link = node.getLink();
	    Object what = from.get(link);
	    if (what == null)
		throw new IOException("reference to undefined template \""
		    + link + "\" from node \"" + node.getNodeName()
		    + "\" in " + in);
	    if (what instanceof MessageStructureNode)
	    {
		// Found a dendency not previously emitted.
		genLocalTemplateRecur(link, from, e, in);
	    }
	    else if (((Boolean) what).booleanValue())
	    {
		// We have a circular dependency.
		throw new IOException(
		    "circular local template dependency: "
		    + in + " -> " + link);
	    }
	}
	// Check the children.
	for (node = node.getFirstChild(); node != null;
	    node = node.getNextSibling())
		genLocalRefsBelow(node, from, e, in);
    }

    /**
     * Emits a field description in an SSC file, including its children.
     *
     * @param node  the field descriptor of the node
     * @param e  the output stream
     * @throws IOException for output problems
     */
    private void genNode
	(MessageStructureNode node, Emit e)
	throws IOException
    {
	String name = node.getNodeName();
	String nodeType = mapType(node.getNodeType());
	int min = node.getMinRep();
	int max = node.getMaxRep();
	String link = node.getLink();
	if (link != null && link.endsWith(".xsc"))
	    link = link.substring(0, link.length() - 4);
	String ref = node.getRefPath();
	String iMatch = node.getTag();
	String dValue = node.getDefaultString();
	byte[] dBytes = node.getDefaultBytes();
	String dCode  = node.getDefaultCode();
	if (dCode == null || dCode.equals(sscEncoding))
	{
	    // No explicit literal prefix for default value.
	    dCode = "";
	}
	else
	{
	    // Need explicit prefix.
	    dCode = "#" + java2monkCode(
		(dCode.startsWith(":") ? dCode = dCode.substring(1) : dCode));
	}
	if (ref != null)
	{
	    /* External reference.  The SSC node will be something like:
	     *  (msg ON 0 1 "hl7/2.2/a01.ssc" a01-struct und und)
	     */
	    SSC_File ssc = fileSet.resolveGlobalTemplate(ref);
	    if (ssc == null)
		throw new RuntimeException("unresolved external reference ["
		    + ref + "]");
	    MessageStructureNode root = ssc.getEtd();
	    if (root == null)
		throw new RuntimeException("rootless external template ["
		    + ref + "]");
	    if (ref.endsWith(".xsc"))
		ref = ref.substring(0, ref.length() - 4);
	    if (! ref.endsWith(".ssc"))
		ref = ref + ".ssc";
	    iMatch = makeStringLiteral(ref);
	    dValue = root.getNodeName() + "-struct";
	}
	else if (link != null)
	{
	    /* Internal reference to local template.  Will produce SSC like:
	     *  (msg ON 0 1 und a01-struct und und)
	     */
	    iMatch = "und";
	    dValue = link + "-struct";
	}
	else
	{
	    // Composite or leaf node; fix up match pattern and default output.
	    iMatch = ((iMatch == null) ? "und" : makeStringLiteral(iMatch));
	    dValue = ((dValue == null) ? "und" : makeStringLiteral(dValue));
	}
	int offset = node.getOffset();
	int lentyp = node.getLenTyp();
	int length = node.getLength();
	int lenoff = node.getLenOff();
	int lensiz = node.getLenSiz();
	int chMin = node.getChildMin();
	int chMax = node.getChildMax();
	boolean scaOut = node.getScavOutput();
	String scaChr = node.getScavenger();
	e.part("(");

	// Emit local delimiter properties.
	String pre = "(";
	if (node.getExact())
	    { e.part(pre + "Ex"); pre = " "; }
	if (node.getGroup())
	    { e.part(pre + "Gr"); pre = " "; }
	if (node.getAvoid())
	    { e.part(pre + "Nt"); pre = " "; }
	if (node.getParentPrec())
	    { e.part(pre + "Pp"); pre = " "; }
	if (chMin != node.UNDEFINED && chMax != node.UNDEFINED)
	{
	    e.part(pre + "(NofN (" + chMin + " "
		+ (chMax == node.UNBOUNDED ? "INF" : Integer.toString(chMax))
		+ ")) ");
	    pre = " ";
	}
	if (scaChr != null)
	{
	    e.part(pre + "(" + (scaOut ? "Sc " : "ScN ")
		+ makeStringLiteral(scaChr) + ") ");
	    pre = " ";
	}
	Xsc.Delim delim = node.getDelim();
	if (delim != null)
	{
	    e.part(pre); pre = " ";
	    genDelim(delim, false, e);
	}
	if (pre.equals(" "))
	    e.part(") ");

	e.part(name + " " + nodeType + " "
	     + min + " " + ((max == node.UNBOUNDED) ? "INF" : "" + max) + " "
	     + iMatch + " ");
	if (dBytes != null)
	{
	    /* When the "defaultBytes" attribute is present, it takes
	     * precedence over the "defaultValue" attribute.  In this case,
	     * we also need to takes special steps to write out the byte
	     * sequence, i.e. bypass the normal character stream encoding.
	     *
	     * NOTE: this code is not perfect, as the quotes should really
	     * be emitted according to the "defaultEncoding" value, not
	     * the current encoding given by sscEncoding...
	     */
	  //e.part(dCode + '"');
	  e.flush();
	  os.write(makeBytesLiteral(dBytes));
	  //e.part("\"");
	}
	else
	{
	    // Ordinary string or "und" symbol.
	    e.part(dValue);
	}
	if (debug)
	    System.out.println("[ offset=<" + offset + ">, length=<" + length
		+ ">, lensiz=<" + lensiz + ">, lenoff=<" + lenoff + "> ]");
	e.part(" "
	    + ((offset == node.UNDEFINED) ? "und" : Integer.toString(offset))
	    + " "
	    + ((lensiz == node.UNDEFINED)
		? ((lentyp == node.UNDEFINED) // simple length
		    ? ((nodeType.equals("OF") || nodeType.equals("AF"))
			? "0" : "-1") // simple implicit
		    : Integer.toString(length)) // simple explicit
		: ("(" + lenoff + " " + lensiz + ")"))); // encoded length
	MessageStructureNode c = node.getFirstChild();
	if (c != null)
	{
	    e.emit();
	    e.indent();
	    for (; c != null; c = c.getNextSibling())
		genNode(c, e);
	    e.undent();
	}
	e.emit(")");
    }

    /**
     * Converts string to Monk string literal representation.
     *
     * @param value  any non-null string
     * @return a quoted Monk string literal
     */
    public static String makeStringLiteral (String value)
    {
	return makeStringLiteral(value, true);
    }

    /**
     * Converts string to Monk string literal representation.
     *
     * @param value  any non-null string
     * @param quote  surround with double-quotes?
     * @return a quoted Monk string literal
     */
    public static String makeStringLiteral (String value, boolean quote)
    {
	StringBuffer buf = new StringBuffer();
	buf.append("\"");
	for (int i = 0, len = value.length(); i < len; i++)
	{
	    char ch = value.charAt(i);
	    switch (ch)
	    {
	    case '\007': buf.append("\\a"); break;
	    case '\010': buf.append("\\b"); break;
	    case '\011': buf.append("\\t"); break;
	    case '\012': buf.append("\\n"); break;
	    case '\013': buf.append("\\v"); break;
	    case '\014': buf.append("\\f"); break;
	    case '\015': buf.append("\\r"); break;

	    case '\\':
		switch ((i+1 < len) ? value.charAt(i+1) : '\0')
		{
		case '(': case ')':
		case '{': case '}':
		case '[': case ']':
		case '?': case '|':
		case '+': case '*':
		case '^': case '$':
		case '.':
		    // Special case in Monk: don't need to escape
		    // backslash before regex meta-characters.
		    buf.append(ch);
		    break;
		default:
		    buf.append('\\');
		    buf.append(ch);
		}
		break;

	    case '"':
		buf.append('\\');
		buf.append(ch);
		break;

	    default:
		if (ch < '\u0020' || ('\u007F' <= ch && ch <= '\u00FF'))
		{
		    // Escape unprintable as hex code.
		    buf.append("\\x");
		    buf.append("0123456789ABCDEF".charAt(ch >> 4));
		    buf.append("0123456789ABCDEF".charAt(ch & 0xF));
		}
		else
		{
		    // Simple character.
		    buf.append(ch);
		}
	    }
	}
	buf.append("\"");
	return buf.toString();
    }

    /**
     * Converts bytes to Monk byte[] literal representation.
     *
     * @param value  any non-null byte[]
     * @return a quoted Monk byte[] literal
     */
    public static byte[] makeBytesLiteral (byte[] value)
    {
      int len		= value.length;
      int newLen	= len + 2;

      for (int i = 0; i < len; i++)
	{
	  switch (value[i])
	    {
	    case '\\':
	    case '"':
	      newLen++;
	      break;
	    }
	} 
      byte[] buf = new byte[newLen];
      buf[0] = '"';
      for (int i = 0, j = 1; i < len; i++, j++)
	{
	  switch (value[i])
	    {
	    case '\\':
	    case '"':
	      buf[j++] = '\\';
	      break;
	    }
	  buf[j] = value[i];
	}
      buf[newLen - 1] = '"';
      return buf;
    }

    /**
     * Converts string to Monk symbol name representation.
     *
     * @param value  any non-null string
     * @return unquoted Monk name literal
     */
    public static String makeSymbolLiteral (String value)
    {
	StringBuffer buf = new StringBuffer();
	for (int i = 0, len = value.length(); i < len; i++)
	{
	    char ch = value.charAt(i);
	    switch (ch)
	    {
	    case '\007': buf.append("\\a"); break;
	    case '\010': buf.append("\\b"); break;
	    case '\011': buf.append("\\t"); break;
	    case '\012': buf.append("\\n"); break;
	    case '\013': buf.append("\\v"); break;
	    case '\014': buf.append("\\f"); break;
	    case '\015': buf.append("\\r"); break;

	    case '\\':
	    case '"':
		buf.append('\\');
		buf.append(ch);
		break;

	    default:
		if (ch < '\u0020' || ('\u007F' <= ch && ch <= '\u00FF'))
		{
		    // Escape unprintable as hex code.
		    buf.append("\\x");
		    buf.append("0123456789ABCDEF".charAt(ch >> 4));
		    buf.append("0123456789ABCDEF".charAt(ch & 0xF));
		}
		else
		{
		    // Simple character.
		    buf.append(ch);
		}
	    }
	}
	return buf.toString();
    }
}
