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
 * @(#)Xsc.java 
 *
 * Copyright 2004-2008 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.stc.jcs.ssc;

import com.sun.stc.jcs.ssc.MessageStructureNode;

/**
 * Wrapper class for XSC data.
 * Contains classes that correspond to the various entities defined
 * in an XSC file.  Note that these classes are supposed to be an internal
 * format only, not necessarily in strict 1-to-1 correspondence with XSC
 * external (file-based) text representations.
 */
public class Xsc
{
    /**
     * Class to represent an extra JAR file for the ETD.
     * Corresponds to the XSC &lt;jar&gt; entity.
     */
    public static class Jar
    {
	private String file; // eGate-relative path to the JAR file
	private String comm;
	private String guid;
	private Jar next;  // sibling

	/**
	 * Constructs from XSC attributes.
	 *
	 * @param comm  comment string, or null
	 * @param guid  UID value, or null
	 * @param file  the eGate-relative path to the JAR file, or null
	 */
	public Jar (String comm, String guid, String file)
	{
	    this.file = file;
	    this.comm = comm;
	    this.guid = guid;
	    this.next = null;
	}

	/**
	 * Appends JAR file to given list.
	 *
	 * @param list  the list to append to, may be null
	 * @return the modified list, linked by "next"
	 */
	public Jar appendTo (Jar list)
	{
	    if (list == null)
		return this;

	    Jar rest = list.getNext();
	    if (rest == null)
		list.setNext(this);
	    else
		appendTo(rest);
	    return list;
	}

	// Access.
	public String getFile ()    { return file; }
	public String getComment () { return comm; }
	public String getUid ()     { return guid; }
	public Jar getNext ()       { return next; }
	public void setNext (Jar next) { this.next = next; }
    }

    /**
     * Class to represent the Java-specific XSC information.
     * Corresponds to the XSC &lt;javaProps&gt; entity.
     */
    public static class JavaProps
    {
	private boolean code; // code available?
	private String pack; // Java package name
	private String base; // Java class basename
	private String jarf; // eGate-relative path to the JAR file
	private String wrap; // eGate-relative path of a "derived" ETD's wrapper
	private Jar jars; // list of additional JAR files
	private String comm;
	private String guid;
	//-private JavaProps next;  // sibling

	/**
	 * Constructs from XSC attributes.
	 *
	 * @param comm  comment string, or null
	 * @param guid  UID value, or null
	 * @param code  code avaialble?
	 * @param pack  Java package name, or null
	 * @param base  Java class basename, or null
	 * @param jarf  the eGate-relative path to the JAR file, or null
	 * @param wrap  the eGate-relative path of a "derived" ETD's wrapper
	 */
	public JavaProps (String comm, String guid, boolean code,
	    String pack, String base, String jarf, String wrap)
	{
	    this.code = code;
	    this.pack = pack;
	    this.base = base;
	    this.jarf = jarf;
	    this.wrap = wrap;
	    this.jars = null;
	    this.comm = comm;
	    this.guid = guid;
	    //-this.next = null;
	}

	/**
	 * Append new JAR file to list.
	 *
	 * @param jar  the JAR file to add
	 */
	public void addJar (Jar jar)
	{
	    jars = jar.appendTo(jars);
	}

	/**
	 * Computes the fully qualified class name.
	 * This assumes package and base class have been set.
	 * Convenience function.
	 *
	 * @return the Java class name
	 */
	public String getFqClass ()
	{
	    return ((pack == null || pack.equals("")) ? "" : (pack + "."))
		+ base;
	}

	// Access.
	public boolean getCode ()   { return code; }
	public String getPackage () { return pack; }
	public String getBase ()    { return base; }
	public String getJarFile () { return jarf; }
	public String getSource ()  { return wrap; }
	public Jar getJars ()       { return jars; }
	public String getComment () { return comm; }
	public String getUid ()     { return guid; }
	//- public JavaProps getNext() { return next; }
	//- public void setNext (JavaProps next) { this.next = next; }
	public void setCode (boolean code)   { this.code = code; }
	public void setPackage (String pack) { this.pack = pack; }
	public void setBase (String base)    { this.base = base; }
	public void setJarFile (String jarf) { this.jarf = jarf; }
	public void setSource (String wrap)  { this.wrap = wrap; }
	public void setJars (Jar jars)       { this.jars = jars; }
    }

    /**
     * Class to represent a single delimiter, local or global.
     * Corresponds to the XSC &lt;beginDelim&gt; or &lt;endDelim&gt; entity.
     * Delimiters can be either explicit (defined as a string) or
     * implicit (specified in the event itself; define by length/offset).
     */
    public static class OneDelim
    {
	private int length;   // length of implicit delimiter; 0=explicit
	private int offset;   // offset to implicit delimiter, if length>0
	private byte[] data;  // raw byte representation, or null
	private String text;  // delimiter as text, or null
	private String code;  // encoding, or null
	private String comm;
	private String guid;
	private OneDelim next;  // sibling

	/**
	 * Constructs explicit delimiter from XSC attributes.
	 *
	 * @param comm  comment string, or null
	 * @param guid  UID value, or null
	 * @param data  the delimiter raw byte representation, or null
	 * @param text  delimiter as text, or null
	 * @param code  the encoding of the text, or null
	 */
	public OneDelim (String comm, String guid, byte[] data, String text,
	    String code)
	{
	    this.length = MessageStructureNode.UNDEFINED;
	    this.offset = MessageStructureNode.UNDEFINED;
	    this.data = data;
	    this.text = text;
	    this.code = code;
	    this.comm = comm;
	    this.guid = guid;
	    this.next = null;
	}

	/**
	 * Constructs implicit delimiter from XSC attributes.
	 *
	 * @param comm  comment string, or null
	 * @param guid  UID value, or null
	 * @param length  the delimiter length, in bytes
	 * @param offset  offset from event start, in bytes
	 */
	public OneDelim (String comm, String guid, int length, int offset)
	{
	    this.length = length;
	    this.offset = offset;
	    this.data = null;
	    this.text = null;
	    this.code = null;
	    this.comm = comm;
	    this.guid = guid;
	    this.next = null;
	}

	/**
	 * Appends delimiter to given list.
	 *
	 * @param list  the list to append to, may be null
	 * @return the modified list, linked by "next"
	 */
	public OneDelim appendTo (OneDelim list)
	{
	    if (list == null)
		return this;

	    OneDelim rest = list.getNext();
	    if (rest == null)
		list.setNext(this);
	    else
		appendTo(rest);
	    return list;
	}

	/**
	 * Tests if this delimiter is implicit.
	 *
	 * @return true if implicit, false if explicit
	 */
	public boolean implicit ()
	{
	    return length <= 0;
	}

	// Access.
	public int getLength ()     { return length; }
	public int getOffset ()     { return offset; }
	public byte[] getData ()    { return data; }
	public String getText ()    { return text; }
	public String getCode ()    { return code; }
	public String getComment () { return comm; }
	public String getUid ()     { return guid; }
	public OneDelim getNext ()  { return next; }
	public void setNext (OneDelim next) { this.next = next; }
    }

    /**
     * Class to represent a group of paired delimiters.
     * Corresponds to the XSC &lt;delimGroup&gt; entity.
     */
    public static class DelimGroup
    {
	private OneDelim begs, ends;
	private String comm;
	private String guid;
	private DelimGroup next;

	/**
	 * Constructs with given begin/end delimiters.
	 *
	 * @param comm  comment string, or null
	 * @param guid  UID value, or null
	 * @param begs  begin-delimiter list, linked by "next"
	 * @param ends  end-delimiter list, linked by "next"
	 */
	public DelimGroup
	    (String comm, String guid, OneDelim begs, OneDelim ends)
	{
	    this.begs = begs;
	    this.ends = ends;
	    this.comm = comm;
	    this.guid = guid;
	    this.next = null;
	}

	/**
	 * Constructs empty.
	 *
	 * @param comm  comment string, or null
	 * @param guid  UID value, or null
	 */
	public DelimGroup (String comm, String guid)
	{
	    this(comm, guid, null, null);
	}

	/**
	 * Appends delimiter to begin-delimiter list.
	 *
	 * @param d  new delimiter
	 */
	public void addBegin (OneDelim d)
	{
	    begs = d.appendTo(begs);
	}

	/**
	 * Appends delimiter to end-delimiter list.
	 *
	 * @param d  new delimiter
	 */
	public void addEnd (OneDelim d)
	{
	    ends = d.appendTo(ends);
	}

	/**
	 * Appends delimiter group to given list.
	 *
	 * @param list  the list to append to, may be null
	 * @return the modified list, linked by "next"
	 */
	public DelimGroup appendTo (DelimGroup list)
	{
	    if (list == null)
		return this;

	    DelimGroup rest = list.getNext();
	    if (rest == null)
		list.setNext(this);
	    else
		appendTo(rest);
	    return list;
	}

	/**
	 * Append new delimiter to group.
	 *
	 * @param begin  flag: add as begin-delimiter?
	 * @param delim  the delim to add
	 */
	public void addOne (boolean begin, OneDelim delim)
	{
	    if (begin)
		begs = delim.appendTo(begs);
	    else
		ends = delim.appendTo(ends);
	}

	// Access.
	public OneDelim getBegins () { return begs; }
	public OneDelim getEnds ()   { return ends; }
	public String getComment ()  { return comm; }
	public String getUid ()      { return guid; }
	public DelimGroup getNext () { return next; }
	public void setBegins (OneDelim begs) { this.begs = begs; }
	public void setEnds (OneDelim ends) { this.ends = ends; }
	public void setNext (DelimGroup next) { this.next = next; }
    }
    /**
     * Class to represent an escape delimiter, local or global.
     * Corresponds to the XSC &lt;escape&gt entity.
     * Escape delimiters are explicit.
     */
    public static class Escape
    {
	private byte[] data;  // raw byte representation, or null
	private String text;  // escape as text, or null
	private String code;  // encoding, or null
	private String comm;
	private String guid;
	private Escape next;  // sibling

	/**
	 * Constructs explicit escape from XSC attributes.
	 *
	 * @param comm  comment string, or null
	 * @param guid  UID value, or null
	 * @param data  the escape raw byte representation, or null
	 * @param text  escape as text, or null
	 * @param code  the encoding of the text, or null
	 */
	public Escape (String comm, String guid, byte[] data, String text,
	    String code)
	{
	    this.data = data;
	    this.text = text;
	    this.code = code;
	    this.comm = comm;
	    this.guid = guid;
	    this.next = null;
	}

	/**
	 * Appends escape to given list.
	 *
	 * @param list  the list to append to, may be null
	 * @return the modified list, linked by "next"
	 */
	public Escape appendTo (Escape list)
	{
	    if (list == null)
		return this;

	    Escape rest = list.getNext();
	    if (rest == null)
		list.setNext(this);
	    else
		appendTo(rest);
	    return list;
	}

	// Access.
	public byte[] getData ()    { return data; }
	public String getText ()    { return text; }
	public String getCode ()    { return code; }
	public String getComment () { return comm; }
	public String getUid ()     { return guid; }
	public Escape getNext ()  { return next; }
	public void setNext (Escape next) { this.next = next; }
    }


    /**
     * Class to represent a local or global delimiter level.
     * Corresponds to the XSC &lt;delim&gt; entity.
     * This represents a set of modifiers, plus a list of groups of
     * paired begin/end delimiters and escape delimiters.
     * The order of the delimiters is significant, in that the first is
     * used for marshaling.
     */
    public static class Delim
    {
	public static final int ANCHORED  = 1 << 0;
	public static final int BEGINANCH = 1 << 1;
	public static final int ENDANCH   = 1 << 2;
	public static final int ARRAY     = 1 << 3;
	public static final int ENDOFREC  = 1 << 4;
	public static final int REQUIRED  = 1 << 5;
	public static final int SEPARATOR = 1 << 6;

	private int flags; // bits for boolean modifiers
	private DelimGroup groups;
	private Escape escapes;
	private String comm;
	private String guid;
	private Delim next;

	/**
	 * Constructs with given flags.
	 *
	 * @param comm  comment string, or null
	 * @param guid  UID value, or null
	 * @param flags  see ANCHORED etc. above
	 */
	public Delim (String comm, String guid, byte flags)
	{
	    this.flags = flags;
	    this.groups = null;
	    this.escapes = null;
	    this.comm = comm;
	    this.guid = guid;
	    this.next = null;
	}

	/**
	 * Appends delimiter level to given list.
	 *
	 * @param list  the list to append to, may be null
	 * @return the modified list, linked by "next"
	 */
	public Delim appendTo (Delim list)
	{
	    if (list == null)
		return this;

	    Delim rest = list.getNext();
	    if (rest == null)
		list.setNext(this);
	    else
		appendTo(rest);
	    return list;
	}

	/**
	 * Append new delimiter group.
	 *
	 * @param group  the group to add
	 */
	public void addGroup (DelimGroup group)
	{
	    if (group != null)
	        groups = group.appendTo(groups);
	}

	/**
	 * Append new escape delimiter.
	 *
	 * @param escape  the escape to add
	 */
	public void addEscape (Escape escape)
	{
	    if (escape != null)
	        escapes = escape.appendTo(escapes);
	}

	// Flag tests.
	public boolean isArray () { return (flags & ARRAY) != 0; }

	// Access.
	public int getFlags () { return flags; }
	public DelimGroup getGroups () { return groups; }
	public Escape getEscapes () { return escapes; }
	public String getComment () { return comm; }
	public String getUid () { return guid; }
	public Delim getNext () { return next; }
	public void setFlags (byte flags) { this.flags = flags; }
	public void setGroups (Xsc.DelimGroup groups) { this.groups = groups; }
	public void setEscapes (Xsc.Escape escapes) { this.escapes = escapes; }
	public void setNext (Delim next) { this.next = next; }
    }

    /**
     * Class to represent a method parameter.
     * Corresponds to the XSC &lt;param&gt; entity.
     */
    public static class Param
    {
	private String name;
	private String type;
	private String comm;
	private String guid;
	private Param next;

	/**
	 * Constructs with given flags.
	 *
	 * @param comm  comment string, or null
	 * @param guid  UID value, or null
	 * @param name  parameter name
	 * @param type  Java type of parameter (fully qualified)
	 */
	public Param (String comm, String guid, String name, String type)
	{
	    this.name = name;
	    this.type = type;
	    this.comm = comm;
	    this.guid = guid;
	    this.next = null;
	}

	/**
	 * Appends parameter to given list.
	 *
	 * @param list  the list to append to, may be null
	 * @return the modified list, linked by "next"
	 */
	public Param appendTo (Param list)
	{
	    if (list == null)
		return this;

	    Param rest = list.getNext();
	    if (rest == null)
		list.setNext(this);
	    else
		appendTo(rest);
	    return list;
	}

	// Access.
	public String getName ()    { return name; }
	public String getType ()    { return type; }
	public String getComment () { return comm; }
	public String getUid ()     { return guid; }
	public Param getNext ()     { return next; }
	public void setNext (Param next) { this.next = next; }
    }

    /**
     * Class to represent a method exception.
     * Corresponds to the XSC &lt;throws&gt; entity.
     */
    public static class Throws
    {
	private String type;
	private String comm;
	private String guid;
	private Throws next;

	/**
	 * Constructs with given flags.
	 *
	 * @param comm  comment string, or null
	 * @param guid  UID value, or null
	 * @param type  exception type
	 */
	public Throws (String comm, String guid, String type)
	{
	    this.type = type;
	    this.comm = comm;
	    this.guid = guid;
	    this.next = null;
	}

	/**
	 * Appends parameter to given list.
	 *
	 * @param list  the list to append to, may be null
	 * @return the modified list, linked by "next"
	 */
	public Throws appendTo (Throws list)
	{
	    if (list == null)
		return this;

	    Throws rest = list.getNext();
	    if (rest == null)
		list.setNext(this);
	    else
		appendTo(rest);
	    return list;
	}

	// Access.
	public String getType ()    { return type; }
	public String getComment () { return comm; }
	public String getUid ()     { return guid; }
	public Throws getNext ()    { return next; }
	public void setNext (Throws next) { this.next = next; }
    }

    /**
     * Class to represent a method.
     * Corresponds to the XSC &lt;method&gt; entity.
     */
    public static class Method
    {
	private String name;
	private String type;
	private Param pars;
	private Throws excs;
	private String comm;
	private String guid;
	private Method next;

	/**
	 * Constructs with given flags.
	 *
	 * @param comm  comment string, or null
	 * @param guid  UID value, or null
	 * @param name  method name
	 * @param type  Java return type of method (fully qualified)
	 */
	public Method (String comm, String guid, String name, String type)
	{
	    this.name = name;
	    this.type = type;
	    this.pars = null;
	    this.excs = null;
	    this.comm = comm;
	    this.guid = guid;
	    this.next = null;
	}

	/**
	 * Appends parameter to given list.
	 *
	 * @param list  the list to append to, may be null
	 * @return the modified list, linked by "next"
	 */
	public Method appendTo (Method list)
	{
	    if (list == null)
		return this;

	    Method rest = list.getNext();
	    if (rest == null)
		list.setNext(this);
	    else
		appendTo(rest);
	    return list;
	}

	/**
	 * Append new parameter to the method's parameter list.
	 *
	 * @param par  the parameter to add
	 */
	public void addParam (Param par)
	{
	    pars = par.appendTo(pars);
	}

	/**
	 * Append new exception to the method's exception list.
	 *
	 * @param exc  the exception to add
	 */
	public void addThrows (Throws exc)
	{
	    excs = exc.appendTo(excs);
	}

	// Access.
	public String getName () { return name; }
	public String getType () { return type; }
	public Xsc.Param getParams () { return pars; }
	public Xsc.Throws getThrows () { return excs; }
	public String getComment () { return comm; }
	public String getUid () { return guid; }
	public Method getNext () { return next; }
	public void setParams (Param pars) { this.pars = pars; }
	public void setThrows (Throws excs) { this.excs = excs; }
	public void setNext (Method next) { this.next = next; }
    }
}
