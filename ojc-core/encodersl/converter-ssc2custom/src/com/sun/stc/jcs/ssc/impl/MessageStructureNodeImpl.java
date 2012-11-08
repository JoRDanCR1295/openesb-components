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
 * @(#)MessageStructureNodeImpl.java 
 *
 * Copyright 2004-2008 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.stc.jcs.ssc.impl;

import com.sun.stc.jcs.ssc.*;

/**
 * XSC support for SSC-based event structures.
 */
public class MessageStructureNodeImpl
    implements MessageStructureNode
{
    public MessageStructureNodeImpl
	(MessageStructureNode parent, String nodeName)
    {
	this.nodeName = nodeName;
	javaName = nickName = null;
	nodeType = -1;
	access = A_NONE;
	optional = false;
	override = false;
	minRep = 1;
	maxRep = 1;
	offset = this.UNDEFINED;
	length = 0;
	lentyp = this.UNDEFINED;
	lensiz = this.UNDEFINED;
	lenoff = this.UNDEFINED;
	delimType = 0;
	beginDelim = null;
	endDelim = null;
	delim = null;
	scavenger = null;
	scavOutput = parentPrec = avoid = exact = group = false;
	childMin = this.UNDEFINED;
	childMax = this.UNDEFINED;
	methods = null;
	if (parent != null)
	    parent.appendChild(this);
	fixedValue = null;
	defaultValue = null;
	refNode = refPath = null;
	refCode = 0;
    }

    /**
     * Tests whether node is a leaf node.
     *
     * @return the test result
     */
    public boolean isLeaf ()
    {
	return (nodeType & (LOCAL_REF|REFERENCE)) == 0 && firstChild == null;
    }

    /**
     * Tests whether node is an array node.
     *
     * @return the test result
     */
    public boolean isArray () {
	return 0 != (nodeType & ARRAY);
    }

    /**
     * Tests whether node is a template reference.
     *
     * @return the test result
     */
    public boolean isReference ()
    {
	return (nodeType & (LOCAL_REF|REFERENCE)) != 0;
    }

    public SSC_File getFile ()
	{ return file; }

    public void setFile (SSC_File file)
	{ this.file = file; }

    public String getNodeName ()
	{ return nodeName; }
    public void setNodeName (String nodeName)
	{ this.nodeName = nodeName; }

    public String getJavaName ()
	{ return (javaName == null) ? nodeName : javaName; }
    public void setJavaName (String javaName)
	{ this.javaName = javaName; }

    public String getNickName ()
	{ return nickName; }
    public void setNickName (String nickName)
	{ this.nickName = nickName; }

    public boolean getGlobal ()
	{ return global; }
    public void setGlobal (boolean global)
	{ this.global = global; }

    public int getNodeType ()
	{ return nodeType; }
    public void setNodeType (int type)
	{ nodeType = type; }

    public String getJavaType ()
	{ return javaType; }
    public void setJavaType (String type)
	{ javaType = type; }

    public String getEncoding ()
	{ return encoding; }
    public void setEncoding (String encoding)
	{ this.encoding = encoding; }

    public String getFormat ()
	{ return format; }
    public void setFormat (String format)
	{ this.format = format; }

    public void setAccess (byte n)
    {
	switch (n)
	{
	case A_NONE:
	case A_READ:
	case A_WRITE:
	case A_MODIFY:
            access = n;
	    break;
	default:
	    throw new IllegalArgumentException("invalid access value " + n);
        }
    }
    public byte getAccess ()
	{ return access; }
    public boolean isReadable ()
	{ return access == A_READ || access == A_MODIFY; }
    public boolean isWritable ()
	{ return access == A_WRITE || access == A_MODIFY; }

    public void setOptional (boolean n)
	{ optional = n; }
    public boolean getOptional ()
	{ return optional; }

    public void setOverride (boolean n)
	{ override = n; }
    public boolean getOverride ()
	{ return override; }

    public void setMinRep (int n)
	{ minRep = n; }
    public int getMinRep ()
	{ return minRep; }

    public void setMaxRep (int n)
	{ maxRep = n; }
    public int getMaxRep ()
	{ return maxRep; }

    public void setTag (String tag)
	{ this.tag = tag; }
    public String getTag ()
	{ return tag; }

    public void setDefaultCode (String enc)
	{ defaultCode = enc; }
    public String getDefaultCode ()
	{ return defaultCode; }

    public void setDefaultBytes (byte[] data)
	{ defaultBytes = data; }
    public byte[] getDefaultBytes ()
	{ return defaultBytes; }

    public void setDefaultString (String data)
	{ defaultString = data; }
    public String getDefaultString ()
	{ return defaultString; }

    public void setLink (String link)
	{ this.link = link; }
    public String getLink ()
	{ return link; }

    public void setRefPath (String refPath)
	{ this.refPath = refPath; }
    public String getRefPath ()
	{ return refPath; }

    public void setRefNode (String refNode)
	{ this.refNode = refNode; }
    public String getRefNode ()
	{ return refNode; }

    public void setRefCode (int refCode)
	{ this.refCode = refCode; }
    public int getRefCode ()
	{ return refCode; }

    public void setOffset (int offset)
	{ this.offset = offset; }
    public int getOffset ()
	{ return offset; }

    public void setLength (int length)
	{ this.length = length; }
    public int getLength ()
	{ return length; }

    public void setLenTyp (int lentyp)
	{ this.lentyp = lentyp; }
    public int getLenTyp ()
	{ return lentyp; }

    public void setLenOff (int lenoff)
	{ this.lenoff = lenoff; }
    public int getLenOff ()
	{ return lenoff; }

    public void setLenSiz (int lensiz)
	{ this.lensiz = lensiz; }
    public int getLenSiz ()
	{ return lensiz; }

    public void setUid (String uid)
	{ this.uid = uid; }
    public String getUid ()
	{ return uid; }

    public void setComment (String comment)
	{ this.comment = comment; }
    public String getComment ()
	{ return comment; }

    public int getDelimType ()
    {
       Xsc.Delim delim = getDelim();
       return (delim == null) ? 0 : delim.getFlags();
    }

    public void setDelim (Xsc.Delim delim)
	{ this.delim = delim; }
    public Xsc.Delim getDelim ()
	{ return delim; }

    public void setMethods (Xsc.Method methods)
	{ this.methods = methods; }
    public Xsc.Method getMethods ()
	{ return methods; }

    public void setScavenger (String scavenger)
	{ this.scavenger = scavenger; }
    public String getScavenger ()
	{ return scavenger; }

    public void setScavOutput (boolean scavOutput)
	{ this.scavOutput = scavOutput; }
    public boolean getScavOutput ()
	{ return scavOutput; }

    public void setParentPrec (boolean parentPrec)
	{ this.parentPrec = parentPrec; }
    public boolean getParentPrec ()
	{ return parentPrec; }

    public void setExact (boolean exact)
	{ this.exact = exact; }
    public boolean getExact ()
	{ return exact; }

    public void setGroup (boolean group)
	{ this.group = group; }
    public boolean getGroup ()
	{ return group; }

    public void setAvoid (boolean avoid)
	{ this.avoid = avoid; }
    public boolean getAvoid ()
	{ return avoid; }

    public void setChildMin (int n)
	{ childMin = n; }
    public int getChildMin ()
	{ return childMin; }

    public void setChildMax (int n)
	{ childMax = n; }
    public int getChildMax ()
	{ return childMax; }

    public String getFixedValue ()
	{ return fixedValue; }
    public void setFixedValue (String fixedValue)
	{ this.fixedValue = fixedValue; }

    public String getDefaultValue ()
	{ return defaultValue; }
    public void setDefaultValue (String defaultValue)
	{ this.defaultValue = defaultValue; }

    public MessageStructureNode getParent ()
	{ return parent; }

    public int getChildCount ()
	{ return 0; } // fix me
    public MessageStructureNode getFirstChild ()
	{ return firstChild; }
    public MessageStructureNode getLastChild ()
	{ return lastChild; }

    public MessageStructureNode getNextSibling ()
	{ return nextSibling; }
    public MessageStructureNode getPreviousSibling ()
	{ return prevSibling; }

    // Remove all children.
    public void noMoreKids ()
    {
	firstChild = lastChild = null;
    }

    public void appendChild (MessageStructureNode c)
    {
	// unlink child if necessary
	MessageStructureNodeImpl child = (MessageStructureNodeImpl) c;
	MessageStructureNodeImpl p = child.parent;
	if (p != null)
	{
	    if (child == p.firstChild)
		p.firstChild = child.nextSibling;
	    if (child == p.lastChild)
		p.lastChild = child.prevSibling;
	}
	MessageStructureNodeImpl prev = child.prevSibling;
	if (prev != null)
	    prev.nextSibling = child.nextSibling;
	if (child.nextSibling != null)
	    child.nextSibling.prevSibling = prev;
	// link here
	child.parent = this;
	if (firstChild == null)
	{
	    firstChild = lastChild = child;
	    child.prevSibling = child.nextSibling = null;
	}
	else
	{
	    lastChild.nextSibling = child;
	    child.prevSibling = lastChild;
	    lastChild = child;
	}
    }

    // Return node type as displayable string.
    public static String showNodeType (int type)
    {
	StringBuffer sb = new StringBuffer();

	if ((type & FIXED) != 0)
	    sb.append(",FIXED");
	if ((type & DELIM) != 0)
	    sb.append(",DELIM");
	if ((type & SET) != 0)
	    sb.append(",SET");
	if ((type & ARRAY) != 0)
	    sb.append(",ARRAY");
	if ((type & SEQUENCE) != 0)
	    sb.append(",SEQUENCE");
	if ((type & ANY) != 0)
	    sb.append(",ANY");
	if ((type & CHOICE) != 0)
	    sb.append(",CHOICE");
	if ((type & REFERENCE) != 0)
	    sb.append(",REFERENCE");
	if ((type & CLASS) != 0)
	    sb.append(",CLASS");
	if ((type & FIELD) != 0)
	    sb.append(",FIELD");
	if ((type & LOCAL_REF) != 0)
	    sb.append(",LOCAL_REF");
	if ((type & ENUMERATE) != 0)
	    sb.append(",ENUMERATE");
	if ((type & DATA_NODE) != 0)
	    sb.append(",DATA_NODE");
	if ((type & TEMP_ROOT) != 0)
	    sb.append(",TEMP_ROOT");
	if ((type & FILE_ROOT) != 0)
	    sb.append(",FILE_ROOT");
	if ((type & MAIN_ROOT) != 0)
	    sb.append(",MAIN_ROOT");
	return (sb.length() > 0) ? sb.substring(1) : "none";
    }

    /**
     * Append new method description.
     *
     * @param method  the group to add
     */
    public void addMethod (Xsc.Method method)
    {
	methods = method.appendTo(methods);
    }

    String nodeName, javaName, nickName;
    String encoding;
    String format;
    int nodeType;
    boolean global;
    String javaType;
    boolean optional, override;
    byte access;
    int minRep, maxRep;
    String tag;
    String defaultCode;
    byte[] defaultBytes;
    String defaultString;
    int offset;
    int length, lentyp, lenoff, lensiz;
    String uid;
    String comment;
    int delimType;
    String beginDelim, endDelim;
    Xsc.Delim delim;
    String scavenger;
    boolean scavOutput, parentPrec, avoid, exact, group;
    int childMin, childMax;
    Xsc.Method methods;

    MessageStructureNodeImpl parent;
    MessageStructureNodeImpl firstChild;
    MessageStructureNodeImpl lastChild;
    MessageStructureNodeImpl nextSibling;
    MessageStructureNodeImpl prevSibling;

    String link;
    String refPath;
    String refNode;
    int refCode;
    SSC_File file;

    String fixedValue;
    String defaultValue;
}
