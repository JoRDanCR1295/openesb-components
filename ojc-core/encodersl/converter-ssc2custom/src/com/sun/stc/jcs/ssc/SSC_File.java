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
 * @(#)SSC_File.java 
 *
 * Copyright 2004-2008 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.stc.jcs.ssc;

import com.sun.stc.jcs.ssc.Xsc;

import java.util.*;

/**
 * Class to represent an SSC-based XSC file and associated information.
 * Part of the e*Gate Java collaboration service support tools.
 */
public class SSC_File
{
    private int uidMax = 0; // max. integer "uid" value so far
    private String sscFileName; // SSC filename, relative to SSC root
    private String xscFileName; // XSC filename, relative to XSC root
    private Xsc.Delim delimiters = null;
    private boolean derived = false, editable = false,
	root = false;
    private int codeVersion = 0;
    private Map localTemplateMap = new Hashtable ();
    private MessageStructureNode etd;
    private String comment = null, datacode = null, encoding = null,
	jarFile = null, name = null, source = null, type = null, uid = null;
    private String delimitersComment = null, delimitersUid = null;
    private Xsc.JavaProps jprops = null;
    public boolean generate = false;

    public String getType ()
	{ return type; }

    public void setType (String type)
	{ this.type = type; }

    /**
     * Constructs from XSC filename.
     *
     * @param xscFileName  the XSC filename, relative to the XSC root
     */
    public SSC_File (String xscFileName)
    {
	this.xscFileName = xscFileName;
    }

    /**
     * Gets original SSC filename. If unknown, derives it from XSC name.
     *
     * @return the SSC file path, normalized to the SSC root
     */
    public String getSscFileName ()
    {
	if (sscFileName != null)
	    return sscFileName;
	String ssc = xscFileName;
	if (ssc != null)
	{
	    if (ssc.endsWith(".xsc"))
		ssc = ssc.substring(0, ssc.length() - 4);
	    if (! ssc.endsWith(".ssc"))
		ssc = ssc + ".ssc";
	}
	return ssc;
    }

    public String getXscFileName ()
	{ return xscFileName; }

    public void setSscFileName (String sscFileName)
	{ this.sscFileName = sscFileName; }

    public void setXscFileName (String xscFileName)
	{ this.xscFileName = xscFileName; }

    public boolean isRoot ()
	{ return root; }

    public void setIsRoot (boolean value)
	{ root = value; }

    public void setPackageName (String packageName)
    {
	if (packageName != null && jprops != null)
	    jprops.setPackage(packageName.replace('\\', '/'));
    }

    public String getGlobalName ()
	{ return this.name; }

    public void setGlobalName (String name)
	{ this.name = name; }

    public void setCodeVersion (int version)
	{ codeVersion = version; }

    public int getCodeVersion ()
	{ return codeVersion; }

    public void setDelimiters (Xsc.Delim delims)
	{ delimiters = delims; }

    public Xsc.Delim getDelimiters ()
	{ return delimiters; }

    public MessageStructureNode resolveLocalTemplate (String name)
	{ return (MessageStructureNode)localTemplateMap.get(name); }

    public void defineLocalTemplate (String name, MessageStructureNode tem)
	{ localTemplateMap.put(name, tem); }

    public void clearLocalTemplates ()
	{ localTemplateMap.clear(); }

    public String[] localTemplateNames ()
    {
	Enumeration e = ((Hashtable)localTemplateMap).keys();
	String[] names = new String[localTemplateMap.size()];
	for (int i = 0; e.hasMoreElements(); i++)
	    names[i] = (String)e.nextElement();
	return names;
    }

    public String getEncoding ()
	{ return encoding; }

    public void setEncoding (String encoding)
	{ this.encoding = encoding; }

    public String getDataCode ()
	{ return datacode; }

    public void setDataCode (String datacode)
	{ this.datacode = datacode; }

    public boolean getDerived ()
	{ return derived; }

    public void setDerived (boolean value)
	{ this.derived = value; }

    public boolean getEditable ()
	{ return editable; }

    public void setEditable (boolean value)
	{ this.editable = value; }

    public Xsc.JavaProps getJavaProps ()
	{ return jprops; }

    public void setJavaProps (Xsc.JavaProps value)
	{ this.jprops = value; }

    public String getEtdComment ()
	{ return comment; }

    public void setEtdComment (String value)
	{ this.comment = value; }

    public String getEtdUid ()
	{ return uid ; }

    public void setEtdUid (String value)
	{ this.uid = value; }

    public String getDelimsComment ()
	{ return delimitersComment ; }

    public void setDelimsComment (String value)
	{ this.delimitersComment = value; }

    public String getDelimsUid ()
	{ return delimitersUid ; }

    public void setDelimsUid (String value)
	{ this.delimitersUid = value; }

    /**
     * Gets the root node of the ETD.  This is the top-level node named
     * by the "name" attribute of the &lt;etd&gt; entity in the XSC file.
     *
     * @return the root node reference
     */
    public MessageStructureNode getEtd ()
	{ return etd; }

    public void setEtd (MessageStructureNode etd)
	{ this.etd = etd; }

    /**
     * Generates next purely numeric UID value.
     *
     * @return numeric UID string
     */
    public String makeUid ()
	{ return Integer.toString(++ uidMax); }

    /**
     * Updates uidMax if this UID is purely numeric.
     *
     * @param uid  UID string, numeric or not
     */
    public void seenUid (String uid)
    {
	if (uid == null) { return; }
	try { uidMax = java.lang.Math.max(uidMax, Integer.parseInt(uid)); }
	catch (NumberFormatException nfe) { }
    }

    /* The part below corresponds to information stored in <javaProps>
     * below <etd>, and should really be represented by a separate
     * Xsc.JavaProps object.
     */

    public String getClassName ()
	{ return jprops == null ? null : jprops.getBase(); }

    public String getPackageName ()
	{ return jprops == null ? null : jprops.getPackage(); }

    public String getJarFile ()
	{ return jprops == null ? null : jprops.getJarFile(); }

    public boolean getCodeAvail ()
	{ return jprops == null ? false : jprops.getCode(); }

    public String getSource ()
	{ return jprops == null ? null : jprops.getSource(); }

    public String getJpComment ()
	{ return jprops == null ? null : jprops.getComment(); }

    public String getJpUid ()
	{ return jprops == null ? null : jprops.getUid(); }
}
