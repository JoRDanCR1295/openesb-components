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
 * @(#)MessageStructureNode.java 
 *
 * Copyright 2004-2008 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.stc.jcs.ssc;

import com.sun.stc.jcs.ssc.Xsc;

/**
 * Implementation of an XSC node representation.
 * XSC is the XML-based metadata representation of an event description,
 * used in e*Gate 4.5.X.
 */
public interface MessageStructureNode
{
    // Values for the "structure" attribute.
    final int FIXED  = 1<<0;
    final int DELIM  = 1<<1;
    final int SET    = 1<<2;
    final int ARRAY  = 1<<3;
    //
    final int S_MASK = 0x000F;

    // Values for the "order" attribute.
    final int SEQUENCE = 1<<4;
    final int ANY      = 1<<5;
    final int CHOICE   = 1<<6;
    //
    final int O_MASK = 0x0070;

    // Values for the "type" attribute.
    final int REFERENCE = 1<<7;
    final int CLASS     = 1<<8;
    final int FIELD     = 1<<9;
    final int LOCAL_REF = 1<<10;
    final int ENUMERATE = 1<<11;
    //
    final int T_MASK = 0x0F80;

    // The kind of node position.
    final int DATA_NODE = 1<<12; // node contains data (not <method> etc.)
    final int TEMP_ROOT = 1<<13; // root of a local template
    final int FILE_ROOT = 1<<14; // root of an XSC file (see <etd>'s "name")
    final int MAIN_ROOT = 1<<15; // root of main file (has send() etc.)
    //
    final int P_MASK = 0xF000;

    final int SOT_MASK = (S_MASK | O_MASK | T_MASK);

    // Original SSC node types.
    final int ANAp = (ANY|ARRAY|CLASS);
    final int ASp  = (ANY|SET|CLASS);
    final int AFp  = (ANY|FIXED|CLASS);
    final int ANp  = (ANY|DELIM|CLASS);
    final int ONAp = (SEQUENCE|ARRAY|CLASS);
    final int OSp  = (SEQUENCE|SET|CLASS);
    final int OFp  = (SEQUENCE|FIXED|CLASS);
    final int ONp  = (SEQUENCE|DELIM|CLASS);
    final int ANAs = (ANY|ARRAY|FIELD);
    final int ASs  = (ANY|SET|FIELD);
    final int AFs  = (ANY|FIXED|FIELD);
    final int ANs  = (ANY|DELIM|FIELD);
    final int ONAs = (SEQUENCE|ARRAY|FIELD);
    final int OSs  = (SEQUENCE|SET|FIELD);
    final int OFs  = (SEQUENCE|FIXED|FIELD);
    final int ONs  = (SEQUENCE|DELIM|FIELD);
    final int GTN = (REFERENCE|DELIM);
    final int GTS = (REFERENCE|SET);
    final int GTF = (REFERENCE|FIXED);
    final int LTN = (LOCAL_REF|DELIM);
    final int LTS = (LOCAL_REF|SET);
    final int LTF = (LOCAL_REF|FIXED);

    // Special values for numeric fields.
    public final int UNDEFINED = -1;
    public final int UNBOUNDED = -2;
    public final int HAS_VALUE = -3;
    public final int DECIMAL   = -4;

    // Access values.
    public final byte A_NONE   = 0;
    public final byte A_MODIFY = 1;
    public final byte A_READ   = 2;
    public final byte A_WRITE  = 3;

    /**
     * Tests whether node is a leaf node.
     *
     * @return the test result
     */
    public boolean isLeaf ();

    /**
     * Tests whether node is an array node.
     *
     * @return the test result
     */
    public boolean isArray ();

    /**
     * Tests whether node is a template reference.
     *
     * @return the test result
     */
    public boolean isReference ();

    public SSC_File getFile();

    public String getNodeName();
    public String getJavaName();
    public String getNickName();

    public String getLink();
    public String getRefPath();
    public String getRefNode(); // "member" of reference
    public int getRefCode(); // "codeVersion" of reference

    public int getNodeType();
    public boolean getGlobal();
    public String getJavaType();

    public byte getAccess();
    public boolean isReadable();
    public boolean isWritable();
    public boolean getOptional();
    public boolean getOverride();
    public int getMinRep();
    public int getMaxRep();

    public String getEncoding();
    public String getFormat();

    public String getTag();

    public String getDefaultCode();
    public byte[] getDefaultBytes();
    public String getDefaultString();

    public int getOffset();
    public int getLength();
    public int getLenTyp();
    public int getLenOff();
    public int getLenSiz();
    public String getUid();
    public String getComment();

    public int getDelimType();
    public Xsc.Delim getDelim();
    public Xsc.Method getMethods();

    public String getScavenger(); // Sc/ScN modifiers
    public boolean getScavOutput(); // Sc or ScN modifier?
    public boolean getParentPrec(); // Pp modifier
    public boolean getAvoid(); // Nt modifier
    public boolean getExact(); // Ex modifier
    public boolean getGroup(); // Gr modifier
    public int getChildMin(); // NofN modifier, 1st arg (-1=undef)
    public int getChildMax(); // NofN modifier, 2nd arg (-1=undef, -2=infin)

    // For XML-based ETDs.
    public String getFixedValue();
    public String getDefaultValue();

    public int getChildCount();
    public MessageStructureNode getParent();
    public MessageStructureNode getFirstChild();
    public MessageStructureNode getLastChild();

    public MessageStructureNode getNextSibling();
    public MessageStructureNode getPreviousSibling();

    public void appendChild(MessageStructureNode child);
    public void addMethod(Xsc.Method method);
}
