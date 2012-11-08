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

package com.sun.encoder.custom.runtime.provider;

import com.sun.encoder.custom.runtime.provider.Nodes.Node;
import com.sun.encoder.runtime.provider.ByteBuilder;
import com.sun.encoder.runtime.provider.Misc;

/**
 * Class presents Context containing current Node, Delimiters, ParentNode,
 * and other information for generating output.
 */
class Context {

    static final String LN = System.getProperty("line.separator");

    /**
     * node index within Nodes.mNodes[].
     */
    public int mNodeIndex;

    /**
     * mNode's child index within its parent node.
     */
    public int mChildIndex;

    /**
     * repeating index of the instance.
     */
    public int mRepeatIndex;

    /**
     * current node, and subject to update when moving from node
     * to node.
     */
    public Node mNode;

    /**
     * Parent node.
     */
    public Node mParentNode;

    /**
     * The start position of the current node, used to handle the offset
     * property of fixed length field.
     */
    public long mStartPos;

    /**
     * Delimiter Level.
     */
    public OtdDelim.DelimLevel mDelimLevel;

    /**
     * Plain delimiter.
     */
    public Delim mPlainDelim;

    /**
     * Array delimiter.
     */
    public Delim mArrayDelim;

    /**
     * True if has data content.
     */
    public boolean mHasContent;

    /**
     * Flag indicating an element start tag has just been encountered.
     * This flag is used to optimize the stack/context push and pop in
     * such a way that when an element tag starts followed by a child
     * element tag starts, then the context is pushed into stack. If an
     * element start tag is followed by another element end tag, then
     * neither push nor pop is involved. If an end tag starts after
     * another end tag, then the parent context is poped from stack.
     */
    public boolean mIsOpen;

    /**
     * for building bytes with data value from fixed-length fields(?).
     */
    public ByteBuilder mVal;

    /**
     * ID used for marking this Context for debugging purpose. This ID
     * contains name of the node where the Context is created.
     */
    String mId;

    /**
     * Saved delimiter level if there is an override by a node's
     * local DelimLevel.
     * IN=114500 Open-ESB-Issue-722
     */
    public OtdDelim.DelimLevel mPrevDelimLevel;

    /**
     * Saved plain delimiter if there is an override by a node's
     * local DelimLevel.
     * IN=114500 Open-ESB-Issue-722
     */
    public Delim mPrevPlainDelim;

    /**
     * Saved array delimiter if there is an override by a node's
     * local DelimLevel.
     * IN=114500 Open-ESB-Issue-722
     */
    public Delim mPrevArrayDelim;

    /**
     * True if there is an override by a node's local DelimLevel.
     * IN=114500 Open-ESB-Issue-722
     */
    public boolean mDelimOverridden;

    /**
     * Constructor.
     * @param nodeName the node name.
     */
    public Context(String nodeName) {
        mId = "Context@" + nodeName;
        mDelimOverridden = false;
    }

    public String getId() {
        return mId;
    }

    @Override
    public String toString() {
        StringBuilder sb = new StringBuilder(mId);
        sb.append(" nodeIndex=").append(mNodeIndex);
        sb.append(" childIndex=").append(mChildIndex);
        sb.append(" repeatIndex=").append(mRepeatIndex);
        sb.append(" startPos=").append(mStartPos);
        sb.append(" hasContent=").append(mHasContent);
        sb.append(" isOpen=").append(mIsOpen);
        sb.append(" delimOverridden=").append(mDelimOverridden);
        sb.append(LN).append(" node=[");
        sb.append(mNode != null ? mNode : "null").append("]");
        sb.append(LN).append(" parentNode=[");
        sb.append(mParentNode != null ? mParentNode : "null").append("]");
        sb.append(LN).append(" delimLevel=[");
        sb.append(mDelimLevel != null ? mDelimLevel : "null").append("]");
        sb.append(LN).append(" plainDelim=[");
        sb.append(mPlainDelim != null ? mPlainDelim : "null").append("]");
        sb.append(LN).append(" arrayDelim=[");
        sb.append(mArrayDelim != null ? mArrayDelim : "null").append("]");
        if (mDelimOverridden) {
            sb.append(LN).append(" prevDelimLevel=[");
            sb.append(mPrevDelimLevel != null ? mPrevDelimLevel : "null").append("]");
            sb.append(LN).append(" prevPlainDelim=[");
            sb.append(mPrevPlainDelim != null ? mPrevPlainDelim : "null").append("]");
            sb.append(LN).append(" prevArrayDelim=[");
            sb.append(mPrevArrayDelim != null ? mPrevArrayDelim : "null").append("]");
        }
        sb.append(LN);
        if (mVal != null) {
            sb.append(" valueSize=").append(mVal.size());
            sb.append(" valueBytes=\"").append(
                Misc.showFragment(mVal.getBytes(), 0, mVal.size(), true));
            sb.append("\"");
        }
        return sb.toString();
    }
}
