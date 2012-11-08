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
 * @(#)Nodes.java
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 *
 * END_HEADER - DO NOT EDIT
 */
package com.sun.encoder.custom.runtime.provider;

import java.io.IOException;

import com.sun.encoder.custom.runtime.provider.OtdDelim.DelimLevel;
import com.sun.encoder.custom.runtime.provider.OtdDelim.MutableDelimLevel;
import com.sun.encoder.runtime.CoderFactory;
import com.sun.encoder.runtime.StringCoder;
import com.sun.encoder.runtime.TransCoder;
import com.sun.encoder.runtime.provider.Misc;

import java.net.URL;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.regex.Pattern;
import org.apache.xmlbeans.SchemaType;

/**
 * Class to hold the complete set of node descriptors for a single BUD OTD.
 *
 * The BUD code generator creates code in the shared root class of all tops
 * for an OTD, to have a single static instance of Nodes, with a class
 * initialization that fills this instance once.  The resulting descriptor
 * in core is then used by Parse to unmarshal incoming data into the run-time
 * Java tree.
 *
 * @author Michael Libourel, Jun Xu
 */
public final class Nodes {

    public static final String DEFAULT_ANTECODING = "iso-8859-1";
    public static final String DEFAULT_DECODING = "iso-8859-1";
    public static final String DEFAULT_ENCODING = "iso-8859-1";
    public static final String DEFAULT_POSTCODING = "iso-8859-1";
    public static final String DEFAULT_PREDECODECHARCODING = "iso-8859-1";
    public static final String DEFAULT_POSTENCODECHARCODING = "iso-8859-1";
    /**
     * The set of nodes in the OTD, including referenced templates.
     */
    final Node[] mNodes;
    /**
     * The location from where the metadata (nodes) is loaded.
     */
    private URL mMetadataLocation;
    /**
     * The namespace of the metadata.
     */
    private String mMetadataNamespace;
    /**
     * The delimiter definition in the OTD.
     */
    public final OtdDelim mOtdDelim;
    /**
     * Bytes input trans-coder (antecoder).  This coder is used to convert
     * bytes coded in one coding into bytes coded in another coding before
     * unmarshaling (decoding)
     */
    private TransCoder mAntecoder =
        CoderFactory.getTrans(DEFAULT_ANTECODING, DEFAULT_DECODING);
    /**
     * Coding of the bytes input.  Basically this coding tells how to
     * convert bytes input into a string.
     */
    private String mAnteCoding = DEFAULT_ANTECODING;
    /**
     * Parsing coder.  Used to decode byte[] into string during parsing.
     */
    private StringCoder mDecoder = CoderFactory.getCoder(DEFAULT_DECODING);
    /**
     * Serializing coder.  Used to encode string to byte[] during serializing.
     */
    private StringCoder mEncoder = CoderFactory.getCoder(DEFAULT_ENCODING);
    /**
     * Bytes output trans-coder (postcoder).  This coder is used to convert
     * bytes coded in one coding to bytes coded in another coding after
     * marshaling.
     */
    private TransCoder mPostcoder =
        CoderFactory.getTrans(DEFAULT_ENCODING, DEFAULT_POSTCODING);
    /**
     * Coding of a string output.  Basically this coding tells how to
     * convert string output into bytes
     */
    private String mPostCoding = DEFAULT_POSTCODING;
    /**
     * Pre-decoding coder, used to encode string into bytes before decoding.
     * Usually this coder should be same as mDecoder.
     */
    private StringCoder mPreDecodeCharCoder;
    /**
     * Post-encoding coder, used to decode bytes into string after encoding.
     * Usually this coder should be same as mEncoder.
     */
    private StringCoder mPostEncodeCharCoder;
    /**
     * If undefined trailing fields are allowed and/or should be mapped.
     * @see Node.UndefDataPolicy
     */
    private Node.UndefDataPolicy mUndefDataPolicy;
    /**
     * If the delimiter inheritance is per delimiter or per delimiter level.
     */
    private boolean mFineGrainedInherit;
    /**
     * Index (within mNodes) of the root node
     */
    private int mRootNodeIndex;
    /**
     * Flag indicating if verify() is invoked yet.
     */
    private boolean mVerified = false;

    static final String LN = System.getProperty("line.separator");

    /**
     * Used to indicate if has corresponding scavenger byte.
     */
    public static final byte OFF = (byte) 0;
    public static final byte ON = (byte) 1;

    /**
     * Constructs for a given number of nodes.
     * The actual nodes descriptors are added later, see add().
     *
     * @param nodeCount  the number of nodes
     * @param delim  the delimiter descriptors
     */
    Nodes(final int nodeCount, final OtdDelim delim) {
        mNodes = new Node[nodeCount];
        mOtdDelim = delim;
    }

    /**
     * Retrieves the associated constant OTD delimiter information.
     *
     * @return the constant delimiter info
     */
    public OtdDelim getDelim() {
        return mOtdDelim;
    }

    /**
     * Gets the actual descriptor for a node.
     *
     * @param index  the Node.mNodes[] index
     * @return the descriptor
     */
    public Node get(final int index) {
        if (index < 0 || mNodes.length <= index) {
            throw new IllegalArgumentException("Out of range node index="
                + index);
        }
        if (mNodes[index] == null) {
            throw new RuntimeException("Missing node descriptor #" + index);
        }
        return mNodes[index];
    }

    /**
     * Adds the actual descriptor for a node.
     *
     * @param index  the Node.mNodes[] index
     * @param from  the parent node's Fog ID, for tracing
     * @param to  the child node's Fog ID, for tracing
     * @param type  the node type, from Node.Type
     * @param namespace  the namespace from XSD (since 6.0)
     * @param name  display name; NYI: use OtdMeta link instead
     * @param child  field number below parent
     * @param order  the child ordering on input, from Node.Order
     * @param length  for limited fixed fields, else use 0
     * @param align  the match rule algorithm, from Node.Align
     * @param match  exact input match sequence, null if none
     * @param subnodes  the sub-nodes, as indices in Node.mNodes[], null if none
     * @param level  local delimiter list, or null if none
     * @param offset offset of fixed length field
     * @param position offset from the current position to the position
     *                  where actual length can be found
     * @param defaultValue default value of non-optional field
     * @param scavengerChars scavenger characters
     * @param output1stChar flag indicates if first character of the
     *              scavenger characters should be marshaled out into
     *              output.
     * @param minNOfChildren minimum number of children must have data
     * @param maxNOfChildren maximum number of children must have data
     * @param minOccurs minimum number of occurrences
     * @param maxOccurs maximum number of occurrences, -1 if no limit
     * @param isNoMatch flag indicates if the match condition should be
     *              reverted
     * @param delimForFixedLen
     * @param elmType element schema type.
     */
    public void addNode(int index, int from, int to, Node.Type type,
        String namespace, String name, int child, Node.Order order, int length,
        Node.Align align, byte[] match, List<Integer> subnodes, int[][] level,
        int offset, int position, String defaultValue, String scavengerChars,
        boolean output1stChar, int minNOfChildren, int maxNOfChildren,
        int minOccurs, int maxOccurs, boolean isNoMatch,
        DelimForFixedLen delimForFixedLen, int elmType) {
        if (index < 0 || mNodes.length <= index) {
            throw new IndexOutOfBoundsException("Out of range node index="
                + index);
        }
        if (mNodes[index] != null) {
            throw new RuntimeException("duplicate node descriptor #" + index);
        }
        if (type != Node.Type.FIXED && length != 0) {
            throw new IllegalArgumentException("non-fixed node needs length=0");
        }
        // convert type: List<Integer> into primitive array type: int[]
        int[] iSubnodes = null;
        if (subnodes != null && subnodes.size() > 0) {
            iSubnodes = new int[subnodes.size()];
            for (int i = 0; i < subnodes.size(); i++) {
                iSubnodes[i] = subnodes.get(i);
            }
        }
        Node node = new Node(from, to, type, namespace, name, child,
            iSubnodes, order, length, align, match, mOtdDelim.build(level),
            offset, position, defaultValue, scavengerChars, output1stChar,
            minNOfChildren, maxNOfChildren, minOccurs, maxOccurs, isNoMatch,
            delimForFixedLen, elmType);
        mNodes[index] = node;
    }

    /**
     * Sets the metadata location.
     * @param location url location.
     */
    public void setMetadataLocation(URL location) {
        mMetadataLocation = location;
    }

    /**
     * Gets the metadata location.
     * @return url location of the metadata.
     */
    public URL getMetadataLocation() {
        return mMetadataLocation;
    }

    /**
     * Sets the metadata namespace.
     * @param namespace the namespace to be set.
     */
    public void setMetadataNamespace(String namespace) {
        mMetadataNamespace = namespace;
    }

    /**
     * Gets the metadata namespace.
     * @return the metadata namespace.
     */
    public String getMetadataNamespace() {
        return mMetadataNamespace;
    }

    @Override
    public String toString() {
        StringBuffer buf = new StringBuffer("Nodes@")
            .append(Integer.toHexString(hashCode()));
        for (int i = 0; i < mNodes.length; i++) {
            buf.append(" Node#").append(i).append("=[")
                .append(mNodes[i]).append("]").append(LN);
        }
        return buf.toString();
    }

    /**
     * Verify the consistency of the child references.
     * This is for internal debugging.
     * It should only be called once per instance.
     */
    public void verify() {
        if (mVerified) {
            throw new RuntimeException("verify() can only be called once.");
        }
        boolean[] seen = new boolean[mNodes.length];
        int[] sub;
        for (int i = 0; i < mNodes.length; i++) {
            seen[i] = false;
        }
        for (int i = 0; i < mNodes.length; i++) {
            Node node = mNodes[i];
            if (node == null) {
                throw new RuntimeException("node #" + i + " was not set");
            }
            if ((sub = node.mSubnodes) != null) {
                for (int j = 0; j < sub.length; j++) {
                    int child = sub[j];
                    if (child < 0 || mNodes.length <= child) {
                        throw new RuntimeException("node #" + i + ", ref #"
                            + j + " is invalid: " + child);
                    }
                    if (seen[child]) {
                        throw new RuntimeException("node #" + i + ", ref #"
                            + j + " is duplicate parent of " + child);
                    }
                    if (mNodes[child].mChildIndex != j) {
                        throw new RuntimeException("node #" + i + " says node["
                            + child + "].mChild should be " + j + ", not " + mNodes[child].mChildIndex);
                    }
                    seen[child] = true;
                }
            }
        }
        mVerified = true;
    }

    public TransCoder getAntecoder() {
        return mAntecoder;
    }

    public void setAntecoder(TransCoder antecoder) {
        mAntecoder = antecoder;
    }

    public StringCoder getDecoder() {
        return mDecoder;
    }

    public void setDecoder(StringCoder decoder) {
        mDecoder = decoder;
    }

    public StringCoder getEncoder() {
        return mEncoder;
    }

    public void setEncoder(StringCoder encoder) {
        mEncoder = encoder;
    }

    /**
     * Gets the bytes output trans-coder (postcoder).  This coder is used to
     * convert bytes coded in one coding to bytes coded in another coding after
     * marshaling.
     * @return the bytes output trans-coder (postcoder).
     */
    public TransCoder getPostcoder() {
        return mPostcoder;
    }

    /**
     * Sets the bytes output trans-coder (postcoder).
     * @param postcoder the bytes output trans-coder.
     */
    public void setPostcoder(TransCoder postcoder) {
        mPostcoder = postcoder;
    }

    public StringCoder getPreDecodeCharCoder() {
        if (mPreDecodeCharCoder != null) {
            return mPreDecodeCharCoder;
        }
        return mDecoder;
    }

    public void setPreDecodeCharCoder(StringCoder coder) {
        mPreDecodeCharCoder = coder;
    }

    public StringCoder getPostEncodeCharCoder() {
        if (mPostEncodeCharCoder != null) {
            return mPostEncodeCharCoder;
        }
        return mEncoder;
    }

    public void setPostEncodeCharCoder(StringCoder coder) {
        mPostEncodeCharCoder = coder;
    }

    public String getAnteCoding() {
        return mAnteCoding;
    }

    public void setAnteCoding(String coding) {
        mAnteCoding = coding;
    }

    public String getPostCoding() {
        return mPostCoding;
    }

    public void setPostCoding(String coding) {
        mPostCoding = coding;
    }

    public int getRootNodeIndex() {
        return mRootNodeIndex;
    }

    public void setRootNodeIndex(int rootNodeIndex) {
        mRootNodeIndex = rootNodeIndex;
    }

    public Node.UndefDataPolicy getUndefDataPolicy() {
        return mUndefDataPolicy;
    }

    public void setUndefDataPolicy(Node.UndefDataPolicy undefDataPolicy) {
        mUndefDataPolicy = undefDataPolicy;
    }

    public boolean isFineGrainedInherit() {
        return mFineGrainedInherit;
    }

    public void setFineGrainedInherit(boolean fineGrainedInherit) {
        mFineGrainedInherit = fineGrainedInherit;
    }

    /**
     * Builds up fine grained delimiter inheritance for the root node.
     */
    public void buildFineGrainedInherit() {
        DelimLevel inheritLevel = null;
        Node rootNode = mNodes[mRootNodeIndex];
        Set<Node> visitedNodes = new HashSet<Node>();
        buildFineGrainedInherit(inheritLevel, rootNode, visitedNodes);
    }

    /**
     * Builds up fine grained delimiter inheritance for nodes that has
     * both inherited delimiter set from ancestor node(s) and its own defined
     * delimiter set.
     *
     * @param inheritLevel delimiter level to be inherited from, null if
     * root node.
     * @param node the node whose delimiter levels to be checked
     * @param visitedNodes the nodes that have been visited so far
     */
    public void buildFineGrainedInherit(DelimLevel inheritLevel, Node node,
        Set<Node> visitedNodes) {
        if (visitedNodes.contains(node)) {
            // already built fine-grained inheritance for node
            return;
        }
        visitedNodes.add(node);
        DelimLevel nodeLevel = node.mDelimLevel;
        MutableDelimLevel lastMutLevel = null;
        if (inheritLevel != null && nodeLevel != null) {
            // has both ancestor's delimLevel and its own delimLevel, so we need
            // to merge delimiter information on all levels involved based on fine-grained inheritance.
            MutableDelimLevel tmpLevel = null;
            while (inheritLevel != null || nodeLevel != null) {
                // loop thru potential multiple delimiter levels (because
                // each delimiter set can have one or more delimiter levels)
                // till both inheritLevel and nodelevel are null.
                if (inheritLevel != null && nodeLevel != null) {
                    tmpLevel = nodeLevel.cloneMutable();
                    tmpLevel.inheritFrom(inheritLevel);
                } else if (inheritLevel != null) {
                    tmpLevel = inheritLevel.cloneMutable();
                } else {
                    // i.e. level != null && inheritLevel == null
                    tmpLevel = nodeLevel.cloneMutable();
                }
                if (inheritLevel != null) {
                    // move on to inherited next delimiter level
                    inheritLevel = inheritLevel.mNextDelimLevel;
                }
                if (nodeLevel != null) {
                    // move on to node's next delimiter level
                    nodeLevel = nodeLevel.mNextDelimLevel;
                }
                // tmpLevel becomes a linked ladder with first delimLevel
                // becoming last
                tmpLevel.mNext = lastMutLevel;
                lastMutLevel = tmpLevel;
            } // end-- while

            // now the core part: build node's mDelimLevel based on
            // the information-merged tmpLevel
            node.mDelimLevel = mOtdDelim.buildDelimLevel(tmpLevel);
        } // end-- if (inheritLevel != null && level != null)

        int[] subnodes = node.mSubnodes;
        if (subnodes == null || subnodes.length == 0) {
            // current node is a leaf field node, done.
            return;
        }
        if (node.isDelim() || node.isArray()) {
            // if delimited or array node, then it takes one delimiter level off
            if (node.mDelimLevel != null) {
                nodeLevel = node.mDelimLevel.mNextDelimLevel;
            } else if (inheritLevel != null) {
                nodeLevel = inheritLevel.mNextDelimLevel;
            } else {
                nodeLevel = null;
            }
        } else {
            if (node.mDelimLevel != null) {
                // pass this DelimLevel onto its child nodes in recursive call
                nodeLevel = node.mDelimLevel;
            } else if (inheritLevel != null) {
                nodeLevel = inheritLevel;
            } else {
                nodeLevel = null;
            }
        }
        for (int i = 0; i < subnodes.length; i++) {
            // recursive call into child node with next delimLevel from node.
            buildFineGrainedInherit(nodeLevel, mNodes[subnodes[i]],
                visitedNodes);
        }
    }

    /**
     * Immutable descriptor for a single node in the BUD (Basic User-Defined)
     * OTD.
     */
    public static class Node {

        /**
         * The node type of a BUD (Basic User-Defined) node.
         * Note: UNDEF and EXREF don't really belong in the runtime
         * interface, they are only used in the BUD test-builder.
         */
        public enum Type {

            /** Choice between alternatives. */
            ALTER,
            /** Delimited and use array separator. */
            ARRAY,
            /** Delimited. */
            DELIM,
            /** Fixed length. */
            FIXED,
            /** Grouped sequence. */
            GROUP,
            /** Transient field (no serialized form). */
            TRANS,
            /** Only used internally for forward references. */
            UNDEF,
            /** Only used internally for external references. */
            EXREF
        }

        /**
         * The child order of a parent node.
         */
        public enum Order {

            /**
             * Child nodes must appear in the sequence given in the metadata.
             */
            SEQ,
            /**
             * Child nodes can appear in any order.
             */
            MIX,
            /**
             * Child nodes must remain grouped, but the groups can appear
             * in any order.
             */
            ANY
        }

        /**
         * The interpretation/alignment mode for a match pattern.
         */
        public enum Align {

            /**
             * Always performs a match without looking at data
             * (just pass-through). Any value set for the match is ignored.
             */
            BLIND,
            /**
             * Exactly matches the byte pattern.
             */
            EXACT,
            /**
             * Fully matches beginning portion of data bytes.
             */
            BEGIN,
            /**
             * Fully matches final portion of data bytes.
             */
            FINAL,
            /**
             * Fully matches slice of data bytes.
             */
            INTER,
            /**
             * Data bytes must be slice of match pattern.
             */
            SUPER,
            /**
             * Data is one of the prefix-delim'ed list of legal data
             * specified in match.
             */
            ONEOF,
            /**
             * Data must match a regular expression specified in match.
             */
            REGEX
        }

        /**
         * Policy for handling undefined data encountered during parsing.
         */
        public enum UndefDataPolicy {

            /**
             * Undefined (trailing) data is skipped silently.
             */
            SKIP,
            /**
             * Undefined (trailing) data is allowed and will be mapped to local
             * field named '_undefined_' with the predefined namespace of
             * "urn:com.sun:encoder:instance".
             * @see Match#UNDEF_NODE_NAME
             * @see Match#UNDEF_NS
             */
            MAP,
            /**
             * Undefined data is not allowed and exception will be thrown if
             * it does appear.
             */
            PROHIBIT
        }

        public final int mFromId;
        public final int mNodeId;
        /** namespace of the node. */
        public final String mNamespace;
        /** NYI: should be link to OtdMeta instead. */
        public final String mName;
        /** from Node.Type */
        public final Node.Type mType;
        /** delim list, linked by mNextDelimLevel. */
        public OtdDelim.DelimLevel mDelimLevel;
        /** field number below parent. */
        public final int mChildIndex;
        /** list of sub-nodes, null if none. */
        public final int[] mSubnodes;
        /**
         * @see Node.Order
         */
        public final Node.Order mOrder;
        /** for fixed field; <= 0 means no limit. */
        public final int mLength;
        /** @see Node.Align */
        public final Node.Align mAlign;
        /** Pre-compiled pattern if the alignment. */
        public final Pattern mPattern;
        /** regex match pattern. */
        public final byte[] mMatch;
        public final boolean mIsNoMatch;
        /** the offset  in terms of bytes from the zero position where the
         * first sibling starts. */
        public final int mOffset;
        /** the offset in terms of bytes (as a required long integer) between
         * the current parsing position and the position where the encoded field
         * length can be found. */
        public final int mPosition;
        public final String mDefaultValue;
        /** scavenger characters to skip during delimiter searching. */
        public final String mScavengerChars;
        /** Scavenger bytes corresponding to scavenger characters. */
        public final byte[] mScavengerBytes;
        /**
         * a size of 256 byte array with the slot(s) of index corresponding to
         * the int value of each byte in mScavengerBytes filled with the value
         * of ON, and the rest with OFF.
         * @see #ON @see #OFF
         */
        public final byte[] mScavengerIndex;
        /**
         * whether or not to output the first scavenger character before
         * outputting the field (even before the begin delimiter if there is
         * any) */
        public final boolean mOutput1stChar;
        /** the minimum number of children (different kinds) that must have data. */
        public final int mMinNOfChildren;
        /** the maximum number of children (different kinds) that must have data. */
        public final int mMaxNOfChildren;
        public final int mMinOccurs;
        public final int mMaxOccurs;
        public final boolean mIsOptional;
        public final boolean mIsRepeatable;
        public final DelimForFixedLen mDelimForFixedLen;
        public final int mElmType;

        /**
         * Creates descriptor from given attributes.
         *
         * @param from  the parent node's Fog ID, for tracing (obsolete)
         * @param to  the child node's Fog ID, for tracing (obsolete)
         * @param type  the node type, from Node.Type
         * @param namespace  the namespace from XSD (since 6.0)
         * @param name  the local name from an element declaration in XSD
         * @param childIndex  field number below parent, -1 if none
         * @param subnodes  the sub-nodes, as indices in Node.mNodes[],
         * null if none
         * @param order  the child ordering on input, from Node.Order
         * @param length  for limited fixed fields, else use 0
         * @param align  the match rule algorithm, from Node.Align
         * @param match  exact input match sequence, null if none
         * @param level  local delimiter list, or null if none
         * @param offset offset of fixed length field relative to the start
         *                      position of parent node
         * @param position offset from the current position to the position
         *                  where actual length can be found
         * @param defaultValue default value of non-optional field
         * @param scavengerChars scavenger characters
         * @param output1stChar flag indicates if first character of the
         *              scavenger characters should be marshaled out into
         *              output.
         * @param minNOfChildren minimum number of children must have data
         * @param maxNOfChildren maximum number of children must have data
         * @param minOccurs minimum number of occurrences
         * @param maxOccurs maximum number of occurrences, -1 if no limit
         * @param isNoMatch flag indicates if the match condition should be
         *              reverted
         * @param delimForFixedLen simple delimiter used in fixed length fields
         *                to skip some bytes before getting the value.
         * @param elmType element schema type.
         */
        public Node(int from, int to, Node.Type type, String namespace,
            String name, int childIndex, int[] subnodes, Node.Order order,
            int length, Node.Align align, byte[] match,
            OtdDelim.DelimLevel level, int offset, int position,
            String defaultValue, String scavengerChars, boolean output1stChar,
            int minNOfChildren, int maxNOfChildren, int minOccurs,
            int maxOccurs, boolean isNoMatch,
            DelimForFixedLen delimForFixedLen, int elmType) {
            if (subnodes != null && match != null) {
                throw new IllegalArgumentException("Only a leaf node can have match."); //I18N
            }
            if (align == Node.Align.EXACT &&
                match != null && type == Type.FIXED && length > 0 &&
                match.length > length) {
                throw new IllegalArgumentException("Match length="
                    + match.length + " exceeds fixed field length=" + length + "."); //I18N
            }
            mFromId = from;
            mNodeId = to;
            mNamespace = namespace;
            mName = name;
            mType = type;
            mChildIndex = childIndex;
            mSubnodes = subnodes;
            mOrder = order;
            mLength = length;
            mAlign = align;
            mMatch = match;
            if (mAlign == Node.Align.REGEX && mMatch != null
                && mMatch.length > 0) {
                mPattern = Pattern.compile(Misc.bytes2str(mMatch));
            } else {
                mPattern = null;
            }
            mDelimLevel = level;
            mOffset = offset;
            mPosition = position;
            mDefaultValue = defaultValue;
            mScavengerChars = scavengerChars;
            if (mScavengerChars != null && mScavengerChars.length() > 0) {
                mScavengerBytes =
                    Misc.str2bytes(Misc.nonPrintable(mScavengerChars));
                mScavengerIndex = new byte[256];
                Arrays.fill(mScavengerIndex, OFF);
                for (int i = 0; i < mScavengerBytes.length; i++) {
                    mScavengerIndex[mScavengerBytes[i]] = ON;
                }
            } else {
                mScavengerBytes = null;
                mScavengerIndex = null;
            }
            mOutput1stChar = output1stChar;
            mMinNOfChildren = minNOfChildren;
            mMaxNOfChildren = maxNOfChildren;
            mMinOccurs = minOccurs;
            mMaxOccurs = maxOccurs;
            mIsOptional = (mMinOccurs == 0);
            mIsRepeatable = (mMaxOccurs == -1 || mMaxOccurs > 1);
            mIsNoMatch = isNoMatch;
            mDelimForFixedLen = delimForFixedLen;
            mElmType = elmType;
        }

        /**
         * For debug purpose.
         * @return string representation of this Node object.
         */
        @Override
        public String toString() {
            StringBuffer sb = new StringBuffer("Node@")
                .append(Integer.toHexString(hashCode()));
            sb.append(" name=").append(mName);
            if (mChildIndex >= 0) {
                sb.append(" childIndex=").append(mChildIndex);
            }
            if (mSubnodes != null && mSubnodes.length > 0) {
                sb.append(" subnodes={");
                for (int i = 0; i < mSubnodes.length; i++) {
                    sb.append(mSubnodes[i]);
                    if (i < mSubnodes.length - 1) {
                        sb.append(",");
                    }
                }
                sb.append("}");
            }
            if (mType != null) {
                sb.append(" type=").append(mType);
            }
            if (mOrder != null) {
                sb.append(" order=").append(mOrder);
            }
            if (mAlign != null) {
                sb.append(" align=").append(mAlign);
            }
            if (mMinOccurs >= 0) {
                sb.append(" minOccurs=").append(mMinOccurs);
            }
            if (mMaxOccurs >= 0) {
                sb.append(" maxOccurs=").append(mMaxOccurs);
            }
            sb.append(" optional=").append(mIsOptional);
            sb.append(" repeatable=").append(mIsRepeatable);
            if (mLength > 0) {
                sb.append(" fixedLength=").append(mLength);
            }
            if (mMatch != null && mMatch.length > 0) {
                sb.append(" match=").append(Misc.printable(mMatch));
                sb.append(" isNoMatch=").append(mIsNoMatch);
            }
            if (mOffset >= 0) {
                sb.append(" offset=").append(mOffset);
            }
            if (mPosition >= 0) {
                sb.append(" position=").append(mPosition);
            }
            if (mDefaultValue != null && mDefaultValue.length() > 0) {
                sb.append(" defaultValue=").append(mDefaultValue);
            }
            if (mFromId >= 0) {
                sb.append(" fromId=").append(mFromId);
            }
            if (mNodeId >= 0) {
                sb.append(" nodeId=").append(mNodeId);
            }
            if (mScavengerChars != null && mScavengerChars.length() > 0) {
                sb.append(" scavengerChars=").append(mScavengerChars);
                sb.append(" output1stChar=").append(mOutput1stChar);
            }
            if (mMinNOfChildren >= 0) {
                sb.append(" minNOfChildren=").append(mMinNOfChildren);
            }
            if (mMaxNOfChildren >= 0) {
                sb.append(" maxNOfChildren=").append(mMaxNOfChildren);
            }
            if (mDelimForFixedLen != null) {
                sb.append(" delimForFixedLen=").append(mDelimForFixedLen);
            }
            if (mDelimLevel != null) {
                sb.append(LN).append(" node's delimLevel=[");
                sb.append(mDelimLevel).append("]");
            } else {
                sb.append(" delimLevel=[]");
            }
            if (mElmType != SchemaType.NOT_SIMPLE) {
                sb.append(" elmType=[" + mElmType + "]");
            }
            return sb.toString();
        }

        public boolean isAlter() {
            return mType == Type.ALTER;
        }

        public boolean isArray() {
            return mType == Type.ARRAY;
        }

        public boolean isDelim() {
            return mType == Type.DELIM;
        }

        public boolean isFixed() {
            return mType == Type.FIXED;
        }

        public boolean isGroup() {
            return mType == Type.GROUP;
        }

        public boolean isTrans() {
            return mType == Type.TRANS;
        }

        public boolean isFixedAndLeaf() {
            return mType == Type.FIXED && mSubnodes == null;
        }

        public boolean isOrdered() {
            return mType != Type.ALTER &&
                mOrder != Order.ANY && mOrder != Order.MIX;
        }

        /**
         * Tests if this describes a leaf node field.
         * Same as testing whether there is a child list.
         *
         * @return true if leaf, else false
         */
        public boolean isLeaf() {
            return mSubnodes == null;
        }

        public Type type() {
            return mType;
        }

        public Order order() {
            return mOrder;
        }

        public Align align() {
            return mAlign;
        }
    }

    /**
     * This class is used to describe a simple delimiter, which is used by
     * fixed length fields to skip some bytes before getting the value.
     */
    public static class DelimForFixedLen {

        /**
         * Begin bytes of the delimiter.
         */
        public final byte[] mBeginBytes;
        /**
         * If begin bytes shall be anchored.
         */
        public final boolean mIsBeginBytesAnchored;

        /**
         * Constructs a DelimForFixedLen instance.
         * @param beginBytes begin bytes of the delimiter.
         * @param isBeginBytesAnchored if begin bytes is anchored.
         */
        public DelimForFixedLen(byte[] beginBytes, boolean isBeginBytesAnchored) {
            mBeginBytes = beginBytes;
            mIsBeginBytesAnchored = isBeginBytesAnchored;
        }

        /**
         * For debug purpose.
         * @return  a string representation of the object.
         */
        @Override
        public String toString() {
            StringBuffer buf = new StringBuffer("DelimForFixedLen@")
                .append(Integer.toHexString(hashCode()));
            if (mBeginBytes != null && mBeginBytes.length > 0) {
                buf.append(" beginBytes=").append(Misc.printable(mBeginBytes));
                buf.append(" isBeginBytesAnchored=").append(mIsBeginBytesAnchored);
            }
            return buf.toString();
        }
    }

    /*-----------*\
    |  DEBUGGING  |
    \*-----------*/
    /**
     * Builds a simple OTD node description.
     * Meant for testing only.
     * @return a simple OTD node description.
     * @exception IOException -
     */
    public static Nodes buildTestNodes()
        throws IOException {
        Delim[] delims = new Delim[]{
            new Delim((byte) 3, "A"),
            new Delim((byte) 3, "B")
        };
        OtdDelim otd = new OtdDelim(delims, null);
        Nodes nodes = new Nodes(4, otd);
        /* Built-in example.
         * @param index  the Node.mNodes[] index
         * @param from  the parent node's Fog ID, for tracing
         * @param to  the child node's Fog ID, for tracing
         * @param type  the node type, from Node.Type
         * @param namespace  the namespace from XSD (since 6.0)
         * @param name  display name; NYI: use OtdMeta link instead
         * @param child  field number below parent
         * @param order  the child ordering on input, from Node.Order
         * @param length  for limited fixed fields, else use 0
         * @param align  the match rule algorithm, from Node.Align
         * @param match  exact input match sequence, null if none
         * @param sub  the sub-nodes, as indices in Node.mNodes[], null if none
         * @param level  local delimiter list, or null if none
         * @param offset offset of fixed length field
         * @param position offset from the current position to the position
         *                  where actual length can be found
         * @param defaultValue default value of non-optional field
         * @param scavengerChars scavenger characters
         * @param output1stChar flag indicates if first character of the
         *              scavenger characters should be marshaled out into
         *              output.
         * @param minNOfChildren minimum number of children must have data
         * @param maxNOfChildren maximum number of children must have data
         * @param minOccurs minimum number of occurrences
         * @param maxOccurs maximum number of occurrences, -1 if no limit
         * @param isNoMatch flag indicates if the match condition should be
         *              reverted
         * @param delimForFixedLen
         */
        nodes.addNode(0, -1, -1, Node.Type.GROUP, null, "eins", 0,
            Node.Order.SEQ, 0, Node.Align.BLIND, null, Arrays.asList(1),
            null, -1, -1, null, null, false, -1, -1, 1, 1, false, null,
            SchemaType.NOT_SIMPLE);
        nodes.addNode(1, -1, -1, Node.Type.DELIM, null, "zwei", 0,
            Node.Order.SEQ, 0, Node.Align.BLIND, null, Arrays.asList(2, 3),
            // Local delimiter list.
            new int[][]{
                new int[]{0},
                new int[]{1}
            }, -1, -1, null, null, false, -1, -1, 1, 1, false, null,
            SchemaType.BTC_STRING);
        nodes.addNode(2, -1, -1, Node.Type.DELIM, null, "drei", 0,
            Node.Order.SEQ, 0, Node.Align.BLIND, null, null, null,
            -1, -1, null, null, false, -1, -1, 1, 1, false, null,
            SchemaType.BTC_STRING);
        nodes.addNode(3, -1, -1, Node.Type.DELIM, null, "vier", 1,
            Node.Order.SEQ, 0, Node.Align.BLIND, null, null, null,
            -1, -1, null, null, false, -1, -1, 1, 1, false, null,
            SchemaType.BTC_STRING);
        return nodes;
    }

    /**
     * Tests the node descriptor with a simple case.
     * @param args arguments.
     */
    public static void main(final String[] args) {
        try {
            System.out.println("--BEGIN--");
            Nodes nodes = buildTestNodes();
            System.out.println("--VERIFY--");
            nodes.verify();
            System.out.println("--READY--");
        } catch (Exception ex) {
            ex.printStackTrace();
            System.exit(1);
        }
    }
}
