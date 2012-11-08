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

import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;

import org.xml.sax.Attributes;
import org.xml.sax.ContentHandler;
import org.xml.sax.SAXException;

import com.sun.encoder.runtime.OtdInputStream;
import com.sun.encoder.runtime.StringCoder;
import com.sun.encoder.custom.runtime.provider.Nodes.Node;
import com.sun.encoder.runtime.provider.Misc;
import com.sun.encoder.tools.xml.SchemaLocationAttributes;
import java.net.URL;
import java.util.Collections;
import java.util.List;
import java.util.logging.Level;
import java.util.logging.Logger;
import javax.xml.bind.DatatypeConverter;
import org.apache.xmlbeans.SchemaType;

/**
 * Match state buffer for the puropose of backtracking during parsing.
 * A sequence of match descriptors forms a linear recipe for constructing
 * the equivalent run-time Java data tree.
 *
 * @author Michael Libourel, Jun Xu
 * @version
 */
public final class Match {

    /**
     * undefined data mapped to this node name.
     */
    public static final String UNDEF_NODE_NAME = "_undefined_";
    /**
     * undefined data mapped to this namespace.
     */
    public static final String UNDEF_NS = "urn:com.sun:encoder:instance";
    private static final EmptyAttributes EMPTY_ATTRS = new EmptyAttributes();
    private Logger mLog = Logger.getLogger(getClass().getName());
    static final String LN = System.getProperty("line.separator");

    /**
     * The url location of the metadata.
     */
    private URL mMetadataLocation;

    /**
     * The namespace of the metadata.
     */
    private String mMetadataNamespace;

    /**
     * Item list.
     */
    private List<Item> mItemList = new ArrayList<Item>();

    /**
     * Index of next free item in map.
     */
    private int mNextItemIdx = 0;

    /** for sorting. */
    private int[] mHead = new int[0];
    private int[] mTail = new int[0];

    /**
     * The index of the last Leave item.
     */
    private int mLastLeaveIndex;

    /**
     * Sets the metadata location.
     * @param location url location.
     */
    public void setMetadataLocation(final URL location) {
        mMetadataLocation = location;
    }

    /**
     * Gets the metadata location.
     * @return the metadata url location.
     */
    public URL getMetadataLocation() {
        return mMetadataLocation;
    }

    /**
     * Sets the metadata namespace.
     * @param namespace the metadata namespace.
     */
    public void setMetadataNamespace(final String namespace) {
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
        StringBuffer buf = new StringBuffer("Match@")
            .append(Integer.toHexString(hashCode()));
        buf.append(" total items=").append(mItemList.size());
        if (mItemList.size() > 0) {
            buf.append(" {").append(LN);
            for (int i = 0; i < mItemList.size(); i++) {
                buf.append(i).append("-> ").append(mItemList.get(i)).append(LN);
            }
            buf.append("} ");
        }
        buf.append(" nextItemIdx=").append(mNextItemIdx);
        buf.append(" lastLeaveIndex=").append(mLastLeaveIndex);
        return buf.toString();
    }

    /*---------------*\
    |  DATA MATCHING  |
    \*---------------*/

    /**
     * Clears all entries.
     */
    public void clear() {
        mNextItemIdx = 0;
        mLastLeaveIndex = -1;
    }

    /**
     * Inserts an item entry.
     *
     * @param itemIdx the item index to which the new item to be inserted.
     * @param type Type of inserted item.
     * @param childIdx the child index of this node below its parent
     * @return item entry added.
     */
    private Item insert(final int itemIdx, final Item.Type type,
        final int childIdx) {
        Item item = new Item();
        mItemList.add(itemIdx, item);
        mNextItemIdx++;
        item.mType = type;
        item.mChildIndex = childIdx;
        return item;
    }

    /**
     * Adds an item entry.
     *
     * @param type Item type.
     * @param childIndex For ENTER and FIELD items, this is the child index
     * of this node below its parent. For LEAVE items, this is the corresponding
     * ENTER item, used to indicate the parent node during a walk.
     * @return Item entry.
     */
    private Item addItem(final Item.Type type, final int childIndex) {
        Item item = null;
        if (mNextItemIdx < mItemList.size()) {
            // Recycle old item.
            item = getItemAt(mNextItemIdx);
        } else {
            // Expand the map buffer.
            item = new Item();
            mItemList.add(item);
        }
        mNextItemIdx++;
        item.mType = type;
        item.mChildIndex = childIndex;
        return item;
    }

    /**
     * Adds an item entry to enter a composite node.
     *
     * @param childIndex  the child index of this node below its parent.
     * @param repIndex  the repetition index, if any, else -1.
     * @param pos  input location, -1 if not known.
     * @return the item index.
     */
    public int enter(final int childIndex, final int repIndex, final long pos) {
        int idx = mNextItemIdx;
        Item item = addItem(Item.Type.ENTER, childIndex);
        item.mRepIndex = repIndex;
        item.mOffset = pos;
        if (mLog.isLoggable(Level.FINE)) {
            mLog.fine("ENTER item added.");
        }
        if (mLog.isLoggable(Level.FINEST)) {
            mLog.finest("Added item[" + idx + "]: " + item);
        }
        return idx;
    }

    /**
     * Adds an entry to enter a singleton composite node.
     *
     * @param child  the child index of this node below its parent.
     * @param pos  input location, -1 if not known.
     * @return the item index.
     */
    public int enter(final int child, final long pos) {
        int repIndex = -1;
        return enter(child, repIndex, pos);
    }

    /**
     * Wipes all entries from the given ENTER onwards.
     *
     * @param enterItemIndex the item index of the corresponding ENTER item.
     */
    public void reset(final int enterItemIndex) {
        if (mLog.isLoggable(Level.FINE)) {
            mLog.fine("resetting from ENTER index=" + enterItemIndex);
        }
        if (mNextItemIdx <= enterItemIndex) {
            throw new IllegalArgumentException("Can not do forward reset.");
        }
        mNextItemIdx = enterItemIndex;
    }

    /**
     * Adds entry to leave a composite node.
     *
     * @param enterItemIndex the item index of the corresponding ENTER item.
     */
    public void leave(final int enterItemIndex) {
        if (mNextItemIdx <= enterItemIndex) {
            throw new IllegalArgumentException("Can not do forward leave.");
        }
        mLastLeaveIndex = mNextItemIdx;
        Item item = addItem(Item.Type.LEAVE, enterItemIndex);
        if (mLog.isLoggable(Level.FINE)) {
            mLog.fine("LEAVE item added.");
        }
        if (mLog.isLoggable(Level.FINEST)) {
            mLog.finest("Added item[" + mLastLeaveIndex + "]: " + item);
        }
    }

    /**
     * Adds an entry for a leaf field.
     *
     * @param childIndex  the child index of this node below its parent
     * @param repIndex  the repetition index, if any, else -1
     * @param offset  the offset to the start of data
     * @param length  the number of bytes matched for the field
     */
    public void field(final int childIndex, final int repIndex,
        final long offset, final int length) {
        int idx = mNextItemIdx;
        Item item = addItem(Item.Type.FIELD, childIndex);
        item.mRepIndex = repIndex;
        item.mOffset = offset;
        item.mLength = length;
        if (mLog.isLoggable(Level.FINE)) {
            mLog.fine("FIELD item added.");
        }
        if (mLog.isLoggable(Level.FINEST)) {
            mLog.finest("Added item[" + idx + "]: " + item);
        }
    }

    /**
     * Inserts a field before the specific location.
     *
     * @param itemIdx the item index to which the field to be inserted before.
     *            if negative, then the field will be added to the end.
     * @param childIndex the child index within its parent
     * @param repIndex the repetition index
     * @param offset the offset to the start of data
     * @param length the number of bytes matched for the field
     */
    public void insertField(final int itemIdx, final int childIndex,
        final int repIndex, final long offset, final int length) {
        if (itemIdx < 0) {
            field(childIndex, repIndex, offset, length);
            return;
        }
        Item item = insert(itemIdx, Item.Type.FIELD, childIndex);
        item.mRepIndex = repIndex;
        item.mOffset = offset;
        item.mLength = length;
        if (mLog.isLoggable(Level.FINE)) {
            mLog.fine("Inserted a field item::" + item);
        }
    }

    /*-----------------*\
    |  DATA CONVERSION  |
    \*-----------------*/

    /**
     * Given a root node, walks the completed map to construct the run-time
     * data tree from the input data, and fires SAX events.
     *
     * @param nodes the nodes of the OTD
     * @param handler the SAX content handler that will receive parsing results
     * @param decoder the character decoder used to decode byte[] into string
     * @param inputStream the input data stream to convert
     * @throws IOException
     * @throws SAXException
     */
    public void apply(Nodes nodes, ContentHandler handler,
            StringCoder decoder, OtdInputStream inputStream)
            throws IOException, SAXException {
        if (handler == null) {
            throw new NullPointerException("No content handler."); //I18N
        }
        if (decoder == null) {
            throw new NullPointerException("No character decoder."); //I18N
        }
        Item item;
        // Find the maximum field length.
        int max = 0;
        for (int i = 1; i < mNextItemIdx - 1; i++) {
            item = getItemAt(i);
            if (item.mType == Item.Type.FIELD) {
                if (max < item.mLength) {
                    max = item.mLength;
                }
            }
        }
        if (mLog.isLoggable(Level.FINEST)) {
            mLog.finest("Maximum field length=" + max);
        }
        // make at most 2 times of max size due to hexBinary requires
        // 2x orginal byte size, and base64Binary requires more than 1x
        // original byte size but less than 2x
        byte[] byteBuf = new byte[max * 2];
        // assumed current input stream position
        long currPos = 0L;

        // rewind to the very beginning of the input stream.
        inputStream.rewind();

        // starts from root node.
        Node rootNode = nodes.get(nodes.getRootNodeIndex());
        if (rootNode.mOrder != Node.Order.SEQ
            && getItemAt(0).mType == Item.Type.ENTER) {
            // rootNode may be in "mixed" (MIX) or "any" (ANY) order
            sort(rootNode, 0);
        }
        Node node = rootNode;
        Node childNode;
        String dataStr;
        // start up
        handler.startDocument();
        handler.startPrefixMapping("xsi",
            "http://www.w3.org/2001/XMLSchema-instance");
        handler.startElement(node.mNamespace, node.mName, node.mName,
                new SchemaLocationAttributes(
                mMetadataNamespace, mMetadataLocation));

        /* Note we ignore the outermost match, which has to be an enter/leave
         * pair, because the root node cannot be a simple node (it needs a
         * generated class, to implement OtdRoot).
         */
        int next = 1;
        for (int i = 1; i < mNextItemIdx - 1; i++) {
            item = getItemAt(next);
            switch (item.mType) {
                case ENTER:
                    // Entering a deeper node, save current in ENTER item.
                    item.mParentNode = node;
                    node = nodes.get(node.mSubnodes[item.mChildIndex]);
                    if (node.mOrder != Node.Order.SEQ) {
                        // node in MIX or ANY order
                        sort(node, next);
                    }
                    if (mLog.isLoggable(Level.FINEST)) {
                        mLog.finest("Start element with name=" + node.mName);
                    }
                    handler.startElement(node.mNamespace, node.mName,
                            node.mName, EMPTY_ATTRS);
                    break;
                case LEAVE:
                    // Pop a node level, restore parent.
                    Item back = getItemAt(item.mChildIndex);
                    // Ensure consistency
                    if (back.mType != Item.Type.ENTER) {
                        throw new RuntimeException("Data map is corrupted; leave #="
                            + next + " has non-enter link=" + item.mChildIndex); //I18N
                    }
                    if (mLog.isLoggable(Level.FINEST)) {
                        mLog.finest("End element with name=" + node.mName);
                    }
                    handler.endElement(node.mNamespace, node.mName, node.mName);
                    node = back.mParentNode;
                    break;
                case FIELD:
                    // Convert the field data.
                    long skipByteCnt = item.mOffset - currPos;
                    if (skipByteCnt < 0) {
                        inputStream.rewind();
                        currPos = 0L;
                        skipByteCnt = item.mOffset;
                    }
                    if (skipByteCnt > 0) {
                        inputStream.skip(skipByteCnt);
                    }
                    currPos += skipByteCnt;
                    inputStream.read(byteBuf, 0, item.mLength);

                    currPos += item.mLength;
                    Node aNode = null;
                    try {
                        aNode = nodes.get(node.mSubnodes[item.mChildIndex]);
                    } catch (ArrayIndexOutOfBoundsException e) {
                        // must be a unmapped node if undefDataPolicy="map"
                    }
                    if (aNode != null && aNode.mElmType == SchemaType.BTC_BASE_64_BINARY) {
                        if (item.mLength == byteBuf.length) {
                            dataStr = DatatypeConverter.printBase64Binary(byteBuf);
                        } else {
                            byte[] bytes = new byte[item.mLength];
                            System.arraycopy(byteBuf, 0, bytes, 0, item.mLength);
                            dataStr = DatatypeConverter.printBase64Binary(bytes);
                        }
                    } else if (aNode != null && aNode.mElmType == SchemaType.BTC_HEX_BINARY) {
                        if (item.mLength == byteBuf.length) {
                            dataStr = DatatypeConverter.printHexBinary(byteBuf);
                        } else {
                            byte[] bytes = new byte[item.mLength];
                            System.arraycopy(byteBuf, 0, bytes, 0, item.mLength);
                            dataStr = DatatypeConverter.printHexBinary(bytes);
                        }
                    } else {
                        dataStr = decoder.decode(byteBuf, 0, item.mLength);
                    }
                    char[] charBuf = new char[dataStr.length()];
                    dataStr.getChars(0, dataStr.length(), charBuf, 0);
                    if (item.mChildIndex != -1) {
                        childNode = nodes.get(node.mSubnodes[item.mChildIndex]);
                        if (mLog.isLoggable(Level.FINEST)) {
                            mLog.finest("Start&End element with name='" + childNode.mName + "', data=" + Misc.printable(dataStr));
                        }
                        handler.startElement(childNode.mNamespace,
                            childNode.mName, childNode.mName, EMPTY_ATTRS);
                        handler.characters(charBuf, 0, dataStr.length());
                        handler.endElement(childNode.mNamespace,
                            childNode.mName, childNode.mName);
                    } else {
                        // must be the undefined field
                        if (mLog.isLoggable(Level.FINEST)) {
                            mLog.finest("Start&End element '" + UNDEF_NODE_NAME + "', data=" + Misc.printable(dataStr));
                        }
                        handler.startElement(UNDEF_NS, UNDEF_NODE_NAME,
                                UNDEF_NODE_NAME, EMPTY_ATTRS);
                        handler.characters(charBuf, 0, dataStr.length());
                        handler.endElement(UNDEF_NS, UNDEF_NODE_NAME,
                                UNDEF_NODE_NAME);
                    }
                    break;
                default:
                    throw new RuntimeException("Item with unknown type=" + item.mType);
            }
            if (item.mNext < 0) {
                next++;
            } else {
                next = item.mNext;
            }
        }
        if (rootNode != node) {
            throw new RuntimeException("Unbalanced data map.");
        }

        // finish up
        handler.endElement(node.mNamespace, node.mName, node.mName);
        handler.endPrefixMapping("xsi");
        handler.endDocument();
        clear();
    }

    /**
     * Apply sorting to make sure the children in "mixed" or "any" order be
     * in right order when SAX XML events are fired (Jun Xu).
     * @param node - Node object.
     * @param itemIdx - item index.
     */
    private void sort(final Node node, int itemIdx) {
        if (getItemAt(itemIdx).mType != Item.Type.ENTER) {
            throw new IllegalArgumentException("Must be ENTER event"); //I18N
        }
        if (node.mSubnodes == null) {
            return;
        }
        if (mHead.length < node.mSubnodes.length) {
            mHead = new int[node.mSubnodes.length];
            mTail = new int[node.mSubnodes.length];
        }
        Arrays.fill(mHead, 0, node.mSubnodes.length, -1);
        Arrays.fill(mTail, 0, node.mSubnodes.length, -1);
        int start = itemIdx;
        int enterChild = -1;
        Item item;
        Item childItem;
        int level = -1;
loop:   while (++itemIdx < mNextItemIdx) {
            item = getItemAt(itemIdx);
            switch (item.mType) {
                case ENTER:
                    if (level == -1) {
                        enterChild = itemIdx;
                    }
                    level++;
                    break;
                case LEAVE:
                    if (level == -1) {
                        if (item.mChildIndex != start) {
                            throw new RuntimeException("Data map is corrupted; leave #="
                                + start + " does not match enter link=" + item.mChildIndex); //I18N
                        }
                        break loop;
                    }
                    --level;
                    if (level == -1) {
                        if (item.mChildIndex != enterChild) {
                            throw new RuntimeException("Data map is corrupted; leave #=" + itemIdx
                                + " does not match enter link=" + item.mChildIndex); //I18N
                        }
                        childItem = getItemAt(enterChild);
                        if (mHead[childItem.mChildIndex] == -1) {
                            //tail must be -1
                            mHead[childItem.mChildIndex] = enterChild;
                            mTail[childItem.mChildIndex] = itemIdx;
                        } else {
                            //tail must not be -1
                            //this is the case for repeating node
                            getItemAt(mTail[childItem.mChildIndex]).mNext = enterChild;
                            mTail[childItem.mChildIndex] = itemIdx;
                        }
                    }
                    break;
                case FIELD:
                    if (level == -1) {
                        childItem = getItemAt(itemIdx);
                        if (mHead[childItem.mChildIndex] == -1) {
                            //tail must be -1
                            mHead[childItem.mChildIndex] = itemIdx;
                            mTail[childItem.mChildIndex] = itemIdx;
                        } else {
                            //tail must not be -1
                            //this is the case for repeating node
                            getItemAt(mTail[childItem.mChildIndex]).mNext = itemIdx;
                            mTail[childItem.mChildIndex] = itemIdx;
                        }
                    }
                    break;
                default:
                    throw new RuntimeException("Item with unknown type=" + item.mType); //I18N
            }
        }
        int last = -1;
        for (int i = 0; i < node.mSubnodes.length; i++) {
            if (mHead[i] != -1) {
                if (last == -1) {
                    getItemAt(start).mNext = mHead[i];
                } else {
                    getItemAt(mTail[last]).mNext = mHead[i];
                }
                last = i;
            }
        }
        if (last >= 0) {
            getItemAt(mTail[last]).mNext = itemIdx;
        }
    }

    /**
     * Returns the last LEAVE item index.
     * @return the last LEAVE item index.
     */
    public int getLastLeaveIndex() {
        return mLastLeaveIndex;
    }

    /*-----------*\
    |  DEBUGGING  |
    \*-----------*/

    /**
     * Gets a description of the last entered node in the match map.
     * The result is an array of pairs: the even elements contain the
     * child indices, the following odd elements contain the repetition
     * indices (if any, -1 for single).
     * For example, the numeric path "3.1[4]" would be equivalent to int[]
     * of {3,-1,   1,4}.
     *
     * @return the descriptor array
     */
    public Integer[] getLastNodePath() {
        List<Integer> reverseList = new ArrayList<Integer>();
        // hierachy level with inner most being 0, and its parent being -1
        // and so on. Note that ENTER-LEAVE pair forms a hierachy level.
        int hierLevel = 0;
        int lastLevel = 0;
        for (int i = mNextItemIdx; --i > 0;) {
            Item item = getItemAt(i);
            switch (item.mType) {
                case ENTER:
                    hierLevel--;
                    if (lastLevel > hierLevel) {
                        lastLevel = hierLevel;
                        reverseList.add(item.mRepIndex);
                        reverseList.add(item.mChildIndex);
                    }
                    break;
                case LEAVE:
                    if (reverseList.isEmpty()) {
                        // Tail of path.
                        reverseList.add(item.mRepIndex);
                        reverseList.add(item.mChildIndex);
                    }
                    hierLevel++;
                    break;
                case FIELD:
                    if (reverseList.isEmpty()) {
                        // Tail of path.
                        reverseList.add(item.mRepIndex);
                        reverseList.add(item.mChildIndex);
                    }
                    break;
                default:
                    // no-op, impossible to be here
            }
        }
        // convert into a normal order array by reversing reverseList.
        Collections.reverse(reverseList);
        if (reverseList.size() == 0) {
            return null;
        } else {
            return reverseList.toArray(new Integer[]{0});
        }
        /*
        int[] result = new int[reverseList.size()];
        int i = 0;
        int j = reverseList.size();
        for (; j-- > 0; i++) {
            result[i] = ((Integer) reverseList.get(j)).intValue();
        }
        return result;*/
    }

    /**
     * Logs the current item list to a logger at a given level.
     * For logging/debug purpose.
     *
     * @param logger a JDK logger.
     */
    public void log(final Logger logger, byte[] data) {
        if (!logger.isLoggable(Level.FINE)) {
            return;
        }
        StringBuffer buff = new StringBuffer("Total items=" + mNextItemIdx);
        for (int i = 0; i < mNextItemIdx; i++) {
            Item item = getItemAt(i);
            buff.append(LN).append("item[").append(i).append("]: ");
            switch (item.mType) {
                case ENTER:
                    buff.append("enter ");
                    if (item.mChildIndex >= 0) {
                        buff.append("child#").append(item.mChildIndex);
                    } else {
                        buff.append("top"); //NOI18N
                    }
                    if (item.mRepIndex >= 0) {
                        buff.append("[").append(item.mRepIndex).append("]");
                    }
                    break;
                case LEAVE:
                    buff.append("leave item[").append(item.mChildIndex).append("]");
                    break;
                case FIELD:
                    buff.append("field child#").append(item.mChildIndex);
                    if (item.mRepIndex >= 0) {
                        buff.append("[").append(item.mRepIndex).append("]");
                    }
                    buff.append(" off/len=").append(item.mOffset);
                    buff.append("/").append(item.mLength);
                    if (logger.isLoggable(Level.FINEST)) {
                        buff.append(" data=\"").append(Misc.bytes2str(
                            data, (int) item.mOffset, item.mLength));
                        buff.append("\"");
                    }
                    break;
                default:
                // no-op. Should not be here.
            }
        }
        if (logger.isLoggable(Level.FINEST)) {
            logger.finest(buff.toString());
        } else {
            logger.fine(buff.toString());
        }
    }

    /**
     * Gets the item in the match list at the given index.
     *
     * @param index the item index in the map.
     * @return the item in the match list at the given index.
     */
    public Item getItemAt(final int index) {
        return mItemList.get(index);
    }

    /**
     * Descriptor on how a particular node was matched during parsing.
     * A sequence of match descriptors forms a linear recipe for constructing
     * the equivalent run-time Java data tree.
     */
    public static class Item {
        /**
         * The type of match item.
         */
        public enum Type {
            /** Start of parent node. */
            ENTER,
            /** Leaf node match to section of input. */
            FIELD,
            /** End of parent node. */
            LEAVE;
        }

        /**
         * The type of match for this Item.
         */
        public Type mType;

        /**
         * For ENTER and FIELD items, this holds the field number below
         * the parent node.  For LEAVE items, this holds the item number
         * of the corresponding ENTER item, used to store the parent node
         * during a walk.
         */
        public int mChildIndex;

        /**
         * index for repetition, -1 if singleton.
         */
        public int mRepIndex;

        /**
         * only for type=FIELD: byte count for field data.
         */
        public int mLength;

        /**
         * only for type=FIELD: offset from message start.
         */
        public long mOffset;

        /**
         * The parent node, saved on processing the enter.  Used to save the
         * parent link while executing the item list; only for type=ENTER.
         */
        public Node mParentNode;

        public int mNext = -1;

        @Override
        public String toString() {
            StringBuffer buf = new StringBuffer("Item@")
                .append(Integer.toHexString(hashCode()));
            buf.append(" type=").append(mType);
            if (mType != Type.LEAVE) {
                if (mChildIndex >= 0) {
                    buf.append(" childIndex=").append(mChildIndex);
                }
            } else {
                buf.append(" Enter item=item[").append(mChildIndex).append("]");
            }
            if (mType == Type.FIELD) {
                buf.append(" offset=").append(mOffset);
                buf.append(" length=").append(mLength);
            }
            if (mType == Type.ENTER) {
                if (mRepIndex >= 0) {
                    buf.append(" repIndex=").append(mRepIndex);
                }
            }
            if (mNext >= 0) {
                buf.append(" next=").append(mNext);
            }
            if (mParentNode != null) {
                buf.append(" parentNode=").append(mParentNode);
            }
            return buf.toString();
        }
    }

    /**
     * Represents Empty Attributes.
     */
    private static final class EmptyAttributes implements Attributes {

        public int getLength() {
            return 0;
        }

        public String getURI(int index) {
            return null;
        }

        public String getLocalName(int index) {
            return null;
        }

        public String getQName(int index) {
            return null;
        }

        public String getType(int index) {
            return null;
        }

        public String getValue(int index) {
            return null;
        }

        public int getIndex(String uri, String localName) {
            return -1;
        }

        public int getIndex(String qName) {
            return -1;
        }

        public String getType(String uri, String localName) {
            return null;
        }

        public String getType(String qName) {
            return null;
        }

        public String getValue(String uri, String localName) {
            return null;
        }

        public String getValue(String qName) {
            return null;
        }
    }
}
