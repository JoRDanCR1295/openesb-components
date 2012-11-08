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
 * @(#)MarshalHandler.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.encoder.coco.runtime;

import java.io.IOException;
import java.io.OutputStream;
import java.math.BigDecimal;
import java.util.LinkedList;

import javax.xml.namespace.QName;

import org.xml.sax.Attributes;
import org.xml.sax.ContentHandler;
import org.xml.sax.Locator;
import org.xml.sax.SAXException;

import com.sun.encoder.coco.runtime.messages.ErrorManager;
import com.sun.encoder.coco.runtime.messages.Message;
import com.sun.encoder.coco.runtime.messages.MessageCatalog;
import com.sun.encoder.runtime.provider.Misc;
import java.util.logging.Level;
import java.util.logging.Logger;
import com.sun.encoder.runtime.provider.Misc;

/**
 * This class implements SAX <code>ContentHandler</code> interface.
 * It translates SAX events into COBOL Copybook encoded data.
 * 
 * @author Jun Xu
 * @since 6.0
 * @version $Revision: 1.2 $
 */
public final class MarshalHandler implements ContentHandler {

    private static final ErrorManager cErrorMgr =
            ErrorManager.getManager("OpenESB.encoder.COBOLCopybook."
            + MarshalHandler.class.getName());

    private final RuleNode mTopRule;
    private final OutputStream mOutputStream;
    private final NoSyncStack<MarshalContext> mStack;
    private final StringBuilder mBuffer = new StringBuilder(1024);
    private final boolean[] mTrackRedefined;
    private final Integer[] mOccursDependOn;

    Logger mLog = Logger.getLogger(getClass().getName());
    static final String LN = System.getProperty("line.separator");

    private enum MatchDirection {
        FIRST_CHILD,
        CURRENT,
        NEXT_SIBLING
    }
    private MatchDirection mMatchDirection;
    private RuleNode mRuleToBeSkipped = null;
    private int mSkippingLevel;
    
    public MarshalHandler(RuleNode topRule, OutputStream output) {
        if (topRule == null) {
            throw new NullPointerException("no top rule.");
        }
        if (output == null) {
            throw new NullPointerException("no output");
        }
        mTopRule = topRule;
        mTrackRedefined =
            new boolean[mTopRule.getContext().getRedefinedNodes().length];
        mOccursDependOn =
            new Integer[mTopRule.getContext().getOccursDependOnNodes().length];
        mOutputStream = output;
        mStack = new NoSyncStack<MarshalContext>();
        mStack.push(new MarshalContext(RuleNode.wrapAsSuperNode(mTopRule)));
        mMatchDirection = MatchDirection.FIRST_CHILD;
        mSkippingLevel = 0;
        if (mLog.isLoggable(Level.FINE)) {
            mLog.fine(" topRule=" + mTopRule);
        }
    }
    
    public void startElement(String uri, String localName, String qName,
            Attributes atts)
            throws SAXException {
        if (mLog.isLoggable(Level.FINE)) {
            StringBuilder sb = new StringBuilder();
            sb.append("Enter: startElement '").append(localName).append("'");
            if (!localName.equals(qName)) {
                sb.append(", qName='").append(qName).append("'");
            }
            if (mLog.isLoggable(Level.FINER)) {
                if (atts.getLength() > 0) {
                    sb.append(",").append(LN).append(" attributes={");
                }
                for (int i = 0; i < atts.getLength(); i++) {
                    sb.append(" attribute[").append(i).append("]=[");
                    sb.append(atts.getQName(i)).append("=");
                    sb.append(atts.getValue(i)).append("]");
                }
                if (atts.getLength() > 0) {
                    sb.append("}");
                }
            }
            sb.append(". ");
            if (mLog.isLoggable(Level.FINEST)) {
                sb.append(getContextDetails(Level.FINEST));
                mLog.finest(sb.toString());
            } else {
                sb.append(getContextDetails(Level.FINE));
                mLog.fine(sb.toString());
            }
        }
        if (isSkipping() && mSkippingLevel > 0) {
            mSkippingLevel++;
            return;
        }
        RuleNode matchedNode = match(uri, localName);
        if (matchedNode != mStack.peek().mRule) {
            //end of repetition or found first child
            mStack.push(new MarshalContext(matchedNode));
            mStack.peek().mRepetition = 1;
        } else {
            mStack.peek().mRepetition++;
        }
        enterProc(mStack.peek());
        mMatchDirection = MatchDirection.FIRST_CHILD;
        if (!isSkipping()) {
            mBuffer.setLength(0);
        }
        
        if (mLog.isLoggable(Level.FINE)) {
            StringBuilder sb = new StringBuilder("Exit: startElement '");
            sb.append(localName).append("'. ");
            if (mLog.isLoggable(Level.FINEST)) {
                sb.append(getContextDetails(Level.FINEST));
                mLog.finest(sb.toString());
            } else {
                sb.append(getContextDetails(Level.FINE));
                mLog.fine(sb.toString());
            }
        }
    }

    public void characters(char[] chars, int start, int length)
            throws SAXException {
        if (mLog.isLoggable(Level.FINE)) {
            mLog.fine("Receive: character data="
                + Misc.printable(new String(chars, start, length)) + ".");
        }
        if (!isSkipping()) {
            mBuffer.append(chars, start, length);
        }
    }

    public void endElement(String uri, String localName, String qName)
            throws SAXException {
        if (mLog.isLoggable(Level.FINE)) {
            StringBuilder sb = new StringBuilder("Enter: endElement '");
            sb.append(localName).append("'");
            if (!localName.equals(qName)) {
                sb.append(", qName='").append(qName).append("'");
            }
            sb.append(". ");
            if (mLog.isLoggable(Level.FINEST)) {
                sb.append(getContextDetails(Level.FINEST));
                mLog.finest(sb.toString());
            } else {
                sb.append(getContextDetails(Level.FINE));
                mLog.fine(sb.toString());
            }
        }
        if (isSkipping() && mSkippingLevel > 0) {
            mSkippingLevel--;
            if (mSkippingLevel == 0) {
                mMatchDirection = mStack.peek().mRule.getMaxOccurs() > 1 ?
                        MatchDirection.CURRENT : MatchDirection.NEXT_SIBLING;
            }
            return;
        }
        if (mMatchDirection != MatchDirection.FIRST_CHILD) {
            exitProc(mStack.pop(), uri, localName);
        } else {
            try {
            flushData();
            } catch (IOException e) {
                // re-throw
                throw new SAXException(e);
            }
        }
        RuleNode rule = mStack.peek().mRule;
        mMatchDirection = rule.getMaxOccurs() > 1 ?
                MatchDirection.CURRENT : MatchDirection.NEXT_SIBLING;
        int idx = rule.getOccursDependOnIndex();
        if (idx >= 0) {
            mOccursDependOn[idx] = new Integer(mBuffer.toString());
        }
        RuleNode[] children = rule.getChildren(); 
        if (children != null) {
            for (int i = 0; i < children.length; i++) {
                if (children[i].getRedefineIndex() >= 0
                        && !mTrackRedefined[children[i].getRedefineIndex()]) {
                    throw new SAXException("Value is missing for "
                            + "redefined entry: "
                            + rule.getContext().getRedefinedNodes()
                                [children[i].getRedefineIndex()].getQName());
                                                                    
                }
            }
        }
        
        if (mLog.isLoggable(Level.FINE)) {
            StringBuilder sb = new StringBuilder("Exit: endElement '");
            sb.append(localName).append("'. ");
            if (mLog.isLoggable(Level.FINEST)) {
                sb.append(getContextDetails(Level.FINEST));
                mLog.finest(sb.toString());
            } else {
                sb.append(getContextDetails(Level.FINE));
                mLog.fine(sb.toString());
            }
        }
    }
    
    public void startDocument() throws SAXException {
        //no-op
    }
    
    public void processingInstruction(String target, String data)
            throws SAXException {
        //no-op
    }

    public void setDocumentLocator(Locator locator) {
        //no-op
    }
    
    public void skippedEntity(String name) throws SAXException {
        //no-op
    }

    public void ignorableWhitespace(char[] ch, int start, int length)
            throws SAXException {
        //no-op
    }

    public void startPrefixMapping(String prefix, String uri)
            throws SAXException {
        //no-op
    }
    
    public void endPrefixMapping(String prefix) throws SAXException {
        //no-op
    }

    public void endDocument() throws SAXException {
        //no-op
    }

    private RuleNode match(String uri, String localName)
            throws SAXException {
        if (mLog.isLoggable(Level.FINER)) {
            mLog.finer("Match node: '" + localName + "', uri='" + uri + "'.");
        }
        RuleNode currentRule = mStack.peek().mRule;
        int index;
        switch (mMatchDirection) {
            case FIRST_CHILD:
                index = 0;
                break;
            case CURRENT:
                if (currentRule.getQName().getLocalPart().equals(localName)) {
                    if (mLog.isLoggable(Level.FINER)) {
                        mLog.finer("Matched node=" + currentRule);
                    }
                    return currentRule;
                }
            default: //Must be NEXT_SIBLING
                exitProc(mStack.pop(), uri, localName);
                index = currentRule.getIndex() + 1;
                currentRule = mStack.peek().mRule;
                break;
        }
        RuleNode[] children = currentRule.getChildren();
        if (children == null) {
            throw new SAXException(
                    "No matched rule for tag: '" + localName + "'");
        }
        while (index < children.length) {
            if (children[index].getQName().getLocalPart().equals(localName)) {
                if (mLog.isLoggable(Level.FINER)) {
                    mLog.finer("Matched node=" + children[index]);
                }
                return children[index];
            } else if (children[index].getMinOccurs() > 0) {
                throw new SAXException("Expecting: " + children[index].getQName()
                        + ", found: " + new QName(uri, localName));
            }
            index++;
        }
        throw new SAXException(
                "No matched rule for tag: '" + localName + "'");
    }
    
    /**
     * Process when pushed into the stack.
     * 
     * @param context
     */
    private void enterProc(MarshalContext context) {
        RuleNode redefinedNode = context.mRule.getRedefinedNode();
        if (redefinedNode != null) {
            if (mTrackRedefined[redefinedNode.getRedefineIndex()]) {
                mRuleToBeSkipped = context.mRule;
                mSkippingLevel = 1;
            }
        }
        if (!isSkipping()) {
            RuleNode[] children = context.mRule.getChildren();
            if (children != null) {
                for (int i = 0; i < children.length; i++) {
                    if (children[i].getRedefineIndex() >= 0) {
                        mTrackRedefined[children[i].getRedefineIndex()] = false;
                    }
                }
            }
        }
    }

    private boolean isSkipping() {
        return mRuleToBeSkipped != null;
    }
    
    /**
     * Process when popped from the stack.
     * 
     * @param context
     * @param uri namespace URI of the tag currently seen
     * @param localName local name of the tag currently seen
     * @throws SAXException
     */
    private void exitProc(MarshalContext context, String uri, String localName)
            throws SAXException {
        if (context.mRule == mRuleToBeSkipped) {
            mRuleToBeSkipped = null;
            mSkippingLevel = 0;
            return;
        }
        if (context.mRule.getRedefineIndex() >= 0) {
            mTrackRedefined[context.mRule.getRedefineIndex()] = true;
        }
        RuleNode redefined = context.mRule.getRedefinedNode();
        if (redefined != null) {
            mTrackRedefined[redefined.getRedefineIndex()] = true;
        }
        RuleNode dependOn = context.mRule.getOccursDependOn();
        if (dependOn != null) {
            Integer occurs = mOccursDependOn[dependOn.getOccursDependOnIndex()];
            if (occurs == null) {
                throw new SAXException(
                        "Occurs Depending On value is missing for "
                        + context.mRule.getQName());
            }
            if (context.mRepetition < occurs.intValue()) {
                throw new SAXException(
                        context.mRule.getQName()
                        + " should repeat " + occurs
                        + " time(s) but now it only repeated "
                        + context.mRepetition + " time(s)");
            } else if (context.mRepetition < occurs.intValue()) {
                throw new SAXException(
                        context.mRule.getQName()
                        + " can only repeat " + occurs + " time(s) but now it"
                        + " repeated " + context.mRepetition + " time(s)");
            }
        } else {
            if (context.mRepetition < context.mRule.getMinOccurs()) {
                throw new SAXException("Expecting: " + context.mRule.getQName()
                        + ", found: " + new QName(uri, localName));
            }
        }
    }
    
    private void flushData() throws SAXException, IOException {
        RuleNode rule = mStack.peek().mRule;
        if (rule.getChildren() != null && rule.getChildren().length > 0) {
            // non-leaf rule node, then do nothing
            return;
        }
        // must be a leaf rule
        String data = mBuffer.toString();
        if (mLog.isLoggable(Level.FINEST)) {
            mLog.fine("Save buffer to output, data=" + Misc.printable(data));
        } else {
            mLog.fine("Save buffer to output.");
        }
        switch (rule.getCharacteristics().getUsage()) {
            case CobolCharacteristics.USAGE_COMP1:
                CobolDataConverter.encodeToFloat(
                        mOutputStream,
                        Float.parseFloat(data));
                break;
            case CobolCharacteristics.USAGE_COMP2:
                CobolDataConverter.encodeToDouble(
                        mOutputStream,
                        Double.parseDouble(data));
                break;
            case CobolCharacteristics.USAGE_BINARY:
            case CobolCharacteristics.USAGE_COMP:
            case CobolCharacteristics.USAGE_COMP4:
                CobolDataConverter.encodeToBinary(
                        mOutputStream,
                        new BigDecimal(data),
                        rule.getPicture(),
                        rule.getCharacteristics());
                break;
            case CobolCharacteristics.USAGE_PACKED:
            case CobolCharacteristics.USAGE_COMP3:
                CobolDataConverter.encodeToPacked(
                        mOutputStream,
                        new BigDecimal(data),
                        rule.getPicture(),
                        rule.getCharacteristics());
                break;
            case CobolCharacteristics.USAGE_COMP5:
                CobolDataConverter.encodeToNativeBinary(
                        mOutputStream,
                        new BigDecimal(data),
                        rule.getPicture(),
                        rule.getCharacteristics());
                break;
            case CobolCharacteristics.USAGE_INDEX:
                CobolDataConverter.encodeToIndex(
                        mOutputStream,
                        Integer.parseInt(data),
                        rule.getCharacteristics());
                break;
            case CobolCharacteristics.USAGE_DISPLAY:
                switch (rule.getCharacteristics().getPicCategory()) {
                    case CobolCharacteristics.PIC_EXFLOAT:
                        CobolDataConverter.encodeToExternalFloat(
                                mOutputStream,
                                new BigDecimal(data),
                                rule.getPicture(),
                                rule.getCharacteristics(),
                                rule.getCharEncoding());
                        break;
                    case CobolCharacteristics.PIC_NUM:
                        CobolDataConverter.encodeToZoned(
                                mOutputStream,
                                new BigDecimal(data),
                                rule.getPicture(),
                                rule.getCharacteristics(),
                                rule.getCharEncoding());
                        break;
                    case CobolCharacteristics.PIC_ALPHA:
                    case CobolCharacteristics.PIC_ALPHANUM:
                    case CobolCharacteristics.PIC_ALPHANUME:
                    case CobolCharacteristics.PIC_NUME:
                        CobolDataConverter.encodeToDisplay(
                                mOutputStream,
                                data,
                                rule.getCharacteristics(),
                                rule.getCharEncoding());
                        break;
                    case CobolCharacteristics.PIC_DBCS:
                        //Don't know if this possible, but just in case
                        CobolDataConverter.encodeToDbcs(
                                mOutputStream,
                                data.getBytes(
                                    rule.getCharEncoding()),
                                    rule.getCharacteristics().getSize());
                        break;
                    default:
                        Message msg =
                                MessageCatalog.getMessage("CCCB4113");
                        String err =
                                msg.formatText(new Object[]{rule.getQName(),
                                    rule.getCharacteristics().getUsage()});
                        cErrorMgr.log(
                                ErrorManager.Severity.ERROR, null, err);
                        throw new SAXException(err);
                } // end of switch (PicCategory)
                break;
            case CobolCharacteristics.USAGE_DISPLAY1:
                CobolDataConverter.encodeToDbcs(
                        mOutputStream,
                        data.getBytes(
                            rule.getCharEncoding()),
                            rule.getCharacteristics().getSize());
                break;
            default:
                Message msg = MessageCatalog.getMessage("CCCB4112");
                String err =
                        msg.formatText(new Object[]{rule.getQName(),
                            rule.getCharacteristics().getUsage()});
                cErrorMgr.log(ErrorManager.Severity.ERROR, null, err);
                throw new SAXException(err);
        } // end of switch (Usage)
    }
    
    private static final class MarshalContext {
        
        public final RuleNode mRule;
        public int mRepetition;
        
        public MarshalContext(RuleNode rule) {
            mRule = rule;
        }

        @Override
        public String toString() {
            StringBuffer buf = new StringBuffer("MarshalContext@")
                    .append(Integer.toHexString(hashCode()));
            buf.append(" ruleNode=").append(mRule);
            buf.append(" repetition=").append(mRepetition);
            return buf.toString();
        }

        // for debug
        String id() {
            return mRule.getQName().getLocalPart();
        }
    }
    
    private class NoSyncStack<E> extends LinkedList<E> {
        
        private static final long serialVersionUID = 1L;

        public void push(E o) {
            add(o);
        }

        public E pop() {
            if (!isEmpty()) {
                return removeLast();
            }

            throw new RuntimeException("Stack underflow.");
        }

        @Override
        public E peek() {
            if (!isEmpty()) {
                return getLast();
            }

            throw new RuntimeException("Stack underflow.");
        }

        // for debug
        String id() {
            StringBuilder sb = new StringBuilder();
            for (int i = 0; i < size(); i++) {
                sb.append(((MarshalContext) get(i)).id());
                if (i < size() - 1) {
                    sb.append(", ");
                }
            }
            return sb.toString();
        }
    }
    
    private String getContextDetails(Level logLevel) {
        StringBuilder sb = new StringBuilder();
        sb.append(" Details: buffer=").append(Misc.printable(mBuffer.toString()));
        if (mStack != null) {
            sb.append(" stack=");
            if (Level.FINEST.equals(logLevel)) {
                sb.append(mStack);
            } else {
                sb.append("[").append(mStack.id()).append("]");
            }
        }
        if (mTrackRedefined.length > 0) {
            sb.append(" trackRedefined=[");
            for (int i = 0; i < mTrackRedefined.length; i++) {
                sb.append(mTrackRedefined[i]);
                if (i < mTrackRedefined.length - 1) {
                    sb.append(", ");
                }
            }
            sb.append("]");
        }
        if (mOccursDependOn.length > 0) {
            sb.append(" occursDependOn=[");
            for (int i = 0; i < mOccursDependOn.length; i++) {
                sb.append(mOccursDependOn[i]);
                if (i < mOccursDependOn.length - 1) {
                    sb.append(", ");
                }
            }
            sb.append("]");
        }
        if (mMatchDirection != null) {
            sb.append(" matchDirection=").append(mMatchDirection);
        }
        if (mRuleToBeSkipped != null) {
            sb.append(" ruleToBeSkipped=");
            if (Level.FINEST.equals(logLevel)) {
                sb.append(mRuleToBeSkipped);
            } else {
                sb.append(mRuleToBeSkipped.getQName().getLocalPart());
            }
        }
        if (mSkippingLevel > -1) {
            sb.append(" skippingLevel=").append(mSkippingLevel);
        }
        return sb.toString();
    }
}
