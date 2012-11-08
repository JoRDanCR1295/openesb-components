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

package com.sun.encoder.hl7.runtime.provider;

import java.util.Arrays;
import javax.xml.namespace.QName;
import org.xml.sax.Attributes;
import org.xml.sax.SAXException;
import com.sun.encoder.hl7.i18n.Messages;

/**
 *
 * @author sun
*/
final class WildcardMarshaler implements Marshaler {

	private static Messages mMessages =
        Messages.getMessages(WildcardMarshaler.class);

    public static final int SEGMENT_LEVEL = 1;
    public static final int FIELD_LEVEL = 2;
    public static final int COMPO_LEVEL = 3;
    public static final int SUBCOMPO_LEVEL = 4;
    public static final int NUM_OF_LEVELS = SUBCOMPO_LEVEL + 1;

    private final DelimDataWriter mWriter;

    private Marshaler mParent;
    private int mStartingAt;
    private int mCurrentLevel;
    private QName[] mLastMatched = new QName[NUM_OF_LEVELS];
    private int[] mChildIndex = new int[NUM_OF_LEVELS];
    private int[] mRepetIndex = new int[NUM_OF_LEVELS];
    private boolean[] mStartTagOpen = new boolean[NUM_OF_LEVELS];

    public WildcardMarshaler(Marshaler parent, QName name,
            DelimDataWriter writer, int level) {
        mWriter = writer;
        reset(parent, name, level);
    }

    public void reset(Marshaler parent, QName name, int level) {
        mParent = parent;
        mStartingAt = level;
        mCurrentLevel = mStartingAt + 1;
        Arrays.fill(mLastMatched, null);
        Arrays.fill(mChildIndex, -1);
        Arrays.fill(mRepetIndex, -1);
        Arrays.fill(mStartTagOpen, false);
        mLastMatched[mStartingAt] = name;
    }

    public Marshaler startElement(String uri, String localName,
            String qName, Attributes atts) throws SAXException {
        String errMsg = null;
        if (mStartTagOpen[mCurrentLevel]) {
            mCurrentLevel++;
            if (mCurrentLevel >= NUM_OF_LEVELS) {
                errMsg = mMessages.getString("HL7ENC-E0012.Wildcard_is_deeper",
                            new Object[]{""+mCurrentLevel, uri, localName});
                throw new SAXException(errMsg);
            }
            mLastMatched[mCurrentLevel] = new QName(uri, localName);
            mStartTagOpen[mCurrentLevel] = true;
            mChildIndex[mCurrentLevel] = 0;
            mRepetIndex[mCurrentLevel] = 0;
            return this;
        }
        if (mChildIndex[mCurrentLevel] >= 0) {
            if (mLastMatched[mCurrentLevel].getLocalPart().equals(
                    localName)) {
                mRepetIndex[mCurrentLevel]++;
            } else {
                mLastMatched[mCurrentLevel] = new QName(uri, localName);
                mChildIndex[mCurrentLevel]++;
                mRepetIndex[mCurrentLevel] = 0;
            }
            switch (mCurrentLevel) {
            case FIELD_LEVEL:
                if (mRepetIndex[mCurrentLevel] > 0) {
                    mWriter.writeRepetSep();
                } else {
                    mWriter.writeFieldSep();
                }
                break;
            case COMPO_LEVEL:
                mWriter.writeCompoSep();
                break;
            case SUBCOMPO_LEVEL:
                mWriter.writeSubCompoSep();
                break;
            default:
                errMsg = mMessages.getString("HL7ENC-E0013.Illegal_Level", new Object[]{"" + mCurrentLevel, uri, localName});
                // Not possible unless there is programming error
                throw new SAXException(errMsg);
            }
        } else {
            mLastMatched[mCurrentLevel] = new QName(uri, localName);
            mChildIndex[mCurrentLevel] = 0;
            mRepetIndex[mCurrentLevel] = 0;
        }
        mStartTagOpen[mCurrentLevel] = true;
        return this;
    }

    public Marshaler endElement(String uri, String localName,
            String qName) throws SAXException {
        String errMsg = null;
        if (!mStartTagOpen[mCurrentLevel]) {
            mCurrentLevel--;
        } else {
            mWriter.endData();
        }
        if (mLastMatched[mCurrentLevel] == null) {
            errMsg = mMessages.getString("HL7ENC-E0014.No_matching_Start_Tag",
                new Object[]{uri, localName});
            // This is not possible unless programming error
            throw new SAXException(errMsg);
        }
        if (!mLastMatched[mCurrentLevel].getLocalPart().equals(
                localName)) {
            errMsg = mMessages.getString("HL7ENC-E0004.Unexpected_Element",
                new Object[]{mLastMatched[mCurrentLevel], uri, localName});
            throw new SAXException(errMsg);
        }
        mStartTagOpen[mCurrentLevel] = false;
        if (mCurrentLevel <= mStartingAt) {
            if (mStartingAt == SEGMENT_LEVEL) {
                mWriter.writeSegSep();
            }
            return mParent;
        } else {
            return this;
        }
    }

    public void characters(char[] ch, int start, int length)
            throws SAXException {
        mWriter.writeData(ch, start, length);
    }
}

