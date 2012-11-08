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

import com.sun.encoder.hl7.util.Util;
import javax.xml.namespace.QName;
import org.apache.xmlbeans.SchemaType;
import org.xml.sax.SAXException;

/**
 *
 * @author sun
 */
final class HDRSegmentMarshaler
    extends SegmentMarshaler {

    int mCharIndex = -1;
    int mLength = 0;
    String mSegName = Util.MSH; // default to MSH

    public void setSegName(String segName) {
        mSegName = segName;
    }

    public HDRSegmentMarshaler(Marshaler parent, QName name,
        SchemaType type, DelimDataWriter writer) {
        super(parent, name, type, writer);
    }

    @Override
    public void characters(char[] ch, int start, int length)
        throws SAXException {
        if (mChildIndex == 0 && mMatchedSimpleElem != null) { //lt added !=null check
            // i.e. first child element of a header segment
            if (length > 0) {
                mWriter.writeSegName(mSegName);
                mWriter.setFieldSep(ch[start]);
                return;
            }
        } else if (mChildIndex == 1 && mMatchedSimpleElem != null) { //lt added !=null check
            // i.e. second child element of a header segment. It has 4 encoding
            // characters to handle: (1) component separator, (2) repetition
            // separator, (3) escape character, and (4) sub-component separator.
            if (mCharIndex == -1) {
                mCharIndex = 0;
            }
            int oldLength = mLength;
            mLength += length;
            for (; mCharIndex < mLength; mCharIndex++) {
                char c = ch[start + mCharIndex - oldLength];
                switch (mCharIndex) {
                    case 0:
                        mWriter.setComponentSep(c);
                        break;
                    case 1:
                        mWriter.setRepetSep(c);
                        break;
                    case 2:
                        mWriter.setEscapeChar(c);
                        break;
                    case 3:
                        mWriter.setSubCompoSep(c);
                    default:
                    // not possible, but let it go
                }
            }
            mWriter.mEscapeCoder = new HL7CharEscapeCoder(
                mWriter.getFieldSep(),
                mWriter.getComponentSep(),
                mWriter.getSubCompoSep(),
                mWriter.getRepetSep(),
                mWriter.getEscapeChar());
            // no escaping for this one
            mWriter.writeData(new String(ch, start, length));
            return;
        }
        super.characters(ch, start, length);
    }
}


