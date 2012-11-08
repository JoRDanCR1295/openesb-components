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
import org.apache.xmlbeans.SchemaGlobalElement;
import org.xml.sax.Attributes;
import org.xml.sax.SAXException;

/**
 *
 * @author sun
 */
final class DocumentMarshaler extends BaseMarshaler {

    private final SchemaGlobalElement mElement;
    private final MatchResult mMatchResult = new MatchResult();
    private final DelimDataWriter mWriter;
    // e.g. "ADT_A01.BATCH_ON_ADT_A01."
    private final String mPseudoGroupPrefix;
    private MarshalerFactory mMarshalFactory = null;

    public DocumentMarshaler(SchemaGlobalElement element,
        DelimDataWriter writer, String pseudoGroupPrefix, int occursFactor) {
        super(element.getType());
        mElement = element;
        mWriter = writer;
        mPseudoGroupPrefix = pseudoGroupPrefix;
    }

    public void setFactory(MarshalerFactory factory) {
        mMarshalFactory = factory;
    }

    @Override
    public void endOfRepetition() {
        // no-op
    }

    public Marshaler startElement(String uri, String localName,
        String qName, Attributes atts)
        throws SAXException {

        match(uri, localName, mMatchResult);
        if (mMatchResult.mResult == MatchResult.MATCH_ELEMENT) {
            if (localName.startsWith(mPseudoGroupPrefix)
                || localName.equals(Util.MESSAGEBATCH)) {
                return mMarshalFactory.getPseudoGrpMarshaler(
                    this,
                    mMatchResult.mElementName,
                    mMatchResult.mElementType,
                    mWriter, mPseudoGroupPrefix);
            } else {
                if (Util.isHeaderSegmentName(localName)) {
                    Marshaler marshaler =
                        mMarshalFactory.getHDRSegmentMarshaler(
                            this, mMatchResult.mElementName,
                            mMatchResult.mElementType, mWriter);
                    ((HDRSegmentMarshaler) marshaler).setSegName(localName);
                    return marshaler;
                }
                mWriter.writeSegName(localName);
                mWriter.writeFieldSep();
                return mMarshalFactory.getSegmentMarshaler(
                    this,
                    mMatchResult.mElementName,
                    mMatchResult.mElementType,
                    mWriter);
            }
        } else {
            mWriter.writeSegName(localName);
            mWriter.writeFieldSep();
            SchemaGlobalElement knownElement =
                mElement.getTypeSystem().findElement(
                new QName(uri, localName));
            if (knownElement != null) {
                return mMarshalFactory.getSegmentMarshaler(
                    this,
                    knownElement.getName(),
                    knownElement.getType(),
                    mWriter);
            } else {
                return mMarshalFactory.getWildcardMarshaler(
                    this,
                    new QName(uri, localName),
                    mWriter, WildcardMarshaler.SEGMENT_LEVEL);
            }
        }
    }

    public Marshaler endElement(String uri, String localName,
        String qName) throws SAXException {
        if (!mElement.getName().getLocalPart().equals(localName)) {
            String errMsg = mMessages.getString("HL7ENC-E0005.Unexpected_Element",
                new Object[]{mElement.getName(), uri, localName});
            throw new SAXException(errMsg);
        }
        return null;
    }

    public void characters(char[] ch, int start, int length)
        throws SAXException {
        // no-op // at document level, ignore received characters
    }
}

