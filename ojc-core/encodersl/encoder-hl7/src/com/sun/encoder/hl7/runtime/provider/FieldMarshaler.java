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

import javax.xml.namespace.QName;
import org.apache.xmlbeans.SchemaParticle;
import org.apache.xmlbeans.SchemaType;
import org.xml.sax.Attributes;
import org.xml.sax.SAXException;

/**
 *
 * @author sun
 */
final class FieldMarshaler
    extends BaseMarshaler {

    private final MatchResult mMatchResult = new MatchResult();
    private final DelimDataWriter mWriter;
    private Marshaler mParent;
    private QName mName;
    private MarshalerFactory mMarshalFactory = null;
    private QName mMatchedSimpleElem = null;
    private boolean mReceivedEscape = false;
    private boolean mIsFirst = true;
    private int mIndexBeforeMatch;

    public FieldMarshaler(Marshaler parent, QName name,
        SchemaType type, DelimDataWriter writer) {
        super(type);
        mParent = parent;
        mName = name;
        mWriter = writer;
    }

    public void setFactory(MarshalerFactory factory) {
        mMarshalFactory = factory;
    }

    public void reset(Marshaler parent, QName name, SchemaType type) {
        super.reset(type);
        mParent = parent;
        mName = name;
        mIsFirst = true;
    }

    @Override
    protected void endOfRepetition() {
    }

    public Marshaler startElement(String uri, String localName, String qName,
        Attributes atts) throws SAXException {
        String errMsg = null;
        if (mMatchedSimpleElem != null) {
            if ("escape".equals(localName)) {
                mWriter.writeData("\\");
                mWriter.writeData(atts.getValue("V"));
                mWriter.writeData("\\");
                mReceivedEscape = true;
                return this;
            }
            errMsg = mMessages.getString("HL7ENC-E0007.Missing_End_Tag",
                new Object[]{mMatchedSimpleElem});
            throw new SAXException(errMsg);
        }
        mIndexBeforeMatch = mChildIndex;
        match(uri, localName, mMatchResult);
        if (mIsFirst) {
            switch (mModel.getParticleType()) {
                case SchemaParticle.SEQUENCE:
                case SchemaParticle.CHOICE:
                // In case of meaningless group, element or wildcard particle is
                // also possible
                case SchemaParticle.ELEMENT:
                case SchemaParticle.WILDCARD:
                    for (; mIndexBeforeMatch < mChildIndex - 1; mIndexBeforeMatch++) {
                        // sub-component is always non-repeating
                        mWriter.writeCompoSep();
                    }
                    break;
                case SchemaParticle.ALL:
                    break;
                default:
                    errMsg = mMessages.getString("HL7ENC-E0008.Illegal_Particle_Type",
                        new Object[]{mModel.getParticleType()});
                    // Programming error
                    throw new SAXException(errMsg);
            }
        } else {
            switch (mModel.getParticleType()) {
                case SchemaParticle.SEQUENCE:
                // In case of meaningless group, element or wildcard particle is
                // also possible
                case SchemaParticle.ELEMENT:
                case SchemaParticle.WILDCARD:
                    for (; mIndexBeforeMatch < mChildIndex;
                        mIndexBeforeMatch++) {
                        // component is always non-repeating
                        mWriter.writeCompoSep();
                    }
                    break;
                case SchemaParticle.CHOICE:
                    errMsg = mMessages.getString("HL7ENC-E0009.Failed_handling_repetitive_component",
                        new Object[]{uri, localName});
                    // component is always non-repeating
                    throw new SAXException(errMsg);
                case SchemaParticle.ALL:
                    // component is always non-repeating
                    mWriter.writeCompoSep();
                    break;
                default:
                    errMsg = mMessages.getString("HL7ENC-E0008.Illegal_Particle_Type",
                        new Object[]{mModel.getParticleType()});
                    // Programming error
                    throw new SAXException(errMsg);
            }
        }
        mIsFirst = false;
        if (mMatchResult.mResult == MatchResult.MATCH_ELEMENT) {
            if (MarshalHandler.isSimpleContent(mMatchResult.mElementType)) {
                mMatchedSimpleElem = mMatchResult.mElementName;
                return this;
            } else {
                return mMarshalFactory.getComponentMarshaler(
                    this, mMatchResult.mElementName,
                    mMatchResult.mElementType, mWriter);
            }
        } else {
            return mMarshalFactory.getWildcardMarshaler(
                this, new QName(uri, localName),
                mWriter, WildcardMarshaler.COMPO_LEVEL);
        }
    }

    public Marshaler endElement(String uri, String localName,
        String qName) throws SAXException {
        String errMsg = null;
        if (mMatchedSimpleElem != null) {
            if (mReceivedEscape) {
                if ("escape".equals(localName)) {
                    mReceivedEscape = false;
                    return this;
                }
                errMsg = mMessages.getString("HL7ENC-E0006.Unexpected_Element",
                    new Object[]{uri, localName});
                throw new SAXException(errMsg);
            }
            if (!mMatchedSimpleElem.getLocalPart().equals(localName)) {
                errMsg = mMessages.getString("HL7ENC-E0005.Unexpected_Element",
                    new Object[]{mMatchedSimpleElem, uri, localName});
                throw new SAXException(errMsg);
            }
            mWriter.endData();
            mMatchedSimpleElem = null;
            return this;
        } else {
            if (!mName.getLocalPart().equals(localName)) {
                errMsg = mMessages.getString("HL7ENC-E0005.Unexpected_Element",
                    new Object[]{mName, uri, localName});
                throw new SAXException(errMsg);
            }
            return mParent;
        }
    }

    public void characters(char[] ch, int start, int length)
        throws SAXException {
        if (length <= 0 || mMatchedSimpleElem == null) {
            return;
        }

        mWriter.writeData(ch, start, length);
    }
}

