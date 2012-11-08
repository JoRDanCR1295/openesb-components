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
import org.apache.xmlbeans.SchemaGlobalElement;
import org.apache.xmlbeans.SchemaType;

/**
 *
 * @author sun
 */
final class MarshalerFactory {

    private DocumentMarshaler mDocumentMarshaler = null;
    private SegmentMarshaler mSegmentMarshaler = null;
    private FieldMarshaler mFieldMarshaler = null;
    private ComponentMarshaler mComponentMarshaler = null;
    private WildcardMarshaler mWildcardMarshaler = null;
    private HDRSegmentMarshaler mHDRSegmentMarshaler = null;

    Marshaler getDocumentMarshaler(SchemaGlobalElement rootElement,
        DelimDataWriter writer, String pseudoGroupPrefix) {
        if (mDocumentMarshaler == null) {
            mDocumentMarshaler = new DocumentMarshaler(rootElement, writer,
                pseudoGroupPrefix, 1);
            mDocumentMarshaler.setFactory(this);
        } else {
            mDocumentMarshaler.reset(rootElement.getType());
        }
        return mDocumentMarshaler;
    }

    Marshaler getSegmentMarshaler(Marshaler parent, QName name,
        SchemaType type, DelimDataWriter writer) {
        if (mSegmentMarshaler == null) {
            mSegmentMarshaler =
                new SegmentMarshaler(parent, name, type, writer);
            mSegmentMarshaler.setFactory(this);
        } else {
            mSegmentMarshaler.reset(parent, name, type);
        }
        return mSegmentMarshaler;
    }

    Marshaler getFieldMarshaler(Marshaler parent, QName name,
        SchemaType type, DelimDataWriter writer) {
        if (mFieldMarshaler == null) {
            mFieldMarshaler =
                new FieldMarshaler(parent, name, type, writer);
            mFieldMarshaler.setFactory(this);
        } else {
            mFieldMarshaler.reset(parent, name, type);
        }
        return mFieldMarshaler;
    }

    Marshaler getComponentMarshaler(Marshaler parent, QName name,
        SchemaType type, DelimDataWriter writer) {
        if (mComponentMarshaler == null) {
            mComponentMarshaler =
                new ComponentMarshaler(parent, name, type, writer);
        } else {
            mComponentMarshaler.reset(parent, name, type);
        }
        return mComponentMarshaler;
    }

    Marshaler getWildcardMarshaler(Marshaler parent, QName name,
        DelimDataWriter writer, int level) {
        if (mWildcardMarshaler == null) {
            mWildcardMarshaler =
                new WildcardMarshaler(parent, name, writer, level);
        } else {
            mWildcardMarshaler.reset(parent, name, level);
        }
        return mWildcardMarshaler;
    }

    Marshaler getPseudoGrpMarshaler(Marshaler parent, QName name,
        SchemaType type, DelimDataWriter writer,
        String pseudoGroupPrefix) {
        PseudoGrpMarshaler marshaler =
            new PseudoGrpMarshaler(parent, name, type, writer,
                pseudoGroupPrefix);
        marshaler.setFactory(this);
        return marshaler;
    }

    Marshaler getHDRSegmentMarshaler(Marshaler parent, QName name,
        SchemaType type, DelimDataWriter writer) {
        if (mHDRSegmentMarshaler == null) {
            mHDRSegmentMarshaler =
                new HDRSegmentMarshaler(parent, name, type, writer);
            mHDRSegmentMarshaler.setFactory(this);
        } else {
            mHDRSegmentMarshaler.reset(parent, name, type);
        }
        return mHDRSegmentMarshaler;
    }
}

