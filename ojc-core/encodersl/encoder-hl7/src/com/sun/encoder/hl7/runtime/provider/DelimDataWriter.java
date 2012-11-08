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

import java.io.IOException;
import java.io.Writer;
import org.xml.sax.SAXException;
import com.sun.encoder.hl7.runtime.provider.HL7CharEscapeCoder.Result;

/**
 *
 * @author sun
*/
final class DelimDataWriter {

    public HL7CharEscapeCoder mEscapeCoder;

    private final Writer mWriter;

    private char mSegSep = Delimiter.DEFAULT_SEG_DELIM;
    private char mFieldSep = Delimiter.DEFAULT_FIELD_SEP;
    private char mCompoSep = Delimiter.DEFAULT_COMPO_SEP;
    private char mRepetSep = Delimiter.DEFAULT_REPET_SEP;
    private char mEscape = Delimiter.DEFAULT_ESCAPE;
    private char mSubCompoSep = Delimiter.DEFAULT_SUBCOMPO_SEP;

    @Override
    public String toString() {
        StringBuffer buff = new StringBuffer("(DelimDataWriter@")
            .append(Integer.toHexString(hashCode()));
        buff.append(" writer=").append(mWriter.toString());
        return buff.toString();
    }

    public DelimDataWriter(Writer writer) {
        mWriter = writer;
    }

    void writeSegName(String segName) throws SAXException {
        try {
            mWriter.write(segName);
        } catch (IOException e) {
            throw new SAXException(e);
        }
    }

    void writeSegSep() throws SAXException {
        try {
            mWriter.write(mSegSep);
        } catch (IOException e) {
            throw new SAXException(e);
        }
    }

    void writeFieldSep() throws SAXException {
        try {
            mWriter.write(mFieldSep);
        } catch (IOException e) {
            throw new SAXException(e);
        }
    }

    void writeCompoSep() throws SAXException {
        try {
            mWriter.write(mCompoSep);
        } catch (IOException e) {
            throw new SAXException(e);
        }
    }

    void writeSubCompoSep() throws SAXException {
        try {
            mWriter.write(mSubCompoSep);
        } catch (IOException e) {
            throw new SAXException(e);
        }
    }

    void writeRepetSep() throws SAXException {
        try {
            mWriter.write(mRepetSep);
        } catch (IOException e) {
            throw new SAXException(e);
        }
    }

    void writeData(String data) throws SAXException {
        try {
            mWriter.write(data);
        } catch (IOException e) {
            throw new SAXException(e);
        }
    }

    /**
     * This method is used to write out character data when characters event is fired.
     *
     * @param data character array
     * @param start start position of the data
     * @param length length of the data
     * @throws SAXException exception happening during writing
     */
    void writeData(char[] data, int start, int length) throws SAXException {
        try {
            if (mEscapeCoder != null) {
                mEscapeCoder.escape(data, start, length);
                if (mEscapeCoder.hasResult()) {
                    Result res = mEscapeCoder.getResult();
                    mWriter.write(res.mData, res.mStart, res.mCount);
                    return;
                }
            }
            mWriter.write(data, start, length);
        } catch (IOException e) {
            throw new SAXException(e);
        }
    }

    void endData() throws SAXException {
        // no-op
    }

    char getSegSep() {
        return mSegSep;
    }

    void setSegSep(char sep) {
        mSegSep = sep;
    }

    char getFieldSep() {
        return mFieldSep;
    }

    void setFieldSep(char sep) {
        mFieldSep = sep;
    }

    char getComponentSep() {
        return mCompoSep;
    }

    void setComponentSep(char sep) {
        mCompoSep = sep;
    }

    char getSubCompoSep() {
        return mSubCompoSep;
    }

    void setSubCompoSep(char sep) {
        mSubCompoSep = sep;
    }

    char getRepetSep() {
        return mRepetSep;
    }

    void setRepetSep(char sep) {
        mRepetSep = sep;
    }

    public char getEscapeChar() {
        return mEscape;
    }

    public void setEscapeChar(char escape) {
        mEscape = escape;
    }
}


