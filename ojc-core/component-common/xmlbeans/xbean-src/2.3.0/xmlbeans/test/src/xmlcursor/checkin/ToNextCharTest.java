/*   Copyright 2004 The Apache Software Foundation
 *
 *   Licensed under the Apache License, Version 2.0 (the "License");
 *   you may not use this file except in compliance with the License.
 *   You may obtain a copy of the License at
 *
 *       http://www.apache.org/licenses/LICENSE-2.0
 *
 *   Unless required by applicable law or agreed to in writing, software
 *   distributed under the License is distributed on an "AS IS" BASIS,
 *   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *   See the License for the specific language governing permissions and
 *  limitations under the License.
 */


package xmlcursor.checkin;

import org.apache.xmlbeans.XmlOptions;
import junit.framework.*;
import junit.framework.Assert.*;

import java.io.*;

import org.apache.xmlbeans.XmlObject;
import org.apache.xmlbeans.XmlCursor;
import org.apache.xmlbeans.XmlBeans;
import org.apache.xmlbeans.XmlCursor.TokenType;
import org.apache.xmlbeans.XmlDocumentProperties;
import org.apache.xmlbeans.XmlCursor.XmlBookmark;

import javax.xml.namespace.QName;

import xmlcursor.common.*;

import java.net.URL;


/**
 *
 *
 */
public class ToNextCharTest extends BasicCursorTestCase {
    public ToNextCharTest(String sName) {
        super(sName);
    }

    public static Test suite() {
        return new TestSuite(ToNextCharTest.class);
    }

    public void testToNextCharNegative() throws Exception {
        m_xc = XmlObject.Factory.parse("<foo>early<bar>text</bar></foo>").newCursor();
        toNextTokenOfType(m_xc, TokenType.TEXT);
        assertEquals("early", m_xc.getChars());
        assertEquals(5, m_xc.toNextChar(-1));
        assertEquals(TokenType.START, m_xc.currentTokenType());
        assertEquals("text", m_xc.getTextValue());
    }

    public void testToNextCharGTLength() throws Exception {
        m_xc = XmlObject.Factory.parse("<foo>early<bar>text</bar></foo>").newCursor();
        toNextTokenOfType(m_xc, TokenType.TEXT);
        assertEquals("early", m_xc.getChars());
        assertEquals(5, m_xc.toNextChar(999));
        assertEquals(TokenType.START, m_xc.currentTokenType());
        assertEquals("text", m_xc.getTextValue());
    }

    public void testToNextCharLTLength() throws Exception {
        m_xc = XmlObject.Factory.parse("<foo>early<bar>text</bar></foo>").newCursor();
        toNextTokenOfType(m_xc, TokenType.TEXT);
        assertEquals("early", m_xc.getChars());
        assertEquals(3, m_xc.toNextChar(3));
        assertEquals(TokenType.TEXT, m_xc.currentTokenType());
        assertEquals("ly", m_xc.getChars());
    }

    public void testToNextCharZero() throws Exception {
        m_xc = XmlObject.Factory.parse("<foo>early<bar>text</bar></foo>").newCursor();
        toNextTokenOfType(m_xc, TokenType.TEXT);
        assertEquals("early", m_xc.getChars());
        assertEquals(0, m_xc.toNextChar(0));
        assertEquals(TokenType.TEXT, m_xc.currentTokenType());
        assertEquals("early", m_xc.getChars());
    }

    public void testToNextCharFromATTR() throws Exception {
        m_xc = XmlObject.Factory.parse("<foo attr0=\"val0\">early<bar>text</bar></foo>").newCursor();
        toNextTokenOfType(m_xc, TokenType.ATTR);
        assertEquals("val0", m_xc.getTextValue());
        assertEquals(0, m_xc.toNextChar(2));
        assertEquals(TokenType.ATTR, m_xc.currentTokenType());
        assertEquals("val0", m_xc.getTextValue());
    }
}

