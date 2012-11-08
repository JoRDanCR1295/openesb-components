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
 * @(#)SAX2Writer.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.wsdl.model.common.visitor;

import org.xml.sax.SAXException;
import org.xml.sax.Attributes;
import org.xml.sax.helpers.DefaultHandler;
import org.xml.sax.helpers.AttributesImpl;

import java.io.IOException;
import java.io.UnsupportedEncodingException;
import java.io.Writer;
import java.io.BufferedWriter;
import java.io.PrintWriter;
import java.io.OutputStream;
import java.io.OutputStreamWriter;

import java.util.HashMap;
import java.util.Iterator;

/**
 * Implements a SAX writer that receives callbacks in order to print
 * a document. Adapted from Apache's sample.
 *
 * @author Sun Microsystems
 * @version 
 */
public class SAX2Writer
    extends DefaultHandler {
    
    /** Print writer. */
    protected BufferedWriter out;
    
    /** Number of open elements. */
    int numOpenElements = 0;
    
    /** Flag to escape non-ASCIIs. */
    boolean escapeNonAscii = false;
    
    /** Canonical output. */
    protected boolean canonical;
    
    /** Holds the character encoding. */
    protected String encoding = null;
    
    /**
     * Added by Rico for pretty-printing.
     * TBD: May not work for all cases, especially when element is MIXED/ANY
     * type.
     */
    private boolean mPretty = false;
    
    /** Indentation delta */
    private String mIndentation = "    ";
    
    /** Indentation delta length. */
    private int mIndentationLen = mIndentation.length();
    
    /** Current indentation */
    private String mIndent = "";
    
    /** Current indentation length */
    private int mIndentLen = 0;
    
    /** Flag to emit starting newline */
    private boolean mNeedStartNL;
    
    /** Flag to emit ending newline */
    private boolean mNeedEndNL;
    
    /** Flag to end indentation */
    private boolean mIndentEnd;
    
    /** Holds the prefix map. */
    HashMap prefixMap = new HashMap();
    
    /** Flag to indicate whether XML namespaces are declared. */
    boolean namespacesDeclared = false;
    
    /** Holds indentation spaces for pretty attribute print */
    private String mAttrIndent = "                                        ";
    
    /**
     * Sets escape non-ASCII characters flag.
     * @param   val     <code>true</code> to escape non-ASCIIs.
     */
    public void setEscapeNonAscii(boolean val) {
        escapeNonAscii = val;
    }
    
    /**
     * Gets escape non-ASCII characters flag.
     * @return  <code>true</code> to escape non-ASCIIs.
     */
    public boolean getEscapeNonAscii() {
        return escapeNonAscii;
    }
    
    /**
     * Sets pretty print flag.
     * @param   val     <code>true</code> to do pretty print.
     */
    public void setPrettyPrint(boolean val) {
        mPretty = val;
    }
    
    /**
     * Sets the indentation string.
     * @param val the indentation string
     */
    public void setIndentation(String val) {
        mIndentation = val;
        mIndentationLen = val.length();
    }
    
    /**
     * Emits current indentation.
     * @throws  IOException     When I/O problems occur.
     */
    private void printIndent()
        throws IOException {
        out.write(mIndent, 0, mIndentLen);
    }
    
    /**
     * Increments indentation level
     */
    private void indent() {
        mIndent += mIndentation;
        mIndentLen += mIndentationLen;
    }
    
    /**
     * Decrements indentation level.
     */
    private void undent() {
        mIndentLen -= mIndentationLen;
        mIndent = mIndent.substring(0, mIndentLen);
    }
    
    /** Emits indention for pretty printing of attributes.
     * @param   first   <code>true</code> to indicate that first attribute is being printed.
     * @param   raw     Raw attribute name.
     * @throws  IOException     When IO problems occur.
     */
    private void printIndentAttribute(boolean first, String raw) throws IOException {
        if (first) {
            out.write(' ');
        } else {
            out.newLine();
            printIndent();
            int xtraIn = raw.length() + 2; // <...sp
            while (mAttrIndent.length() < xtraIn) {
                mAttrIndent += mAttrIndent;
            }
            out.write(mAttrIndent.substring(0, xtraIn));
        }
    }
    
    /**
     * Default constructor for <code>SAX2Writer</code> object.
     * @param   canonical   <code>true</code> if canonical SAX writer is to be
     *                      used.
     * @throws  UnsupportedEncodingException    If unsupported encoding error
     *          is detected.
     */
    public SAX2Writer(boolean canonical)
        throws UnsupportedEncodingException {
        this(null, canonical, null);
    }
    
    /**
     * Constructor for <code>SAX2Writer</code> object.
     * @param   canonical   <code>true</code> if canonical SAX writer is to be
     *                      used.
     * @param   stream      Output stream to use.
     * @throws  UnsupportedEncodingException    If unsupported encoding error
     *          is detected.
     */
    public SAX2Writer(boolean canonical, OutputStream stream)
        throws UnsupportedEncodingException {
        this(null, canonical, stream);
    }
    
    /**
     * Constructor for <code>SAX2Writer</code> object.
     * @param   encoding    Character encoding to use.
     * @param   canonical   <code>true</code> if canonical SAX writer is to be
     *                      used.
     * @param   stream      Output stream to use.
     * @throws  UnsupportedEncodingException    If unsupported encoding error
     *          is detected.
     */
    public SAX2Writer(String encoding, boolean canonical, OutputStream stream)
        throws UnsupportedEncodingException {
        numOpenElements = 0;
        if (encoding == null) {
            encoding = "UTF-8";
        }
        if (stream == null) {
            stream = System.out;
        }
        out = new BufferedWriter(new OutputStreamWriter(stream, encoding));
        this.canonical = canonical;
        this.encoding = encoding;
        
    }
    
    /**
     * Constructor for <code>SAX2Writer</code> object.
     * @param   canonical   <code>true</code> if canonical SAX writer is to be
     *                      used.
     * @param   writer      Output writer to use.
     */
    public SAX2Writer(boolean canonical, Writer writer) { 
        numOpenElements = 0;
        out = new BufferedWriter(writer);
        this.canonical = canonical;    
    }
    
    /**
     * Constructor for <code>SAX2Writer</code> object.
     * @param   canonical   <code>true</code> if canonical SAX writer is to be
     *                      used.
     * @param   writer      Print writer to use.
     */
    public SAX2Writer(boolean canonical, PrintWriter writer) { 
        numOpenElements = 0;
        out = new BufferedWriter(writer);
        this.canonical = canonical; 
    }
    
    /**
     * Outputs a string (doctype, comments, whatever).
     * Warning: if you're going to write character data,
     * better use characters() so that proper encoding is performed.
     *
     * @param   str     String to emit.
     * @throws  SAXException    If SAX error occurs.
     */
    public void writeString(String str)
        throws SAXException {
        try {
            out.write(str, 0, str.length());
        } catch (IOException exc) {
            throw new SAXException(exc);
        }
    }
    
    /**
     * Outputs a comment.
     * @param   c   comment.
     * @throws  SAXException    If SAX error occurs.
     */
    public void comment(String c) throws SAXException {
        try {
            if (mPretty) {
                if (mNeedStartNL) {
                    //out.write('\n');
                    out.newLine();
                }
                printIndent();
            }
            
            out.write('<');
            out.write('!');
            out.write('-');
            out.write('-');
            out.write(c, 0, c.length());
            out.write('-');
            out.write('-');
            out.write('>');
        } catch (IOException exc) {
            throw new SAXException(exc);
        }
    }
    
    /**
     * Processing instruction.
     * @param   target          Target of processing instruction.
     * @param   data            Data of processing instruction.
     * @throws  SAXException    If SAX error occurs.
     */
    public void processingInstruction(String target, String data)
            throws SAXException {
        try {
            out.write('<');
            out.write('?');
            out.write(target, 0, target.length());
            if (data != null && data.length() > 0) {
                out.write(' ');
                out.write(data, 0, data.length());
            }
            out.write('?');
            out.write('>');
        } catch (IOException exc) {
            throw new SAXException(exc);
        }
        
    }
    
    /**
     * Starts the document.
     */
    public void startDocument() throws SAXException {
        try {
            mIndent = "";
            mIndentLen = 0;
            mNeedStartNL = false;
            mNeedEndNL = false;
            mIndentEnd = false;
            out.write("<?xml version=\"1.0\" encoding=\"utf-8\" ?>");
            out.newLine();
        } catch (IOException exc) {
            throw new SAXException(exc);
        }
    }
    
    /**
     * Ends the document. The output stream is closed.
     * @throws  SAXException    If SAX error occurs.
     */
    public void endDocument() throws SAXException {
        try {
            out.flush();
            out.close();
        } catch (IOException exc) {
            throw new SAXException(exc);
        }
    }
    
    /**
     * Starts the prefix mapping.
     * @param   prefix  Prefix of namespace.
     * @param   uri     URI of namespace.
     */
    public void startPrefixMapping(String prefix, String uri) {
        prefixMap.put(prefix, uri);
    }
    
    /**
     * Ends the prefix mapping.
     * @param   prefix  Prefix of namespace.
     */
    public void endPrefixMapping(String prefix) {
        prefixMap.remove(prefix);
    }
    
    /**
     * Starts an XML element.
     * @param   uri     URI of namespace.
     * @param   local   Local value of XML tag.
     * @param   raw     Raw value of XML tag.
     * @param   attrs   XML attributes for element.
     * @throws  SAXException    If SAX error occurs.
     */
    public void startElement(String uri, String local, String raw,
                             Attributes attrs) throws SAXException {
        try {
            if (mPretty) {
                if (mNeedStartNL) {
                    out.newLine();
                }
                printIndent();
            }
            
            numOpenElements++;
            out.write('<');
            out.write(raw, 0, raw.length());
            if (attrs != null) {
                if (mPretty) {
                    attrs = sortAttributes(attrs);
                }
                if (!namespacesDeclared) {
                    namespacesDeclared = true;
                    Iterator iter = prefixMap.keySet().iterator();
                    for (int ia = 0; iter.hasNext(); ia++) {
                        String prefix = (String) iter.next();
                        String ns = (String) prefixMap.get(prefix);
                        if (ns.length() > 0) {
                            String attrName = "xmlns";
                            if (prefix.length() > 0) {
                                attrName += ":" + prefix;
                            }
                            if (mPretty) {
                                printIndentAttribute((0 == ia), raw);
                            } else {
                                out.write(' ');
                            }
                            out.write(attrName, 0, attrName.length());
                            out.write('=');
                            out.write('"');
                            normalize(ns, out);
                            out.write('"');
                        }
                    }
                }
                int len = attrs.getLength();
                for (int i = 0; i < len; i++) {
                    if (mPretty) {
                        printIndentAttribute((0 == i), raw);
                    } else {
                        out.write(' ');
                    }
                    String qName = attrs.getQName(i);
                    out.write(qName, 0, qName.length());
                    out.write('=');
                    out.write('"');
                    normalize(attrs.getValue(i), out);
                    out.write('"');
                }
            }
            out.write('>');
            
            if (mPretty) {
                mNeedStartNL = true;
                mNeedEndNL = false;
                mIndentEnd = false;
                indent();
            }
        } catch (IOException exc) {
            throw new SAXException(exc);
        }
    }
    
    /**
     * Ends an XML element.
     * @param   uri     URI of namespace.
     * @param   local   Local value of XML tag.
     * @param   raw     Raw value of XML tag.
     * @throws  SAXException    If SAX error occurs.
     */
    public void endElement(String uri, String local, String raw)
            throws SAXException {
        try {
            if (mPretty) {
                if (mNeedEndNL) {
                    out.newLine();
                }
                if (mIndentEnd) {
                    undent();
                    printIndent();
                    indent();
                }
            }
            out.write('<');
            out.write('/');
            out.write(raw, 0, raw.length());
            out.write('>');
            numOpenElements--;
            if (numOpenElements == 0) {
                out.flush();
            }
            
            if (mPretty) {
                undent();
                mIndentEnd = true;
                mNeedEndNL = true;
                mNeedStartNL = true;
            }
        } catch (IOException exc) {
            throw new SAXException(exc);
        }
        
    }
    
    /**
     * Emits an empty element.
     * @param   uri     URI of namespace.
     * @param   local   Local value of XML tag.
     * @param   raw     Raw value of XML tag.
     * @param   attrs   XML attributes for element.
     * @throws  SAXException    If SAX error occurs.
     */
    public void emptyElement(String uri, String local, String raw,
                             Attributes attrs) throws SAXException {
        try {
            if (mPretty) {
                if (mNeedStartNL) {
                    out.newLine();
                }
                printIndent();
            }
            
            out.write('<');
            out.write(raw, 0, raw.length());
            if (attrs != null) {
                if (mPretty) {
                    attrs = sortAttributes(attrs);
                }
                int len = attrs.getLength();
                for (int i = 0; i < len; i++) {
                    if (mPretty) {
                        printIndentAttribute((0 == i), raw);
                    } else {
                        out.write(' ');
                    }
                    String qName = attrs.getQName(i);
                    out.write(qName, 0, qName.length());
                    out.write('=');
                    out.write('"');
                    normalize(attrs.getValue(i), out);
                    out.write('"');
                }
            }
            out.write('/');
            out.write('>');
            if (numOpenElements == 0) {
                out.flush();
            }
            
            if (mPretty) {
                mNeedStartNL = true;
                mNeedEndNL = true;
                mIndentEnd = true;
            }
        } catch (IOException exc) {
            throw new SAXException(exc);
        }
    }
    
    /**
     * Emits characters.
     * @param   ch      Character array to emit.
     * @param   start   Starting offset in array.
     * @param   length  Number of characters to emit.
     * @throws  SAXException    If SAX error occurs.
     */
    public void characters(char ch[], int start, int length)
            throws SAXException {
        try {
            normalize(ch, start, length, out);
        } catch (IOException exc) {
            throw new SAXException(exc);
        }
        mNeedStartNL = false;
        mNeedEndNL = false;
        mIndentEnd = false;
        
    }
    
    /**
     * Emits ignorable whitespace.
     * @param   ch      Character array of whitespace.
     * @param   start   Starting offset in array.
     * @param   length  Number of characters to emit.
     * @throws SAXException SAX errors
     */
    public void ignorableWhitespace(char ch[], int start, int length)
            throws SAXException {
        characters(ch, start, length);
    }
    
    /**
     * Gets the writer used.
     * @return  the writer in use
     */
    public Writer getWriter() {
        return out;
    }
    
    /**
     * Normalizes the given string.
     * @param   chs     Characters to normalize.
     * @param   start   Starting offset in array.
     * @param   len     Number of characters to normalize.
     * @param   out     Writer to use.
     * @throws  IOException     If I/O error occurs.
     */
    protected void normalize(char chs[], int start, int len, Writer out)
            throws IOException {
        int end = start + len;
        int mark = start;
        String esc = null;
        for (int i = start; i < end; i++) {
            char ch = chs[i];
            switch(ch) {
                case '<': {
                    esc = "&lt;";
                    break;
                }
                case '>': {
                    esc = "&gt;";
                    break;
                }
                case '&': {
                    esc = "&amp;";
                    break;
                }
                case '"': {
                    esc = "&quot;";
                    break;
                }
                case '\'': {
                    esc = "&apos;";
                    break;
                }
                // else, default append char
                default: {
                    if (escapeNonAscii) {
                        if (('\u0020' > ch) || (ch > '\u007E')) {
                            // Unprintable or non-ASCII: use XML numeric
                            // character reference.
                            esc = "&#x";
                            for (int n = 12; n >= 0; n -= 4) {
                                esc += ("0123456789ABCDEF".charAt(
                                            (ch >> n) & 0xF));
                            }
                            esc += ";";
                        }
                    }
                }
            }
            if (esc != null) {
                out.write(chs, mark, i - mark);
                out.write(esc, 0, esc.length());
                mark = i + 1;
                esc = null;
            }
        }
        out.write(chs, mark, end - mark);
    }
    
  
    /**
     * If the character data contains predefined entities and does not contain
     * CDEnd ("]]>"), then print out a CDATA section.
     *
     * @param   chs     Characters to normalize.
     * @param   start   Starting offset in array.
     * @param   len     Number of characters to normalize.
     * @throws  IOException     If I/O error occurs.
     */
    public void cdataSection(char chs[], int start, int len)
        throws IOException {
        String str = new String(chs, start, len);
        if (((str.indexOf('<') != -1)
                    || (str.indexOf('>') != -1)
                    || (str.indexOf('"') != -1)
                    || (str.indexOf('\'') != -1)
                    || (str.indexOf('&') != -1))
                && (str.indexOf("]]>") == -1)) {
            if (canUseCDATA(chs, start, len)) {
                out.write("<![CDATA[", 0, 9);
                out.write(chs, start, len);
                out.write("]]>", 0, 3);
            } else {
                normalize(chs, start, len, out);
            }
        }
    }
    
    /**
     * Tests whether CDATA section can be used.
     * @param chs the character array
     * @param start the start index to check
     * @param len the length of the block to check
     * @return  <code>true</code> if it can
     */
    private boolean canUseCDATA(char[] chs, int start, int len) {
        for (int i = start; i < len; i++) {
            char ch = chs[i];
            if ((ch == '<')
                    || (ch == '>')
                    || (ch == '"')
                    || (ch == '\'')
                    || (ch == '&')) {
                for (int j = i + 1; j < len; j++) {
                    ch = chs[j];
                    if (ch == ']') {
                        if (((j + 2) < len)
                                && (chs[j + 1] == ']')
                                && (chs[j + 2] == '>')) {
                            return false;
                        }
                    }
                }
            } else if (ch == ']') {
                if (((i + 2) < len)
                        && (chs[i + 1] == ']')
                        && (chs[i + 2] == '>')) {
                    return false;
                }
            }
        }
        return true;
    }
    
    /**
     * Normalizes a string.
     * @param   s   String to normalize.
     * @param   out Writer to use.
     * @throws  IOException     If I/O error occurs
     */
    protected void normalize(String s, Writer out)
        throws IOException {
        normalize(s.toCharArray(), 0, s.length(), out);
    }
    
    /**
     * Returns a sorted list of attributes.
     * @param   attrs   XML attributes to sort.
     * @return sorted list of attributes
     */
    protected Attributes sortAttributes(Attributes attrs) { 
        AttributesImpl attributes = new AttributesImpl(attrs);
        return attributes; 
    }
}
