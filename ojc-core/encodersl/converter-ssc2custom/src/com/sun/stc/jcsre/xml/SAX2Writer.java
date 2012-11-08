/*
 * The Apache Software License, Version 1.1
 *
 *
 * Copyright (c) 1999 The Apache Software Foundation.  All rights 
 * reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 *
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer. 
 *
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in
 *    the documentation and/or other materials provided with the
 *    distribution.
 *
 * 3. The end-user documentation included with the redistribution,
 *    if any, must include the following acknowledgment:  
 *       "This product includes software developed by the
 *        Apache Software Foundation (http://www.apache.org/)."
 *    Alternately, this acknowledgment may appear in the software itself,
 *    if and wherever such third-party acknowledgments normally appear.
 *
 * 4. The names "Xerces" and "Apache Software Foundation" must
 *    not be used to endorse or promote products derived from this
 *    software without prior written permission. For written 
 *    permission, please contact apache@apache.org.
 *
 * 5. Products derived from this software may not be called "Apache",
 *    nor may "Apache" appear in their name, without prior written
 *    permission of the Apache Software Foundation.
 *
 * THIS SOFTWARE IS PROVIDED ``AS IS'' AND ANY EXPRESSED OR IMPLIED
 * WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
 * OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 * DISCLAIMED.  IN NO EVENT SHALL THE APACHE SOFTWARE FOUNDATION OR
 * ITS CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
 * SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
 * LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF
 * USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
 * ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
 * OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT
 * OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
 * SUCH DAMAGE.
 * ====================================================================
 *
 * This software consists of voluntary contributions made by many
 * individuals on behalf of the Apache Software Foundation and was
 * originally based on software copyright (c) 1999, International
 * Business Machines, Inc., http://www.apache.org.  For more
 * information on the Apache Software Foundation, please see
 * <http://www.apache.org/>.
 */

package com.sun.stc.jcsre.xml;
                    
import java.io.*;
import org.xml.sax.helpers.*;
import org.xml.sax.*;
import java.util.*;


/**
 * A sample SAX2 writer. This sample program illustrates how to
 * register a SAX2 ContentHandler and receive the callbacks in
 * order to print a document that is parsed.
 *
 */
public class SAX2Writer 
    extends DefaultHandler {
    
    //
    // Constants
    //
    
    /** Default parser name. */
    private static final String 
        DEFAULT_PARSER_NAME = "org.apache.xerces.parsers.SAXParser";
    
    //
    // Data
    //
    
    /** Print writer. */
    protected BufferedWriter out;
    int numOpenElements = 0;
    boolean escapeNonAscii = false;
    
    /** Canonical output. */
    protected boolean canonical;

    protected String encoding = null;
    
    // Added by Rico for pretty-printing.
    // TBD: May not work for all cases, especially when element is MIXED/ANY type.
    private boolean _pretty = false;
    private String _indentation = "    ";
    private int _indentationLen = _indentation.length();
    private String _indent = "";
    private int _indentLen = 0;
    private boolean _needStartNL;
    private boolean _needEndNL;
    private boolean _indentEnd;

    public void setEscapeNonAscii(boolean val) {
	escapeNonAscii = val;
    }

    public boolean getEscapeNonAscii() {
	return escapeNonAscii;
    }
    
    public void setPrettyPrint(boolean val) {
        _pretty = val;
    }
    
    public void setIndentation(String val) {
        _indentation = val;
	_indentationLen = val.length();
    }
    
    private void printIndent() throws IOException {
	out.write(_indent, 0, _indentLen);
    }
    
    private void indent() {
	_indent += _indentation;
	_indentLen += _indentationLen;
    }
    
    private void undent() {
	_indentLen -= _indentationLen;
	_indent = _indent.substring(0, _indentLen);
    }

    //
    // Constructors
    //

    /** Default constructor. */
    public SAX2Writer(boolean canonical) throws UnsupportedEncodingException {
        this(null, canonical, null);
    }
    public SAX2Writer(boolean canonical, OutputStream stream) throws UnsupportedEncodingException {
        this(null, canonical, stream);
    }

    public SAX2Writer(String encoding, boolean canonical, OutputStream stream) throws UnsupportedEncodingException {

	numOpenElements = 0;

        if (encoding == null) {
            encoding = "UTF-8";
        }
	if(stream == null) {
	    stream = System.out;
	}
        out = new BufferedWriter(new OutputStreamWriter(stream, encoding));
        this.canonical = canonical;
	this.encoding = encoding;
	
    } // <init>(String,boolean)
    
    public SAX2Writer(boolean canonical, Writer writer) {

	numOpenElements = 0;	
        out = new BufferedWriter(writer);
        this.canonical = canonical;
	
    } // <init>(String,boolean)
    public SAX2Writer(boolean canonical, PrintWriter writer) {
	
	numOpenElements = 0;
        out = new BufferedWriter(writer);
        this.canonical = canonical;
	
    } // <init>(String,boolean)
    
    //
    // Public static methods
    //

    /** Prints the output from the SAX callbacks. */
    public static void print(String parserName, String uri, boolean canonical) {

        try {
            DefaultHandler handler = new SAX2Writer(canonical);
	    
            XMLReader parser = (XMLReader)Class.forName(parserName).newInstance();
            parser.setContentHandler(handler);
            parser.setErrorHandler(handler);
            parser.parse(uri);
        }
        catch (Exception e) {
            e.printStackTrace(System.err);
        }

    } // print(String,String,boolean)

    /** Outputs a string (doctype, comments, whatever). Warning: if you're going to
     * write character data, better use characters() so that proper encoding is
     * performed.
     */
    public void writeString(String str) throws SAXException {
	try {
	    out.write(str, 0, str.length());
	} catch(IOException exc) {
	    throw new SAXException(exc);
	}
    }
    
    //
    // DocumentHandler methods
    //
    
    /** Processing instruction. */
    public void processingInstruction(String target, String data) throws SAXException {
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
	} catch(IOException exc) {
	    throw new SAXException(exc);
	}

    } // processingInstruction(String,String)

    /** Start document. */
    public void startDocument() {
        _indent = "";
	_indentLen = 0;
        _needStartNL = false;
        _needEndNL = false;
        _indentEnd = false;
    } // startDocument()

    public void endDocument() throws SAXException {
	try {
	    out.flush();
	    out.close();
	} catch(IOException exc) {
	    throw new SAXException(exc);
	}
    }


    HashMap prefixMap = new HashMap();
    boolean namespacesDeclared = false;

    public void startPrefixMapping(String prefix, String uri) {
	prefixMap.put(prefix, uri);
    }

    public void endPrefixMapping(String prefix) {
	prefixMap.remove(prefix);
    }

    /** Start element. */
    public void startElement(String uri, String local, String raw, Attributes attrs) throws SAXException {
	try {
            java.util.Vector prefixList = new Vector();
	    if (_pretty) {
		if (_needStartNL) {
		    //out.write('\n');
		    out.newLine();
		}
		printIndent();
	    }

	    numOpenElements++;
	    out.write('<');
	    out.write(raw, 0, raw.length());
	    if (attrs != null) {
		if (_pretty) {
		    attrs = sortAttributes(attrs);
		}
		if(!namespacesDeclared) {
		    namespacesDeclared = true;
		    Iterator iter = prefixMap.keySet().iterator();
		    while(iter.hasNext()) {
			String prefix = (String)iter.next();
			String ns = (String)prefixMap.get(prefix);
			if(ns.length() > 0) {
			    String attrName = "xmlns";
			    if(prefix.length() > 0) {
				attrName += ":" + prefix;
                                prefixList.add(prefix);
			    }
			    out.write(' ');
			    out.write(attrName, 0, attrName.length());
			    out.write('=');
			    out.write('"');
			    normalize(ns, out);
			    out.write('"');
			}
		    }
		}

		//esr 64016
		normalizeXsiType(attrs, prefixList);

		int len = attrs.getLength();
		for (int i = 0; i < len; i++) {
		    out.write(' ');
		    String qName = attrs.getQName(i);
		    out.write(qName, 0, qName.length());
		    out.write('=');
		    out.write('"');
		    normalize(attrs.getValue(i), out);
		    out.write('"');
		}
	    }
	    out.write('>');
	    
	    if (_pretty) {
		_needStartNL = true;
		_needEndNL = false;
		_indentEnd = false;
		indent();
	    }
            prefixList.removeAllElements();
	} catch(IOException exc) {
	    throw new SAXException(exc);
	}
    } // startElement(String,String,String,Attributes)

    private void normalizeXsiType(Attributes attrs, Vector xsiList) throws SAXException {
	try {
            /* 
             *  ESR 80756
             *  Check all attributes for namespace definitions.
             *  Add found  namespace to vector which will be used 
             *  to determine whether a namespace definition needs 
             *  to be generated.
            */
            Vector nameSpaceList = new Vector();
	    int length = attrs.getLength();
	    for (int i = 0; i < length; i++) {
	        String qName = attrs.getQName(i);
                if (qName != null && qName.indexOf("xmlns:") >= 0) {
                    String nameSpace = 
                        qName.substring(qName.indexOf(':')+1, qName.length());
                    nameSpaceList.add(nameSpace);
                }
            }            

            String xsiPrefix = null;
	    int len = attrs.getLength();
	    for (int i = 0; i < len; i++) {
	        String qName = attrs.getQName(i);
                if (qName != null && qName.endsWith(":type")) {
	            String attr = attrs.getValue(i);
                    xsiPrefix = null; 
                    if (attr != null && attr.indexOf(":") > 0) {
                        xsiPrefix = attr.substring(0, attr.indexOf(":"));
                        if (xsiList.contains(xsiPrefix) 
                            || nameSpaceList.contains(xsiPrefix)) {
                            xsiPrefix = null;
                        }
                    }
                    break;
                }
	    }
            
	    if (xsiPrefix != null) {
	        Iterator iter = prefixMap.keySet().iterator();
	        while(iter.hasNext()) {
		    String prefix = (String)iter.next();
		    String ns = (String)prefixMap.get(prefix);
		    if(prefix.equals(xsiPrefix) && ns.length() > 0) {
		        String attrName = "xmlns";
		        if(prefix.length() > 0) {
			    attrName += ":" + prefix;
		        }
		        out.write(' ');
		        out.write(attrName, 0, attrName.length());
		        out.write('=');
		        out.write('"');
		        normalize(ns, out);
		        out.write('"');
                           
                        break;
                    }
	        }
	    }
	} catch(IOException exc) {
	    throw new SAXException(exc);
	}

    }
    
    /** End element. */
    public void endElement(String uri, String local, String raw) throws SAXException {
	try {
	    if (_pretty) {
		if (_needEndNL) {
		    //out.write('\n');
		    out.newLine();
		}
		if (_indentEnd) {
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
	    if(numOpenElements == 0) out.flush();
	 	    
	    if (_pretty) {
		undent();
		_indentEnd = true;
		_needEndNL = true;
		_needStartNL = true;
	    }
	} catch(IOException exc) {
	    throw new SAXException(exc);
	}

    } // endElement(String)

    /** Empty element. */
    public void emptyElement(String uri, String local, String raw, Attributes attrs) throws SAXException {
	try {
	    if (_pretty) {
		if (_needStartNL) {
		    //out.write('\n');
		    out.newLine();
		}
		printIndent();
	    }
	    
	    out.write('<');
	    out.write(raw, 0, raw.length());
	    if (attrs != null) {
		if (_pretty) {
		    attrs = sortAttributes(attrs);
		}
		int len = attrs.getLength();
		for (int i = 0; i < len; i++) {
		    out.write(' ');
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
	    if(numOpenElements == 0) out.flush();
	    
	    if (_pretty) {
		_needStartNL = true;
		_needEndNL = true;
		_indentEnd = true;
	    }
	} catch(IOException exc) {
	    throw new SAXException(exc);
	}
    } // emptyElement(String,String,String,Attributes)

    /** Characters. */
    public void characters(char ch[], int start, int length) throws SAXException {
	try {

          /* esr 68186
           * allow DTDCompiler to pass a negative length
           * through org.xml.sax.ContentHandler.characters api
           * to avoid characters normalization
           */
          if (length<0) {
            out.write(ch, start, -1*length);
          } else {
        
	    normalizeCharacters(ch, start, length, out);

          } // esr 68186

	} catch(IOException exc) {
	    throw new SAXException(exc);
	}
	_needStartNL = false;
	_needEndNL = false;
	_indentEnd = false;
	
    } // characters(char[],int,int);

    /** Ignorable whitespace. */
    public void ignorableWhitespace(char ch[], int start, int length) throws SAXException {
        characters(ch, start, length);
    } // ignorableWhitespace(char[],int,int);

    //
    // ErrorHandler methods
    //

    /** Warning. */
    public void warning(SAXParseException ex) {
        System.err.println("[Warning] "+
                           getLocationString(ex)+": "+
                           ex.getMessage());
    }

    /** Error. */
    public void error(SAXParseException ex) {
        System.err.println("[Error] "+
                           getLocationString(ex)+": "+
                           ex.getMessage());
    }

    /** Fatal error. */
    public void fatalError(SAXParseException ex) throws SAXException {
        System.err.println("[Fatal Error] "+
                           getLocationString(ex)+": "+
                           ex.getMessage());
        throw ex;
    }
  
  public Writer getWriter() {
    return out;
  }

    /** Returns a string of the location. */
    private String getLocationString(SAXParseException ex) {
        StringBuffer str = new StringBuffer();
	
        String systemId = ex.getSystemId();
        if (systemId != null) {
            int index = systemId.lastIndexOf('/');
            if (index != -1) 
                systemId = systemId.substring(index + 1);
            str.append(systemId);
        }
        str.append(':');
        str.append(ex.getLineNumber());
        str.append(':');
        str.append(ex.getColumnNumber());
	
        return str.toString();
	
    } // getLocationString(SAXParseException):String

    //
    // Protected static methods
    //
    
    /** Normalizes the given string. */
    protected void normalize(char chs[], int start, int len, Writer out) throws IOException {
	int end = start + len;
	int mark = start;
	String esc = null;
	for(int i = start; i < end; i++) {
	    char ch = chs[i];
	    switch(ch) {
// 	    case '\r': {
// 		if (canonical) {
// 		    esc = "&#xD;";
// 		}
// 		break;
// 	    }
// 	    case '\n': {
// 		if (canonical) {
// 		    esc = "&#xA;";
// 		}
// 		break;
// 	    }
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
		if(escapeNonAscii) {
		    if('\u0020' > ch || ch > '\u007E') {
			// Unprintable or non-ASCII: use XML numeric character reference.
			esc = "&#x";
			for (int n = 12; n >= 0; n -= 4)
			    esc += ("0123456789ABCDEF".charAt((ch >> n) & 0xF));
			esc += ";";
		    }
		}
	    }
	    }
	    if(esc != null) {
		out.write(chs, mark, i - mark);
		out.write(esc, 0, esc.length());
		mark = i + 1; 
		esc = null;
	    }
	}
	out.write(chs, mark, end-mark);
    }

    // If the character data contains predefined entities and does not contain
    // CDEnd ("]]>"), then print out a CDATA section.
    protected void normalizeCharacters(char chs[], int start, int len, Writer out) throws IOException {
	normalize(chs, start, len, out);
// 	String str = new String(chs, start, len);
// 	if (((str.indexOf('<') != -1) ||
// 	     (str.indexOf('>') != -1) ||
// 	     (str.indexOf('"') != -1) ||
// 	     (str.indexOf('\'') != -1) ||
// 	     (str.indexOf('&') != -1)) &&
// 	    (str.indexOf("]]>") == -1)) {
// 	    // 	if(canUseCDATA(chs, start, len)) {
// 	    out.write("<![CDATA[", 0, 9);
// 	    out.write(chs, start, len);
// 	    out.write("]]>", 0, 3);
// 	} else {
// 	    normalize(chs, start, len, out);
// 	}
    }

    private boolean canUseCDATA(char chs[], int start, int len) {
	for(int i = start; i < len; i++) {
	    char ch = chs[i];
	    if ((ch == '<') || (ch == '>') || (ch == '"') || (ch == '\'') || (ch == '&')) {
		for(int j = i+1; j < len; j++) {
		    ch = chs[j];
		    if(ch == ']') {
			if(((j+2) < len) && (chs[j+1] == ']') && (chs[j+2] == '>')) {
			    return false;
			}
		    }
		}
	    } else if(ch == ']') {
		if (((i+2) < len) && (chs[i+1] == ']') && (chs[i+2] == '>')) {
		    return false;
		}
	    }
	}
	return true;
    }

    protected void normalize(String s, Writer out) throws IOException {
	normalize(s.toCharArray(), 0, s.length(), out);
    } // normalize(String):String

    /** Returns a sorted list of attributes. */
    protected Attributes sortAttributes(Attributes attrs) {

        AttributesImpl attributes = new AttributesImpl(attrs);
        return attributes;

    } // sortAttributes(AttributeList):AttributeList

    //
    // Main
    //

    /** Main program entry point. */
    public static void main(String argv[]) {

        // is there anything to do?
        if (argv.length == 0) {
            printUsage();
            System.exit(1);
        }

        // vars
        String  parserName = DEFAULT_PARSER_NAME;
        boolean canonical  = false;

        // check parameters
        for (int i = 0; i < argv.length; i++) {
            String arg = argv[i];

            // options
            if (arg.startsWith("-")) {
                if (arg.equals("-p")) {
                    if (i == argv.length - 1) {
                        System.err.println("error: missing parser name");
                        System.exit(1);
                    }
                    parserName = argv[++i];
                    continue;
                }

                if (arg.equals("-c")) {
                    canonical = true;
                    continue;
                }

                if (arg.equals("-h")) {
                    printUsage();
                    System.exit(1);
                }
            }

            // print uri
            System.err.println(arg+':');
            print(parserName, arg, canonical);
            System.out.println();
        }

    } // main(String[])

    /** Prints the usage. */
    private static void printUsage() {

        System.err.println("usage: java sax.SAX2Writer (options) uri ...");
        System.err.println();
        System.err.println("options:");
        System.err.println("  -p name  Specify SAX parser by name.");
        System.err.println("           Default parser: "+DEFAULT_PARSER_NAME);
        System.err.println("  -c       Canonical XML output.");
        System.err.println("  -h       This help screen.");

    } // printUsage()

} // class SAX2Writer
