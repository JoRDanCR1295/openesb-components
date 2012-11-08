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
 * @(#)SAX2XscWriter.java 
 *
 * Copyright 2004-2008 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.stc.jcs;

import java.io.*;
import java.util.*;

import org.xml.sax.helpers.AttributesImpl;

import com.sun.stc.jcsre.xml.SAX2Writer;
import com.sun.stc.compilers.Compiler;

/**
 * Class that implements the <code>XscWriter</code> interface using SAX2 type
 * XML writers for SeeBeyond XSC files.
 * 
 * @version     $Revision: 1.1 $
 */
public class SAX2XscWriter extends XscWriter { 

    protected PrintStream xscOut = null;
    protected SAX2Writer xscHandler = null;
    protected int nextUid = 0;
    protected String uidPrefix = "";
    protected boolean uidAdded = false;
    protected Stack curTagName = null;
    protected AttributesImpl curAttribs = null;
    protected String xscVersion = "0.4";
    protected String[] fieldTypes = {
        "String", "java.lang.String", "boolean", "char", "byte", "short", "int",
        "long", "double", "float"
    };
    
    /**
     * Constructs the SAX2 XSC Writer object.
     * 
     * @param     xscFname    fully-qualified pathname to XSC file
     * @exception java.io.IOException
     *              throw when an I/O error occurs
     */
    SAX2XscWriter(String xscFname)
        throws IOException { 
        super();
        
        xscOut = Compiler.createPrintStream(xscFname);
        xscHandler = new SAX2Writer(false, xscOut);
        xscHandler.setPrettyPrint(true);
        xscHandler.setEscapeNonAscii(true);
        nextUid = 0;
        xscVersion = "0.4";
    }

    public void setUidPrefix(String given) { 
        uidPrefix=given;
    }

    // essential
    /**
     * Opens the XSC and inserts the start tag for the root "etd" element in it.
     * 
     * @param     etdName           name of the ETD
     * @param     type              type of ETD, ex. "dtd", "bapi"
     * @param     pkg               java package name containing the ETD
     * @exception com.sun.stc.jcs.XscWriterException    when a XML output error occurs
     */
    public void open(String etdName, String type, String pkg)
        throws XscWriterException {
        xscVersion = "0.3";
        open(etdName, type, pkg, xscVersion);
    }

    // essential
    /**
     * Opens the XSC and inserts the start tag for the root "etd" element in it.
     * 
     * @param     etdName           name of the ETD
     * @param     type              type of ETD, ex. "dtd", "bapi"
     * @param     pkg               java package name containing the ETD
     * @param     version           XSC version, ex. "0.3"
     * @exception com.sun.stc.jcs.XscWriterException    when a XML output error occurs
     */
    public void open(String etdName, String type, String pkg, String version)
        throws XscWriterException { 
        try { 
            if (null == version || version.trim().length() == 0) { 
                version = "0.3";
            }
            xscVersion = version;
            AttributesImpl attrs = new AttributesImpl();
            attrs.addAttribute("", "name",        "name",        "CDATA", JCSCompiler.normalSafe(etdName));
            attrs.addAttribute("", "type",        "type",        "CDATA", type);
            attrs.addAttribute("", "packageName", "packageName", "CDATA", pkg);
            attrs.addAttribute("", "xscVersion",  "xscVersion",  "CDATA", version);
            attrs.addAttribute("", "uid",         "uid",         "CDATA", uidPrefix+nextUid++);

            xscHandler.startDocument();
            xscHandler.startElement("", "etd", "etd", attrs);
            curTagName = new Stack();
        } catch (Exception e) { 
            throw new XscWriterException(e.getMessage());
        }
    }
    /**
     * Opens the XSC and inserts the start tag for the root "etd" element in it.
     * 
     * @param     etdName           name of the ETD
     * @param     type              type of ETD, ex. "dtd", "bapi"
     * @exception com.sun.stc.jcs.XscWriterException    when a XML output error occurs
     */
    public void open(String etdName, String type)
        throws XscWriterException { 
        try { 
            AttributesImpl attrs = new AttributesImpl();
            if ((etdName != null) && (etdName.length() > 0)) {
                attrs.addAttribute("", "name",        "name",        "CDATA", JCSCompiler.normalSafe(etdName));
            }
            attrs.addAttribute("", "type",        "type",        "CDATA", type);
            xscVersion = "0.4";
            attrs.addAttribute("", "xscVersion",  "xscVersion",  "CDATA", xscVersion);
            attrs.addAttribute("", "uid",         "uid",         "CDATA", uidPrefix+nextUid++);

            xscHandler.startDocument();
            xscHandler.startElement("", "etd", "etd", attrs);
            curTagName = new Stack();
        } catch (Exception e) { 
            throw new XscWriterException(e.getMessage());
        }
    }
    
    // essential
    /**
     * Inserts the end tag for the root "etd" element and closes the XSC.
     */
    public void close() throws XscWriterException { 
        try {
            xscHandler.endElement("", "etd", "etd");
            xscHandler.endDocument();
            xscOut.flush();
            xscOut.close();
        } catch(Exception e) {
            throw new XscWriterException(e.getMessage());
        }
        
        nextUid = 0;
        curTagName = null;
        curAttribs = null;
        xscOut = null;
        xscHandler = null;
    }
    
    // essential
    /**
     * Sets the output XSC style.
     * 
     * @param     pretty            <code>true</code> (default) for pretty output; <code>false</code> otherwise
     */
    public void setPrettyPrint(boolean pretty) { 
        xscHandler.setPrettyPrint(pretty);
    }
    
    // essential
    /**
     * Flushes all current output to the XSC file.
     */
    public void flush() { 
        xscOut.flush();
    }
    
    // essential
    /**
     * Called to begin preparations for an element.
     * 
     * @param     tagName           tag name for the element
     */
    public void prepareNewElement(String tagName) { 
        curTagName.push(tagName);
        curAttribs = new AttributesImpl();
        uidAdded = false;
    }
    
    // essential
    /**
     * Called to add new attributes to an element that's being prepared.
     * 
     * @param     name              name of an attribute
     * @param     value             value of the attribute
     * @exception com.sun.stc.jcs.XscWriterException    when a XML output error occurs
     */
    public void addAttribute(String name, String value)
        throws XscWriterException { 
        if (null == curAttribs) { 
            throw new XscWriterException("...call prepareNewElement() first!");
        }
        try { 
            if (name.equals("uid")) { 
                uidAdded = true;
                value = uidPrefix+nextUid++;
            }
            curAttribs.addAttribute("", name, name, "CDATA", value);
        } catch (Exception e) { 
            throw new XscWriterException(e.getMessage());
        }
    }
    
    // essential
    /**
     * Called to emit the start form of an element that's being prepared.  It concludes
     * the element preparation.
     * 
     * @exception com.sun.stc.jcs.XscWriterException    when a XML output error occurs
     */
    public void startElement()
        throws XscWriterException { 
        if (null == curAttribs) { 
            throw new XscWriterException("...call prepareNewElement() first, [and addAttribute()]!");
        }
        try { 
            if (!uidAdded) { 
                curAttribs.addAttribute("", "uid", "uid", "CDATA", uidPrefix+nextUid++);
                uidAdded = true;
            }
            String tagName = (String) curTagName.peek();
            xscHandler.startElement("", tagName, tagName, curAttribs);
            curAttribs = null;
        } catch (Exception e) { 
            throw new XscWriterException(e.getMessage());
        }
    }

    /**
     * Starts a "class" tag element.  Note, all the standard attributes are supplied
     * here as parameters, thus <code>prepareNewElement()</code> must not be used.
     * 
     * @param     name              name of the class element
     * @param     minOccurs         minimum number of occurences for the element
     * @param     maxOccurs         maximum number of occurences for the element
     * @param     optional          <code>true</code> if the element is optional
     * @param     comment           comment for the element
     * @exception com.sun.stc.jcs.XscWriterException    when a XML output error occurs
     */
    public void startClassElement(String name, String minOccurs, String maxOccurs,
                                  boolean optional, String comment)
        throws XscWriterException { 
        startClassElement(name, minOccurs, maxOccurs, optional, comment, null, null);
    }

    /**
     * Starts a "class" tag element.  Note, all the standard attributes are supplied
     * here as parameters, thus <code>prepareNewElement()</code> must not be used.
     * 
     * @param     name              name of the class element
     * @param     minOccurs         minimum number of occurences for the element
     * @param     maxOccurs         maximum number of occurences for the element
     * @param     optional          <code>true</code> if the element is optional
     * @param     comment           comment for the element
     * @param     javaName          Java name of the element, if different from name
     * @exception com.sun.stc.jcs.XscWriterException    when a XML output error occurs
     */
    public void startClassElement(String name, String minOccurs, String maxOccurs,
                                  boolean optional, String comment, String javaName)
        throws XscWriterException { 
        startClassElement(name, minOccurs, maxOccurs, optional, comment, javaName, null);
    }
    
    /**
     * Starts a "class" tag element.  Note, all the standard attributes are supplied
     * here as parameters, thus <code>prepareNewElement()</code> must not be used.
     * 
     * @param     name              name of the class element
     * @param     minOccurs         minimum number of occurences for the element
     * @param     maxOccurs         maximum number of occurences for the element
     * @param     optional          <code>true</code> if the element is optional
     * @param     comment           comment for the element
     * @param     javaName          Java name of the element, if different from name
     * @param     order             ordering of child elements, e.g., "choice", "sequence", "any"
     * @exception com.sun.stc.jcs.XscWriterException    when a XML output error occurs
     */
    public void startClassElement(String name, String minOccurs, String maxOccurs,
                                  boolean optional, String comment, String javaName,
                                  String order)
        throws XscWriterException { 
        try { 
            String tagName = "class";
            curTagName.push(tagName);
            xscHandler.startElement("", tagName, tagName,
                                    addStandardAttributes(name, "CLASS", minOccurs, maxOccurs,
                                                          optional, comment, javaName, order));
        } catch (Exception e) { 
            throw new XscWriterException(e.getMessage());
        }
    }
                                      
    
    /**
     * Emits the a node element.  Note, all the standard attributes are supplied
     * here as parameters, thus <code>prepareNewElement()</code> must not be used.
     * 
     * @param     name              name of the element
     * @param     type              type of element
     * @param     minOccurs         minimum number of occurences for the element
     * @param     maxOccurs         maximum number of occurences for the element
     * @param     optional          <code>true</code> if the element is optional
     * @param     comment           comment for the element
     * @param     javaName          Java name of the element, if different from name
     * @exception com.sun.stc.jcs.XscWriterException    when a XML output error occurs
     */
    public void nodeElement(String name, String type, String minOccurs, String maxOccurs,
                            boolean optional, String comment, String javaName)
        throws XscWriterException { 
        emptyElement(name, type, minOccurs, maxOccurs, optional, comment, javaName);
    }
    
    /**
     * Starts a "method" tag element.  Note, all the standard attributes are supplied
     * here as parameters, thus <code>prepareNewElement()</code> must not be used.
     * 
     * @param     name              name of the method element
     * @param     signature         Java-style signature for the method
     * @param     returnType        Java return type for the method
     * @param     comment           comment for the element
     * @exception com.sun.stc.jcs.XscWriterException    when a XML output error occurs
     */
    public void startMethodElement(String name, String signature, String returnType, String comment)
        throws XscWriterException { 
        try { 
            String tagName = "method";
            curTagName.push(tagName);
            xscHandler.startElement("", tagName, tagName,
                                    addMethodAttributes(name, signature, returnType, comment));
        } catch (Exception e) { 
            throw new XscWriterException(e.getMessage());
        }
    }

    // essential
    /**
     * Called to emit the end form of an element that has been started with
     * <code>startElement()</code>.
     * 
     * @exception com.sun.stc.jcs.XscWriterException    when a XML output error occurs
     */
    public void endElement()
        throws XscWriterException { 
        if (curTagName.empty()) { 
            throw new XscWriterException("...ended one too many elements???");
        }
        try { 
            String tagName = (String) curTagName.pop();
            xscHandler.endElement("", tagName, tagName);
        } catch (Exception e) { 
            throw new XscWriterException(e.getMessage());
        }
    }
    
    /**
     * Called to emit the end form of a class element that has been started with
     * <code>startClassElement()</code>.
     * 
     * @exception com.sun.stc.jcs.XscWriterException    when a XML output error occurs
     */
    public void endClassElement()
        throws XscWriterException { 
        endElement();
    }
    
    /**
     * Called to emit the end form of a method element that has been started with
     * <code>startMethodElement()</code>.
     * 
     * @exception com.sun.stc.jcs.XscWriterException    when a XML output error occurs
     */
    public void endMethodElement()
        throws XscWriterException { 
        endElement();
    }
    
    // essential
    /**
     * Called to emit the empty form of an element that's being prepared.  Concludes the
     * element preparation.
     * 
     * @exception com.sun.stc.jcs.XscWriterException    when a XML output error occurs
     */
    public void emptyElement()
        throws XscWriterException { 
        if (null == curAttribs) { 
            throw new XscWriterException("...call prepareNewElement() first, [and addAttribute()]!");
        }
        try { 
            if (!uidAdded) { 
                curAttribs.addAttribute("", "uid", "uid", "CDATA", uidPrefix+nextUid++);
                uidAdded = true;
            }
            String tagName = (String) curTagName.pop();
            xscHandler.emptyElement("", tagName, tagName, curAttribs);
            curAttribs = null;
        } catch (Exception e) { 
            throw new XscWriterException(e.getMessage());
        }
    }
    
    /**
     * Emits the empty form of an element.  Note, all the standard attributes are supplied
     * here as parameters, thus <code>prepareNewElement()</code> must not be used.
     * 
     * @param     name              name of the element
     * @param     type              type of element
     * @param     minOccurs         minimum number of occurences for the element
     * @exception com.sun.stc.jcs.XscWriterException    when a XML output error occurs
     */
    public void emptyElement(String name, String type, String minOccurs)
        throws XscWriterException { 
        emptyElement(name, type, minOccurs, null, false, null, null);
    }
    
    /**
     * Emits the empty form of an element.  Note, all the standard attributes are supplied
     * here as parameters, thus <code>prepareNewElement()</code> must not be used.
     * 
     * @param     name              name of the element
     * @param     type              type of element
     * @param     minOccurs         minimum number of occurences for the element
     * @param     maxOccurs         maximum number of occurences for the element
     * @exception com.sun.stc.jcs.XscWriterException    when a XML output error occurs
     */
    public void emptyElement(String name, String type, String minOccurs, String maxOccurs)
        throws XscWriterException { 
        emptyElement(name, type, minOccurs, maxOccurs, false, null, null);
    }
    
    /**
     * Emits the empty form of an element.  Note, all the standard attributes are supplied
     * here as parameters, thus <code>prepareNewElement()</code> must not be used.
     * 
     * @param     name              name of the element
     * @param     type              type of element
     * @param     minOccurs         minimum number of occurences for the element
     * @param     maxOccurs         maximum number of occurences for the element
     * @param     optional          <code>true</code> if the element is optional
     * @exception com.sun.stc.jcs.XscWriterException    when a XML output error occurs
     */
    public void emptyElement(String name, String type, String minOccurs, String maxOccurs,
                             boolean optional)
        throws XscWriterException { 
        emptyElement(name, type, minOccurs, maxOccurs, optional, null, null);
    }
    
    /**
     * Emits the empty form of an element.  Note, all the standard attributes are supplied
     * here as parameters, thus <code>prepareNewElement()</code> must not be used.
     * 
     * @param     name              name of the element
     * @param     type              type of element
     * @param     minOccurs         minimum number of occurences for the element
     * @param     maxOccurs         maximum number of occurences for the element
     * @param     optional          <code>true</code> if the element is optional
     * @param     comment           comment for the element
     * @exception com.sun.stc.jcs.XscWriterException    when a XML output error occurs
     */
    public void emptyElement(String name, String type, String minOccurs, String maxOccurs,
                             boolean optional, String comment)
        throws XscWriterException { 
        emptyElement(name, type, minOccurs, maxOccurs, optional, comment, null);
    }
    
    /**
     * Emits the empty form of an element.  Note, all the standard attributes are supplied
     * here as parameters, thus <code>prepareNewElement()</code> must not be used.
     * 
     * @param     name              name of the element
     * @param     type              type of element
     * @param     minOccurs         minimum number of occurences for the element
     * @param     maxOccurs         maximum number of occurences for the element
     * @param     optional          <code>true</code> if the element is optional
     * @param     comment           comment for the element
     * @param     javaName          Java name of the element, if different from name
     * @exception com.sun.stc.jcs.XscWriterException    when a XML output error occurs
     */
    public void emptyElement(String name, String type, String minOccurs, String maxOccurs,
                             boolean optional, String comment, String javaName)
        throws XscWriterException { 
        try { 
            String tagName = "node";
            xscHandler.emptyElement("", tagName, tagName,
                                    addStandardAttributes(name, type, minOccurs, maxOccurs,
                                                          optional, comment, javaName));
        } catch (Exception e) { 
            throw new XscWriterException(e.getMessage());
        }
    }

    
    /**
     * Emits the empty form of a method element.  Note, all the standard attributes are
     * supplied here as parameters, thus <code>prepareNewElement()</code> must not be used.
     * 
     * @param     name              name of the method element
     * @param     signature         Java-style signature for the method
     * @param     returnType        Java return type for the method
     * @param     comment           comment for the element
     * @exception com.sun.stc.jcs.XscWriterException    when a XML output error occurs
     */
    public void emptyMethodElement(String name, String signature, String returnType, String comment)
        throws XscWriterException { 
        try { 
            String tagName = "method";
            xscHandler.emptyElement("", tagName, tagName,
                                    addMethodAttributes(name, signature, returnType, comment));
        } catch (Exception e) { 
            throw new XscWriterException(e.getMessage());
        }
    }
    
    /**
     * Emits the empty form of a parameter element.  Note, all the standard attributes are
     * supplied here as parameters, thus <code>prepareNewElement()</code> must not be used.
     * 
     * @param     name              name of the method element
     * @param     paramType         Java type for the parameter
     * @param     comment           comment for the element
     * @exception com.sun.stc.jcs.XscWriterException    when a XML output error occurs
     */
    public void emptyParamElement(String name, String type, String comment)
        throws XscWriterException { 
        try { 
            String tagName = "param";
            xscHandler.emptyElement("", tagName, tagName, addParamAttributes(name, type, comment));
        } catch (Exception e) { 
            throw new XscWriterException(e.getMessage());
        }
    }
    
    /**
     * Emits the empty form of a reference element.  Note, all the standard attributes are
     * supplied here as parameters, thus <code>prepareNewElement()</code> must not be used.
     * 
     * @param     name              name of the method element
     * @param     reference         top-level internal template for referencing to
     * @param     comment           comment for the element
     * @exception com.sun.stc.jcs.XscWriterException    when a XML output error occurs
     */
    public void emptyReferenceElement(String name, String reference, String comment)
        throws XscWriterException { 
        try { 
            String tagName = "node";
            xscHandler.emptyElement("", tagName, tagName,
                                    addReferenceAttributes(name, reference, comment));
        } catch (Exception e) { 
            throw new XscWriterException(e.getMessage());
        }
    }
    
    
    /**
     * Emits an empty "class" reference element to an internal template.
     * Note, all the standard attributes are supplied here as parameters, thus
     * <code>prepareNewElement()</code> must not be used.
     * @param     name              name of the class element
     * @param     minOccurs         minimum number of occurences for the element
     * @param     maxOccurs         maximum number of occurences for the element
     * @param     member            name of internal or external template
     * @param     comment           comment for the element
     * @param     javaName          Java name of the element, if different from name
     * @exception com.sun.stc.jcs.XscWriterException    when a XML output error occurs
     */
    public void classReferenceElement(String name, String minOccurs, String maxOccurs,
                                      String member, String comment, String javaName)
        throws XscWriterException {
        classReferenceElement(name, minOccurs, maxOccurs, member, null, comment, javaName);
    }
    
    
    /**
     * Emits an empty "class" reference element.  Note, all the standard attributes
     * are supplied here as parameters, thus <code>prepareNewElement()</code> must
     * not be used.
     * @param     name              name of the class element
     * @param     minOccurs         minimum number of occurences for the element
     * @param     maxOccurs         maximum number of occurences for the element
     * @param     member            name of internal or external template
     * @param     reference         normalized path of external template or <tt>null</tt>
     *                              if internal
     * @param     comment           comment for the element
     * @param     javaName          Java name of the element, if different from name
     * @exception com.sun.stc.jcs.XscWriterException    when a XML output error occurs
     */
    public void classReferenceElement(String name, String minOccurs, String maxOccurs,
                                      String member, String reference, String comment,
                                      String javaName)
        throws XscWriterException {
        try { 
            String tagName = "class";
            xscHandler.emptyElement("", tagName, tagName,
                                    addReferenceAttributes(name, member, comment, javaName, reference));
        } catch (Exception e) { 
            throw new XscWriterException(e.getMessage());
        }
    }

    
    protected AttributesImpl addStandardAttributes(String name, String type, String minOccurs,
                                                   String maxOccurs, boolean optional, String comment)
        throws Exception { 
        return addStandardAttributes(name, type, minOccurs, maxOccurs, optional, comment, null);
    }
    
    protected AttributesImpl addStandardAttributes(String name, String type, String minOccurs,
                                                   String maxOccurs, boolean optional,
                                                   String comment, String javaName)
        throws Exception { 
        return addStandardAttributes(name, type, minOccurs, maxOccurs, optional, comment, javaName, null);
    }
    
    protected AttributesImpl addStandardAttributes(String name, String type, String minOccurs,
                                                   String maxOccurs, boolean optional, String comment,
                                                   String javaName, String order)
        throws Exception { 
        AttributesImpl attribs = new AttributesImpl();
        attribs.addAttribute("", "name", "name", "CDATA", JCSCompiler.normalSafe(name));
        if (javaName != null) {
            attribs.addAttribute("", "javaName", "javaName", "CDATA", javaName);
        }
        if (xscVersion.compareTo("0.4") >= 0) {
            String javaType = null;
            for (int i = 0; i < fieldTypes.length; i++) {
                if (type.equals(fieldTypes[i])) {
                    javaType = type;
                    break;
                }
            }
            if (null == javaType) {
                if ((type.indexOf('.') != -1) || (type.indexOf('[') != -1)) {
                    javaType = type;
                }
            }
            if (javaType != null) {
                attribs.addAttribute("", "javaType", "javaType", "CDATA", javaType);
                type = "FIELD";
            }
        }
        attribs.addAttribute(  "", "type", "type", "CDATA", type);
        if (minOccurs != null) {
            attribs.addAttribute("", "minOccurs", "minOccurs", "CDATA", minOccurs);
        }
        if (maxOccurs != null) {
            attribs.addAttribute("", "maxOccurs", "maxOccurs", "CDATA", maxOccurs);
        }
        attribs.addAttribute(  "", "optional",  "optional",  "CDATA", (optional ? "true" : "false"));
        if (order != null) {
            attribs.addAttribute("", "order", "order", "CDATA", order);
        }
        if (comment != null) {
            attribs.addAttribute("", "comment", "comment", "CDATA", JCSCompiler.normalSafe(comment));
        }
        attribs.addAttribute("", "uid", "uid", "CDATA", uidPrefix+nextUid++);
        return attribs;
    }
    
    protected AttributesImpl addMethodAttributes(String name, String signature, String returnType,
                                                 String comment)
        throws Exception { 
        AttributesImpl attribs = new AttributesImpl();
        attribs.addAttribute(  "", "name",       "name",       "CDATA", name);
        attribs.addAttribute(  "", "type",       "type",       "CDATA", "METHOD");
        attribs.addAttribute(  "", "signature",  "signature",  "CDATA", signature);
        attribs.addAttribute(  "", "returnType", "returnType", "CDATA", returnType);
        if (comment != null) {
            attribs.addAttribute("", "comment",    "comment",    "CDATA", JCSCompiler.normalSafe(comment));
        }
        attribs.addAttribute(  "", "uid",        "uid",        "CDATA", uidPrefix+nextUid++);
        return attribs;
    }
    
    protected AttributesImpl addParamAttributes(String name, String type, String comment)
        throws Exception { 
        AttributesImpl attribs = new AttributesImpl();
        attribs.addAttribute(  "", "name",      "name",      "CDATA", name);
        attribs.addAttribute(  "", "type",      "type",      "CDATA", "PARAM");
        attribs.addAttribute(  "", "paramType", "paramType", "CDATA", type);
        if (comment != null) {
            attribs.addAttribute("", "comment",   "comment",   "CDATA", JCSCompiler.normalSafe(comment));
        }
        attribs.addAttribute(  "", "uid",       "uid",       "CDATA", uidPrefix+nextUid++);
        return attribs;
    }
    
    
    protected AttributesImpl addReferenceAttributes(String name, String member, String comment)
        throws Exception {
        return addReferenceAttributes(name, member, comment, null, null);
    }
    
    
    protected AttributesImpl addReferenceAttributes(String name, String member, String comment,
                                                    String javaName)
        throws Exception {
        return addReferenceAttributes(name, member, comment, javaName);
    }
    
    
    protected AttributesImpl addReferenceAttributes(String name, String member, String comment,
                                                    String javaName, String reference)
        throws Exception { 
        AttributesImpl attribs = new AttributesImpl();
        attribs.addAttribute("", "name", "name", "CDATA", JCSCompiler.normalSafe(name));
        if (javaName != null) {
            attribs.addAttribute("", "javaName", "javaName", "CDATA", javaName);
        }
        if (xscVersion.compareTo("0.4") >= 0) {
            attribs.addAttribute("", "member", "member", "CDATA", member);
            member = "REFERENCE";
            
            if (reference != null) {
                attribs.addAttribute("", "reference", "reference", "CDATA", reference);
            }
        }
        attribs.addAttribute("", "type", "type", "CDATA", member);
        attribs.addAttribute("", "minOccurs", "minOccurs", "CDATA", "1");
        attribs.addAttribute("", "maxOccurs", "maxOccurs", "CDATA", "1");
        attribs.addAttribute("", "optional",  "optional",  "CDATA", "false");
        if (comment != null) {
            attribs.addAttribute("", "comment", "comment", "CDATA", JCSCompiler.normalSafe(comment));
        }
        attribs.addAttribute("", "uid", "uid", "CDATA", uidPrefix+nextUid++);
        return attribs;
    }
    
    
    protected AttributesImpl addJavaPropsAttributes(String pkgName, boolean codeAvailable, String jarFile, String comment) { 
        AttributesImpl attribs = new AttributesImpl();
        attribs.addAttribute(  "", "package",      "package",        "CDATA", pkgName);
        if (codeAvailable) { 
            attribs.addAttribute("", "codeAvailable", "codeAvailable", "CDATA", "true");
        }
        attribs.addAttribute(  "", "jarFile",       "jarFile",       "CDATA", JCSCompiler.normalSafe(jarFile));
        if (comment != null) {
            attribs.addAttribute("", "comment",       "comment",       "CDATA", JCSCompiler.normalSafe(comment));
        }
        attribs.addAttribute(  "", "uid",           "uid",           "CDATA", uidPrefix+nextUid++);
        return attribs;
    }
    
    /**
     * Begins the Java Properties element for the XSC file.
     * 
     * @param       pkgName         package name the Java ETD belongs to
     * @param       codeAvailable   <code>true</code> if associated Java code has been successfully
     *                              created and compiled; <code>false</code> otherwise
     * @param       jarFile         JAR file which will contain the compile Java ETD
     * @param       comment         comment text
     * @exception com.sun.stc.jcs.XscWriterException    when a XML output error occurs
     */
    public void startJavaPropsElement(String pkgName, boolean codeAvailable, String jarFile, String comment)
        throws XscWriterException { 
        try { 
            String tagName = "javaProps";
            curTagName.push(tagName);
            xscHandler.startElement("", tagName, tagName,
                                    addJavaPropsAttributes(pkgName, codeAvailable, jarFile, comment));
        } catch (Exception e) { 
            throw new XscWriterException(e.getMessage());
        }
    }
    
    /**
     * Ends the Java properties element for the XSC file.
     */
    public void endJavaPropsElement() { 
        endElement();
    }
    
    protected AttributesImpl addInterfaceAttributes(String fqClass, String comment) { 
        AttributesImpl attribs = new AttributesImpl();
        attribs.addAttribute(  "", "fqClass", "fqClass", "CDATA", fqClass);
        if (comment != null) {
            attribs.addAttribute("", "comment", "comment", "CDATA", JCSCompiler.normalSafe(comment));
        }
        attribs.addAttribute(  "", "uid",     "uid",     "CDATA", uidPrefix+nextUid++);
        return attribs;
    }
    
    public void startInterfaceElement(String fqClass, String comment) { 
        try { 
            String tagName = "interface";
            curTagName.push(tagName);
            xscHandler.startElement("", tagName, tagName,
                                    addInterfaceAttributes(fqClass, comment));
        } catch (Exception e) { 
            throw new XscWriterException(e.getMessage());
        }
    }
    
    public void endInterfaceElement() { 
        endElement();
    }

    public SAX2Writer getHandler() {
        return xscHandler;
    }
}
