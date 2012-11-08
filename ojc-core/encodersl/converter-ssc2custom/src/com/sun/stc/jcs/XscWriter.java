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
 * @(#)XscWriter.java 
 *
 * Copyright 2004-2008 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.stc.jcs;

/**
 * An abstract class that defines the methods available from a XSC Writer to
 * create a SeeBeyond XSC file.  For example:
 * 
 * <pre>
 * try
 * {
 *   XscWriter xw = XscWriterFactory.createWriter("hello.xsc");
 *   xw.open("hello", "com.sun.stc.mysbyn");
 * 
 *   xw.prepareNewElement("greeting");
 *   xw.addAttribute("importance", "normal");
 *   xw.addAttribute("from", "SeeBeyond");
 *   xw.startElement();
 * 
 *   xw.prepareNewElement("line");
 *   xw.addAttribute("content", "hello, world!");
 *   xw.emptyElement();
 * 
 *   xw.endElement();
 * 
 *   xw.close();
 * }
 * catch (XscWriterException e)
 * {
 *   // handle exception
 * }
 * </pre>
 * will create a XSC file, hello.xsc, containing:
 * <pre>
 * <etd name="hello" type="dtd" packageName="com.sun.stc.mysbyn" uid="0">
 *   <greeting importance="normal" from="SeeBeyond">
 *     <line content="hello, world!"/>
 *   </greeting>
 * </etd>
 * </pre>
 * 
 * @author    Ed Wong
 * @version   $Revision: 1.1 $
 * 
 */
public abstract class XscWriter
{
  // essential
  /**
   * Opens the XSC and inserts the start tag for the root "etd" element in it.
   * 
   * @param     etdName           name of the ETD
   * @param     type              type of ETD, ex. "dtd", "bapi"
   * @param     pkg               java package name containing the ETD
   * @exception com.sun.stc.jcs.XscWriterException    when a XML output error occurs
   */
  public abstract void open(String etdName, String type, String pkg)
    throws XscWriterException;


  /**
   * Set the prefix to all UID strings written to a XSC file.
   * When this function is called, all subsequent elements/nodes in the
   * XSC file will have this string prefixed to their uid attributes.
   * This allowes the writer to use unique uids across XSC files.
   * 
   * @param     given             the prefix string
   * @exception none              none
   */
  public abstract void setUidPrefix(String given);
  
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
  public abstract void open(String etdName, String type, String pkg, String version)
    throws XscWriterException;

  /**
   * Opens the XSC and inserts the start tag for the root "etd" element in it.
   * 
   * @param     etdName           name of the ETD
   * @param     type              type of ETD, ex. "dtd", "bapi"
   * @exception com.sun.stc.jcs.XscWriterException    when a XML output error occurs
   */
  public abstract void open(String etdName, String type)
      throws XscWriterException;
  
  // essential
  /**
   * Inserts the end tag for the root "etd" element and closes the XSC.
   */
  public abstract void close();
  
  // essential
  /**
   * Sets the output XSC style.
   * 
   * @param     pretty            <code>true</code> (default) for pretty output; <code>false</code> otherwise
   */
  public abstract void setPrettyPrint(boolean pretty);
  
  // essential
  /**
   * Flushes all current output to the XSC file.
   */
  public abstract void flush();
  
  // essential
  /**
   * Called to begin preparations for an element.
   * 
   * @param     tagName           tag name for the element
   */
  public abstract void prepareNewElement(String tagName);
  
  // essential
  /**
   * Called to add new attributes to an element that's being prepared.
   * 
   * @param     name              name of an attribute
   * @param     value             value of the attribute
   * @exception com.sun.stc.jcs.XscWriterException    when a XML output error occurs
   */
  public abstract void addAttribute(String name, String value)
    throws XscWriterException;
  
  // essential
  /**
   * Called to emit the start form of an element that's being prepared.  It concludes
   * the element preparation.
   * 
   * @exception com.sun.stc.jcs.XscWriterException    when a XML output error occurs
   */
  public abstract void startElement()
    throws XscWriterException;
  
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
  public void startClassElement(String name, String minOccurs, String maxOccurs, boolean optional, String comment)
    throws XscWriterException
  {
    throw new XscWriterException("This method not yet implemented");
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
  public void startClassElement(String name, String minOccurs, String maxOccurs, boolean optional, String comment, String javaName)
    throws XscWriterException
  {
    throw new XscWriterException("This method not yet implemented");
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
  public void startClassElement(String name, String minOccurs, String maxOccurs, boolean optional, String comment, String javaName, String order)
    throws XscWriterException
  {
    throw new XscWriterException("This method not yet implemented");
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
    throws XscWriterException
  {
    throw new XscWriterException("This method not yet implemented");
  }
  
  // essential
  /**
   * Called to emit the end form of an element that has been started with
   * <code>startElement()</code>.
   * 
   * @exception com.sun.stc.jcs.XscWriterException    when a XML output error occurs
   */
  public abstract void endElement()
    throws XscWriterException;
  
  /**
   * Called to emit the end form of a class element that has been started with
   * <code>startClassElement()</code>.
   * 
   * @exception com.sun.stc.jcs.XscWriterException    when a XML output error occurs
   */
  public void endClassElement()
    throws XscWriterException
  {
    throw new XscWriterException("This method not yet implemented");
  }
  
  /**
   * Called to emit the end form of a method element that has been started with
   * <code>startMethodElement()</code>.
   * 
   * @exception com.sun.stc.jcs.XscWriterException    when a XML output error occurs
   */
  public void endMethodElement()
    throws XscWriterException
  {
    throw new XscWriterException("This method not yet implemented");
  }
  
  // essential
  /**
   * Called to emit the empty form of an element that's being prepared.  Concludes the
   * element preparation.
   * 
   * @exception com.sun.stc.jcs.XscWriterException    when a XML output error occurs
   */
  public abstract void emptyElement()
    throws XscWriterException;
  
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
    throws XscWriterException
  {
    throw new XscWriterException("This method not yet implemented");
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
    throws XscWriterException
  {
    throw new XscWriterException("This method not yet implemented");
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
  public void emptyElement(String name, String type, String minOccurs, String maxOccurs, boolean optional)
    throws XscWriterException
  {
    throw new XscWriterException("This method not yet implemented");
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
  public void emptyElement(String name, String type, String minOccurs, String maxOccurs, boolean optional, String comment)
    throws XscWriterException
  {
    throw new XscWriterException("This method not yet implemented");
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
  public void emptyElement(String name, String type, String minOccurs, String maxOccurs, boolean optional, String comment, String javaName)
    throws XscWriterException
  {
    throw new XscWriterException("This method not yet implemented");
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
    throws XscWriterException
  {
    throw new XscWriterException("This method not yet implemented");
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
    throws XscWriterException
  {
    throw new XscWriterException("This method not yet implemented");
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
    throws XscWriterException
  {
    throw new XscWriterException("This method not yet implemented");
  }
  
  public void nodeElement(String name, String type, String minOccurs, String maxOccurs,
			  boolean optional, String comment, String javaName)
      throws XscWriterException {
    throw new XscWriterException("This method not yet implemented");
  }
}
