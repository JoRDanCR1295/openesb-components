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
 * @(#)XscWriterFactory.java 
 *
 * Copyright 2004-2008 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.stc.jcs;

import java.io.*;

/**
 * A factory class to create different types of XSC Writers.  For example:
 * 
 * <pre>
 * XscWriter xw = XscWriterFactory.createWriter("hello.xsc");
 * </pre>
 * 
 * @author    Ed Wong
 * @version   $Revision: 1.1 $
 *
 */
public class XscWriterFactory
{
  private XscWriterFactory()
  {
    super();
  }
  
  /**
   * Creates a XSC Writer of a default type (SAX2)
   * 
   * @param     xscFname              output XSC filename
   * @return    an instance of a XSC Writer
   * @exception java.io.IOException   when there's a problem with the output XSC file
   * @see       com.sun.stc.jcs.XscWriter
   */
  public static XscWriter createWriter(String xscFname)
    throws IOException
  {
    return createWriter("SAX2", xscFname);
  }
  
  /**
   * Creates a XSC Writer of specified type
   * 
   * @param     type                  type of XSC Writer to create (ex. SAX2)
   * @param     xscFname              output XSC filename
   * @return    an instance of the XSC Writer requested or <code>null</code> if type not supported
   * @exception java.io.IOException   when there's a problem with the output XSC file
   * @see       com.sun.stc.jcs.XscWriter
   */
  public static XscWriter createWriter(String type, String xscFname)
    throws IOException
  {
    XscWriter x = null;
    
    if (type.equals("SAX2"))
    {
      x = new SAX2XscWriter(xscFname);
    }
    return x;
  }
}