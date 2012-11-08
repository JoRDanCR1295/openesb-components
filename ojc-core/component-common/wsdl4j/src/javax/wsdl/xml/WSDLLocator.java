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
 * @(#)WSDLLocator.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package javax.wsdl.xml;

import org.xml.sax.*;

/**
 * This interface can act as an additional layer of indirection between
 * a WSDLReader and the actual location of WSDL documents. One
 * use could be to retrieve WSDL documents from JAR files, while still
 * retaining the ability to resolve imported documents using relative
 * URIs.
 *
 * @author Owen Burroughs (owenb@uk.ibm.com)
 *
 * @see WSDLReader#readWSDL(WSDLLocator)
 */
public interface WSDLLocator
{
  /**
   * Returns an InputSource "pointed at" the base document.
   * 
   * @return the InputSource object or null if the base document could
   * not be found
   */
  public InputSource getBaseInputSource();

  /**
   * Returns an InputSource "pointed at" an imported wsdl document.
   *
   * @param parentLocation a URI specifying the location of the
   * document doing the importing. This can be null if the import location
   * is not relative to the parent location.
   * @param importLocation a URI specifying the location of the
   * document to import. This might be relative to the parent document's
   * location.
   * @return the InputSource object or null if the import cannot be found.
   */
  public InputSource getImportInputSource(String parentLocation,
                                          String importLocation);

  /**
   * Returns a URI representing the location of the base document.
   */
  public String getBaseURI();

  /**
   * Returns a URI representing the location of the last import document
   * to be resolved. This is used in resolving nested imports where an
   * import location is relative to the parent document.
   */
  public String getLatestImportURI();
  
  /**
   * Releases all associated system resources such as the InputStreams
   * associated with the Base and Import InputSources. 
   */
  public void close();
}
