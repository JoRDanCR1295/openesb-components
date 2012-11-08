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
 * @(#)BaseURIResolver.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.wsdl.model.uri;



import java.beans.PropertyChangeListener;
import java.util.Map;
import org.exolab.castor.net.URIException;
import org.exolab.castor.net.URILocation;
import org.exolab.castor.net.URIResolver;

/**
 * Describes the base SeeBeyond URI Resolver.
 *
 * @author Sun Microsystems
 * @version 
 */
public interface BaseURIResolver extends URIResolver, PropertyChangeListener {
    
    /** Gets the current parent repository project element.
     * @return  Parent repository project element.
     */

    
    /** Pushes the parent repository project element onto stack.
     * @param   parentProjElem  Parent project element.
     */

    
    /** Pops off a parent repository project element from the stack.
     * @return  The project element that was popped off.
     */

    
    /** Gets the XML Registry to use to resolve location URI to the respective repository object.
     * @return  XML Registry used to resolve location URI.
     */

    
    /** Sets the map of inlined Schema extensibility elements from the WSDL types element.
     * @param   map     Map of inlined Schema extensibility elements.
     */
    void setMap(Map map);
    
    
    // The following comes from org.exolab.castor.net.URIResolver and is repeated here
    // because the classloader that knows about the eInsightIntegration API doesn't
    // necessarily know about the castor implementation jar which contains the aforementioned
    // interface (and the same classloader that loaded the eInsightIntegration API can ONLY
    // load the castor superclass).  Thus, a dynamic adapter will be needed everytime a URIResolver
    // object needs to be given to castor and thus the existence of the URIResolver methods needs
    // to be guaranteed here in order for delegation in the adapter to work.
    
    
    /**
     * Resolves the given href and documentBase. An
     * implementation of this method should never return
     * null. A URIException may be thrown instead.
     *
     * @param   href            Hyperlink URI reference to resolve.
     * @param   documentBase    Document base URI.
     * @return the URILocation for the URI. [not null]
     * @throws  URIException    When problems occur.
     */
    URILocation resolve(String href, String documentBase)
        throws URIException;
    
    
    /**
     * Resolves the given urn. An implementation of this
     * method may return null if the URN could not be resolved.
     * 
     * @param   urn     URN to resolve.
     * @return the URILocation for the URN.
     * @throws  URIException    When problems occur.
     */
    URILocation resolveURN(String urn)
        throws URIException;
    
    /**
     * Modified by hlin 02-12-2003 to keep defaultSchemaLocation
     * @param   defaultFileLocation     Default file location.
     */
    void setDefaultFileLocation(String defaultFileLocation);
    
 
}
