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
 * @(#)DynamicString.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.bpel.xml.common.model;



/**
 * Describes a dynamic string.
 *
 * @author Sun Microsystems
 * @version 
 * @since   5.1.0
 */
public interface DynamicString {
    
    /** Gets the dynamic string repository object context.
     * @return  Repository object context for dynamic string.
     */

    
    /** Sets the dynamic string repository object context.
     * @param   repObj  Repository object context for dynamic string.
     */

    
    /** Gets the XPath to where the string's information originates from.
     * @return  XPath of where the string's value originates from.
     */
    String getXPathToInfoSource();
    
    /** Sets the XPath to where the string's information originates from.
     * @param   xpath   XPath of where the string's value originates from.
     */
    void setXPathToInfoSource(String xpath);
    
    /** Gets the evaluated value of the dynamic string.
     * @return  The evaluated value of the dynamic string.
     */
    String getEvaluatedValue();
    
    /** Gets the persisting form of the dynamic string; used when saving to the repository.
     * @return  Persisting form of the dynamic string.
     */
    String getPersistingValue();
    
    /** Parses a persisting form of the dynamic string.
     * @param   data    Persisting form of the dynamic string.
     */
    void parsePersistingValue(String data);
    
}
