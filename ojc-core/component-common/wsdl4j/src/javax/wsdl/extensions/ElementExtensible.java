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
 * @(#)ElementExtensible.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package javax.wsdl.extensions;

import java.util.*;
import javax.wsdl.extensions.ExtensibilityElement;

/**
 * Classes that implement this interface can contain extensibility
 * elements.
 * 
 * @author John Kaputin
 */
public interface ElementExtensible {
    
    /**
     * Add an extensibility element.
     *
     * @param extElement the extensibility element to be added
     */
    public void addExtensibilityElement(ExtensibilityElement extElement);
    
    /**
     * Remove an extensibility element.
     *
     * @param extElement the extensibility element to be removed
     * @return the extensibility element which was removed
     */
    public ExtensibilityElement removeExtensibilityElement(ExtensibilityElement extElement);

    /**
     * Get all the extensibility elements defined here.
     */
    public List getExtensibilityElements();


}
