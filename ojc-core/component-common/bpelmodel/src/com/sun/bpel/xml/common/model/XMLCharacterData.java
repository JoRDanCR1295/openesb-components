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
 * @(#)XMLCharacterData.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.bpel.xml.common.model;

import java.io.Serializable;

/**
 * Describes XML character data (such as text, comment, CDATA).
 *
 * @author Sun Microsystems
 * @version 
 */
public interface XMLCharacterData extends Serializable {
    
    /** Getter for value of XML text.
     * @return  Value of text.
     */
    String getValue();
    
    /** Setter for value of XML text.
     * @param   v   value of text.
     */
    void setValue(String v);
    
    /**
     * Tests if CDATA form is to be used.
     * @return  <code>true</code> if CDATA form is used.
     */
    boolean isCDATAForm();
    
    /**
     * Specifies whether the CDATA form is to be used.
     * @param   useCDATA    <code>true</code> if CDATA form is to be used.
     */
    void setCDATAForm(boolean useCDATA);

}
