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
 * @(#)HL7Output.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.hl7bc.extensions;

import java.io.Serializable;

/**
 *
 * author S. Nageswara Rao
 */
public class HL7Output implements Serializable {
   
   private static final long serialVersionUID = 1L;

    HL7Message mHL7Message = null;
    
    /**
     * Set the hl7:message element properties for HL7 binding Input
     */
    public void setHL7Message(HL7Message message) {
        mHL7Message = message;
    }
    
    /**
     * Get the hl7:message element properties for HL7 binding Input
     */
    public HL7Message getHL7Message() {
        return mHL7Message;
    }
    
    
}
