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
 * @(#)OperationReference.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.bpel.model;

import javax.wsdl.Operation;

public interface OperationReference {

        /**
         * Describes the attributes for this element.
         */
        public interface ATTR {
        /**
             * operation atribute name.
             */
            public static final String OPERATION = "operation";
        }	
        
	/**
	 * Gets the value of the operation property.
	 * 
	 * @return possible object is {@link String }
	 */
	String getOperation();

	/**
	 * Sets the value of the operation property.
	 * 
	 * @param value
	 *            allowed object is {@link String }
	 
	 */
	void setOperation( String value );
        
        /** Getter for property operation.
         * @return Value of property operation.
         *
         */
        Operation getWSDLOperation();
    
    
        /** Setter for property operation.
         * @param operation New value of property operation.
         *
         */
        void setWSDLOperation(Operation operation);
    
}
