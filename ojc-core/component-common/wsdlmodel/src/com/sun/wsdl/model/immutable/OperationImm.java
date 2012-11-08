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
 * @(#)OperationImm.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.wsdl.model.immutable;

import java.util.List;


/**
 * Describes the portType &lt;operation&gt; element.
 *
 * @author Sun Microsystems
 * @version 
 * @since   5.1.0
 */
public interface OperationImm extends WSDLElementImm {
   
    /**
     * Getter for property name.
     * @return Value of property name.
     */
    String getName();
    
    /**
     * Gets the operation type.
     * @return the operation type.
     * @see com.sun.wsdl.model.common.integration.model.wsdlproducer.WSDLConstants
	 * @see com.sun.wsdl.model.common.integration.model.wsdlproducer.WSDLConstants#UNKNOWN_OPERATION 
	 * @see com.sun.wsdl.model.common.integration.model.wsdlproducer.WSDLConstants#ONE_WAY_OPERATION 
	 * @see com.sun.wsdl.model.common.integration.model.wsdlproducer.WSDLConstants#REQUEST_RESPONSE_OPERATION 
	 * @see com.sun.wsdl.model.common.integration.model.wsdlproducer.WSDLConstants#SOLICIT_RESPONSE_OPERATION 
	 * @see com.sun.wsdl.model.common.integration.model.wsdlproducer.WSDLConstants#NOTIFICATION_OPERATION 
	 * @see com.sun.wsdl.model.common.integration.model.wsdlproducer.WSDLConstants#REQUEST_RESPONSE_ONE_WAY_OPERATION
	 * @see com.sun.wsdl.model.common.integration.model.wsdlproducer.WSDLConstants#SOLICIT_RESPONSE_NOTIFICATION_OPERATION
	 */
    int getOperationType();
    
    /**
     * Gets the &lt;input&gt; element.
     * @return the input element
     */
    OperationInputImm getInput();
    
    /**
     * Gets the &lt;output&gt; element.
     * @return the output element
     */
    OperationOutputImm getOutput();
    
    /**
     * Gets the list of all faults.
     * @return a read-only list of OperationFaultsImm.
     */
    List getFaults();
}
