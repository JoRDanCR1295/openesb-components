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
 * @(#)WSDLConstants.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.wsdl.model.immutable;

/**
 * WSDL constants.
 *
 * Created on Jul 9, 2004
 * Defines the constants required for the wsdl model.
 *
 * @author Sun Microsystems
 * @version 
 * @since 5.1.0
 */

public interface WSDLConstants {
	
    /** unknown operation type. */
    int UNKNOWN_OPERATION = -1;
    
    /** one-way-operation type */
    int ONE_WAY_OPERATION = 1;
    
    /** request-response-operation type */
    int REQUEST_RESPONSE_OPERATION = 2;
    
    /** solicit-response-operation type */
    int SOLICIT_RESPONSE_OPERATION = 4;
    
    /** notification-operation type */
    int NOTIFICATION_OPERATION = 8;
    
    /** request-response-one-way-operation type */
    int REQUEST_RESPONSE_ONE_WAY_OPERATION = 3;
    
    /** solicit-response-notification-operation type */
    int SOLICIT_RESPONSE_NOTIFICATION_OPERATION = 12;
    
    /** PropertyChange name for the entire WSDLDocumentImm. */
    public static final String WSDL_DOCUMENT_PROP_CHANGE =
        "WSDL_DOCUMENT_PROP_CHANGE";                        // Not I18N
    
    /** PropertyChange name for the targetNamespace. */
    public static final String WSDL_TARGET_NAMESPACE_PROP_CHANGE =
        "WSDL_TARGET_NAMESPACE_PROP_CHANGE";                // Not I18N
    
    /** PropertyChange name for the message. */
    public static final String WSDL_MESSAGE_PROP_CHANGE =
        "WSDL_MESSAGE_PROP_CHANGE";                         // Not I18N
    
    /** PropertyChange name for the portType. */
    public static final String WSDL_PORT_TYPE_PROP_CHANGE =
        "WSDL_PORT_TYPE_PROP_CHANGE";                       // Not I18N
    
    /** PropertyChange name for the binding. */
    public static final String WSDL_BINDING_PROP_CHANGE =
        "WSDL_BINDING_PROP_CHANGE";                         // Not I18N
    
    /** PropertyChange name for the service. */
    public static final String WSDL_SERVICE_PROP_CHANGE =
        "WSDL_SERVICE_PROP_CHANGE";                         // Not I18N
}
