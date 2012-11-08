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
 * @(#)TaleService.java
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.common.tale.core.domain.service;

import javax.xml.namespace.QName;

import com.sun.jbi.common.tale.core.domain.TaleRequest;
import com.sun.jbi.common.tale.core.util.TaleException;

/**
 * Defines contract for TALE service implementations.
 * @author Kevan Simpson
 */
public interface TaleService {
    // XXX change wsdl/xsd namespaces?
    public static final String ALE_WSDL_NS = "http://www.sun.com/ALE/1.0/ALEService.wsdl";
    public static final String ALE_TYPE_NS = "http://www.sun.com/ALE/1.0/ALETypes.xsd";
    
    public static final QName LOGGING_SERVICE   = new QName(ALE_WSDL_NS, "LoggingService", "ale"); 
    public static final QName ALERT_SERVICE     = new QName(ALE_WSDL_NS, "AlertService", "ale"); 
    public static final QName ERROR_SERVICE     = new QName(ALE_WSDL_NS, "ErrorService", "ale");
    
    public static final String LOGGING_ENDPOINT = "ALELoggingEndpoint"; 
    public static final String ALERT_ENDPOINT   = "ALEAlertEndpoint";
    public static final String ERROR_ENDPOINT   = "ALEErrorEndpoint";
    
    public void execute(TaleRequest request) throws TaleException;
}
