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
 * @(#)SOAPValidatorFactory.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.httpsoapbc.validation.conf.wsi.bp.soap.api;

import com.sun.jbi.httpsoapbc.validation.conf.wsi.api.WSIConstants;
import com.sun.jbi.httpsoapbc.validation.conf.wsi.api.WSIValidationException;
import com.sun.jbi.httpsoapbc.validation.conf.wsi.bp.v10.impl.BPv10SOAPMsgValidatorImpl;
import com.sun.jbi.internationalization.Messages;

/**
 * The factory used for creating object instances to validate the SOAP Message
 * for WSI Basic Profile conformance.
 *
 */
public class SOAPValidatorFactory {
    private static final Messages messages =
        Messages.getMessages(com.sun.jbi.httpsoapbc.validation.conf.wsi.api.WSIConstants.class);
    
    private SOAPValidatorFactory() {
    }

    private static class LazyHolder {
        static final SOAPValidatorFactory instance = new SOAPValidatorFactory();
    }

    public static SOAPValidatorFactory getInstance() {
        return LazyHolder.instance;
    }

    public SOAPMsgValidator createSOAPMsgValidator(String bpVersion, String level)
            throws WSIValidationException {
        if (bpVersion == null) {
            bpVersion = WSIConstants.BPv10;
        }
        
        if (level == null) {
            level = WSIConstants.ERROR;
        }
        
        if (bpVersion.equals(WSIConstants.BPv10)) {
            return new BPv10SOAPMsgValidatorImpl(level);
        } else {
            throw new WSIValidationException(messages.getString("BP_Version_Not_Supported", bpVersion));
        }
    }
}
