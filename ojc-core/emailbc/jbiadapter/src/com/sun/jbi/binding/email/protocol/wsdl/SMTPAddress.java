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
 * @(#)SMTPAddress.java 
 *
 * Copyright 2004-2009 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.binding.email.protocol.wsdl;

import javax.xml.namespace.QName;
import com.sun.jbi.binding.email.protocol.EmailBCConstants;

/**
 * @author Harry Liu (Harry.Liu@sun.com)
 */
public class SMTPAddress extends EmailAddress {
    private static final long serialVersionUID = 1L;
    public static final String ELEM_SMTP_ADDRESS = "SMTPaddress";
    public static final String ATTR_SMTP_LOCATION = "location";
    public static final QName QNAME_SMTP_ADDRESS =
        new QName(EmailBCConstants.NS_URI_EMAILBC, ELEM_SMTP_ADDRESS);

    private String mLocation;// = "mailto:user@domain";

    public SMTPAddress() {
        // apply defaults
        this.setPort(465);
        this.setUseSSL(true);
    }
    
    /**
     * @see com.sun.jbi.binding.email.protocol.wsdl.EmailAddress#getElementType()
     */
    @Override
    public QName getElementType() {
        return QNAME_SMTP_ADDRESS;
    }

    public String getLocation() {
        return mLocation;
    }

    public void setLocation(final String val) {
        mLocation = val;
    }    

    /**
     * @see com.sun.jbi.binding.email.protocol.wsdl.EmailAddress#toString()
     */
    @Override
    public String toString() {
        return super.toString() + ("\nLocation = " + mLocation);
    }
}

