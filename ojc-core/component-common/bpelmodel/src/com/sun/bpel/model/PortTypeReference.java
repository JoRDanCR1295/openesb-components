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
 * @(#)PortTypeReference.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.bpel.model;

import javax.xml.namespace.QName;
import javax.wsdl.PortType;

public interface PortTypeReference {

        public interface ATTR {
            /** "portType" attribute token */
            public static final String PORT_TYPE = "portType";
        
        }
        
	/** Getter for attribute portType.
         * @return Value of attribute portType.
         *
         */
        QName getPortType();

        /** Setter for attribute portType.
         * @param portType New value of attribute portType.
         *
         */
        void setPortType(QName portType);
        
        
        /** Getter for property portType.
         * @return Value of property portType.
         *
         */
        PortType getWSDLPortType();

        /** Setter for property portType.
         * @param portType New value of property portType.
         *
         */
        void setWSDLPortType(PortType portType);
}
