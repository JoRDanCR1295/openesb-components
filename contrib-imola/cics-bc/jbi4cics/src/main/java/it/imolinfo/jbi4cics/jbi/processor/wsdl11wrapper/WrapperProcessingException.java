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
 * @(#)WrapperProcessingException.java - ver 1.0 - 10/31/2006
 *
 * Copyright 2004-2006 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package it.imolinfo.jbi4cics.jbi.processor.wsdl11wrapper;

import it.imolinfo.jbi4cics.jbi.processor.wsdl11wrapper.*;

/**
 * An exception occurred in handling the JBI normalized message wsdl 1.1 wrapper.
 * @author Sun Microsystems
 */
public class WrapperProcessingException extends Exception {

    public WrapperProcessingException() {
        super();
    }

    public WrapperProcessingException(String message) {
        super(message);
    }

    public WrapperProcessingException(String message, Throwable cause) {
        super(message, cause);
    }

    public WrapperProcessingException(Throwable cause) {
        super(cause);
    }
}
