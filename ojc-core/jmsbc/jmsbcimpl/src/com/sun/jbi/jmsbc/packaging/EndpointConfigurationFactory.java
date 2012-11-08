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
 * @(#)EndpointConfigurationFactory.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.jmsbc.packaging;

/**
 * Factory class for creating an EndpointConfiguration
 * 
 */
public class EndpointConfigurationFactory {
        
    /**
     * Constructs an EndpointConfiguration given the type of configuration and
     * SU root dir.
     *
     * @param configType The type of configuration used for endpoints.
     * @param suRootDir The SU root directory path.
     *
     * @throws Exception upon error.
     */
    public static EndpointConfiguration getEndpointConfiguration (String suRootDir) 
        throws Exception {
        return new EndpointConfigurationSUDescriptor (suRootDir);
    }
}
