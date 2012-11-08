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
 * @(#)ConsumerEndpoint.java
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */
package org.glassfish.openesb.pojose.api.annotation;

import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;
import org.glassfish.openesb.pojose.api.Consumer;

/**
 * Annotates member variables of type {@link Consumer Consumer} in a 
 * {@link Provider Provider} class instance. 
 * {@link Consumer Consumer} instance will be injected before {@link Operation Operation} 
 * method is called by the service engine.
 * 
 * {@link Consumer Consumer} instance will be null when service engine fails to 
 * resolve/discover the ServiceEndpoint.
 * 
 * @author gmpatil
 * @see Consumer
 */
@Target(ElementType.FIELD)
@Retention(RetentionPolicy.RUNTIME)
public @interface ConsumerEndpoint {
    /** JBI Endpoint name */
    public String name();
    /** 
     * JBI Endpoint servce QName.
     * Follows QName toString syntax. Ex: "{someNameSpace}localName" 
     */
    public String serviceQN();

    /** 
     * JBI Endpoint interface QName. 
     * Follows QName toString syntax. Ex: "{someNameSpace}localName" 
     */
    public String interfaceQN() default "" ;       
        
    /** 
     * Input message type. 
     * Follows QName toString syntax. Ex: "{someNameSpace}localName" 
     */    
    public String inMessageTypeQN() default "" ;    
        
    /** 
     * Operation QName. 
     * Follows QName toString syntax. Ex: "{someNameSpace}localName". 
     */    
    public String operationQN() default "" ;        
}
