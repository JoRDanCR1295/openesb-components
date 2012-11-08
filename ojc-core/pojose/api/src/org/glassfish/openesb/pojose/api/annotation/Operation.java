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
 * @(#)Operation.java
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

/**
 * This annotation designates a method to be called with all incoming request messages.
 * Method signature should have one argument of type String, Node, byte[], Source, 
 * NormalizedMessage or MessageExchange.
 * <br>
 * Return type can be one of void, String, Node, Source, byte[], NormalizedMessage or MessageExchange.
 * <br>
 * If another method annotated with @OnDone is present, its return type determines 
 * the message exchange pattern and output message of this service. If method 
 * annotated with @OnDone returns void, then message exchange pattern for this service is InOnly 
 * or else pattern will be InOut. Even for InOnly pattern, service engine will send done/error 
 * status only after completion of method annotated with @Operation and @OnDone if one is present. 
 * <br>
 * This behaivior is different from BPEL engine in order to account for non availibility of 
 * inbuilt message persistence.
 * 
 * @author gmpatil
 * @author sgenipudi
 * 
 */
@Target(ElementType.METHOD)
@Retention(RetentionPolicy.RUNTIME)
public @interface Operation {
    
    public String name() default "";
    
    /**
     * Output message type for InOut message exchange pattern service operation.
     * User must specify when implicit WSDL 1.1 message wrapping is used for the 
     * return type of String, Source, Node and byte[].
     * Follows QName.toString syntax. Ex: "{someNameSpace}localName" 
     */
    public String outMessageTypeQN() default "";    
    
    /**
     * @deprecated 11/06/08, use outMessageTypeQN.
     */
    public String outMessageTypeNS() default "";
    
    /**
     * @deprecated 11/06/08, use outMessageTypeQN.
     * @return String messageType local name.
     */    
    public String outMessageType() default "";
}