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
 * @(#)Endpoint.java
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */
package org.glassfish.openesb.pojose.api.annotation;

import java.lang.annotation.*;
/**
 * Declares member variable of type ServiceEndpoint as one of endpiont POJO 
 * service will consume.
 * ServiceEndpoint instance will be injected when POJO instance is created.
 * ServiceEndpoint instance will be null when POJO Engine fails to resolve/discover the
 * Service endpoint.
 * <br>
 * @deprecated since 02/03/2009.   Use {@link ConsumerEndpoint}.
 * <br>
 * @author sgenipud
 * @author gmpatil
 *
 * @see ConsumerEndpoint
 */
@Target(ElementType.FIELD)
@Retention(RetentionPolicy.RUNTIME)
public @interface Endpoint {
    /** JBI Endpoint name */
    public String name();
    /** 
     * JBI Endpoint servce QName.
     * Follows QName toString syntax. Ex: "{someNameSpace}localName" 
     */
    public String serviceQN();
    /** 
     * JBI Endpoint servce name 
     * @deprecated 11/05/2008: Use serviceQN.
     */
    public String serviceName() default "" ; 
    /** 
     * JBI Endpoint servce namespace 
     * @deprecated 11/05/2008: Use serviceQN.
     */
    public String serviceNS() default "" ;    

    /** 
     * JBI Endpoint interface QName. 
     * Follows QName toString syntax. Ex: "{someNameSpace}localName" 
     */
    public String interfaceQN() default "" ;       
    
    /** 
     * JBI Endpoint interface namespace. Used as Operation namespace as well 
     *  as input message type namespace. 
     * @deprecated 11/05/2008: Use interfaceQN.
     */
    public String interfaceNS() default "" ;
    
    /** 
     * JBI Endpoint interface 
     * @deprecated 11/05/2008: Use interfaceQN.
     */    
    public String interfaceName() default "" ;
    
    /** 
     * Input message type. 
     * Follows QName toString syntax. Ex: "{someNameSpace}localName" 
     */    
    public String inMessageTypeQN() default "" ;    
    
    /** 
     * Input message type. Interface namespace will be used as MessageType
     * namspace.  
     * @deprecated 11/05/2008: Use inMessageTypeQN.
     */    
    public String inMessageType() default "" ;
    
    /** 
     * Operation QName. 
     * Follows QName toString syntax. Ex: "{someNameSpace}localName". 
     */    
    public String operationQN() default "" ;        
    
    /** 
     * Operation name. Interface namespace will be used as operation namspace.
     * @deprecated 11/05/2008: Use operationQN.
     */    
    public String operationName() default "" ;    

}
