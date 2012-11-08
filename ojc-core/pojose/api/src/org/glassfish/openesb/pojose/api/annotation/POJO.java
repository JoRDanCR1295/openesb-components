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
 * @(#)POJO.java
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
 * Class level annotation designates a class as a POJO service.
 * When none of the properties specified, POJO Service Engine will defaults
 * ServiceEndpiont as below.
 * Simple class name as Endpoint name.
 * Http URI constructed from reverse package name and Endpoint name is used as 
 * default for Service and Interface namespaces.
 * Service local name defaults to Endpoint name suffixed with "Service".
 * Interface local name defaults to Endpoint name suffixed with "Interface".
 * 
 * @author sgenipud
 * @author gmpatil
 * <br>
 * @deprecated since 02/05/2009. Use {@link Provider}.
 * <br>
 * @see Provider
 * 
 */
@Target(ElementType.TYPE)
@Retention(RetentionPolicy.RUNTIME)
public @interface POJO {
   /**
    * POJO Name also used as JBI Service Endpoint Name.
    */
   public String name() default "";;
   
   /**
    * JBI endpoint Service QName.
    * If this is not specified, default Service
    * namespace will be http URL using reverse package name of the the class
    * as host name and endpoint name as the path.
    * Ex: For class "Echo" in package name "org.glassfish.openesb.soabi" 
    * default namespace will be "http://soabi.openesb.glassfish.org/Echo/".
    * 
    * If this is not specified, local name will be class name suffixed with
    * "Service"
    * Ex: If annotated the "org.glassfish.MyPojo" class with @POJO but
    * serviceName attribute is not specified, service local name defaults to
    * "MyPojoService"
    * Thus default service QName will be 
    * "{http://soabi.openesb.glassfish.org/Echo/}MyPojoService"
    */
    public String serviceQN() default "";       
    
   /**
    * Namespace of the JBI Service name. If not specified, default Service
    * namespace will be http URL using reverse package name of the the class
    * as host name and endpoint name as the path.
    * Ex: For class "Echo" in package name "org.glassfish.openesb.soabi" 
    * default namespace will be "http://soabi.openesb.glassfish.org/Echo/".
    * 
     * @deprecated 11/05/2008: Use serviceQN.
    */
   public String serviceNS() default "";       

   /**
    * Local service name. If not specified, will be class name suffixed with
    * "Service"
    * Ex: If annotated the "org.glassfish.MyPojo" class with @POJO but
    * serviceName attribute is not specified, service local name defaults to
    * "MyPojoService"
    * 
     * @deprecated 11/05/2008: Use serviceQN.
    */   
   public String serviceName() default "";
    
   
   /**
    * QName of the JBI endpoint interface. If not specified, default interface 
    * namespace will be http URL using reverse package name of the the class
    * as host name and endpoint name as the path.
    * Ex: For class "Echo" in package name "org.glassfish.openesb.soabi" 
    * default namespace will be "http://soabi.openesb.glassfish.org/Echo/".
    * 
    */
   public String interfaceQN() default "";  
   
   /**
    * Namespace of the JBI Service name. If not specified, default Service
    * namespace will be http URL using reverse package name of the the class
    * as host name and endpoint name as the path.
    * Ex: For class "Echo" in package name "org.glassfish.openesb.soabi" 
    * default namespace will be "http://soabi.openesb.glassfish.org/Echo/".
    * @deprecated 11/05/2008: Use inetrfaceQN.
    */
   public String interfaceNS() default "";  
   
   /**
    * Local interface name. If not specified, will be class name suffixed with
    * "Interface"
    * Ex: If annotated the "org.glassfish.MyPojo" class with @POJO but
    * interfaceName attribute is not specified, interface local name defaults to
    * "MyPojoInterface"
    * @deprecated 11/05/2008: Use inetrfaceQN.
    */      
   public String interfaceName() default "";
   
   /*
    * Use this to specify additional ineterface namespaes. Default
    * will be applied for empty element by suffixing default namespace with 
    * element position starting with number zero.
    * Ex: interfaceNSs = {"NS0", "", "PL3"} 
    * Seconf element defaults to "http://soabi.openesb.glassfish.org/1" assuming 
    * package name of the class as "org.glassfish.openesb.soabi".
    */      
   
   /* TODO Support later multi endpoint POJO service.
   public String[] interfaceNSs() default {};
   public String[] interfaceNames() default {};   
   public String[] serviceNSs() default {};
   public String[] serviceNames() default {};   
   public String[] endpoints() default {};   
   */
}